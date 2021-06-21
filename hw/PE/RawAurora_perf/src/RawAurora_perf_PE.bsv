package RawAurora_perf_PE;

import ClientServer :: *;
import GetPut :: *;
import Vector :: *;
import FIFOF :: *;
import FIFO :: *;
import DReg :: *;

import BlueAXI :: *;
import BlueLib :: *;

// **************** config ****************

// size of counter in byte
Integer c_sz = 8;
typedef Bit#(64) Counter_t;

// axi stream iface
Integer cfg_buf_len = 2;
typedef 0   AXIS_USER_WIDTH;
typedef 512	AXIS_DATA_WIDTH;

// axi control interface
typedef 12 CONFIG_ADDR_WIDTH;
typedef 64 CONFIG_DATA_WIDTH;
// ****************************************

function Integer toByteLen(Bit#(bitlen) x) = valueOf(TDiv#(bitlen, 8));

function Bit#(len) toBitmap(Bit#(param_len) number_of_ones);
	Bit#(len) mask = unpack('1);
	return mask >> (fromInteger(valueOf(len)) - number_of_ones);
endfunction

function Bit#(m) fromBitmap(Bit#(n) x);
	Bit#(m) res = 0;
	for (Integer i=0; i<valueOf(n); i=i+1)
		if (x[i] == 1) res = fromInteger(i+1);
	return res;
endfunction

function AXI4_Stream_Pkg#(dw, 0) mkPkg(Bit#(dw) data, Bit#(TDiv#(dw, 8)) keep, Bool last) = AXI4_Stream_Pkg {
	data: data,
	keep: keep,
	last: last,
	user: 0,
	dest: 0
};

/* types */
typedef enum {
	MODE_SRV_THR = 0,
	MODE_CLT_THR = 1,
	MODE_SRV_LAT = 2,
	MODE_CLT_LAT = 3
} OperMode_t deriving(Bits, Eq, FShow);

/* Interface */
interface RawAurora_perf_PE;
	(*prefix="S00_AXI"*) interface AXI4_Lite_Slave_Rd_Fab#(CONFIG_ADDR_WIDTH, CONFIG_DATA_WIDTH) s_rd;
	(*prefix="S00_AXI"*) interface AXI4_Lite_Slave_Wr_Fab#(CONFIG_ADDR_WIDTH, CONFIG_DATA_WIDTH) s_wr;

	(*prefix="axis_net_tx_0"*) interface AXI4_Stream_Wr_Fab #(AXIS_DATA_WIDTH, AXIS_USER_WIDTH) i_tx;
	(*prefix="axis_net_rx_0"*) interface AXI4_Stream_Rd_Fab #(AXIS_DATA_WIDTH, AXIS_USER_WIDTH) i_rx;

	(*always_enabled*) method bit interrupt;
endinterface

(* default_reset = "ap_rst_n", clock_prefix = "ap_clk" *)
module mkRawAurora_perf_PE(RawAurora_perf_PE);

	Reg#(OperMode_t) oper_mode <- mkRegU;
	Reg#(Bit#(3)) oper_state <- mkReg(0);
	Reg#(bit) r_interrupt <- mkDReg(0);
	Reg#(Maybe#(Bool)) success <- mkReg(tagged Invalid);

	Reg#(Bit#(40)) received_bytes <- mkReg(0);
	Reg#(Bit#(40)) sent_bytes <- mkReg(0);
	Reg#(Bit#(40)) clk <- mkReg(0);
	Reg#(Counter_t) counter <- mkReg(0);
	PulseWire reset_clk <- mkPulseWire();

	/* single-word ping packet, Aurora MAC has 256bit interface */
	Vector#(4, Bit#(64)) ping_payload_vec;
	for (Integer i=0; i<4; i=i+1) ping_payload_vec[i] = {48'hDEADBEEFABCD, fromInteger(i)};
	Bit#(256) ping_payload = pack(ping_payload_vec);
	Bit#(16) ping_payload_len = fromInteger(toByteLen(ping_payload));

	/* MAC interface */
	AXI4_Stream_Wr #(AXIS_DATA_WIDTH, AXIS_USER_WIDTH) tx_port <- mkAXI4_Stream_Wr(cfg_buf_len);
	AXI4_Stream_Rd #(AXIS_DATA_WIDTH, AXIS_USER_WIDTH) rx_port <- mkAXI4_Stream_Rd(cfg_buf_len);

	/* control interface */
	Reg#(Bool) ctrl_start <- mkDReg(False);
	Reg#(Bit#(64)) ctrl_ret <- mkReg(0);
	Reg#(Bit#(32)) ctrl_cmd <- mkReg(0);
	Reg#(Bit#(64)) ctrl_length <- mkReg(1000);

	List#(RegisterOperator#(axiAddrWidth, CONFIG_DATA_WIDTH)) operators = Nil;
	operators = registerHandler('h00, ctrl_start, operators);
	operators = registerHandler('h10, ctrl_ret, operators);
	operators = registerHandler('h20, ctrl_cmd, operators);    // tapasco param 1
	operators = registerHandler('h30, ctrl_length, operators); // tapasco param 2
	GenericAxi4LiteSlave#(CONFIG_ADDR_WIDTH, CONFIG_DATA_WIDTH) s_config <- mkGenericAxi4LiteSlave(operators, 1, 1);

	(*no_implicit_conditions, fire_when_enabled*)
	rule arbiter(oper_state == 0);
		if (success matches tagged Valid .succ) begin
			ctrl_ret <= succ ? extend(clk) : 0;
			r_interrupt <= 1;
			success <= tagged Invalid;
		end

		if (ctrl_start) begin
			OperMode_t m = unpack(truncate(ctrl_cmd));
			$display("[Arbiter] Starting mode=%d : ", m, fshow(m));

			received_bytes <= 0;
			sent_bytes <= 0;
			counter <= 0;

			oper_mode <= m;
			oper_state <= 1;
		end
	endrule

	rule r_clk;
		clk <= reset_clk ? 0 : (clk + 1);
	endrule


	/****************************************************************
	 * SERVER - throughput
	 ***************************************************************/
	rule s_t_0(oper_mode == MODE_SRV_THR && oper_state == 1);
		let raw <- rx_port.pkg.get();
		Bit#(8) word_len = fromBitmap(raw.keep);
		Bit#(8) num_fields = word_len / fromInteger(c_sz);
		printColorTimed(BLUE, $format("[RX] received_bytes %d, wordLen %d, isLast %d, clk %d", received_bytes, word_len, raw.last, clk));

		// check payload
		Bool ok = True;
		Vector#(8, Counter_t) data_vec = unpack(raw.data);
		for (Integer i=0; i<8; i=i+1) begin
			if (raw.keep[(c_sz*i) + c_sz-1] == 1) begin
				if (data_vec[i] == counter + fromInteger(i)) begin
					printColorTimed(BLUE, $format("--> [%d] OK!", i));
				end else begin
					printColorTimed(BLUE, $format("--> [%d] Mismatch! Is %x, expected %x", i, data_vec[i], counter+fromInteger(i)));
					ok = False;
				end
			end
		end

		if (!ok) begin
			printColorTimed(BLUE, $format("--> Check failed, aborting"));
			success <= tagged Valid False;
			oper_state <= 0;
		end else begin
			if (received_bytes == 0) begin
				printColorTimed(BLUE, $format("--> First word of first pkt in tfer, starting clock"));
				reset_clk.send();
			end

			if (raw.last) begin
				printColorTimed(BLUE, $format("--> Pkt done"));
				oper_state <= 2;
			end
		end

		counter <= counter + extend(num_fields);
		received_bytes <= received_bytes + extend(word_len);
	endrule

	rule s_t_1(oper_mode == MODE_SRV_THR && oper_state == 2);
		if (received_bytes >= truncate(ctrl_length)) begin
			printColorTimed(BLUE, $format("[RX2] Tfer done! %d bytes, expected %d. CLK=%d", received_bytes, ctrl_length, clk));
			success <= tagged Valid True;
			oper_state <= 0;
		end else begin
			oper_state <= 1;
		end
	endrule


	/****************************************************************
	 * CLIENT - throughput
	 ***************************************************************/

	// word = 64 byte = 8*8byte counter
	rule c_t_0(oper_mode == MODE_CLT_THR && oper_state == 1);

		Bit#(8) word_len = fromInteger(valueOf(AXIS_DATA_WIDTH) / 8);
		printColorTimed(YELLOW, $format("[TX] Send: %d sent total, clk %d", sent_bytes, clk));

		if (sent_bytes == 0) begin
			printColorTimed(YELLOW, $format("--> First word in tfer, starting clock"));
			reset_clk.send();
		end

		Bool isLast = False;
		if (sent_bytes + extend(word_len) >= truncate(ctrl_length)) begin
			printColorTimed(YELLOW, $format("--> Tfer done: %d bytes sent, %d requested. CLK=%d", sent_bytes+extend(word_len), ctrl_length, clk));
			isLast = True;

			success <= tagged Valid True;
			oper_state <= 0;
		end

		counter <= counter + (extend(word_len) / fromInteger(c_sz));
		sent_bytes <= sent_bytes + extend(word_len);

		Vector#(8, Counter_t) data_vec;
		for (Integer i=0; i<8; i=i+1) begin
			data_vec[i] = counter + fromInteger(i);
		end
		tx_port.pkg.put(mkPkg(pack(data_vec), toBitmap(word_len), isLast));
	endrule


	/****************************************************************
	 * SERVER - latency
	 ***************************************************************/
	rule s_l_0(oper_mode == MODE_SRV_LAT && oper_state == 1);
		let pkt <- rx_port.pkg.get();
		printColorTimed(YELLOW, $format("[SRV] Returning ping"));
		tx_port.pkg.put(pkt);
	endrule


	/****************************************************************
	 * CLIENT - latency
	 ***************************************************************/
	rule c_l_0(oper_mode == MODE_CLT_LAT && oper_state == 1);
		printColorTimed(YELLOW, $format("[CLT] 1) Sending ping, reset clk"));
		AXI4_Stream_Pkg#(AXIS_DATA_WIDTH, 0) pkt = AXI4_Stream_Pkg {
			data: extend(ping_payload),
			keep: toBitmap(ping_payload_len),
			last: True,
			user: 0,
			dest: 0
		};
		tx_port.pkg.put(pkt);

		oper_state <= 2;
		reset_clk.send();
	endrule

	rule c_t_1(oper_mode == MODE_CLT_LAT && oper_state == 2);
		let pkt <- rx_port.pkg.get();
		if (pkt.keep[31] == 1 && truncate(pkt.data) == ping_payload) begin
			printColorTimed(YELLOW, $format("[CLT] 2) payload matches! clk=%d", clk));
			success <= tagged Valid True;
		end else begin
			printColorTimed(YELLOW, $format("[CLT] 2) MISMATCH! clk=%d", clk));
			success <= tagged Valid False;
		end

		oper_state <= 0;
	endrule


	/****************************************************************
	 * Interfaces
	 ***************************************************************/
	interface s_rd = s_config.s_rd;
	interface s_wr = s_config.s_wr;

	interface i_tx = tx_port.fab;
	interface i_rx = rx_port.fab;

	method bit interrupt = r_interrupt;
endmodule

endpackage
