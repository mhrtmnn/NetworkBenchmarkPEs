package RawEth_perf_PE;

import SpecialFIFOs :: *;
import ClientServer :: *;
import BRAMFIFO :: * ;
import GetPut :: *;
import Vector :: *;
import FIFOF :: *;
import FIFO :: *;
import DReg :: *;

import BlueAXI :: *;
import BlueLib :: *;

// **************** config ****************
Integer payload_len = 4096;

// size of counter in byte
Integer c_sz = 2;
typedef Bit#(16) Counter_t;

// axi stream iface
Integer cfg_buf_len = 2;
typedef 0   AXIS_USER_WIDTH;
typedef 512	AXIS_DATA_WIDTH;

// axi control interface
typedef 12 CONFIG_ADDR_WIDTH;
typedef 64 CONFIG_DATA_WIDTH;
// ****************************************

/* Helper Functions */
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

function Bit#(bitwidth) swapBytes(Bit#(bitwidth) bit_in) provisos(
	Mul#(TDiv#(bitwidth, 8), 8, bitwidth),
	Div#(bitwidth, 8, bytewidth)
);
	Vector#(bytewidth, Bit#(8)) v = unpack(bit_in);
	return pack(reverse(v));
endfunction

function EthHeader make_hdr(Bit#(n) src, Bit#(n) dst) provisos (Add#(48,__x,n));
	let hdr = EthHeader {
		dst_mac: swapBytes(truncate(dst)),
		src_mac: swapBytes(truncate(src)),
		ethertype: swapBytes(16'h1234)
	};
	return hdr;
endfunction

function EthHeader parse_hdr(Bit#(112) raw);
	EthHeader hdr1 = unpack(raw);
	let hdr2 = EthHeader {
		dst_mac: swapBytes(hdr1.dst_mac),
		src_mac: swapBytes(hdr1.src_mac),
		ethertype: swapBytes(hdr1.ethertype)
	};
	return hdr2;
endfunction

function Action mac(Bit#(48) addr);
	Vector#(6, Bit#(8)) v = unpack(addr);
	$write("%x:%x:%x:%x:%x:%x", v[5], v[4], v[3], v[2], v[1], v[0]);
endfunction

/* types */
typedef enum {
	MODE_SRV_THR = 0,
	MODE_CLT_THR = 1,
	MODE_SRV_LAT = 2,
	MODE_CLT_LAT = 3
} OperMode_t deriving(Bits, Eq, FShow);

// BSV ordering is reversed
typedef struct {
	Bit#(16) ethertype;
	Bit#(48) src_mac;
	Bit#(48) dst_mac;
} EthHeader deriving(Bits, FShow);
Integer eth_header_len = valueOf(SizeOf#(EthHeader))/8;

/* Interface */
interface RawEth_perf_PE;
	(*prefix="S00_AXI"*) interface AXI4_Lite_Slave_Rd_Fab#(CONFIG_ADDR_WIDTH, CONFIG_DATA_WIDTH) s_rd;
	(*prefix="S00_AXI"*) interface AXI4_Lite_Slave_Wr_Fab#(CONFIG_ADDR_WIDTH, CONFIG_DATA_WIDTH) s_wr;

	(*prefix="axis_net_tx_0"*) interface AXI4_Stream_Wr_Fab #(AXIS_DATA_WIDTH, AXIS_USER_WIDTH) i_tx;
	(*prefix="axis_net_rx_0"*) interface AXI4_Stream_Rd_Fab #(AXIS_DATA_WIDTH, AXIS_USER_WIDTH) i_rx;

	(*always_enabled*) method bit interrupt;
endinterface

(* default_reset = "ap_rst_n", clock_prefix = "ap_clk" *)
module mkRawEth_perf_PE(RawEth_perf_PE);

	/* 32byte ping packet payload */
	Vector#(4, Bit#(64)) ping_payload_vec;
	for (Integer i=0; i<4; i=i+1) ping_payload_vec[i] = {48'hDEADBEEFABCD, fromInteger(i)};
	Bit#(256) ping_payload = pack(ping_payload_vec);
	Bit#(16) ping_payload_len = fromInteger(toByteLen(ping_payload));

	Reg#(OperMode_t) oper_mode <- mkRegU;
	Reg#(Bit#(3)) oper_state <- mkReg(0);
	Reg#(bit) r_interrupt <- mkDReg(0);
	Reg#(Maybe#(Bool)) success <- mkReg(tagged Invalid);

	Reg#(Bit#(40)) received_bytes <- mkReg(0);
	Reg#(Bit#(40)) sent_bytes <- mkReg(0);
	Reg#(Bit#(16)) sent_in_pkt <- mkReg(0);
	Reg#(Bit#(40)) clk <- mkReg(0);
	Reg#(Counter_t) counter <- mkReg(0);

	PulseWire reset_clk <- mkPulseWire();

	/* MAC interface */
	AXI4_Stream_Wr #(AXIS_DATA_WIDTH, AXIS_USER_WIDTH) tx_port <- mkAXI4_Stream_Wr(cfg_buf_len);
	AXI4_Stream_Rd #(AXIS_DATA_WIDTH, AXIS_USER_WIDTH) rx_port <- mkAXI4_Stream_Rd(cfg_buf_len);

	/* control interface */
	Reg#(Bool) ctrl_start <- mkDReg(False);
	Reg#(Bit#(64)) ctrl_ret <- mkReg(0);
	Reg#(Bit#(32)) ctrl_cmd <- mkReg(0);
	Reg#(Bit#(64)) ctrl_ourMAC   <- mkReg(0);
	Reg#(Bit#(64)) ctrl_theirMAC <- mkReg(0);
	Reg#(Bit#(64)) ctrl_length <- mkReg(1000);

	List#(RegisterOperator#(axiAddrWidth, CONFIG_DATA_WIDTH)) operators = Nil;
	operators = registerHandler('h00, ctrl_start, operators);
	operators = registerHandler('h10, ctrl_ret, operators);
	operators = registerHandler('h20, ctrl_cmd, operators);      // tapasco param 1
	operators = registerHandler('h30, ctrl_ourMAC, operators);   // tapasco param 2
	operators = registerHandler('h40, ctrl_theirMAC, operators); // tapasco param 3
	operators = registerHandler('h50, ctrl_length, operators);   // tapasco param 4
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

			oper_state <= 1;
			oper_mode <= m;
		end
	endrule

	rule r_clk;
		clk <= reset_clk ? 0 : (clk + 1);
	endrule


	/****************************************************************
	 * SERVER - throughput
	 ***************************************************************/
	function ActionValue#(Bool) check_payload(Vector#(n, Counter_t) data_vec, Vector#(m, bit) keep, Counter_t ctr)
		provisos (Mul#(c_sz, n, m));
	actionvalue
		Vector#(n, bit) cmp_res;
		for (Integer i=0; i<valueOf(n); i=i+1) begin
			if (data_vec[i] == ctr + fromInteger(i)) begin
				$display("--> [%d] OK!", i);
				cmp_res[i] = 1;
			end else begin
				$display("--> [%d] Mismatch! Is %d, expected %d (entry valid=%d)",
					i, data_vec[i], ctr+fromInteger(i), keep[(c_sz*i) + c_sz-1] == 1);
				cmp_res[i] = 0;
			end
		end

		Vector#(n, bit) entry_valid;
		for (Integer i=0; i<valueOf(n); i=i+1) begin
			entry_valid[i] = keep[(c_sz*i) + c_sz-1];
		end

		return (pack(entry_valid) & pack(cmp_res)) == pack(entry_valid);
	endactionvalue
	endfunction

	// handles first word that includes ethernet header
	rule re0(oper_mode == MODE_SRV_THR && oper_state == 1);
		let raw <- rx_port.pkg.get();
		EthHeader hdr = parse_hdr(truncate(raw.data));
		$write("[S,T 1] src="); mac(hdr.src_mac); $write(" dst="); mac(hdr.dst_mac); $display(" type=%x", hdr.ethertype);

		if (raw.keep[eth_header_len] == 1 && hdr.dst_mac == truncate(ctrl_ourMAC)) begin
			$display("--> MAC Matched!");
			Bit#(8) tfer_amount = fromBitmap(raw.keep) - fromInteger(eth_header_len);
			Bit#(8) num_fields = tfer_amount / fromInteger(c_sz);

			// (word_bytes - eth_header_len) / c_sz = 25
			Vector#(25, Counter_t) data_vec = takeTail(unpack(raw.data));
			// word_bits - eth_header_len = 50
			Vector#(50, bit) keep = takeTail(unpack(raw.keep));

			Bool ok <- check_payload(data_vec, keep, counter);
			if (!ok) begin
				$display("--> Check failed, aborting");
				success <= tagged Valid False;
				oper_state <= 0;
			end else begin
				if (received_bytes == 0) begin
					$display("--> First word of first pkt in tfer, starting clock");
					reset_clk.send();
				end
				oper_state <= 2;
			end
			counter <= counter + extend(num_fields);
			received_bytes <= received_bytes + extend(tfer_amount);
		end else begin
			$display("--> Not ours, dropping");
			oper_state <= 4;
		end
	endrule

	// handles all subsequent words that are pure payload
	rule re1(oper_mode == MODE_SRV_THR && oper_state == 2);
		let raw <- rx_port.pkg.get();
		Bit#(8) tfer_amount = fromBitmap(raw.keep);
		Bit#(8) num_fields = tfer_amount / fromInteger(c_sz);
		$display("[S,T 2] received_bytes %d, wordLen %d, isLast %d, clk %d", received_bytes, tfer_amount, raw.last, clk);

		// word_len / c_sz = 32
		Vector#(32, Counter_t) data_vec = unpack(raw.data);

		Bool ok <- check_payload(data_vec, unpack(raw.keep), counter);
		if (!ok) begin
			$display("--> Check failed, aborting");
			success <= tagged Valid False;
			oper_state <= 0;
		end else if (raw.last) begin
			$display("--> Pkt done");
			oper_state <= 3;
		end

		received_bytes <= received_bytes + extend(tfer_amount);
		counter <= counter + extend(num_fields);
	endrule

	rule re2(oper_mode == MODE_SRV_THR && oper_state == 3);
		if (received_bytes >= truncate(ctrl_length)) begin
			$display("[S,T 3] Tfer done! %d bytes, expected %d. CLK=%d", received_bytes, ctrl_length, clk);
			success <= tagged Valid True;
			oper_state <= 0;
		end else begin
			oper_state <= 1;
		end
	endrule

	rule re3(oper_mode == MODE_SRV_THR && oper_state == 4);
		$display("[S,T 4] Dropping frame ...");
		let raw <- rx_port.pkg.get();
		if (raw.last) begin
			oper_state <= 1;
		end
	endrule


	/****************************************************************
	 * CLIENT - throughput
	 ***************************************************************/

	// handles first word that includes ethernet header
	rule r0(oper_mode == MODE_CLT_THR && oper_state == 1);
		Bit#(8) word_len = fromInteger(valueOf(AXIS_DATA_WIDTH)/8 - eth_header_len);
		Bit#(8) num_fields = fromInteger((valueOf(AXIS_DATA_WIDTH)/8 - eth_header_len) / c_sz);
		$display("[C,T 1] Starting new packet, word %d bytes, total sent %d, clk %d", word_len, sent_bytes, clk);

		if (sent_bytes == 0) begin
			$display("--> First word in tfer, starting clock");
			reset_clk.send();
		end

		Bool isLast = False;
		if (sent_bytes + extend(word_len) >= truncate(ctrl_length)) begin
			$display("--> Tfer done: %d bytes sent, %d requested. CLK=%d", sent_bytes+extend(word_len), ctrl_length, clk);
			isLast = True;

			success <= tagged Valid True;
			oper_state <= 0;
		end else begin
			oper_state <= 2;
		end

		counter <= counter + extend(num_fields);
		sent_bytes <= sent_bytes + extend(word_len);
		sent_in_pkt <= extend(word_len);

		Vector#(25, Counter_t) data_vec;
		for (Integer i=0; i<25; i=i+1) begin
			data_vec[i] = counter + fromInteger(i);
		end
		Bit#(AXIS_DATA_WIDTH) data = {pack(data_vec), pack(make_hdr(ctrl_ourMAC, ctrl_theirMAC))};
		tx_port.pkg.put(mkPkg(data, toBitmap(16'd64), isLast));
	endrule

	// handles all subsequent words that are pure payload
	rule r1(oper_mode == MODE_CLT_THR && oper_state == 2);
		Bit#(8) word_len = truncate(min(fromInteger(valueOf(AXIS_DATA_WIDTH) / 8), fromInteger(payload_len) - sent_in_pkt));
		Bit#(8) num_fields = word_len / fromInteger(c_sz);
		$display("[C,T 2] Send: %d sent in pkt, %d total, clk %d", sent_in_pkt, sent_bytes, clk);

		Bool isLast = False;
		if (sent_bytes + extend(word_len) >= truncate(ctrl_length)) begin
			$display("--> Tfer done: %d bytes sent, %d requested. CLK=%d", sent_bytes+extend(word_len), ctrl_length, clk);
			isLast = True;

			success <= tagged Valid True;
			oper_state <= 0;
		end else if (sent_in_pkt + extend(word_len) >= fromInteger(payload_len)) begin
			$display("--> Pkt done");
			isLast = True;

			oper_state <= 1;
		end

		counter <= counter + extend(num_fields);
		sent_bytes <= sent_bytes + extend(word_len);
		sent_in_pkt <= sent_in_pkt + extend(word_len);

		Vector#(32, Counter_t) data_vec;
		for (Integer i=0; i<32; i=i+1) begin
			data_vec[i] = counter + fromInteger(i);
		end
		tx_port.pkg.put(mkPkg(pack(data_vec), toBitmap(word_len), isLast));
	endrule


	/****************************************************************
	 * SERVER - latency
	 ***************************************************************/
	rule s_l_0(oper_mode == MODE_SRV_LAT && oper_state == 1);
		let pkt <- rx_port.pkg.get();

		EthHeader hdr = parse_hdr(truncate(pkt.data));
		pkt.data = {pkt.data[(valueOf(AXIS_DATA_WIDTH)-1):(eth_header_len*8)], pack(make_hdr(hdr.dst_mac, hdr.src_mac))};
		$write("[S,L 1] Returning ping to "); mac(hdr.src_mac); $display("");

		tx_port.pkg.put(pkt);
	endrule


	/****************************************************************
	 * CLIENT - latency
	 ***************************************************************/
	rule c_l_0(oper_mode == MODE_CLT_LAT && oper_state == 1);
		$display("[C,L 1] Sending ping, reset clk");

		Bit#(AXIS_DATA_WIDTH) data = extend({ping_payload, pack(make_hdr(ctrl_ourMAC, ctrl_theirMAC))});
		tx_port.pkg.put(mkPkg(data, toBitmap(ping_payload_len + fromInteger(eth_header_len)), True));

		oper_state <= 2;
		reset_clk.send();
	endrule

	rule c_t_1(oper_mode == MODE_CLT_LAT && oper_state == 2);
		let pkt <- rx_port.pkg.get();
		EthHeader hdr = parse_hdr(truncate(pkt.data));
		$write("[C,L 2] src="); mac(hdr.src_mac); $write(" dst="); mac(hdr.dst_mac); $display(" type=%x", hdr.ethertype);

		if (pkt.keep[eth_header_len] == 1 && hdr.dst_mac == truncate(ctrl_ourMAC)) begin
			$display("--> MAC Matched!");

			Bit#(16) idx_l = fromInteger(eth_header_len);
			Bit#(16) idx_h = ping_payload_len + fromInteger(eth_header_len);
			if (pkt.keep[idx_h-1] == 1 && pkt.data[(8*idx_h-1):(8*idx_l)] == ping_payload) begin
				$display("--> payload matches! clk=%d", clk);
				success <= tagged Valid True;
			end else begin
				$display("--> MISMATCH! clk=%d", clk);
				success <= tagged Valid False;
			end

			oper_state <= 0;
		end else begin
			$display("--> Not ours, dropping");
			oper_state <= 3;
		end

	endrule

	rule c_t_2(oper_mode == MODE_CLT_LAT && oper_state == 3);
		$display("[C,L 3] Dropping frame ...");
		let pkt <- rx_port.pkg.get();
		if (pkt.last) begin
			oper_state <= 2;
		end
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
