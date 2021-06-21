package TCP_perf_PE;

import DReg :: *;
import GetPut :: *;
import BlueAXI :: *;
import Vector:: * ;
import BlueLib :: *;
import NetworkLib :: *;

// **************** config ****************
typedef 1024 USER_API_WIDTH; // 128 byte

// axis data ports to nw kernel
`ifdef DWIDTH
typedef `DWIDTH DATA_WIDTH;
`else
typedef 512 DATA_WIDTH; // 64 byte
`endif

// axi control interface
typedef 12 CONFIG_ADDR_WIDTH;
typedef 64 CONFIG_DATA_WIDTH;

// approx simulator frequency
// Integer f_sys = 200;

// design frequency
Integer f_sys = 250 * 10**6;

// timeout in seconds after which TCP con is closed
Integer close_timeout_sec = 3;
// ****************************************

typedef enum {
	MODE_SRV_THR     = 0,
	MODE_CLT_THR     = 1,
	MODE_SRV_LAT     = 2,
	MODE_CLT_LAT     = 3,
	MODE_CLT_LAT_HS  = 4,
	MODE_SRV_LAT_UDP = 5,
	MODE_CLT_LAT_UDP = 6,
	MODE_SRV_THR_UDP = 7,
	MODE_CLT_THR_UDP = 8
} OperMode_t deriving(Bits, Eq, FShow);

interface TCP_perf_PE;
	(*prefix="s_axi_AXILiteS"*) interface AXI4_Lite_Slave_Rd_Fab#(CONFIG_ADDR_WIDTH, CONFIG_DATA_WIDTH) s_rd;
	(*prefix="s_axi_AXILiteS"*) interface AXI4_Lite_Slave_Wr_Fab#(CONFIG_ADDR_WIDTH, CONFIG_DATA_WIDTH) s_wr;

	(*prefix=""*) interface TCP_fab#(DATA_WIDTH) tcp_fab;
	(*prefix=""*) interface UDP_fab#(DATA_WIDTH) udp_fab;
	(*always_enabled*) method bit interrupt;
endinterface

(* default_reset = "ap_rst_n", clock_prefix = "ap_clk" *)
module mkTCP_perf_PE(TCP_perf_PE);

	NetworkLib#(USER_API_WIDTH, DATA_WIDTH) nw <- mkNetworkLib(NW_config {
		udp_rx    : True,
		tcp_tx    : True,
		tcp_rx    : True,
		tcp_open  : True,
		tcp_listen: True
	});

	Integer to_init = f_sys * close_timeout_sec;

	/* payload for throughput test */
	Vector#(16, Bit#(64)) payload_vec;
	for (Integer i=0; i<16; i=i+1) payload_vec[i] = {48'hDEADBEEFABCD, fromInteger(i)};
	Bit#(USER_API_WIDTH) word_payload = pack(payload_vec);

	/* 32byte ping payload for latency test */
	Bit#(256) ping_payload = pack(take(payload_vec));
	Bit#(16) ping_payload_len = fromInteger(toByteLen(ping_payload));

	// hardcoded open UDP port
	Integer udp_src_port = 5001;

	Reg#(Bit#(64)) timeout <- mkReg(fromInteger(to_init));
	Reg#(Session_t) sessionID <- mkRegU();
	Reg#(OperMode_t) oper_mode <- mkRegU;
	Reg#(Bit#(3)) oper_state <- mkReg(0);
	Reg#(bit) r_interrupt <- mkDReg(0);

	Reg#(Bit#(16)) curr_session <- mkReg(0);
	Reg#(Bit#(64)) received_amount <- mkReg(0);
	Reg#(Bit#(64)) sent_bytes <- mkReg(0);
	Reg#(Bit#(16)) rem_in_pkt <- mkReg(0);
	Reg#(Bit#(64)) clk <- mkReg(0);
	PulseWire reset_clk <- mkPulseWire();

	/* control interface */
	Reg#(Bool) ctrl_start <- mkDReg(False);
	Reg#(Bit#(64)) ctrl_ret <- mkReg(0);
	Reg#(Bit#(32)) ctrl_cmd <- mkReg(0);
	Reg#(Bit#(16)) ctrl_num_sess <- mkReg(0);
	Reg#(Bit#(32)) ctrl_dstIp <- mkReg(make_ip(10, 3, 3, 55));
	Reg#(Bit#(64)) ctrl_length <- mkReg(1000);
	Reg#(Bit#(16)) ctrl_dstPort <- mkReg(6666);

	List#(RegisterOperator#(axiAddrWidth, CONFIG_DATA_WIDTH)) operators = Nil;
	operators = registerHandler('h00, ctrl_start, operators);
	operators = registerHandler('h10, ctrl_ret, operators);
	operators = registerHandler('h20, ctrl_cmd, operators);     // tapasco param 1
	operators = registerHandler('h30, ctrl_dstIp, operators);   // tapasco param 2
	operators = registerHandler('h40, ctrl_dstPort, operators); // tapasco param 3
	operators = registerHandler('h50, ctrl_length, operators);  // tapasco param 4
	operators = registerHandler('h60, ctrl_num_sess, operators);  // tapasco param 5
	GenericAxi4LiteSlave#(CONFIG_ADDR_WIDTH, CONFIG_DATA_WIDTH) s_config <- mkGenericAxi4LiteSlave(operators, 1, 1);

	function Action send_triplet(String s);
	action
		Bit#(32) data = pack(str2byteVec(strConcat(s, "\n")));
		nw.udp.setMeta(5566, 7777, ctrl_dstIp, 4);
		nw.udp.putData(extend(data), 4);
	endaction
	endfunction

	rule arbiter(oper_state == 0 && ctrl_start);
		OperMode_t m = unpack(truncate(ctrl_cmd));
		$display("[Arbiter] Starting mode=%d : ", m, fshow(m));

		oper_mode <= m;
		oper_state <= 1;

		// indicate
		send_triplet("ok0");
	endrule

	rule r_clk;
		clk <= reset_clk ? 0 : (clk + 1);
	endrule

	/****************************************************************
	 * SERVER - throughput
	 ***************************************************************/
	rule re0(oper_mode == MODE_SRV_THR && oper_state == 1);
		$display("[S,T 1] Opening port %d", ctrl_dstPort);
		nw.tcp.listenPorts(1, ctrl_dstPort);
		send_triplet("ok1");

		oper_state <= 2;
	endrule

	rule re1(oper_mode == MODE_SRV_THR && oper_state == 2);
		if (nw.tcp.isListening()) begin
			$display("[S,T 2] Port now open, listening for connection ...");
			send_triplet("ok2");

			oper_state <= 3;
		end
	endrule

	/************** First packet **************/

	rule re2(oper_mode == MODE_SRV_THR && oper_state == 3);
		DataRX#(USER_API_WIDTH) r <- nw.tcp.datawordRx.get();
		$display("[S,T 3] Got first data for session %d: %d of %d bytes. Starting timer", r.sessionID, r.word_len, r.pkt_len);

		if (r.isLast) begin
			// Assumption: Tfer > 1 word
			oper_state <= 4;
		end else begin
			oper_state <= 5;
		end

		received_amount <= extend(r.word_len);
		reset_clk.send();
	endrule

	(*descending_urgency="re2,re2_cl"*)
	rule re2_cl(oper_mode == MODE_SRV_THR && oper_state == 3);
		let s <- nw.tcp.closedByClient.get();
		$display("[S,T 3b] Session %d was closed by remote client", s);
		send_triplet("ok3");

		oper_state <= 0;
	endrule

	/************** Subsequent start of packets / closed con / restart **************/

	(* descending_urgency="re3,re3_cl,re3_restart" *)
	rule re3(oper_mode == MODE_SRV_THR && oper_state == 4);
		DataRX#(USER_API_WIDTH) r <- nw.tcp.datawordRx.get();
		$display("[S,T 4a] Got new data for session %d: %d of %d bytes. (total=%d, clk=%d)",
			r.sessionID, r.word_len, r.pkt_len, received_amount, clk);

		if (r.isLast) begin
			if (received_amount + extend(r.word_len) >= ctrl_length) begin
				$display("--> Tfer done! expected=%d, got=%d, final clk=%d",
					ctrl_length, received_amount + extend(r.word_len), clk);

				oper_state <= 3;
				r_interrupt <= 1;
				ctrl_ret <= clk;
			end
		end else begin
			oper_state <= 5;
		end

		received_amount <= received_amount + extend(r.word_len);
	endrule

	rule re3_cl(oper_mode == MODE_SRV_THR && oper_state == 4);
		let s <- nw.tcp.closedByClient.get();
		$display("[S,T 4b] Session %d was closed by remote client", s);
		send_triplet("ok4");

		oper_state <= 0;
		r_interrupt <= 1;
		ctrl_ret <= 0;
	endrule

	rule re3_restart(oper_mode == MODE_SRV_THR && oper_state == 4 && ctrl_start);
		$display("[S,T 4c] Restarting");

		oper_state <= 3;
	endrule

	/************** Send Continuation **************/

	rule re4(oper_mode == MODE_SRV_THR && oper_state == 5);
		DataRX#(USER_API_WIDTH) r <- nw.tcp.datawordRx.get();
		$display("[S,T 5] Continued send in session %d: %d bytes (total=%d, clk=%d, last=%d)",
			r.sessionID, r.word_len, received_amount, clk, r.isLast);

		received_amount <= received_amount + extend(r.word_len);

		if (r.isLast) begin
			if (received_amount + extend(r.word_len) >= ctrl_length) begin
				$display("--> Tfer done! expected=%d, got=%d, final clk=%d",
					ctrl_length, received_amount + extend(r.word_len), clk);

				oper_state <= 3;
				r_interrupt <= 1;
				ctrl_ret <= clk;
			end else begin
				oper_state <= 4;
			end
		end
	endrule


	/****************************************************************
	 * CLIENT - throughput
	 ***************************************************************/
	rule r0(oper_mode == MODE_CLT_THR && oper_state == 1);
		$display("[C,T 1] Opening connection to %x:%d", ctrl_dstIp, ctrl_dstPort);
		nw.tcp.openConnections(ctrl_num_sess, ctrl_dstIp, ctrl_dstPort);

		oper_state <= 2;
	endrule

	rule r1(oper_mode == MODE_CLT_THR && oper_state == 2);
		let id <- nw.tcp.sessionIDs.get();
		$display("[C,T 2] Opened session %d", id);

		if (curr_session == ctrl_num_sess - 1) begin
			oper_state <= 3;
			curr_session <= 0;

			ctrl_ret <= 555;
			r_interrupt <= 1;
		end else begin
			curr_session <= curr_session + 1;
		end
	endrule

	/************** first word **************/

	rule r2(oper_mode == MODE_CLT_THR && oper_state == 3 && ctrl_start);
		Bit#(16) word_sz = truncate(min(fromInteger(valueOf(USER_API_WIDTH))/8, ctrl_length));

		$display("[C,T 3] Announce tfer (sz %d), put %d bytes for sess %d, starting clock", ctrl_length, word_sz, curr_session);
		nw.tcp.announceTfer(curr_session, ctrl_length);
		nw.tcp.putData(word_payload, word_sz);
		reset_clk.send();

		if (ctrl_length - extend(word_sz) == 0) begin
			$display("--> Tfer done: %d bytes sent", word_sz);
			oper_state <= 5;
			timeout <= fromInteger(to_init);
		end else begin
			sent_bytes <= extend(word_sz);
			oper_state <= 4;
		end
	endrule

	/************** subsequent words **************/

	rule pkt_arbiter(oper_mode == MODE_CLT_THR && oper_state == 4);
		Session_t next_sess = ?;
		if (curr_session == ctrl_num_sess - 1) begin
			next_sess = 0;
		end else begin
			next_sess = curr_session + 1;
		end

		nw.tcp.overrideSession(next_sess);
		curr_session <= next_sess;
	endrule

	rule r3(oper_mode == MODE_CLT_THR && oper_state == 4);
		Bit#(64) remaining_in_tfer = ctrl_length - sent_bytes;
		Bit#(16) word_len = truncate(min(fromInteger(valueOf(USER_API_WIDTH))/8, remaining_in_tfer));

		$display("[C,T 4] Continued send for sess %d: %d bytes, %d rem in tfer, clk=%d",
			curr_session, word_len, remaining_in_tfer, clk);

		nw.tcp.putData(word_payload, word_len);

		sent_bytes <= sent_bytes + extend(word_len);

		if (remaining_in_tfer - extend(word_len) == 0) begin
			$display("--> Tfer done: %d bytes sent", sent_bytes+extend(word_len));
			oper_state <= 5;
			ctrl_ret <= clk;
			r_interrupt <= 1;
			timeout <= fromInteger(to_init);
			curr_session <= 0;
		end
	endrule

	/************** close **************/

	rule r4(oper_mode == MODE_CLT_THR && oper_state == 5);
		if (timeout == 0) begin

			$display("[C,T 5] Closing session %d", curr_session);
			nw.tcp.closeConnection(curr_session);

			if (curr_session == ctrl_num_sess - 1) begin
				$display("--> All closed");
				curr_session <= 0;
				sent_bytes <= 0;
				oper_state <= 0;
			end else begin
				curr_session <= curr_session + 1;
			end
		end else begin
			timeout <= timeout - 1;
		end
	endrule


	/****************************************************************
	 * SERVER - latency
	 ***************************************************************/
	rule s_l_0(oper_mode == MODE_SRV_LAT && oper_state == 1);
		$display("[S,L 1] Opening port %d", ctrl_dstPort);
		nw.tcp.listenPorts(1, ctrl_dstPort);
		send_triplet("ok1");

		oper_state <= 2;
	endrule

	rule s_l_1(oper_mode == MODE_SRV_LAT && oper_state == 2);
		if (nw.tcp.isListening()) begin
			$display("[S,L 2] Port now open, listening for connection ...");
			send_triplet("ok2");

			oper_state <= 3;
		end
	endrule

	rule s_l_2(oper_mode == MODE_SRV_LAT && oper_state == 3);
		DataRX#(USER_API_WIDTH) r <- nw.tcp.datawordRx.get();
		$display("[S,L 3] Got data for session %d: Echoing %d of %d bytes", r.sessionID, r.word_len, r.pkt_len);

		// announce and echo back
		nw.tcp.announceTfer(r.sessionID, extend(r.pkt_len));
		nw.tcp.putData(r.data, r.word_len);

		if (r.isLast) begin
			r_interrupt <= 1;
		end
	endrule

	(*descending_urgency="s_l_2,s_l_2_cl"*)
	rule s_l_2_cl(oper_mode == MODE_SRV_LAT && oper_state == 3);
		let s <- nw.tcp.closedByClient.get();
		$display("[S,L 3b] Session %d was closed by remote client", s);
		send_triplet("ok3");

		oper_state <= 0;
	endrule


	/****************************************************************
	 * CLIENT - latency
	 ***************************************************************/
	function Bool isLatencyClient(OperMode_t m);
		return m == MODE_CLT_LAT || m == MODE_CLT_LAT_HS;
	endfunction

	rule c_l_0(isLatencyClient(oper_mode) && oper_state == 1);
		$display("[S,C 1] Opening connection to %x:%d", ctrl_dstIp, ctrl_dstPort);
		nw.tcp.openConnections(1, ctrl_dstIp, ctrl_dstPort);

		if (oper_mode == MODE_CLT_LAT_HS) begin
			// Mode: measure latency including TCP handshake
			$display("--> starting clk");
			reset_clk.send();
		end

		oper_state <= 2;
	endrule

	rule c_l_1(isLatencyClient(oper_mode) && oper_state == 2);
		let id <- nw.tcp.sessionIDs.get();
		$display("[S,C 2] Opened session %d", id);

		if (oper_mode == MODE_CLT_LAT) begin
			// signal to runtime that user can start the test now
			ctrl_ret <= 12345;
			r_interrupt <= 1;
		end

		sessionID <= id;
		oper_state <= 3;
	endrule

	rule c_l_2(isLatencyClient(oper_mode) && oper_state == 3);
		if ((oper_mode == MODE_CLT_LAT && ctrl_start) || oper_mode == MODE_CLT_LAT_HS) begin
			$display("[S,C 3]  Sending Ping");
			nw.tcp.announceTfer(sessionID, extend(ping_payload_len));
			nw.tcp.putData(extend(ping_payload), ping_payload_len);

			if (oper_mode == MODE_CLT_LAT) begin
				// Mode: measure latency without handshake
				$display("--> starting clk");
				reset_clk.send();
			end

			oper_state <= 4;
		end
	endrule

	rule c_l_4(isLatencyClient(oper_mode) && oper_state == 4);
		DataRX#(USER_API_WIDTH) pkt <- nw.tcp.datawordRx.get();
		$display("[S,C 4]  Received reply, len=%d", pkt.word_len);

		if (pkt.word_len == ping_payload_len && truncate(pkt.data) == ping_payload) begin
			$display("--> payload matches! clk=%d", clk);
			ctrl_ret <= clk + 1;
		end else begin
			$display("--> MISMATCH! clk=%d, is=%x, expected=%x", clk, pkt.data, ping_payload);
			ctrl_ret <= 0;
		end

		if (oper_mode == MODE_CLT_LAT) begin
			oper_state <= 3;
		end else begin
			$display("--> Closing session %d", sessionID);
			nw.tcp.closeConnection(sessionID);
			oper_state <= 0;
		end

		r_interrupt <= 1;
	endrule


	/****************************************************************
	 * SERVER - latency - UDP
	 ***************************************************************/
	rule s_lu_0(oper_mode == MODE_SRV_LAT_UDP && oper_state == 1);
		$display("[S,L,U 1] Echoing packet", ctrl_dstPort);

		let m <- nw.udp.getMeta();
		nw.udp.setMeta(m.dst_port, m.src_port, m.their_addr, m.length);

		// assumption: single word packet
		let d <- nw.udp.datawordRx.get();
		nw.udp.putData(d.data, d.word_bytes);

		ctrl_ret <= 56789;
		r_interrupt <= 1;
	endrule


	/****************************************************************
	 * CLIENT - latency - UDP
	 ***************************************************************/
	rule c_lu_0(oper_mode == MODE_CLT_LAT_UDP && oper_state == 1);
		if (ctrl_start) begin
			$display("[S,C,U 1] Sending Ping, starting clk");

			nw.udp.setMeta(fromInteger(udp_src_port), ctrl_dstPort, ctrl_dstIp, ping_payload_len);
			nw.udp.putData(extend(ping_payload), ping_payload_len);

			reset_clk.send();
			oper_state <= 2;
		end
	endrule

	rule c_lu_1(oper_mode == MODE_CLT_LAT_UDP && oper_state == 2);
		let m <- nw.udp.getMeta();
		let d <- nw.udp.datawordRx.get();
		$display("[S,C,U 2]  Received reply, len=%d", m.length);

		if (m.length == ping_payload_len && truncate(d.data) == ping_payload) begin
			$display("--> payload matches! clk=%d", clk);
			ctrl_ret <= clk + 1;
		end else begin
			$display("--> MISMATCH! clk=%d, is=%x, expected=%x", clk, d.data, ping_payload);
			ctrl_ret <= 0;
		end

		r_interrupt <= 1;
		oper_state <= 1;
	endrule	


	/****************************************************************
	 * SERVER - throughput - UDP
	 ***************************************************************/
	rule r_tu_s_1(oper_mode == MODE_SRV_THR_UDP && oper_state == 1);
		let m <- nw.udp.getMeta();
		let d <- nw.udp.datawordRx.get();
		$display("[S,T,U 1] Got first word in pkt: %d of %d bytes", d.word_bytes, m.length);

		if (d.last_word) begin
			oper_state <= 3;
		end else begin
			oper_state <= 2;
		end

		if (received_amount == 0) begin
			$display("--> first word, starting timer");
			reset_clk.send();
		end

		received_amount <= received_amount + extend(d.word_bytes);
	endrule

	rule r_tu_s_2(oper_mode == MODE_SRV_THR_UDP && oper_state == 2);
		let d <- nw.udp.datawordRx.get();
		$display("[S,T,U 2] Got %d bytes. (total=%d, clk=%d)", d.word_bytes, received_amount, clk);

		if (d.last_word) begin
			$display("--> Pkt done");
			oper_state <= 3;
		end

		received_amount <= received_amount + extend(d.word_bytes);
	endrule

	rule r_tu_s_3(oper_mode == MODE_SRV_THR_UDP && oper_state == 3);
		if (received_amount >= ctrl_length) begin
			$display("[S,T,U 3]  Tfer done! expected=%d, got=%d, final clk=%d", ctrl_length, received_amount, clk);

			oper_state <= 0;
			received_amount <= 0;
			r_interrupt <= 1;
			ctrl_ret <= clk;
		end else begin
			$display("[S,T,U 3] Tfer not done, continuing");
			oper_state <= 1;
		end
	endrule


	/****************************************************************
	 * CLIENT - throughput - UDP
	 ***************************************************************/
	rule r_tu_c_1(oper_mode == MODE_CLT_THR_UDP && oper_state == 1);
		Bit#(16) word_sz = fromInteger(valueOf(USER_API_WIDTH)/8);
		$display("[C,T,U 1] Starting new pkt, sending %d bytes", word_sz);

		nw.udp.setMeta(fromInteger(udp_src_port), fromInteger(udp_src_port), ctrl_dstIp, fromInteger(mss));
		nw.udp.putData(word_payload, word_sz);

		if (sent_bytes == 0) begin
			$display("--> First word in tfer, starting clk");
			reset_clk.send();
		end

		oper_state <= 2;
		rem_in_pkt <= fromInteger(mss) - word_sz;
		sent_bytes <= sent_bytes + extend(word_sz);
	endrule

	rule r_tu_c_2(oper_mode == MODE_CLT_THR_UDP && oper_state == 2);
		Bit#(16) word_sz = fromInteger(valueOf(USER_API_WIDTH)/8);
		$display("[C,T,U 2] Continued send: %d bytes, %d rem, clk=%d", word_sz, rem_in_pkt, clk);

		nw.udp.putData(word_payload, word_sz);

		if (rem_in_pkt == word_sz) begin
			$display("--> Pkt done");
			oper_state <= 3;
		end

		rem_in_pkt <= rem_in_pkt - word_sz;
		sent_bytes <= sent_bytes + extend(word_sz);
	endrule

	rule r_tu_c_3(oper_mode == MODE_CLT_THR_UDP && oper_state == 3);
		$display("[C,T,U 3] %d bytes sent, %d requested", sent_bytes, ctrl_length);
		if (sent_bytes >= ctrl_length) begin
			$display("--> Tfer done: final clk=%d", clk);
			oper_state <= 0;
			sent_bytes <= 0;
			ctrl_ret <= clk;
			r_interrupt <= 1;
		end else begin
			$display("--> not done, continuing");
			oper_state <= 1;
		end
	endrule


	/****************************************************************
	 * Interfaces
	 ***************************************************************/
	interface tcp_fab = nw.tcp.fab;
	interface udp_fab = nw.udp.fab;

	interface s_rd = s_config.s_rd;
	interface s_wr = s_config.s_wr;

	method bit interrupt = r_interrupt;
endmodule
endpackage
