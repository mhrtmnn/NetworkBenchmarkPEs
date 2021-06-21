package Testbench;

import ClientServer :: *;
import Connectable :: *;
import StmtFSM :: *;
import GetPut :: *;
import Vector :: *;
import GetPut :: *;

import NetworkLib :: *;
import BlueAXI :: *;
import BlueLib :: *;

typedef 512 USER_WIDTH;
typedef 512 DATA_WIDTH;


interface TestHandler;
	method Action go();
	method Bool done();
endinterface

(* synthesize *)
module [Module] mkTestbench();
	let c = NW_config {
		udp_rx: True,
		tcp_tx: True,
		tcp_rx: True,
		tcp_open: True,
		tcp_listen: True
	};
	NetworkLib#(USER_WIDTH, DATA_WIDTH) dut <- mkNetworkLib(c);

	TestHandler udp_test <- mkUdpTest(dut.udp);
	TestHandler tcp_test <- mkTcpTest(dut.tcp);

	Stmt s = {
	seq
		printColorTimed(GREEN, $format("Starting UDP test"));
		udp_test.go();
		await(udp_test.done());

		$display("\n\n\n");

		printColorTimed(GREEN, $format("Starting TCP test"));
		tcp_test.go();
		await(tcp_test.done());
	endseq
	};
	mkAutoFSM(s);
endmodule


/****************************************************************
 * Testbench UDP
 ***************************************************************/
module mkUdpTest#(LibUDP#(user_width, data_width) udp_dut)(TestHandler) provisos (Add#(a__, 64, data_width));
	AXI4_Stream_Wr#(data_width, AXI_USER_WIDTH) udp_in 				<- mkAXI4_Stream_Wr(cfg_buf_len);
	AXI4_Stream_Rd#(data_width, AXI_USER_WIDTH) udp_out 			<- mkAXI4_Stream_Rd(cfg_buf_len);
	AXI4_Stream_Wr#(UDP_META_WIDTH, AXI_USER_WIDTH) udp_meta_in 	<- mkAXI4_Stream_Wr(cfg_buf_len);
	AXI4_Stream_Rd#(UDP_META_WIDTH, AXI_USER_WIDTH) udp_meta_out 	<- mkAXI4_Stream_Rd(cfg_buf_len);

	mkConnection(udp_in.fab, udp_dut.fab.i_udp_rx);
	mkConnection(udp_dut.fab.i_udp_tx, udp_out.fab);
	mkConnection(udp_meta_in.fab, udp_dut.fab.i_udp_rx_meta);
	mkConnection(udp_dut.fab.i_udp_tx_meta, udp_meta_out.fab);

	// state elements
	Reg#(int) i <- mkRegU;
	Reg#(int) errors <- mkReg(0);
	Wire#(Bool) err1 <- mkDWire(False);
	Wire#(Bool) err2 <- mkDWire(False);

	/*
	 * Helper Function for FSM
	 */
	function Action check_udp_tx_data(Bit#(data_width) data, Bit#(7) byte_len, Bool isLast);
	action
		let raw <- udp_out.pkg.get();
		printColorTimed(BLUE, $format("UDP data out: data=%0x, keep=%0x, last=%0d", raw.data, raw.keep, raw.last));

		if (raw.data != data || raw.last != isLast) begin
			$display("ERR1: is %d, should be %d", raw.last, isLast);
			err1 <= True;
		end
	endaction
	endfunction

	function Action check_udp_tx_meta(Bit#(16) src_port, Bit#(16) dst_port, Bit#(32) dst_addr, Bit#(16) length);
	action
		let raw <- udp_meta_out.pkg.get();
		// AxisUdpMetaPkg h = unpack(raw.data[valueOf(UDP_META_HEADER_LEN)-1:0]);
		AxisUdpMetaPkg h = unpack(truncate(raw.data));
		printColorTimed(BLUE, $format("UDP meta out: addr=%x, src_port=%0d, dst_port=%0d, length=%0d",
			h.their_addr[31:0], h.my_port, h.their_port, h.length));

		if (h.their_addr[31:0] != dst_addr || h.my_port != src_port || h.their_port != dst_port || h.length != length) begin
			$display("ERR2");
			err2 <= True;
		end
	endaction
	endfunction

	rule err;
		let inc = 0;
		if (err1) inc = inc + 1;
		if (err2) inc = inc + 1;
		errors <= errors + inc;
	endrule

	Stmt s = {
	seq
		printColorTimed(YELLOW, $format("Testing UDP packet send (10 bytes)"));
		action
			udp_dut.setMeta(16'd456, 16'd654, 32'h55667788, 16'd10);
			udp_dut.putData('h112233445566778899aa, 'd10);
		endaction

		// *** check output ***
		action
			printColorTimed(YELLOW, $format("Check 1.1"));
			check_udp_tx_meta(16'd456, 16'd654, 32'h88776655, 16'd10);
			check_udp_tx_data('h112233445566778899aa, 10, True);
		endaction
		for (i<=0; i<50; i<=i+1) seq
			noAction;
		endseq

		// ***********************************

		printColorTimed(YELLOW, $format("Testing UDP packet send (70 bytes)"));
		action
			printColorTimed(YELLOW, $format("--> 64 / 70 bytes"));
			udp_dut.setMeta(16'd456, 16'd654, 32'h55667788, 70);
			udp_dut.putData('1, 64);
		endaction
		action
			printColorTimed(YELLOW, $format("--> 6 / 70 bytes"));
			udp_dut.putData('h1122, 6);
		endaction
		for (i<=0; i<50; i<=i+1) seq
			noAction;
		endseq

		// *** check output ***
		action
			printColorTimed(YELLOW, $format("Check 2.1"));
			check_udp_tx_meta(16'd456, 16'd654, 32'h88776655, 70);
			check_udp_tx_data('1, 64, False);
		endaction
		action
			printColorTimed(YELLOW, $format("Check 2.2"));
			check_udp_tx_data('h1122, 6, True);
		endaction
		for (i<=0; i<50; i<=i+1) seq
			noAction;
		endseq

		// ***********************************

		printColorTimed(YELLOW, $format("Testing UDP packet send (13 bytes)"));
		action
			udp_dut.setMeta(16'd123, 16'd321, 32'h11223344, 16'd13);
			udp_dut.putData('hDEADBEEF112233, 13);
		endaction

		// *** check output ***
		action
			printColorTimed(YELLOW, $format("Check 3.1"));
			check_udp_tx_meta(16'd123, 16'd321, 32'h44332211, 16'd13);
			check_udp_tx_data('hDEADBEEF112233, 13, True);
		endaction
		for (i<=0; i<50; i<=i+1) seq
			noAction;
		endseq

		// ***********************************

		printColorTimed(YELLOW, $format("Testing UDP packet receive"));
		action
			Bit#(32) addr = 'haabbccdd;

			Bit#(UDP_META_HEADER_LEN) enc_header = pack(AxisUdpMetaPkg {
				their_addr: {addr,addr,addr,addr},
				their_port: 1111,
				my_port: 2222,
				length: 10 + 8
			});
			udp_meta_in.pkg.put(mkPkgWithKeep(enc_header, True));

			Bit#(64) payload = 'h288;
			udp_in.pkg.put(mkPkgWithKeep(payload, False));
		endaction

		// *** check output ***

		action
			let p <- udp_dut.getMeta();
			let d <- udp_dut.datawordRx.get();

			printColorTimed(YELLOW, $format("Meta: addr=%x,srcPort=%d,dstPort=%d,len=%d", 
				p.their_addr[31:0],
				p.src_port, p.dst_port,
				p.length));

			printColorTimed(YELLOW, $format("Data: data=%0x, bytes_in_word=%d, last=%d",
				d.data, d.word_bytes, d.last_word));
		endaction

		// ***********************************

		if (errors > 0)
			printColorTimed(RED, $format("UDP test failed!"));
		else
			printColorTimed(GREEN, $format("UDP test succeeded!"));
	endseq
	};
	FSM testFSM <- mkFSM(s);

	method Action go();
		testFSM.start();
	endmethod

	method Bool done();
		return testFSM.done();
	endmethod
endmodule


/****************************************************************
 * Testbench TCP
 ***************************************************************/
module mkTcpTest#(LibTCP#(user_width, data_width) tcp_dut)(TestHandler);
	AXI4_Stream_Rd #(TCP_LISTEN_PORT_WIDTH, AXI_USER_WIDTH) listen_port <- mkAXI4_Stream_Rd(cfg_buf_len);
	AXI4_Stream_Wr #(TCP_PORT_STATUS_WIDTH, AXI_USER_WIDTH) port_status <- mkAXI4_Stream_Wr(cfg_buf_len);

	AXI4_Stream_Rd #(TCP_OPEN_CONN_WIDTH, AXI_USER_WIDTH) open_connection <- mkAXI4_Stream_Rd(cfg_buf_len);
	AXI4_Stream_Wr #(TCP_OPEN_STATUS_WIDTH, AXI_USER_WIDTH) open_status <- mkAXI4_Stream_Wr(cfg_buf_len);
	AXI4_Stream_Rd #(TCP_CLOSE_CONN_WIDTH, AXI_USER_WIDTH) close_connection <- mkAXI4_Stream_Rd(cfg_buf_len);

	AXI4_Stream_Wr #(TCP_NOTIFICATION_WIDTH, AXI_USER_WIDTH) notification <- mkAXI4_Stream_Wr(cfg_buf_len);
	AXI4_Stream_Rd #(TCP_READ_PKG_WIDTH, AXI_USER_WIDTH) read_pkg <- mkAXI4_Stream_Rd(cfg_buf_len);

	AXI4_Stream_Wr #(TCP_RX_META_WIDTH, AXI_USER_WIDTH) rx_meta <- mkAXI4_Stream_Wr(cfg_buf_len);
	AXI4_Stream_Wr #(data_width, AXI_USER_WIDTH) rx_data <- mkAXI4_Stream_Wr(cfg_buf_len);

	AXI4_Stream_Rd #(TCP_TX_META_WIDTH, AXI_USER_WIDTH) tx_meta <- mkAXI4_Stream_Rd(cfg_buf_len);
	AXI4_Stream_Rd #(data_width, AXI_USER_WIDTH) tx_data <- mkAXI4_Stream_Rd(cfg_buf_len);
	AXI4_Stream_Wr #(TCP_TX_STATUS_WIDTH, AXI_USER_WIDTH) tx_status <- mkAXI4_Stream_Wr(cfg_buf_len);

	mkConnection(tcp_dut.fab.i_listen_port, listen_port.fab);
	mkConnection(port_status.fab, tcp_dut.fab.i_port_status);
	mkConnection(tcp_dut.fab.i_open_connection, open_connection.fab);
	mkConnection(open_status.fab, tcp_dut.fab.i_open_status);
	mkConnection(tcp_dut.fab.i_close_connection, close_connection.fab);
	mkConnection(notification.fab, tcp_dut.fab.i_notification);
	mkConnection(tcp_dut.fab.i_read_pkg, read_pkg.fab);
	mkConnection(rx_meta.fab, tcp_dut.fab.i_rx_meta);
	mkConnection(rx_data.fab, tcp_dut.fab.i_rx_data);
	mkConnection(tcp_dut.fab.i_tx_meta, tx_meta.fab);
	mkConnection(tcp_dut.fab.i_tx_data, tx_data.fab);
	mkConnection(tx_status.fab, tcp_dut.fab.i_tx_status);

	// state elements
	Reg#(int) i <- mkRegU;
	Reg#(int) errors <- mkReg(0);

	Vector#(10, Reg#(Session_t)) sessionIDs <- replicateM(mkRegU);
	// BRAM_PORT#(Bit#(16), Session_t) session_bram <- mkBRAMCore1(max_sessions, True);

	/*
	 * Test FSM
	 */
	Stmt s = {
	seq
		// open 10 sessions
		tcp_dut.openConnections(10, 'h11223344, 2222);
		for (i<=0; i<10; i<=i+1) par
			action
				AxisTcpOpenConn p <- parse_from(open_connection);
				printColorTimed(YELLOW, $format("Got open conn: ip=%x, port=%d", p.ipAddress, p.basePort));
			endaction
			action
				Bit#(TCP_OPEN_STATUS_LEN) enc_pkg = pack(AxisTcpOpenStatus {
					sessionID: 303,
					success: True
				});
				open_status.pkg.put(mkPkgWithKeep(enc_pkg, True));
			endaction
			action
				let id <- tcp_dut.sessionIDs.get();
				printColorTimed(YELLOW, $format("Got session: %d", id));
				sessionIDs[i] <= id;
			endaction
		endpar

		// announce and send data for first session
		tcp_dut.announceTfer(sessionIDs[0], 31);
		action
			let p <- tx_meta.pkg.get();
			printColorTimed(YELLOW, $format("Got tx meta: %d", p));
		endaction
		action
			Bit#(TCP_TX_STATUS_LEN) enc_pkg = pack(AxisTcpTxStatus {
				sessionID: 0,
				length: 0,
				remainingSpace: 0,
				error: NO_ERROR
			});
			tx_status.pkg.put(mkPkgWithKeep(enc_pkg, True));
		endaction
		tcp_dut.putData('hDEADBEEF, 31);


		// open 10 ports
		tcp_dut.listenPorts(10, 1000);
		for (i<=0; i<10; i<=i+1) par
			action
				AxisTcpListenPort p <- parse_from(listen_port);
				printColorTimed(YELLOW, $format("Got open port=%d", p.basePort));
			endaction
			action
				port_status.pkg.put(mkPkg(1, 1, True));
			endaction
		endpar

		// anounce incoming data
		action
			Bit#(TCP_NOTIFICATION_LEN) enc_pkg = pack(AxisTcpNotification {
				sessionID: 123,
				length: 31,
				ipAddress: 0,
				dstPort: 2222,
				closed: False
			});
			notification.pkg.put(mkPkgWithKeep(enc_pkg, True));
		endaction

		// put data
		action
			let p <- read_pkg.pkg.get();
			printColorTimed(YELLOW, $format("Got read_pkg: %d", p));
		endaction
		action
			Bit#(TCP_RX_META_LEN) enc_pkg = pack(AxisTcpRxMeta {
				sessionID: 123
			});
			rx_meta.pkg.put(mkPkgWithKeep(enc_pkg, True));
		endaction
		rx_data.pkg.put(mkPkg(123, 32, False));
		rx_data.pkg.put(mkPkg(456, 32, True));

		// get data
		action
			let d <- tcp_dut.datawordRx.get();
			printColorTimed(YELLOW, $format("Got data 1: %d", d.data));
		endaction
		action
			let d <- tcp_dut.datawordRx.get();
			printColorTimed(YELLOW, $format("Got data 2: %d", d.data));
		endaction

		// TODO: check results
		if (errors > 0)
			printColorTimed(RED, $format("TCP test failed!"));
		else
			printColorTimed(GREEN, $format("TCP test succeeded!"));
	endseq
	};
	FSM testFSM <- mkFSM(s);

	method Action go();
		testFSM.start();
	endmethod

	method Bool done();
		return testFSM.done();
	endmethod
endmodule

endpackage
