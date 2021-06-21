package Testbench;
	import ClientServer :: *;
	import Connectable :: *;
	import StmtFSM :: *;
	import GetPut :: *;
	import Vector :: *;
	import GetPut :: *;

	import BlueAXI :: *;
	import BlueLib :: *;

	import NetworkLib :: *;
	import TCP_perf_PE :: *;

	(* synthesize *)
	module [Module] mkTestbench();

		Reg#(int) i <- mkRegU();
		Reg#(int) j <- mkRegU();
		TCP_perf_PE dut <- mkTCP_perf_PE();

		// UDP
		AXI4_Stream_Wr#(DATA_WIDTH, AXI_USER_WIDTH) udp_in          	<- mkAXI4_Stream_Wr(cfg_buf_len);
		AXI4_Stream_Rd#(DATA_WIDTH, AXI_USER_WIDTH) udp_out         	<- mkAXI4_Stream_Rd(cfg_buf_len);
		AXI4_Stream_Wr#(UDP_META_WIDTH, AXI_USER_WIDTH) udp_meta_in     <- mkAXI4_Stream_Wr(cfg_buf_len);
		AXI4_Stream_Rd#(UDP_META_WIDTH, AXI_USER_WIDTH) udp_meta_out    <- mkAXI4_Stream_Rd(cfg_buf_len);

		mkConnection(udp_in.fab, dut.udp_fab.i_udp_rx);
		mkConnection(dut.udp_fab.i_udp_tx, udp_out.fab);
		mkConnection(udp_meta_in.fab, dut.udp_fab.i_udp_rx_meta);
		mkConnection(dut.udp_fab.i_udp_tx_meta, udp_meta_out.fab);

		// TCP
		AXI4_Stream_Rd #(TCP_LISTEN_PORT_WIDTH, AXI_USER_WIDTH) listen_port <- mkAXI4_Stream_Rd(cfg_buf_len);
		AXI4_Stream_Wr #(TCP_PORT_STATUS_WIDTH, AXI_USER_WIDTH) port_status <- mkAXI4_Stream_Wr(cfg_buf_len);
		AXI4_Stream_Rd #(TCP_OPEN_CONN_WIDTH, AXI_USER_WIDTH) open_connection <- mkAXI4_Stream_Rd(cfg_buf_len);
		AXI4_Stream_Wr #(TCP_OPEN_STATUS_WIDTH, AXI_USER_WIDTH) open_status <- mkAXI4_Stream_Wr(cfg_buf_len);
		AXI4_Stream_Rd #(TCP_CLOSE_CONN_WIDTH, AXI_USER_WIDTH) close_connection <- mkAXI4_Stream_Rd(cfg_buf_len);
		AXI4_Stream_Wr #(TCP_NOTIFICATION_WIDTH, AXI_USER_WIDTH) notification <- mkAXI4_Stream_Wr(cfg_buf_len);
		AXI4_Stream_Rd #(TCP_READ_PKG_WIDTH, AXI_USER_WIDTH) read_pkg <- mkAXI4_Stream_Rd(cfg_buf_len);
		AXI4_Stream_Wr #(TCP_RX_META_WIDTH, AXI_USER_WIDTH) rx_meta <- mkAXI4_Stream_Wr(cfg_buf_len);
		AXI4_Stream_Wr #(DATA_WIDTH, AXI_USER_WIDTH) rx_data <- mkAXI4_Stream_Wr(cfg_buf_len);
		AXI4_Stream_Rd #(TCP_TX_META_WIDTH, AXI_USER_WIDTH) tx_meta <- mkAXI4_Stream_Rd(cfg_buf_len);
		AXI4_Stream_Rd #(DATA_WIDTH, AXI_USER_WIDTH) tx_data <- mkAXI4_Stream_Rd(cfg_buf_len);
		AXI4_Stream_Wr #(TCP_TX_STATUS_WIDTH, AXI_USER_WIDTH) tx_status <- mkAXI4_Stream_Wr(cfg_buf_len);

		mkConnection(dut.tcp_fab.i_listen_port, listen_port.fab);
		mkConnection(port_status.fab, dut.tcp_fab.i_port_status);
		mkConnection(dut.tcp_fab.i_open_connection, open_connection.fab);
		mkConnection(open_status.fab, dut.tcp_fab.i_open_status);
		mkConnection(dut.tcp_fab.i_close_connection, close_connection.fab);
		mkConnection(notification.fab, dut.tcp_fab.i_notification);
		mkConnection(dut.tcp_fab.i_read_pkg, read_pkg.fab);
		mkConnection(rx_meta.fab, dut.tcp_fab.i_rx_meta);
		mkConnection(rx_data.fab, dut.tcp_fab.i_rx_data);
		mkConnection(dut.tcp_fab.i_tx_meta, tx_meta.fab);
		mkConnection(dut.tcp_fab.i_tx_data, tx_data.fab);
		mkConnection(tx_status.fab, dut.tcp_fab.i_tx_status);

		// control
		AXI4_Lite_Master_Wr#(CONFIG_ADDR_WIDTH, CONFIG_DATA_WIDTH) writeMaster <- mkAXI4_Lite_Master_Wr(16);
		AXI4_Lite_Master_Rd#(CONFIG_ADDR_WIDTH, CONFIG_DATA_WIDTH) readMaster <- mkAXI4_Lite_Master_Rd(16);

		mkConnection(writeMaster.fab, dut.s_wr);
		mkConnection(readMaster.fab, dut.s_rd);

		rule intr;
			let i = dut.interrupt();
			if (i == 1) $display("==> INTERRUPT");
		endrule

		rule dumpData;
			let raw <- udp_out.pkg.get();
			printColorTimed(BLUE, $format("Got data: %0x ('%s'), last=%d", raw.data, raw.data, raw.last));
		endrule

		rule dumpMeta;
			AxisUdpMetaPkg p <- parse_from(udp_meta_out);
			printColorTimed(BLUE, $format("Got meta: ", fshow(p)));
		endrule

		Stmt s = {
		seq
			printColorTimed(GREEN, $format("Opening Con"));
			axi4_lite_write(writeMaster, 'h00, 1);
			action
				AXI4_Lite_Response wresp <- axi4_lite_write_response(writeMaster);
				printColorTimed(BLUE, $format("Write resp: %d", wresp));
			endaction
			for (j<=0; j<10; j<=j+1) seq
				noAction;
			endseq
		endseq
		};
		mkAutoFSM(s);
	endmodule
endpackage
