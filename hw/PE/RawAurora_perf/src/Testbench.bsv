package Testbench;
import StmtFSM :: *;
import ClientServer :: *;
import Connectable :: *;
import StmtFSM :: *;
import GetPut :: *;
import Vector :: *;
import GetPut :: *;

import BlueAXI :: *;
import BlueLib :: *;
import RawAurora_perf_PE :: *;

function Action get_resp(AXI4_Lite_Master_Wr#(a, d) m);
	action
		AXI4_Lite_Response wresp <- axi4_lite_write_response(m);
		printColorTimed(BLUE, $format("Write resp: %d", wresp));
	endaction
endfunction

(* synthesize *)
module [Module] mkTestbench();
	Reg#(int) i <- mkRegU;

	// dut
	RawAurora_perf_PE dut1 <- mkRawAurora_perf_PE();
	RawAurora_perf_PE dut2 <- mkRawAurora_perf_PE();
	mkConnection(dut1.i_tx, dut2.i_rx);
	mkConnection(dut2.i_tx, dut1.i_rx);

	// control 1
	AXI4_Lite_Master_Wr#(CONFIG_ADDR_WIDTH, CONFIG_DATA_WIDTH) writeMaster1 <- mkAXI4_Lite_Master_Wr(16);
	AXI4_Lite_Master_Rd#(CONFIG_ADDR_WIDTH, CONFIG_DATA_WIDTH) readMaster1 <- mkAXI4_Lite_Master_Rd(16);
	mkConnection(writeMaster1.fab, dut1.s_wr);
	mkConnection(readMaster1.fab, dut1.s_rd);

	// control 2
	AXI4_Lite_Master_Wr#(CONFIG_ADDR_WIDTH, CONFIG_DATA_WIDTH) writeMaster2 <- mkAXI4_Lite_Master_Wr(16);
	AXI4_Lite_Master_Rd#(CONFIG_ADDR_WIDTH, CONFIG_DATA_WIDTH) readMaster2 <- mkAXI4_Lite_Master_Rd(16);
	mkConnection(writeMaster2.fab, dut2.s_wr);
	mkConnection(readMaster2.fab, dut2.s_rd);


	Stmt fsm_throughput = {
	seq
		/**
		 * Setup TX PE
		 */
		axi4_lite_write(writeMaster1, 'h20, 0); // select srv mode
		get_resp(writeMaster1);

		axi4_lite_write(writeMaster1, 'h30, 3000); // set tfer len
		get_resp(writeMaster1);


		/**
		 * Setup RX PE
		 */
		axi4_lite_write(writeMaster2, 'h20, 1); // select clt mode
		get_resp(writeMaster2);

		axi4_lite_write(writeMaster2, 'h30, 3000); // set tfer len
		get_resp(writeMaster2);

		/**
		 * Start PEs
		 */
		axi4_lite_write(writeMaster1, 'h00, 1); // start PE1
		get_resp(writeMaster1);

		axi4_lite_write(writeMaster2, 'h00, 1); // start PE2
		get_resp(writeMaster2);

		// wait for finish
		par
			await(dut1.interrupt == 1);
			await(dut2.interrupt == 1);
		endpar

		// get return codes, ie tfer duration in cycles
		axi4_lite_read(readMaster1, 'h10);
		action
			let r <- readMaster1.response.get();
			printColorTimed(GREEN, $format("DUT1: Returned %d", r.data));
		endaction
		axi4_lite_read(readMaster2, 'h10);
		action
			let r <- readMaster2.response.get();
			printColorTimed(GREEN, $format("DUT2: Returned %d", r.data));
		endaction
	endseq
	};

	Stmt fsm_latency = {
	seq
		/**
		 * Setup TX PE
		 */
		axi4_lite_write(writeMaster1, 'h20, 2); // select srv mode
		get_resp(writeMaster1);

		/**
		 * Setup RX PE
		 */
		axi4_lite_write(writeMaster2, 'h20, 3); // select clt mode
		get_resp(writeMaster2);

		/**
		 * Start PEs
		 */
		axi4_lite_write(writeMaster1, 'h00, 1); // start PE1
		get_resp(writeMaster1);

		axi4_lite_write(writeMaster2, 'h00, 1); // start PE2
		get_resp(writeMaster2);

		// wait for finish
		await(dut2.interrupt == 1);

		// get return codes, ie ping latency in cycles
		axi4_lite_read(readMaster2, 'h10);
		action
			let r <- readMaster2.response.get();
			printColorTimed(GREEN, $format("DUT2: Returned latency %d", r.data));
		endaction
	endseq
	};

	mkAutoFSM(fsm_throughput);
endmodule
endpackage
