package Testbench;

import ClientServer :: *;
import Connectable :: *;
import StmtFSM :: *;
import GetPut :: *;
import Vector :: *;
import GetPut :: *;

import AxisWidthConverter :: *;
import BlueAXI :: *;
import BlueLib :: *;

typedef 192 USER_WIDTH;
typedef 64 DATA_WIDTH;

Integer cfg_buf_len = 2;

function Bit#(len) toBitmap(Bit#(TAdd#(TLog#(len),1)) number_of_ones);
	Bit#(len) mask = unpack('1);
	return mask >> (fromInteger(valueOf(len)) - number_of_ones);
endfunction

function Bit#(m) fromBitmap(Bit#(n) x);
	Bit#(m) res = 0;
	for (Integer i=0; i<valueOf(n); i=i+1)
		if (x[i] == 1) res = res + 1;
	return res;
endfunction


module [Module] mkTestbench();
	Reg#(int) j <- mkRegU();

	// ***** Downsizer *****
	AXI4_Stream_Wr#(DATA_WIDTH, 0) small_write <- mkAXI4_Stream_Wr(cfg_buf_len);
	AXI4_Stream_Rd#(DATA_WIDTH, 0) small_read  <- mkAXI4_Stream_Rd(cfg_buf_len);
	mkConnection(small_write.fab, small_read.fab);

	Downsizer#(USER_WIDTH, DATA_WIDTH) downsizer <- mkDownsizer(small_write);


	// ***** Upsizer *****
	AXI4_Stream_Wr#(DATA_WIDTH, 0) small_write2 <- mkAXI4_Stream_Wr(cfg_buf_len);
	AXI4_Stream_Rd#(DATA_WIDTH, 0) small_read2  <- mkAXI4_Stream_Rd(cfg_buf_len);
	mkConnection(small_write2.fab, small_read2.fab);

	Upsizer#(DATA_WIDTH, USER_WIDTH) upsizer <- mkUpsizer(small_read2);


	// helper rules
	rule dumpData_axis;
		let raw <- small_read.pkg.get();
		printColorTimed(BLUE, $format("Got data=%0x, keep=%0x, last=%d", raw.data, raw.keep, raw.last));
	endrule

	rule dumpData_adapter;
		let raw <- upsizer.pkg.get();
		printColorTimed(BLUE, $format("[UP] Got data=%0x, keep=%0x, last=%d", raw.data, raw.keep, raw.last));
	endrule

	Stmt s = {
		seq
			// **************** Down: FROM 24 TO 8 ****************
			action
				printColorTimed(YELLOW, $format("16 bytes pkt"));
				Bit#(USER_WIDTH) large_data = 'hdeadbeefdeadbeef1122334455667788;
				downsizer.pkg.put(AXI4_Stream_Pkg{
				data: large_data,
				keep: toBitmap(16),
				dest: 0,
				last: True
			});
			endaction
			for (j<=0; j<10; j<=j+1) seq
				noAction;
			endseq

			action
				printColorTimed(YELLOW, $format("1 bytes pkt"));
				Bit#(USER_WIDTH) large_data = 'hbb;
				downsizer.pkg.put(AXI4_Stream_Pkg{
				data: large_data,
				keep: toBitmap(1),
				dest: 0,
				last: True
			});
			endaction
			for (j<=0; j<10; j<=j+1) seq
				noAction;
			endseq

			action
				printColorTimed(YELLOW, $format("3 bytes pkt"));
				Bit#(USER_WIDTH) large_data = 'hbbccdd;
				downsizer.pkg.put(AXI4_Stream_Pkg{
				data: large_data,
				keep: toBitmap(3),
				dest: 0,
				last: True
			});
			endaction
			for (j<=0; j<10; j<=j+1) seq
				noAction;
			endseq

			action
				printColorTimed(YELLOW, $format("3 bytes pkt, error last"));
				Bit#(USER_WIDTH) large_data = 'hbbccdd;
				downsizer.pkg.put(AXI4_Stream_Pkg{
				data: large_data,
				keep: toBitmap(3),
				dest: 0,
				last: False
			});
			endaction
			for (j<=0; j<10; j<=j+1) seq
				noAction;
			endseq

			action
				printColorTimed(YELLOW, $format("24 bytes pkt, not last"));
				Bit#(USER_WIDTH) large_data = 'h1122334455667788abcdaabbccddeeff8877665544332211;
				downsizer.pkg.put(AXI4_Stream_Pkg{
				data: large_data,
				keep: toBitmap(24),
				dest: 0,
				last: False
			});
			endaction
			action
				printColorTimed(YELLOW, $format("1 bytes pkt, last"));
				Bit#(USER_WIDTH) large_data = 'haabbcc;
				downsizer.pkg.put(AXI4_Stream_Pkg{
				data: large_data,
				keep: toBitmap(1),
				dest: 0,
				last: True
			});
			endaction
			for (j<=0; j<10; j<=j+1) seq
				noAction;
			endseq

			action
				printColorTimed(YELLOW, $format("24 bytes pkt, not last"));
				Bit#(USER_WIDTH) large_data = 'haa112233445566778899aabbccddeeff;
				downsizer.pkg.put(AXI4_Stream_Pkg{
				data: large_data,
				keep: toBitmap(24),
				dest: 0,
				last: False
			});
			endaction
			action
				printColorTimed(YELLOW, $format("10 bytes pkt, is last"));
				Bit#(USER_WIDTH) large_data = 'h112233445566778899aabbccddeeff00;
				downsizer.pkg.put(AXI4_Stream_Pkg{
				data: large_data,
				keep: toBitmap(10),
				dest: 0,
				last: True
			});
			endaction
			for (j<=0; j<10; j<=j+1) seq
				noAction;
			endseq
		endseq
	};
	FSM testFSM_down <- mkFSM(s);

	Stmt s2 = {
		seq

			// **************** UP: FROM 8 to 24 ****************
			action
				printColorTimed(YELLOW, $format("1 bytes pkt, last"));
				Bit#(DATA_WIDTH) large_data = 'h1122334455667788;
				small_write2.pkg.put(AXI4_Stream_Pkg{
					data: large_data,
					keep: toBitmap(1),
					dest: 0,
					last: True
				});
			endaction
			for (j<=0; j<10; j<=j+1) seq
				noAction;
			endseq

			// This error check WILL CORRUPT future transfers!
			// action
			// 	printColorTimed(YELLOW, $format("Err: 1 bytes pkt, not last"));
			// 	Bit#(DATA_WIDTH) large_data = 'h1122334455667788;
			// 	small_write2.pkg.put(AXI4_Stream_Pkg{
			// 		data: large_data,
			// 		keep: toBitmap(1),
			// 		dest: 0,
			// 		last: False
			// 	});
			// endaction
			// for (j<=0; j<10; j<=j+1) seq
			// 	noAction;
			// endseq

			action
				printColorTimed(YELLOW, $format("8 bytes pkt, not last"));
				Bit#(DATA_WIDTH) large_data = 'h1111111122222222;
				small_write2.pkg.put(AXI4_Stream_Pkg{
					data: large_data,
					keep: toBitmap(8),
					dest: 0,
					last: False
				});
			endaction
			action
				printColorTimed(YELLOW, $format("8 bytes pkt, not last"));
				Bit#(DATA_WIDTH) large_data = 'h3333333344444444;
				small_write2.pkg.put(AXI4_Stream_Pkg{
					data: large_data,
					keep: toBitmap(8),
					dest: 0,
					last: False
				});
			endaction
			action
				printColorTimed(YELLOW, $format("8 bytes pkt, not last"));
				Bit#(DATA_WIDTH) large_data = 'h5555555566666666;
				small_write2.pkg.put(AXI4_Stream_Pkg{
					data: large_data,
					keep: toBitmap(8),
					dest: 0,
					last: False
				});
			endaction
			action
				printColorTimed(YELLOW, $format("8 bytes pkt, last"));
				Bit#(DATA_WIDTH) large_data = 'hdeadbeef;
				small_write2.pkg.put(AXI4_Stream_Pkg{
					data: large_data,
					keep: toBitmap(4),
					dest: 0,
					last: True
				});
			endaction
			for (j<=0; j<10; j<=j+1) seq
				noAction;
			endseq

			action
				printColorTimed(YELLOW, $format("5 bytes pkt, last"));
				Bit#(DATA_WIDTH) large_data = 'h1122334455667788;
				small_write2.pkg.put(AXI4_Stream_Pkg{
					data: large_data,
					keep: toBitmap(5),
					dest: 0,
					last: True
				});
			endaction
			for (j<=0; j<10; j<=j+1) seq
				noAction;
			endseq

		endseq
	};
	FSM testFSM_up <- mkFSM(s2);

	Stmt s3 = {
		seq
			printColorTimed(GREEN, $format("###################### TEST Down ######################"));
			testFSM_down.start();
			await(testFSM_down.done());

			printColorTimed(GREEN, $format("###################### TEST Up ######################"));
			testFSM_up.start();
			await(testFSM_up.done());
		endseq
	};
	mkAutoFSM(s3);

endmodule

endpackage
