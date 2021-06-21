package AxisWidthConverter;

import SpecialFIFOs :: *;
import ClientServer :: *;
import GetPut :: *;
import Vector :: *;
import FIFOF :: *;
import FIFO :: *;

import BlueAXI :: *;
import BlueLib :: *;

// toggle debug messages
Bool dbg_msgs = False;

function Action dbg(Fmt msg);
	action
		if (dbg_msgs) $display(msg);
	endaction
endfunction


interface Downsizer#(numeric type i_width_in, numeric type i_width_out);
	interface Put#(AXI4_Stream_Pkg#(i_width_in, 0)) pkg;
endinterface

module mkDownsizer#(AXI4_Stream_Wr#(width_out, 0) axis_out)(Downsizer#(width_in, width_out))
provisos (
	Mul#(num_words, width_out, width_in),
	Mul#(num_words, TDiv#(width_out, 8), TDiv#(width_in, 8))
);
	FIFOF#(AXI4_Stream_Pkg#(width_in, 0)) fifo_in <- mkFIFOF();
	Reg#(int) offset <- mkReg(0);

	rule r1;
		Bool is_last = False;
		AXI4_Stream_Pkg#(width_in, 0) pkt_in;
		pkt_in = fifo_in.first();

		Vector#(num_words, Bit#(width_out)) in_data_vec = unpack(pkt_in.data);
		Vector#(num_words, Bit#(TDiv#(width_out, 8))) in_keep_vec = unpack(pkt_in.keep);

		Bit#(width_out) out_data = in_data_vec[offset];
		Bit#(TDiv#(width_out, 8)) out_keep = in_keep_vec[offset];

		Bool data_remaining = False;
		for (int j=0; j<fromInteger(valueOf(num_words)); j=j+1) begin
			if (j > offset && in_keep_vec[j] != 0) begin
				data_remaining = True;
			end
		end

		dbg($format("[Downsizer] --> offs=%0d, keep:", offset, fshow(in_keep_vec)));
		if (offset + 1 == fromInteger(valueOf(num_words))) begin
			if (pkt_in.last) begin
				dbg($format("[Downsizer] CASE: last small pkt in large pkt, is_last of large pkt is set"));
				is_last = True;
			end else begin
				dbg($format("[Downsizer] CASE: last small pkt in large pkt, is_last of large pkt NOT set"));
			end

			fifo_in.deq();
			offset <= 0;
		end else if (!data_remaining) begin
			if (!pkt_in.last) begin
				$display("[Downsizer] ERRoR: BROKEN KEEP: partial keep that is not end-of-transfer!! (Keep=0x%x)", pkt_in.keep);
			end
			dbg($format("[Downsizer] CASE: Middle, but remainder all zeroes"));

			fifo_in.deq();
			is_last = True;
			offset <= 0;
		end else begin
			dbg($format("[Downsizer] CASE: middle of large pkt, New offset = %0d", offset + 1));
			offset <= offset + 1;
		end

		axis_out.pkg.put(AXI4_Stream_Pkg{
			data: out_data,
			keep: out_keep,
			last: is_last,
			dest: 0
		});
	endrule

	interface pkg = toPut(fifo_in);
endmodule


interface Upsizer#(numeric type i_width_in, numeric type i_width_out);
	interface Get#(AXI4_Stream_Pkg#(i_width_out, 0)) pkg;
endinterface

module mkUpsizer#(AXI4_Stream_Rd#(width_in, 0) axis_in)(Upsizer#(width_in, width_out))
provisos (
	Mul#(num_words, width_in, width_out),
	Mul#(num_words, TDiv#(width_in, 8), TDiv#(width_out, 8))
);
	Vector#(num_words, Reg#(Bit#(width_in))) out_data_vec <- replicateM(mkReg(0));
	Vector#(num_words, Reg#(Bit#(TDiv#(width_in,8)))) out_keep_vec <- replicateM(mkReg(0));

	FIFO#(Tuple3#(Bit#(width_out), Bit#(TDiv#(width_out,8)), Bool)) field_buf_snap <- mkBypassFIFO();
	FIFOF#(AXI4_Stream_Pkg#(width_out, 0)) fifo_out <- mkFIFOF();

	Reg#(Bool) is_last <- mkReg(False);
	Reg#(int) offset <- mkReg(0);

	rule r1;
		let pkt_in <- axis_in.pkg.get();
		Bit#(width_in) in_data = pkt_in.data;
		Bit#(TDiv#(width_in,8)) in_keep = pkt_in.keep;

		out_data_vec[offset] <= in_data;
		out_keep_vec[offset] <= in_keep;
	
		if (pkt_in.last || offset + 1 == fromInteger(valueOf(num_words))) begin
			dbg($format("[Upsizer] CASE: Taking snapshot, resetting offs, small is last=%d", pkt_in.last));

			Vector#(num_words, Bit#(width_in)) out_data_vec_snap = replicate(0);
			Vector#(num_words, Bit#(TDiv#(width_in,8))) out_keep_vec_snap = replicate(0);
			for (int i=0; i<fromInteger(valueOf(num_words)); i=i+1) begin
				if (i < offset) begin
					out_data_vec_snap[i] = out_data_vec[i];
					out_keep_vec_snap[i] = out_keep_vec[i];
				end
			end

			out_data_vec_snap[offset] = in_data;
			out_keep_vec_snap[offset] = in_keep;
			dbg($format("[Upsizer] --> out_keep_vec_snap = 0x%x", out_keep_vec_snap));

			field_buf_snap.enq(tuple3(
				pack(out_data_vec_snap),
				pack(out_keep_vec_snap),
				pkt_in.last
			));

			offset <= 0;
		end else begin
			dbg($format("[Upsizer] CASE: Middle of large word, new offset = %d", offset + 1));
			offset <= offset + 1;

			// sanity check
			if (!pkt_in.last && pkt_in.keep != '1) begin
				$display("[Upsizer] ERRoR: BROKEN KEEP: partial keep that is not end-of-transfer!! (Keep=0x%x)", pkt_in.keep);
			end
		end
	endrule

	rule r2;
		dbg($format("[Upsizer] --> got new data from snapshot FIFO"));

		field_buf_snap.deq();
		fifo_out.enq(AXI4_Stream_Pkg{
			data: tpl_1(field_buf_snap.first),
			keep: tpl_2(field_buf_snap.first),
			last: tpl_3(field_buf_snap.first),
			dest: 0
		});
	endrule

	interface pkg = toGet(fifo_out);
endmodule
endpackage
