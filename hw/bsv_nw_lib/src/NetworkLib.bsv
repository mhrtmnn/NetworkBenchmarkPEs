package NetworkLib;

import SpecialFIFOs :: *;
import ClientServer :: *;
import BRAMFIFO :: * ;
import GetPut :: *;
import Vector :: *;
import FIFOF :: *;
import FIFO :: *;

import AxisWidthConverter :: *;
import BlueAXI :: *;
import BlueLib :: *;

/**
 * Common
 */
Bool dbg_msgs = True;

// MSS must be a multiple of user_api_width/8
// Integer mss = 1024;
// Integer mss = 1408;
// Integer mss = 2048;
Integer mss = 4096;
// Integer mss = 8192;

Integer cfg_buf_len = 2;
typedef 0   AXI_USER_WIDTH;

/**
 * UDP axis
 */
typedef 256 UDP_META_WIDTH;

/**
 * TCP axis
 */
typedef 16 	TCP_LISTEN_PORT_WIDTH;
typedef 8 	TCP_PORT_STATUS_WIDTH;
typedef 64 	TCP_OPEN_CONN_WIDTH;
typedef 32 	TCP_OPEN_STATUS_WIDTH;
typedef 16 	TCP_CLOSE_CONN_WIDTH;
typedef 128 TCP_NOTIFICATION_WIDTH;
typedef 32 	TCP_READ_PKG_WIDTH;
typedef 16 	TCP_RX_META_WIDTH;
typedef 32 	TCP_TX_META_WIDTH;
typedef 64 	TCP_TX_STATUS_WIDTH;

// Library config
typedef struct {
	Bool udp_rx;
	Bool tcp_tx;
	Bool tcp_rx;
	Bool tcp_open;
	Bool tcp_listen;
} NW_config;

// returns 2^n-1
function Bit#(len) toBitmap(Bit#(param_len) number_of_ones);
	Bit#(len) mask = unpack('1);
	return mask >> (fromInteger(valueOf(len)) - number_of_ones);
endfunction

function Bit#(m) fromBitmap(Bit#(n) x);
	Bit#(m) res = 0;
	for (Integer i=0; i<valueOf(n); i=i+1)
		if (x[i] == 1) res = res + 1;
	return res;
endfunction

function Integer toByteLen(Bit#(bitlen) x) = valueOf(TDiv#(bitlen, 8));

function AXI4_Stream_Pkg#(dw, 0) mkPkg(Bit#(dw) data, Bit#(TDiv#(dw, 8)) keep, Bool last) = AXI4_Stream_Pkg {
	data: data,
	keep: keep,
	last: last,
	user: 0,
	dest: 0
};

// automatically calculate TKEEP depending on parameter data type
function AXI4_Stream_Pkg#(data_width, 0) mkPkgWithKeep(Bit#(payload_bits) data, Bool last) provisos (
	Add#(_u, payload_bits, data_width),
	Div#(payload_bits, 8, payload_bytes)
);
	Bit#(32) num_bytes = fromInteger(valueOf(payload_bytes));
	AXI4_Stream_Pkg#(data_width, 0) pp = AXI4_Stream_Pkg {
		data: extend(data),
		keep: toBitmap(num_bytes),
		last: last,
		user: 0,
		dest: 0
	};
	return pp;
endfunction

function ActionValue#(t) parse_from(AXI4_Stream_Rd #(data_width, AXI_USER_WIDTH) axis) provisos (
	Bits#(t, sz_t),
	Add#(sz_t, _u, data_width)
);
	actionvalue
		let raw <- axis.pkg.get();
		return unpack(truncate(raw.data));
	endactionvalue
endfunction

function Bit#(32) make_ip(Bit#(8) x0, Bit#(8) x1, Bit#(8) x2, Bit#(8) x3);
	Vector#(4, Bit#(8)) out_ip;
	out_ip[0] = x3;
	out_ip[1] = x2;
	out_ip[2] = x1;
	out_ip[3] = x0;
	return pack(out_ip);
endfunction

function Bit#(32) reverse_ip(Bit#(32) inv_ip);
	Vector#(4, Bit#(8)) in_ip = unpack(inv_ip);
	Vector#(4, Bit#(8)) out_ip;
	out_ip[0] = in_ip[3];
	out_ip[1] = in_ip[2];
	out_ip[2] = in_ip[1];
	out_ip[3] = in_ip[0];
	return pack(out_ip);
endfunction

function Vector#(n, Bit#(8)) str2byteVec(String s);
	List#(Char) cl = stringToCharList(s);
	List#(Integer) il = List::map(charToInteger, cl);
	List#(Bit#(8)) bl = List::map(fromInteger, il);
	return toVector(bl);
endfunction

function Bit#(bitwidth) swapBytes(Bit#(bitwidth) bit_in) provisos(
	Mul#(TDiv#(bitwidth, 8), 8, bitwidth),
	Div#(bitwidth, 8, bytewidth)
);
	Vector#(bytewidth, Bit#(8)) v = unpack(bit_in);
	return pack(reverse(v));
endfunction

function Action dbg(Fmt msg);
	action
		if (dbg_msgs) $display(msg);
	endaction
endfunction


/****************************************************************
 * NetworkLib - Top
 ***************************************************************/
interface NetworkLib#(numeric type i_user_api_width, numeric type i_data_width);
	(*prefix=""*) interface LibUDP#(i_user_api_width, i_data_width) udp;
	(*prefix=""*) interface LibTCP#(i_user_api_width, i_data_width) tcp;
endinterface

module mkNetworkLib#(NW_config cfg)(NetworkLib#(user_api_width, data_width)) provisos(
	Mul#(num_words, data_width, user_api_width),
	Mul#(num_words, TDiv#(data_width, 8), TDiv#(user_api_width, 8)),
	Add#(TLog#(TDiv#(user_api_width, 8)), a__, 15)
);
	LibUDP#(user_api_width, data_width) udp_server <- mkLibUDP(cfg.udp_rx);
	LibTCP#(user_api_width, data_width) tcp_server <- mkLibTCP(cfg.tcp_open, cfg.tcp_listen, cfg.tcp_tx, cfg.tcp_rx);

	interface udp = udp_server;
	interface tcp = tcp_server;
endmodule


/****************************************************************
 * NetworkLib - Sub: UDP
 ***************************************************************/
typedef struct {
	Bit#(16) length;
	Bit#(16) my_port;
	Bit#(16) their_port; // can be dst or src, depending on direction!!
	Bit#(128) their_addr;
} AxisUdpMetaPkg deriving (Bits, FShow);
typedef 176 UDP_META_HEADER_LEN;

// user exposed
typedef struct {
	Bit#(32) their_addr;
	Bit#(16) src_port;
	Bit#(16) dst_port;
	Bit#(16) length;
} UdpMetaPkg deriving (Bits, FShow);

typedef struct {
	Bit#(n) data;
	Bit#(16) word_bytes;
	Bool last_word;
} UdpDataWord#(numeric type n) deriving (Bits);

interface UDP_fab#(numeric type i_data_width);
	(*prefix="s_axis_udp_rx"*)interface AXI4_Stream_Rd_Fab#(i_data_width, AXI_USER_WIDTH) i_udp_rx;
	(*prefix="m_axis_udp_tx"*)interface AXI4_Stream_Wr_Fab#(i_data_width, AXI_USER_WIDTH) i_udp_tx;

	(*prefix="s_axis_udp_rx_meta"*)interface AXI4_Stream_Rd_Fab#(UDP_META_WIDTH, AXI_USER_WIDTH) i_udp_rx_meta;
	(*prefix="m_axis_udp_tx_meta"*)interface AXI4_Stream_Wr_Fab#(UDP_META_WIDTH, AXI_USER_WIDTH) i_udp_tx_meta;
endinterface

interface LibUDP#(numeric type i_user_api_width, numeric type i_data_width);
	/*
	 * User exposed
	 */
	method Action setMeta(Bit#(16) src_port, Bit#(16) dst_port, Bit#(32) dst_addr, Bit#(16) pkg_bytes);
	method Action putData(Bit#(i_user_api_width) data, Bit#(16) length);

	method ActionValue#(UdpMetaPkg) getMeta();
	interface Get#(UdpDataWord#(i_user_api_width)) datawordRx;

	method Bool newDataAvailable();

	/*
	 * To Ntwrk Kernel
	 */
	(*prefix=""*) interface UDP_fab#(i_data_width) fab;
endinterface

module mkLibUDP#(Bool en_udp_rx)(LibUDP#(user_api_width, data_width)) provisos(
	Mul#(num_words, data_width, user_api_width),
	Mul#(num_words, TDiv#(data_width, 8), TDiv#(user_api_width, 8))
);
	AXI4_Stream_Rd#(data_width, AXI_USER_WIDTH) udp_rx <- mkAXI4_Stream_Rd(cfg_buf_len);
	AXI4_Stream_Wr#(data_width, AXI_USER_WIDTH) udp_tx <- mkAXI4_Stream_Wr(cfg_buf_len);
	AXI4_Stream_Rd#(UDP_META_WIDTH, AXI_USER_WIDTH) udp_rx_meta <- mkAXI4_Stream_Rd(cfg_buf_len);
	AXI4_Stream_Wr#(UDP_META_WIDTH, AXI_USER_WIDTH) udp_tx_meta <- mkAXI4_Stream_Wr(cfg_buf_len);

	Reg#(Bit#(16)) sent_bytes <- mkReg(0);
	FIFOF#(UdpDataWord#(user_api_width)) datawords_rx <- mkFIFOF();
	FIFO#(UdpMetaPkg) meta_rx <- mkFIFO();
	FIFO#(Bit#(16)) bytes_in_packet <- mkFIFO();
	FIFO#(Tuple2#(Bit#(user_api_width), Bit#(16))) datawords_tx <- mkFIFO();

	// axis width converters
	Downsizer#(user_api_width, data_width) downsize <- mkDownsizer(udp_tx);
	Upsizer#(data_width, user_api_width) upsize <- mkUpsizer(udp_rx);

	rule tx;
		let data = tpl_1(datawords_tx.first());
		let word_bytes = tpl_2(datawords_tx.first());
		datawords_tx.deq();

		let expected_bytes = bytes_in_packet.first();
		Bool is_last = False;
		if (sent_bytes + word_bytes < expected_bytes) begin
			sent_bytes <= sent_bytes + word_bytes;
		end else begin
			sent_bytes <= 0;
			is_last = True;
			bytes_in_packet.deq();
		end

		dbg($format("[UDP TX] sent=%d, with this word=%d, expected=%d", sent_bytes, sent_bytes+word_bytes, expected_bytes));
		downsize.pkg.put(mkPkg(data, toBitmap(word_bytes), is_last));
	endrule

if (en_udp_rx) begin
	rule rx_data;
		let raw <- upsize.pkg.get();
		let d = UdpDataWord {
			data: raw.data,
			word_bytes: fromBitmap(raw.keep),
			last_word: raw.last
		};
		datawords_rx.enq(d);
		dbg($format("[UDP RX] got data %0x ('%s')", d, d));
	endrule

	rule rx_meta;
		AxisUdpMetaPkg p <- parse_from(udp_rx_meta);
		UdpMetaPkg user_p = UdpMetaPkg {
			their_addr: reverse_ip(p.their_addr[31:0]), // addr is sent in reverse order
			src_port: p.their_port, // RX path: "their" == src
			dst_port: p.my_port,
			length: p.length - 8 // tell user the length without UDP header
		};
		meta_rx.enq(user_p);
		dbg($format("[UDP RX] got metadata: ", fshow(user_p)));
	endrule
end // en_udp_rx

	/*
	 * TIE OFF
	 */
if (!en_udp_rx) begin
	rule tie_off_udp;
		let p <- upsize.pkg.get();
		dbg($format("[UDP tie off] rx_data=%x", p.data));
	endrule

	rule tie_off_udp2;
		let p <- udp_rx_meta.pkg.get();
		dbg($format("[UDP tie off] rx_meta=%x", p.data));
	endrule
end // !en_udp_rx


	/****************************************************************
	 * Interfaces
	 ***************************************************************/
	method Action setMeta(Bit#(16) src_port, Bit#(16) dst_port, Bit#(32) inv_dst_addr, Bit#(16) pkg_bytes);
		// quirk of TCP/IP stack: UDP addr is expexted in reverse order (not the case for TCP)
		Bit#(32) dst_addr = reverse_ip(inv_dst_addr);
		Bit#(UDP_META_HEADER_LEN) enc_header = pack(AxisUdpMetaPkg {
			their_addr: {dst_addr,dst_addr,dst_addr,dst_addr},
			their_port: dst_port, // TX path: "their" == dst
			my_port: src_port,
			length: pkg_bytes // without UDP header len (+8 is done in UDP stack)
		});
		udp_tx_meta.pkg.put(mkPkgWithKeep(enc_header, True));
		bytes_in_packet.enq(pkg_bytes);
	endmethod

	method Action putData(Bit#(user_api_width) data, Bit#(16) word_bytes);
		datawords_tx.enq(tuple2(data, word_bytes));
	endmethod

	method ActionValue#(UdpMetaPkg) getMeta();
		UdpMetaPkg p = meta_rx.first(); meta_rx.deq();
		return p;
	endmethod
	interface datawordRx = toGet(datawords_rx);

	method Bool newDataAvailable();
		return datawords_rx.notEmpty();
	endmethod

	interface UDP_fab fab;
		interface i_udp_rx = udp_rx.fab;
		interface i_udp_tx = udp_tx.fab;
		interface i_udp_rx_meta = udp_rx_meta.fab;
		interface i_udp_tx_meta = udp_tx_meta.fab;
	endinterface
endmodule


/****************************************************************
 * NetworkLib - Sub: TCP
 ***************************************************************/
typedef Bit#(16) Session_t;
typedef Bit#(16) Port_t;
typedef Bit#(32) Address_t;

typedef enum {
	NO_ERROR      = 0,
	ERROR_NOCONN  = 1,
	ERROR_NOSPACE = 2
} TcpErr_t deriving(Bits, Eq, FShow);

/*
 * NOTE:
 * BSV packs structs such that the first member is in the _MOST_ significant bits (Ref. Guide p. 125)
 * C/HLS packs structs such that it is in the _LEAST_ significant bits.
 * Thus the fields of BSV structs are in reverse order to the C/HLS structs (e.g. in toe.hpp)
 */

// m_axis_tcp_open_connection
typedef struct {
	Port_t		basePort;
	Address_t 	ipAddress;
} AxisTcpOpenConn deriving (Bits);
typedef 48 TCP_OPEN_CONN_LEN;

// s_axis_tcp_open_status
typedef struct {
	Bool 		success;
	Session_t 	sessionID;
} AxisTcpOpenStatus deriving (Bits);
typedef 17 TCP_OPEN_STATUS_LEN;

// m_axis_tcp_close_connection
typedef struct {
	Session_t 	sessionID;
} AxisTcpCloseConn deriving (Bits);
typedef 16 TCP_CLOSE_CONN_LEN;

// s_axis_tcp_rx_meta
typedef struct {
	Session_t 	sessionID;
} AxisTcpRxMeta deriving (Bits);
typedef 16 TCP_RX_META_LEN;

// m_axis_tcp_tx_meta
typedef struct {
	Bit#(16) 	length; // in bytes
	Session_t 	sessionID;
} AxisTcpTxMeta deriving (Bits);
typedef 32 TCP_TX_META_LEN;

// s_axis_tcp_tx_status
typedef struct {
	TcpErr_t 	error;
	Bit#(30) 	remainingSpace;
	Bit#(16) 	length;
	Session_t 	sessionID;
} AxisTcpTxStatus deriving (Bits);
typedef 64 TCP_TX_STATUS_LEN;

// m_axis_tcp_listen_port
typedef struct {
	Port_t		basePort;
} AxisTcpListenPort deriving (Bits);
typedef 16 TCP_LISTEN_CONN_LEN;

// s_axis_tcp_port_status
typedef struct {
	Bool 		success;
} AxisTcpPortStatus deriving (Bits);

// s_axis_tcp_notification
typedef struct {
	Bool	 	closed;
	Port_t 		dstPort;
	Address_t 	ipAddress;
	Bit#(16) 	length;
	Session_t 	sessionID;
} AxisTcpNotification deriving (Bits);
typedef 81 TCP_NOTIFICATION_LEN;

// m_axis_tcp_read_pkg
typedef struct {
	Bit#(16) 	length;
	Session_t 	sessionID;
} AxisTcpReadPkg deriving (Bits);
typedef 32 TCP_RD_PKG_LEN;

/*
 * User Iface structs
 */
typedef struct {
	Address_t 	ipAddress;
	Port_t		basePort;
	Bit#(16) 	numConn;
} OpenConnReq deriving (Bits);

typedef struct {
	Port_t		basePort;
	Bit#(16) 	numConn;
} ListenConnReq deriving (Bits);

typedef struct {
	Session_t	sessionID;
	Bit#(16) 	pkt_len;
	Bit#(16) 	word_len;
	Bit#(n) data;
	Bool 		isLast;
} DataRX#(numeric type n) deriving (Bits);

interface TCP_fab#(numeric type i_data_width);
	(*prefix="m_axis_tcp_listen_port"*) interface AXI4_Stream_Wr_Fab #(TCP_LISTEN_PORT_WIDTH, AXI_USER_WIDTH) i_listen_port;
	(*prefix="s_axis_tcp_port_status"*) interface AXI4_Stream_Rd_Fab #(TCP_PORT_STATUS_WIDTH, AXI_USER_WIDTH) i_port_status;

	(*prefix="m_axis_tcp_open_connection"*) interface AXI4_Stream_Wr_Fab #(TCP_OPEN_CONN_WIDTH, AXI_USER_WIDTH) i_open_connection;
	(*prefix="s_axis_tcp_open_status"*) interface AXI4_Stream_Rd_Fab #(TCP_OPEN_STATUS_WIDTH, AXI_USER_WIDTH) i_open_status;
	(*prefix="m_axis_tcp_close_connection"*) interface AXI4_Stream_Wr_Fab #(TCP_CLOSE_CONN_WIDTH, AXI_USER_WIDTH) i_close_connection;

	(*prefix="s_axis_tcp_notification"*) interface AXI4_Stream_Rd_Fab #(TCP_NOTIFICATION_WIDTH, AXI_USER_WIDTH) i_notification;
	(*prefix="m_axis_tcp_read_pkg"*) interface AXI4_Stream_Wr_Fab #(TCP_READ_PKG_WIDTH, AXI_USER_WIDTH) i_read_pkg;

	(*prefix="s_axis_tcp_rx_meta"*) interface AXI4_Stream_Rd_Fab #(TCP_RX_META_WIDTH, AXI_USER_WIDTH) i_rx_meta;
	(*prefix="s_axis_tcp_rx_data"*) interface AXI4_Stream_Rd_Fab #(i_data_width, AXI_USER_WIDTH) i_rx_data;

	(*prefix="m_axis_tcp_tx_meta"*) interface AXI4_Stream_Wr_Fab #(TCP_TX_META_WIDTH, AXI_USER_WIDTH) i_tx_meta;
	(*prefix="m_axis_tcp_tx_data"*) interface AXI4_Stream_Wr_Fab #(i_data_width, AXI_USER_WIDTH) i_tx_data;
	(*prefix="s_axis_tcp_tx_status"*) interface AXI4_Stream_Rd_Fab #(TCP_TX_STATUS_WIDTH, AXI_USER_WIDTH) i_tx_status;
endinterface

interface LibTCP#(numeric type i_user_api_width, numeric type i_data_width);
	/*
	 * User exposed
	 */
	method Action openConnections(Bit#(16) numConn, Address_t ipAddress, Port_t basePort);
	interface Get#(Session_t) sessionIDs;
	method Action closeConnection(Bit#(16) sessionID);
	interface Get#(Session_t) closedByClient;

	method Action announceTfer(Session_t id, Bit#(64) num_bytes);
	method Action putData(Bit#(i_user_api_width) data, Bit#(16) word_bytes);
	method Action overrideSession(Session_t s);

	method Action listenPorts(Bit#(16) numConn, Port_t basePort);
	method Bool isListening();
	interface Get#(DataRX#(i_user_api_width)) datawordRx;

	/*
	 * To Ntwrk Kernel
	 */
	(*prefix=""*) interface TCP_fab#(i_data_width) fab;
endinterface

module mkLibTCP#(Bool en_open, Bool en_listen, Bool en_tx, Bool en_rx)(LibTCP#(user_api_width, data_width)) provisos (
	Mul#(num_words, data_width, user_api_width),
	Mul#(num_words, TDiv#(data_width, 8), TDiv#(user_api_width, 8))
);
	AXI4_Stream_Wr #(TCP_LISTEN_PORT_WIDTH, AXI_USER_WIDTH) listen_port <- mkAXI4_Stream_Wr(cfg_buf_len);
	AXI4_Stream_Rd #(TCP_PORT_STATUS_WIDTH, AXI_USER_WIDTH) port_status <- mkAXI4_Stream_Rd(cfg_buf_len);
	AXI4_Stream_Wr #(TCP_OPEN_CONN_WIDTH, AXI_USER_WIDTH) open_connection <- mkAXI4_Stream_Wr(cfg_buf_len);
	AXI4_Stream_Rd #(TCP_OPEN_STATUS_WIDTH, AXI_USER_WIDTH) open_status <- mkAXI4_Stream_Rd(cfg_buf_len);
	AXI4_Stream_Wr #(TCP_CLOSE_CONN_WIDTH, AXI_USER_WIDTH) close_connection <- mkAXI4_Stream_Wr(cfg_buf_len);
	AXI4_Stream_Rd #(TCP_NOTIFICATION_WIDTH, AXI_USER_WIDTH) notification <- mkAXI4_Stream_Rd(cfg_buf_len);
	AXI4_Stream_Wr #(TCP_READ_PKG_WIDTH, AXI_USER_WIDTH) read_pkg <- mkAXI4_Stream_Wr(cfg_buf_len);
	AXI4_Stream_Rd #(TCP_RX_META_WIDTH, AXI_USER_WIDTH) rx_meta <- mkAXI4_Stream_Rd(cfg_buf_len);
	AXI4_Stream_Rd #(data_width, AXI_USER_WIDTH) rx_data <- mkAXI4_Stream_Rd(cfg_buf_len);
	AXI4_Stream_Wr #(TCP_TX_META_WIDTH, AXI_USER_WIDTH) tx_meta <- mkAXI4_Stream_Wr(cfg_buf_len);
	AXI4_Stream_Wr #(data_width, AXI_USER_WIDTH) tx_data <- mkAXI4_Stream_Wr(cfg_buf_len);
	AXI4_Stream_Rd #(TCP_TX_STATUS_WIDTH, AXI_USER_WIDTH) tx_status <- mkAXI4_Stream_Rd(cfg_buf_len);

	FIFO#(OpenConnReq) open_conn_reqs <- mkFIFO();
	FIFO#(ListenConnReq) listen_conn_reqs <- mkFIFO();
	FIFO#(Bit#(16)) announce_pkt <- mkFIFO();
	FIFO#(Bool) open_await_status <- mkFIFO();
	FIFO#(Bool) listen_await_status <- mkFIFO();
	FIFO#(Session_t) meta_rx <- mkBypassFIFO();
	FIFO#(Bit#(16)) send_data_reqs <- mkFIFO();
	FIFO#(Tuple2#(Session_t, Bit#(64))) pending_tfer <- mkFIFO();
	FIFOF#(Tuple2#(Bit#(user_api_width), Bit#(16))) datawords_tx <- mkBypassFIFOF();
	FIFO#(DataRX#(user_api_width)) datawords_rx <- mkFIFO();
	FIFO#(Session_t) sessions <- mkFIFO();
	FIFO#(Session_t) closedSessions <- mkFIFO();

	Reg#(Bit#(16)) open_i <- mkReg(0);
	Reg#(Bit#(16)) listen_i <- mkReg(0);

	Reg#(Bool) opened <- mkReg(False);
	Reg#(Bool) listening <- mkReg(False);
	FIFOF#(Session_t) session_override <- mkSizedFIFOF(1);

	Reg#(Bit#(16)) pkt_remaining <- mkRegU();
	Reg#(Bit#(64)) sent_bytes <- mkReg(0);
	Reg#(Bit#(3)) send_state <- mkReg(0);
	Reg#(Bit#(16)) retry_len <- mkReg(0);

	// axis width converters
	Downsizer#(user_api_width, data_width) downsize <- mkDownsizer(tx_data);
	Upsizer#(data_width, user_api_width) upsize <- mkUpsizer(rx_data);

	// at least one active connection in which pkts can be sent or received
	function Bool activeCon() = (opened || listening);

	/*
	 * OPEN
	 */
if (en_open) begin
	rule open_0;
		let req = open_conn_reqs.first();
		Bit#(TCP_OPEN_CONN_LEN) enc_pkg = pack(AxisTcpOpenConn {
			ipAddress:req.ipAddress,
			basePort: req.basePort
		});
		open_connection.pkg.put(mkPkgWithKeep(enc_pkg, True));
		dbg($format("[TCP open] Open request %d/%d", open_i+1, req.numConn));

		Bool last_request;
		if (open_i == req.numConn - 1) begin
			open_i <= 0;
			dbg($format("[TCP open] Dequeuing open request"));
			open_conn_reqs.deq();
			last_request = True;
		end else begin
			open_i <= open_i + 1;
			last_request = False;
		end
		open_await_status.enq(last_request);
	endrule

	rule open_1;
		AxisTcpOpenStatus status <- parse_from(open_status);
		if (status.success) begin
			sessions.enq(status.sessionID);
			dbg($format("[TCP open] Connection %d successfully opened.", status.sessionID));
		end
		if (open_await_status.first())
			opened <= True;
		open_await_status.deq();
	endrule
end // en_open

	/*
	 * SEND
	 */
if (en_tx) begin
	rule send_0(activeCon && send_state == 1);
		AxisTcpTxStatus resp <- parse_from(tx_status);
		dbg($format("[TCP TX] Got tx status: err=%d, len=%d, remSpace=%d", resp.error, resp.length, resp.remainingSpace));
		case (resp.error) matches
		NO_ERROR: begin

			// pipelined announce of next packet
			Bit#(64) remaining_in_tfer = tpl_2(pending_tfer.first) - sent_bytes;
			if (remaining_in_tfer > fromInteger(mss)) begin
				Bit#(64) next_pkt_len = min(fromInteger(mss), remaining_in_tfer - fromInteger(mss));

				Session_t sess = tpl_1(pending_tfer.first);

				// FIXME: only works with '-aggressive-conditions' which is off by default
				// if (session_override.notEmpty) begin
				// 	sess = session_override.first();
				// 	session_override.deq();
				// end

				dbg($format("[TCP TX] Announce next pkt of %d, session %d", next_pkt_len, sess));
				Bit#(TCP_TX_META_LEN) enc_pkg = pack(AxisTcpTxMeta {
					sessionID: sess,
					length: truncate(next_pkt_len)
				});
				tx_meta.pkg.put(mkPkgWithKeep(enc_pkg, True));
				retry_len <= truncate(next_pkt_len);
			end

			pkt_remaining <= truncate(min(fromInteger(mss), remaining_in_tfer));
			send_state <= 2;

		end
		ERROR_NOCONN: begin
			dbg($format("[TCP TX] Conn %d was torn down!", resp.sessionID));
			// TODO: Notify user about failure
			send_state <= 0;
			if (datawords_tx.notEmpty) begin
				// User might already have put data
				datawords_tx.deq();
			end
		end
		ERROR_NOSPACE: begin
			// error handling: repeat announce
			$display("[TCP TX] TX Meta: No buffer space remaining, retrying ... (len=%d)", retry_len);
			Bit#(TCP_TX_META_LEN) enc_pkg = pack(AxisTcpTxMeta {
				sessionID: resp.sessionID,
				length: retry_len
			});
			tx_meta.pkg.put(mkPkgWithKeep(enc_pkg, True));
		end
		endcase
	endrule
end // en_tx

	// send words, until packet is complete
	rule send_3(activeCon && send_state == 2);
		Bit#(16) word_sz = min(fromInteger(valueOf(user_api_width))/8, pkt_remaining);
		let data = tpl_1(datawords_tx.first());
		datawords_tx.deq();

		// sanity check
		if (word_sz != tpl_2(datawords_tx.first)) begin
			$display("[TCP TX] ERRoR: BROKEN WORD LEN: partial keep that is not end-of-transfer!! Is %d, expected %d",
				tpl_2(datawords_tx.first), word_sz);
		end

		let is_last = False;
		if (pkt_remaining - word_sz == 0) begin
			is_last = True;
			if (sent_bytes + extend(word_sz) >= tpl_2(pending_tfer.first)) begin
				$display("[TCP TX] Tfer done: %d bytes sent", sent_bytes+extend(word_sz));
				pending_tfer.deq();

				send_state <= 0;
				sent_bytes <= 0;
			end else begin
				$display("[TCP TX] Pkt done: %d bytes sent", sent_bytes+extend(word_sz));

				sent_bytes <= sent_bytes + extend(word_sz);
				send_state <= 1;
			end

		end else begin
			sent_bytes <= sent_bytes + extend(word_sz);
			pkt_remaining <= pkt_remaining - word_sz;
		end

		dbg($format("[TCP TX] Sent=%d, this word=%d, remaining in pkt=%d (last=%d)",
			sent_bytes, word_sz, pkt_remaining, is_last));

		downsize.pkg.put(mkPkg(data, toBitmap(word_sz), is_last));
	endrule

	/*
	 * LISTEN
	 */
if (en_listen) begin
	rule listen_0;
		let req = listen_conn_reqs.first();
		Bit#(TCP_LISTEN_CONN_LEN) enc_pkg = pack(AxisTcpListenPort {
			basePort: req.basePort + listen_i
		});
		listen_port.pkg.put(mkPkgWithKeep(enc_pkg, True));

		Bool last_request;
		if (listen_i == req.numConn - 1) begin
			listen_i <= 0;
			dbg($format("[TCP listen] Dequeuing listen request"));
			listen_conn_reqs.deq();
			last_request = True;
		end else begin
			dbg($format("[TCP listen] Listen request %d of %d", listen_i+1, req.numConn));
			listen_i <= listen_i + 1;
			last_request = False;
		end
		listen_await_status.enq(last_request);
	endrule

	rule listen_1;
		AxisTcpPortStatus status <- parse_from(port_status);
		if (status.success) begin
			dbg($format("[TCP listen] Connection is listening."));
		end
		if (listen_await_status.first())
			listening <= True;
		listen_await_status.deq();
	endrule
end // en_listen

	/*
	 * RECEIVE
	 */

if (en_rx) begin
	// get announcement for outstanding packet, then issue a read request
	rule recv_0(activeCon);
		AxisTcpNotification notif <- parse_from(notification);
		dbg($format("[TCP RX] Got announcement for %d bytes in session %d", notif.length, notif.sessionID));

		// has size 0 eg if connection was closed or reset. A pkt with FIN flag set can still contain data
		if (notif.length > 0) begin
			Bit#(TCP_RD_PKG_LEN) enc_pkg = pack(AxisTcpReadPkg {
				sessionID: notif.sessionID,
				length: notif.length
			});
			read_pkg.pkg.put(mkPkgWithKeep(enc_pkg, True));
			announce_pkt.enq(notif.length);
		end

		if (notif.closed) begin
			// connection was closed by remote client
			closedSessions.enq(notif.sessionID);
		end
	endrule

	// get meta info of RX packet to be read
	rule recv_1(activeCon);
		AxisTcpRxMeta meta <- parse_from(rx_meta);
		meta_rx.enq(meta.sessionID);
	endrule

	// get (potentially) multiple data words of RX packet to be read
	rule recv_2(activeCon);
		let pkt_sz = announce_pkt.first();
		let sessionID = meta_rx.first();
		let word <- upsize.pkg.get();
		Bit#(16) word_len = fromBitmap(word.keep);

		let p = DataRX {
			sessionID: sessionID,
			pkt_len: pkt_sz,
			word_len: word_len,
			data: word.data,
			isLast: word.last
		};
		datawords_rx.enq(p);
		dbg($format("[TCP RX] Receiving %d byte word for %d byte packet in session %d", word_len, pkt_sz, sessionID));

		// instead of keeping track of pkt_sz simply recv words until TLAST is set 
		if (word.last) begin
			dbg($format("[TCP RX] Got last word"));
			announce_pkt.deq();
			meta_rx.deq();
		end
	endrule
end // en_rx

	/*
	 * TIE OFF
	 */
if (!en_listen) begin
	rule r_tieOff_listen;
		AxisTcpPortStatus p <- parse_from(port_status);
		dbg($format("[TCP tie off] port_status success=%d", p.success));
	endrule
end

if (!en_rx) begin
	rule r_tieOff_rx;
		AxisTcpNotification p <- parse_from(notification);
		dbg($format("[TCP tie off] notification=%d", p));
	endrule

	rule r_tieOff_rx2;
		AxisTcpRxMeta p <- parse_from(rx_meta);
		dbg($format("[TCP tie off] rx_meta=%d", p));
	endrule

	rule r_tieOff_rx3;
		let raw <- upsize.pkg.get();
		dbg($format("[TCP tie off] rx_data=%d", raw.data));
	endrule
end // !en_rx

if (!en_tx) begin
	rule r_tieOff_tx;
		AxisTcpTxStatus p <- parse_from(tx_status);
		dbg($format("[TCP tie off] tx_status=%d", p));
	endrule
end

if (!en_open) begin
	rule r_tieOff_open;
		AxisTcpOpenStatus p <- parse_from(open_status);
		dbg($format("[TCP tie off] open_status=%d", p));
	endrule
end


	/****************************************************************
	 * Interfaces
	 ***************************************************************/

	method Action openConnections(Bit#(16) numConn, Address_t ipAddress, Port_t basePort);
		open_conn_reqs.enq(OpenConnReq {numConn:numConn, ipAddress:ipAddress, basePort:basePort});
	endmethod
	interface sessionIDs = toGet(sessions);

	method Action closeConnection(Bit#(16) sessionID);
		Bit#(TCP_CLOSE_CONN_LEN) enc_pkg = pack(AxisTcpCloseConn {
			sessionID: sessionID
		});
		close_connection.pkg.put(mkPkgWithKeep(enc_pkg, True));
	endmethod
	interface closedByClient = toGet(closedSessions);

	// announces a new tfer that is auto split into MSS-sized packets
	method Action announceTfer(Session_t id, Bit#(64) num_bytes) if (send_state == 0);
		dbg($format("[TCP TX] Announce %d bytes for sess %d", num_bytes, id));

		// first announcement, all subsequent ones are pipelined
		Bit#(64) pkt_len = min(fromInteger(mss), num_bytes);
		Bit#(TCP_TX_META_LEN) enc_pkg = pack(AxisTcpTxMeta {
			sessionID: id,
			length: truncate(pkt_len)
		});
		tx_meta.pkg.put(mkPkgWithKeep(enc_pkg, True));
		pending_tfer.enq(tuple2(id, num_bytes));

		retry_len <= truncate(pkt_len);
		send_state <= 1;
	endmethod

	// mainly used for debugging/benchmarking
	method Action overrideSession(Session_t s);
		session_override.enq(s);
	endmethod

	method Action putData(Bit#(user_api_width) data, Bit#(16) word_bytes);
		datawords_tx.enq(tuple2(data, word_bytes));
	endmethod

	method Action listenPorts(Bit#(16) numConn, Port_t basePort);
		listen_conn_reqs.enq(ListenConnReq {numConn:numConn, basePort:basePort});
	endmethod
	method Bool isListening() = listening._read;
	interface datawordRx = toGet(datawords_rx);

	interface TCP_fab fab;
		interface i_listen_port = listen_port.fab;
		interface i_port_status = port_status.fab;
		interface i_open_connection = open_connection.fab;
		interface i_open_status = open_status.fab;
		interface i_close_connection = close_connection.fab;
		interface i_notification = notification.fab;
		interface i_read_pkg = read_pkg.fab;
		interface i_rx_meta = rx_meta.fab;
		interface i_rx_data = rx_data.fab;
		interface i_tx_meta = tx_meta.fab;
		interface i_tx_data = tx_data.fab;
		interface i_tx_status = tx_status.fab;
	endinterface

endmodule

endpackage
