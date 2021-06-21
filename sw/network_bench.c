#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <tapasco.h>

#define PE_ID 555

#define GET_L(x) ((uint32_t) (x & 0xFFFFFFFF))
#define GET_H(x) ((uint32_t) ((x >> 32) & 0xFFFFFFFF))

#define NW_KERNEL_OFFS 0x10000
#define ADDR_AP_CTRL             (NW_KERNEL_OFFS + 0x0)
#define ADDR_IP_ADDR_DATA_0      (NW_KERNEL_OFFS + 0x10)
#define ADDR_BOARD_NUMBER_DATA_0 (NW_KERNEL_OFFS + 0x18)
#define ADDR_ARP_DATA_0          (NW_KERNEL_OFFS + 0x20)
#define ADDR_AXI00_PTR0_DATA_0   (NW_KERNEL_OFFS + 0x28)
#define ADDR_AXI00_PTR0_DATA_1   (NW_KERNEL_OFFS + 0x30)
#define ADDR_AXI01_PTR0_DATA_0   (NW_KERNEL_OFFS + 0x38)
#define ADDR_AXI01_PTR0_DATA_1   (NW_KERNEL_OFFS + 0x40)

enum mode_t {
	MODE_SRV_THR,
	MODE_CLT_THR,
	MODE_SRV_LAT,
	MODE_CLT_LAT,
	MODE_CLT_LAT_HS,
	MODE_SRV_LAT_UDP,
	MODE_CLT_LAT_UDP,
	MODE_SRV_THR_UDP,
	MODE_CLT_THR_UDP,

	MODE_SRV_THR_ETH,
	MODE_CLT_THR_ETH,
	MODE_SRV_LAT_ETH,
	MODE_CLT_LAT_ETH,

	NW_ONLY
};

/**
 * Helper
 */
uint32_t make_ip(uint8_t o0, uint8_t o1, uint8_t o2, uint8_t o3)
{ return (o0 << 24) | (o1 << 16) | (o2 << 8) | o3; }

void handle_error() {
	int l = tapasco_last_error_length();
	char *buf = (char *)malloc(sizeof(char) * l);
	tapasco_last_error_message(buf, l);
	printf("ERROR: %s\n", buf);
	free(buf);
}

uint32_t parse_cli(int argc, char **argv) {
	if (argc < 2 )
		goto fail;
	char prot = argv[1][0];
	if (prot == 'n')
		return NW_ONLY;
	if (argc < 4)
		goto fail;
	char mode = argv[2][0];
	char role = argv[3][0];
	switch (prot) {
		case 't':
			if (mode == 't' && role == 's') return MODE_SRV_THR;
			if (mode == 't' && role == 'c') return MODE_CLT_THR;
			if (mode == 'l' && role == 's') return MODE_SRV_LAT;
			if (mode == 'l' && role == 'c') return MODE_CLT_LAT;
			if (mode == 'h' && role == 'c') return MODE_CLT_LAT_HS;
			break;
		case 'u':
			if (mode == 't' && role == 's') return MODE_SRV_THR_UDP;
			if (mode == 't' && role == 'c') return MODE_CLT_THR_UDP;
			if (mode == 'l' && role == 's') return MODE_SRV_LAT_UDP;
			if (mode == 'l' && role == 'c') return MODE_CLT_LAT_UDP;
			break;
		case 'e':
			if (mode == 't' && role == 's') return MODE_SRV_THR_ETH;
			if (mode == 't' && role == 'c') return MODE_CLT_THR_ETH;
			if (mode == 'l' && role == 's') return MODE_SRV_LAT_ETH;
			if (mode == 'l' && role == 'c') return MODE_CLT_LAT_ETH;
			break;
		default:
			break;
	}

fail:
	printf("Usage: PROT MODE ROLE\n");
	printf("PROT: u=UDP, t=TCP, e=Ethernet, n=NW_Kernel only\n");
	printf("MODE: t=throughput, l=latency, h=latency with handshake\n");
	printf("ROLE: s=server, c=client\n");
	exit(1);
}

char *mode2str(mode_t m) {
	char *s;
	switch(m) {
		case MODE_SRV_THR: s = "MODE_SRV_THR"; break;
		case MODE_CLT_THR: s = "MODE_CLT_THR"; break;
		case MODE_SRV_LAT: s = "MODE_SRV_LAT"; break;
		case MODE_CLT_LAT: s = "MODE_CLT_LAT"; break;
		case MODE_CLT_LAT_HS: s = "MODE_CLT_LAT_HS"; break;
		case MODE_SRV_LAT_UDP: s = "MODE_SRV_LAT_UDP"; break;
		case MODE_CLT_LAT_UDP: s = "MODE_CLT_LAT_UDP"; break;
		case MODE_SRV_THR_UDP: s = "MODE_SRV_THR_UDP"; break;
		case MODE_CLT_THR_UDP: s = "MODE_CLT_THR_UDP"; break;
		default: s = "Invalid";
	}
	return s;
}

void handle_throughput(uint64_t pe_ret, float f_mhz, uint64_t byte_per_tfer) {
	double f = 1e6 * f_mhz;

	double duration = pe_ret / f;
	double byte_per_s = byte_per_tfer / duration;
	double gbit_per_s = 8 * byte_per_s / 1e9;

	printf("Dev freq: %f MHz, Tfer len: %lu B, Tfer duration: %lf s\n",
		f_mhz, byte_per_tfer, duration);

	printf("R = %lf Gbit/s\n", gbit_per_s);
}

int main(int argc, char **argv) {
	int ret = 0;

	/**
	 * Config
	 */
	uint32_t src_ip_addr = make_ip(10, 3, 3, SRC_IP);
	uint32_t dst_ip_addr = make_ip(10, 3, 3, DST_IP);
	uint32_t arp = dst_ip_addr;
	uint32_t dst_port = 5001;

	uint32_t board_number = MYMAC; // MAC 00:0a:35:02:9d:eX
	uint32_t oper_mode = parse_cli(argc, argv);

	uint64_t byte_per_tfer = 100 * 1e9;
	uint32_t num_sessions = 50;

	// ethernet mode
	uint64_t src_mac = 0xDEADBEEF0000 + SRC_IP;
	uint64_t dst_mac = 0xDEADBEEF0000 + DST_IP;


	// initialize threadpool
	tapasco_init_logging();
	TLKM *t = tapasco_tlkm_new();
	if (t == 0) {
		handle_error();
		ret = -1;
		goto finish;
	}

	// Retrieve the number of devices from the runtime
	int num_devices = 0;
	if ((num_devices = tapasco_tlkm_device_len(t)) < 0) {
		handle_error();
		ret = -1;
		goto finish_tlkm;
	}

	if (num_devices == 0) {
		printf("No TaPaSCo devices found.\n");
		ret = -1;
		goto finish_tlkm;
	}

	// Allocates the first device
	Device *d = 0;
	if ((d = tapasco_tlkm_device_alloc(t, 0)) == 0) {
		handle_error();
		ret = -1;
		goto finish_tlkm;
	}

	PEId peid = PE_ID;
	if (tapasco_device_num_pes(d, peid) == 0) {
		printf("No matching PE found.\n");
		goto finish_device;
	}

	if (tapasco_device_access(d, TlkmAccessExclusive) < 0) {
		handle_error();
		ret = -1;
		goto finish_device;
	}

	// Acquire PE
	Job *j = tapasco_device_acquire_pe(d, PE_ID);
	if (j == 0) {
		handle_error();
		ret = -1;
		goto finish_device;
	}

	// get PE frequemcy
	float f_mhz = tapasco_device_design_frequency(d);

	/*
	 * NETWORK Kernel
	 */
	if (oper_mode == NW_ONLY) {
		// Allocate mem for rx, tx buffs
		printf("Allocating buffers ...\n");
		const size_t max_sess = 1000;
		const size_t buf_len = max_sess * (1 << 18);
		TapascoOffchipMemory *mem = tapasco_get_default_memory(d);
		DeviceAddress buf0 = tapasco_memory_allocate(mem, buf_len);
		DeviceAddress buf1 = tapasco_memory_allocate(mem, buf_len);

		printf("Writing nw kernel args ...\n");
		tapasco_write_addr(j, ADDR_IP_ADDR_DATA_0, src_ip_addr);
		tapasco_write_addr(j, ADDR_BOARD_NUMBER_DATA_0, board_number);
		tapasco_write_addr(j, ADDR_ARP_DATA_0, arp);

		tapasco_write_addr(j, ADDR_AXI00_PTR0_DATA_0, GET_L(buf0));
		tapasco_write_addr(j, ADDR_AXI00_PTR0_DATA_1, GET_H(buf0));
		tapasco_write_addr(j, ADDR_AXI01_PTR0_DATA_0, GET_L(buf1));
		tapasco_write_addr(j, ADDR_AXI01_PTR0_DATA_1, GET_H(buf1));

		// start nw kernel execution
		printf("Starting nw kernel ...\n");
		tapasco_write_addr(j, ADDR_AP_CTRL, 1);
	}

	/*
	 * USER Kernel
	 */
	if (oper_mode != NW_ONLY) {
		// Create argument list
		JobList *jl = tapasco_job_param_new();

		if (oper_mode >= MODE_SRV_THR_ETH) {
			printf("Ethernet mode\n");
			oper_mode -= MODE_SRV_THR_ETH;
			tapasco_job_param_single32(oper_mode,     jl);
			tapasco_job_param_single64(src_mac,       jl);
			tapasco_job_param_single64(dst_mac,       jl);
			tapasco_job_param_single64(byte_per_tfer, jl);
		} else {
			tapasco_job_param_single32(oper_mode,     jl);
			tapasco_job_param_single32(dst_ip_addr,   jl);
			tapasco_job_param_single32(dst_port,      jl);
			tapasco_job_param_single64(byte_per_tfer, jl);
			tapasco_job_param_single32(num_sessions,  jl);
		}
	
		// start user kernel execution
		printf("Starting user kernel, mode %d (%s)\n", oper_mode, mode2str(oper_mode));
		if (tapasco_job_start(j, &jl) < 0) {
			handle_error();
			ret = -1;
			goto finish_device;
		}

		printf("Waiting for user kernel ...\n");
		if (oper_mode == MODE_SRV_THR || oper_mode == MODE_SRV_THR_UDP)
			printf("-> Expecting %f Gbyte\n", byte_per_tfer/1e9);
		else if (oper_mode == MODE_CLT_THR || oper_mode == MODE_CLT_THR_UDP)
			printf("-> Sending %f Gbyte (%d sess)\n", byte_per_tfer/1e9, num_sessions);

		uint64_t pe_ret;
		if (tapasco_job_release(j, &pe_ret, true) < 0) {
			handle_error();
			ret = -1;
			goto finish_device;
		}
		printf("===========================\n");
		printf("PE return value: %lu\n", pe_ret);

		if (pe_ret > 0) {
			if (oper_mode == MODE_SRV_THR || oper_mode == MODE_CLT_THR || oper_mode == MODE_SRV_THR_UDP || oper_mode == MODE_CLT_THR_UDP)
				handle_throughput(pe_ret, f_mhz, byte_per_tfer);
			else
				printf("Latency: %ld cycles = %ld ns\n", pe_ret, 4*pe_ret);
		}
	}

finish_device:
	tapasco_tlkm_device_destroy(d);
finish_tlkm:
	tapasco_tlkm_destroy(t);
finish:
	return ret;
}
