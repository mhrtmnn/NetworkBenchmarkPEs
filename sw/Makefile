SRC     = network_bench.c
LDFLAGS = -ltapasco -lpthread -ldl -lrt -lm

# Path to TaPaSCo installation
ifndef TPSC_RT
$(error TPSC_RT is not set. Set to <...>/tapasco/runtime)
endif

all:
	cd $(TPSC_RT)/libtapasco && cargo build --release
	$(CC) $(SRC) -o network_bench -I $(TPSC_RT)/build/libtapasco/ -L $(TPSC_RT)/libtapasco/target/release/ $(LDFLAGS) -DMYMAC=0 -DSRC_IP=44 -DDST_IP=33
