###
# DO NOT CHANGE
###
TOP_MODULE=mkRawAurora_perf_PE
TESTBENCH_MODULE=mkTestbench
IGNORE_MODULES=mkTestbench mkTestsMainTest
MAIN_MODULE=RawAurora_perf_PE
TESTBENCH_FILE=src/Testbench.bsv

# Initialize
VIVADO_ADD_PARAMS := ''
CONSTRAINT_FILES := ''
EXTRA_BSV_LIBS += $(PWD)/../../bsv_nw_lib/libraries/BlueAXI/src
EXTRA_BSV_LIBS += $(PWD)/../../bsv_nw_lib/libraries/BlueLib/src
EXTRA_LIBRARIES:=
RUN_FLAGS:=
BASE_DIR:=$(PWD)/../../bsv_nw_lib/BSVTools

PROJECT_NAME=RawAurora_perf_PE

ifeq ($(RUN_TEST),)
RUN_TEST=TestsMainTest
endif

# Default flags
EXTRA_FLAGS=-D "RUN_TEST=$(RUN_TEST)" -D "TESTNAME=mk$(RUN_TEST)"
EXTRA_FLAGS+=-show-schedule -D "BSV_TIMESCALE=1ns/1ps -keep-fires"

###
# User configuration
###

# Comment the following line if -O3 should be used during compilation
# Keep uncommented for short running simulations
CXX_NO_OPT := 1

# Any additional files added during compilation
# For instance for BDPI or Verilog/VHDL files for simulation
# CPP_FILES += $(current_dir)/src/mem_sim.cpp

# Custom defines added to compile steps
# EXTRA_FLAGS+=-D "BENCHMARK=1"

# Flags added to simulator execution
# RUN_FLAGS+=-V dump.vcd

# Add additional parameters for IP-XACT generation. Passed directly to Vivado.
# Any valid TCL during packaging is allowed
# Typically used to fix automatic inference for e.g. clock assignments
VIVADO_ADD_PARAMS += "ipx::associate_bus_interfaces -busif S00_AXI -clock ap_clk [ipx::current_core]"
VIVADO_ADD_PARAMS += "ipx::associate_bus_interfaces -busif axis_net_tx_0 -clock ap_clk [ipx::current_core]"
VIVADO_ADD_PARAMS += "ipx::associate_bus_interfaces -busif axis_net_rx_0 -clock ap_clk [ipx::current_core]"

# Add custom constraint files, Syntax: Filename,Load Order
# CONSTRAINT_FILES += "$(PWD)/constraints/custom.xdc,LATE"

# Do not change: Load libraries such as BlueAXI or BlueLib
ifneq ("$(wildcard $(PWD)/libraries/*/*.mk)", "")
include $(PWD)/libraries/*/*.mk
endif

# Do not change: Include base makefile
include $(PWD)/../../bsv_nw_lib/BSVTools/scripts/rules.mk
