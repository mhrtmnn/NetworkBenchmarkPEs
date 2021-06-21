###
# DO NOT CHANGE
###
TOP_MODULE=mkAxisWidthConverter
TESTBENCH_MODULE=mkTestbench
IGNORE_MODULES=mkTestbench
MAIN_MODULE=AxisWidthConverter
TESTBENCH_FILE=src/Testbench.bsv

# Initialize
VIVADO_ADD_PARAMS := ''
CONSTRAINT_FILES := ''
EXTRA_BSV_LIBS += $(PWD)/../BlueAXI/src
EXTRA_BSV_LIBS += $(PWD)/../BlueLib/src
EXTRA_LIBRARIES:=
RUN_FLAGS:=
BASE_DIR:=$(PWD)/../../BSVTools/

PROJECT_NAME=AxisWidthConverter

ifeq ($(RUN_TEST),)
RUN_TEST=TestsMainTest
endif

# Default flags
EXTRA_FLAGS=-D "RUN_TEST=$(RUN_TEST)" -D "TESTNAME=mk$(RUN_TEST)"
EXTRA_FLAGS+=-show-schedule -D "BSV_TIMESCALE=1ns/1ps" -keep-fires

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
# VIVADO_ADD_PARAMS += 'ipx::associate_bus_interfaces -busif M_AXI -clock sconfig_axi_aclk [ipx::current_core]'

# Add custom constraint files, Syntax: Filename,Load Order
# CONSTRAINT_FILES += "$(PWD)/constraints/custom.xdc,LATE"

# Do not change: Load libraries such as BlueAXI or BlueLib
ifneq ("$(wildcard $(PWD)/libraries/*/*.mk)", "")
include $(PWD)/libraries/*/*.mk
endif

# Do not change: Include base makefile
include $(PWD)/../../BSVTools/scripts/rules.mk
