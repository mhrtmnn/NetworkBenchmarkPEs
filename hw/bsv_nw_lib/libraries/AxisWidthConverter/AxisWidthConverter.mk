MAKEPATH := $(dir $(lastword $(MAKEFILE_LIST)))
MODULENAME := AxisWidthConverter
MODULEPATH := $(MAKEPATH)src
EXTRA_BSV_LIBS += $(MODULEPATH)

$(info Adding $(MODULENAME) from $(MODULEPATH))