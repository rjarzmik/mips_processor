GHDL ?= ghdl

all: $(OUT)/MIPS_Processor/compiled | checkenv

clean: checkenv
	@rm -rf $(OUT)

include Makefile.compiler_rules

include rjarzmik/Makefile.module
include Caches/Makefile.module
include Control/Makefile.module
include IF/Makefile.module
include DI/Makefile.module
include EX/Makefile.module
include MEM/Makefile.module
include MIPS_Processor/Makefile.module
include ProgramCounter/Makefile.module
include WB/Makefile.module
