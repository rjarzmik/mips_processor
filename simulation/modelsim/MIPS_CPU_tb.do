vsim work.mips_cpu_tb

add wave -group Overview -position end -radix hex sim:/mips_cpu_tb/DUT/clk
add wave -group Overview -position end -radix hex sim:/mips_cpu_tb/DUT/o_dbg_*_pc
add wave -group Overview -position end -radix hex sim:/mips_cpu_tb/DUT/di_instr_tag.tag
add wave -group Overview -position end -radix hex sim:/mips_cpu_tb/DUT/ex_instr_tag.tag
add wave -group Overview -position end -radix hex sim:/mips_cpu_tb/DUT/mem_m0_instr_tag.tag
add wave -group Overview -position end -radix hex sim:/mips_cpu_tb/DUT/mem_m1_instr_tag.tag
add wave -group Overview -position end -radix hex sim:/mips_cpu_tb/DUT/mem_m2_instr_tag.tag
add wave -group Overview -position end -radix hex sim:/mips_cpu_tb/DUT/wb_instr_tag.tag
add wave -group Overview -position end -radix hex sim:/mips_cpu_tb/DUT/commited_instr_tag.tag


add wave -group MIPS_CPU -position end -radix hex sim:/mips_cpu_tb/DUT/clk
add wave -group MIPS_CPU -group ports -position end -radix hex -ports sim:/mips_cpu_tb/DUT/*
add wave -group MIPS_CPU -group internals -position end -radix hex -internal sim:/mips_cpu_tb/DUT/*

add wave -group Fetch -group ports -ports -position end -radix hex sim:/mips_cpu_tb/DUT/ife/*
add wave -group Decode -group ports -ports -position end -radix hex sim:/mips_cpu_tb/DUT/di/*
add wave -group Execute -group ports -ports -position end -radix hex sim:/mips_cpu_tb/DUT/ex/*
add wave -group Writeback -group ports -ports -position end -radix hex sim:/mips_cpu_tb/DUT/wb/*
add wave -group Ctrl-Decode -group ports -ports -position end -radix hex sim:/mips_cpu_tb/DUT/ctrl_decode_deps/*
add wave -group PC-Register -group ports -ports -position end -radix hex sim:/mips_cpu_tb/DUT/ife/pc_reg/*
add wave -group I-Tracker -group ports -ports -position end -radix hex sim:/mips_cpu_tb/DUT/ife/pc_reg/itracker/*

add wave -group Fetch -group internals -internal -position end -radix hex sim:/mips_cpu_tb/DUT/ife/*
add wave -group Decode -group internals -internal -position end -radix hex sim:/mips_cpu_tb/DUT/di/*
add wave -group Execute -group internals -internal -position end -radix hex sim:/mips_cpu_tb/DUT/ex/*
add wave -group Writeback -group internals -internal -position end -radix hex sim:/mips_cpu_tb/DUT/wb/*
#add wave -group Ctrl-Decode -group internals -internal -position end -radix hex sim:/mips_cpu_tb/DUT/ctrl_decode_deps/*
add wave -group PC-Register -group internals -internal -position end -radix hex sim:/mips_cpu_tb/DUT/ife/pc_reg/*
#add wave -group Register-File -group internals -internal -position end -radix hex sim:/mips_cpu_tb/DUT/di/rfile/registers
add wave -group I-Tracker -group internals -internal -position end -radix hex sim:/mips_cpu_tb/DUT/ife/pc_reg/itracker/*

add wave -group Fetch -group IProvider -group ports -ports -position end -radix hex sim:/mips_cpu_tb/DUT/ife/iprovider/*
add wave -group Fetch -group IProvider -group internal -internal -position end -radix hex sim:/mips_cpu_tb/DUT/ife/iprovider/*

add wave -group I-Mispredict -group ports -ports -position end -radix hex sim:/mips_cpu_tb/DUT/ife/pc_reg/mispredictor/*
add wave -group I-Mispredict -group internals -internal -position end -radix hex sim:/mips_cpu_tb/DUT/ife/pc_reg/mispredictor/*

add wave -group I-Predict -group ports -ports -position end -radix hex sim:/mips_cpu_tb/DUT/ife/pc_reg/predictor/*
add wave -group I-Predict -group internals -internal -position end -radix hex sim:/mips_cpu_tb/DUT/ife/pc_reg/predictor/*


add wave -group Register-File -group internals -internal -position end -radix hex -label at sim:/mips_cpu_tb/DUT/di/rfile/registers(1)
add wave -group Register-File -group internals -internal -position end -radix hex -label v0 sim:/mips_cpu_tb/DUT/di/rfile/registers(2)
add wave -group Register-File -group internals -internal -position end -radix hex -label v1 sim:/mips_cpu_tb/DUT/di/rfile/registers(3)
add wave -group Register-File -group internals -internal -position end -radix hex -label a0 sim:/mips_cpu_tb/DUT/di/rfile/registers(4)
add wave -group Register-File -group internals -internal -position end -radix hex -label a1 sim:/mips_cpu_tb/DUT/di/rfile/registers(5)
add wave -group Register-File -group internals -internal -position end -radix hex -label a2 sim:/mips_cpu_tb/DUT/di/rfile/registers(6)
add wave -group Register-File -group internals -internal -position end -radix hex -label a3 sim:/mips_cpu_tb/DUT/di/rfile/registers(7)
add wave -group Register-File -group internals -internal -position end -radix hex -label t0 sim:/mips_cpu_tb/DUT/di/rfile/registers(8)
add wave -group Register-File -group internals -internal -position end -radix hex -label t1 sim:/mips_cpu_tb/DUT/di/rfile/registers(9)
add wave -group Register-File -group internals -internal -position end -radix hex -label t2 sim:/mips_cpu_tb/DUT/di/rfile/registers(10)
add wave -group Register-File -group internals -internal -position end -radix hex -label t3 sim:/mips_cpu_tb/DUT/di/rfile/registers(11)
add wave -group Register-File -group internals -internal -position end -radix hex -label t4 sim:/mips_cpu_tb/DUT/di/rfile/registers(12)
add wave -group Register-File -group internals -internal -position end -radix hex -label t5 sim:/mips_cpu_tb/DUT/di/rfile/registers(13)
add wave -group Register-File -group internals -internal -position end -radix hex -label t6 sim:/mips_cpu_tb/DUT/di/rfile/registers(14)
add wave -group Register-File -group internals -internal -position end -radix hex -label t7 sim:/mips_cpu_tb/DUT/di/rfile/registers(15)
add wave -group Register-File -group internals -internal -position end -radix hex -label s0 sim:/mips_cpu_tb/DUT/di/rfile/registers(16)
add wave -group Register-File -group internals -internal -position end -radix hex -label s1 sim:/mips_cpu_tb/DUT/di/rfile/registers(17)
add wave -group Register-File -group internals -internal -position end -radix hex -label s2 sim:/mips_cpu_tb/DUT/di/rfile/registers(18)
add wave -group Register-File -group internals -internal -position end -radix hex -label s3 sim:/mips_cpu_tb/DUT/di/rfile/registers(19)
add wave -group Register-File -group internals -internal -position end -radix hex -label s4 sim:/mips_cpu_tb/DUT/di/rfile/registers(20)
add wave -group Register-File -group internals -internal -position end -radix hex -label s5 sim:/mips_cpu_tb/DUT/di/rfile/registers(21)
add wave -group Register-File -group internals -internal -position end -radix hex -label s6 sim:/mips_cpu_tb/DUT/di/rfile/registers(22)
add wave -group Register-File -group internals -internal -position end -radix hex -label s7 sim:/mips_cpu_tb/DUT/di/rfile/registers(23)
add wave -group Register-File -group internals -internal -position end -radix hex -label t8 sim:/mips_cpu_tb/DUT/di/rfile/registers(24)
add wave -group Register-File -group internals -internal -position end -radix hex -label t9 sim:/mips_cpu_tb/DUT/di/rfile/registers(25)
add wave -group Register-File -group internals -internal -position end -radix hex -label k0 sim:/mips_cpu_tb/DUT/di/rfile/registers(26)
add wave -group Register-File -group internals -internal -position end -radix hex -label k1 sim:/mips_cpu_tb/DUT/di/rfile/registers(27)
add wave -group Register-File -group internals -internal -position end -radix hex -label gp sim:/mips_cpu_tb/DUT/di/rfile/registers(28)
add wave -group Register-File -group internals -internal -position end -radix hex -label sp sim:/mips_cpu_tb/DUT/di/rfile/registers(29)
add wave -group Register-File -group internals -internal -position end -radix hex -label fp sim:/mips_cpu_tb/DUT/di/rfile/registers(30)
add wave -group Register-File -group internals -internal -position end -radix hex -label ra sim:/mips_cpu_tb/DUT/di/rfile/registers(31)


run 2500ps
wave zoom range 0ps 240ps
