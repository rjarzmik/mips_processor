-------------------------------------------------------------------------------
-- Title      : Testbench for design "MIPS_CPU"
-- Project    : 
-------------------------------------------------------------------------------
-- File       : MIPS_CPU_tb.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-12
-- Last update: 2016-12-09
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-12  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cpu_defs.all;
use work.instruction_defs.instr_tag_t;

-------------------------------------------------------------------------------

entity MIPS_CPU_tb is

end entity MIPS_CPU_tb;

-------------------------------------------------------------------------------

architecture rtl of MIPS_CPU_tb is

  -- component generics
  constant ADDR_WIDTH           : integer := 32;
  constant DATA_WIDTH           : integer := 32;
  constant NB_REGISTERS_GP      : integer := 32;
  constant NB_REGISTERS_SPECIAL : integer := 2;

  -- clock
  signal Clk  : std_logic := '1';
  signal Rst  : std_logic := '1';
  signal stop : std_logic := '0';

  -- L2 connections
  signal o_L2c_req       : std_logic;
  signal o_L2c_addr      : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal i_L2c_read_data : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal i_L2c_valid     : std_logic;
  -- Debug signals
  signal dbg_if_pc       : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal dbg_di_pc       : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal dbg_ex_pc       : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal dbg_wb_pc       : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal dbg_commited_pc : std_logic_vector(ADDR_WIDTH - 1 downto 0);

  signal dbg_pc_killed   : std_logic;
  signal dbg_ife_killed  : std_logic;
  signal dbg_di_killed   : std_logic;
  signal dbg_ex_killed   : std_logic;
  signal dbg_wb_killed   : std_logic;
  signal dbg_pc_stalled  : std_logic;
  signal dbg_ife_stalled : std_logic;
  signal dbg_di_stalled  : std_logic;
  signal dbg_ex_stalled  : std_logic;
  signal dbg_wb_stalled  : std_logic;

  signal dbg_jump_pc            : std_logic;
  signal dbg_jump_target        : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal dbg_commited_instr_tag : instr_tag_t;

  signal dbg_wb2di_reg1 : register_port_type;
  signal dbg_wb2di_reg2 : register_port_type;

  function dbg_get_stage_letter(
    rst : std_logic; kill : std_logic; stall : std_logic)
    return string is
    variable o : string(1 to 1);
  begin
    if rst = '1' then
      o := "R";
    elsif kill = '1' then
      o := "K";
    elsif stall = '1' then
      o := "S";
    else
      o := "-";
    end if;
    return o;
  end function dbg_get_stage_letter;

  function dbg_get_stage_string(
    stage : string;
    rst   : std_logic; kill : std_logic; stall : std_logic;
    pc    : std_logic_vector(ADDR_WIDTH - 1 downto 0)) return string is
    constant length : integer := stage'length + 4;
    variable head   : string(1 to length);
  begin
    head(1 to stage'length) := stage;
    head(stage'length + 1)  := '(';
    head(stage'length + 2 to stage'length + 2) :=
      dbg_get_stage_letter(rst, kill, stall);
    head(stage'length + 3 to stage'length + 4) := ")@";
    return head & to_hstring(pc);
  end function dbg_get_stage_string;

  function dbg_get_done_string(
    stage : string;
    rst   : std_logic;
    itag  : instr_tag_t;
    pc    : std_logic_vector(ADDR_WIDTH - 1 downto 0)) return string is
    constant length : integer := stage'length + 6;
    variable head   : string(1 to length);
    variable state  : string(1 to 3);
  begin
    if itag.is_branch then
      if itag.is_branch_taken then
        state := "BR+";
      else
        state := "BR-";
      end if;
    elsif itag.is_ja then
      state := "JA+";
    elsif itag.is_jr then
      state := "JR+";
    else
      state := "---";
    end if;
    head(1 to stage'length)                    := stage;
    head(stage'length + 1)                     := '(';
    head(stage'length + 2 to stage'length + 4) := state;
    head(stage'length + 5 to stage'length + 6) := ")@";
    return head & to_hstring(pc);
  end function dbg_get_done_string;

  function dbg_get_regname(regnum : natural) return string is
    variable name : string(1 to 4);
  begin
    case regnum is
      when 0      => name := "zero";
      when 1      => name := "at  ";
      when 2      => name := "v0  ";
      when 3      => name := "v1  ";
      when 4      => name := "a0  ";
      when 5      => name := "a1  ";
      when 6      => name := "a2  ";
      when 7      => name := "a3  ";
      when 8      => name := "t0  ";
      when 9      => name := "t1  ";
      when 10     => name := "t2  ";
      when 11     => name := "t3  ";
      when 12     => name := "t4  ";
      when 13     => name := "t5  ";
      when 14     => name := "t6  ";
      when 15     => name := "t7  ";
      when 16     => name := "s0  ";
      when 17     => name := "s1  ";
      when 18     => name := "s2  ";
      when 19     => name := "s3  ";
      when 20     => name := "s4  ";
      when 21     => name := "s5  ";
      when 22     => name := "s6  ";
      when 23     => name := "s7  ";
      when 24     => name := "t8  ";
      when 25     => name := "t9  ";
      when 26     => name := "k0  ";
      when 27     => name := "k1  ";
      when 28     => name := "gp  ";
      when 29     => name := "sp  ";
      when 30     => name := "fp  ";
      when 31     => name := "ra  ";
      when 32     => name := "mflo";
      when 33     => name := "mfhi";
      when others => name := "    ";
    end case;
    return name;
  end function dbg_get_regname;

  function dbg_get_regwrite_string(ireg : register_port_type) return string is
    variable rname : string(1 to 4);
    variable rlen  : natural := 4;
  begin
    rname := dbg_get_regname(ireg.idx);
    for i in 4 downto 1 loop
      if rname(i) = ' ' then
        rlen := rlen - 1;
      end if;
    end loop;

    if ireg.we = '1' then
      return "$" & rname(1 to rlen) & "=0x" & to_hstring(ireg.data) & " ";
    else
      return "";
    end if;
  end function dbg_get_regwrite_string;

  function dbg_get_jump_string(is_jump     : std_logic;
                               jump_target : std_logic_vector)
    return string is
  begin
    if is_jump = '1' then
      return "$pc<=0x" & to_hstring(jump_target) & " ";
    else
      return "";
    end if;
  end function dbg_get_jump_string;

begin  -- architecture rtl

  -- component instantiation
  DUT : entity work.MIPS_CPU
    generic map (
      ADDR_WIDTH           => ADDR_WIDTH,
      DATA_WIDTH           => DATA_WIDTH,
      NB_REGISTERS_GP      => NB_REGISTERS_GP,
      NB_REGISTERS_SPECIAL => NB_REGISTERS_SPECIAL)
    port map (
      clk                      => clk,
      rst                      => rst,
      o_L2c_req                => o_L2c_req,
      o_L2c_addr               => o_L2c_addr,
      i_L2c_read_data          => i_L2c_read_data,
      i_L2c_valid              => i_L2c_valid,
      o_dbg_if_pc              => dbg_if_pc,
      o_dbg_di_pc              => dbg_di_pc,
      o_dbg_ex_pc              => dbg_ex_pc,
      o_dbg_wb_pc              => dbg_wb_pc,
      o_dbg_commited_pc        => dbg_commited_pc,
      o_dbg_pc_killed          => dbg_pc_killed,
      o_dbg_ife_killed         => dbg_ife_killed,
      o_dbg_di_killed          => dbg_di_killed,
      o_dbg_ex_killed          => dbg_ex_killed,
      o_dbg_wb_killed          => dbg_wb_killed,
      o_dbg_pc_stalled         => dbg_pc_stalled,
      o_dbg_ife_stalled        => dbg_ife_stalled,
      o_dbg_di_stalled         => dbg_di_stalled,
      o_dbg_ex_stalled         => dbg_ex_stalled,
      o_dbg_wb_stalled         => dbg_wb_stalled,
      o_dbg_jump_pc            => dbg_jump_pc,
      o_dbg_jump_target        => dbg_jump_target,
      o_dbg_commited_instr_tag => dbg_commited_instr_tag,
      o_dbg_wb2di_reg1         => dbg_wb2di_reg1,
      o_dbg_wb2di_reg2         => dbg_wb2di_reg2
      );

  Simulated_Memory_1 : entity work.Simulated_Memory
    generic map (
      ADDR_WIDTH     => ADDR_WIDTH,
      DATA_WIDTH     => DATA_WIDTH,
      MEMORY_LATENCY => 1)
    port map (
      clk                 => clk,
      rst                 => rst,
      i_memory_req        => o_L2c_req,
      i_memory_we         => '0',
      i_memory_addr       => o_L2c_addr,
      i_memory_write_data => (others => 'X'),
      o_memory_read_data  => i_L2c_read_data,
      o_memory_valid      => i_L2c_valid);

  -- reset
  Rst <= '0' or stop after 30 ps;
  -- clock generation
  Clk <= not Clk     after 5 ps;

  -- waveform generation
  WaveGen_Proc : process
  begin
    -- insert signal assignments here

    wait until Clk = '1';
  end process WaveGen_Proc;

  debug_proc : process(clk, rst)
    variable cycle           : integer                                   := 1;
    variable unusable_op     : std_logic_vector(DATA_WIDTH - 1 downto 0) := (others => 'X');
    variable passed_by_addr0 : natural                                   := 0;
    variable fkill           : std_logic;
  begin
    if dbg_ife_killed = '1' or dbg_jump_pc = '1' then
      fkill := '1';
    else
      fkill := '0';
    end if;

    if rising_edge(clk) then
      cycle := cycle + 1;
      report "[" & integer'image(cycle) & "] " &
        dbg_get_stage_string("if", rst, fkill, dbg_ife_stalled, dbg_if_pc) & " " &
        dbg_get_stage_string("di", rst, dbg_di_killed, dbg_di_stalled, dbg_di_pc) & " " &
        dbg_get_stage_string("ex", rst, dbg_ex_killed, dbg_ex_stalled, dbg_ex_pc) & " " &
        dbg_get_stage_string("wb", rst, dbg_wb_killed, dbg_wb_stalled, dbg_wb_pc) & " " &
        dbg_get_done_string("done", rst, dbg_commited_instr_tag, dbg_commited_pc) & " " &
        dbg_get_regwrite_string(dbg_wb2di_reg1) &
        dbg_get_regwrite_string(dbg_wb2di_reg2) &
        dbg_get_jump_string(dbg_jump_pc, dbg_jump_target);
      if dbg_commited_pc /= unusable_op then
        if to_integer(unsigned(dbg_commited_pc)) = 0 then
          passed_by_addr0 := passed_by_addr0 + 1;
        end if;
      end if;

      if passed_by_addr0 > 1 then
        report "PC rolled over to 0, ending simulation." severity error;
        stop <= '1';
      end if;
    end if;
  end process debug_proc;

end architecture rtl;

-------------------------------------------------------------------------------

configuration MIPS_CPU_tb_rtl_cfg of MIPS_CPU_tb is
  for rtl
  end for;
end MIPS_CPU_tb_rtl_cfg;

-------------------------------------------------------------------------------
