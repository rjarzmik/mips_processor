-------------------------------------------------------------------------------
-- Title      : Testbench for design "MIPS_CPU"
-- Project    : 
-------------------------------------------------------------------------------
-- File       : MIPS_CPU_tb.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-12
-- Last update: 2017-02-14
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
use work.cache_defs.all;
use work.instruction_defs.instr_tag_t;
use work.instruction_defs.INSTR_TAG_FIRST_VALID;
use work.instruction_prediction.prediction_t;

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
  constant DEBUG                : boolean := false;

  -- clock
  signal Clk  : std_logic := '1';
  signal Rst  : std_logic := '1';
  signal stop : std_logic := '0';

  -- L2/Memory connections
  signal o_memory_req        : std_logic := '0';
  signal o_memory_we         : std_logic := '0';
  signal o_memory_addr       : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal o_memory_write_data : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal i_memory_read_data  : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal i_memory_valid      : std_logic;

  -- Temprorary Data Memory interface
  signal o_mem_addr       : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal i_mem_rd_valid   : std_logic;
  signal i_mem_rd_data    : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal o_mem_wr_en      : std_logic;
  signal o_mem_word_width : std_logic;
  signal o_mem_wr_data    : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal i_mem_wr_ack     : std_logic;

  -- Debug signals
  signal dbg_if_pc       : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal dbg_di_pc       : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal dbg_ex_pc       : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal dbg_mem_m0_pc   : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal dbg_mem_m1_pc   : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal dbg_mem_m2_pc   : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal dbg_wb_pc       : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal dbg_commited_pc : std_logic_vector(ADDR_WIDTH - 1 downto 0);

  signal dbg_ife_killed  : std_logic;
  signal dbg_di_killed   : std_logic;
  signal dbg_ex_killed   : std_logic;
  signal dbg_mem_killed  : std_logic;
  signal dbg_wb_killed   : std_logic;
  signal dbg_pc_stalled  : std_logic;
  signal dbg_ife_stalled : std_logic;
  signal dbg_di_stalled  : std_logic;
  signal dbg_ex_stalled  : std_logic;
  signal dbg_mem_stalled : std_logic;
  signal dbg_wb_stalled  : std_logic;

  signal dbg_jump_pc     : std_logic;
  signal dbg_jump_target : std_logic_vector(ADDR_WIDTH - 1 downto 0);

  signal dbg_wb2di_reg1 : register_port_type;
  signal dbg_wb2di_reg2 : register_port_type;

  signal dbg_if_itag       : instr_tag_t;
  signal dbg_di_itag       : instr_tag_t;
  signal dbg_ex_itag       : instr_tag_t;
  signal dbg_mem_m0_itag  : instr_tag_t;
  signal dbg_mem_m1_itag  : instr_tag_t;
  signal dbg_mem_m2_itag  : instr_tag_t;
  signal dbg_wb_itag       : instr_tag_t;
  signal dbg_commited_itag : instr_tag_t;
  signal dbg_if_prediction : prediction_t;

  function dbg_get_stage_letter(
    rst  : std_logic; kill : std_logic; stall : std_logic;
    itag : instr_tag_t)
    return string is
    variable o : string(1 to 2);
  begin
    if rst = '1' then
      o := " R";
    elsif kill = '1' then
      o := " K";
    elsif stall = '1' then
      o := " S";
    elsif not itag.valid then
      o := " -";
    else
      if itag.tag < 10 then
        o := ' ' & integer'image(itag.tag);
      else
        o := integer'image(itag.tag);
      end if;
    end if;
    return o;
  end function dbg_get_stage_letter;

  function dbg_get_stage_string(
    stage : string;
    rst   : std_logic; kill : std_logic; stall : std_logic;
    pc    : std_logic_vector(ADDR_WIDTH - 1 downto 0);
    itag  : instr_tag_t) return string is
    constant length : integer := stage'length + 5;
    variable head   : string(1 to length);
  begin
    head(1 to stage'length) := stage;
    head(stage'length + 1)  := '(';
    head(stage'length + 2 to stage'length + 3) :=
      dbg_get_stage_letter(rst, kill, stall, itag);

    if stage /= "di" then
      head(stage'length + 4 to stage'length + 4) := ")";
      return head(1 to length - 1);
    else
      head(stage'length + 4 to stage'length + 5) := ")@";
      return head & to_hstring(pc);
    end if;
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

  function dbg_get_prediction_string(p               : prediction_t;
                                     last_prediction : prediction_t)
    return string is
    variable move : string(1 to 4);
  begin
    if last_prediction.pc = p.pc and
      last_prediction.next_pc = p.next_pc and
      last_prediction.take_branch = p.take_branch
    then
      -- Nothing changed
      return "";
    else
      if p.is_ja_jr then
        move := "JUMP";
      elsif p.is_branch then
        case p.take_branch is
          when 0 => move := "BR--";
          when 1 => move := "BR- ";
          when 2 => move := "BR+ ";
          when 3 => move := "BR++";
        end case;
      else
        move := "----";
      end if;

      if p.valid then
        return to_hstring(p.pc) & "->" & to_hstring(p.next_pc) & "=" & move;
      else
        return "";
      end if;
    end if;
  end function dbg_get_prediction_string;

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
      o_mem_addr               => o_mem_addr,
      i_mem_rd_valid           => i_mem_rd_valid,
      i_mem_rd_data            => i_mem_rd_data,
      o_mem_wr_en              => o_mem_wr_en,
      o_mem_word_width         => o_mem_word_width,
      o_mem_wr_data            => o_mem_wr_data,
      i_mem_wr_ack             => i_mem_wr_ack,
      o_memory_req             => o_memory_req,
      o_memory_we              => o_memory_we,
      o_memory_addr            => o_memory_addr,
      i_memory_rdata           => i_memory_read_data,
      o_memory_wdata           => o_memory_write_data,
      i_memory_done            => i_memory_valid,
      o_dbg_if_pc              => dbg_if_pc,
      o_dbg_di_pc              => dbg_di_pc,
      o_dbg_ex_pc              => dbg_ex_pc,
      o_dbg_mem_m0_pc          => dbg_mem_m0_pc,
      o_dbg_mem_m1_pc          => dbg_mem_m1_pc,
      o_dbg_mem_m2_pc          => dbg_mem_m2_pc,
      o_dbg_wb_pc              => dbg_wb_pc,
      o_dbg_commited_pc        => dbg_commited_pc,
      o_dbg_ife_killed         => dbg_ife_killed,
      o_dbg_di_killed          => dbg_di_killed,
      o_dbg_ex_killed          => dbg_ex_killed,
      o_dbg_mem_killed         => dbg_mem_killed,
      o_dbg_wb_killed          => dbg_wb_killed,
      o_dbg_pc_stalled         => dbg_pc_stalled,
      o_dbg_ife_stalled        => dbg_ife_stalled,
      o_dbg_di_stalled         => dbg_di_stalled,
      o_dbg_ex_stalled         => dbg_ex_stalled,
      o_dbg_mem_stalled        => dbg_mem_stalled,
      o_dbg_wb_stalled         => dbg_wb_stalled,
      o_dbg_jump_pc            => dbg_jump_pc,
      o_dbg_jump_target        => dbg_jump_target,
      o_dbg_commited_instr_tag => dbg_commited_itag,
      o_dbg_wb2di_reg1         => dbg_wb2di_reg1,
      o_dbg_wb2di_reg2         => dbg_wb2di_reg2,
      o_dbg_if_instr_tag       => dbg_if_itag,
      o_dbg_di_instr_tag       => dbg_di_itag,
      o_dbg_ex_instr_tag       => dbg_ex_itag,
      o_dbg_mem_m0_instr_tag   => dbg_mem_m0_itag,
      o_dbg_mem_m1_instr_tag   => dbg_mem_m1_itag,
      o_dbg_mem_m2_instr_tag   => dbg_mem_m2_itag,
      o_dbg_wb_instr_tag       => dbg_wb_itag,
      o_dbg_if_prediction      => dbg_if_prediction
      );

  -- memory simulator
  Simulated_Memory_1 : entity work.Simulated_Memory
    generic map (
      ADDR_WIDTH        => ADDR_WIDTH,
      DATA_WIDTH        => DATA_WIDTH,
      MEMORY_ADDR_WIDTH => 16,
      MEMORY_LATENCY    => 3,
      DEBUG             => DEBUG)
    port map (
      clk                 => clk,
      rst                 => rst,
      i_memory_req        => o_memory_req,
      i_memory_we         => o_memory_we,
      i_memory_addr       => o_memory_addr,
      i_memory_write_data => o_memory_write_data,
      o_memory_read_data  => i_memory_read_data,
      o_memory_valid      => i_memory_valid);

  -- reset
  Rst <= '0' or stop          after 30 ps;
  -- clock generation
  Clk <= not stop and not Clk after 5 ps;

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
    variable last_prediction : prediction_t;

  --alias dbg_mem1_pc is
  --  <<signal DUT.mem_stage.r1_dbg_mem_pc : std_logic_vector(ADDR_WIDTH -1 downto 0) >>;
  --alias dbg_mem2_pc is
  --  <<signal DUT.mem_stage.r2_dbg_mem_pc : std_logic_vector(ADDR_WIDTH -1 downto 0) >>;
  begin
    if not stop then
      if dbg_ife_killed = '1' or dbg_jump_pc = '1' then
        fkill := '1';
      else
        fkill := '0';
      end if;

      if rising_edge(clk) then
        cycle := cycle + 1;
        report "[" & integer'image(cycle) & "] " &
          dbg_get_stage_string("if", rst, fkill, dbg_ife_stalled, dbg_if_pc, dbg_if_itag) & " " &
          dbg_get_stage_string("di", rst, dbg_di_killed, dbg_di_stalled, dbg_di_pc, dbg_di_itag) & " " &
          dbg_get_stage_string("ex", rst, dbg_ex_killed, dbg_ex_stalled, dbg_ex_pc, dbg_ex_itag) & " " &
          dbg_get_stage_string("m0", rst, dbg_mem_killed, dbg_mem_stalled, dbg_mem_m0_pc, dbg_mem_m0_itag) & " " &
          dbg_get_stage_string("m1", rst, dbg_mem_killed, dbg_mem_stalled, dbg_mem_m1_pc, dbg_mem_m1_itag) & " " &
          dbg_get_stage_string("m2", rst, dbg_mem_killed, dbg_mem_stalled, dbg_mem_m2_pc, dbg_mem_m2_itag) & " " &
          dbg_get_stage_string("wb", rst, dbg_wb_killed, dbg_wb_stalled, dbg_wb_pc, dbg_wb_itag) & " " &
          dbg_get_done_string("done", rst, dbg_commited_itag, dbg_commited_pc) & " " &
          dbg_get_regwrite_string(dbg_wb2di_reg1) &
          dbg_get_regwrite_string(dbg_wb2di_reg2) &
          dbg_get_jump_string(dbg_jump_pc, dbg_jump_target) & " " &
          dbg_get_prediction_string(dbg_if_prediction, last_prediction);
        if dbg_commited_pc /= unusable_op then
          if to_integer(unsigned(dbg_commited_pc)) = 0 then
            passed_by_addr0 := passed_by_addr0 + 1;
          end if;
        end if;

        if passed_by_addr0 > 1 then
          report "PC rolled over to 0, ending simulation." severity error;
          stop <= '1';
        end if;

        last_prediction := dbg_if_prediction;

      end if;
    end if;
  end process debug_proc;

  -- purpose: memory
  -- type   : sequential
  -- inputs : clk, rst
  -- outputs:
  mem : process (clk, rst, stop) is
  begin  -- process mem
    if rst = '1' then                   -- asynchronous reset (active low)
      i_mem_rd_valid <= '0';
      i_mem_rd_data  <= (others => '0');
      i_mem_wr_ack   <= '0';
    elsif stop = '0' and rising_edge(clk) then         -- rising clock edge
      i_mem_wr_ack <= '0';
      if o_mem_wr_en = '1' then
        i_mem_rd_valid <= '0';
        i_mem_rd_data  <= i_mem_rd_data;
        assert i_mem_wr_ack = '0' report "Invalid transaction" severity error;
        i_mem_wr_ack   <= '1';
      else
        -- copy rd @ to data and change data order (ABCD -> DCBA)
        i_mem_rd_data <= o_mem_addr(3 downto 0) &
                         o_mem_addr(7 downto 4) &
                         o_mem_addr(11 downto 8) &
                         o_mem_addr(15 downto 12) &
                         o_mem_addr(19 downto 16) &
                         o_mem_addr(23 downto 20) &
                         o_mem_addr(27 downto 24) &
                         o_mem_addr(31 downto 28);

        i_mem_rd_valid <= '1';
      end if;
    end if;
  end process mem;

end architecture rtl;

-------------------------------------------------------------------------------

configuration MIPS_CPU_tb_rtl_cfg of MIPS_CPU_tb is
  for rtl
  end for;
end MIPS_CPU_tb_rtl_cfg;

-------------------------------------------------------------------------------
