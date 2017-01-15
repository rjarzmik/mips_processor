-------------------------------------------------------------------------------
-- Title      : Writeback instruction's result
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : Writeback.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-16
-- Last update: 2017-01-14
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Writes back a MIPS instruction result into the register file
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-16  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.cpu_defs.all;
use work.instruction_defs.all;

-------------------------------------------------------------------------------

entity Writeback is

  generic (
    ADDR_WIDTH   : integer  := 32;
    DATA_WIDTH   : integer  := 32;
    NB_REGISTERS : positive := 34
    );

  port (
    clk           : in std_logic;
    rst           : in std_logic;
    stall_req     : in std_logic;       -- stall current instruction
    kill_req      : in std_logic;       -- kill current instruction
    i_reg1        : in register_port_type;
    i_reg2        : in register_port_type;
    i_jump_target : in std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_jump_op     : in jump_type;

    o_reg1        : out register_port_type;
    o_reg2        : out register_port_type;
    o_is_jump     : out std_logic;
    o_jump_target : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    -- Carry-over signals
    i_instr_tag   : in  instr_tag_t;
    o_instr_tag   : out instr_tag_t;
    -- Debug signal
    i_dbg_wb_pc   : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_dbg_wb_pc   : out std_logic_vector(ADDR_WIDTH - 1 downto 0)
    );

end entity Writeback;

architecture str of Writeback is
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  subtype data_t is std_logic_vector(DATA_WIDTH - 1 downto 0);

  type state_t is (normal, jump_recorded);
  signal is_jump : std_ulogic;

begin
  process(rst, clk, stall_req, kill_req, i_instr_tag)
    variable state           : state_t := normal;
    variable recorded_itag   : instr_tag_t;
    variable recorded_target : addr_t;
    variable is_nop          : boolean;
    variable o_itag          : instr_tag_t;
  begin
    if rst = '1' then
      o_is_jump     <= '0';
      o_jump_target <= (others => 'X');
      o_instr_tag   <= INSTR_TAG_NONE;
      o_reg1        <= REG_IDLE;
      o_reg2        <= REG_IDLE;
      state         := normal;
    else
      is_nop := not i_instr_tag.valid;

      if rising_edge(clk) then
        o_itag := i_instr_tag;

        if kill_req = '1' then
          o_is_jump     <= '0';
          o_jump_target <= i_jump_target;
          o_itag.valid  := false;
          o_reg1        <= REG_IDLE;
          o_reg2        <= REG_IDLE;
          state         := normal;
        elsif stall_req = '1' then
        else
          o_reg1 <= i_reg1;
          o_reg2 <= i_reg2;

          case state is
            when normal =>
              if is_jump = '1' then
                -- Record the jump and transition state
                state           := jump_recorded;
                recorded_target := i_jump_target;
                recorded_itag   := i_instr_tag;
                o_is_jump       <= '0';
                o_itag.valid    := false;
              else
                -- Normal instruction
                state         := normal;
                o_is_jump     <= '0';
                o_itag.valid  := i_instr_tag.valid;
                recorded_itag := i_instr_tag;
              end if;
            when jump_recorded =>
              if is_jump = '1' or not is_nop then
                -- Delay slot instruction
                state                  := normal;
                o_is_jump              <= '1';
                o_itag                 := recorded_itag;
                o_itag.tag             := i_instr_tag.tag;
                o_itag.is_branch_taken := true;
              else
                state        := jump_recorded;
                o_is_jump    <= '0';
                o_itag.valid := false;
              end if;
          end case;
        end if;
        o_instr_tag <= o_itag;
      end if;
      o_jump_target <= recorded_target;
    end if;
  end process;

  jumper : process(i_jump_op, i_instr_tag.flags)
    variable jump                      : jump_type := none;
    variable is_eq, is_lesser, do_jump : boolean;
  begin
    is_eq     := false;
    is_lesser := false;
    if i_instr_tag.flags(flag_zero) = '1' then
      is_eq := true;
    end if;
    if i_instr_tag.flags(flag_carry) = '1' then
      is_lesser := false;
    end if;

    case i_jump_op is
      when always          => do_jump := true;
      when none            => do_jump := false;
      when zero            => do_jump := is_eq;
      when non_zero        => do_jump := not is_eq;
      when lesser_or_zero  => do_jump := is_lesser or is_eq;
      when lesser          => do_jump := is_lesser;
      when greater         => do_jump := not is_lesser and not is_eq;
      when greater_or_zero => do_jump := not is_lesser;
    end case;

    if do_jump then
      is_jump <= '1';
    else
      is_jump <= '0';
    end if;
  end process jumper;

  debug : process(rst, clk, stall_req, kill_req)
  begin
    if rst = '1' then
      o_dbg_wb_pc <= (others => 'X');
    elsif rising_edge(clk) and kill_req = '1' then
      o_dbg_wb_pc <= (others => 'X');
    elsif rising_edge(clk) and stall_req = '1' then
    elsif rising_edge(clk) then
      o_dbg_wb_pc <= i_dbg_wb_pc;
    end if;
  end process debug;

end architecture str;
