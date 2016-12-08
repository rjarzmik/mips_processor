-------------------------------------------------------------------------------
-- Title      : Writeback instruction's result
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : Writeback.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-16
-- Last update: 2016-12-08
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
    i_is_jump     : in std_logic;

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

-------------------------------------------------------------------------------

architecture rtl of Writeback is

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  signal reg1             : register_port_type;
  signal reg2             : register_port_type;
  signal is_nop           : boolean;
  signal is_jump          : std_logic;
  signal last_is_jump     : std_logic;
  signal last_jump_target : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal last_instr_tag   : instr_tag_t;

begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------

  is_nop <= true when i_reg1.we = '0' and i_reg2.we = '0' and i_is_jump = '0' else false;

  process(rst, clk, stall_req)
  begin
    if rst = '1' then
      reg1.we          <= '0';
      reg2.we          <= '0';
      is_jump          <= '0';
      o_instr_tag      <= INSTR_TAG_NONE;
      last_instr_tag   <= INSTR_TAG_NONE;
      last_is_jump     <= '0';
      last_jump_target <= (others => 'X');
    elsif rising_edge(clk) then
      if kill_req = '1' then
        reg1.we       <= '0';
        reg2.we       <= '0';
        is_jump       <= '0';
        o_is_jump     <= '0';
        o_jump_target <= (others => 'X');
        o_instr_tag   <= INSTR_TAG_NONE;
      elsif stall_req = '0' then
        if not is_nop then
          last_instr_tag <=
            get_instr_change_is_branch_taken(i_instr_tag, i_is_jump = '1');
          last_is_jump     <= i_is_jump;
          last_jump_target <= i_jump_target;
        end if;

        -- Branch delay slot of 1 :
        ---   If branch or jump, delay setting o_is_jump and o_jump_target to
        ---   the next no-NOP instruction
        ---   If not, forward as is.
        if (last_instr_tag.is_branch or last_instr_tag.is_ja
            or last_instr_tag.is_jr) and not is_nop then
          -- Falsify the outputs to fake the jump/branch happens on the next
          -- after jump/branch instruction, ie. delay slot of 1.
          -- Transfer the jump and the writeback instruction together
          -- The o_instr_tag is changed from the instruction just after the
          -- branch to the instruction branch, for branch prediction.
          o_is_jump     <= last_is_jump;
          o_jump_target <= last_jump_target;
          o_instr_tag <=
            get_instr_change_is_branch_taken(
              get_instr_change_is_branch(
                get_instr_change_is_ja(
                  get_instr_change_is_jr(i_instr_tag, last_instr_tag.is_jr),
                  last_instr_tag.is_ja),
                last_instr_tag.is_branch),
              last_instr_tag.is_branch_taken);
        elsif (i_instr_tag.is_branch or i_instr_tag.is_ja
               or i_instr_tag.is_jr) then
          -- As the jump information is kept in last_*, wipe out any
          -- jump/branch sign from this instruction, as it will be reapplied on
          -- the next one.
          o_is_jump     <= '0';
          o_jump_target <= (others => 'X');
          o_instr_tag <=
            get_instr_change_is_branch_taken(
              get_instr_change_is_branch(
                get_instr_change_is_ja(
                  get_instr_change_is_jr(i_instr_tag, false),
                  false),
                false),
              false);
        else
          -- Here there wasn't a "jump/branch" kept nor on the input
          -- instruction, so forward normally everything.
          o_is_jump     <= i_is_jump;
          o_jump_target <= i_jump_target;
          o_instr_tag   <= i_instr_tag;
        end if;

        reg1 <= i_reg1;
        reg2 <= i_reg2;
      end if;
    end if;
  end process;

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

  o_reg1 <= reg1;
  o_reg2 <= reg2;
-- o_is_jump     <= is_jump;
-- o_jump_target <= jump_target when is_jump = '1' else (others => 'X');

end architecture rtl;

-------------------------------------------------------------------------------
