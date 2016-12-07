-------------------------------------------------------------------------------
-- Title      : Program Counter
-- Project    : 
-------------------------------------------------------------------------------
-- File       : PC_Register.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-13
-- Last update: 2016-12-07
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: The MIPS processor program counter
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-13  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cpu_defs.all;
use work.instruction_defs.all;
-------------------------------------------------------------------------------

entity PC_Register is

  generic (
    ADDR_WIDTH : integer := 32;
    STEP       : integer := 4
    );

  port (
    clk                    : in  std_logic;
    rst                    : in  std_logic;
    stall_pc               : in  std_logic;
    jump_pc                : in  std_logic;
    -- jump_target: should appear on o_pc
    jump_target            : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_current_pc           : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_current_pc_instr_tag : out instr_tag_t;
    o_next_pc              : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_next_pc_instr_tag    : out instr_tag_t;
    o_mispredicted         : out std_logic
    );

end entity PC_Register;

-------------------------------------------------------------------------------

architecture rtl of PC_Register is
  component PC_Adder is
    generic (
      ADDR_WIDTH : integer;
      STEP       : integer);
    port (
      current_pc : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
      next_pc    : out std_logic_vector(ADDR_WIDTH - 1 downto 0));
  end component PC_Adder;

  procedure update_next_instr_tag(last_used_tag : in  instr_tag_t;
                                  signal itag   : out instr_tag_t) is
  begin
    itag <= last_used_tag;
  end procedure update_next_instr_tag;

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  signal pc                : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal pc_instr_tag      : instr_tag_t;
  signal pc_next           : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal pc_next_instr_tag : instr_tag_t;
  signal pc_next_stepped   : std_logic_vector(ADDR_WIDTH - 1 downto 0);

  --- Instruction tracker
  signal instr_tag : instr_tag_t;

--- Jump internal signals
begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------
  pc_next_add4 : PC_Adder
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      STEP       => STEP)
    port map (
      current_pc => pc_next,
      next_pc    => pc_next_stepped);

  process(clk, rst) is
    variable jump_recorded_valid          : boolean := false;
    variable jump_recorded_target         : std_logic_vector(ADDR_WIDTH - 1 downto 0);
    variable jump_recorded_target_stepped : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  begin
    if rst = '1' then
      pc      <= std_logic_vector(to_signed(0, ADDR_WIDTH));
      pc_next <= std_logic_vector(to_signed(4, ADDR_WIDTH));
      pc_instr_tag <= INSTR_TAG_FIRST_VALID;
      pc_next_instr_tag <= get_next_instr_tag(INSTR_TAG_FIRST_VALID, 1);
      update_next_instr_tag(get_next_instr_tag(INSTR_TAG_FIRST_VALID, 1), instr_tag);
    elsif rising_edge(clk) then
      if jump_pc = '1' then
        jump_recorded_valid  := true;
        jump_recorded_target := jump_target;
      end if;

      if stall_pc = '0' then
        if jump_recorded_valid then
          pc                   <= jump_recorded_target;
          pc_instr_tag         <= get_next_instr_tag(instr_tag, 1);
          pc_next              <= std_logic_vector(unsigned(jump_recorded_target) + STEP);
          pc_next_instr_tag    <= get_next_instr_tag(instr_tag, 2);
          update_next_instr_tag(get_next_instr_tag(instr_tag, 2), instr_tag);
          jump_recorded_valid  := false;
          jump_recorded_target := (others => 'X');
        else
          pc                <= pc_next;
          pc_instr_tag      <= pc_next_instr_tag;
          pc_next           <= pc_next_stepped;
          pc_next_instr_tag <= get_next_instr_tag(instr_tag, 1);
          update_next_instr_tag(get_next_instr_tag(instr_tag, 1), instr_tag);
        end if;
      end if;
    end if;
  end process;

  --- Outputs
  o_current_pc           <= pc;
  o_current_pc_instr_tag <= pc_instr_tag;
  o_next_pc              <= pc_next;
  o_next_pc_instr_tag    <= pc_next_instr_tag;

  --- Misprediction
  --- When fetch mispredicted, signal to kill the pipeline
  o_mispredicted <= jump_pc;

end architecture rtl;

-------------------------------------------------------------------------------
