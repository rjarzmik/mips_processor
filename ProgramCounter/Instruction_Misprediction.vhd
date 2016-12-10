-------------------------------------------------------------------------------
-- Title      : PC flow misprediciton module
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : Instruction_Misprediction.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-10
-- Last update: 2016-12-10
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-12-10  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.cpu_defs.all;
use work.instruction_defs.all;
use work.instruction_record.all;

-------------------------------------------------------------------------------

entity Instruction_Misprediction is

  generic (
    ADDR_WIDTH : integer;
    STEP       : integer
    );

  port (
    clk                     : in  std_logic;
    rst                     : in  std_logic;
    -- Input from instruction tracker
    i_commited_instr_record : in  instr_record;
    i_commited_instr_tag    : in  instr_tag_t;
    i_commited_jump_target  : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    -- Misprediction outputs
    o_mispredict            : out std_logic;
    o_mispredict_correct_pc : out std_logic_vector(ADDR_WIDTH - 1 downto 0)
    );

end entity Instruction_Misprediction;

-------------------------------------------------------------------------------

architecture rtl of Instruction_Misprediction is
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  signal pc_commited_stepped  : addr_t;
  signal pc_corrected_next    : addr_t;
  signal mispredicted         : boolean;
  signal wrongly_taken_branch : boolean;
  signal wrongly_taken_jump   : boolean;

begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------
  pc_stepped_commited : entity work.PC_Adder
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      STEP       => STEP)
    port map (
      current_pc => i_commited_instr_record.pc,
      next_pc    => pc_commited_stepped);

  mispredicted         <= not i_commited_instr_tag.is_branch_taken = i_commited_instr_record.predict_take_branch;
  wrongly_taken_branch <= i_commited_instr_tag.is_branch and
                          i_commited_instr_record.predict_is_branch and
                          mispredicted;
  wrongly_taken_jump <= (i_commited_instr_tag.is_ja or i_commited_instr_tag.is_jr) and
                        (i_commited_instr_record.predict_is_ja or i_commited_instr_record.predict_is_jr) and
                        mispredicted;

  pc_corrected_next <= pc_commited_stepped when wrongly_taken_branch or wrongly_taken_jump else
                       i_commited_jump_target;

  -- Outputs
  o_mispredict            <= '1' when mispredicted else '0';
  o_mispredict_correct_pc <= pc_corrected_next;

end architecture rtl;

-------------------------------------------------------------------------------
