-------------------------------------------------------------------------------
-- Title      : PC flow misprediciton module
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : Instruction_Misprediction.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-10
-- Last update: 2016-12-11
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
use ieee.numeric_std.all;

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
    clk                            : in  std_logic;
    rst                            : in  std_logic;
    -- Input from instruction tracker
    i_commited_instr_record        : in  instr_record;
    i_commited_instr_tag           : in  instr_tag_t;
    i_commited_jump_target         : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    -- Misprediction outputs
    o_mispredict                   : out std_logic;
    o_mispredict_correct_pc        : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_wrongly_taken_branch         : out boolean;
    o_wrongly_not_taken_branch     : out boolean;
    o_wrongly_taken_jump           : out boolean;
    o_wrongly_not_taken_jump       : out boolean;
    o_wrongly_pc_disrupt           : out boolean;
    o_wrongly_predicted_is_branch  : out boolean;
    o_wrongly_predicted_is_jump    : out boolean;
    o_wrongly_predicted_is_stepped : out boolean
    );

end entity Instruction_Misprediction;

-------------------------------------------------------------------------------

architecture rtl of Instruction_Misprediction is
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  signal irecord                      : instr_record;
  signal itag                         : instr_tag_t;
  signal pc_commited_stepped          : addr_t;
  signal pc_corrected_next            : addr_t;
  signal mispredicted                 : boolean;
  signal same_kind                    : boolean;
  signal commited_pc_disrupt          : boolean;
  signal predict_pc_disrupt           : boolean;
  signal wrong_branch_decision        : boolean;
  signal wrongly_taken_branch         : boolean;
  signal wrongly_not_taken_branch     : boolean;
  signal wrong_pc_disrupt             : boolean;
  signal wrong_jump_target            : boolean;
  signal wrongly_predicted_is_branch  : boolean;
  signal wrongly_predicted_is_jump    : boolean;
  signal wrongly_predicted_is_stepped : boolean;

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

  -- Inputs
  irecord <= i_commited_instr_record;
  itag    <= i_commited_instr_tag;

  mispredicted <= itag.valid and
                  ((itag.is_branch_taken /= irecord.predict_take_branch) or
                   wrong_pc_disrupt);

  -- Misprediction of kind of move : branch, ja, jr or casual next stepped instruction
  same_kind <= (irecord.predict_is_branch = itag.is_branch) and
               (irecord.predict_is_ja = itag.is_ja) and
               (irecord.predict_is_jr = itag.is_jr);
  commited_pc_disrupt <= itag.is_ja or itag.is_jr or
                         (itag.is_branch and itag.is_branch_taken);
  predict_pc_disrupt <= irecord.predict_is_ja or irecord.predict_is_jr or
                        (irecord.predict_is_branch and irecord.predict_take_branch);

  -- Branch mispredictions
  wrong_branch_decision <= itag.is_branch and irecord.predict_is_branch and
                           itag.is_branch_taken /= irecord.predict_take_branch;
  wrongly_taken_branch <= wrong_branch_decision and
                          not itag.is_branch_taken;
  wrongly_not_taken_branch <= wrong_branch_decision and
                              itag.is_branch_taken;
  wrongly_predicted_is_branch <= itag.is_branch /= irecord.predict_is_branch;

  -- Jump mispredictions
  wrong_pc_disrupt <= (commited_pc_disrupt and
                       irecord.predict_next_pc /= i_commited_jump_target) or
                      commited_pc_disrupt /= predict_pc_disrupt;
  wrongly_predicted_is_jump <= (itag.is_ja or itag.is_jr) /=
                               (irecord.predict_is_ja or irecord.predict_is_jr);

  -- Stepped misprediction
  wrongly_predicted_is_stepped <= (not itag.is_ja and not itag.is_jr and not itag.is_branch) /=
                                  (not irecord.predict_is_ja and not irecord.predict_is_jr and not
                                   irecord.predict_is_branch);
  
  -- Jump target correction
  pc_corrected_next <= i_commited_jump_target when commited_pc_disrupt else
                       pc_commited_stepped;

  -- Outputs
  o_mispredict               <= '1' when mispredicted else '0';
  o_mispredict_correct_pc    <= pc_corrected_next;
  o_wrongly_taken_branch     <= wrongly_taken_branch;
  o_wrongly_not_taken_branch <= wrongly_not_taken_branch;
  o_wrongly_taken_jump       <= not same_kind and
                          (irecord.predict_is_ja or irecord.predict_is_jr);
  o_wrongly_not_taken_jump <= not same_kind and not
                              (irecord.predict_is_ja or irecord.predict_is_jr);
  o_wrongly_pc_disrupt <= wrong_pc_disrupt;

  o_wrongly_predicted_is_branch  <= wrongly_predicted_is_branch;
  o_wrongly_predicted_is_jump    <= wrongly_predicted_is_jump;
  o_wrongly_predicted_is_stepped <= wrongly_predicted_is_stepped;

end architecture rtl;

-------------------------------------------------------------------------------
