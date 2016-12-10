-------------------------------------------------------------------------------
-- Title      : Program Counter
-- Project    : 
-------------------------------------------------------------------------------
-- File       : PC_Register.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-13
-- Last update: 2016-12-11
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
use work.instruction_record.instr_record;

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
    i_commited_instr_tag   : in  instr_tag_t;
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
  signal pc_next_next      : std_logic_vector(ADDR_WIDTH - 1 downto 0);

  --- Instruction tracker
  signal instr_tag             : instr_tag_t;
  signal itrack_req_pc         : std_logic;
  signal itrack_req_pc_next    : std_logic;
  signal commited_instr_record : instr_record;
  signal commited_instr_tag    : instr_tag_t;

  --- Instruction misprediction tracker
  signal mispredicted                            : std_logic;
  signal mispredict_correct_pc                   : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal mispredict_wrongly_taken_branch         : boolean;
  signal mispredict_wrongly_not_taken_branch     : boolean;
  signal mispredict_wrongly_taken_jump           : boolean;
  signal mispredict_wrongly_not_taken_jump       : boolean;
  signal mispredict_wrongly_pc_disrupt           : boolean;
  signal mispredict_wrongly_predicted_is_branch  : boolean;
  signal mispredict_wrongly_predicted_is_jump    : boolean;
  signal mispredict_wrongly_predicted_is_stepped : boolean;

--- Jump internal signals
begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------
  itracker : entity work.Instruction_Tracker
    generic map (
      ADDR_WIDTH => ADDR_WIDTH)
    port map (
      clk                     => clk,
      rst                     => rst,
      i_record_pc1_req        => itrack_req_pc,
      i_record_pc2_req        => itrack_req_pc_next,
      i_pc1                   => pc,
      i_pc2                   => pc_next,
      i_pc1_instr_tag         => pc_instr_tag,
      i_pc2_instr_tag         => pc_next_instr_tag,
      i_pc1_predict_next_pc   => pc_next,
      i_pc2_predict_next_pc   => pc_next_next,
      i_commited_instr_tag    => i_commited_instr_tag,
      i_jump_target           => jump_target,
      o_commited_instr_record => commited_instr_record,
      o_commited_instr_tag    => commited_instr_tag,
      i_btb_instr_tag         => INSTR_TAG_NONE
      );

  mispredictor : entity work.Instruction_Misprediction
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      STEP       => STEP)
    port map (
      clk                            => clk,
      rst                            => rst,
      i_commited_instr_record        => commited_instr_record,
      i_commited_instr_tag           => commited_instr_tag,
      i_commited_jump_target         => jump_target,
      o_mispredict                   => mispredicted,
      o_mispredict_correct_pc        => mispredict_correct_pc,
      o_wrongly_taken_branch         => mispredict_wrongly_taken_branch,
      o_wrongly_not_taken_branch     => mispredict_wrongly_not_taken_branch,
      o_wrongly_taken_jump           => mispredict_wrongly_taken_jump,
      o_wrongly_not_taken_jump       => mispredict_wrongly_not_taken_jump,
      o_wrongly_pc_disrupt           => mispredict_wrongly_pc_disrupt,
      o_wrongly_predicted_is_branch  => mispredict_wrongly_predicted_is_branch,
      o_wrongly_predicted_is_jump    => mispredict_wrongly_predicted_is_jump,
      o_wrongly_predicted_is_stepped => mispredict_wrongly_predicted_is_stepped
      );

  predictor : entity work.PC_Predictor
    generic map (
      ADDR_WIDTH     => ADDR_WIDTH,
      STEP           => STEP,
      NB_PREDICTIONS => 4)
    port map (
      clk                            => clk,
      rst                            => rst,
      stall_req                      => stall_pc,
      o_itrack_req_pc1               => itrack_req_pc,
      o_itrack_req_pc2               => itrack_req_pc_next,
      -- o_itrack_pc1                   => o_itrack_pc1,
      -- o_itrack_pc2                   => o_itrack_pc2,
      -- o_itrack_pc1_instr_tag         => pc_instr_tag,
      -- o_itrack_pc2_instr_tag         => pc_next_instr_tag,
      i_commited_instr_record        => commited_instr_record,
      i_commited_instr_tag           => commited_instr_tag,
      i_commited_jump_target         => jump_target,
      i_mispredict                   => mispredicted,
      i_mispredict_correct_pc        => mispredict_correct_pc,
      i_wrongly_taken_branch         => mispredict_wrongly_taken_branch,
      i_wrongly_not_taken_branch     => mispredict_wrongly_not_taken_branch,
      i_wrongly_taken_jump           => mispredict_wrongly_taken_jump,
      i_wrongly_not_taken_jump       => mispredict_wrongly_not_taken_jump,
      i_wrongly_predicted_is_branch  => mispredict_wrongly_predicted_is_branch,
      i_wrongly_predicted_is_jump    => mispredict_wrongly_predicted_is_jump,
      i_wrongly_predicted_is_stepped => mispredict_wrongly_predicted_is_stepped,
      o_pc                           => pc,
      o_pc_instr_tag                 => pc_instr_tag,
      o_next_pc                      => pc_next,
      o_next_pc_instr_tag            => pc_next_instr_tag,
      o_next_next_pc                 => pc_next_next);

  --- Outputs
  o_current_pc           <= pc;
  o_current_pc_instr_tag <= pc_instr_tag;
  o_next_pc              <= pc_next;
  o_next_pc_instr_tag    <= pc_next_instr_tag;
  o_mispredicted         <= mispredicted;

  --- Misprediction
  --- When fetch mispredicted, signal to kill the pipeline
  --- This is now handled by the misprediction entity

end architecture rtl;

-------------------------------------------------------------------------------
