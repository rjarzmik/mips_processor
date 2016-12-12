-------------------------------------------------------------------------------
-- Title      : Predicts the next program counters values to be used
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : PC_Predictor.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-10
-- Last update: 2016-12-12
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
use work.instruction_prediction.all;

-------------------------------------------------------------------------------

entity PC_Predictor is

  generic (
    ADDR_WIDTH : integer;
    STEP       : integer
    );

  port (
    clk                            : in  std_logic;
    rst                            : in  std_logic;
    stall_req                      : in  std_logic;
    -- Instruction tracker
    --- Query to record new itag entries
    o_itrack_req_pc1               : out std_logic;
    o_itrack_req_pc2               : out std_logic;
    o_itrack_pc1                   : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_itrack_pc2                   : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_itrack_pc1_instr_tag         : out instr_tag_t;
    o_itrack_pc2_instr_tag         : out instr_tag_t;
    --- Currently commited instruction
    i_commited_instr_record        : in  instr_record;
    i_commited_instr_tag           : in  instr_tag_t;
    i_commited_jump_target         : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    -- Misprediction inputs
    i_mispredict                   : in  std_logic;
    i_mispredict_correct_pc        : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_wrongly_taken_branch         : in  boolean;
    i_wrongly_not_taken_branch     : in  boolean;
    i_wrongly_taken_jump           : in  boolean;
    i_wrongly_not_taken_jump       : in  boolean;
    i_wrongly_predicted_is_branch  : in  boolean;
    i_wrongly_predicted_is_jump    : in  boolean;
    i_wrongly_predicted_is_stepped : in  boolean;
    -- Output predictions
    o_pc                           : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_pc_instr_tag                 : out instr_tag_t;
    o_next_pc                      : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_next_pc_instr_tag            : out instr_tag_t;
    o_next_next_pc                 : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    -- Debug signals
    o_dbg_prediction               : out prediction_t
    );

end entity PC_Predictor;

-------------------------------------------------------------------------------

architecture rtl of PC_Predictor is
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);

  -- Prediction cache

  procedure update_prediction(i_address          : in    addr_t;
                              signal predictions : inout predictions_t;
                              prediction         : in    prediction_t) is
  begin
    for i in predictions'range loop
      if predictions(i).valid and predictions(i).pc = i_address then
        predictions(i) <= prediction;
      end if;
    end loop;
  end procedure update_prediction;

  function create_prediction(i_pc      : addr_t;
                             i_next_pc : addr_t;
                             itag      : instr_tag_t) return prediction_t is
    variable o : prediction_t;
  begin
    o.valid     := true;
    o.pc        := i_pc;
    o.next_pc   := i_next_pc;
    o.is_ja_jr  := itag.is_ja or itag.is_jr;
    o.is_branch := itag.is_branch;
    if itag.is_ja or itag.is_jr then
      o.take_branch := 3;
    elsif itag.is_branch then
      if itag.is_branch_taken then
        o.take_branch := 2;
      else
        o.take_branch := 1;
      end if;
    end if;
    return o;
  end function create_prediction;


  function guess_next_pc(pc          : addr_t;
                         predictions : predictions_t) return addr_t is
    variable o          : addr_t;
    variable prediction : prediction_t;
  begin
    if is_prediction_hit(pc, predictions) then
      prediction := get_prediction(pc, predictions);
      o          := prediction.next_pc;
    else
      o := std_logic_vector(unsigned(pc) + STEP);
    end if;
    return o;
  end;

  function guess_next_itag(pc          : addr_t; itag : instr_tag_t;
                           predictions : predictions_t) return instr_tag_t is
    variable o          : instr_tag_t;
    variable prediction : prediction_t;
  begin
    -- No real prediction yet, just a stepped PC
    o.valid := itag.valid;
    o.tag   := itag.tag;
    if is_prediction_hit(pc, predictions) then
      prediction        := get_prediction(pc, predictions);
      o.is_branch       := prediction.is_branch;
      o.is_ja           := prediction.is_ja_jr;
      o.is_jr           := prediction.is_ja_jr;
      o.is_branch_taken := prediction.take_branch >= 2;
    else
      o.is_branch       := false;
      o.is_ja           := false;
      o.is_jr           := false;
      o.is_branch_taken := false;
    end if;
    return o;
  end function guess_next_itag;

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  signal alloc_itag : instr_tag_t;

  -- Stall logic
  signal stall               : std_logic;
  signal jump_while_stalling : boolean;

  -- Forecasts
  signal pc           : addr_t;
  signal pc_next      : addr_t;
  signal pc_itag      : instr_tag_t;
  signal pc_next_itag : instr_tag_t;
  signal pc_next_next : addr_t;

  -- Prediction data
  signal predictions : predictions_t;

  -- Aliaseses
  alias irecord : instr_record is i_commited_instr_record;
  alias itag    : instr_tag_t is i_commited_instr_tag;

begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------

  itag_allocator : process(clk, rst, stall) is
  begin
    if rst = '1' then
      alloc_itag <= get_next_instr_tag(INSTR_TAG_FIRST_VALID, 2);
    elsif rising_edge(clk) then
      if i_mispredict then
        alloc_itag <= get_next_instr_tag(alloc_itag, 2);
      elsif stall = '0' then
        alloc_itag <= get_next_instr_tag(alloc_itag, 1);
      end if;
    end if;
  end process itag_allocator;

  -- Instruction Tracker
  --- Queries to create entries in itracker
  --- Based on itags generated by itag_allocator
  itrack_recorder : process(clk, rst, stall) is
  begin
    if rst = '1' then
      o_itrack_req_pc1 <= '1';
      o_itrack_req_pc2 <= '1';
    elsif rising_edge(clk) then
      if i_mispredict then
        o_itrack_req_pc1 <= '1';
        o_itrack_req_pc2 <= '1';
      elsif stall = '1' then
        o_itrack_req_pc1 <= '0';
        o_itrack_req_pc2 <= '0';
      else
        o_itrack_req_pc1 <= '0';
        o_itrack_req_pc2 <= '1';
      end if;
    end if;
  end process itrack_recorder;
  o_itrack_pc1           <= pc;
  o_itrack_pc2           <= pc_next;
  o_itrack_pc1_instr_tag <= pc_itag;
  o_itrack_pc2_instr_tag <= pc_next_itag;

  -- Program counter prediction
  pc_predictor : process(clk, rst, stall) is
  begin
    if rst = '1' then
      pc                  <= std_logic_vector(to_signed(0, ADDR_WIDTH));
      pc_next             <= std_logic_vector(to_signed(4, ADDR_WIDTH));
      pc_itag             <= INSTR_TAG_FIRST_VALID;
      pc_next_itag        <= get_next_instr_tag(INSTR_TAG_FIRST_VALID, 1);
      jump_while_stalling <= false;
    elsif rising_edge(clk) then
      if i_mispredict then
        -- Mispredict, break PC flow
        --- So far, don't use any recorded branch history, use next 2 PCs
        pc      <= i_mispredict_correct_pc;
        pc_next <= guess_next_pc(i_mispredict_correct_pc, predictions);
        pc_itag <= alloc_itag;
        pc_next_itag <= guess_next_itag(guess_next_pc(i_mispredict_correct_pc, predictions),
                                        get_next_instr_tag(alloc_itag, 1), predictions);
        if stall_req = '1' then
          jump_while_stalling <= true;
        else
          jump_while_stalling <= false;
        end if;
      elsif stall = '1' then
      else
        -- No mispredict, normal stepped path
        --- Shift pc_next* into pc*, and guess a new pc_next*
        jump_while_stalling <= false;
        pc                  <= pc_next;
        pc_itag             <= pc_next_itag;
        pc_next             <= guess_next_pc(pc_next, predictions);
        pc_next_itag <= guess_next_itag(guess_next_pc(pc_next, predictions),
                                        alloc_itag, predictions);
        pc_next_next <= guess_next_pc(guess_next_pc(pc_next, predictions), predictions);
      end if;
    end if;
  end process pc_predictor;
  o_pc                <= pc;
  o_next_pc           <= pc_next;
  o_pc_instr_tag      <= pc_itag;
  o_next_pc_instr_tag <= pc_next_itag;
  o_next_next_pc      <= pc_next_next;

  -- Stall logic
  stall <=
    '1' when stall_req = '1' or jump_while_stalling else '0';

  predictor_updater : process(clk, rst, stall)
    variable exists               : prediction_t;
    variable alloc_prediction_idx : natural range 0 to NB_PREDICTIONS - 1;
    variable prediction           : prediction_t;
    variable hit                  : boolean;
  begin
    if rst = '1' then
      alloc_prediction_idx := 0;
    elsif itag.valid and rising_edge(clk) then
      hit := is_prediction_hit(irecord.pc, predictions);
      if hit then
        -- Update a prediction
        prediction := get_prediction(irecord.pc, predictions);

        if i_wrongly_predicted_is_branch or i_wrongly_predicted_is_jump then
          -- Remove a prediction entry
          prediction.valid := false;
        elsif itag.is_ja or itag.is_jr then
          -- Replace a prediction entry
          prediction := create_prediction(irecord.pc, i_commited_jump_target, itag);
        elsif itag.is_branch and irecord.predict_next_pc = i_commited_jump_target then
          -- Update a branch prediction
          if itag.is_branch_taken then
            if prediction.take_branch < 3 then
              prediction.take_branch := prediction.take_branch + 1;
            end if;
          else
            if prediction.take_branch > 0 then
              prediction.take_branch := prediction.take_branch - 1;
            end if;
          end if;
        elsif itag.is_branch_taken and irecord.predict_next_pc /= i_commited_jump_target then
          -- Shouldn't be a pc disrupt: remove a prediction entry
          prediction.valid := false;
        elsif i_wrongly_not_taken_branch or i_wrongly_not_taken_jump then
          -- Replace a prediction
          prediction := create_prediction(irecord.pc, i_commited_jump_target, itag);
        end if;
        update_prediction(irecord.pc, predictions, prediction);
        o_dbg_prediction <= prediction;
      elsif i_wrongly_predicted_is_stepped then
        -- Add a new prediction
        predictions(alloc_prediction_idx) <= create_prediction(irecord.pc, i_commited_jump_target, itag);
        o_dbg_prediction                  <= create_prediction(irecord.pc, i_commited_jump_target, itag);
        alloc_prediction_idx              := (alloc_prediction_idx + 1) mod NB_PREDICTIONS;
      end if;
    end if;
  end process predictor_updater;

end architecture rtl;

-------------------------------------------------------------------------------
