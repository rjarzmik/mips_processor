-------------------------------------------------------------------------------
-- Title      : Instruction recording
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : instruction_record.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-07
-- Last update: 2016-12-07
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-12-07  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.instruction_defs.instr_tag_t;
use work.instruction_defs.NB_PIPELINE_STAGES;

-------------------------------------------------------------------------------

package instruction_record is
  constant ADDR_WIDTH : integer := 32;

  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);

  type instr_record is record
    pc                  : addr_t;
    predict_is_branch   : boolean;
    predict_take_branch : boolean;
    commited            : boolean;
    commit_is_branch    : boolean;
    commit_take_branch  : boolean;
  end record;

  type instr_records is array(0 to NB_PIPELINE_STAGES - 1) of instr_record;

  function get_record(itag : in instr_tag_t; irecords : in instr_records)
    return instr_record;

  procedure record_one_instr(signal pc    : in  addr_t; signal itag : in instr_tag_t;
                             signal itags : out instr_records);

  procedure retire_one_instr(signal itag  : in  instr_tag_t;
                             signal itags : out instr_records);

end package instruction_record;

package body instruction_record is
  function get_record(itag : in instr_tag_t;
                      irecords : in instr_records)
    return instr_record is
    variable o : instr_record;
  begin
    o := irecords(itag.tag);
    return o;
  end function get_record;

  procedure record_one_instr(signal pc    : in  addr_t; signal itag : in instr_tag_t;
                             signal itags : out instr_records) is
  begin
    itags(itag.tag) <=
      (pc                  => pc,
       predict_is_branch   => itag.is_branch,
       predict_take_branch => itag.is_branch_taken,
       commited            => false,
       commit_is_branch    => false,
       commit_take_branch  => false
       );
  end procedure record_one_instr;

  procedure retire_one_instr(signal itag  : in  instr_tag_t;
                             signal itags : out instr_records) is
  begin
    itags(itag.tag).commited <= true;
  end procedure retire_one_instr;

end package body instruction_record;
