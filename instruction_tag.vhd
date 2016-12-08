-------------------------------------------------------------------------------
-- Title      : Instruction tagging
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : instruction_tag.vhd.vhd
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

-------------------------------------------------------------------------------

package instruction_defs is
  -- Smallest 2 power with at least : None, Fetch, Decode/Issue, Execute, Memory, Writeback
  constant NB_PIPELINE_STAGES : natural := 8;

  type instr_tag_t is record
    valid           : boolean;
    tag             : natural range 0 to NB_PIPELINE_STAGES - 1;
    is_branch       : boolean;
    is_ja           : boolean;
    is_jr           : boolean;
    is_branch_taken : boolean;
  end record;

  constant INSTR_TAG_NONE : instr_tag_t := (
    tag             => 0, valid => false,
    is_branch       => false, is_ja => false, is_jr => false,
    is_branch_taken => false);
  constant INSTR_TAG_FIRST_VALID : instr_tag_t := (
    tag             => 0, valid => true,
    is_branch       => false, is_ja => false, is_jr => false,
    is_branch_taken => false);

  function get_instr_change_is_branch(itag          : instr_tag_t;
                                      new_is_branch : boolean)
    return instr_tag_t;
  function get_instr_change_is_ja(itag      : instr_tag_t;
                                  new_is_ja : boolean)
    return instr_tag_t;
  function get_instr_change_is_jr(itag      : instr_tag_t;
                                  new_is_jr : boolean)
    return instr_tag_t;
  function get_instr_change_tag(itag    : instr_tag_t;
                                new_tag : natural) return instr_tag_t;
  function get_next_instr_tag(itag : in instr_tag_t;
                              step :    positive) return instr_tag_t;
  function get_instr_change_is_branch_taken(itag                : instr_tag_t;
                                            new_is_branch_taken : boolean)
    return instr_tag_t;
end package instruction_defs;

package body instruction_defs is
  function get_instr_change_is_branch(itag          : instr_tag_t;
                                      new_is_branch : boolean)
    return instr_tag_t is
    variable o : instr_tag_t;
  begin
    o           := itag;
    o.is_branch := new_is_branch;
    return o;
  end function get_instr_change_is_branch;

  function get_instr_change_is_ja(itag      : instr_tag_t;
                                  new_is_ja : boolean)
    return instr_tag_t is
    variable o : instr_tag_t;
  begin
    o       := itag;
    o.is_ja := new_is_ja;
    return o;
  end function get_instr_change_is_ja;

  function get_instr_change_is_jr(itag      : instr_tag_t;
                                  new_is_jr : boolean)
    return instr_tag_t is
    variable o : instr_tag_t;
  begin
    o       := itag;
    o.is_jr := new_is_jr;
    return o;
  end function get_instr_change_is_jr;

  function get_instr_change_is_branch_taken(itag                : instr_tag_t;
                                            new_is_branch_taken : boolean)
    return instr_tag_t is
    variable o : instr_tag_t;
  begin
    o                 := itag;
    o.is_branch_taken := new_is_branch_taken;
    return o;
  end function get_instr_change_is_branch_taken;

  function get_instr_change_tag(itag    : instr_tag_t;
                                new_tag : natural) return instr_tag_t is
    variable o : instr_tag_t;
  begin
    o     := itag;
    o.tag := new_tag;
    return o;
  end function get_instr_change_tag;

  function get_next_instr_tag(itag : in instr_tag_t;
                              step :    positive) return instr_tag_t is
    variable o : instr_tag_t;
  begin
    o     := itag;
    o.tag := (o.tag + step) mod NB_PIPELINE_STAGES;
    return o;
  end function get_next_instr_tag;

end package body instruction_defs;
