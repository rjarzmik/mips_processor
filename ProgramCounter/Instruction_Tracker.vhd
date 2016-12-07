-------------------------------------------------------------------------------
-- Title      : Instruction Tracker
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : Instruction_Tracker.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-07
-- Last update: 2016-12-08
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Each in-flight instruction in the pipeline tracker
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-12-07  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.cpu_defs.all;
use work.instruction_defs.all;

-------------------------------------------------------------------------------

entity Instruction_Tracker is
  generic (
    ADDR_WIDTH : integer
    );

  port (
    clk                  : in std_logic;
    rst                  : in std_logic;
    -- Input instruction recorder
    --- Acquire enable, (i_pc1 and i_pc1_instr_tag will be linked together).
    i_record_pc1_req     : in std_logic;
    --- Acquire enable, (i_pc2 and i_pc2_instr_tag will be linked together).
    i_record_pc2_req     : in std_logic;
    i_pc1                : in std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_pc2                : in std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_pc1_instr_tag      : in instr_tag_t;
    i_pc2_instr_tag      : in instr_tag_t;
    -- Retire instruction recorder
    i_commited_instr_tag : in instr_tag_t
    );

end entity Instruction_Tracker;

-------------------------------------------------------------------------------

architecture rtl of Instruction_Tracker is
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);

  type instr_record is record
    pc       : addr_t;
    commited : boolean;
  end record;

  type instr_records is array(0 to NB_PIPELINE_STAGES - 1) of instr_record;

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  signal itags : instr_records;

  procedure record_one_instr(signal pc    : in  addr_t; signal itag : in instr_tag_t;
                             signal itags : out instr_records) is
  begin
    itags(itag.tag) <= (pc => pc, commited => false);
  end procedure record_one_instr;

  procedure retire_one_instr(signal itag : in instr_tag_t;
                             signal itags : out instr_records) is
  begin
    itags(itag.tag).commited <= true;
  end procedure retire_one_instr;
  
begin  -- architecture rtl

  itrack_recorder : process(clk, rst) is
  begin
    if rst = '1' then
    elsif rising_edge(clk) then
      if i_record_pc1_req = '1' then
        record_one_instr(i_pc1, i_pc1_instr_tag, itags);
      end if;
      if i_record_pc2_req = '1' then
        record_one_instr(i_pc2, i_pc2_instr_tag, itags);
      end if;
      if i_commited_instr_tag.valid then
        retire_one_instr(i_commited_instr_tag, itags);
      end if;
    end if;
  end process itrack_recorder;

end architecture rtl;

-------------------------------------------------------------------------------
