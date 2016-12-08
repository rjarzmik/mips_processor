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
use work.instruction_record.all;

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
    i_commited_instr_tag : in instr_tag_t;

    -- Misprediction computation
    o_mispredict : out std_logic;

    -- Branch prediction module
    i_btb_instr_tag    : in  instr_tag_t;
    o_btb_instr_record : out instr_record  -- available on next cycle
    );

end entity Instruction_Tracker;

-------------------------------------------------------------------------------

architecture rtl of Instruction_Tracker is
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  signal irecords              : instr_records;
  signal commited_instr_record : instr_record;

begin  -- architecture rtl

  itrack_recorder : process(clk, rst) is
  begin
    if rst = '1' then
    elsif rising_edge(clk) then
      if i_record_pc1_req = '1' then
        record_one_instr(i_pc1, i_pc1_instr_tag, irecords);
      end if;
      if i_record_pc2_req = '1' then
        record_one_instr(i_pc2, i_pc2_instr_tag, irecords);
      end if;
      if i_commited_instr_tag.valid then
        retire_one_instr(i_commited_instr_tag, irecords);
      end if;
    end if;
  end process itrack_recorder;

  -- Misprediction
  commited_instr_record <= get_record(i_commited_instr_tag, irecords);
  o_mispredict          <= '0' when
                  i_commited_instr_tag.is_branch_taken = commited_instr_record.predict_take_branch
                  else '1';

end architecture rtl;

-------------------------------------------------------------------------------
