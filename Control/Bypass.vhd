-------------------------------------------------------------------------------
-- Title      : Bypass future register writes to Decode stage
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : Bypass.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-12
-- Last update: 2016-12-17
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Bypassing values before they are commited to register file
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-12-12  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cpu_defs.all;

-------------------------------------------------------------------------------

entity Bypass is

  generic (
    NB_REGISTERS : positive
    );

  port (
    clk                   : in  std_logic;
    rst                   : in  std_logic;
    -- Inputs
    --- Bypass sources
    signal i_ex2mem_reg1  : in  register_port_type;
    signal i_ex2mem_reg2  : in  register_port_type;
    signal i_mem2wb_reg1  : in  register_port_type;
    signal i_mem2wb_reg2  : in  register_port_type;
    signal i_wb2di_reg1   : in  register_port_type;
    signal i_wb2di_reg2   : in  register_port_type;
    --- Bypass register targets
    signal i_src_reg1_idx : in  natural range 0 to NB_REGISTERS - 1;
    signal i_src_reg2_idx : in  natural range 0 to NB_REGISTERS - 1;
    -- Outputs
    signal o_reg1         : out register_port_type;
    signal o_reg2         : out register_port_type
    );

end entity Bypass;

-------------------------------------------------------------------------------

architecture rtl of Bypass is
  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  constant NOT_FOUND_REG : register_port_type := (we => '0', idx => 0, data => (others => 'X'));

begin  -- architecture rtl

  o_reg1 <= i_ex2mem_reg1 when i_ex2mem_reg1.we = '1' and i_ex2mem_reg1.idx = i_src_reg1_idx else
            i_ex2mem_reg2 when i_ex2mem_reg2.we = '1' and i_ex2mem_reg2.idx = i_src_reg1_idx else
            i_mem2wb_reg1 when i_mem2wb_reg1.we = '1' and i_mem2wb_reg1.idx = i_src_reg1_idx else
            i_mem2wb_reg2 when i_mem2wb_reg2.we = '1' and i_mem2wb_reg2.idx = i_src_reg1_idx else
            i_wb2di_reg1  when i_wb2di_reg1.we = '1' and i_wb2di_reg1.idx = i_src_reg1_idx else
            i_wb2di_reg1  when i_wb2di_reg1.we = '1' and i_wb2di_reg1.idx = i_src_reg1_idx else
            NOT_FOUND_REG;

  o_reg2 <= i_ex2mem_reg1 when i_ex2mem_reg1.we = '1' and i_ex2mem_reg1.idx = i_src_reg2_idx else
            i_ex2mem_reg2 when i_ex2mem_reg2.we = '1' and i_ex2mem_reg2.idx = i_src_reg2_idx else
            i_mem2wb_reg1 when i_mem2wb_reg1.we = '1' and i_mem2wb_reg1.idx = i_src_reg2_idx else
            i_mem2wb_reg2 when i_mem2wb_reg2.we = '1' and i_mem2wb_reg2.idx = i_src_reg2_idx else
            i_wb2di_reg1  when i_wb2di_reg1.we = '1' and i_wb2di_reg1.idx = i_src_reg2_idx else
            i_wb2di_reg1  when i_wb2di_reg1.we = '1' and i_wb2di_reg1.idx = i_src_reg2_idx else
            NOT_FOUND_REG;

end architecture rtl;

-------------------------------------------------------------------------------
