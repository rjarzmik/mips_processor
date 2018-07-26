-------------------------------------------------------------------------------
-- Title      : Decode dependencies
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : Control_Decode_Dependencies.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-28
-- Last update: 2016-12-14
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:  Dtall the decode (Read After Write)
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-28  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.cpu_defs.all;

-------------------------------------------------------------------------------

entity Control_Decode_Dependencies is
  generic (
    NB_REGISTERS : integer
    );

  port (
    clk                   : in  std_logic;
    rst                   : in  std_logic;
    -- Decode source registers
    signal rsi            : in  natural range 0 to NB_REGISTERS - 1;
    signal rti            : in  natural range 0 to NB_REGISTERS - 1;
    -- Decode to Execute
    signal i_di2ex_reg1   : in  register_port_type;
    signal i_di2ex_reg2   : in  register_port_type;
    -- Execute to Memory
    signal i_ex2mem_reg1   : in  register_port_type;
    signal i_ex2mem_reg2   : in  register_port_type;
    -- Memory internal pipe
    signal i_mem2ctrl_stage1_reg1 : in register_port_type;
    signal i_mem2ctrl_stage1_reg2 : in register_port_type;
    signal i_mem2ctrl_stage2_reg1 : in register_port_type;
    signal i_mem2ctrl_stage2_reg2 : in register_port_type;
    -- Memory to WriteBack
    signal i_mem2wb_reg1   : in  register_port_type;
    signal i_mem2wb_reg2   : in  register_port_type;
    -- Writeback to Decode
    signal i_wb2di_reg1   : in  register_port_type;
    signal i_wb2di_reg2   : in  register_port_type;
    -- Dependencies
    signal o_raw_detected : out std_logic
    );

end entity Control_Decode_Dependencies;

-------------------------------------------------------------------------------

architecture rtl of Control_Decode_Dependencies is

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------

begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------

  o_raw_detected <= '1' when
                    (i_di2ex_reg1.we = '1' and i_di2ex_reg1.idx = rsi) or
                    (i_di2ex_reg2.we = '1' and i_di2ex_reg2.idx = rsi) or
                    (i_ex2mem_reg1.we = '1' and i_ex2mem_reg1.idx = rsi) or
                    (i_ex2mem_reg2.we = '1' and i_ex2mem_reg2.idx = rsi) or
                    (i_mem2ctrl_stage1_reg1.we = '1' and i_mem2ctrl_stage1_reg1.idx = rsi) or
                    (i_mem2ctrl_stage1_reg2.we = '1' and i_mem2ctrl_stage1_reg2.idx = rsi) or
                    (i_mem2ctrl_stage2_reg1.we = '1' and i_mem2ctrl_stage2_reg1.idx = rsi) or
                    (i_mem2ctrl_stage2_reg2.we = '1' and i_mem2ctrl_stage2_reg2.idx = rsi) or
                    (i_mem2wb_reg1.we = '1' and i_mem2wb_reg1.idx = rsi) or
                    (i_mem2wb_reg2.we = '1' and i_mem2wb_reg2.idx = rsi) or
                    (i_wb2di_reg1.we = '1' and i_wb2di_reg1.idx = rsi) or
                    (i_wb2di_reg2.we = '1' and i_wb2di_reg2.idx = rsi) or
                    (i_ex2mem_reg1.we = '1' and i_ex2mem_reg1.idx = rti) or
                    (i_ex2mem_reg2.we = '1' and i_ex2mem_reg2.idx = rti) or
                    (i_mem2ctrl_stage1_reg1.we = '1' and i_mem2ctrl_stage1_reg1.idx = rti) or
                    (i_mem2ctrl_stage1_reg2.we = '1' and i_mem2ctrl_stage1_reg2.idx = rti) or
                    (i_mem2ctrl_stage2_reg1.we = '1' and i_mem2ctrl_stage2_reg1.idx = rti) or
                    (i_mem2ctrl_stage2_reg2.we = '1' and i_mem2ctrl_stage2_reg2.idx = rti) or
                    (i_mem2wb_reg1.we = '1' and i_mem2wb_reg1.idx = rti) or
                    (i_mem2wb_reg2.we = '1' and i_mem2wb_reg2.idx = rti) or
                    (i_wb2di_reg1.we = '1' and i_wb2di_reg1.idx = rti) or
                    (i_wb2di_reg2.we = '1' and i_wb2di_reg2.idx = rti)                    
                    else '0';

end architecture rtl;

-------------------------------------------------------------------------------
