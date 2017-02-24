-------------------------------------------------------------------------------
-- Title      : Decode dependencies
-- Project    : MIPS Processor
-------------------------------------------------------------------------------
-- File       : Control_Decode_Dependencies.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2016-11-28
-- Last update: 2017-02-24
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:  Detect dependencies for the decode stage (Read After Write)
-------------------------------------------------------------------------------
-- Copyright (c) 2016 Robert Jarzmik <robert.jarzmik@free.fr>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2016-11-28  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.cpu_defs.all;

entity Control_Decode_Dependencies is
  generic (
    NB_REGISTERS : integer
    );

  port (
    clk                           : in  std_logic;
    rst                           : in  std_logic;
    -- Decode source registers
    signal rsi                    : in  natural range 0 to NB_REGISTERS - 1;
    signal rti                    : in  natural range 0 to NB_REGISTERS - 1;
    -- Decode to Execute
    signal i_di2ex_reg1           : in  register_port_type;
    signal i_di2ex_reg2           : in  register_port_type;
    -- Execute to Memory
    signal i_ex2mem_reg1          : in  register_port_type;
    signal i_ex2mem_reg2          : in  register_port_type;
    -- Memory internal pipe
    signal i_mem2ctrl_stage1_reg1 : in  register_port_type;
    signal i_mem2ctrl_stage1_reg2 : in  register_port_type;
    signal i_mem2ctrl_stage2_reg1 : in  register_port_type;
    signal i_mem2ctrl_stage2_reg2 : in  register_port_type;
    -- Memory to WriteBack
    signal i_mem2wb_reg1          : in  register_port_type;
    signal i_mem2wb_reg2          : in  register_port_type;
    -- Writeback to Decode
    signal i_wb2di_reg1           : in  register_port_type;
    signal i_wb2di_reg2           : in  register_port_type;
    -- Dependencies
    signal o_raw_detected         : out std_logic
    );
end entity Control_Decode_Dependencies;

architecture rtl of Control_Decode_Dependencies is
  signal raw_is_candidate1 : std_ulogic;
  signal raw_is_candidate2 : std_ulogic;
  signal raw_candidate1    : natural range 0 to NB_REGISTERS_MAX;
  signal raw_candidate2    : natural range 0 to NB_REGISTERS_MAX;
begin  -- architecture rtl
  -- raw_detector supposes EX, MEM and WB stages are bypassed to DI stage
  raw_detector : process(rst, clk, raw_is_candidate1, raw_is_candidate2,
                         raw_candidate1, raw_candidate2)
  begin
    if rst = '1' then
      raw_is_candidate1 <= '0';
      raw_is_candidate2 <= '0';
      raw_candidate1    <= 0;
      raw_candidate2    <= 0;
    else
      raw_is_candidate1 <= i_di2ex_reg1.we;
      raw_is_candidate2 <= i_di2ex_reg2.we;
      raw_candidate1    <= i_di2ex_reg1.idx;
      raw_candidate2    <= i_di2ex_reg2.idx;
    end if;

    if (raw_is_candidate1 = '1' and
        (rsi = raw_candidate1 or rti = raw_candidate1)) or
      (raw_is_candidate2 = '1' and
       (rsi = raw_candidate2 or rti = raw_candidate2)) then
      o_raw_detected <= '1';
    else
      o_raw_detected <= '0';
    end if;
  end process raw_detector;
end architecture rtl;
