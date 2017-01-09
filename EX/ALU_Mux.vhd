-------------------------------------------------------------------------------
-- Title      : ALU output muxer
-- Project    : Cache implementations
-------------------------------------------------------------------------------
-- File       : ALU_Mux.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2017-01-08
-- Last update: 2017-01-09
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2017 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2017-01-08  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use work.cpu_defs.all;

-------------------------------------------------------------------------------

entity ALU_Mux is
  generic (DATA_WIDTH : integer);

  port (
    i_alu_op : in  alu_op_type;
    i_add    : in  std_logic_vector(DATA_WIDTH * 2 - 1 downto 0);
    i_sub    : in  std_logic_vector(DATA_WIDTH * 2 - 1 downto 0);
    i_mul    : in  std_logic_vector(DATA_WIDTH * 2 - 1 downto 0);
    i_div    : in  std_logic_vector(DATA_WIDTH * 2 - 1 downto 0);
    i_and    : in  std_logic_vector(DATA_WIDTH * 2 - 1 downto 0);
    i_or     : in  std_logic_vector(DATA_WIDTH * 2 - 1 downto 0);
    i_xor    : in  std_logic_vector(DATA_WIDTH * 2 - 1 downto 0);
    i_nor    : in  std_logic_vector(DATA_WIDTH * 2 - 1 downto 0);
    i_slt    : in  std_logic_vector(DATA_WIDTH * 2 - 1 downto 0);
    q        : out std_logic_vector(DATA_WIDTH * 2 - 1 downto 0)
    );

end entity ALU_Mux;

architecture str of ALU_Mux is
begin  -- architecture str

  with i_alu_op select q <=
    i_add when add,
    i_sub when substract,
    i_mul when multiply,
    i_div when divide,
    i_and when log_and,
    i_or  when log_or,
    i_nor when log_nor,
    i_xor when log_xor,
    i_slt when slt,
    i_add when all_zero;

end architecture str;
