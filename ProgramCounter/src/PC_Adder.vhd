-------------------------------------------------------------------------------
-- Title      : Program Counter Adder
-- Project    : 
-------------------------------------------------------------------------------
-- File       : PC_Adder.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-11
-- Last update: 2016-11-14
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: MIPS Program counter adder
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-11  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-------------------------------------------------------------------------------

entity PC_Adder is

  generic (
    ADDR_WIDTH : integer := 32;
    STEP       : integer
    );

  port (
    current_pc : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    next_pc    : out std_logic_vector(ADDR_WIDTH - 1 downto 0)
    );

end entity PC_Adder;

-------------------------------------------------------------------------------

architecture rtl of PC_Adder is

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  signal stepper : unsigned(ADDR_WIDTH - 1 downto 0);

begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------

  stepper <= unsigned(to_signed(STEP, ADDR_WIDTH));
  next_pc <= std_logic_vector(unsigned(current_pc) + stepper);

end architecture rtl;

-------------------------------------------------------------------------------
