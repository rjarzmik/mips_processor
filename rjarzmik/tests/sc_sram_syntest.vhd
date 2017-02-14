-------------------------------------------------------------------------------
-- Title      : Testbench for design "sc_sram" synthesis test
-- Project    : Cache
-------------------------------------------------------------------------------
-- File       : sc_sram_syntest.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2017-02-11
-- Last update: 2017-02-13
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- Copyright (c) 2017 Robert Jarzmik <robert.jarzmik@free.fr>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2017-02-11  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library rjarzmik;
use rjarzmik.memories.sc_sram;

entity sc_sram_syntest is
  generic
    (
      ADDR_WIDTH       : positive := 16;
      DATA_WIDTH       : positive := 8;
      READ_UNDER_WRITE : boolean := false;
      LOOKAHEAD        : boolean := true;
      DEBUG_NAME       : string  := "syntest";
      DEBUG            : boolean := false
      );
  port
    (
      clock : in  std_logic;
      raddr : in  std_logic_vector (ADDR_WIDTH - 1 downto 0);
      waddr : in  std_logic_vector (ADDR_WIDTH - 1 downto 0);
      data  : in  std_logic_vector (DATA_WIDTH - 1 downto 0);
      rren  : in  std_logic;
      wren  : in  std_logic;
      q     : out std_logic_vector (DATA_WIDTH - 1 downto 0) := (others => '0')
      );
end entity sc_sram_syntest;

architecture test of sc_sram_syntest is
begin

  DUT: sc_sram
    generic map (
      ADDR_WIDTH       => ADDR_WIDTH,
      DATA_WIDTH       => DATA_WIDTH,
      READ_UNDER_WRITE => READ_UNDER_WRITE,
      LOOKAHEAD        => LOOKAHEAD,
      DEBUG_NAME       => DEBUG_NAME,
      DEBUG            => DEBUG)
    port map (
      clock => clock,
      raddr => raddr,
      waddr => waddr,
      data  => data,
      rren  => rren,
      wren  => wren,
      q     => q);

end architecture test;

configuration sc_sram_syntest_test_cfg of sc_sram_syntest is
  for test
  end for;
end sc_sram_syntest_test_cfg;
