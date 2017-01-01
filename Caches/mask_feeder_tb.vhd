-------------------------------------------------------------------------------
-- Title      : Testbench for design "mask_feeder"
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : mask_feeder_tb.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-21
-- Last update: 2016-12-22
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-12-21  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

-------------------------------------------------------------------------------

entity mask_feeder_tb is

end entity mask_feeder_tb;

-------------------------------------------------------------------------------

architecture str of mask_feeder_tb is

  -- component generics
  constant WIDTH : natural := 8;

  -- component ports
  signal clk      : std_logic := '1';
  signal sclr     : std_logic;
  signal sdata    : std_logic_vector(WIDTH - 1 downto 0);
  signal bclrena  : std_logic;
  signal bclr     : natural range 0 to WIDTH - 1;
  signal fbitset  : natural range 0 to WIDTH - 1;
  signal allclear : std_logic;
  signal dbg_data : std_logic_vector(WIDTH - 1 downto 0);

  signal clkena : std_logic := '1';
begin  -- architecture str

  -- component instantiation
  DUT : entity work.mask_feeder
    generic map (
      WIDTH => WIDTH)
    port map (
      clk      => clk,
      sclr     => sclr,
      sdata    => sdata,
      bclrena  => bclrena,
      bclr     => bclr,
      fbitset  => fbitset,
      allclear => allclear,
      dbg_data => dbg_data);

  -- clock generation
  clk    <= (clkena and not clk) after 5 ps;
  clkena <= '0'                  after 120 ps;

  sclr  <= '0' after 0 ps, '1' after 10 ps, '0' after 20 ps;
  sdata <= x"5e";

  bclrena <= '0' after 0 ps, '1' after 20 ps, '0' after 120 ps;
  bclr    <= 5   after 21 ps, 6 after 31 ps, 7 after 41 ps, 7 after 51 ps,
          4 after 60 ps, 3 after 71 ps, 2 after 81 ps, 1 after 91 ps,
          0 after 101 ps;

  -- waveform generation
  WaveGen_Proc : process
  begin
    wait until Clk = '1';
    report "FBitSet=" & integer'image(fbitset) &
      ", allclear=" & std_logic'image(allclear) &
      ", bclr=" & integer'image(bclr) &
      ", dbg_data = " & to_bstring(dbg_data);
  end process WaveGen_Proc;

  -- asserts
  asserter : process
  begin
    wait until Clk = '1' and NOW = 20 ps;
    assert fbitset = 6 severity error;
    assert allclear = '0';

    wait until Clk = '1'; -- 30 ps;
    assert fbitset = 6 severity error;
    assert allclear = '0';

    wait until Clk = '1'; -- 40 ps
    assert fbitset = 6 severity error;
    assert allclear = '0';

    wait until Clk = '1'; -- 50 ps
    assert fbitset = 4 severity error;
    assert allclear = '0';

    wait until Clk = '1'; -- 60 ps
    assert fbitset = 4 severity error;
    assert allclear = '0';

    wait until Clk = '1'; -- 70 ps
    assert fbitset = 3 severity error;
    assert allclear = '0';

    wait until Clk = '1'; -- 80 ps
    assert fbitset = 3 severity error;
    assert allclear = '0';

    wait until Clk = '1'; -- 90 ps
    assert fbitset = 2 severity error;
    assert allclear = '0';

    wait until Clk = '1'; -- 100 ps
    assert fbitset = 1 severity error;
    assert allclear = '0';

    wait until Clk = '1'; -- 110 ps
    assert fbitset = 0 severity error;
    assert allclear = '1';
  end process asserter;

end architecture str;

-------------------------------------------------------------------------------

configuration mask_feeder_tb_str_cfg of mask_feeder_tb is
  for str
  end for;
end mask_feeder_tb_str_cfg;

-------------------------------------------------------------------------------
