-------------------------------------------------------------------------------
-- Title      : Testbench for design "scfifo_shrinking"
-- Project    : FIFO
-------------------------------------------------------------------------------
-- File       : scfifo_shrinking_tb.vhd
-- Author     : Robert Jarzmik <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2017-01-30
-- Last update: 2017-02-03
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- Copyright (c) 2017 Robert Jarzmik <robert.jarzmik@free.fr>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2017-01-30  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library rjarzmik;
use rjarzmik.fifos.scfifo_shrinking;

entity scfifo_shrinking_tb is
end entity scfifo_shrinking_tb;

architecture test of scfifo_shrinking_tb is

  -- component generics
  constant INNER_DATA_WIDTH : positive := 16;
  constant OUTER_DATA_WIDTH : positive := 8;
  constant DEPTH            : positive := 2;
  constant LOOKAHEAD        : boolean  := false;
  constant DEBUG_NAME       : string   := "scfifo_enlarging_tb";
  constant DEBUG            : boolean  := true;

  -- component ports
  signal clock : std_ulogic := '1';
  signal aclr  : std_ulogic;
  signal rdreq : std_ulogic;
  signal wrreq : std_ulogic;
  signal data  : std_ulogic_vector(INNER_DATA_WIDTH - 1 downto 0);
  signal q     : std_ulogic_vector(OUTER_DATA_WIDTH - 1 downto 0);
  signal empty : std_ulogic;
  signal full  : std_ulogic;

  -- clock enable
  signal clkena : std_logic := '1';

begin  -- architecture test
  -- component instantiation
  DUT : scfifo_shrinking
    generic map (
      INNER_DATA_WIDTH => INNER_DATA_WIDTH,
      OUTER_DATA_WIDTH => OUTER_DATA_WIDTH,
      DEPTH            => DEPTH,
      LOOKAHEAD        => LOOKAHEAD,
      DEBUG_NAME       => DEBUG_NAME,
      DEBUG            => DEBUG)
    port map (
      clock => clock,
      aclr  => aclr,
      rdreq => rdreq,
      wrreq => wrreq,
      data  => data,
      q     => q,
      empty => empty,
      full  => full);

  -- clock generation
  clock <= not clock and clkena after 5 ps;

  -- test
  Tester : process
  begin
    wrreq <= '0';
    rdreq <= '0';

    wait until clock = '1';
    aclr <= '1';
    wait until clock = '0';
    aclr <= '0';

    assert empty = '1';
    assert full = '0';
    data  <= x"1234";
    wrreq <= '1';

    wait until clock = '1';
    wrreq <= '0';
    rdreq <= '1';
    wait until clock = '0';
    assert empty = '0';
    assert full = '0';

    wait until clock = '1';
    rdreq <= '1';
    wait until clock = '0';
    assert empty = '0';
    assert full = '0';
    assert q = x"34";

    wait until clock = '1';
    rdreq <= '0';
    wait until clock = '0';
    assert empty = '1';
    assert full = '0';
    assert q = x"12";

    wait until clock = '1';
    wait until clock = '0';
    assert empty = '1';
    assert full = '0';

    wait until clock = '1';
    wait until clock = '0';
    assert empty = '1';
    assert full = '0';

    -- Fill the FIFO to full state
    wait until clock = '1';
    data  <= x"1234";
    wrreq <= '1';
    wait until clock = '0';
    assert empty = '1';
    assert full = '0';

    wait until clock = '1';
    data  <= x"5678";
    wrreq <= '1';
    wait until clock = '0';
    assert empty = '0';
    assert full = '0';

    wait until clock = '1';
    wrreq <= '0';
    wait until clock = '0';
    assert empty = '0';
    assert full = '1';

    wait until clock = '1';
    wait until clock = '0';
    assert empty = '0';
    assert full = '1';

    clkena <= '0';
  end process Tester;

end architecture test;

configuration scfifo_shrinking_tb_test_cfg of scfifo_shrinking_tb is
  for test
  end for;
end scfifo_shrinking_tb_test_cfg;
