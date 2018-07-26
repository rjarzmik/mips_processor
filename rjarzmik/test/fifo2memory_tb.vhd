-------------------------------------------------------------------------------
-- Title      : Testbench for design "fifo2memory"
-- Project    : FIFO
-------------------------------------------------------------------------------
-- File       : fifo2memory_tb.vhd
-- Author     : Robert Jarzmik <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2017-01-31
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
-- 2017-01-31  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library rjarzmik;
use rjarzmik.fifos.fifo2memory;
use rjarzmik.fifos.scfifo;

entity fifo2memory_tb is
end entity fifo2memory_tb;

architecture test of fifo2memory_tb is
  -- component generics
  constant ADDR_WIDTH : positive := 16;
  constant DATA_WIDTH : positive := 16;
  constant DEBUG_NAME : string   := "fifo2memory_tb";
  constant DEBUG      : boolean  := true;

  -- component ports
  signal clock    : std_ulogic := '1';
  signal saddr    : std_ulogic;
  signal addr     : std_ulogic_vector(ADDR_WIDTH - 1 downto 0);
  signal ren      : std_ulogic := '0';
  signal nbreads  : natural;
  signal wen      : std_ulogic := '0';
  signal empty    : std_ulogic;
  signal full     : std_ulogic;
  signal rdata    : std_ulogic_vector(DATA_WIDTH - 1 downto 0);
  signal wdata    : std_ulogic_vector(DATA_WIDTH - 1 downto 0);
  signal rdreq    : std_ulogic;
  signal wrreq    : std_ulogic;
  signal mreq     : std_ulogic := '0';
  signal mwen     : std_ulogic := '0';
  signal maddr    : std_ulogic_vector(ADDR_WIDTH - 1 downto 0);
  signal mrdata   : std_ulogic_vector(DATA_WIDTH - 1 downto 0);
  signal mwdata   : std_ulogic_vector(DATA_WIDTH - 1 downto 0);
  signal mwready  : std_ulogic;
  signal mrdvalid : std_ulogic;

  -- fifo ports
  signal aclr     : std_ulogic;
  --- fifo for memory reads signals
  signal fr_full  : std_ulogic;
  signal fr_wdata : std_ulogic_vector(DATA_WIDTH - 1 downto 0);
  signal fr_wrreq : std_ulogic;
  signal fr_empty : std_ulogic;
  signal fr_rdreq : std_ulogic;
  signal fr_rwreq : std_ulogic;
  signal fr_rdata : std_ulogic_vector(DATA_WIDTH - 1 downto 0);
  --- fifo for memory writes signals
  signal fw_full  : std_ulogic;
  signal fw_wdata : std_ulogic_vector(DATA_WIDTH - 1 downto 0);
  signal fw_wrreq : std_ulogic;
  signal fw_empty : std_ulogic;
  signal fw_rdreq : std_ulogic;
  signal fw_rwreq : std_ulogic;
  signal fw_rdata : std_ulogic_vector(DATA_WIDTH - 1 downto 0);

  -- clock
  signal clkena1 : std_logic := '1';
  signal clkena2 : std_logic := '1';

begin  -- architecture test

  -- component instantiation
  scfifo_mread : scfifo
    generic map (
      DATA_WIDTH => DATA_WIDTH,
      DEPTH      => 16,
      LOOKAHEAD  => false,
      DEBUG_NAME => DEBUG_NAME,
      DEBUG      => DEBUG)
    port map (
      clock => clock,
      aclr  => aclr,
      rdreq => fr_rdreq,
      wrreq => fr_wrreq,
      data  => fr_wdata,
      q     => fr_rdata,
      empty => fr_empty,
      full  => fr_full);

  scfifo_mwrite : scfifo
    generic map (
      DATA_WIDTH => DATA_WIDTH,
      DEPTH      => 16,
      LOOKAHEAD  => false,
      DEBUG_NAME => DEBUG_NAME,
      DEBUG      => DEBUG)
    port map (
      clock => clock,
      aclr  => aclr,

      full  => fw_full,
      data  => fw_wdata,
      wrreq => fw_wrreq,

      rdreq => fw_rdreq,
      q     => fw_rdata,
      empty => fw_empty
      );

  DUT_mread : fifo2memory
    generic map (
      ADDR_WIDTH  => ADDR_WIDTH,
      DATA_WIDTH  => DATA_WIDTH,
      MAX_NBREADS => 16,
      DEBUG_NAME  => DEBUG_NAME,
      DEBUG       => DEBUG)
    port map (
      clock   => clock,
      saddr   => saddr,
      addr    => addr,
      ren     => ren,
      nbreads => nbreads,
      wen     => wen,

      -- Memory read queries interface
      full  => fr_full,
      wrreq => fr_wrreq,
      wdata => fr_wdata,

      -- Memory write queries interface
      empty => fw_empty,
      rdreq => fw_rdreq,
      rdata => fw_rdata,

      mreq     => mreq,
      mwen     => mwen,
      maddr    => maddr,
      mrdata   => mrdata,
      mwdata   => mwdata,
      mwready  => mwready,
      mrdvalid => mrdvalid);

  -- memory simulator
  Simulated_Memory_1 : entity work.Simulated_Memory
    generic map (
      ADDR_WIDTH        => ADDR_WIDTH,
      DATA_WIDTH        => DATA_WIDTH,
      MEMORY_ADDR_WIDTH => 16,
      MEMORY_LATENCY    => 3,
      DEBUG_NAME        => DEBUG_NAME,
      DEBUG             => DEBUG)
    port map (
      clk                 => clock,
      rst                 => '0',
      i_memory_req        => mreq,
      i_memory_we         => mwen,
      i_memory_addr       => maddr,
      i_memory_write_data => mwdata,
      o_memory_read_data  => mrdata,
      o_memory_valid      => mrdvalid);
  mwready <= mrdvalid;

  -- clock generation
  clock <= (clkena1 or clkena2) and not clock after 5 ps;

  reader_test : process
  begin
    fr_rdreq <= '0';
    ren      <= '0';
    aclr     <= '1';

    wait until clock = '0';
    saddr <= '1';
    addr  <= x"0040";

    wait until clock = '0';
    aclr    <= '0';
    ren     <= '1';
    nbreads <= 2;
    saddr   <= '0';
    assert fr_empty = '1';
    assert fr_full = '0';

    wait until clock = '0';
    ren <= '0';
    assert fr_empty = '1';
    assert fr_full = '0';

    wait until fr_empty = '0';
    wait until clock = '0';
    fr_rdreq <= '1';

    wait until clock = '0';
    fr_rdreq <= '0';
    assert fr_rdata = x"0140";

    wait until fr_empty = '0';
    wait until clock = '0';
    fr_rdreq <= '1';

    wait until clock = '0';
    fr_rdreq <= '0';
    assert fr_rdata = x"0142";

    ren     <= '0';
    clkena1 <= '0';
    wait until clkena2 = '0';
  end process reader_test;

  writer_test : process
  begin
    fw_wrreq <= '0';
    wen      <= '0';

    wait until clkena1 = '0';
    wen <= '1';

    wait until clock = '0';

    wait until clock = '0';
    fw_wdata <= x"6789";
    fw_wrreq <= '1';
    assert fw_empty = '1';
    assert fw_full = '0';

    wait until clock = '0';
    fw_wdata <= x"abcd";
    fw_wrreq <= '1';
    assert fw_empty = '0';
    assert fw_full = '0';

    wait until clock = '0';
    fw_wrreq <= '0';

    -- no wait for mreq = '1', as it happens after previous wait for clock = '0'
    assert mwen = '1';
    assert maddr = x"0044";
    assert mwdata = x"6789";

    wait until mreq = '1';
    assert mwen = '1';
    assert maddr = x"0046";
    assert mwdata = x"abcd";

    wait until clock = '0';
    wait until clock = '0';
    wait until clock = '0';
    wait until clock = '0';
    wait until clock = '0';
    wait until clock = '0';
    clkena2 <= '0';
  end process writer_test;

end architecture test;

configuration fifo2memory_tb_test_cfg of fifo2memory_tb is
  for test
  end for;
end fifo2memory_tb_test_cfg;
