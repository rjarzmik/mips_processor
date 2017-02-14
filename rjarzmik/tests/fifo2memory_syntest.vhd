-------------------------------------------------------------------------------
-- Title      : FIFO to Memory synthesis test
-- Project    : FIFO
-------------------------------------------------------------------------------
-- File       : fifo2memory_syntest.vhd
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

entity fifo2memory_syntest is
  generic (
    ADDR_WIDTH  : positive := 24;
    DATA_WIDTH  : positive := 16;
    MAX_NBREADS : positive := 2;
    DEBUG_NAME  : string   := "fifo2memory_syntest";
    DEBUG       : boolean  := true
    );

  port (
    clock    : in  std_ulogic;
    -- Query interface
    saddr    : in  std_ulogic;
    addr     : in  std_ulogic_vector(ADDR_WIDTH - 1 downto 0);
    ren      : in  std_ulogic;
    nbreads  : in  natural range 0 to MAX_NBREADS - 1;
    wen      : in  std_ulogic;
    -- FIFO interface
    empty    : in  std_ulogic;
    full     : in  std_ulogic;
    rdata    : in  std_ulogic_vector(DATA_WIDTH - 1 downto 0);
    wdata    : out std_ulogic_vector(DATA_WIDTH - 1 downto 0);
    rdreq    : out std_ulogic;
    wrreq    : out std_ulogic;
    -- Memory interface
    mreq     : out std_ulogic;
    mwen     : out std_ulogic;
    maddr    : out std_ulogic_vector(ADDR_WIDTH - 1 downto 0);
    mrdata   : in  std_ulogic_vector(DATA_WIDTH - 1 downto 0);
    mwdata   : out std_ulogic_vector(DATA_WIDTH - 1 downto 0);
    mwready  : in  std_ulogic;
    mrdvalid : in  std_ulogic
    );
end entity fifo2memory_syntest;

architecture str of fifo2memory_syntest is
begin  -- architecture str

  fifo2memory_1 : entity fifo2memory
    generic map (
      ADDR_WIDTH  => ADDR_WIDTH,
      DATA_WIDTH  => DATA_WIDTH,
      MAX_NBREADS => MAX_NBREADS,
      DEBUG_NAME  => DEBUG_NAME,
      DEBUG       => DEBUG)
    port map (
      clock    => clock,
      saddr    => saddr,
      addr     => addr,
      ren      => ren,
      nbreads  => nbreads,
      wen      => wen,
      empty    => empty,
      full     => full,
      rdata    => rdata,
      wdata    => wdata,
      rdreq    => rdreq,
      wrreq    => wrreq,
      mreq     => mreq,
      mwen     => mwen,
      maddr    => maddr,
      mrdata   => mrdata,
      mwdata   => mwdata,
      mwready  => mwready,
      mrdvalid => mrdvalid);

end architecture str;
