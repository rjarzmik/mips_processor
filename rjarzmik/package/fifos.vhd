-------------------------------------------------------------------------------
-- Title      : FIFOs utilities
-- Project    : FIFO
-------------------------------------------------------------------------------
-- File       : fifos.vhd
-- Author     : Robert Jarzmik <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2017-01-31
-- Last update: 2017-02-03
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Various fifos
-------------------------------------------------------------------------------
-- Copyright (c) 2017 Robert Jarzmik <robert.jarzmik@free.fr>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2017-01-31  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package fifos is
  component scfifo is
    generic (
      DATA_WIDTH : positive;
      DEPTH      : positive;
      LOOKAHEAD  : boolean;
      DEBUG_NAME : string;
      DEBUG      : boolean);
    port (
      clock : in  std_ulogic;
      aclr  : in  std_ulogic;
      full  : out std_ulogic;
      data  : in  std_ulogic_vector(DATA_WIDTH - 1 downto 0);
      wrreq : in  std_ulogic;
      rdreq : in  std_ulogic;
      q     : out std_ulogic_vector(DATA_WIDTH - 1 downto 0);
      empty : out std_ulogic);
  end component scfifo;

  component scfifo_shrinking is
    generic (
      INNER_DATA_WIDTH : positive;
      OUTER_DATA_WIDTH : positive;
      DEPTH            : positive;
      LOOKAHEAD        : boolean;
      DEBUG_NAME       : string;
      DEBUG            : boolean);
    port (
      clock : in  std_ulogic;
      aclr  : in  std_ulogic;
      rdreq : in  std_ulogic;
      wrreq : in  std_ulogic;
      data  : in  std_ulogic_vector(INNER_DATA_WIDTH - 1 downto 0);
      q     : out std_ulogic_vector(OUTER_DATA_WIDTH - 1 downto 0);
      empty : out std_ulogic;
      full  : out std_ulogic);
  end component scfifo_shrinking;

  component scfifo_enlarging is
    generic (
      INNER_DATA_WIDTH : positive;
      OUTER_DATA_WIDTH : positive;
      DEPTH            : positive;
      LOOKAHEAD        : boolean;
      DEBUG_NAME       : string;
      DEBUG            : boolean);
    port (
      clock : in  std_ulogic;
      aclr  : in  std_ulogic;
      rdreq : in  std_ulogic;
      wrreq : in  std_ulogic;
      data  : in  std_ulogic_vector(INNER_DATA_WIDTH - 1 downto 0);
      q     : out std_ulogic_vector(OUTER_DATA_WIDTH - 1 downto 0);
      empty : out std_ulogic;
      full  : out std_ulogic);
  end component scfifo_enlarging;

  component fifo2memory is
    generic (
      ADDR_WIDTH      : positive;
      DATA_WIDTH      : positive;
      MAX_NBREADS     : positive;
      ADDR_STEP       : integer;
      ADDR_STEP_WIDTH : positive;
      DEBUG_NAME      : string;
      DEBUG           : boolean);
    port (
      clock    : in  std_ulogic;
      saddr    : in  std_ulogic;
      addr     : in  std_ulogic_vector(ADDR_WIDTH - 1 downto 0);
      ren      : in  std_ulogic;
      nbreads  : in  natural range 0 to MAX_NBREADS;
      wen      : in  std_ulogic;
      ready    : out std_ulogic;
      full     : in  std_ulogic;
      wdata    : out std_ulogic_vector(DATA_WIDTH - 1 downto 0);
      wrreq    : out std_ulogic;
      empty    : in  std_ulogic;
      rdreq    : out std_ulogic;
      rdata    : in  std_ulogic_vector(DATA_WIDTH - 1 downto 0);
      mreq     : out std_ulogic;
      mwen     : out std_ulogic;
      maddr    : out std_ulogic_vector(ADDR_WIDTH - 1 downto 0);
      mrdata   : in  std_ulogic_vector(DATA_WIDTH - 1 downto 0);
      mwdata   : out std_ulogic_vector(DATA_WIDTH - 1 downto 0);
      mwready  : in  std_ulogic;
      mrdvalid : in  std_ulogic);
  end component fifo2memory;

end package fifos;
