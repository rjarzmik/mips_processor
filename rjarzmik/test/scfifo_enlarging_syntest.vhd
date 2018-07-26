-------------------------------------------------------------------------------
-- Title      : Single clock First-In First-Out buffer Synthesis test
-- Project    : FIFO
-------------------------------------------------------------------------------
-- File       : scfifo_enlarging_syntest.vhd
-- Author     : Robert Jarzmik <robert.jarzmik@free.fr>
-- Company    : None
-- Created    : 2017-01-25
-- Last update: 2017-02-03
-- Platform   : linux
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Fifo for buffering data
-------------------------------------------------------------------------------
-- Copyright (c) 2017 Robert Jarzmik <robert.jarzmik@free.fr>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2017-01-25  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library rjarzmik;
use rjarzmik.fifos.scfifo_enlarging;

entity scfifo_enlarging_syntest is
  generic (
    INNER_DATA_WIDTH : positive := 8;
    OUTER_DATA_WIDTH : positive := 32;
    DEPTH            : positive := 16;
    DEBUG_NAME       : string   := "scfifo_enlarging_syntest";
    DEBUG            : boolean  := false
    );

  port (
    clock : in  std_ulogic;
    aclr  : in  std_ulogic;
    rdreq : in  std_ulogic;
    wrreq : in  std_ulogic;
    data  : in  std_ulogic_vector(INNER_DATA_WIDTH - 1 downto 0);
    q     : out std_ulogic_vector(OUTER_DATA_WIDTH - 1 downto 0);
    empty : out std_ulogic;
    full  : out std_ulogic
    );
end entity scfifo_enlarging_syntest;

architecture str of scfifo_enlarging_syntest is
begin  -- architecture str

  scfifo_enlarging1 : scfifo_enlarging
    generic map (
      INNER_DATA_WIDTH => INNER_DATA_WIDTH,
      OUTER_DATA_WIDTH => OUTER_DATA_WIDTH,
      DEPTH            => DEPTH,
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

end architecture str;
