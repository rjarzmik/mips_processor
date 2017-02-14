-------------------------------------------------------------------------------
-- Title      : Single clock First-In First-Out buffer Synthesis test
-- Project    : FIFO
-------------------------------------------------------------------------------
-- File       : scfifo_syntest.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
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
use rjarzmik.fifos.scfifo;

entity scfifo_syntest is
  generic (
    DATA_WIDTH : positive := 32;
    DEPTH      : positive := 16;
    DEBUG_NAME : string   := "scfifo_syntest";
    DEBUG      : boolean  := false
    );

  port (
    clock : in  std_ulogic;
    aclr  : in  std_ulogic;
    rdreq : in  std_ulogic;
    wrreq : in  std_ulogic;
    data  : in  std_ulogic_vector(DATA_WIDTH - 1 downto 0);
    q     : out std_ulogic_vector(DATA_WIDTH - 1 downto 0);
    empty : out std_ulogic;
    full  : out std_ulogic
    );
end entity scfifo_syntest;

architecture str of scfifo_syntest is
begin  -- architecture str

  scfifo1 : scfifo
    generic map (
      DATA_WIDTH => DATA_WIDTH,
      DEPTH      => DEPTH,
      DEBUG_NAME => DEBUG_NAME,
      DEBUG      => DEBUG)
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
