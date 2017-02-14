-------------------------------------------------------------------------------
-- Title      : Single clock FIFO with output n times larger than input
-- Project    : FIFO
-------------------------------------------------------------------------------
-- File       : scfifo_enlarging.vhd
-- Author     : Robert Jarzmik <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2017-01-30
-- Last update: 2017-02-03
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Single clock FIFO with output n times larger than input
--   Mainly see scfifo description
--   The constraint is that OUTER_DATA_WIDTH = N * INNER_DATA_WIDTH.
--   DEPTH is the INNER input number of elements before FIFO is full.
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
use rjarzmik.slv_utils.all;

entity scfifo_enlarging is
  generic (
    INNER_DATA_WIDTH : positive;
    OUTER_DATA_WIDTH : positive;
    DEPTH            : positive;
    LOOKAHEAD        : boolean := false;
    DEBUG_NAME       : string  := "";
    DEBUG            : boolean
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
end entity scfifo_enlarging;

architecture str of scfifo_enlarging is
  constant TIMES : positive := OUTER_DATA_WIDTH / INNER_DATA_WIDTH;

  subtype fifo_mask_t is std_ulogic_vector(0 to TIMES - 1);
  signal g_full  : fifo_mask_t;
  signal g_empty : fifo_mask_t;
  signal g_wrreq : fifo_mask_t;

  signal fifo_sel : natural range 0 to TIMES - 1;

  type arr_data_t is array(0 to TIMES - 1) of std_ulogic_vector(INNER_DATA_WIDTH - 1 downto 0);
  signal g_q : arr_data_t;

begin  -- architecture str
  assert OUTER_DATA_WIDTH mod INNER_DATA_WIDTH = 0;

  scs : for i in 0 to TIMES - 1 generate
    scfifo : entity work.scfifo
      generic map (
        DATA_WIDTH => INNER_DATA_WIDTH,
        DEPTH      => DEPTH,
        LOOKAHEAD  => LOOKAHEAD,
        DEBUG_NAME => DEBUG_NAME,
        DEBUG      => DEBUG)
      port map (
        clock => clock,
        aclr  => aclr,
        rdreq => rdreq,
        wrreq => g_wrreq(i),
        data  => data,
        q     => g_q(i),
        empty => g_empty(i),
        full  => g_full(i));
    g_wrreq(i) <= wrreq when i = fifo_sel else '0';
  end generate;

  full  <= and_reduce(g_full);
  empty <= or_reduce(g_empty);

  g_qs : for i in 0 to TIMES - 1 generate
    q((i + 1) * INNER_DATA_WIDTH - 1 downto i * INNER_DATA_WIDTH) <= g_q(i);
  end generate;

  process(clock, aclr, wrreq)
  begin
    if aclr = '1' then
      fifo_sel <= 0;
    elsif rising_edge(clock) and wrreq = '1' then
      fifo_sel <= (fifo_sel + 1) mod TIMES;
    end if;
  end process;

end architecture str;
