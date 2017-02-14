-------------------------------------------------------------------------------
-- Title      : Single clock FIFO with input n times larger than output
-- Project    : FIFO
-------------------------------------------------------------------------------
-- File       : scfifo_shrinking.vhd
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
use rjarzmik.slv_utils.all;

entity scfifo_shrinking is
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
end entity scfifo_shrinking;

architecture str of scfifo_shrinking is
  constant TIMES : positive := INNER_DATA_WIDTH / OUTER_DATA_WIDTH;

  subtype fifo_mask_t is std_ulogic_vector(0 to TIMES - 1);
  signal g_full  : fifo_mask_t;
  signal g_empty : fifo_mask_t;
  signal g_rdreq : fifo_mask_t;

  signal fifo_sel : natural range 0 to TIMES - 1;
  signal fifo_mux : natural range 0 to TIMES - 1;

  type arr_data_t is array(0 to TIMES - 1) of std_ulogic_vector(OUTER_DATA_WIDTH - 1 downto 0);
  signal g_data : arr_data_t;
  type arr_q_t is array(0 to TIMES - 1) of std_ulogic_vector(OUTER_DATA_WIDTH - 1 downto 0);
  signal g_q    : arr_q_t;

begin  -- architecture str
  assert INNER_DATA_WIDTH mod OUTER_DATA_WIDTH = 0;

  scs : for i in 0 to TIMES - 1 generate
    scfifo : entity work.scfifo
      generic map (
        DATA_WIDTH => OUTER_DATA_WIDTH,
        DEPTH      => DEPTH,
        LOOKAHEAD  => LOOKAHEAD,
        DEBUG_NAME => DEBUG_NAME,
        DEBUG      => DEBUG)
      port map (
        clock => clock,
        aclr  => aclr,
        rdreq => g_rdreq(i),
        wrreq => wrreq,
        data  => g_data(i),
        q     => g_q(i),
        empty => g_empty(i),
        full  => g_full(i));
    g_rdreq(i) <= rdreq when i = fifo_sel else '0';
  end generate;

  g_datas : for i in 0 to TIMES - 1 generate
    g_data(i) <= data((i + 1) * OUTER_DATA_WIDTH - 1 downto i * OUTER_DATA_WIDTH);
  end generate;

  full  <= or_reduce(g_full);
  empty <= and_reduce(g_empty);

  q <= g_q(fifo_sel) when LOOKAHEAD else g_q(fifo_mux);

  process(clock, aclr, rdreq)
  begin
    if aclr = '1' then
      fifo_sel <= 0;
      fifo_mux <= 0;
    elsif rising_edge(clock) and rdreq = '1' then
      fifo_sel <= (fifo_sel + 1) mod TIMES;
      fifo_mux <= fifo_sel;
    end if;
  end process;

end architecture str;
