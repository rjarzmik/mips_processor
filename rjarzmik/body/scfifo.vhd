-------------------------------------------------------------------------------
-- Title      : Single clock First-In First-Out buffer
-- Project    : FIFO
-------------------------------------------------------------------------------
-- File       : scfifo.vhd
-- Author     : Robert Jarzmik <robert.jarzmik@free.fr>
-- Company    : None
-- Created    : 2017-01-25
-- Last update: 2017-02-03
-- Platform   : linux
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Fifo for buffering data
-- Mode of operation :
--   - on clk positive edge, if wrreq is asserted, push one data from data[..]
--   - on clk positive edge, if rdreq is asserted, pull one data
--       => this assumes q was consumed before this clock edge
--   - when empty = '0', q holds the next data to pull
--   - when full = '1', pushing a data overwrites a former unconsumed entry
--   - if LOOKAHEAD = true, q always shows the first element to pop, and rdreq
--     pops it out
--     if LOOKAHEAD = false, on clock rise and rdreq = '1', q is registered with
--     the value of the poped element
--
-- Normal usage of the FIFO
--   FIFO filler perspective :
--    - provide the data on data input signal
--    - wait until full = '0'
--    - assert wrreq = '1'
--   FIFO consumer perspective :
--    - wait until empty = '0'
--    - assert rdreq = '1' (if in LOOKAHEAD = false mode)
--    - on next cycle, read data on q output signal
-------------------------------------------------------------------------------
-- Copyright (c) 2017 Robert Jarzmik <robert.jarzmik@free.fr>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2017-01-25  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity scfifo is
  generic (
    DATA_WIDTH : positive;
    DEPTH      : positive;
    LOOKAHEAD  : boolean := false;
    DEBUG_NAME : string  := "";
    DEBUG      : boolean
    );

  port (
    clock : in  std_ulogic;
    aclr  : in  std_ulogic;
    -- pushing into FIFO signals
    full  : out std_ulogic;
    data  : in  std_ulogic_vector(DATA_WIDTH - 1 downto 0);
    wrreq : in  std_ulogic;
    -- poping from FIFO signals
    rdreq : in  std_ulogic;
    q     : out std_ulogic_vector(DATA_WIDTH - 1 downto 0);
    empty : out std_ulogic
    );
end entity scfifo;

architecture str of scfifo is
  subtype data_t is std_ulogic_vector(DATA_WIDTH - 1 downto 0);
  signal pin  : natural range 0 to DEPTH - 1;
  signal pout : natural range 0 to DEPTH - 1;

  type mem_t is array(0 to DEPTH - 1) of data_t;
  signal memory : mem_t := (others => (others => '0'));

  function dbg_name(dname : string) return string is
  begin
    if dname'length = 0 then
      return scfifo'instance_name;
    else
      return dname;
    end if;
  end function dbg_name;

begin  -- architecture str

  pq : process(clock, rdreq, pout)
  begin
    if LOOKAHEAD then
      q <= memory(pout);
    elsif rising_edge(clock) and rdreq = '1' then
      q <= memory(pout);
    end if;
  end process pq;

  process(clock, aclr, rdreq, wrreq)
  begin
    if aclr = '1' then
      pin  <= 0;
      pout <= 0;
    elsif rising_edge(clock) then
      if wrreq = '1' then
        pin <= (pin + 1) mod DEPTH;
      end if;
      if rdreq = '1' then
        pout <= (pout + 1) mod DEPTH;
      end if;
    end if;
  end process;

  emptyer : process(clock, aclr, rdreq, wrreq, pin, pout)
    variable next_empty : std_ulogic;
  begin
    if rdreq = '1' and wrreq = '0' and (pout + 1) mod DEPTH = pin then
      next_empty := '1';
    else
      next_empty := '0';
    end if;

    if aclr = '1' then
      empty <= '1';
    elsif rising_edge(clock) and (rdreq or wrreq) = '1' then
      empty <= next_empty;
    end if;
  end process emptyer;

  fuller : process(clock, aclr, rdreq, wrreq, pin, pout)
    variable next_full : std_ulogic;
  begin
    if rdreq = '0' and wrreq = '1' and (pin + 1) mod DEPTH = pout then
      next_full := '1';
    else
      next_full := '0';
    end if;

    if aclr = '1' then
      full <= '0';
    elsif rising_edge(clock) and (rdreq or wrreq) = '1' then
      full <= next_full;
    end if;
  end process fuller;

  writer : process(clock, wrreq)
  begin
    if rising_edge(clock) and wrreq = '1' then
      memory(pin) <= data;
    end if;
  end process writer;

  debugger : process(clock)
  begin
    if DEBUG and rising_edge(clock) then
      -- pragma translate_off
      if rdreq = '1' then
        report dbg_name(DEBUG_NAME) & ": read[" & integer'image(pout) &
          "] => 0x" & to_hstring(memory(pout));
      end if;
      if wrreq = '1' then
        report dbg_name(DEBUG_NAME) & ": write[" & integer'image(pin) &
          "] <= 0x" & to_hstring(data);
      end if;
    -- pragma translate_on
    end if;
  end process debugger;

end architecture str;
