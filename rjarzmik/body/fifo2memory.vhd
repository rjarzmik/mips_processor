-------------------------------------------------------------------------------
-- Title      : FIFO to Memory transfer module
-- Project    : FIFO
-------------------------------------------------------------------------------
-- File       : fifo2memory.vhd
-- Author     : Robert Jarzmik <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2017-01-31
-- Last update: 2017-02-13
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Transfers any available data in FIFO to or from MEMORY
--  For write type module, saddr asks to store and address for further writes.
--
--  This module copes with transfers both from and to memory or another cache.
--  The theory of operation supposes there are 2 FIFOs connectected to it :
--    - one feeding data to be written to memory
--    - one fed by this module from data read from memory
--
--  Memory addresses stepping :
--    - after each operation, a new address is chosen for the next operation
--    - saddr enforces input add to be the next memory operation address
--    - if saddr = '0', then the next memory takes place at :
--        addr(ADDR_WIDTH - 1 downto ADDR_STEP_WIDTH) &
--       (addr(ADDR_STEP_WIDTH - 1 downto 0)+ ADDR_STEP)
--      This means that for an ADDR_STEP_WIDTH = 4, and ADDR_STEP = 4, every 4
--      memory operations the addr rotates back to its original value.
--
--  Mode of operation :
--    READ operation
--      - wait for ready = '1'
--      - setup addr to the address to be read from
--      - set saddr = '1' to enable addr latching
--      - set nbreads to the number of DATA_WIDTH to be read
--      - set ren = '1' to enable read operation to begin
--      - wait for ready = '1' for the operation to be completed
--        This might require poping from the FIFO (connected to full, wdata,
--        wrreq) if its size is smaller than nbreads).
--    WRITE opeartion
--      - wait for ready = '1'
--      - setup addr to the address to be read from
--      - set saddr = '1' to enable addr latching
--      - set wen = '1' to enable read operation to begin
--      - push elements into the FIFO  (connected to empty, rdreq, rdata) and
--        they will be written to memory.
--
-------------------------------------------------------------------------------
-- Copyright (c) 2017 Robert Jarzmik <robert.jarzmik@free.fr>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2017-01-31  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library rjarzmik;
use rjarzmik.slv_utils.slv_is_x;

entity fifo2memory is
  generic (
    ADDR_WIDTH      : positive;
    DATA_WIDTH      : positive;
    ADDR_STEP       : integer;   -- reasonable default being DATA_WIDTH / 8;
    ADDR_STEP_WIDTH : positive;         -- reasonable default begin ADDR_WIDTH;
    MAX_NBREADS     : positive;
    DEBUG_NAME      : string := "";
    DEBUG           : boolean
    );

  port (
    clock    : in  std_ulogic;
    -- Query interface
    saddr    : in  std_ulogic;
    addr     : in  std_ulogic_vector(ADDR_WIDTH - 1 downto 0);
    ren      : in  std_ulogic;
    nbreads  : in  natural range 0 to MAX_NBREADS;
    wen      : in  std_ulogic;
    ready    : out std_ulogic;
    -- FIFO interface
    --- Memory read query signals
    full     : in  std_ulogic;
    wdata    : out std_ulogic_vector(DATA_WIDTH - 1 downto 0);
    wrreq    : out std_ulogic;
    --- Memory write query signals
    empty    : in  std_ulogic;
    rdreq    : out std_ulogic;
    rdata    : in  std_ulogic_vector(DATA_WIDTH - 1 downto 0);
    -- Memory interface
    mreq     : out std_ulogic;
    mwen     : out std_ulogic;
    maddr    : out std_ulogic_vector(ADDR_WIDTH - 1 downto 0);
    mrdata   : in  std_ulogic_vector(DATA_WIDTH - 1 downto 0);
    mwdata   : out std_ulogic_vector(DATA_WIDTH - 1 downto 0);
    mwready  : in  std_ulogic;
    mrdvalid : in  std_ulogic
    );
end entity fifo2memory;

architecture str of fifo2memory is
  type state_t is (s_idle, s_setup_reading, s_reading, s_setup_writting,
                   s_writting);
  signal state  : state_t := s_idle;
  signal lmreq  : std_ulogic;
  signal lmaddr : std_ulogic_vector(ADDR_WIDTH - 1 downto 0);
  signal remain : natural range 0 to MAX_NBREADS - 1;

  function dbg_name(dname : string) return string is
  begin
    if dname'length = 0 then
      return fifo2memory'instance_name;
    else
      return dname;
    end if;
  end function dbg_name;

begin  -- architecture str

  ready <= '1' when state = s_idle else '0';

  -- Memory interface
  mreq  <= lmreq;
  maddr <= lmaddr;

  p_lmaddr : process(clock, saddr, addr, lmreq, lmaddr)
    variable next_maddr : std_ulogic_vector(ADDR_WIDTH - 1 downto 0);
    variable tmp        : unsigned(ADDR_WIDTH - 1 downto 0);
  begin
    if saddr = '1' then
      next_maddr := addr;
    else
      next_maddr := lmaddr;
      tmp        := (others => '0');
      if not slv_is_x(std_logic_vector(lmaddr)) then
        tmp(ADDR_STEP_WIDTH - 1 downto 0) := unsigned(lmaddr(ADDR_STEP_WIDTH - 1 downto 0));
        tmp                               := tmp + ADDR_STEP;
        next_maddr                        := lmaddr(lmaddr'length - 1 downto ADDR_STEP_WIDTH) &
                      std_ulogic_vector(tmp(ADDR_STEP_WIDTH - 1 downto 0));
      end if;
    end if;

    if rising_edge(clock) and (lmreq = '1' or saddr = '1') then
      lmaddr <= next_maddr;
    end if;
  end process p_lmaddr;

  controller : process(clock, state, empty, ren, wen, rdata, mrdata, mrdvalid,
                       lmreq, full, mwready, remain)
    variable ns : state_t;
  begin
    lmreq  <= '0';
    mwen   <= wen;
    mwdata <= rdata;
    wdata  <= mrdata;
    rdreq  <= '0';
    wrreq  <= '0';

    ns := state;
    case state is
      when s_idle =>
        if empty = '0' and wen = '1' then
          ns    := s_setup_writting;
          rdreq <= '1';
        end if;
        if ren = '1' then
          ns    := s_setup_reading;
          rdreq <= '0';
        end if;

      when s_setup_reading =>
        lmreq <= '1';
        ns    := s_reading;

      when s_reading =>
        if mrdvalid = '1' and full = '0' then
          wrreq <= '1';
          if remain = 0 then
            ns := s_idle;
          else
            lmreq <= '1';
            ns    := s_reading;
          end if;
        end if;

      when s_setup_writting =>
        if mwready = '1' then
          ns    := s_writting;
          lmreq <= '1';
        end if;

      when s_writting =>
        if mwready = '1' then
          if empty = '0' and wen = '1' then
            ns    := s_setup_writting;
            rdreq <= '1';
          else
            ns := s_idle;
          end if;
        end if;
    end case;

    if rising_edge(clock) then
      state <= ns;
    end if;

    if rising_edge(clock) then
      if state = s_idle and ren = '1' then
        remain <= nbreads - 1;
      end if;
      if state = s_reading and mrdvalid = '1' and full = '0' then
        remain <= (remain - 1) mod MAX_NBREADS;
      end if;
    end if;

  end process controller;

  debugger : process(clock)
  begin
    if DEBUG and rising_edge(clock) then
      -- pragma translate_off
      if lmreq = '1' then
        if mwen = '0' then
          report dbg_name(DEBUG_NAME) & ": memory read request @" & to_hstring(lmaddr);
        else
          report dbg_name(DEBUG_NAME) & ": memory write request @" & to_hstring(lmaddr) &
            "<=" & to_hstring(rdata);
        end if;
      end if;
    -- pragma translate_on
    end if;
  end process debugger;
end architecture str;
