-------------------------------------------------------------------------------
-- Title      : Single clock static RAM
-- Project    : Memory
-------------------------------------------------------------------------------
-- File       : sc_sram.vhd
-- Author     : Robert Jarzmik <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2017-02-02
-- Last update: 2018-11-28
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
--   The single clock SRAM provides memory read and write operations.
--   The read data output is registered.
--
-- Mode of operation :
--  - if LOOKAHEAD = false, on clock rise and rren = '1', q is registered with
--    the value of the element at raddr
--  - if LOOKAHEAD = true, q always shows the element at raddr, and it is
--    assumed raddr is registered
--
--  Read operation
--    - assert rren = '1' and raddr
--    - on next clock rising edge, q has the read data
--  Write operation
--    - assert wren = '1', waddr and data
--    - on next clock rising edge, data is written to memory
--
-- Bypassing, aka. read under write behavior :
--   If reading and writting to the same address in the same clock cycle, the
--   registered q output will be either :
--     - the old memory data if READ_UNDER_WRITE = false
--     - the data from "data" input bypassed to q if READ_UNDER_WRITE = true
-------------------------------------------------------------------------------
-- Copyright (c) 2017 Robert Jarzmik <robert.jarzmik@free.fr>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2017-02-02  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library rjarzmik;
use rjarzmik.slv_utils.slv_is_x;

entity sc_sram is
  generic
    (
      ADDR_WIDTH       : positive;
      DATA_WIDTH       : positive;
      READ_UNDER_WRITE : boolean := true;
      LOOKAHEAD        : boolean := false;
      DEBUG_NAME       : string  := "";
      DEBUG            : boolean := false
      );
  port
    (
      clock : in  std_logic;
      raddr : in  std_logic_vector (ADDR_WIDTH - 1 downto 0) := (others => '0');
      waddr : in  std_logic_vector (ADDR_WIDTH - 1 downto 0);
      data  : in  std_logic_vector (DATA_WIDTH - 1 downto 0);
      rren  : in  std_logic;
      wren  : in  std_logic;
      q     : out std_logic_vector (DATA_WIDTH - 1 downto 0) := (others => '0')
      );
end entity sc_sram;

architecture str of sc_sram is
  subtype data_t is std_logic_vector(DATA_WIDTH - 1 downto 0);
  type mem_block_t is array(0 to 2**ADDR_WIDTH - 1) of data_t;

  signal memory       : mem_block_t := (others => (others => '0'));
  signal rdata        : data_t;
  signal bypass       : std_ulogic;

begin  -- architecture str

  readwrite : process(clock, memory, raddr, waddr, wren, data)
  begin
    if LOOKAHEAD and not slv_is_x(raddr) then
      if READ_UNDER_WRITE and bypass = '1' then
        rdata <= data;
      else
        rdata <= memory(to_integer(unsigned(raddr)));
      end if;
    end if;

    if rising_edge(clock) then
      if not LOOKAHEAD and rren = '1' and not slv_is_x(raddr) then
        if READ_UNDER_WRITE and bypass = '1' then
          rdata <= data;
        else
          rdata <= memory(to_integer(unsigned(raddr)));
        end if;
      end if;

      if wren = '1' and not slv_is_x(waddr) then
        memory(to_integer(unsigned(waddr))) <= data;

        -- pragma translate_off
        if DEBUG then
          if DEBUG_NAME'length = 0 then
            report sc_sram'instance_name & ": [" &
              to_hstring(waddr) & "] <= " & to_hstring(data);
          else
            report DEBUG_NAME & ": [" &
              to_hstring(waddr) & "] <= " & to_hstring(data);
          end if;
        end if;
      -- pragma translate_on
      end if;
    end if;

  end process;

  bypasser : process(clock, wren, raddr, waddr, data, rdata)
  begin
    if wren = '1' and (raddr = waddr) then
      bypass <= '1';
    else
      bypass <= '0';
    end if;
  end process;

  q <= rdata;
end architecture str;
