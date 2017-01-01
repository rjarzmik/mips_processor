-------------------------------------------------------------------------------
-- Title      : Tags memory with arrays implementation
-- Project    : MIPS processor implementation, compatible MIPS-1
-------------------------------------------------------------------------------
-- File       : memory_cacheline_internal.vhd
-- Author     : Robert Jarzmik (Intel)  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-15
-- Last update: 2016-12-28
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-12-15  1.0      rjarzmik        Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cache_defs.cache_line_t;
use work.cache_defs.cache_line_selector_t;
use work.cache_defs.data_t;

-------------------------------------------------------------------------------

entity memory_cacheline_internal is
  generic
    (
      ADDR_WIDTH : integer := 7;
      DEBUG_IDX : natural := 0;
      DEBUG : boolean := false
      );
  port
    (
      clock : in  std_logic := '1';
      raddr : in  std_logic_vector (ADDR_WIDTH - 1 downto 0);
      waddr : in  std_logic_vector (ADDR_WIDTH - 1 downto 0);
      data  : in  data_t;
      rren  : in  std_logic;
      wren  : in  std_logic;
      q     : out data_t
      );

end entity memory_cacheline_internal;

architecture infer of memory_cacheline_internal is

  type mem_block_t is array(0 to 2**ADDR_WIDTH - 1) of data_t;
  signal memory : mem_block_t := (others => (others => '0'));
  signal raddr_reg : std_logic_vector (ADDR_WIDTH - 1 downto 0) := (others => '0');

begin  -- architecture str

  process(clock, memory, raddr_reg)
  begin
    if rising_edge(clock) then
      if rren = '1' then
        raddr_reg <= raddr;
      end if;

      if wren = '1' then
        memory(to_integer(unsigned(waddr))) <= data;
        
        -- pragma translate_off
        if DEBUG then
          report "Cmem(" & integer'image(DEBUG_IDX) & "): [" &
            to_hstring(waddr) & "] <= " & to_hstring(data);
        end if;
        -- pragma translate_on
      end if;
    end if;

    q <= memory(to_integer(unsigned(raddr_reg)));
  end process;

end architecture infer;

