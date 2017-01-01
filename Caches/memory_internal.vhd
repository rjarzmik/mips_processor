-------------------------------------------------------------------------------
-- Title      : Tags memory with arrays implementation
-- Project    : MIPS processor implementation, compatible MIPS-1
-------------------------------------------------------------------------------
-- File       : memory_internal.vhd
-- Author     : Robert Jarzmik (Intel)  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-15
-- Last update: 2016-12-16
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

-------------------------------------------------------------------------------

entity memory_internal is
  generic
    (
      ADDR_WIDTH : integer := 7;
      DATA_WIDTH : integer := 32
      );
  port
    (
      clock : in  std_logic := '1';
      raddr : in  std_logic_vector (ADDR_WIDTH - 1 downto 0);
      waddr : in  std_logic_vector (ADDR_WIDTH - 1 downto 0);
      data  : in  std_logic_vector (DATA_WIDTH - 1 downto 0);
      wren  : in  std_logic;
      q     : out std_logic_vector (DATA_WIDTH - 1 downto 0)
      );

end entity memory_internal;

-------------------------------------------------------------------------------

architecture str of memory_internal is

  type mem_block_t is array(0 to 2**ADDR_WIDTH - 1) of
    std_logic_vector(DATA_WIDTH - 1 downto 0);

  signal memory : mem_block_t := (others => (others => '0'));

begin  -- architecture str

  process(clock)
  begin
    if rising_edge(clock) then
      q <= memory(to_integer(unsigned(raddr)));

      if wren = '1' then
        memory(to_integer(unsigned(waddr))) <= data;
      end if;

    end if;
  end process;

end architecture str;

-------------------------------------------------------------------------------
