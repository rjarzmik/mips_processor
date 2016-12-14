-------------------------------------------------------------------------------
-- Title      : Main Memory
-- Project    : 
-------------------------------------------------------------------------------
-- File       : Main_memory.vhd
-- Author     :   <simon@simon-laptop>
-- Company    : 
-- Created    : 2016-11-17
-- Last update: 2016-12-01
-- Platform   : 
-- Standard   : VHDL'2008
-------------------------------------------------------------------------------
-- Description: Description of the central RAM. Its interface is 32 bits, but
-- only the first 4K is mapped. The begining of this memory is initialized with
-- the actual program. The addressing mode is byte but is converted in 32 bits
-- aligned. The initialization program is currently written in ASCII hex, ie
-- e0ffbd27
-- 1c00bfaf
-- 1800beaf
-- 1400b0af
-- 25f0a003
--- TODO:
--      - stop using absolute path.
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-17  1.0      simon   Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.textio.all;
-------------------------------------------------------------------------------

entity Main_memory is

  generic (
    MEMORY_FILE     : string  := "mem.txt";
    ADDR_WIDTH      : integer := 32;
    REAL_ADDR_WIDTH : integer := 11;
    DATA_WIDTH      : integer := 32
    );

  port (
    clk      : in  std_logic;
    address  : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    wr_en    : in  std_logic;
    wr_data  : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    rd_data  : out std_logic_vector(DATA_WIDTH -1 downto 0);
    rd_valid : out std_logic
    );

end entity Main_memory;

-------------------------------------------------------------------------------

architecture rtl of Main_memory is

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------

  type memory is array(0 to 2**REAL_ADDR_WIDTH - 1) of std_logic_vector(DATA_WIDTH - 1 downto 0);


  function init_ram(FileName : string)
    return memory is

    variable tmp         : memory := (others => (others => '0'));
    file FileHandle      : text open read_mode is FileName;
    variable CurrentLine : line;
    variable TempWord    : bit_vector(DATA_WIDTH - 1 downto 0);

  begin
    for addr_pos in 0 to 2**REAL_ADDR_WIDTH - 1 loop
      -- Initialize each address with the address itsel
      exit when endfile(FileHandle);

      readline(FileHandle, CurrentLine);
      hread(CurrentLine, TempWord);

      tmp(addr_pos) := To_StdLogicVector(TempWord);
    end loop;
    return tmp;
  end init_ram;

  signal ram : memory := init_ram(MEMORY_FILE);
begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------

  process(clk)
  begin
    if rising_edge(clk) then
      rd_data  <= ram(to_integer(unsigned(address(REAL_ADDR_WIDTH - 1 downto 0))) / 4);
      rd_valid <= '1';
      if wr_en = '1' then
        ram(to_integer(unsigned(address(REAL_ADDR_WIDTH - 1 downto 0))) / 4) <= wr_data;

        rd_valid <= '0';
      end if;
    end if;

  end process;

end architecture rtl;
