-------------------------------------------------------------------------------
-- Title      : Main_Memory_tb
-- Project    : 
-------------------------------------------------------------------------------
-- File       : Vhdl1.vhd
-- Author     :   <simon@simon-laptop>
-- Company    : 
-- Created    : 2016-11-17
-- Last update: 2016-12-01
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: This is the testbench of the main memory
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

entity Main_Memory_tb is

  generic (
    MEMORY_FILE     : string  := "mem.txt";
    ADDR_WIDTH      : integer := 32;
    REAL_ADDR_WIDTH : integer := 11;
    DATA_WIDTH      : integer := 32);
  
 -- port ();

end entity Main_Memory_tb;

architecture tb of Main_Memory_tb is

  component Main_memory is

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

  end component Main_memory;

  signal clk      : std_logic                                 := '0';
  signal address  : std_logic_vector(ADDR_WIDTH - 1 downto 0) := (others => '0');
  signal wr_en    : std_logic                                 := '0';
  signal wr_data  : std_logic_vector(DATA_WIDTH - 1 downto 0) := (others => '0');
  signal rd_data  : std_logic_vector(DATA_WIDTH -1 downto 0);
  signal rd_valid : std_logic;

begin

  clk <= not clk after 20ns;

  process (clk) is
  begin
    if rising_edge(clk) then
      address <= std_logic_vector(unsigned(address) + 4);
    end if;
  end process;


  uut : Main_memory
    generic map (
      MEMORY_FILE     => MEMORY_FILE,
      ADDR_WIDTH      => ADDR_WIDTH,
      REAL_ADDR_WIDTH => REAL_ADDR_WIDTH,
      DATA_WIDTH      => DATA_WIDTH)
    port map (
      clk      => clk,
      address  => address,
      wr_en    => wr_en,
      wr_data  => wr_data,
      rd_data  => rd_data,
      rd_valid => rd_valid);

end architecture tb;
