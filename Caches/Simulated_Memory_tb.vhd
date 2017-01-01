-------------------------------------------------------------------------------
-- Title      : Testbench for design "Simulated_Memory"
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : Simulated_Memory_tb.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-21
-- Last update: 2016-12-29
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-21  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-------------------------------------------------------------------------------

entity Simulated_Memory_tb is

end entity Simulated_Memory_tb;

-------------------------------------------------------------------------------

architecture rtl of Simulated_Memory_tb is

  -- component generics
  constant ADDR_WIDTH     : integer                                  := 32;
  constant DATA_WIDTH     : integer                                  := 32;
  constant MEMORY_LATENCY : natural                                  := 1;
  constant addr_zero      : std_logic_vector(ADDR_WIDTH -1 downto 0) := std_logic_vector(to_unsigned(0, ADDR_WIDTH));

  -- component ports
  signal clk                 : std_logic                                 := '1';
  signal rst                 : std_logic                                 := '1';
  signal i_memory_req        : std_logic                                 := '1';
  signal i_memory_we         : std_logic := '0';
  signal i_memory_addr       : std_logic_vector(ADDR_WIDTH - 1 downto 0) := addr_zero;
  signal i_memory_write_data : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal o_memory_read_data  : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal o_memory_valid      : std_logic;
  signal addr                : std_logic_vector(ADDR_WIDTH - 1 downto 0) := addr_zero;
  signal next_addr           : std_logic_vector(ADDR_WIDTH - 1 downto 0) := addr_zero;
  signal output_next_addr    : boolean;
  signal first_clk           : boolean                                   := true;

  -- clock
  signal cycle : natural := 0;

  -- test
  signal test_block_i_memory_req       : boolean := false;
  signal test_force_clear_i_memory_req : boolean := false;
  signal in_read_continuous            : boolean := false;
  signal in_read_always_memory_req_on  : boolean := false;

  signal read_continuous_incr_addr           : boolean := false;
  signal read_always_memory_req_on_incr_addr : boolean := false;
  signal read_continuous_first_clk           : boolean := true;
  signal read_always_memory_req_on_first_clk : boolean := true;

begin  -- architecture rtl

  -- component instantiation
  DUT : entity work.Simulated_Memory
    generic map (
      ADDR_WIDTH     => ADDR_WIDTH,
      DATA_WIDTH     => DATA_WIDTH,
      MEMORY_LATENCY => MEMORY_LATENCY)
    port map (
      clk                 => clk,
      rst                 => rst,
      i_memory_req        => i_memory_req,
      i_memory_we         => i_memory_we,
      i_memory_addr       => i_memory_addr,
      i_memory_write_data => i_memory_write_data,
      o_memory_read_data  => o_memory_read_data,
      o_memory_valid      => o_memory_valid);

  -- reset
  rst <= '0' after 12 ps;

  -- clock generation
  clk <= not clk after 5 ps;

  -- waveform generation
  clk_cycles : process(clk)
  begin
    if rst = '0' and rising_edge(clk) then
      cycle <= cycle + 1;
      if cycle > 1 then
        first_clk <= false;
      end if;
    end if;
  end process clk_cycles;

  process_incr_addr : process(clk, read_continuous_incr_addr,
                              read_always_memory_req_on_incr_addr)
  begin
    if rst = '0' and rising_edge(clk) then
      if in_read_continuous or in_read_always_memory_req_on then
        if read_continuous_incr_addr or
          read_always_memory_req_on_incr_addr then
          addr <= std_logic_vector(to_unsigned(
            (to_integer(unsigned(addr)) + 4) mod 16, ADDR_WIDTH));
        end if;
      else
        addr <= (others => '0');
      end if;
    end if;
  end process process_incr_addr;

  read_continuous : process(clk, o_memory_valid)
  begin
    if rst = '0' and rising_edge(clk)
      and cycle > 0 and cycle   <= (MEMORY_LATENCY + 1) * 7 and rising_edge(clk) then
      in_read_continuous        <= true;
      read_continuous_first_clk <= false;
      if MEMORY_LATENCY > 0 then
        if o_memory_valid = '1' then
          read_continuous_incr_addr <= true;
        else
          read_continuous_incr_addr <= false;
        end if;
      else
        read_continuous_incr_addr <= true;
      end if;
    elsif rst = '0' and rising_edge(clk) then
      in_read_continuous        <= false;
      read_continuous_incr_addr <= false;
    end if;
  end process read_continuous;

  read_always_memory_req_on : process(clk, o_memory_valid)
  begin
    if rst = '0' and rising_edge(clk)
      and cycle > (MEMORY_LATENCY + 1) * 10 and cycle <= (MEMORY_LATENCY + 1) * 20 then
      in_read_always_memory_req_on                    <= true;
      test_block_i_memory_req                         <= true;
      read_always_memory_req_on_first_clk             <= false;
    elsif rst = '0' and rising_edge(clk) then
      in_read_always_memory_req_on <= false;
    end if;
  end process read_always_memory_req_on;

  read_always_memory_req_on_incr_addr <= in_read_always_memory_req_on and
                                         (MEMORY_LATENCY = 0 or o_memory_valid = '1');

  output_next_addr <= (MEMORY_LATENCY = 0 or o_memory_valid = '1');
  i_memory_addr    <= next_addr when output_next_addr else addr;
  next_addr <= std_logic_vector(to_unsigned(
    (to_integer(unsigned(addr)) + 4) mod 16, ADDR_WIDTH));
  i_memory_req <= '1' when (in_read_continuous or in_read_always_memory_req_on) and
                  (MEMORY_LATENCY = 0 or test_block_i_memory_req or (output_next_addr or first_clk))
                  else '0';

end architecture rtl;

-------------------------------------------------------------------------------

configuration Simulated_Memory_tb_rtl_cfg of Simulated_Memory_tb is
  for rtl
  end for;
end Simulated_Memory_tb_rtl_cfg;

-------------------------------------------------------------------------------
