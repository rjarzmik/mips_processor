-------------------------------------------------------------------------------
-- Title      : Torture test for associative cache
-- Project    : Cache implementations
-------------------------------------------------------------------------------
-- File       : SinglePort_Associative_Cache_Torture.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-30
-- Last update: 2016-12-30
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-12-30  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

use work.cache_defs.all;

entity SinglePort_Associative_Cache_Torture_tb is
end entity SinglePort_Associative_Cache_Torture_tb;

architecture str of SinglePort_Associative_Cache_Torture_tb is

  -- component generics
  constant MEMORY_LATENCY    : integer := 3;
  constant DEBUG             : boolean := false;
  constant MEMORY_ADDR_WIDTH : natural := 16;

  type memory is array(0 to 2**(MEMORY_ADDR_WIDTH - DATA_WIDTH / 8) - 1) of data_t;

  -- component ports
  signal clk                      : std_logic                                 := '1';
  signal clkena                   : std_logic                                 := '1';
  signal rst                      : std_logic                                 := '1';
  signal i_porta_req              : std_logic                                 := '0';
  signal i_porta_we               : std_logic                                 := '0';
  signal i_porta_addr             : addr_t                                    := (others => '0');
  signal i_porta_do_write_through : std_logic;
  signal i_porta_write_data       : std_logic_vector(DATA_WIDTH - 1 downto 0) := (others => 'X');
  signal o_porta_read_data        : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal o_porta_valid            : std_logic;

  signal o_memory_req        : std_logic := '0';
  signal o_memory_we         : std_logic := '0';
  signal o_memory_addr       : addr_t;
  signal o_memory_write_data : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal i_memory_read_data  : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal i_memory_valid      : std_logic;
  signal o_dbg_state         : cache_state;
  signal o_dbg_cstats        : cache_stats_t;

  signal porta_req        : std_logic := '0';
  signal porta_we         : std_logic;
  signal porta_addr       : addr_t    := (others => '0');
  signal porta_wthrough   : std_logic;
  signal porta_write_data : data_t;

  signal cls_req   : cls_op;
  signal cls_creq  : cache_request_t;
  signal cls_cresp : cache_response_t;

  procedure rand_bit(variable seed1, seed2 : inout positive;
                     b                     : out   std_logic) is
    variable r : real;
  begin
    uniform(seed1, seed2, r);
    if r < 0.5 then
      b := '0';
    else
      b := '1';
    end if;
  end procedure rand_bit;

  procedure rand_natural(variable seed1, seed2 : inout positive;
                         max                   : in    natural;
                         n                     : out   natural) is
    variable r : real;
  begin
    uniform(seed1, seed2, r);
    n := integer(r * real(max));
  end procedure rand_natural;

  procedure report_cache_stats(stats : cache_stats_t; seed1, seed2 : positive) is
  begin
    report "Cache stats (seed1=" & integer'image(seed1) &
      ",seed2=" & integer'image(seed2) & ")";
    report "  read  : hits=" & integer'image(stats.read_hits) &
      " misses=" & integer'image(stats.read_misses);
    report "  write : hits=" & integer'image(stats.write_hits) &
      " misses=" & integer'image(stats.write_misses) & " |" &
      " wbacks=" & integer'image(stats.write_backs) &
      " wthrou=" & integer'image(stats.write_throughs);
    report "  outer : flushes=" & integer'image(stats.flushes) & " " &
      "refills=" & integer'image(stats.refills);
  end procedure report_cache_stats;

  function cache_state_name(c : cache_state) return string is
  begin
    case c is
      when s_idle             => return "idle";
      when s_searching        => return "searching";
      when s_prepare_flushing => return "prepare_flush";
      when s_flush_outer      => return "flush_outer";
      when s_flushing         => return "flushing";
      when s_refill_memory    => return "refill_memory";
      when s_refill_cache     => return "refill_cache";
      when s_writethrough     => return "writethrough";
      when s_write_allocate   => return "write_allocate";
    end case;
  end function cache_state_name;

  function test_report_header(num_test : natural; num_action : natural;
                              state    : cache_state)
    return string is
    constant testname : string := "Test";
  begin
    return "[" & testname & ":" & integer'image(num_action) & "]" &
      "[" & cache_state_name(state) & "]";
  end function test_report_header;

  function init_ram_data_offsets_addr(ofs : natural) return memory is
    variable o : memory;
    variable d : natural;
  begin
    for i in o'range loop
      d    := (i * DATA_WIDTH / 8 + ofs);  -- mod 2**memory(0)'length;
      o(i) := std_logic_vector(to_unsigned(d, DATA_WIDTH));
    end loop;
    return o;
  end function init_ram_data_offsets_addr;
  signal check_ram : memory := init_ram_data_offsets_addr(16#100#);

begin  -- architecture str

  DUT : entity work.SinglePort_Associative_Cache(rtl)
    generic map (DEBUG => DEBUG)
    port map (
      clk                      => clk,
      rst                      => rst,
      i_porta_req              => i_porta_req,
      i_porta_we               => i_porta_we,
      i_porta_addr             => i_porta_addr,
      i_porta_do_write_through => i_porta_do_write_through,
      i_porta_write_data       => i_porta_write_data,
      o_porta_read_data        => o_porta_read_data,
      o_porta_valid            => o_porta_valid,
      -- Carry-over signals
      o_creq                   => cls_creq,
      i_cresp                  => cls_cresp,
      -- Debug
      o_dbg_state              => o_dbg_state,
      o_dbg_cstats             => o_dbg_cstats);

  cls : entity work.cache_line_streamer
    generic map (
      ADDR_WIDTH           => ADDR_WIDTH,
      DATA_WIDTH           => DATA_WIDTH,
      DATAS_PER_LINE_WIDTH => DATAS_PER_LINE_WIDTH)
    port map (
      clk     => clk,
      rst     => rst,
      i_creq  => cls_creq,
      o_cresp => cls_cresp,

      o_memory_req   => o_memory_req,
      o_memory_we    => o_memory_we,
      o_memory_addr  => o_memory_addr,
      o_memory_wdata => o_memory_write_data,
      i_memory_rdata => i_memory_read_data,
      i_memory_done  => i_memory_valid);

  -- memory simulator
  Simulated_Memory_1 : entity work.Simulated_Memory
    generic map (
      ADDR_WIDTH        => ADDR_WIDTH,
      DATA_WIDTH        => DATA_WIDTH,
      MEMORY_ADDR_WIDTH => 16,
      MEMORY_LATENCY    => MEMORY_LATENCY,
      DEBUG             => DEBUG)
    port map (
      clk                 => clk,
      rst                 => rst,
      i_memory_req        => o_memory_req,
      i_memory_we         => o_memory_we,
      i_memory_addr       => o_memory_addr,
      i_memory_write_data => o_memory_write_data,
      o_memory_read_data  => i_memory_read_data,
      o_memory_valid      => i_memory_valid);

  -- reset
  rst <= '0'                  after 12 ps;
  -- clock generation
  clk <= (clkena and not clk) after 5 ps;

  torture : process
    variable expected_read : data_t;
    variable seed1         : positive := 5;
    variable seed2         : positive := 7;
    variable randb         : std_logic;
    variable randaddr      : natural range 0 to check_ram'length - 1;
    variable randdata      : natural range 0 to 2**(DATA_WIDTH - 8);
    variable num_req       : natural;

    variable pa_addr  : addr_t;
    variable pa_we    : std_ulogic;
    variable pa_wt    : std_ulogic;
    variable pa_wdata : data_t;
  begin
    if rst = '1' then
      wait until rst = '0';
      wait until clk = '1';
      num_req := 0;
    end if;

    num_req := (num_req + 1) mod (1024 * 16);
    if num_req = 0 then
      report_cache_stats(o_dbg_cstats, seed1, seed2);
    end if;

    -- Enqueue a random request
    i_porta_req                                             <= '1';
    rand_bit(seed1, seed2, pa_we); i_porta_we               <= pa_we;
    rand_bit(seed1, seed2, pa_wt); i_porta_do_write_through <= pa_wt;

    rand_natural(seed1, seed2, 2**(MEMORY_ADDR_WIDTH - DATA_WIDTH / 8 - ADDR_DATA_NBITS) - 1, randaddr);
    randaddr     := randaddr * 2**(ADDR_DATA_NBITS);
    pa_addr      := std_logic_vector(to_unsigned(randaddr, pa_addr'length));
    i_porta_addr <= pa_addr;
    if pa_we = '1' then
      rand_natural(seed1, seed2, 2**(DATA_WIDTH - 8), randdata);
      pa_wdata := std_logic_vector(to_unsigned(randdata, i_porta_write_data'length));
    else
      pa_wdata := (others => 'X');
    end if;
    i_porta_write_data <= pa_wdata;

    -- Report the request
    if DEBUG then
      if pa_we = '1' then
        if pa_wt = '1' then
          report test_report_header(0, num_req, o_dbg_state) &
            "writethrough: @" & to_hstring(pa_addr) & "<=" &
            to_hstring(pa_wdata);
        else
          report test_report_header(0, num_req, o_dbg_state) &
            "writeback: @" & to_hstring(pa_addr) & "<=" &
            to_hstring(pa_wdata);
        end if;
      else
        report test_report_header(0, num_req, o_dbg_state) &
          "read: @" & to_hstring(pa_addr);
      end if;
    end if;

    -- Remember the writes to check all subsequent reads are consistent
    if pa_we = '1' then
      check_ram(randaddr / (DATA_WIDTH / 8)) <= pa_wdata;
    end if;

    wait until clk = '1';
    wait until clk = '0';
    if o_porta_valid = '0' then
      i_porta_req <= '0';
    end if;

    -- Wait for an answer and in case of read check against check_ram
    if o_porta_valid = '0' then
      wait until clk = '0' and o_porta_valid = '1';
    end if;

    if pa_we = '0' then
      expected_read := check_ram(randaddr / (DATA_WIDTH / 8));
      if o_porta_read_data /= expected_read then
        report test_report_header(0, num_req, o_dbg_state) &
          "Read @" & to_hstring(pa_addr) & " should return " &
          to_hstring(expected_read) & " while it returned " &
          to_hstring(o_porta_read_data) severity failure;
      end if;
    end if;

  end process torture;

end architecture str;

-------------------------------------------------------------------------------
