-------------------------------------------------------------------------------
-- Title      : Testbench for design "cache_line_streamer"
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : cache_line_streamer_tb.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-22
-- Last update: 2016-12-26
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-12-22  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cache_defs.all;

-------------------------------------------------------------------------------

entity cache_line_streamer_tb is

end entity cache_line_streamer_tb;

-------------------------------------------------------------------------------

architecture test of cache_line_streamer_tb is

  -- component generics
  constant ADDR_WIDTH           : natural := 32;
  constant DATA_WIDTH           : natural := 32;
  constant DATAS_PER_LINE_WIDTH : natural := 3;
  constant MEMORY_LATENCY       : natural := 3;

  -- component ports
  signal clk   : std_logic := '1';
  signal rst   : std_logic := '1';
  signal i_req : cls_op;

  signal creq  : cache_request_t;
  signal cresp : cache_response_t;

  signal o_memory_req   : std_logic;
  signal o_memory_we    : std_logic;
  signal o_memory_addr  : addr_t;
  signal i_memory_rdata : data_t;
  signal o_memory_wdata : data_t;
  signal i_memory_done  : std_logic;

  -- clock
  signal clkena           : std_logic := '1';
  signal test_refill_done : boolean   := false;

  function get_ireq_str(req : cls_op) return string is
  begin
    case req is
      when cls_none   => return "none";
      when cls_refill => return "refill";
      when cls_flush  => return "flush";
    end case;
  end function get_ireq_str;

  function cline_to_hstring(cline : cache_line_t;
                            i     : natural) return string is
  begin
    if i = 0 then
      return to_hstring(cline(i));
    else
      return to_hstring(cline(i)) & ":" & cline_to_hstring(cline, i - 1);
    end if;
  end function cline_to_hstring;

begin  -- architecture test

  -- component instantiation
  DUT : entity work.cache_line_streamer
    generic map (
      ADDR_WIDTH           => ADDR_WIDTH,
      DATA_WIDTH           => DATA_WIDTH,
      DATAS_PER_LINE_WIDTH => DATAS_PER_LINE_WIDTH)
    port map (
      clk     => clk,
      rst     => rst,
      i_creq  => creq,
      o_cresp => cresp,

      o_memory_req   => o_memory_req,
      o_memory_we    => o_memory_we,
      o_memory_addr  => o_memory_addr,
      i_memory_rdata => i_memory_rdata,
      o_memory_wdata => o_memory_wdata,
      i_memory_done  => i_memory_done);

  -- memory simulator
  Simulated_Memory_1 : entity work.Simulated_Memory
    generic map (
      ADDR_WIDTH        => ADDR_WIDTH,
      DATA_WIDTH        => DATA_WIDTH,
      MEMORY_ADDR_WIDTH => 16,
      MEMORY_LATENCY    => MEMORY_LATENCY)
    port map (
      clk                 => clk,
      rst                 => rst,
      i_memory_req        => o_memory_req,
      i_memory_we         => o_memory_we,
      i_memory_addr       => o_memory_addr,
      i_memory_write_data => o_memory_wdata,
      o_memory_read_data  => i_memory_rdata,
      o_memory_valid      => i_memory_done);

  -- clock generation
  rst    <= '0'                  after 12 ps;
  clk    <= (clkena and not clk) after 5 ps;
  clkena <= '0'                  after 340 ps;

  creq.req <= cls_refill after 28 ps,
           cls_none  after 35 ps,
           cls_flush after 198 ps,
           cls_none  after 207 ps;

  creq.addr <= std_logic_vector(to_unsigned(16#0060#, ADDR_WIDTH)) after 21 ps;
  creq.sel  <= (6 => '1', 4 => '1', 2 => '1', 0 => '1', others => '0');

  creq.cline(6) <= x"66666666";
  creq.cline(4) <= x"44444444";
  creq.cline(2) <= x"22222222";
  creq.cline(0) <= x"00000000";

  -- waveform generation
  WaveGen_Proc : process
  begin
    wait until Clk = '1';
    if not test_refill_done then
      report "i_req=" & get_ireq_str(creq.req) &
        ", cresp.rdy=" & std_logic'image(cresp.rdy) &
        ", cresp.done=" & std_logic'image(cresp.done) &
        ", cresp.cline = " & cline_to_hstring(cresp.cline, cresp.cline'length - 1);
    else
      report "i_req=" & get_ireq_str(creq.req) &
        ", cresp.rdy=" & std_logic'image(cresp.rdy) &
        ", cresp.done=" & std_logic'image(cresp.done) &
        ", o_memory_req=" & std_logic'image(o_memory_req) &
        ", o_memory_addr=" & to_hstring(o_memory_addr) &
        ", o_memory_wdata=" & to_hstring(o_memory_wdata) &
        ", i_memory_done=" & std_logic'image(i_memory_done);
    end if;
  end process WaveGen_Proc;

  refill_watch : process
  begin
    wait for 1 ps;
    -- wait until NOW = 1 ps;
    assert cresp.rdy = '0' report "During reset ready must be 0";

    wait until rst = '0';
    wait until clk = '1';
    wait until clk = '1';
    assert cresp.rdy = '1' report "after reset ready must be 1";

    wait until clk = '1';
    assert cresp.rdy = '0' report "after request ready must be 0";
    assert cresp.done = '0' report "after request done must be 0";

    wait for (MEMORY_LATENCY * 4 + 2) * 10 ps;
    assert cresp.rdy = '1' report "after refilling, ready must be 1";
    assert cresp.done = '1' report "after refilling, done must be 1";
    -- Expected data: 0x160 + idx * 4
    assert cresp.cline(6) = x"00000178"
      report "Data at index 7 not refilled correctly";
    assert cresp.cline(4) = x"00000170"
      report "Data at index 4 not refilled correctly";
    assert cresp.cline(2) = x"00000168"
      report "Data at index 2 not refilled correctly";
    assert cresp.cline(0) = x"00000160"
      report "Data at index 0 not refilled correctly";

    wait until clk = '1';
    assert cresp.rdy = '1' report "while idling, ready must be 1";
    wait until clk = '1';
    assert cresp.rdy = '1' report "while idling, ready must be 1";

    test_refill_done <= true;
    wait on rst;

  end process refill_watch;

  flush_watch : process
  begin
    wait until test_refill_done = true;
    wait until clk = '1';

    -- wait until NOW = 1 ps;
    assert o_memory_req = '1' report "memory request should be asserted";
    assert o_memory_addr = x"00000078" report "memory request target address incorrect";
    assert o_memory_wdata = x"66666666" report "memory request wrong data sent";
    assert cresp.rdy = '0' report "after request ready must be 1";
    assert cresp.done = '0' report "after request done must be 0";

    wait for MEMORY_LATENCY * 10 ps;
    assert cresp.rdy = '0' report "after request ready must be 1";
    assert cresp.done = '0' report "after request done must be 0";
    assert o_memory_req = '1' report "memory request should be asserted";
    assert o_memory_addr = x"00000070" report "memory request target address incorrect";
    assert o_memory_wdata = x"44444444" report "memory request wrong data sent";

    wait for MEMORY_LATENCY * 10 ps;
    assert cresp.rdy = '0' report "after request ready must be 1";
    assert cresp.done = '0' report "after request done must be 0";
    assert o_memory_req = '1' report "memory request should be asserted";
    assert o_memory_addr = x"00000068" report "memory request target address incorrect";
    assert o_memory_wdata = x"22222222" report "memory request wrong data sent";

    wait for MEMORY_LATENCY * 10 ps;
    assert cresp.rdy = '0' report "after request ready must be 1";
    assert cresp.done = '0' report "after request done must be 0";
    assert o_memory_req = '1' report "memory request should be asserted";
    assert o_memory_addr = x"00000060" report "memory request target address incorrect";
    assert o_memory_wdata = x"00000000" report "memory request wrong data sent";

    wait for MEMORY_LATENCY * 10 ps;
    wait until clk = '1';
    wait until clk = '1';
    assert cresp.done = '1' report "last request has not completed";
    assert cresp.rdy = '1' report "after flushing ready must be 1";
    assert cresp.done = '1' report "after flushing, done must be 1";

    wait until clk = '1';
    wait on rst;
  end process flush_watch;

end architecture test;

-------------------------------------------------------------------------------

configuration cache_line_streamer_tb_test_cfg of cache_line_streamer_tb is
  for test
  end for;
end cache_line_streamer_tb_test_cfg;

-------------------------------------------------------------------------------
