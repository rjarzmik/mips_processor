-------------------------------------------------------------------------------
-- Title      : Testbench for design "branch_target_buffer"
-- Project    : MIPS Processor
-------------------------------------------------------------------------------
-- File       : branch_target_buffer_tb.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2018-07-31
-- Last update: 2018-08-04
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2018 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2018-07-31  1.0      rjarzmik	Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library rjarzmik;
use rjarzmik.slv_utils.and_reduce;

entity branch_target_buffer_tb is
end entity branch_target_buffer_tb;

architecture test of branch_target_buffer_tb is

  -- component generics
  constant ADDR_WIDTH       : natural := 32;
  constant NB_WAYS          : positive := 2;
  constant CACHE_SIZE_BYTES : positive := 32;
  constant STEP             : natural := 4;
  constant DEBUG            : boolean := false;

  -- component ports
  signal clk          : std_logic := '1';
  signal stall        : std_logic := '0';
  signal query_addr   : std_logic_vector(ADDR_WIDTH - 1 downto 0) := (others => '0');
  signal reply_addr   : std_logic_vector(ADDR_WIDTH - 1 downto 0)  := (others => '0');
  signal reply_wfound : std_logic := '0';
  signal update       : std_logic;
  signal wsrc_addr    : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal wtgt_addr    : std_logic_vector(ADDR_WIDTH - 1 downto 0);

  -- clock
  signal clkena           : std_logic := '1';

  function to_addr(addr : in natural) return std_logic_vector is
  begin
    return std_logic_vector(to_unsigned(addr, ADDR_WIDTH));
  end;

  procedure report_query(do_print : boolean) is
  begin
    if do_print then
      report "Query=@" & to_hstring(query_addr) &
        ", Update={" & boolean'image(update = '1') & ",@from=" &
        to_hstring(wsrc_addr) & ",@to=" & to_hstring(wtgt_addr) & "}";
    end if;
  end procedure report_query;

  procedure report_response(do_print : boolean) is
  begin
    if do_print then
      report "Reply=" &
        "{found=" & boolean'image(reply_wfound = '1') &
        ",@=" & to_hstring(reply_addr) & "}";
    end if;
  end procedure report_response;

  procedure do_cycle(do_print : boolean) is
  begin
    wait for 1 ps;
    report_query(do_print);
    wait until clk = '1';
    wait for 1 ps;
    report_response(do_print);
    wait until clk = '0';
    wait for 3 ps;
  end procedure do_cycle;

begin  -- architecture test

  -- component instantiation
  DUT: entity work.branch_target_buffer
    generic map (
      ADDR_WIDTH       => ADDR_WIDTH,
      NB_WAYS          => NB_WAYS,
      CACHE_SIZE_BYTES => CACHE_SIZE_BYTES,
      STEP             => STEP,
      DEBUG            => DEBUG)
    port map (
      clk          => clk,
      stall        => stall,
      query_addr   => query_addr,
      reply_addr   => reply_addr,
      reply_wfound => reply_wfound,
      update       => update,
      wsrc_addr    => wsrc_addr,
      wtgt_addr    => wtgt_addr);

  -- clock generation
  clk <= (clkena and not clk) after 5 ps;

  -- waveform generation
  WaveGen_Proc: process
  begin
    update <= '0';

    wait until clk = '0';
    query_addr <= to_addr(16#0010#);
    do_cycle(DEBUG);

    -- test that unknown value doesn't yield a match
    do_cycle(DEBUG);
    assert reply_wfound = '0';

    -- @25ps-35ps: create an entry for 16#0010# address
    query_addr <= to_addr(16#0010#);
    update <= '1';
    wsrc_addr <= to_addr(16#0010#);
    wtgt_addr <= to_addr(16#001c#);
    do_cycle(DEBUG);

    -- @35ps: verify; the btb memories are now udpated
    assert reply_wfound = '0';             -- previous value, 1 cycle in the past
    assert reply_addr = to_addr(16#0000#); -- previous value, 1 cycle in the past

    -- @40ps: don't update anything, change query_addr
    update <= '0';
    query_addr <= to_addr(16#0010#);
    do_cycle(DEBUG);

    -- @45ps: verify the entry is updated
    assert reply_wfound = '1';
    assert reply_addr = to_addr(16#001c#); -- previous value, 1 cycle in the past
    do_cycle(DEBUG);

    -- @55ps-65ps: create a second entry with same tag
    query_addr <= to_addr(16#0010#);
    update <= '1';
    wsrc_addr <= to_addr(16#0020#);
    wtgt_addr <= to_addr(16#002c#);
    do_cycle(DEBUG);

    -- @65ps
    assert reply_wfound = '1';
    assert reply_addr = to_addr(16#001c#); -- previous value or 16#001c# address

    -- @65ps-75ps
    query_addr <= to_addr(16#0020#);
    update <= '0';
    do_cycle(DEBUG);

    -- @75ps: verify the entry is updated
    assert reply_wfound = '1';
    assert reply_addr = to_addr(16#002c#); -- previous value, 1 cycle in the past

    -- @75ps-85ps: create a third entry with same tag
    query_addr <= to_addr(16#0010#);
    update <= '1';
    wsrc_addr <= to_addr(16#0030#);
    wtgt_addr <= to_addr(16#003c#);
    do_cycle(DEBUG);

    -- @85ps: this is the last cycle where 16#0010# hasn't been evicted
    assert reply_wfound = '1';
    assert reply_addr = to_addr(16#001c#); -- previous value or 16#001c# address

    -- @85ps-95ps: verify the entry is now evicted
    update <= '0';
    do_cycle(DEBUG);

    -- @95ps: verify the entry is now evicted
    assert reply_wfound = '0';

    -- end simulation
    clkena <= '0';
    report "Simulation ended successfully";
  end process WaveGen_Proc;

end architecture test;

configuration branch_target_buffer_tb_test_cfg of branch_target_buffer_tb is
  for test
  end for;
end branch_target_buffer_tb_test_cfg;
