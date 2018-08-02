-------------------------------------------------------------------------------
-- Title      : Testbench for design "jump_prediction"
-- Project    : MIPS Processor
-------------------------------------------------------------------------------
-- File       : jump_prediction_tb.vhd<ProgramCounter>
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2018-08-02
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
-- 2018-08-02  1.0      rjarzmik        Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity jump_prediction_tb is
end entity jump_prediction_tb;

architecture test of jump_prediction_tb is

  -- component generics
  constant ADDR_WIDTH       : natural  := 32;
  constant NB_WAYS          : positive := 2;
  constant CACHE_SIZE_BYTES : positive := 32;
  constant NB_PREDICT_BITS  : natural  := 2;
  constant DEBUG            : boolean  := false;

  -- component ports
  signal clk                : std_logic := '1';
  signal stall              : std_logic := '0';
  signal query_addr         : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal reply_take_branch  : std_logic;
  signal reply_ctxt_way     : std_logic_vector(0 to NB_WAYS - 1);
  signal reply_ctxt_predict : std_logic_vector(NB_PREDICT_BITS - 1 downto 0);
  signal update             : std_logic;
  signal wsrc_addr          : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal wsrc_ctxt_way      : std_logic_vector(0 to NB_WAYS - 1);
  signal wsrc_ctxt_predict  : std_logic_vector(NB_PREDICT_BITS - 1 downto 0);
  signal wsrc_taken         : std_logic;

  -- clock
  signal clkena : std_logic := '1';

  function to_addr(addr : in natural) return std_logic_vector is
  begin
    return std_logic_vector(to_unsigned(addr, ADDR_WIDTH));
  end;

  procedure report_query(do_print : boolean) is
  begin
    if do_print then
      report "Query=@" & to_hstring(query_addr) &
        ", Update={" & boolean'image(update = '1') & ",@" & to_hstring(wsrc_addr) &
        ",taken=" & boolean'image(wsrc_taken = '1') &
        ",way=" & to_bstring(wsrc_ctxt_way) &
        ",predict=" & to_bstring(wsrc_ctxt_predict) & "}";
    end if;
  end procedure report_query;

  procedure report_response(do_print : boolean) is
  begin
    if do_print then
      report "Reply=" &
        "{take=" & boolean'image(reply_take_branch = '1') &
        ",way=" & to_bstring(reply_ctxt_way) &
        ",predict=" & to_bstring(reply_ctxt_predict) & "}";
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
  DUT : entity work.jump_prediction
    generic map (
      ADDR_WIDTH       => ADDR_WIDTH,
      NB_WAYS          => NB_WAYS,
      CACHE_SIZE_BYTES => CACHE_SIZE_BYTES,
      NB_PREDICT_BITS  => NB_PREDICT_BITS,
      DEBUG            => DEBUG)
    port map (
      clk                => clk,
      stall              => stall,
      query_addr         => query_addr,
      reply_take_branch  => reply_take_branch,
      reply_ctxt_way     => reply_ctxt_way,
      reply_ctxt_predict => reply_ctxt_predict,
      update             => update,
      wsrc_addr          => wsrc_addr,
      wsrc_ctxt_way      => wsrc_ctxt_way,
      wsrc_ctxt_predict  => wsrc_ctxt_predict,
      wsrc_taken         => wsrc_taken);

  -- clock generation
  clk <= (clkena and not clk) after 5 ps;

  -- waveform generation
  WaveGen_Proc : process
  begin
    update <= '0';

    wait until clk = '0';
    query_addr <= to_addr(16#0010#);
    do_cycle(DEBUG);

    -- test that unknown value doesn't yield a match
    do_cycle(DEBUG);
    assert reply_take_branch = '0';

    -- @25ps-35ps: create an entry for 16#0010# address
    query_addr        <= to_addr(16#0010#);
    update            <= '1';
    wsrc_addr         <= to_addr(16#0010#);
    wsrc_ctxt_way     <= ('0', '0');
    wsrc_ctxt_predict <= ('0', '1');
    wsrc_taken        <= '1';
    do_cycle(DEBUG);

    -- @35ps: verify; the btb memories are now udpated
    assert reply_take_branch = '0';     -- previous value, 1 cycle in the past

    -- @40ps: don't update anything, change query_addr
    update     <= '0';
    query_addr <= to_addr(16#0010#);
    do_cycle(DEBUG);

    -- @45ps: verify the entry is updated
    assert reply_take_branch = '1';
    assert reply_take_branch = '1';
    assert reply_ctxt_predict = ('1', '0');
    do_cycle(DEBUG);

    -- @55ps-65ps: create a second entry with same tag
    query_addr        <= to_addr(16#0010#);
    update            <= '1';
    wsrc_addr         <= to_addr(16#0020#);
    wsrc_ctxt_way     <= ('0', '0');
    wsrc_ctxt_predict <= ('0', '1');
    wsrc_taken        <= '0';
    do_cycle(DEBUG);

    -- @65ps
    assert reply_take_branch = '1';     -- previous value for 16#0010# address
    assert reply_ctxt_predict = ('1', '0');

    -- @65ps-75ps
    query_addr <= to_addr(16#0020#);
    update     <= '0';
    do_cycle(DEBUG);

    -- @75ps: verify the entry is updated
    assert reply_take_branch = '1';
    assert reply_ctxt_predict = ('1', '0');

    -- @75ps-85ps: create a third entry with same tag
    query_addr        <= to_addr(16#0010#);
    update            <= '1';
    wsrc_addr         <= to_addr(16#0030#);
    wsrc_ctxt_way     <= ('0', '0');
    wsrc_ctxt_predict <= ('0', '1');
    wsrc_taken        <= '1';
    do_cycle(DEBUG);

    -- @85ps: this is the last cycle where 16#0010# hasn't been evicted
    assert reply_take_branch = '1';
    assert reply_ctxt_predict = ('1', '0');

    -- @85ps-95ps: verify the entry is now evicted
    update <= '0';
    do_cycle(DEBUG);

    -- @95ps: verify the entry is now evicted
    assert reply_take_branch = '0';
    query_addr        <= to_addr(16#0020#);
    update            <= '1';
    wsrc_addr         <= to_addr(16#0020#);
    wsrc_ctxt_way     <= ('0', '0');
    wsrc_ctxt_predict <= ('0', '1');
    wsrc_taken        <= '1';
    do_cycle(DEBUG);

    -- @105ps: update the second entry

    -- end simulation
    clkena <= '0';
    report "Simulation ended successfully";
  end process WaveGen_Proc;

end architecture test;

configuration jump_prediction_tb_test_cfg of jump_prediction_tb is
  for test
  end for;
end jump_prediction_tb_test_cfg;
