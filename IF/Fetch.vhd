-------------------------------------------------------------------------------
-- Title      : Instruction Fetch stage
-- Project    : 
-------------------------------------------------------------------------------
-- File       : Fetch.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-10
-- Last update: 2017-02-22
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Fetch instruction from I-Cache and forward to Decode-Issue
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-10  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cpu_defs.all;
use work.cache_defs.all;
use work.instruction_defs.all;
use work.instruction_prediction.prediction_t;

-------------------------------------------------------------------------------

entity Fetch is

  generic (
    ADDR_WIDTH : integer := 32;
    DATA_WIDTH : integer := 32;
    STEP       : natural := 4
    );

  port (
    clk       : in std_logic;
    rst       : in std_logic;
    stall_req : in std_logic;           -- stall current instruction
    kill_req  : in std_logic;           -- kill current instruction

    o_pc_instr                  : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_instruction               : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_instr_tag                 : out instr_tag_t;
    o_mispredict_kill_pipeline  : out std_logic;
    -- L2 connections
    o_l2c_req                   : out std_logic;
    o_l2c_we                    : out std_logic;
    o_l2c_addr                  : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_l2c_rdata                 : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_l2c_wdata                 : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    i_l2c_done                  : in  std_logic;
    -- Writeback feedback signals
    i_is_jump                   : in  std_logic;
    i_jump_target               : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_commited_instr_tag        : in  instr_tag_t;
    -- Debug signals
    o_dbg_if_pc                 : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_dbg_if_fetching_pc        : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_dbg_if_fetching_instr_tag : out instr_tag_t;
    o_dbg_prediction            : out prediction_t
    );

end entity Fetch;

-------------------------------------------------------------------------------

architecture rtl3 of Fetch is
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  subtype data_t is std_logic_vector(DATA_WIDTH - 1 downto 0);

  constant nop_instruction : std_logic_vector(DATA_WIDTH - 1 downto 0) := (others => '0');

  --- Control signal
  signal kill_fetch  : std_logic;       -- Fetch stage is killed, wipe out.
  signal do_stall_pc : std_logic;

  --- Signal from program counter provider
  signal pcprovider_pc                : addr_t;
  signal pcprovider_pc_instr_tag      : instr_tag_t;
  signal pcprovider_next_pc           : addr_t;
  signal pcprovider_next_pc_instr_tag : instr_tag_t;
  signal pcprovider_mispredicted      : std_logic;

  --- Signals from instruction provider
  signal iprovider_pc                : addr_t;
  signal iprovider_pc_instr_tag      : instr_tag_t;
  signal iprovider_data              : data_t;
  signal iprovider_data_valid        : std_logic;
  signal iprovider_do_step_pc        : std_logic;
  signal dbg_iprovider_fetching      : addr_t;
  signal dbg_iprovider_fetching_itag : instr_tag_t;

  --- Outgoing to next pipeline stage instruction
  signal out_pc   : addr_t;
  signal out_data : data_t;
  signal out_itag : instr_tag_t;

  --- Debug
  signal dbg_pcprovider_prediction : prediction_t;

begin
  iprovider : entity work.Instruction_Provider
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      DATA_WIDTH => DATA_WIDTH)
    port map (
      clk                      => clk,
      rst                      => rst,
      kill_req                 => kill_fetch,
      stall_req                => stall_req,
      i_next_pc                => pcprovider_pc,
      i_next_pc_instr_tag      => pcprovider_pc_instr_tag,
      i_next_next_pc           => pcprovider_next_pc,
      i_next_next_pc_instr_tag => pcprovider_next_pc_instr_tag,
      o_pc                     => iprovider_pc,
      o_instr_tag              => iprovider_pc_instr_tag,
      o_data                   => iprovider_data,
      o_valid                  => iprovider_data_valid,
      o_do_step_pc             => iprovider_do_step_pc,
      o_l2c_req                => o_l2c_req,
      o_l2c_we                 => o_l2c_we,
      o_l2c_addr               => o_l2c_addr,
      i_l2c_rdata              => i_l2c_rdata,
      o_l2c_wdata              => o_l2c_wdata,
      i_l2c_done               => i_l2c_done,
      o_dbg_fetching           => dbg_iprovider_fetching,
      o_dbg_fetching_itag      => dbg_iprovider_fetching_itag);

  pc_reg : entity work.PC_Register(rtl)
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      STEP       => STEP)
    port map (
      clk                    => clk,
      rst                    => rst,
      stall_pc               => do_stall_pc,
      jump_pc                => i_is_jump,
      jump_target            => i_jump_target,
      i_commited_instr_tag   => i_commited_instr_tag,
      o_current_pc           => pcprovider_pc,
      o_current_pc_instr_tag => pcprovider_pc_instr_tag,
      o_next_pc              => pcprovider_next_pc,
      o_next_pc_instr_tag    => pcprovider_next_pc_instr_tag,
      o_mispredicted         => pcprovider_mispredicted,
      o_dbg_prediction       => dbg_pcprovider_prediction);

  --- PC stepper
  do_stall_pc <= '1' when iprovider_do_step_pc = '0' else '0';

  --- PC jump handler
  kill_fetch <= kill_req;               --RJK or i_is_jump;

  --- When PC program mispredicted, signal to kill the pipeline
  o_mispredict_kill_pipeline <= pcprovider_mispredicted;

  --- Decode input provider
  o_instruction <= out_data;
  o_pc_instr    <= out_pc;
  o_instr_tag   <= out_itag;

  fetch_outputs_latcher : process(clk, rst, kill_fetch, stall_req)
  begin
    if rst = '1' then
      out_pc   <= (others => 'X');
      out_data <= (others => '0');
      out_itag <= INSTR_TAG_NONE;
    elsif rising_edge(clk) then
      if kill_fetch = '1' then
        out_pc   <= (others => 'X');
        out_data <= nop_instruction;
        out_itag <= INSTR_TAG_NONE;
      elsif stall_req = '1' then
      else
        if iprovider_data_valid = '1' then
          out_pc   <= iprovider_pc;
          out_data <= iprovider_data;
          out_itag <= iprovider_pc_instr_tag;
        else
          out_pc   <= (others => 'X');
          out_data <= nop_instruction;
          out_itag <= INSTR_TAG_NONE;
        end if;
      end if;
    end if;
  end process fetch_outputs_latcher;

  --- Debug signals
  o_dbg_if_pc                 <= out_pc;
  o_dbg_if_fetching_pc        <= dbg_iprovider_fetching;
  o_dbg_if_fetching_instr_tag <= dbg_iprovider_fetching_itag;
  o_dbg_prediction            <= dbg_pcprovider_prediction;

end architecture rtl3;

architecture instr_is_pc of Fetch is
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  subtype data_t is std_logic_vector(DATA_WIDTH - 1 downto 0);

  constant nop_instruction : std_logic_vector(DATA_WIDTH - 1 downto 0) := (others => '0');
begin

  fake_fetch : process(rst, clk)
    variable pc : unsigned(ADDR_WIDTH - 1 downto 0) := (others => '0');
  begin
    if rst = '1' then
      pc            := to_unsigned(0, pc'length);
      o_instruction <= std_logic_vector(pc);
      o_pc_instr    <= std_logic_vector(pc);
    elsif rising_edge(clk) then
      if kill_req = '1' then
        o_instruction     <= (others => '0');
        o_instr_tag.valid <= false;
      elsif stall_req then
      else
        if i_is_jump then
          pc := unsigned(i_jump_target);
        else
          pc := pc + 4;
        end if;
        o_instruction     <= std_logic_vector(pc);
        o_pc_instr        <= std_logic_vector(pc);
        o_instr_tag.valid <= true;
      end if;
    end if;
    o_mispredict_kill_pipeline <= i_is_jump;
  end process fake_fetch;
end architecture instr_is_pc;

architecture simple of Fetch is
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  subtype data_t is std_logic_vector(DATA_WIDTH - 1 downto 0);

  constant nop_instruction : std_logic_vector(DATA_WIDTH - 1 downto 0) := (others => '0');

  -- Instruction killing
  signal r_kill_req      : std_logic;
  signal kill_next_valid : boolean;

  -- Cycles of addresses
  --   query_pc    ->    fetching_pc             -> fetched_pc
  --   (on cache I/F)    wait for fetching_valid -> output for IF

  -- Cache address queries
  signal first_query   : boolean := true;
  signal change_query  : boolean;
  signal query_pc      : addr_t;
  signal next_query_pc : addr_t;

  -- Currently fetching
  signal fetching_pc    : addr_t;
  signal fetching_data  : data_t;
  signal fetching_valid : std_logic;

  -- Fetched instruction
  signal fetched_pc   : addr_t;
  signal fetched_data : data_t;
  signal itag         : instr_tag_t;

  -- Outgoing to next pipeline stage instruction
  signal out_pc     : addr_t;
  signal out_data   : data_t;
  signal out_itag   : instr_tag_t;
  signal data_valid : std_logic;
begin

  L1C : entity work.Instruction_Cache
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      DATA_WIDTH => DATA_WIDTH)
    port map (
      clk         => clk,
      rst         => rst,
      addr        => query_pc,
      data        => fetching_data,
      data_valid  => fetching_valid,
      o_l2c_req   => o_l2c_req,
      o_l2c_addr  => o_l2c_addr,
      i_l2c_rdata => i_l2c_rdata,
      i_l2c_done  => i_l2c_done);

  jumper : process(rst, clk, i_is_jump, fetching_valid)
  begin
    if rst = '1' then
      kill_next_valid <= false;
    elsif rising_edge(clk) and i_is_jump = '1' then
      kill_next_valid <= true;
    elsif rising_edge(clk) and fetching_valid = '1' then
      kill_next_valid <= false;
    end if;
  end process jumper;

  cache_driver : process(rst, clk, stall_req, i_is_jump, i_jump_target,
                         fetching_valid, first_query, query_pc, next_query_pc)
  begin
    if i_is_jump = '1' then
      next_query_pc <= i_jump_target;
    else
      next_query_pc <= std_logic_vector(unsigned(query_pc) + STEP);
    end if;

    change_query <= (first_query or fetching_valid = '1' or i_is_jump = '1')
                    and stall_req = '0';

    if first_query and rst = '0' and rising_edge(clk) then
      first_query <= false;
    end if;

    if rst = '1' then
      query_pc <= (others => '0');
    elsif rising_edge(clk) and change_query then
      fetching_pc <= query_pc;
      query_pc    <= next_query_pc;
    end if;
  end process cache_driver;

  cache_aquire : process(rst, clk, stall_req, kill_req, fetching_valid)
  begin
    if rst = '1' then
      itag <= INSTR_TAG_FIRST_VALID;
    elsif rising_edge(clk) then
      r_kill_req <= kill_req;

      if fetching_valid = '1' and rst = '0' and
        kill_req = '0' and r_kill_req = '0' and not kill_next_valid and
        stall_req = '0' then
        fetched_pc   <= fetching_pc;
        fetched_data <= fetching_data;
        itag         <= get_next_instr_tag(itag, 1);
        out_itag     <= get_next_instr_tag(itag, 1);
      elsif stall_req = '0' then
        fetched_pc   <= (others => 'X');
        fetched_data <= nop_instruction;
        out_itag     <= INSTR_TAG_NONE;
      end if;
    end if;
  end process cache_aquire;

  o_l2c_we <= '0';

  out_pc   <= fetched_pc;
  out_data <= fetched_data;

  o_pc_instr                 <= out_pc;
  o_instruction              <= out_data;
  o_instr_tag                <= out_itag;
  o_mispredict_kill_pipeline <= i_is_jump;

  o_dbg_if_pc                 <= fetched_pc;
  o_dbg_if_fetching_pc        <= fetching_pc;
  o_dbg_if_fetching_instr_tag <= itag;

end architecture simple;
