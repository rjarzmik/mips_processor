-------------------------------------------------------------------------------
-- Title      : Cache line streaming to and from memory
-- Project    : MIPS processor implementation, compatible MIPS-1
-------------------------------------------------------------------------------
-- File       : cache_line_streamer.vhd
-- Author     : Robert Jarzmik (Intel)  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-21
-- Last update: 2017-01-04
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
--   Breaks a cache line into data pieces and either :
--    - refill a whole or partial cache line
--    - flush a whole or partial cache line
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-12-21  1.0      rjarzmik        Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cache_defs.all;

entity cache_line_streamer is
  generic (
    ADDR_WIDTH           : natural := 32;
    DATA_WIDTH           : natural := 32;
    DATAS_PER_LINE_WIDTH : natural := 4
    );

  port (
    clk : in std_logic;
    rst : in std_logic;

    i_creq  : in  cache_request_t;
    o_cresp : out cache_response_t;

    -- outer mem interface
    o_memory_req   : out std_logic;
    o_memory_we    : out std_logic;
    o_memory_addr  : out addr_t;
    i_memory_rdata : in  data_t;
    o_memory_wdata : out data_t;
    i_memory_done  : in  std_logic
    );
end entity cache_line_streamer;

architecture str of cache_line_streamer is
  -- Input signal aliases
  alias i_req                : cls_op is i_creq.req;
  alias i_addr               : addr_t is i_creq.addr;
  alias i_flushed_cache_line : cache_line_t is i_creq.cline;
  alias i_datas_select       : cache_line_selector_t is i_creq.sel;
  alias o_filled_cache_line  : cache_line_t is o_cresp.cline;
  alias o_done               : std_logic is o_cresp.done;
  signal o_busy              : std_logic;

  type state_t is (s_idle, s_refilling, s_flushing);

  constant EMPTY_DATAS_IN_LINE : cache_line_selector_t := (others => '0');

  function get_next_state(i_req : cls_op) return state_t is
  begin
    if i_req = cls_refill then
      return s_refilling;
    elsif i_req = cls_flush then
      return s_flushing;
    else
      return s_idle;
    end if;
  end function get_next_state;

  function get_memory_addr(refill_base_addr : addr_t;
                           refill_data_idx  : natural range 0 to DATAS_PER_LINE - 1)
    return addr_t is
  begin
    return std_logic_vector(unsigned(refill_base_addr) +
                            refill_data_idx * DATA_WIDTH / 8);
  end function get_memory_addr;

  function to_std_logic_vector(sel : cache_line_selector_t)
    return std_logic_vector is
    variable o : std_logic_vector(sel'length - 1 downto 0);
  begin
    for i in sel'range loop
      o(i) := sel(i);
    end loop;
    return o;
  end function to_std_logic_vector;

  signal state : state_t := s_idle;

  -- Refill signals
  signal refill_datas      : cache_line_selector_t;
  signal refill_data_idx   : natural range 0 to DATAS_PER_LINE - 1;
  signal refill_done       : boolean;
  signal refill_base_addr  : addr_t;
  signal refill_cache_line : cache_line_t;

  --- refill memory I/F
  signal refill_memory_req   : std_ulogic;
  signal refill_memory_we    : std_ulogic;
  signal refill_memory_addr  : addr_t;
  signal refill_memory_wdata : data_t;
  --- refill mask feeder
  signal mf1_sclr            : std_logic;
  signal mf1_sdata           : std_logic_vector(DATAS_PER_LINE - 1 downto 0);
  signal mf1_data            : std_logic_vector(DATAS_PER_LINE - 1 downto 0);
  signal mf1_bclrena         : std_logic;
  signal mf1_bclr            : natural range 0 to DATAS_PER_LINE - 1;
  signal mf1_fbitset         : natural range 0 to DATAS_PER_LINE - 1;
  signal mf1_allclear        : std_logic;

  -- Flush signals
  signal flush_datas        : cache_line_selector_t;
  signal flush_data_idx     : natural range 0 to DATAS_PER_LINE - 1;
  signal flush_done         : boolean;
  signal flush_base_addr    : addr_t;
  signal flush_cache_line   : cache_line_t;
  --- flush memory I/F
  signal flush_memory_req   : std_ulogic;
  signal flush_memory_we    : std_ulogic;
  signal flush_memory_addr  : addr_t;
  signal flush_memory_wdata : data_t;
  --- flush mask feeder
  signal mf2_sclr           : std_logic;
  signal mf2_sdata          : std_logic_vector(DATAS_PER_LINE - 1 downto 0);
  signal mf2_data           : std_logic_vector(DATAS_PER_LINE - 1 downto 0);
  signal mf2_bclrena        : std_logic;
  signal mf2_bclr           : natural range 0 to DATAS_PER_LINE - 1;
  signal mf2_fbitset        : natural range 0 to DATAS_PER_LINE - 1;
  signal mf2_allclear       : std_logic;


begin  -- architecture str
  o_cresp.rdy <= not o_busy;
  o_cresp.sel <= (others => 'X');

  mf1 : entity work.mask_feeder
    generic map (
      WIDTH => DATAS_PER_LINE)
    port map (
      clk      => clk,
      sclr     => mf1_sclr,
      sdata    => mf1_sdata,
      bclrena  => mf1_bclrena,
      bclr     => mf1_bclr,
      fbitset  => mf1_fbitset,
      allclear => mf1_allclear);
  mf1_sclr  <= '1'                                 when i_req = cls_refill or rst = '1' else '0';
  mf1_sdata <= to_std_logic_vector(i_datas_select) when rst = '0'
               else (others => '0');

  mf2 : entity work.mask_feeder
    generic map (
      WIDTH => DATAS_PER_LINE)
    port map (
      clk      => clk,
      sclr     => mf2_sclr,
      sdata    => mf2_sdata,
      bclrena  => mf2_bclrena,
      bclr     => mf2_bclr,
      fbitset  => mf2_fbitset,
      allclear => mf2_allclear);
  mf2_sclr  <= '1'                                 when i_req = cls_flush or rst = '1' else '0';
  mf2_sdata <= to_std_logic_vector(i_datas_select) when rst = '0'
               else (others => '0');

  controller : process(rst, clk, i_req, state, i_addr, i_flushed_cache_line,
                       refill_cache_line)
    variable ns : state_t;
  begin
    if rst = '1' then
      o_busy <= '1';
      o_done <= '0';
      state  <= s_idle;
    -- mf2_bclrena <= '0';
    elsif rising_edge(clk) then
      case state is
        when s_idle =>
          o_done <= '0';
          if i_req = cls_refill then
            state            <= s_refilling;
            refill_base_addr <= i_addr;
            o_busy           <= '1';
          elsif i_req = cls_flush then
            state            <= s_flushing;
            flush_base_addr  <= i_addr;
            flush_cache_line <= i_flushed_cache_line;
            o_busy           <= '1';
          else
            o_busy <= '0';
          end if;
        when s_refilling =>
          if refill_done then
            ns                         := get_next_state(i_req);
            state                      <= ns;
            o_done                     <= '1';
            o_filled_cache_line        <= refill_cache_line;
            if ns = s_idle then o_busy <= '0'; else o_busy <= '1'; end if;
            if ns = s_flushing then
              flush_cache_line <= i_flushed_cache_line;
            end if;
          else
            o_done <= '0';
            o_busy <= '1';
          end if;
        when s_flushing =>
          if flush_done then
            ns                         := get_next_state(i_req);
            state                      <= ns;
            o_done                     <= '1';
            if ns = s_idle then o_busy <= '0'; else o_busy <= '1'; end if;
            if ns = s_flushing then
              flush_cache_line <= i_flushed_cache_line;
            end if;
          else
            o_done <= '0';
            o_busy <= '1';
          end if;
      end case;
    end if;
  end process controller;

  refiller : process(rst, clk, i_req, i_memory_done, mf1_allclear, mf1_bclr,
                     refill_base_addr, refill_memory_req, mf1_fbitset, state,
                     i_memory_rdata)
    variable waiting_memory_rsp : std_logic := '0';
  begin
    if rst = '1'then
      refill_done       <= true;
      refill_cache_line <= (others => (others => 'X'));
    elsif rising_edge(clk) and state = s_refilling then
      if (mf1_allclear = '1' and waiting_memory_rsp = '1')
        or mf1_allclear = '0' then
        -- True work
        if waiting_memory_rsp = '1' and i_memory_done = '1' then
          -- Memory response ready
          refill_cache_line(refill_data_idx) <= i_memory_rdata;
        end if;

        if refill_memory_req = '1' then
          waiting_memory_rsp := '1';
          refill_data_idx    <= mf1_fbitset;
        elsif i_memory_done = '1' then
          waiting_memory_rsp := '0';
        end if;
      end if;

    elsif rising_edge(clk) and state /= s_refilling then
      refill_cache_line <= (others => (others => 'X'));
    end if;

    if mf1_allclear = '1' or
      (waiting_memory_rsp = '1' and i_memory_done = '0') then
      refill_memory_req <= '0';
    else
      refill_memory_req <= '1';
    end if;

    mf1_bclrena <= refill_memory_req;
    mf1_bclr    <= mf1_fbitset;

    refill_done <= mf1_allclear = '1' and i_memory_done = '1'
                   and waiting_memory_rsp = '0';

    refill_memory_we    <= '0';
    refill_memory_addr  <= get_memory_addr(refill_base_addr, mf1_fbitset);
    refill_memory_wdata <= (others => 'X');
  end process refiller;

  flusher : process(rst, clk, i_req, i_memory_done, mf2_allclear, mf2_bclr, flush_base_addr,
                    flush_memory_req, mf2_fbitset, flush_data_idx, state,
                    flush_cache_line, flush_done)
    variable waiting_memory_rsp : std_logic  := '0';
    variable ffirst_flush       : std_ulogic := '1';
    variable all_clear_r        : std_logic;
  begin
    if rst = '1' then
      flush_done   <= true;
    end if;

    if flush_done then
      flush_memory_req <= '0';
    else
      flush_memory_req <= '1';
    end if;

    mf2_bclrena <= flush_memory_req;
    mf2_bclr    <= mf2_fbitset;

    flush_data_idx <= mf2_fbitset;
    flush_done     <= mf2_allclear = '1' and all_clear_r = '1' and i_memory_done = '1';

    flush_memory_req   <= not mf2_allclear and (i_memory_done or ffirst_flush);
    flush_memory_we    <= '1';
    flush_memory_addr  <= get_memory_addr(flush_base_addr, mf2_fbitset);
    flush_memory_wdata <= flush_cache_line(flush_data_idx);

    if rst = '1' then
    elsif state = s_flushing and rising_edge(clk) then
      all_clear_r := mf2_allclear;
      if flush_done then
        ffirst_flush := '1';
      else
        ffirst_flush := '0';
      end if;
    end if;
  end process flusher;

  with state select o_memory_req <=
    '0'               when s_idle,
    refill_memory_req when s_refilling,
    flush_memory_req  when s_flushing;
  with state select o_memory_we <=
    '0'              when s_idle,
    refill_memory_we when s_refilling,
    flush_memory_we  when s_flushing;
  with state select o_memory_addr <=
    (others => 'X')    when s_idle,
    refill_memory_addr when s_refilling,
    flush_memory_addr  when s_flushing;

  with state select o_memory_wdata <=
    (others => 'X')     when s_idle,
    refill_memory_wdata when s_refilling,
    flush_memory_wdata  when s_flushing;

  --o_memory_we    <= std_logic;
  --o_memory_addr  <= std_logic_vector(ADDR_WIDTH - 1 downto 0);
  --o_memory_wdata <= std_logic_vector(DATA_WIDTH - 1 downto 0);


end architecture str;
