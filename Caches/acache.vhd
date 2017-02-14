-------------------------------------------------------------------------------
-- Title      : Single input port associative cache
-- Project    : Cache
-------------------------------------------------------------------------------
-- File       : acache.vhd
-- Author     : Robert Jarzmik <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2016-11-30
-- Last update: 2017-02-14
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Cache suited for an L1 cache
-------------------------------------------------------------------------------
-- Copyright (c) 2016
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2016-11-30  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

use work.cache_defs.all;
use work.cache_sizing.all;

entity acache is
  generic (
    ADDR_WIDTH       : positive;
    DATA_WIDTH       : positive;
    DATAS_PER_LINE   : positive;
    NB_WAYS          : positive;
    CACHE_SIZE_BYTES : positive;
    LOWER_DATA_WIDTH : positive;
    WRITE_BACK       : boolean;
    STATISTICS       : boolean := false;
    DEBUG            : boolean := false
    );

  port (
    clk : in std_logic;
    rst : in std_logic;

    i_req              : in  std_logic;
    i_wen              : in  std_logic;
    i_addr             : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_wdata            : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    i_do_write_through : in  std_logic;
    o_rdata            : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_rdata_valid      : out std_logic;
    o_wready           : out std_logic;

    -- outer mem interface
    o_memory_req   : out std_logic;
    o_memory_we    : out std_logic;
    o_memory_addr  : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_memory_wdata : out std_logic_vector(LOWER_DATA_WIDTH - 1 downto 0);
    i_memory_rdata : in  std_logic_vector(LOWER_DATA_WIDTH - 1 downto 0);
    i_memory_done  : in  std_logic;

    o_dbg_cstats : out cache_stats_t
    );
end entity acache;

architecture rtl of acache is
  constant cs : csizes := to_cache_sizing(ADDR_WIDTH, DATA_WIDTH,
                                          DATAS_PER_LINE, NB_WAYS,
                                          CACHE_SIZE_BYTES);
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  subtype data_t is std_logic_vector(DATA_WIDTH - 1 downto 0);
  subtype mdata_t is std_logic_vector(LOWER_DATA_WIDTH - 1 downto 0);
  subtype way_selector_t is std_logic_vector(0 to cs.nb_ways - 1);

  -- Tags data
  constant TAG_SLV_EMPTY_CTXT : std_logic_vector(0 downto 0) := (others => '0');
  subtype tag_slv_t is std_logic_vector(cs.tag_slv_width - 1 downto 0);

  type tdata_out_t is record
    found     : boolean;
    way       : natural range 0 to NB_WAYS - 1;
    rway      : way_selector_t;
    rtag      : tag_slv_t;
    way_evict : natural range 0 to NB_WAYS - 1;
    etag      : tag_slv_t;
  end record;

  signal tdata_raddr : addr_t         := (others => '0');
  signal tdata_re    : std_logic      := '0';
  signal tdata_waddr : addr_t         := (others => '0');
  signal tdata_wway  : way_selector_t := (others => '0');
  signal tdata_wtag  : tag_slv_t;
  signal tdata_evict : std_logic      := '0';
  signal tdatao      : tdata_out_t;

  -- Cache data memory
  signal cmem_raddr      : addr_t         := (others => '0');
  signal cmem_wide       : std_logic;
  signal cmem_re         : std_logic      := '0';
  signal cmem_we         : std_logic;
  signal cmem_rway       : natural range 0 to NB_WAYS - 1;
  signal cmem_rdata      : data_t;
  signal cmem_rdata_wide : mdata_t;
  signal cmem_waddr      : addr_t         := (others => '0');
  signal cmem_wway       : way_selector_t := (others => '0');
  signal cmem_wdata      : data_t;
  signal cmem_wdata_wide : mdata_t;
  --- Flush/refill additionnal signals
  signal cmem_rf_we      : std_logic;
  signal cmem_rf_waddr   : addr_t         := (others => '0');
  signal cmem_rf_raddr   : addr_t;
  --- WriteAllocated additionnal signals
  signal cmem_wa_we      : std_logic;
  signal cmem_wa_waddr   : addr_t         := (others => '0');
  signal cmem_wa_wdata   : data_t;
  --- Writethrough signals
  signal cmem_wt_raddr   : addr_t;
  signal cmem_wt_rway    : natural range 0 to NB_WAYS - 1;

  -- Lower memory interface
  --- Refill/Flush
  signal memory_rf_req   : std_logic;
  signal memory_rf_we    : std_logic;
  signal memory_rf_addr  : addr_t;
  signal memory_rf_rdata : mdata_t;
  signal memory_rf_wdata : mdata_t;
  signal memory_rf_done  : std_logic;
  --- Lower memory writethrough
  signal memory_wt_req   : std_logic;
  signal memory_wt_we    : std_logic;
  signal memory_wt_addr  : addr_t;
  signal memory_wt_wdata : mdata_t;
  signal memory_wt_done  : std_logic;

  -- Searcher
  type search_t is record
    ren       : std_ulogic;
    wen       : std_ulogic;
    addr      : addr_t;
    wdata     : data_t;
    wthrough  : std_ulogic;
    found     : boolean;
    way       : natural range 0 to NB_WAYS - 1;
    rtag      : tag_slv_t;
    way_evict : natural range 0 to NB_WAYS - 1;
    etag      : tag_slv_t;
  end record search_t;
  signal search, missed_search : search_t;
  signal cache_hit             : boolean;
  signal delayed_ren           : std_ulogic;
  signal delayed_wen           : std_ulogic;
  signal delayed_wthrough      : std_ulogic;

  -- Reader and Writer
  signal w_ready : std_ulogic;
  type misser_state_t is (m_idle, m_prep_flush_refill, m_flush_refill,
                          m_wmiss_cdata, m_wthrough_prep, m_whit_through_prep,
                          m_wthrough, m_finish_wthrough);
  signal misser_state : misser_state_t;

  -- Flusher
  type flusher_control_t is record
    addr : addr_t;
    way  : natural range 0 to NB_WAYS - 1;
    ena  : std_ulogic;
    done : std_ulogic;
  end record;
  signal flusher_ctrl      : flusher_control_t;
  signal flush_memory_addr : addr_t;

  -- Refiller
  type refiller_control_t is record
    addr : addr_t;
    way  : natural range 0 to NB_WAYS - 1;
    ena  : std_ulogic;
    done : std_ulogic;
  end record;
  signal refill_ctrl        : refiller_control_t;
  signal refill_memory_addr : addr_t;
  signal refill_wway_mask   : way_selector_t;

  -- Flusher and refiller control
  signal rf_ena  : std_ulogic;
  signal rf_done : std_ulogic;

  -- Statistics
  signal cstats : cache_stats_t;
begin
  gtdm : entity work.tags_data_mem
    generic map (ADDR_WIDTH       => ADDR_WIDTH,
                 DATA_WIDTH       => DATA_WIDTH,
                 DATAS_PER_LINE   => DATAS_PER_LINE,
                 NB_WAYS          => NB_WAYS,
                 CACHE_SIZE_BYTES => CACHE_SIZE_BYTES,
                 DEBUG            => DEBUG)
    port map (
      clk             => clk,
      i_raddr         => tdata_raddr,
      i_re            => tdata_re,
      o_found         => tdatao.found,
      o_way_found     => tdatao.way,
      o_tag_slv       => tdatao.rtag,
      o_way_evict     => tdatao.way_evict,
      o_evict_tag_slv => tdatao.etag,
      i_waddr         => tdata_waddr,
      i_wway          => tdata_wway,
      i_wtag_slv      => tdata_wtag,
      i_evict_compute => tdata_evict);

  gcdmem : entity work.cache_data_mem
    generic map (ADDR_WIDTH       => ADDR_WIDTH,
                 DATA_WIDTH       => DATA_WIDTH,
                 DATAS_PER_LINE   => DATAS_PER_LINE,
                 NB_WAYS          => NB_WAYS,
                 CACHE_SIZE_BYTES => CACHE_SIZE_BYTES,
                 WIDTH_WIDENER    => integer(log2(real(LOWER_DATA_WIDTH / DATA_WIDTH))),
                 DEBUG            => DEBUG)
    port map (
      clk          => clk,
      wide         => cmem_wide,
      i_raddr      => cmem_raddr,
      i_re         => cmem_re,
      i_rway       => cmem_rway,
      o_rdata      => cmem_rdata,
      o_rdata_wide => cmem_rdata_wide,
      i_waddr      => cmem_waddr,
      i_wway       => cmem_wway,
      i_wdata      => cmem_wdata,
      i_wdata_wide => cmem_wdata_wide);

  refill_flusher : entity work.refill_flush
    generic map (
      ADDR_WIDTH       => ADDR_WIDTH,
      CACHE_DATA_WIDTH => LOWER_DATA_WIDTH,
      DATAS_PER_LINE   => DATAS_PER_LINE / (LOWER_DATA_WIDTH / DATA_WIDTH),
      DEBUG            => DEBUG)
    port map (
      clk         => clk,
      ena         => rf_ena,
      flush       => flusher_ctrl.ena,
      refill      => refill_ctrl.ena,
      refill_addr => refill_ctrl.addr,
      flush_addr  => flusher_ctrl.addr,
      o_done      => rf_done,

      o_cmem_we    => cmem_rf_we,
      o_cmem_waddr => cmem_rf_waddr,
      o_cmem_wdata => cmem_wdata_wide,
      o_cmem_raddr => cmem_rf_raddr,
      i_cmem_rdata => cmem_rdata_wide,

      o_memory_req   => memory_rf_req,
      o_memory_we    => memory_rf_we,
      o_memory_addr  => memory_rf_addr,
      i_memory_rdata => memory_rf_rdata,
      o_memory_wdata => memory_rf_wdata,
      i_memory_done  => memory_rf_done);

  refill_wway_mask <= (others => cmem_we);
  cmem_wway        <= to_way_selector(tdatao.way, cs) and refill_wway_mask when misser_state = m_idle else
               to_way_selector(search.way_evict, cs) and refill_wway_mask;

  -- Searcher
  cache_hit <= tdatao.found;

  tdata_raddr <= i_addr;
  tdata_waddr <= cmem_wa_waddr when misser_state = m_idle else missed_search.addr;
  tdata_re    <= '1';

  -- Cache memory signals
  cmem_raddr <= i_addr when misser_state = m_idle else
                cmem_rf_raddr when misser_state = m_flush_refill else cmem_wt_raddr;
  cmem_wide <= '1'        when misser_state = m_flush_refill else '0';
  cmem_re   <= '1';
  cmem_rway <= tdatao.way when misser_state = m_idle else
               refill_ctrl.way when misser_state = m_flush_refill else cmem_wt_rway;
  cmem_we    <= cmem_rf_we    when misser_state = m_flush_refill else cmem_wa_we;
  cmem_waddr <= cmem_rf_waddr when misser_state = m_flush_refill else cmem_wa_waddr;
  cmem_wdata <= cmem_wa_wdata;

  -- Lower memory signals
  o_memory_req <= memory_rf_req when misser_state = m_flush_refill
                  else memory_wt_req;
  o_memory_we <= memory_rf_we when misser_state = m_flush_refill
                 else memory_wt_we;
  o_memory_addr <= memory_rf_addr when misser_state = m_flush_refill
                   else memory_wt_addr;
  o_memory_wdata <= memory_rf_wdata when misser_state = m_flush_refill
                    else memory_wt_wdata;
  memory_rf_rdata <= i_memory_rdata;
  memory_rf_done  <= i_memory_done;
  memory_wt_done  <= i_memory_done;

  searcher : process(rst, clk, misser_state, search, delayed_wen)
    variable delayed_addr  : addr_t;
    variable delayed_wdata : data_t;
  begin
    if rising_edge(clk) then
      search.ren      <= delayed_ren;
      search.wen      <= delayed_wen;
      search.addr     <= delayed_addr;
      search.wdata    <= delayed_wdata;
      search.wthrough <= delayed_wthrough;

      search.found     <= cache_hit;
      search.way       <= tdatao.way;
      search.rtag      <= tdatao.rtag;
      search.way_evict <= tdatao.way_evict;
      search.etag      <= tdatao.etag;

      delayed_ren <= i_req and not i_wen;
      delayed_wen <= i_req and i_wen;
      delayed_addr := i_addr;
      delayed_wdata := i_wdata;
      if WRITE_BACK then
        delayed_wthrough <= i_do_write_through;
      else
        delayed_wthrough <= '1';
      end if;
    end if;

    if (misser_state /= m_idle or delayed_wen = '1' or
        (search.wen = '1' and not search.found)) then
      w_ready <= '0';
    else
      w_ready <= '1';
    end if;
  end process searcher;

  -- Reader
  -- r_ready       <= '0'        when not cache_hit          else '1';
  o_rdata       <= cmem_rdata when cache_hit else (others => 'X');
  o_rdata_valid <= '1'        when cache_hit else '0';
  -- Reader and Writer
  o_wready      <= w_ready;

  misser : process(rst, clk, misser_state, search, missed_search, tdatao,
                   rf_done, delayed_wen, delayed_wthrough, cmem_wa_waddr,
                   i_addr, i_wdata, cache_hit, memory_wt_done,
                   cmem_rdata_wide)
    variable should_refill     : boolean;
    variable should_flush      : boolean;
    variable search_hit        : boolean;
    variable missed_search_hit : boolean;
    variable ns                : misser_state_t;
  begin
    search_hit        := search.found;
    missed_search_hit := missed_search.found;
    should_flush      := not missed_search_hit and get_tag_dirty(missed_search.etag, cs) = '1';
    should_refill     := not missed_search_hit;

    if should_refill then
      refill_ctrl.ena <= '1';
    else
      refill_ctrl.ena <= '0';
    end if;
    if should_flush then
      flusher_ctrl.ena <= '1';
    else
      flusher_ctrl.ena <= '0';
    end if;

    rf_ena <= '0';

    flusher_ctrl.addr <= get_address(get_tag(missed_search.etag, cs),
                                     get_address_index(missed_search.addr, cs), 0, cs);
    flusher_ctrl.way <= missed_search.way_evict;

    refill_ctrl.addr <= missed_search.addr;
    refill_ctrl.way  <= missed_search.way_evict;

    tdata_evict <= '0';
    tdata_wway  <= (others => '0');

    if misser_state = m_idle then
      tdata_wtag <= to_tag_slv(get_address_tag(cmem_wa_waddr, cs),
                               '1',                               -- valid
                               get_tag_dirty(tdatao.rtag, cs) or  -- dirty
                               (delayed_wen and not delayed_wthrough),
                               TAG_SLV_EMPTY_CTXT);
    else
      tdata_wtag <= to_tag_slv(get_address_tag(missed_search.addr, cs),
                               '1',                               -- valid
                               missed_search.wen and not missed_search.wthrough,
                               TAG_SLV_EMPTY_CTXT);
    end if;

    cmem_wa_we    <= '0';
    cmem_wt_raddr <= missed_search.addr;

    memory_wt_req   <= '0';
    memory_wt_we    <= '1';
    memory_wt_addr  <= cmem_wa_waddr;
    memory_wt_wdata <= cmem_rdata_wide;

    ns := misser_state;
    case misser_state is
      when m_idle =>
        if not search_hit and (search.ren = '1' or search.wen = '1') then
          ns := m_prep_flush_refill;
        end if;
        if cache_hit and delayed_wen = '1' then
          cmem_wa_we <= '1';
          tdata_wway <= to_way_selector(tdatao.way, cs);
          if delayed_wthrough = '1' then
            ns := m_whit_through_prep;
          end if;
        end if;

      when m_prep_flush_refill =>
        ns     := m_flush_refill;
        rf_ena <= '1';

      when m_flush_refill =>
        if rf_done = '1' then
          tdata_wway  <= to_way_selector(missed_search.way_evict, cs);
          tdata_evict <= '1';
          if missed_search.wen = '1' then
            ns := m_wmiss_cdata;
          else
            ns := m_idle;
          end if;
        end if;

      when m_wmiss_cdata =>
        tdata_wway <= to_way_selector(missed_search.way_evict, cs);
        cmem_wa_we <= '1';
        if missed_search.wthrough = '1' then
          ns := m_wthrough_prep;
        else
          ns := m_idle;
        end if;

      when m_whit_through_prep =>
        cmem_wt_raddr <= search.addr;
        ns            := m_wthrough;

      when m_wthrough_prep =>
        ns := m_wthrough;

      when m_wthrough =>
        memory_wt_req <= '1';
        ns            := m_finish_wthrough;

      when m_finish_wthrough =>
        if memory_wt_done = '1' then
          ns := m_idle;
        end if;
    end case;

    if rising_edge(clk) then
      misser_state <= ns;

      if misser_state = m_idle then
        cmem_wa_waddr <= i_addr;
        cmem_wa_wdata <= i_wdata;
        cmem_wt_rway  <= tdatao.way;
      end if;

      if misser_state = m_flush_refill then
        cmem_wa_waddr <= missed_search.addr;
        cmem_wa_wdata <= missed_search.wdata;
      end if;

      if misser_state = m_wmiss_cdata then
        cmem_wt_rway <= missed_search.way_evict;
      end if;

    end if;

    if rising_edge(clk) and misser_state = m_idle then
      missed_search <= search;
    end if;
  end process misser;

  statistician : process(clk, misser_state, delayed_ren, delayed_wen,
                         delayed_wthrough, flusher_ctrl, refill_ctrl, cache_hit)
  begin
    if STATISTICS and rising_edge(clk) then
      case misser_state is
        when m_idle =>
          if cache_hit and delayed_ren = '1' then
            cstats.read_hits <= cstats.read_hits + 1;
          end if;
          if cache_hit and delayed_wen = '1' then
            cstats.write_hits <= cstats.write_hits + 1;
            if delayed_wthrough then
              cstats.write_throughs <= cstats.write_throughs + 1;
            else
              cstats.write_backs <= cstats.write_backs + 1;
            end if;
          end if;

          if not cache_hit and delayed_ren = '1' then
            cstats.read_misses <= cstats.read_misses + 1;
          end if;
          if not cache_hit and delayed_wen = '1' then
            cstats.write_misses <= cstats.write_misses + 1;
            if delayed_wthrough then
              cstats.write_throughs <= cstats.write_throughs + 1;
            else
              cstats.write_backs <= cstats.write_backs + 1;
            end if;
          end if;

        when m_prep_flush_refill =>
          if flusher_ctrl.ena = '1' then
            cstats.flushes <= cstats.flushes + 1;
          end if;
          if refill_ctrl.ena = '1' then
            cstats.refills <= cstats.refills + 1;
          end if;
        when others =>
      end case;
    end if;
  end process statistician;

  o_dbg_cstats <= cstats;
end architecture rtl;
