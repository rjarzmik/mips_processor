-------------------------------------------------------------------------------
-- Title      : Single input port associative cache
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : SinglePort_Associative_Cache.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-30
-- Last update: 2017-01-01
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Cache suited for an L1 cache
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-30  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

use work.cache_defs.all;
-------------------------------------------------------------------------------

entity SinglePort_Associative_Cache is

  generic (
    WRITE_BACK : boolean := true;
    DEBUG      : boolean := false
    );

  port (
    clk : in std_logic;
    rst : in std_logic;

    i_porta_req              : in  std_logic;
    i_porta_we               : in  std_logic;
    i_porta_addr             : in  addr_t;
    i_porta_write_data       : in  data_t;
    i_porta_do_write_through : in  std_logic;
    o_porta_read_data        : out data_t;
    o_porta_valid            : out std_logic;
    o_porta_rdy              : out std_logic;

    o_creq  : out cache_request_t;
    i_cresp : in  cache_response_t;

    o_dbg_state  : out cache_state;
    o_dbg_cstats : out cache_stats_t
    );

end entity SinglePort_Associative_Cache;

-------------------------------------------------------------------------------

architecture rtl of SinglePort_Associative_Cache is

  signal allocated_way : natural range 0 to NB_WAYS - 1;
  signal update_alloc  : boolean;

  --- Searcher
  type search_t is record
    way             : natural range 0 to NB_WAYS - 1;
    way_found       : boolean;
    way_alloc       : natural range 0 to NB_WAYS - 1;
    valid           : std_logic;
    dirty           : std_logic;
    all_dirty       : std_logic;
    data            : data_t;
    tag_entry       : tag_entry_t;
    evict_dirty     : boolean;
    evict_tag_entry : tag_entry_t;
  end record;
  signal search : search_t;

  signal state : cache_state := s_idle;

  type flusher_control is record
    addr : addr_t;
    way  : natural range 0 to NB_WAYS - 1;
    sel  : cache_line_selector_t;
  end record;

  type tdm_control is record
    re            : std_logic;
    raddr         : addr_t;
    waddr         : addr_t;
    we            : std_logic;
    update_way    : natural range 0 to NB_WAYS - 1;
    wtag_entry    : tag_entry_t;
    evict_compute : std_logic;
  end record tdm_control;
  constant TDM_CONTROL_IDLE : tdm_control :=
    (re         => '1', we => '0', evict_compute => '0',
     raddr      => (others => '0'), waddr => (others => '0'),
     update_way => 0, wtag_entry => (others => (others => '0')));

  type cdm_control is record
    re    : std_logic;
    raddr : addr_t;
    rway  : natural range 0 to NB_WAYS - 1;
    waddr : addr_t;
    wway  : way_selector_t;             -- write way by way
    wdata : data_t;
  end record cdm_control;
  constant CDM_CONTROL_IDLE : cdm_control :=
    (re    => '1', wway => (others => '0'),
     raddr => (others => '0'), rway => 0,
     waddr => (others => '0'), wdata => (others => '0'));

  type porta_control is record
    porta_rdata : data_t;
    porta_valid : std_logic;
    porta_rdy   : std_logic;
  end record porta_control;
  constant PA_CONTROL_IDLE : porta_control :=
    (porta_valid => '0', porta_rdy => '1', porta_rdata => (others => 'X'));
  constant PA_CONTROL_BUSY : porta_control :=
    (porta_valid => '0', porta_rdy => '0', porta_rdata => (others => 'X'));

  type flush_prepare_t is record
    flush_mask  : cache_line_selector_t;
    base_addr   : addr_t;
    way         : natural range 0 to NB_WAYS - 1;
    sclr        : std_logic;
    sdata       : cache_line_selector_t;
    bclr        : natural range 0 to DATAS_PER_LINE - 1;
    bclrena     : std_logic;
    cline       : cache_line_t;
    allclear    : std_logic;
    fbitset     : natural range 0 to DATAS_PER_LINE - 1;
    idx_rdata   : natural range 0 to DATAS_PER_LINE - 1;
    tag_entry   : tag_entry_t;
    next_victim : std_logic;
  end record flush_prepare_t;

  type refill_prepare_t is record
    refill_mask : cache_line_selector_t;
    base_addr   : addr_t;
    way         : natural range 0 to NB_WAYS - 1;
    sclr        : std_logic;
    sdata       : cache_line_selector_t;
    bclr        : natural range 0 to DATAS_PER_LINE - 1;
    bclrena     : std_logic;
    allclear    : std_logic;
    fbitset     : natural range 0 to DATAS_PER_LINE - 1;
    cline       : cache_line_t;
    tag_entry   : tag_entry_t;
    next_victim : std_logic;
  end record refill_prepare_t;

  --- Tags data
  signal tdata_addr      : addr_t    := (others => '0');
  signal tdata_re        : std_logic := '0';
  signal tdata_waddr     : addr_t    := (others => '0');
  signal tdata_we        : std_logic := '0';
  signal tdata_way       : natural range 0 to NB_WAYS - 1;
  signal tdata_way_alloc : std_logic;
  signal tdata_wtag      : tag_entry_t;

  --- Cache data memory
  signal cmem_addr  : addr_t         := (others => '0');
  signal cmem_re    : std_logic      := '0';
  signal cmem_rway  : natural range 0 to NB_WAYS - 1;
  signal cmem_waddr : addr_t         := (others => '0');
  signal cmem_wway  : way_selector_t := (others => '0');
  signal cmem_wdata : data_t;

  --- Fetcher signals
  signal porta_rdy                    : std_logic;  -- deasserted when porta request ongoing
  signal porta_requested_addr         : addr_t;
  signal porta_requested_we           : std_logic;
  signal porta_requested_writethrough : std_logic;
  signal porta_requested_wdata        : data_t;

  -- Cache line flusher prepaartor mask feeder
  signal ffeed_sclr     : std_logic;
  signal ffeed_sdata    : cache_line_selector_t;
  signal ffeed_data     : cache_line_selector_t;
  signal ffeed_bclrena  : std_logic;
  signal ffeed_bclr     : natural range 0 to DATAS_PER_LINE - 1;
  signal ffeed_fbitset  : natural range 0 to DATAS_PER_LINE - 1;
  signal ffeed_allclear : std_logic;

  -- Cache line refiller preparator mask feeder
  signal rfeed_sclr     : std_logic;
  signal rfeed_sdata    : cache_line_selector_t;
  signal rfeed_bclrena  : std_logic;
  signal rfeed_bclr     : natural range 0 to DATAS_PER_LINE - 1;
  signal rfeed_fbitset  : natural range 0 to DATAS_PER_LINE - 1;
  signal rfeed_allclear : std_logic;

  -- Outer interface
  signal cls_req   : cls_op;
  signal cls_creq  : cache_request_t;
  signal cls_cresp : cache_response_t;

  -- Debug and statistics
  signal cstats : cache_stats_t := (others => 0);

-----------------------------------------------------------------------------
-- Internal functions declarations
-----------------------------------------------------------------------------
  function reverse_any_vector (a : in std_logic_vector)
    return std_logic_vector is
    variable result : std_logic_vector(a'range);
    alias aa        : std_logic_vector(a'reverse_range) is a;
  begin
    for i in aa'range loop
      result(i) := aa(i);
    end loop;
    return result;
  end function reverse_any_vector;

  function get_acquire_new_search(req                 : std_ulogic; state : cache_state;
                                  rfeed_allclear      : std_ulogic;
                                  force_write_through : boolean)
    return boolean is
    variable o : boolean;
  begin
    o := (state = s_idle or state = s_searching or
          (state = s_refill_cache and rfeed_allclear = '1') or
          (state = s_write_allocate and not force_write_through) or
          state = s_writethrough) and
         req = '1';
    return o;
  end function get_acquire_new_search;

begin

  gtdm : entity work.tags_data_mem
    generic map (DEBUG => DEBUG)
    port map (
      clk               => clk,
      i_raddr           => tdata_addr,
      i_re              => tdata_re,
      o_tag_found       => search.way_found,
      o_way_found       => search.way,
      o_tag_entry       => search.tag_entry,
      o_data_valid      => search.valid,
      o_way_evict       => search.way_alloc,
      o_evict_dirty     => search.evict_dirty,
      o_evict_tag_entry => search.evict_tag_entry,
      i_waddr           => tdata_waddr,
      i_we              => tdata_we,
      i_update_way      => tdata_way,
      i_wtag_entry      => tdata_wtag,
      i_evict_compute   => tdata_way_alloc);

  gcdmem : entity work.cache_data_mem
    generic map (DEBUG => DEBUG)
    port map (
      clk     => clk,
      i_raddr => cmem_addr,
      i_re    => cmem_re,
      i_rway  => cmem_rway,
      o_rdata => search.data,
      i_waddr => cmem_waddr,
      i_wway  => cmem_wway,
      i_wdata => cmem_wdata);

  flush_feeder : entity work.mask_feeder
    generic map (
      WIDTH => DATAS_PER_LINE)
    port map (
      clk      => clk,
      sclr     => ffeed_sclr,
      sdata    => ffeed_sdata,
      bclrena  => ffeed_bclrena,
      bclr     => ffeed_bclr,
      fbitset  => ffeed_fbitset,
      allclear => ffeed_allclear);

  refill_feeder : entity work.mask_feeder
    generic map (
      WIDTH => DATAS_PER_LINE)
    port map (
      clk      => clk,
      sclr     => rfeed_sclr,
      sdata    => rfeed_sdata,
      bclrena  => rfeed_bclrena,
      bclr     => rfeed_bclr,
      fbitset  => rfeed_fbitset,
      allclear => rfeed_allclear);

  controller : process(rst, clk, i_porta_addr, i_porta_req, i_cresp, search,
                       porta_requested_we, porta_requested_addr,
                       porta_requested_wdata, state,
                       ffeed_allclear, ffeed_fbitset, rfeed_allclear, rfeed_fbitset,
                       cstats)
    variable wt_cline : cache_line_t;
    variable creq     : cache_request_t;
    variable cresp    : cache_response_t;

    -- Tags Data Memory control
    variable tdm                 : tdm_control;
    variable cdm                 : cdm_control;
    variable pa                  : porta_control;
    variable fprep               : flush_prepare_t;
    variable rprep               : refill_prepare_t;
    variable acquire_new_search  : boolean := false;
    variable force_write_through : boolean := false;
  begin
    -- Input variables following signals update
    fprep.allclear := ffeed_allclear;
    fprep.fbitset  := ffeed_fbitset;
    rprep.allclear := rfeed_allclear;
    rprep.fbitset  := rfeed_fbitset;
    cresp          := i_cresp;

    acquire_new_search := get_acquire_new_search(i_porta_req, state, rfeed_allclear, force_write_through);

    if rst = '1' then
      state              <= s_idle;
      cdm                := CDM_CONTROL_IDLE;
      tdm                := TDM_CONTROL_IDLE;
      pa                 := PA_CONTROL_IDLE;
      acquire_new_search := false;

      porta_requested_addr  <= (others => '0');
      porta_requested_we    <= '0';
      porta_requested_wdata <= (others => 'X');
    elsif rising_edge(clk) then
      if acquire_new_search then
        porta_requested_addr         <= i_porta_addr;
        porta_requested_we           <= i_porta_we;
        porta_requested_writethrough <= i_porta_do_write_through;
        porta_requested_wdata        <= i_porta_write_data;
      end if;

      case state is
        when s_idle =>
          if i_porta_req = '1' then
            state <= s_searching;
          else
            state <= s_idle;
          end if;
        when s_searching =>
          fprep.base_addr :=
            get_address(search.evict_tag_entry.tag,
                        get_address_index(porta_requested_addr), 0);

          if search.way_found and
            ((search.valid = '1' and porta_requested_we = '0') or
             porta_requested_we = '1') then
            if porta_requested_we = '0' then
              -- Read cache hit
              cstats.read_hits <= cstats.read_hits + 1;

              if i_porta_req = '0' then
                state <= s_idle;
              else
                state <= s_searching;
              end if;
            else
              -- Write cache hit
              cstats.write_hits <= cstats.write_hits + 1;

              state               <= s_write_allocate;
              fprep.way           := search.way;
              fprep.tag_entry     := search.tag_entry;
              fprep.next_victim   := '0';

              force_write_through := not WRITE_BACK or porta_requested_writethrough = '1';
              if force_write_through then
                cstats.write_throughs <= cstats.write_throughs + 1;
              else
                cstats.write_backs <= cstats.write_backs + 1;
              end if;
            end if;
          elsif porta_requested_we = '0' then
            -- Read miss
            cstats.read_misses <= cstats.read_misses + 1;

            if search.way_found then
              --- Way is found, but data in the cache line is not valid
              state             <= s_refill_memory;
              rprep.way         := search.way;
              rprep.tag_entry   := search.tag_entry;
              rprep.next_victim := '0';
              cstats.refills    <= cstats.refills + 1;
            elsif search.evict_dirty then
              state             <= s_prepare_flushing;
              fprep.way         := search.way_alloc;
              fprep.cline       := (others => (others => 'X'));
              rprep.way         := search.way_alloc;
              rprep.tag_entry   := TAG_ENTRY_EMPTY;
              rprep.next_victim := '1';
              fprep.flush_mask  := reverse_any_vector(search.evict_tag_entry.dirtys);
              cstats.flushes    <= cstats.flushes + 1;
            else
              state             <= s_refill_memory;
              rprep.way         := search.way_alloc;
              rprep.tag_entry   := TAG_ENTRY_EMPTY;
              rprep.next_victim := '1';
              cstats.refills    <= cstats.refills + 1;
            end if;
          else
            -- Write miss
            cstats.write_misses <= cstats.write_misses + 1;

            force_write_through := not WRITE_BACK or porta_requested_writethrough = '1';
            if search.evict_dirty then
              state             <= s_prepare_flushing;
              fprep.way         := search.way_alloc;
              fprep.tag_entry   := TAG_ENTRY_EMPTY;
              fprep.next_victim := '1';
              fprep.cline       := (others => (others => 'X'));
              fprep.flush_mask  := reverse_any_vector(search.evict_tag_entry.dirtys);
              cstats.flushes    <= cstats.flushes + 1;
            else
              state             <= s_write_allocate;
              fprep.way         := search.way_alloc;
              fprep.tag_entry   := TAG_ENTRY_EMPTY;
              fprep.cline       := (others => (others => 'X'));
              fprep.next_victim := '1';
            end if;

            if force_write_through then
              cstats.write_throughs <= cstats.write_throughs + 1;
            else
              cstats.write_backs <= cstats.write_backs + 1;
            end if;
          end if;


        when s_prepare_flushing =>
          fprep.cline(fprep.idx_rdata) := search.data;
          if fprep.allclear = '1' then
            state <= s_flush_outer;
          else
            state <= s_prepare_flushing;
          end if;
          fprep.idx_rdata := fprep.fbitset;


        when s_flush_outer =>
          state <= s_flushing;
          -- pragma translate_off
          if DEBUG then
            report "Cache: flush @" & to_hstring(fprep.base_addr) &
              " mask=" & to_bstring(fprep.flush_mask) &
              " at way " & integer'image(fprep.way);
          end if;
          -- pragma translate_on

        when s_writethrough =>
          if cresp.done = '0' then
            state <= s_writethrough;
          else
            if i_porta_req = '0' then
              state <= s_idle;
            else
              state <= s_searching;
            end if;
          end if;

        when s_flushing =>
          fprep.idx_rdata := fprep.fbitset;

          if cresp.done = '1' then
            if porta_requested_we = '0' then
              state          <= s_refill_memory;
              cstats.refills <= cstats.refills + 1;
            else
              state <= s_write_allocate;
            end if;
          else
            state <= s_flushing;
          end if;


        when s_write_allocate =>
          if force_write_through then
            state <= s_writethrough;
          else
            if i_porta_req = '0' then
              state <= s_idle;
            else
              state <= s_searching;
            end if;
          end if;


        when s_refill_memory =>
          if cresp.done = '1' then
            state <= s_refill_cache;
          else
            state <= s_refill_memory;
          end if;


        when s_refill_cache =>
          if rprep.allclear = '1' then
            if i_porta_req = '1' then
              state <= s_searching;
            else
              state <= s_idle;
            end if;

            -- pragma translate_off
            if DEBUG then
              report "Cache: refill @" & to_hstring(rprep.base_addr) &
                " mask=" & to_bstring(rprep.sdata) &
                " at way " & integer'image(rprep.way);
            end if;
          -- pragma translate_on
          else
            state <= s_refill_cache;
          end if;
      end case;

      if acquire_new_search then
        -- Cowardly, we're only refilling one data of the dataline
        rprep.refill_mask :=
          reverse_any_vector(std_logic_vector(to_unsigned(
            2**get_data_set_index(i_porta_addr), rprep.refill_mask'length)));
        rprep.base_addr :=
          get_address(get_address_tag(i_porta_addr),
                      get_address_index(i_porta_addr), 0);
      end if;
    end if;

    -- Default values
    fprep := (
      flush_mask  => fprep.flush_mask,
      base_addr   => fprep.base_addr,
      sclr        => '0',
      sdata       => reverse_any_vector(search.evict_tag_entry.dirtys),  -- s_prepare_flushing
      bclr        => fprep.idx_rdata, bclrena => '0',
      allclear    => ffeed_allclear, fbitset => ffeed_fbitset,
      cline       => fprep.cline,
      way         => fprep.way,         -- registered
      idx_rdata   => fprep.idx_rdata,   -- registered
      next_victim => fprep.next_victim,  -- registered
      tag_entry   => fprep.tag_entry
      );

    rprep := (
      refill_mask => rprep.refill_mask,
      -- refill_mask => (others => '0'),
      base_addr   => rprep.base_addr,
      way         => rprep.way, tag_entry => rprep.tag_entry,  -- registered
      next_victim => rprep.next_victim,                      -- registered
      sclr        => '0',
      sdata       => reverse_any_vector(rprep.refill_mask),  -- s_refill_memory
      bclr        => 0,  -- don't care, value is overwritten anyway
      bclrena     => '0',
      allclear    => rfeed_allclear, fbitset => rfeed_fbitset,
      cline       => (others => (others => ('X'))));

    cdm      := CDM_CONTROL_IDLE;
    cdm.rway := search.way;

    tdm := TDM_CONTROL_IDLE;
    pa  := PA_CONTROL_BUSY;             -- default
    creq := (req => cls_none, addr => (others => 'X'),
             sel => (others => '0'), cline => fprep.cline);
    cresp := i_cresp;

    for i in wt_cline'range loop
      wt_cline(i) := porta_requested_wdata;
    end loop;

    -- Combinational s_refill_memory
    if rst = '1' then
    else
      case state is
        when s_idle =>
          pa := PA_CONTROL_IDLE;

        when s_searching =>
          if search.way_found and
            ((search.valid = '1' and porta_requested_we = '0') or
             porta_requested_we = '1') then
            if porta_requested_we = '0' then
              -- Read cache hit
              --RJK dbg_incr_read_cache_hits(dbg_stats);
              pa := (porta_rdata => search.data, porta_valid => '1',
                     porta_rdy   => '1');
            else
              -- Write cache hit
              --RJK dbg_incr_write_cache_hits(dbg_stats);
              tdm.update_way    := search.way;
              tdm.wtag_entry    := search.tag_entry;
              tdm.evict_compute := '0';

              pa := PA_CONTROL_BUSY;
            end if;
          else
            -- Read of Write miss
            if porta_requested_we = '0' then
              -- Read miss
              if search.way_found then
                --- Way is found, but data in the cache line is not valid
                creq := (req => cls_refill, addr => rprep.base_addr,
                         sel => rprep.refill_mask, cline => rprep.cline);
              elsif search.evict_dirty then
                fprep.sclr    := '1';
                fprep.bclrena := '0';
              else
                creq := (req => cls_refill, addr => rprep.base_addr,
                         sel => rprep.refill_mask, cline => rprep.cline);
              end if;
            else
              -- Write miss
              if search.evict_dirty then
                fprep.sclr    := '1';
                fprep.bclrena := '0';
              else
              end if;
            end if;
          end if;

        when s_prepare_flushing =>
          fprep.sclr    := '0';
          fprep.bclr    := fprep.fbitset;
          fprep.bclrena := '1';
          cdm.rway      := fprep.way;

          if fprep.allclear = '1' then
          -- Cannot launch creq yet as the last transfer to frep.cline shoud
          -- be done first
          else
            cdm.raddr := std_logic_vector(
              unsigned(fprep.base_addr) + fprep.fbitset * DATA_WIDTH / 8);
            cdm.re := '1';
          end if;


        when s_flush_outer =>
          creq := (req => cls_flush, addr => fprep.base_addr,
                   sel => reverse_any_vector(fprep.flush_mask), cline => fprep.cline);


        when s_flushing =>
          creq.req := cls_none;

          if cresp.done = '1' then
            if porta_requested_we = '0' then
              creq := (req => cls_refill, addr => rprep.base_addr,
                       sel => rprep.refill_mask, cline => rprep.cline);
            end if;
          end if;


        when s_refill_memory =>
          if cresp.done = '1' then
            rprep.sclr    := '1';
            rprep.bclrena := '0';

            tdm.waddr             := porta_requested_addr;
            tdm.we                := '1';
            tdm.update_way        := rprep.way;
            tdm.wtag_entry        := rprep.tag_entry;
            tdm.wtag_entry.tag    := get_address_tag(rprep.base_addr);
            tdm.wtag_entry.valids := tdm.wtag_entry.valids or rprep.refill_mask;
            tdm.wtag_entry.dirtys := tdm.wtag_entry.dirtys and (not rprep.refill_mask);
            tdm.evict_compute     := rprep.next_victim;
          end if;


        when s_refill_cache =>

          rprep.sclr    := '0';
          rprep.bclrena := '1';
          rprep.bclr    := rprep.fbitset;

          if rprep.allclear = '1' then
            cdm       := CDM_CONTROL_IDLE;
            cdm.raddr := i_porta_addr;
            cdm.re    := i_porta_req;
            cdm.rway  := rprep.way;

            tdm       := TDM_CONTROL_IDLE;
            tdm.raddr := i_porta_addr;
            tdm.re    := i_porta_req;

            pa := (porta_rdata => search.data, porta_valid => '1',
                   porta_rdy   => '1');
          else
            cdm.raddr := porta_requested_addr;
            cdm.re    := '1';
            cdm.rway  := rprep.way;

            tdm.raddr := porta_requested_addr;
            tdm.re    := '1';
            tdm.we    := '0';

            cdm.waddr := std_logic_vector(
              unsigned(rprep.base_addr) + rprep.fbitset * DATA_WIDTH / 8);
            cdm.wway  := to_way_selector(rprep.way);
            cdm.wdata := cresp.cline(rprep.fbitset);
          end if;

        when s_write_allocate =>
          cdm       := CDM_CONTROL_IDLE;
          cdm.raddr := i_porta_addr;
          cdm.re    := i_porta_req;
          cdm.rway  := rprep.way;
          cdm.waddr := porta_requested_addr;
          cdm.wdata := porta_requested_wdata;
          cdm.wway  := to_way_selector(fprep.way);

          tdm                   := TDM_CONTROL_IDLE;
          tdm.raddr             := i_porta_addr;
          tdm.re                := i_porta_req;
          tdm.waddr             := porta_requested_addr;
          tdm.we                := '1';
          tdm.update_way        := fprep.way;
          tdm.wtag_entry        := fprep.tag_entry;
          tdm.wtag_entry.tag    := get_address_tag(porta_requested_addr);
          tdm.wtag_entry.valids := tdm.wtag_entry.valids
                                   or to_cacheline_selector(porta_requested_addr);
          if force_write_through then
            tdm.wtag_entry.dirtys := tdm.wtag_entry.dirtys and not to_cacheline_selector(porta_requested_addr);
          else
            tdm.wtag_entry.dirtys := tdm.wtag_entry.dirtys or to_cacheline_selector(porta_requested_addr);
          end if;
          tdm.evict_compute := fprep.next_victim;

          if not force_write_through then
            pa := (porta_rdata => (others => 'X'), porta_valid => '1',
                   porta_rdy   => '1');
          else
            -- Prepare the writethrough
            creq := (req   => cls_flush,
                     addr  => get_address(get_address_tag(porta_requested_addr),
                                         get_address_index(porta_requested_addr), 0),
                     sel   => reverse_any_vector(std_logic_vector(to_unsigned(
                       2**get_data_set_index(porta_requested_addr), creq.sel'length))),
                     cline => wt_cline);
          end if;


        when s_writethrough =>
          tdm       := TDM_CONTROL_IDLE;
          tdm.raddr := i_porta_addr;
          tdm.re    := i_porta_req;

          cdm       := CDM_CONTROL_IDLE;
          cdm.raddr := i_porta_addr;
          cdm.re    := i_porta_req;

          if cresp.done = '1' then
            pa := (porta_rdata => (others => 'X'), porta_valid => '1',
                   porta_rdy   => '1');
          end if;
      end case;
    end if;

    -- Control signal
    --- Tags data memory
    if rst = '0' and (state = s_idle or state = s_searching) then
      tdata_addr <= i_porta_addr;
      tdata_re   <= i_porta_req;
    else
      tdata_addr <= tdm.raddr;
      tdata_re   <= tdm.re;
    end if;
    tdata_waddr     <= tdm.waddr;
    tdata_we        <= tdm.we;
    tdata_way       <= tdm.update_way;
    tdata_wtag      <= tdm.wtag_entry;
    tdata_way_alloc <= tdm.evict_compute;

    --- Cache data memory
    if rst = '0' and (state = s_idle or state = s_searching) then
      cmem_addr <= i_porta_addr;
      cmem_re   <= i_porta_req;
    else
      cmem_addr <= cdm.raddr;
      cmem_re   <= cdm.re;
    end if;
    cmem_rway  <= cdm.rway;
    cmem_waddr <= cdm.waddr;
    cmem_wway  <= cdm.wway;
    cmem_wdata <= cdm.wdata;

    --- Flush : cache line mask feeder
    ffeed_sclr    <= fprep.sclr;
    ffeed_sdata   <= fprep.sdata;
    ffeed_bclrena <= fprep.bclrena;
    ffeed_bclr    <= fprep.bclr;

    --- Refiller : cache line mask feeder
    rfeed_sclr    <= rprep.sclr;
    rfeed_sdata   <= rprep.sdata;
    rfeed_bclrena <= rprep.bclrena;
    rfeed_bclr    <= rprep.bclr;

    -- Outer interface
    o_porta_read_data <= pa.porta_rdata;
    o_porta_valid     <= pa.porta_valid;
    o_porta_rdy       <= pa.porta_rdy;

    o_creq <= creq;

    -- Debug signals
    o_dbg_state  <= state;
    o_dbg_cstats <= cstats;

  end process controller;

end architecture rtl;

