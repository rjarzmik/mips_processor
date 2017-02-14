-------------------------------------------------------------------------------
-- Title      : Cache flusher and refiller
-- Project    : Cache
-------------------------------------------------------------------------------
-- File       : refill.vhd
-- Author     : Robert Jarzmik <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2017-01-24
-- Last update: 2017-02-07
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Cache refiller
-------------------------------------------------------------------------------
-- Copyright (c) 2017
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2017-01-24  1.0      rjarzmik        Created
-------------------------------------------------------------------------------

library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;
use ieee.math_real.all;

library rjarzmik;
use rjarzmik.fifos.scfifo;
use rjarzmik.fifos.fifo2memory;

entity refill_flush is
  generic (
    ADDR_WIDTH       : natural;
    CACHE_DATA_WIDTH : natural;
    DATAS_PER_LINE   : natural;
    DEBUG            : boolean
    );

  port (
    clk            : in  std_ulogic;
    ena            : in  std_ulogic;
    flush          : in  std_ulogic;
    refill         : in  std_ulogic;
    refill_addr    : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    flush_addr     : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_done         : out std_ulogic;
    -- upper cache interface
    --- refill signals
    o_cmem_we      : out std_ulogic;
    o_cmem_waddr   : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_cmem_wdata   : out std_logic_vector(CACHE_DATA_WIDTH - 1 downto 0);
    --- flush signals
    o_cmem_re      : out std_ulogic;
    o_cmem_raddr   : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_cmem_rdata   : in  std_logic_vector(CACHE_DATA_WIDTH - 1 downto 0);
    -- lower cache/memory interface
    o_memory_req   : out std_ulogic;
    o_memory_we    : out std_ulogic;
    o_memory_addr  : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_memory_rdata : in  std_logic_vector(CACHE_DATA_WIDTH - 1 downto 0);
    o_memory_wdata : out std_logic_vector(CACHE_DATA_WIDTH - 1 downto 0);
    i_memory_done  : in  std_ulogic
    );
end entity refill_flush;

architecture str of refill_flush is
  constant DATAS_PER_LINE_WIDTH : natural := integer(log2(real(DATAS_PER_LINE)));
  constant ADDR_DATA_NBITS      : natural := integer(log2(real(CACHE_DATA_WIDTH / 8)));

  subtype addr_t is std_ulogic_vector(ADDR_WIDTH - 1 downto 0);
  subtype cdata_t is std_ulogic_vector(CACHE_DATA_WIDTH - 1 downto 0);
  subtype mdata_t is std_ulogic_vector(CACHE_DATA_WIDTH - 1 downto 0);

  type state_t is (idle, flushing, refilling);
  signal state : state_t := idle;

  signal aclr : std_ulogic;

  -- Cache to FIFOs query signals
  signal c2fifos_saddr : std_ulogic;
  signal c2fifos_addr  : addr_t;
  signal c2fifos_ren   : std_ulogic;
  signal c2fifos_wen   : std_ulogic;
  signal c2fifos_ready : std_ulogic;

  -- FIFOs to Memory query signals
  signal fifos2mem_saddr : std_ulogic;
  signal fifos2mem_addr  : addr_t;
  signal fifos2mem_ren   : std_ulogic;
  signal fifos2mem_wen   : std_ulogic;
  signal fifos2mem_ready : std_ulogic;

  signal cmem_req   : std_logic;
  signal cmem_we    : std_logic;
  signal cmem_addr  : addr_t;
  signal cmem_rdata : cdata_t;
  signal cmem_wdata : cdata_t;

  signal cmem_raddr_overena  : boolean;
  signal cmem_raddr_override : addr_t;

  -- Memory read operation signals
  --- memory to fifo and vice-versa
  signal f2m_full  : std_ulogic;
  signal m2f_data  : mdata_t;
  signal m2f_wrreq : std_ulogic;
  --- cache queries to fifo
  signal f2c_empty : std_ulogic;
  signal c2f_rdreq : std_ulogic;
  signal f2c_rdata : cdata_t;

  -- Memory write operation signals
  --- cache queries to fifo
  signal f2c_full  : std_ulogic;
  signal c2f_wdata : cdata_t;
  signal c2f_wrreq : std_ulogic;
  --- memory to fifo
  signal f2m_empty : std_ulogic;
  signal m2f_rdreq : std_ulogic;
  signal f2m_data  : mdata_t;

  -- Memory unresolved logic convertors
  signal memory_addr  : addr_t;
  signal memory_rdata : mdata_t;
  signal memory_wdata : mdata_t;

begin  -- architecture str

  scfifo_mread : scfifo
    generic map (
      DATA_WIDTH => CACHE_DATA_WIDTH,
      DEPTH      => DATAS_PER_LINE,
      LOOKAHEAD  => false,
      DEBUG_NAME => "scfifo_mread",
      DEBUG      => DEBUG)
    port map (
      clock => clk,
      aclr  => aclr,

      full  => f2m_full,
      data  => m2f_data,
      wrreq => m2f_wrreq,

      empty => f2c_empty,
      rdreq => c2f_rdreq,
      q     => f2c_rdata);

  scfifo_mwrite : scfifo
    generic map (
      DATA_WIDTH => CACHE_DATA_WIDTH,
      DEPTH      => DATAS_PER_LINE,
      LOOKAHEAD  => false,
      DEBUG_NAME => "scfifo_mwrite",
      DEBUG      => DEBUG)
    port map (
      clock => clk,
      aclr  => aclr,

      full  => f2c_full,
      data  => c2f_wdata,
      wrreq => c2f_wrreq,

      empty => f2m_empty,
      rdreq => m2f_rdreq,
      q     => f2m_data);

  fifos2m : fifo2memory
    generic map (
      ADDR_WIDTH      => ADDR_WIDTH,
      DATA_WIDTH      => CACHE_DATA_WIDTH,
      ADDR_STEP       => CACHE_DATA_WIDTH / 8,
      ADDR_STEP_WIDTH => DATAS_PER_LINE_WIDTH + ADDR_DATA_NBITS,
      MAX_NBREADS     => DATAS_PER_LINE,
      DEBUG_NAME      => "fifos2mem",
      DEBUG           => DEBUG)
    port map (
      clock   => clk,
      saddr   => fifos2mem_saddr,
      addr    => fifos2mem_addr,
      ren     => fifos2mem_ren,
      nbreads => DATAS_PER_LINE,
      wen     => fifos2mem_wen,
      ready   => fifos2mem_ready,

      -- Memory read queries interface
      full  => f2m_full,
      wrreq => m2f_wrreq,
      wdata => m2f_data,

      -- Memory write queries interface
      empty => f2m_empty,
      rdreq => m2f_rdreq,
      rdata => f2m_data,

      mreq     => o_memory_req,
      mwen     => o_memory_we,
      maddr    => memory_addr,
      mrdata   => memory_rdata,
      mwdata   => memory_wdata,
      mwready  => i_memory_done,
      mrdvalid => i_memory_done);

  o_memory_addr  <= std_logic_vector(memory_addr);
  memory_rdata   <= std_ulogic_vector(i_memory_rdata);
  o_memory_wdata <= std_logic_vector(memory_wdata);

  c2fifos : fifo2memory
    generic map (
      ADDR_WIDTH      => ADDR_WIDTH,
      DATA_WIDTH      => CACHE_DATA_WIDTH,
      ADDR_STEP       => CACHE_DATA_WIDTH / 8,
      ADDR_STEP_WIDTH => DATAS_PER_LINE_WIDTH + ADDR_DATA_NBITS,
      MAX_NBREADS     => DATAS_PER_LINE,
      DEBUG_NAME      => "cache2fifos",
      DEBUG           => DEBUG)
    port map (
      clock   => clk,
      saddr   => c2fifos_saddr,
      addr    => c2fifos_addr,
      ren     => c2fifos_ren,
      nbreads => DATAS_PER_LINE,
      wen     => c2fifos_wen,
      ready   => c2fifos_ready,

      -- Cache read queries (aka flush) interface
      full  => f2c_full,
      wdata => c2f_wdata,
      wrreq => c2f_wrreq,

      -- Cache write queries (aka refill) interface
      empty => f2c_empty,
      rdreq => c2f_rdreq,
      rdata => f2c_rdata,

      mreq     => cmem_req,
      mwen     => cmem_we,
      maddr    => cmem_addr,
      mrdata   => cmem_rdata,
      mwdata   => cmem_wdata,
      mwready  => '1',
      mrdvalid => '1');
  o_cmem_re    <= cmem_req;
  o_cmem_we    <= cmem_we and cmem_req;
  o_cmem_raddr <= std_logic_vector(cmem_raddr_override) when cmem_raddr_overena
                  else std_logic_vector(cmem_addr);
  o_cmem_waddr <= std_logic_vector(cmem_addr);
  cmem_rdata   <= std_ulogic_vector(i_cmem_rdata);
  o_cmem_wdata <= std_logic_vector(cmem_wdata);

  o_done <= '1' when state = idle else '0';

  fifos2mem_saddr <= c2fifos_saddr;

  controller : process(clk, ena, state, refill_addr, flush_addr, refill, flush,
                       fifos2mem_ready, f2c_empty, f2m_empty, c2fifos_ready)
    variable ns        : state_t;
    variable do_refill : std_ulogic;
    variable l_addr    : addr_t;
  begin
    ns := state;

    aclr          <= ena;
    c2fifos_saddr <= '1';
    c2fifos_addr  <= std_ulogic_vector(refill_addr);
    c2fifos_ren   <= '0';
    c2fifos_wen   <= '0';

    fifos2mem_addr <= std_ulogic_vector(refill_addr);
    fifos2mem_ren  <= '0';
    fifos2mem_wen  <= '0';

    -- Align addresses for shrinking/enlarging scfifos
    fifos2mem_addr(DATAS_PER_LINE_WIDTH + ADDR_DATA_NBITS - 1 downto 0) <=
      (others => '0');
    c2fifos_addr(DATAS_PER_LINE_WIDTH + ADDR_DATA_NBITS - 1 downto 0) <=
      (others => '0');

    -- After a refill, the read address must come back to the initial
    -- programmed value for cache to hit.
    cmem_raddr_overena  <= false;
    cmem_raddr_override <= l_addr;

    case state is
      when idle =>
        if ena = '1' and refill = '1' then
          ns            := refilling;
          if flush = '0' then
            fifos2mem_ren <= '1';
          end if;
        end if;
        if ena = '1' and flush = '1' then
          ns             := flushing;
          c2fifos_addr   <= std_ulogic_vector(flush_addr);
          c2fifos_ren    <= '1';
          fifos2mem_addr <= std_ulogic_vector(flush_addr);
        end if;
        cmem_raddr_overena <= true;

      when flushing =>
        c2fifos_saddr <= '0';
        fifos2mem_wen <= '1';
        if c2fifos_ready = '1' and f2m_empty = '1' and fifos2mem_ready = '1' then
          if do_refill = '1' then
            ns             := refilling;
            fifos2mem_ren  <= '1';
            c2fifos_saddr  <= '1';
            c2fifos_addr   <= l_addr;
            fifos2mem_addr <= l_addr;
            -- Align addresses for shrinking/enlarging scfifos
            fifos2mem_addr(DATAS_PER_LINE_WIDTH + ADDR_DATA_NBITS - 1 downto 0) <=
              (others => '0');
            c2fifos_addr(DATAS_PER_LINE_WIDTH + ADDR_DATA_NBITS - 1 downto 0) <=
              (others => '0');
          else
            ns := idle;
          end if;
        end if;

      when refilling =>
        c2fifos_saddr <= '0';
        c2fifos_wen <= '1';
        if fifos2mem_ready = '1' and f2c_empty = '1' then
          ns := idle;
        end if;
    end case;

    if rising_edge(clk) then
      state <= ns;
    end if;

    if rising_edge(clk) and ena = '1' then
      l_addr    := std_ulogic_vector(refill_addr);
      do_refill := refill;
    end if;
  end process controller;

end architecture str;
