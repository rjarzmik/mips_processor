-------------------------------------------------------------------------------
-- Title      : Branch target buffer
-- Project    : MIPS Processor
-------------------------------------------------------------------------------
-- File       : branch_target_buffer.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2017-02-25
-- Last update: 2018-08-03
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
--   This is a branch target predictior.
--   Due to internal FPGA constraints, the query is replied in 2 cycles, and
--   not 1.
--
--   Mode of operation :
--     - query_addr is sampled at rising edge of clk.
--       At this edge, reply_addr is filled with previous query_addr answer.
--       Before next rising edge, reply_addr is filled with this query_addr answer
--       (combinatory with a 1 cycle delay).
--
--     - if update = '1', the (wsrc_addr -> wtgt_addr) is added, evicting a
--       random way.
-------------------------------------------------------------------------------
-- Copyright (c) 2017  Robert Jarzmik <robert.jarzmik@free.fr>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2017-02-25  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library rjarzmik;
use rjarzmik.memories.sc_sram;
use rjarzmik.slv_utils.slv_is_x;
use rjarzmik.slv_utils.or_reduce;
use rjarzmik.slv_utils.and_reduce;

use work.cache_defs.all;
use work.cache_sizing.all;

entity branch_target_buffer is
  generic (
    ADDR_WIDTH       : natural;
    NB_WAYS          : positive;
    CACHE_SIZE_BYTES : positive;
    STEP             : natural;
    DEBUG            : boolean := false
    );

  port (
    clk          : in  std_logic;
    query_addr   : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    reply_addr   : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    reply_wfound : out std_logic;
    --
    update       : in  std_logic;
    wsrc_addr    : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    wtgt_addr    : in  std_logic_vector(ADDR_WIDTH - 1 downto 0)
    );
end entity branch_target_buffer;

architecture str of branch_target_buffer is
  constant cs : csizes := to_cache_sizing(ADDR_WIDTH, ADDR_WIDTH,
                                          1, NB_WAYS,
                                          CACHE_SIZE_BYTES);
  constant TAG_SLV_EMPTY_CTXT : std_logic_vector(0 downto 0) := (others => '0');
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  subtype mem_addr_t is std_logic_vector(cs.index_width - 1 downto 0);
  subtype tag_slv_t is std_logic_vector(cs.tag_slv_width - 1 downto 0);
  type tag_slv_vector is array(0 to NB_WAYS - 1) of tag_slv_t;
  type target_entry_t is array(0 to NB_WAYS - 1) of addr_t;

  -- Reader signals
  signal r_query_addr                : addr_t;
  signal mem_raddr                   : mem_addr_t;
  signal tags_read_data              : tag_slv_vector;
  signal r_tags_read_data            : tag_slv_vector;
  signal tags_read_data_validfound   : std_ulogic_vector(0 to NB_WAYS - 1);
  signal r_tags_read_data_validfound : std_ulogic_vector(0 to NB_WAYS - 1);
  signal f_tags_read_data_validfound : std_ulogic_vector(0 to NB_WAYS - 1);
  signal rfound                      : std_logic;
  signal taddr_read_data             : target_entry_t;

  -- Writer signals
  signal mem_waddr          : mem_addr_t;
  signal tags_write_data    : tag_slv_vector;
  signal mem_wren           : std_logic_vector(0 to NB_WAYS - 1);
  signal way_to_update      : natural range 0 to NB_WAYS - 1 := 0;
  signal way_to_update_mask : std_logic_vector(0 to NB_WAYS - 1);

  function get_mem_addr(i_addr : addr_t) return mem_addr_t is
    variable index : natural range 0 to cs.nb_lines - 1;
  begin
    if slv_is_x(i_addr) then
      return (others => 'X');
    else
      index := get_address_index(i_addr, cs);
      return std_logic_vector(to_unsigned(index, cs.index_width));
    end if;
  end function get_mem_addr;

begin  -- architecture str

  -- Common reader part
  mem_raddr <= get_mem_addr(query_addr);

  -- Common writer part
  mem_waddr <= get_mem_addr(wsrc_addr);
  way_updater : process(clk, update)
  begin
    if update = '1' and rising_edge(clk) then
      way_to_update <= (way_to_update + 1) mod NB_WAYS;
    end if;
  end process way_updater;

  common_writer : process(update, way_to_update, way_to_update_mask)
  begin
    way_to_update_mask <= (others => update);
    mem_wren           <= to_way_selector(way_to_update, cs) and way_to_update_mask;
  end process common_writer;

  ------------------------------------------------------------------------------
  ------------------------------------ Tags ------------------------------------
  ------------------------------------------------------------------------------
  tmem : for i in 0 to NB_WAYS - 1 generate
    tdata : sc_sram
      generic map (ADDR_WIDTH       => cs.index_width,
                   DATA_WIDTH       => cs.tag_slv_width,
                   READ_UNDER_WRITE => false,
                   LOOKAHEAD        => true,
                   DEBUG_NAME       => "tdata_w" & integer'image(i),
                   DEBUG            => DEBUG)
      port map (clk, mem_raddr, mem_waddr, tags_write_data(i),
                '1', mem_wren(i), tags_read_data(i));
  end generate tmem;

  -- Tags reader part
  tags_reader : process(clk, mem_raddr, query_addr, r_query_addr,
                        tags_read_data, tags_read_data_validfound)
    variable te : tag_slv_t;
  begin
    for i in 0 to NB_WAYS - 1 loop
      te := tags_read_data(i);
      if get_address_tag(query_addr, cs)(cs.tag_width / 2 - 1 downto 0)
        = get_tag(te, cs)(cs.tag_width / 2 - 1 downto 0) and
        get_tag_valid(te, cs) = '1' then
        tags_read_data_validfound(i) <= '1';
      else
        tags_read_data_validfound(i) <= '0';
      end if;
    end loop;

    if rising_edge(clk) then
      for i in 0 to NB_WAYS - 1 loop
        r_tags_read_data(i)            <= tags_read_data(i);
        r_tags_read_data_validfound(i) <= tags_read_data_validfound(i);
      end loop;
      r_query_addr <= query_addr;
    end if;
  end process tags_reader;

  -- Tags writer part
  tags_writer : process(wsrc_addr, update, way_to_update, way_to_update_mask)
  begin
    for way in tags_write_data'range loop
      tags_write_data(way) <= to_tag_slv(get_address_tag(wsrc_addr, cs),
                                         '1', '0', TAG_SLV_EMPTY_CTXT);
    end loop;
  end process tags_writer;

  ------------------------------------------------------------------------------
  ------------------------------ Target addresses ------------------------------
  ------------------------------------------------------------------------------
  targetmems : for i in 0 to NB_WAYS - 1 generate
    cdata : sc_sram
      generic map (
        ADDR_WIDTH       => cs.index_width,
        DATA_WIDTH       => ADDR_WIDTH,
        READ_UNDER_WRITE => false,
        LOOKAHEAD        => false,
        DEBUG_NAME       => "cdata_w" & integer'image(i),
        DEBUG            => DEBUG)
      port map (clk, mem_raddr, mem_waddr, wtgt_addr,
                '1', mem_wren(i), taddr_read_data(i));
  end generate targetmems;

  target_reader : process(r_query_addr, r_tags_read_data, taddr_read_data,
                          f_tags_read_data_validfound,
                          r_tags_read_data_validfound, rfound)
    variable te     : tag_slv_t;
    variable result : addr_t;
  begin
    for i in 0 to NB_WAYS - 1 loop
      te := r_tags_read_data(i);
      if get_address_tag(r_query_addr, cs)(cs.tag_width - 1 downto cs.tag_width / 2)
        = get_tag(te, cs)(cs.tag_width - 1 downto cs.tag_width / 2) and
        r_tags_read_data_validfound(i) = '1' then
        f_tags_read_data_validfound(i) <= '1';
      else
        f_tags_read_data_validfound(i) <= '0';
      end if;
    end loop;

    result := taddr_read_data(to_way(std_logic_vector(f_tags_read_data_validfound)));
    rfound <= or_reduce(f_tags_read_data_validfound);

    reply_addr <= result;
    reply_wfound <= rfound;
  end process target_reader;

end architecture str;
