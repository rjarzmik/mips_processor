-------------------------------------------------------------------------------
-- Title      : Branch target buffer
-- Project    : MIPS Processor
-------------------------------------------------------------------------------
-- File       : branch_target_buffer.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2017-02-25
-- Last update: 2018-12-02
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
--   This is a branch target predictior.
--   Due to internal FPGA constraints, the query is replied in 2 cycles, and
--   not 1.
--
--   Mode of operation :
--     - query_addr has to be registered.
--     - if update = '1', the (wsrc_addr -> wtgt_addr) is added, evicting a
--       random way.
--     - if one-cycle operation mode (TWO_CYCLES_ANSWER == false)
--       After next rising edge of clk, reply_addr, reply_ways and reply_predict
--       are populated for the query_addr of the current cycle.
--     - if two-cycle operation mode (TWO_CYCLES_ANSWER == true)
--       After next rising edge of clk, reply_addr, reply_ways and reply_predict
--       are populated for the query_addr of the former cycle.
--
--   Neither reply_addr, reply_ways nor reply_predict are registered.
--
--   Outputs:
--     - reply_addr: this is the target address which might be valid
--     - reply_ways: this is the way found array.
--                   If one of the bits is 1, then reply_addr is valid
--     - reply_predict : this is the prediction, up to the understanding of the
--                       user code
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
    ADDR_WIDTH        : natural;
    PREDICT_WIDTH     : natural := 0;
    NB_WAYS           : positive;
    CACHE_SIZE_BYTES  : positive;
    TWO_CYCLES_ANSWER : boolean := false;
    DEBUG             : boolean := false
    );

  port (
    clk           : in  std_logic;
    stall         : in  std_logic;
    query_addr    : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    reply_addr    : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    reply_predict : out std_logic_vector(PREDICT_WIDTH - 1 downto 0);
    reply_ways    : out std_logic_vector(0 to NB_WAYS - 1);
    --
    update        : in  std_logic;
    wsrc_addr     : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    wsrc_ways     : in  std_logic_vector(0 to NB_WAYS - 1);
    wtgt_addr     : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    wtgt_predict  : in  std_logic_vector(PREDICT_WIDTH - 1 downto 0)
    );
end entity branch_target_buffer;

architecture str of branch_target_buffer is
  constant cs : csizes := to_cache_sizing(ADDR_WIDTH, ADDR_WIDTH + PREDICT_WIDTH,
                                          1, NB_WAYS,
                                          CACHE_SIZE_BYTES);
  constant TAG_SLV_EMPTY_CTXT : std_logic_vector(0 downto 0) := (others => '0');
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  subtype data_t is std_logic_vector(ADDR_WIDTH + PREDICT_WIDTH - 1 downto 0);
  subtype mem_addr_t is std_logic_vector(cs.index_width - 1 downto 0);
  subtype tag_slv_t is std_logic_vector(cs.tag_slv_width - 1 downto 0);
  type tag_slv_vector is array(0 to NB_WAYS - 1) of tag_slv_t;
  type target_entry_t is array(0 to NB_WAYS - 1) of data_t;

  -- Reader signals
  signal r_query_addr      : addr_t;
  signal mem_raddr         : mem_addr_t;
  signal tags_read_data    : tag_slv_vector;
  signal r_tags_read_data  : tag_slv_vector;
  signal taddr_read_data   : target_entry_t;
  signal r_taddr_read_data : target_entry_t;

  -- Tag comparision signals
  signal r_high_half_match : std_logic_vector(0 to NB_WAYS - 1);
  signal high_half_match   : std_logic_vector(0 to NB_WAYS - 1);
  signal low_half_match    : std_logic_vector(0 to NB_WAYS - 1);
  signal way_match         : std_logic_vector(0 to NB_WAYS - 1);

  -- Writer signals
  signal mem_waddr          : mem_addr_t;
  signal tags_write_data    : tag_slv_vector;
  signal mem_wren           : std_logic_vector(0 to NB_WAYS - 1);
  signal way_to_update      : natural range 0 to NB_WAYS - 1 := 0;
  signal way_to_update_mask : std_logic_vector(0 to NB_WAYS - 1);
  signal wtgt               : std_logic_vector(wtgt_addr'length +
                                               wtgt_predict'length - 1 downto 0);

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

  function get_high_half_slv(i_addr : std_logic_vector)
    return std_logic_vector is
  begin
    if i_addr'ascending then
      return i_addr(i_addr'length / 2 to i_addr'length - 1);
    else
      return i_addr(i_addr'length - 1 downto i_addr'length / 2);
    end if;
  end function get_high_half_slv;

  function get_low_half_slv(i_addr : std_logic_vector)
    return std_logic_vector is
  begin
    if i_addr'ascending then
      return i_addr(0 to i_addr'length / 2 - 1);
    else
      return i_addr(i_addr'length / 2 - 1 downto 0);
    end if;
  end function get_low_half_slv;

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
    if or_reduce(wsrc_ways) = '0' then
      mem_wren <= to_way_selector(way_to_update, cs) and way_to_update_mask;
    else
      mem_wren <= wsrc_ways and way_to_update_mask;
    end if;
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
  tags_reader : process(clk, stall, query_addr, tags_read_data)
    variable te : tag_slv_t;
  begin
    if stall = '0' and rising_edge(clk) then
      r_query_addr <= query_addr;
      for i in 0 to NB_WAYS - 1 loop
        r_tags_read_data(i) <= tags_read_data(i);
      end loop;
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
  wtgt <= wtgt_addr & wtgt_predict;

  targetmems : for i in 0 to NB_WAYS - 1 generate
    cdata : sc_sram
      generic map (
        ADDR_WIDTH       => cs.index_width,
        DATA_WIDTH       => ADDR_WIDTH + PREDICT_WIDTH,
        READ_UNDER_WRITE => false,
        LOOKAHEAD        => true,
        DEBUG_NAME       => "cdata_w" & integer'image(i),
        DEBUG            => DEBUG)
      port map (clk, mem_raddr, mem_waddr, wtgt,
                '1', mem_wren(i), taddr_read_data(i));
  end generate targetmems;

  target_reader : process(clk, stall, taddr_read_data)
  begin
    if stall = '0' and rising_edge(clk) then
      for i in 0 to NB_WAYS - 1 loop
        r_taddr_read_data(i) <= taddr_read_data(i);
      end loop;
    end if;
  end process target_reader;

  tags_matcher : process(clk, stall, query_addr, r_query_addr,
                         tags_read_data, r_taddr_read_data)
    variable te              : tag_slv_t;
    variable addr            : addr_t;
    variable high_query_addr : std_logic_vector(ADDR_WIDTH / 2 - 1 downto 0);
    variable low_query_addr  : std_logic_vector(ADDR_WIDTH / 2 - 1 downto 0);
  begin
    for i in 0 to NB_WAYS - 1 loop
      te := tags_read_data(i);
      if get_high_half_slv(get_address_tag(query_addr, cs)) =
        get_high_half_slv(get_tag(te, cs)) then
        high_half_match(i) <= '1';
      else
        high_half_match(i) <= '0';
      end if;
    end loop;

    if stall = '0' and rising_edge(clk) then
      r_high_half_match <= high_half_match;
    end if;

    for i in 0 to NB_WAYS - 1 loop
      if TWO_CYCLES_ANSWER then
        te   := r_tags_read_data(i);
        addr := r_query_addr;
      else
        te   := tags_read_data(i);
        addr := query_addr;
      end if;

      if get_low_half_slv(get_address_tag(addr, cs)) =
        get_low_half_slv(get_tag(te, cs)) then
        low_half_match(i) <= '1';
      else
        low_half_match(i) <= '0';
      end if;
    end loop;

    for i in 0 to NB_WAYS - 1 loop
      if TWO_CYCLES_ANSWER then
        way_match(i) <= r_high_half_match(i) and low_half_match(i);
      else
        way_match(i) <= high_half_match(i) and low_half_match(i);
      end if;
    end loop;
  end process tags_matcher;

  outputer : process(clk, way_match, taddr_read_data, r_taddr_read_data)
    variable result : target_entry_t;
  begin
    for i in 0 to NB_WAYS - 1 loop
      if TWO_CYCLES_ANSWER then
        result := r_taddr_read_data;
      else
        result := taddr_read_data;
      end if;
    end loop;

    reply_ways    <= way_match;
    reply_addr    <= result(to_way(way_match))(result(0)'length - 1
                                               downto PREDICT_WIDTH);
    reply_predict <= result(to_way(way_match))(PREDICT_WIDTH - 1 downto 0);
  end process outputer;

end architecture str;
