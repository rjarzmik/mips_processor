-------------------------------------------------------------------------------
-- Title      : Jump predictor
-- Project    : MIPS Processor
-------------------------------------------------------------------------------
-- File       : jump_prediction.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2018-08-27
-- Last update: 2018-08-04
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
--   This is a jump predictior.
--   Due to internal FPGA constraints, the query is replied in 2 cycles, and
--   not 1.
--
--   Mode of operation :
--     - query_addr is sampled at rising edge of clk.
--       At this edge, reply_take_branch is filled with previous query_addr answer.
--       Before next rising edge, reply_take_branch is filled with this
--       query_addr answer (combinatory with a 1 cycle delay).
--
--     - if update = '1', the (wsrc_addr, wsrc_taken) is added, either evicting a
--       random way or updating the existing entry
--
--   The update should be inputed a "context". This context is either :
--     - (others => '0') if the update is a new entry
--     - the previous "reply_ctxt" if the update is really an update to an
--       existing entry, previously retrieved out of the predictor
-------------------------------------------------------------------------------
-- Copyright (c) 2018  Robert Jarzmik <robert.jarzmik@free.fr>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2018-08-27  1.0      robert.jarzmik@free.fr  Created
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

entity jump_prediction is
  generic (
    ADDR_WIDTH       : natural;
    NB_WAYS          : positive;
    CACHE_SIZE_BYTES : positive;
    NB_PREDICT_BITS  : natural := 2;
    DEBUG            : boolean := false
    );

  port (
    clk                : in  std_logic;
    stall              : in  std_logic;
    query_addr         : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    reply_take_branch  : out std_logic;
    reply_ctxt_way     : out std_logic_vector(0 to NB_WAYS - 1);
    reply_ctxt_predict : out std_logic_vector(NB_PREDICT_BITS - 1 downto 0);
    --
    update             : in  std_logic;
    wsrc_addr          : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    wsrc_ctxt_way      : in  std_logic_vector(0 to NB_WAYS - 1);
    wsrc_ctxt_predict  : in  std_logic_vector(NB_PREDICT_BITS - 1 downto 0);
    wsrc_taken         : in  std_logic
    );
end entity jump_prediction;

architecture str of jump_prediction is
  constant cs : csizes := to_cache_sizing(ADDR_WIDTH, ADDR_WIDTH,
                                          1, NB_WAYS,
                                          CACHE_SIZE_BYTES);
  constant TAG_SLV_EMPTY_CTXT : std_logic_vector(0 downto 0) := (others => '0');
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  subtype data_t is std_logic_vector(NB_PREDICT_BITS - 1 downto 0);
  subtype mem_addr_t is std_logic_vector(cs.index_width - 1 downto 0);
  subtype tag_slv_t is std_logic_vector(cs.tag_slv_width - 1 downto 0);
  type tag_slv_vector is array(0 to NB_WAYS - 1) of tag_slv_t;
  type target_entry_t is array(0 to NB_WAYS - 1) of data_t;

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
  signal mem_wdata          : data_t;
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
    variable next_update : natural range 0 to NB_WAYS - 1 := 0;
  begin
    if update = '1' and rising_edge(clk) then
      next_update := (next_update + 1) mod NB_WAYS;
    end if;

    if wsrc_ctxt_way = ('0', '0') then
      way_to_update <= next_update;
    else
      way_to_update <= to_way(wsrc_ctxt_way);
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

    if stall = '0' and rising_edge(clk) then
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
  ----------------------------- Branch predictions -----------------------------
  ------------------------------------------------------------------------------
  predictmems : for i in 0 to NB_WAYS - 1 generate
    cdata : sc_sram
      generic map (
        ADDR_WIDTH       => cs.index_width,
        DATA_WIDTH       => 2,
        READ_UNDER_WRITE => false,
        LOOKAHEAD        => false,
        DEBUG_NAME       => "cdata_w" & integer'image(i),
        DEBUG            => DEBUG)
      port map (clk, mem_raddr, mem_waddr, mem_wdata,
                '1', mem_wren(i), taddr_read_data(i));
  end generate predictmems;

  predict_reader : process(r_query_addr, r_tags_read_data, taddr_read_data,
                           f_tags_read_data_validfound,
                           r_tags_read_data_validfound, rfound)
    variable te     : tag_slv_t;
    variable result : data_t;
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

    reply_take_branch  <= not result(0) and rfound;
    reply_ctxt_way     <= f_tags_read_data_validfound;
    reply_ctxt_predict <= result;
  end process predict_reader;

  predict_writer : process(wsrc_ctxt_way, wsrc_ctxt_predict)
    variable previous : natural;
    variable setter   : natural;
    variable is_max   : boolean;
  begin
    previous := to_integer(unsigned(wsrc_ctxt_predict));
    is_max   := and_reduce(wsrc_ctxt_predict) = '1';

    setter := previous;
    if wsrc_taken = '1' and not is_max then
      setter := previous + 1;
    end if;
    if wsrc_taken = '0' and previous /= 0 then
      setter := previous - 1;
    end if;

    mem_wdata <= std_logic_vector(to_unsigned(setter,
                                              wsrc_ctxt_predict'length));
  end process predict_writer;

end architecture str;
