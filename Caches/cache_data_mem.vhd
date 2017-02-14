-------------------------------------------------------------------------------
-- Title      : Cache content memory
-- Project    : Cache
-------------------------------------------------------------------------------
-- File       : cache_data_mem.vhd
-- Author     : Robert Jarzmik <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-17
-- Last update: 2017-02-13
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2016-12-17  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library rjarzmik;
use rjarzmik.memories.sc_dualw_sram;
use rjarzmik.slv_utils.assign_row;
use rjarzmik.slv_utils.slv_2d;
use rjarzmik.slv_utils.slv_2d_mux;
use rjarzmik.slv_utils.slv_is_x;

use work.cache_defs.all;
use work.cache_sizing.all;

entity cache_data_mem is
  generic (
    ADDR_WIDTH       : positive;
    DATA_WIDTH       : positive;
    DATAS_PER_LINE   : positive;
    NB_WAYS          : positive;
    CACHE_SIZE_BYTES : positive;
    WIDTH_WIDENER    : natural;
    DEBUG            : boolean := false
    );

  port (
    clk          : in  std_logic;
    wide         : in  std_logic;
    -- Reader
    i_raddr      : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_re         : in  std_logic;
    i_rway       : in  natural range 0 to NB_WAYS - 1;
    -- o_rdata = o_data_by_way(i_rway)
    o_rdata      : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_rdata_wide : out std_logic_vector(DATA_WIDTH * 2**WIDTH_WIDENER - 1 downto 0);

    -- Writer
    i_waddr      : in std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_wway       : in std_logic_vector(0 to NB_WAYS - 1);   -- write way by way
    i_wdata      : in std_logic_vector(DATA_WIDTH - 1 downto 0);
    i_wdata_wide : in std_logic_vector(DATA_WIDTH * 2**WIDTH_WIDENER - 1 downto 0)
    );

end entity cache_data_mem;

architecture str of cache_data_mem is
  constant cs : csizes := to_cache_sizing(ADDR_WIDTH, DATA_WIDTH,
                                          DATAS_PER_LINE, NB_WAYS,
                                          CACHE_SIZE_BYTES);
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  subtype way_selector_t is std_logic_vector(0 to cs.nb_ways - 1);

  constant data_one_nb_bits  : natural := cs.addr_data_nbits;
  constant data_line_nb_bits : natural := cs.datas_per_line_width;
  constant data_set_nb_bits  : natural := data_one_nb_bits + data_line_nb_bits;
  constant index_nb_bits     : natural := cs.index_width;

  -- Entry anatomy, ie. entry for a given address index
  --- entry  [ [way_M-1_data] [way_M-2_data] ... [way_0_data] ]
  --- way_X_data = cached data
  type cache_entry_t is array(0 to NB_WAYS - 1) of
    std_logic_vector(DATA_WIDTH - 1 downto 0);

  --type cache_entry_t is array(0 to NB_WAYS - 1) of
  --  std_logic_vector(DATA_WIDTH - 1 downto 0);
  type cache_entry_wide_t is array(0 to NB_WAYS - 1) of
    std_logic_vector(DATA_WIDTH * 2**WIDTH_WIDENER - 1 downto 0);

  -- Tag Status Memories interface
  subtype mem_addr_t is std_logic_vector(data_line_nb_bits + index_nb_bits - 1 downto 0);
  subtype mem_entry_t is std_logic_vector(NB_WAYS * DATA_WIDTH - 1 downto 0);
  signal mem_raddr              : mem_addr_t;
  signal mem_waddr              : mem_addr_t;
  signal mem_read_data          : cache_entry_t      := (others => (others => '0'));
  signal mem_read_data_mux      : slv_2d(0 to NB_WAYS - 1, DATA_WIDTH - 1 downto 0);
  signal mem_write_data         : cache_entry_t      := (others => (others => '0'));
  signal mem_write_data_mux     : slv_2d(0 to NB_WAYS - 1, DATA_WIDTH - 1 downto 0);
  signal mem_read_data_wide     : cache_entry_wide_t := (others => (others => '0'));
  signal mem_read_data_wide_mux : slv_2d(0 to NB_WAYS - 1, DATA_WIDTH * 2**WIDTH_WIDENER - 1 downto 0);
  signal mem_write_data_wide    : cache_entry_wide_t := (others => (others => '0'));
  signal mem_rren               : way_selector_t     := (others => '0');
  signal mem_wren               : way_selector_t     := (others => '0');

  function get_mem_addr(i_addr : addr_t) return mem_addr_t is
    variable index        : natural range 0 to cs.nb_lines - 1;
    variable data_in_line : natural range 0 to cs.datas_per_line - 1;
  begin
    if slv_is_x(i_addr) then
      return (others => 'X');
    else
      index        := get_address_index(i_addr, cs);
      data_in_line := get_data_set_index(i_addr, cs);
      return std_logic_vector(to_unsigned(index, cs.index_width)) &
        std_logic_vector(to_unsigned(data_in_line, cs.datas_per_line_width));
    end if;
  end function get_mem_addr;

begin  -- architecture str

  cds : for i in 0 to NB_WAYS - 1 generate
    cdata : sc_dualw_sram
      generic map (
        ADDR_WIDTH       => index_nb_bits + data_line_nb_bits,
        DATA_WIDTH       => DATA_WIDTH,
        WIDTH_WIDENER    => WIDTH_WIDENER,
        READ_UNDER_WRITE => false,
        LOOKAHEAD        => false,
        DEBUG_NAME       => "cdata_w" & integer'image(i),
        DEBUG            => DEBUG)
      port map (
        clock      => clk,
        wide       => wide,
        rren       => mem_rren(i),
        raddr      => mem_raddr,
        waddr      => mem_waddr,
        data_small => mem_write_data(i),
        data_wide  => mem_write_data_wide(i),
        wren       => mem_wren(i),
        q_small    => mem_read_data(i),
        q_wide     => mem_read_data_wide(i));
  end generate cds;

  mem_raddr <= get_mem_addr(i_raddr);
  mem_waddr <= get_mem_addr(i_waddr);

  mem_rrens : for way in mem_rren'range generate
    mem_rren(way) <= i_re;
  end generate mem_rrens;

  mem_wren <= i_wway;

  mem_read_datas : process(mem_read_data, mem_read_data_wide)
  begin
    for i in mem_read_data'range loop
      assign_row(mem_read_data_mux, mem_read_data(i), i);
    end loop;

    for i in mem_read_data_wide'range loop
      assign_row(mem_read_data_wide_mux, mem_read_data_wide(i), i);
    end loop;
  end process mem_read_datas;

  o_rdata_mux : slv_2d_mux
    generic map (NB => NB_WAYS, WIDTH => o_rdata'length)
    port map (mem_read_data_mux, i_rway, o_rdata);

  o_rdata_wide_mux : slv_2d_mux
    generic map (NB => NB_WAYS, WIDTH => o_rdata_wide'length)
    port map (mem_read_data_wide_mux, i_rway, o_rdata_wide);

  memwdatas : for way in mem_write_data'range(1) generate
    mem_write_data(way)      <= i_wdata;
    mem_write_data_wide(way) <= i_wdata_wide;
  end generate;

end architecture str;
