-------------------------------------------------------------------------------
-- Title      : Single input port associative cache
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : SinglePort_Associative_Cache.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-30
-- Last update: 2016-12-09
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

-------------------------------------------------------------------------------

entity SinglePort_Associative_Cache is

  generic (
    -- address width
    ADDR_WIDTH      : natural;
    -- single data width, ie. what will be returned on o_porta_read_data
    DATA_WIDTH      : natural;
    -- how many address-contiguous data are in a set, must be 2**n
    NB_DATA_PER_SET : natural;
    -- number of ways, must be 2**n.
    -- Regardless, the lower cache/memory interface is DATA_WIDTH wide and not
    -- DATA * NB_DATA_PER_SET wide
    NB_WAYS         : natural;
    -- number of sets, ie. number of indexes in the cache, must be 2**n
    NB_SETS         : natural
    );

  port (
    clk : in std_logic;
    rst : in std_logic;

    i_porta_req        : in  std_logic;
    i_porta_we         : in  std_logic;
    i_porta_addr       : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_porta_write_data : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_porta_read_data  : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_porta_valid      : out std_logic;

    o_memory_req        : out std_logic;
    o_memory_we         : out std_logic;
    o_memory_addr       : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_memory_write_data : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    i_memory_read_data  : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    i_memory_valid      : in  std_logic
    );

end entity SinglePort_Associative_Cache;

-------------------------------------------------------------------------------

architecture ways_N_associative of SinglePort_Associative_Cache is
  constant data_one_nb_bits  : natural  := integer(ceil(log2(real(DATA_WIDTH / 8))));
  constant data_line_nb_bits : natural  := integer(ceil(log2(real(NB_DATA_PER_SET))));
  constant data_set_nb_bits  : natural  := data_one_nb_bits + data_line_nb_bits;
  constant index_nb_bits     : positive := integer(ceil(log2(real(NB_SETS))));
  constant tag_nb_bits       : positive := ADDR_WIDTH - data_set_nb_bits - index_nb_bits;

  -- common types
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  subtype data_t is std_logic_vector(DATA_WIDTH - 1 downto 0);

  -- cache data
  type cache_data_t is array(0 to NB_DATA_PER_SET - 1) of
    std_logic_vector(DATA_WIDTH - 1 downto 0);
  type cache_data_valid_t is array(0 to NB_DATA_PER_SET - 1) of std_logic;

  type cache_line_t is record
    valids : cache_data_valid_t;
    tag    : unsigned(tag_nb_bits - 1 downto 0);
    data   : cache_data_t;
  end record;

  type cache_ways_t is array(0 to NB_WAYS - 1) of cache_line_t;
  type cache_n_way_t is record
    ways         : cache_ways_t;
    next_flushed : natural range 0 to NB_WAYS - 1;
  end record;

  type cache_mem_t is array(0 to 2**index_nb_bits - 1) of cache_n_way_t;

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  signal porta_requested_addr : addr_t;

  -- Signals reporting in real-time if i_porta_addr is found in the cache
  signal porta_is_cache_hit                 : boolean;
  signal porta_data_cache_hit               : data_t;
  -- Signals reporting in real-time if memory_last_requested_addr is found in the cache
  signal memory_last_requested_is_cache_hit : boolean;
  signal memory_never_requested             : boolean := true;
  signal porta_never_requested              : boolean := true;

  constant cache_line_zero : cache_line_t := (
    valids => (others => '0'),
    tag    => (others => '0'),
    data   => (others => (others => '0'))
    );
  constant cache_n_way_zero : cache_n_way_t := (
    ways         => (others => cache_line_zero),
    next_flushed => 0
    );
  signal memory : cache_mem_t :=
    (others => cache_n_way_zero);

  -- outgoing memory transaction
  signal outgoing_mem_addr : addr_t;

  -- need_memory_req: in this cycle, o_memory_req should be asserted
  signal need_memory_req            : boolean := false;
  -- address for which a o_memory_valid answers
  --   => this is not necessarily outgoing_mem_addr as a new request might be
  --      queued in the same cycle as o_memory_valid = '1'
  signal memory_last_requested_addr : addr_t;


  -----------------------------------------------------------------------------
  -- Internal functions declarations
  -----------------------------------------------------------------------------
  function get_address_index(
    i_address : in addr_t) return natural is
    variable idx : natural;
  begin
    idx := to_integer(unsigned(i_address(
      index_nb_bits + data_set_nb_bits - 1 downto data_set_nb_bits)));
    return idx;
  end function get_address_index;

  function get_address_tag(i_address : in addr_t)
    return unsigned is
    variable tag : unsigned(tag_nb_bits - 1 downto 0);
  begin
    tag := unsigned(i_address(ADDR_WIDTH - 1 downto ADDR_WIDTH - tag_nb_bits));
    return tag;
  end function get_address_tag;

  function get_data_set_index(i_address : in addr_t)
    return natural is
    variable set_index : natural;
  begin
    if data_set_nb_bits = data_one_nb_bits then
      set_index := 0;
    else
      set_index := to_integer(unsigned(i_address(data_set_nb_bits - 1 downto data_one_nb_bits)));
    end if;
    return set_index;
  end function get_data_set_index;

  function is_cache_hit(i_address     : in addr_t;
                        i_cache_lines : in cache_mem_t) return boolean is
    variable ways      : cache_ways_t;
    variable tag       : unsigned(tag_nb_bits - 1 downto 0);
    variable found     : boolean := false;
    variable set_index : natural range 0 to NB_DATA_PER_SET - 1;
  begin
    tag       := get_address_tag(i_address);
    ways      := i_cache_lines(get_address_index(i_address)).ways;
    set_index := get_data_set_index(i_address);
    for i in ways'range loop
      found := found or (ways(i).tag = tag and ways(i).valids(set_index) = '1');
    end loop;
    return found;
  end;

  function data_cache_hit(i_address     : in addr_t;
                          i_cache_lines : in cache_mem_t) return data_t is
    variable ways      : cache_ways_t;
    variable tag       : unsigned(tag_nb_bits - 1 downto 0);
    variable found     : data_t := (others => '0');
    variable set_index : natural range 0 to NB_DATA_PER_SET - 1;
  begin
    tag       := get_address_tag(i_address);
    ways      := i_cache_lines(get_address_index(i_address)).ways;
    set_index := get_data_set_index(i_address);
    for i in ways'range loop
      if ways(i).tag = tag and ways(i).valids(set_index) = '1' then
        found := found or ways(i).data(set_index);
      end if;
    end loop;
    return found;
  end;

begin  -- architecture ways_N_associative

  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------

  porta_is_cache_hit <= false when rst = '1' or porta_never_requested else
                        is_cache_hit(porta_requested_addr, memory);
  porta_data_cache_hit <= (others => 'X') when rst = '1' else
                          data_cache_hit(porta_requested_addr, memory);

  memory_last_requested_is_cache_hit <=
    false when memory_never_requested or rst = '1' else
    is_cache_hit(memory_last_requested_addr, memory);

  o_porta_valid     <= '1'                  when porta_is_cache_hit else '0';
  o_porta_read_data <= porta_data_cache_hit when porta_is_cache_hit else (others => 'X');
  o_memory_req      <= '1'                  when need_memory_req    else '0';

  memory_tracker : process(clk, rst, need_memory_req)
  begin
    if rst = '1' then
      memory_never_requested     <= true;
      memory_last_requested_addr <= (others => '0');
    elsif rst = '0' and rising_edge(clk) then
      if need_memory_req then
        memory_last_requested_addr <= outgoing_mem_addr;
        memory_never_requested     <= false;
      end if;
    end if;
  end process memory_tracker;

  memory_request : process(clk, rst, i_porta_req)
  begin
    if rst = '1' then
      need_memory_req       <= false;
      o_memory_addr         <= (others => '0');
      o_memory_we           <= '0';
      o_memory_write_data   <= (others => 'X');
      porta_requested_addr  <= (others => '0');
      porta_never_requested <= true;
    elsif rising_edge(clk) then
      if i_porta_req = '1' and
        (not porta_is_cache_hit or i_porta_addr /= porta_requested_addr) then
        outgoing_mem_addr     <= i_porta_addr;
        porta_never_requested <= false;
        porta_requested_addr  <= i_porta_addr;
        need_memory_req       <= true;
        o_memory_addr         <= i_porta_addr;
        o_memory_we           <= i_porta_we;
        o_memory_write_data   <= i_porta_write_data;
      else
        need_memory_req   <= false;
        outgoing_mem_addr <= (others => 'X');
      end if;
    end if;
  end process memory_request;

  memory_completed : process(clk, i_memory_valid)
    variable index       : natural;
    variable flushed_way : natural;
    variable set_index   : natural range 0 to NB_DATA_PER_SET - 1;
  begin
    if rising_edge(clk) then
      if i_memory_valid = '1' and not memory_last_requested_is_cache_hit then
        index                      := get_address_index(memory_last_requested_addr);
        set_index                  := get_data_set_index(memory_last_requested_addr);
        -- calculated which way is used for new data
        flushed_way                := memory(index).next_flushed;
        memory(index).next_flushed <= ((memory(index).next_flushed + 1) mod NB_WAYS);

        memory(index).ways(flushed_way).valids(set_index) <= '1';
        memory(index).ways(flushed_way).tag               <= get_address_tag(memory_last_requested_addr);

        memory(index).ways(flushed_way).data(set_index) <= i_memory_read_data;
      end if;
    end if;
  end process memory_completed;

end architecture ways_N_associative;

-------------------------------------------------------------------------------
