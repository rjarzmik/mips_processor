-------------------------------------------------------------------------------
-- Title      : Testbench for design "acache"
-- Project    : Cache
-------------------------------------------------------------------------------
-- File       : acache_tb.vhd
-- Author     : Robert Jarzmik <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2017-02-01
-- Last update: 2017-02-14
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
-- It is supposed that :
--   NB_LINES = 2
--   DATAS_PER_LINE = 2
--   NB_WAY = 2
-------------------------------------------------------------------------------
-- Copyright (c) 2017
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2017-02-01  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cache_defs.all;

entity acache_tb is
end entity acache_tb;

architecture test of acache_tb is

  -- component generics
  constant ADDR_WIDTH       : positive := 32;
  constant DATA_WIDTH       : positive := 32;
  constant DATAS_PER_LINE   : positive := 2;
  constant NB_WAYS          : positive := 2;
  constant CACHE_SIZE_BYTES : positive := 32;
  constant LOWER_DATA_WIDTH : positive := 32;
  constant WRITE_BACK       : boolean  := true;
  constant MEMORY_LATENCY   : integer  := 3;
  constant DEBUG            : boolean  := true;

  constant cline_refills : natural := DATAS_PER_LINE / (LOWER_DATA_WIDTH / DATA_WIDTH);
  constant cline_flushes : natural := DATAS_PER_LINE / (LOWER_DATA_WIDTH / DATA_WIDTH);

  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  subtype data_t is std_logic_vector(DATA_WIDTH - 1 downto 0);

  -- component ports
  signal clk                : std_logic := '1';
  signal clkena             : std_logic := '1';
  signal rst                : std_logic := '1';
  signal i_req              : std_logic := '0';
  signal i_wen              : std_logic := '0';
  signal i_addr             : addr_t;
  signal i_wdata            : data_t;
  signal i_do_write_through : std_logic;
  signal o_rdata            : data_t;
  signal o_rdata_valid      : std_logic;
  signal o_wready           : std_logic;
  signal o_memory_req       : std_logic;
  signal o_memory_we        : std_logic;
  signal o_memory_addr      : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal o_memory_wdata     : std_logic_vector(LOWER_DATA_WIDTH - 1 downto 0);
  signal i_memory_rdata     : std_logic_vector(LOWER_DATA_WIDTH - 1 downto 0);
  signal i_memory_done      : std_logic;
  signal o_dbg_cstats       : cache_stats_t;

  signal porta_req        : std_logic := '0';
  signal porta_we         : std_logic;
  signal porta_addr       : addr_t    := (others => '0');
  signal porta_wthrough   : std_logic;
  signal porta_write_data : data_t;

  signal test_num    : natural               := 1;
  signal test_action : unsigned(31 downto 0) := (others => 'Z');
  type requestor_t is record
    req                    : std_logic;
    we                     : std_logic;
    wthrough               : std_logic;
    addr                   : addr_t;
    data                   : data_t;
    expected_valid_latency : signed(7 downto 0);  -- if < 0, no expectation
    expected_data          : data_t;
  end record;
  signal requestor : requestor_t :=
    (req                    => '0', we => '0', wthrough => '0',
     addr                   => (others => '0'), data => (others => 'X'),
     expected_valid_latency => (others => '1'), expected_data => (others => 'X'));

  procedure report_cache_stats(stats : cache_stats_t) is
  begin
    report "Cache stats";
    report "  read  : hits=" & integer'image(stats.read_hits) &
      " misses=" & integer'image(stats.read_misses);
    report "  write : hits=" & integer'image(stats.write_hits) &
      " misses=" & integer'image(stats.write_misses) & " |" &
      " wbacks=" & integer'image(stats.write_backs) &
      " wthrou=" & integer'image(stats.write_throughs);
    report "  outer : flushes=" & integer'image(stats.flushes) & " " &
      "refills=" & integer'image(stats.refills);
  end procedure report_cache_stats;

  function testname(num : natural) return string is
  begin
    case num is
      when 1      => return "read_nominal";
      when 2      => return "read_exhaust_ways";
      when 3      => return "write_nominal";
      when 4      => return "write_exhaust_ways";
      when 5      => return "write_and_reread";
      when 6      => return "writethrough_and_reread";
      when others => return "unknown";
    end case;
  end function testname;

  function test_report_header(num_test : natural; num_action : unsigned; clk_after_req : natural)
    return string is
  begin
    return "[" & testname(num_test) & ":" & integer'image(to_integer(num_action)) & "]" &
      "[T.req + " & integer'image(clk_after_req) & "] ";
  end function test_report_header;

  -------------------------------------
  -- Debug note : cycles estimations --
  -------------------------------------
  --
  -- Search cycles
  --- Searched             : 1 cycle

  -- Flush cycles
  --- Prepare flushing     : 1 + nb_to_flush cycles
  --- Flush outer          : 1 cycle
  --- Flushing             : MEMORY_LATENCY * nb_to_flush + 2 cycles
  --- TOTAL                : 4 + nb_to_flush + nb_to_flush * MEMORY_LATENCY
  --
  -- Refill cycles
  --- Refill Memory        : MEMORY_LATENCY * nb_to_refill + 5 cycles
  --- Refill Cache         : nb_to_refill + 2
  --- TOTAL                : 7 + nb_to_refill * MEMORY_LATENCY

  -- Write Allocate cycles : 1 cycle

  type latency_kind is (read_hit, write_hit, write_hit_through,
                        read_miss, write_miss, write_miss_through);

  function estimate_latency(lk : latency_kind; nb_refill : natural; nb_flush : natural)
    return natural is
    variable lat                  : natural := 0;
    constant refill_const_penalty : natural := 8;
    constant flush_const_penalty  : natural := 6;
  begin
    lat := lat + 1;                     -- searched
    case lk is
      when read_hit =>
      when write_hit =>
        lat := lat + 1;                 -- write allocate
      when read_miss =>
        lat := lat + refill_const_penalty + nb_refill * MEMORY_LATENCY;  -- refill
        if nb_flush > 0 then
          lat := lat + flush_const_penalty + nb_flush * MEMORY_LATENCY;  -- flush
        end if;
      when write_miss =>
        lat := lat + refill_const_penalty + nb_refill * MEMORY_LATENCY;  -- refill
        if nb_flush > 0 then
          lat := lat + flush_const_penalty + nb_flush * MEMORY_LATENCY;  -- flush
        end if;
      -- lat := lat + 1;                 -- write allocate
      when write_hit_through =>
        lat := lat + 1;                 -- write allocate
        lat := lat + 2 + 1 * MEMORY_LATENCY;  -- write through
      when write_miss_through =>
        lat := lat + refill_const_penalty + nb_refill * MEMORY_LATENCY;  -- refill
        if nb_flush > 0 then
          lat := lat + flush_const_penalty + nb_flush * MEMORY_LATENCY;  -- flush
        end if;
        lat := lat + 2 + 1 * MEMORY_LATENCY;  -- write through
    end case;
    return lat;
  end function estimate_latency;

  procedure request_read(addr                   : in  natural;
                         expected_valid_latency : in  integer;
                         expected_data          : in  natural;
                         signal requestor       : out requestor_t) is
  begin
    requestor.req                    <= '1';
    requestor.we                     <= '0';
    requestor.addr                   <= std_logic_vector(to_unsigned(addr, ADDR_WIDTH));
    requestor.expected_valid_latency <= to_signed(expected_valid_latency, requestor.expected_valid_latency'length);
    requestor.expected_data          <= std_logic_vector(to_unsigned(expected_data, DATA_WIDTH));
    requestor.wthrough               <= '0';
  end procedure request_read;

  procedure request_write(addr                   : in  natural;
                          data                   : in  natural;
                          expected_valid_latency : in  integer;
                          write_through          : in  boolean;
                          signal requestor       : out requestor_t) is
  begin
    requestor.req                    <= '1';
    requestor.we                     <= '1';
    requestor.addr                   <= std_logic_vector(to_unsigned(addr, ADDR_WIDTH));
    requestor.data                   <= std_logic_vector(to_unsigned(data, DATA_WIDTH));
    requestor.expected_valid_latency <= to_signed(expected_valid_latency, requestor.expected_valid_latency'length);
    requestor.expected_data          <= (others => 'X');
    if write_through then
      requestor.wthrough <= '1';
    else
      requestor.wthrough <= '0';
    end if;
  end procedure request_write;

begin  -- architecture test

  -- component instantiation
  DUT : entity work.acache
    generic map (ADDR_WIDTH       => ADDR_WIDTH,
                 DATA_WIDTH       => DATA_WIDTH,
                 DATAS_PER_LINE   => DATAS_PER_LINE,
                 NB_WAYS          => NB_WAYS,
                 CACHE_SIZE_BYTES => CACHE_SIZE_BYTES,
                 LOWER_DATA_WIDTH => LOWER_DATA_WIDTH,
                 WRITE_BACK       => WRITE_BACK,
                 DEBUG            => DEBUG)
    port map (
      clk                => clk,
      rst                => rst,
      i_req              => i_req,
      i_wen              => i_wen,
      i_addr             => i_addr,
      i_wdata            => i_wdata,
      i_do_write_through => i_do_write_through,
      o_rdata            => o_rdata,
      o_rdata_valid      => o_rdata_valid,
      o_wready           => o_wready,
      o_memory_req       => o_memory_req,
      o_memory_we        => o_memory_we,
      o_memory_addr      => o_memory_addr,
      o_memory_wdata     => o_memory_wdata,
      i_memory_rdata     => i_memory_rdata,
      i_memory_done      => i_memory_done,
      o_dbg_cstats       => o_dbg_cstats);

  -- reset
  rst <= '0'                  after 12 ps;
  -- clock generation
  clk <= (clkena and not clk) after 5 ps;

  requestor_handler : process(clk, requestor, o_rdata_valid, o_rdata, o_wready)
    variable queried_addr            : addr_t;
    variable queried_we              : std_ulogic;
    variable expecting_valid         : integer := -1;
    variable expecting_valid_latency : integer := -1;
    variable expecting_data          : data_t;
    variable clk_after_req           : integer := -1;
  begin
    if rising_edge(clk) and clk_after_req >= -1 then
      clk_after_req := clk_after_req + 1;
    end if;

    if rising_edge(clk) and expecting_valid > 0 then
      expecting_valid := expecting_valid - 1;
    end if;

    if falling_edge(clk) then

      --report "RJK: T+" & integer'image(clk_after_req) &
      --  " expecting_valid=" & integer'image(expecting_valid) &
      --  " requestor.req = " & std_logic'image(requestor.req);

      if clk_after_req > 0 and expecting_valid >= 0 then
        if queried_we = '0' and o_rdata_valid = '1' and expecting_valid > 0 then
          report test_report_header(test_num, test_action, clk_after_req) &
            "Unexpected cache read response too early, remaining " &
            integer'image(expecting_valid) & " cycles, " &
            integer'image(clk_after_req) & " cycles after request" severity error;
        end if;

        if queried_we = '1' and o_wready = '1' and expecting_valid > 0 then
          report test_report_header(test_num, test_action, clk_after_req) &
            "Unexpected write read response too early, remaining " &
            integer'image(expecting_valid) & " cycles, " &
            integer'image(clk_after_req) & " cycles after request" severity error;
        end if;

        if queried_we = '0' and o_rdata_valid = '1' and expecting_valid = 0 then
          if expecting_data = o_rdata then
            report test_report_header(test_num, test_action - 1, clk_after_req) &
              "Cache responded as expected @" & to_hstring(queried_addr) &
              " => " & to_hstring(expecting_data) severity note;
            clk_after_req := -2;
          else
            report test_report_header(test_num, test_action - 1, clk_after_req) &
              "Cache responded wrong data in expected cycle @" & to_hstring(queried_addr) &
              " => " & to_hstring(o_rdata) & " while expecting " &
              to_hstring(expecting_data) severity error;
          end if;
          clk_after_req := -2;
        end if;

        if queried_we = '1' and o_wready = '1' and expecting_valid = 0 then
          report test_report_header(test_num, test_action - 1, clk_after_req) &
            "Cache accepted write as expected @" & to_hstring(queried_addr) &
            " <= " & to_hstring(porta_write_data) severity note;
          clk_after_req := -2;
        end if;

        if ((queried_we = '0' and o_rdata_valid = '0') or
            (queried_we = '1' and o_wready = '0')) and expecting_valid = 0 then
          report test_report_header(test_num, test_action - 1, clk_after_req) &
            "Cache didn't respond in time @" & to_hstring(queried_addr) &
            ", had to be in " &
            integer'image(expecting_valid_latency) &
            " cycles" severity error;
          clk_after_req := -2;
        end if;
      end if;

      if requestor.req = '1' then
        clk_after_req           := 0;
        queried_addr            := requestor.addr;
        queried_we              := requestor.we;
        expecting_valid         := to_integer(requestor.expected_valid_latency);
        expecting_valid_latency := expecting_valid;
        expecting_data          := requestor.expected_data;
        porta_req               <= '1';
        porta_addr              <= requestor.addr;
        porta_we         <= requestor.we;
        porta_write_data <= requestor.data;
        porta_wthrough   <= requestor.wthrough;
      end if;

      if requestor.req = '1' then
      elsif clk_after_req >= 0 then
        porta_req <= '0';
      else
        porta_req <= '0';
      end if;

    end if;
  end process requestor_handler;

  read_nominal_test : process(clk)
    variable nb_clk          : natural  := 0;
    variable next_clk_action : natural  := 0;
    variable update_action   : integer  := -1;
    variable action          : natural  := 0;
    variable next_action     : positive := 1;
    variable action_latency  : natural;
  begin
    if rising_edge(clk) and test_num = 1 then
      if update_action >= 0 then
        action        := update_action;
        update_action := -1;
      end if;

      if nb_clk = next_clk_action then
        update_action := next_action;
        test_action   <= to_unsigned(next_action, test_action'length);
        case next_action is
          when 1 =>
            action_latency := estimate_latency(read_miss, cline_refills, 0);
            request_read(16#0004#, action_latency, 16#0104#, requestor);
          when 2 =>
            action_latency := estimate_latency(read_miss, cline_refills, 0);
            request_read(16#0008#, action_latency, 16#0108#, requestor);
          when 3 =>
            action_latency := estimate_latency(read_hit, cline_refills, 0);
            request_read(16#000c#, action_latency, 16#010c#, requestor);
          when 4 =>
            action_latency := estimate_latency(read_miss, cline_refills, 0);
            request_read(16#0010#, action_latency, 16#0110#, requestor);
          when 5 =>
            action_latency := estimate_latency(read_hit, cline_refills, 0);
            request_read(16#0014#, action_latency, 16#0114#, requestor);
          when 6 =>
            action_latency := estimate_latency(read_hit, 0, 0);
            request_read(16#0004#, action_latency, 16#0104#, requestor);
          when 7 =>
            -- ... then switch to a possible request.
            action_latency := estimate_latency(read_hit, 0, 0);
            request_read(16#0004#, action_latency, 16#0104#, requestor);
          when others =>
            requestor.req <= '0';
        end case;
        next_action     := next_action + 1;
        next_clk_action := nb_clk + action_latency;
      else
        requestor.req <= '0';
      end if;

      nb_clk := nb_clk + 1;
    elsif test_num /= 1 then
      requestor.req                    <= 'Z';
      requestor.we                     <= 'Z';
      requestor.addr                   <= (others => 'Z');
      requestor.data                   <= (others => 'Z');
      requestor.expected_valid_latency <= (others => 'Z');
      requestor.expected_data          <= (others => 'Z');
      requestor.wthrough               <= 'Z';
      test_action                      <= (others => 'Z');
    end if;
  end process read_nominal_test;

  read_exhaust_ways : process(clk)
    variable nb_clk          : natural  := 0;
    variable next_clk_action : natural  := 0;
    variable update_action   : integer  := -1;
    variable action          : natural  := 0;
    variable next_action     : positive := 1;
    variable action_latency  : natural;
  begin
    if rising_edge(clk) and test_num = 2 then
      if update_action >= 0 then
        action        := update_action;
        update_action := -1;
      end if;

      if nb_clk = next_clk_action then
        update_action := next_action;
        test_action   <= to_unsigned(next_action, test_action'length);
        case next_action is
          when 1 =>
            action_latency := estimate_latency(read_miss, cline_refills, 0);
            request_read(16#0024#, action_latency, 16#0124#, requestor);
          when 2 =>
            action_latency := estimate_latency(read_miss, cline_refills, 0);
            request_read(16#0034#, action_latency, 16#0134#, requestor);
          when 3 =>
            action_latency := estimate_latency(read_miss, cline_refills, 0);
            request_read(16#0044#, action_latency, 16#0144#, requestor);
          when 4 =>
            action_latency := estimate_latency(read_miss, cline_refills, 0);
            request_read(16#0024#, action_latency, 16#0124#, requestor);
          when 5 =>
            -- Way should still contain this data, as previous flushed @0034
            action_latency := estimate_latency(read_hit, 0, 0);
            request_read(16#0044#, action_latency, 16#0144#, requestor);
          when 6 =>
            action_latency := estimate_latency(read_hit, 0, 0);
            request_read(16#0044#, action_latency, 16#0144#, requestor);
          when 7 =>
            action_latency := estimate_latency(read_hit, 0, 0);
            request_read(16#0024#, action_latency, 16#0124#, requestor);
          when others =>
            requestor.req <= '0';
        end case;
        next_action     := next_action + 1;
        next_clk_action := nb_clk + action_latency;
      else
        requestor.req <= '0';
      end if;

      nb_clk := nb_clk + 1;
    elsif test_num /= 2 then
      requestor.req                    <= 'Z';
      requestor.we                     <= 'Z';
      requestor.addr                   <= (others => 'Z');
      requestor.data                   <= (others => 'Z');
      requestor.expected_valid_latency <= (others => 'Z');
      requestor.expected_data          <= (others => 'Z');
      requestor.wthrough               <= 'Z';
      test_action                      <= (others => 'Z');
    end if;
  end process read_exhaust_ways;

  write_nominal_test : process(clk)
    variable nb_clk          : natural  := 0;
    variable next_clk_action : natural  := 0;
    variable update_action   : integer  := -1;
    variable action          : natural  := 0;
    variable next_action     : positive := 1;
    variable action_latency  : natural;
  begin
    if rising_edge(clk) and test_num = 3 then
      if update_action >= 0 then
        action        := update_action;
        update_action := -1;
      end if;

      if nb_clk = next_clk_action then
        update_action := next_action;
        test_action   <= to_unsigned(next_action, test_action'length);
        case next_action is
          when 1 =>
            action_latency := estimate_latency(write_miss, cline_refills, 0);
            request_write(16#0004#, 16#0204#, action_latency, false, requestor);
          when 2 =>
            -- read_nominal_test:2 has filled this entry which is still valid
            action_latency := estimate_latency(write_hit, cline_refills, 0);
            request_write(16#0008#, 16#0208#, action_latency, false, requestor);
          when 3 =>
            action_latency := estimate_latency(write_hit, 0, 0);
            request_write(16#000c#, 16#020c#, action_latency, false, requestor);
          when 4 =>
            action_latency := estimate_latency(write_miss, cline_refills, 0);
            request_write(16#0010#, 16#0210#, action_latency, false, requestor);
          when 5 =>
            action_latency := estimate_latency(write_hit, 0, 0);
            request_write(16#0014#, 16#0214#, action_latency, false, requestor);
          when 6 =>
            action_latency := estimate_latency(write_hit, 0, 0);
            request_write(16#0004#, 16#0204#, action_latency, false, requestor);
          when 7 =>
            action_latency := estimate_latency(write_hit, 0, 0);
            request_write(16#0004#, 16#0204#, action_latency, false, requestor);
          when others =>
            requestor.req <= '0';
        end case;
        next_action     := next_action + 1;
        next_clk_action := nb_clk + action_latency;
      else
        requestor.req <= '0';
      end if;

      nb_clk := nb_clk + 1;
    elsif test_num /= 3 then
      requestor.req                    <= 'Z';
      requestor.we                     <= 'Z';
      requestor.addr                   <= (others => 'Z');
      requestor.data                   <= (others => 'Z');
      requestor.expected_valid_latency <= (others => 'Z');
      requestor.expected_data          <= (others => 'Z');
      requestor.wthrough               <= 'Z';
      test_action                      <= (others => 'Z');
    end if;
  end process write_nominal_test;

  write_exhaust_ways : process(clk)
    variable nb_clk          : natural  := 0;
    variable next_clk_action : natural  := 0;
    variable update_action   : integer  := -1;
    variable action          : natural  := 0;
    variable next_action     : positive := 1;
    variable action_latency  : natural;
  begin
    if rising_edge(clk) and test_num = 4 then
      if update_action >= 0 then
        action        := update_action;
        update_action := -1;
      end if;

      if nb_clk = next_clk_action then
        update_action := next_action;
        test_action   <= to_unsigned(next_action, test_action'length);
        case next_action is
          when 1 =>
            -- Way will be flushed as there is 1 dirty data in it
            action_latency := estimate_latency(write_miss, cline_refills, cline_flushes);
            request_write(16#0024#, 16#0224#, action_latency, false, requestor);
          when 2 =>
            -- Way will be flushed as there are 2 dirty data in it
            action_latency := estimate_latency(write_miss, cline_refills, cline_flushes);
            request_write(16#0034#, 16#0234#, action_latency, false, requestor);
          when 3 =>
            -- Way will be flushed as there are dirty data in them
            action_latency := estimate_latency(write_miss, cline_refills, cline_flushes);
            request_write(16#0044#, 16#0244#, action_latency, false, requestor);
          when 4 =>
            -- Way will be flushed as there are dirty data in them
            action_latency := estimate_latency(write_miss, cline_refills, cline_flushes);
            request_write(16#0024#, 16#0224#, action_latency, false, requestor);
          when 5 =>
            -- Way should still contain this data, and a write hit should occur
            action_latency := estimate_latency(write_hit, 0, 0);
            request_write(16#0044#, 16#0244#, action_latency, false, requestor);
          when 6 =>
            action_latency := estimate_latency(write_hit, 0, 0);
            request_write(16#0044#, 16#0244#, action_latency, false, requestor);
          when 7 =>
            action_latency := estimate_latency(write_hit, 0, 0);
            request_write(16#0024#, 16#0224#, action_latency, false, requestor);
          when 8 =>
            action_latency := estimate_latency(write_hit, 0, 0);
            request_write(16#0020#, 16#0220#, action_latency, false, requestor);
          when others =>
            requestor.req <= '0';
        end case;
        next_action     := next_action + 1;
        next_clk_action := nb_clk + action_latency;
      else
        requestor.req <= '0';
      end if;

      nb_clk := nb_clk + 1;
    elsif test_num /= 4 then
      requestor.req                    <= 'Z';
      requestor.we                     <= 'Z';
      requestor.addr                   <= (others => 'Z');
      requestor.data                   <= (others => 'Z');
      requestor.expected_valid_latency <= (others => 'Z');
      requestor.expected_data          <= (others => 'Z');
      requestor.wthrough               <= 'Z';
      test_action                      <= (others => 'Z');
    end if;
  end process write_exhaust_ways;

  write_and_reread : process(clk)
    variable nb_clk          : natural  := 0;
    variable next_clk_action : natural  := 0;
    variable update_action   : integer  := -1;
    variable action          : natural  := 0;
    variable next_action     : positive := 1;
    variable action_latency  : natural;
  begin
    if rising_edge(clk) and test_num = 5 then
      if update_action >= 0 then
        action        := update_action;
        update_action := -1;
      end if;

      if nb_clk = next_clk_action then
        update_action := next_action;
        test_action   <= to_unsigned(next_action, test_action'length);
        case next_action is
          when 1 =>
            action_latency := estimate_latency(write_hit, 0, 0);
            request_write(16#0020#, 16#0320#, action_latency, false, requestor);
          when 2 =>
            action_latency := estimate_latency(write_hit, 0, 0);
            request_write(16#0024#, 16#0324#, action_latency, false, requestor);
          when 3 =>
            action_latency := estimate_latency(write_miss, cline_refills, cline_flushes);
            request_write(16#0034#, 16#0334#, action_latency, false, requestor);
          when 4 =>
            action_latency := estimate_latency(read_hit, 0, 0);
            request_read(16#0024#, action_latency, 16#0324#, requestor);
          when 5 =>
            action_latency := estimate_latency(read_hit, 0, 0);
            request_read(16#0034#, action_latency, 16#0334#, requestor);
          when 6 =>
            -- Flush 0020, 0024, refill 0044
            action_latency := estimate_latency(read_miss, cline_refills, cline_flushes);
            request_read(16#0044#, action_latency, 16#0244#, requestor);
          when 7 =>
            action_latency := estimate_latency(write_hit, 0, 0);
            request_write(16#0044#, 16#0344#, action_latency, false, requestor);
          when 8 =>
            action_latency := estimate_latency(read_hit, 0, 0);
            request_read(16#0034#, action_latency, 16#0334#, requestor);
          when 9 =>
            action_latency := estimate_latency(write_hit, 0, 0);
            request_write(16#0034#, 16#0334#, action_latency, false, requestor);
          when others =>
            requestor.req <= '0';
        end case;
        next_action     := next_action + 1;
        next_clk_action := nb_clk + action_latency;
      else
        requestor.req <= '0';
      end if;

      nb_clk := nb_clk + 1;
    elsif test_num /= 5 then
      requestor.req                    <= 'Z';
      requestor.we                     <= 'Z';
      requestor.addr                   <= (others => 'Z');
      requestor.data                   <= (others => 'Z');
      requestor.expected_valid_latency <= (others => 'Z');
      requestor.expected_data          <= (others => 'Z');
      requestor.wthrough               <= 'Z';
      test_action                      <= (others => 'Z');
    end if;
  end process write_and_reread;

  writethrough_and_reread : process(clk)
    variable nb_clk          : natural  := 0;
    variable next_clk_action : natural  := 0;
    variable update_action   : integer  := -1;
    variable action          : natural  := 0;
    variable next_action     : positive := 1;
    variable action_latency  : natural;
  begin
    if rising_edge(clk) and test_num = 6 then
      if update_action >= 0 then
        action        := update_action;
        update_action := -1;
      end if;

      if nb_clk = next_clk_action then
        update_action := next_action;
        test_action   <= to_unsigned(next_action, test_action'length);
        case next_action is
          when 1 =>
            action_latency := estimate_latency(write_hit, 0, 0);
            request_write(16#0034#, 16#0434#, action_latency, false, requestor);
          when 2 =>
            action_latency := estimate_latency(write_hit, 0, 0);
            -- action_latency := estimate_latency(write_miss, 0, 1);
            request_write(16#0040#, 16#0440#, action_latency, false, requestor);
          when 3 =>
            action_latency := estimate_latency(write_hit, 0, 0);
            request_write(16#0044#, 16#0444#, action_latency, false, requestor);
          --
          -- Cache is dirtied on address 0034, 0040, 0044
          --
          when 4 =>
            action_latency := estimate_latency(write_hit_through, 0, 0);
            request_write(16#0044#, 16#0444#, action_latency, true, requestor);
          when 5 =>
            action_latency := estimate_latency(read_hit, 0, 0);
            request_read(16#0044#, action_latency, 16#0444#, requestor);
          when 6 =>
            action_latency := estimate_latency(read_hit, 0, 0);
            request_read(16#0034#, action_latency, 16#0434#, requestor);
          when 7 =>
            action_latency := estimate_latency(write_hit_through, 0, 0);
            request_write(16#0034#, 16#0434#, action_latency, true, requestor);
          when 8 =>
            -- Previous writethrough didn't remove the need for flushing (0034)
            action_latency := estimate_latency(read_miss, cline_refills, cline_flushes);
            request_read(16#0004#, action_latency, 16#0204#, requestor);
          when 9 =>
            -- As 0040 should still be dirty, there is a flush + refill (0040)
            action_latency := estimate_latency(read_miss, cline_refills, cline_flushes);
            request_read(16#0014#, action_latency, 16#0214#, requestor);
          when 10 =>
            action_latency := estimate_latency(write_hit, 0, 0);
            request_write(16#0000#, 16#0400#, action_latency, false, requestor);
          when 11 =>
            action_latency := estimate_latency(write_miss_through, cline_refills, cline_flushes);
            request_write(16#0040#, 16#0440#, action_latency, true, requestor);

          when 12 =>
            action_latency := estimate_latency(write_miss, cline_refills, 0);
            request_write(16#0050#, 16#0550#, action_latency, false, requestor);
          when 13 =>
            action_latency := estimate_latency(write_hit, 0, 0);
            request_write(16#0050#, 16#0550#, action_latency, false, requestor);
          when 14 =>
            action_latency := estimate_latency(write_hit, 0, 0);
            request_write(16#0050#, 16#0550#, action_latency, false, requestor);
          when 15 =>
            action_latency := estimate_latency(write_hit_through, 0, 0);
            request_write(16#0054#, 16#0554#, action_latency, true, requestor);
          when others =>
            requestor.req <= '0';
        end case;
        next_action     := next_action + 1;
        next_clk_action := nb_clk + action_latency;
      else
        requestor.req <= '0';
      end if;

      nb_clk := nb_clk + 1;
    elsif test_num /= 6 then
      requestor.req                    <= 'Z';
      requestor.we                     <= 'Z';
      requestor.addr                   <= (others => 'Z');
      requestor.data                   <= (others => 'Z');
      requestor.expected_valid_latency <= (others => 'Z');
      requestor.expected_data          <= (others => 'Z');
      requestor.wthrough               <= 'Z';
      test_action                      <= (others => 'Z');
    end if;
  end process writethrough_and_reread;

  reporter : process(clk, rst, i_req, i_wen, i_addr, i_wdata, o_rdata_valid, o_rdata)
    variable nb_clk        : natural := 0;
    constant clk_after_req : integer := 0;
  begin
    if rising_edge(clk) and clkena = '1' then
      if nb_clk = 0 then
        test_num <= 1;
        report_cache_stats(o_dbg_cstats);
      elsif nb_clk = 100 then
        test_num <= 2;
        report_cache_stats(o_dbg_cstats);
      elsif nb_clk = 200 then
        test_num <= 3;
        report_cache_stats(o_dbg_cstats);
      elsif nb_clk = 400 then
        test_num <= 4;
        report_cache_stats(o_dbg_cstats);
      elsif nb_clk = 600 then
        test_num <= 5;
        report_cache_stats(o_dbg_cstats);
      elsif nb_clk = 800 then
        test_num <= 6;
        report_cache_stats(o_dbg_cstats);
      elsif nb_clk > 1000 then
        test_num <= 7;
        report_cache_stats(o_dbg_cstats);
      end if;
      nb_clk := nb_clk + 1;

      if test_num = 7 then
        clkena <= '0';
        report "Ending simulation";
      end if;
    end if;

    if rising_edge(i_req) and i_wen = '0' then
      report test_report_header(test_num, test_action, clk_after_req) &
        "read request @" & to_hstring(i_addr);
    end if;

    if rising_edge(i_req) and i_wen = '1' then
      if i_do_write_through = '0' then
        report test_report_header(test_num, test_action, clk_after_req) &
          "write request @" & to_hstring(i_addr) & "<= " &
          to_hstring(i_wdata);
      else
        report test_report_header(test_num, test_action, clk_after_req) &
          "writethrough request @" & to_hstring(i_addr) & "<= " &
          to_hstring(i_wdata);
      end if;
    end if;
  end process reporter;

  -- memory simulator
  Simulated_Memory_1 : entity work.Simulated_Memory
    generic map (
      ADDR_WIDTH        => ADDR_WIDTH,
      DATA_WIDTH        => LOWER_DATA_WIDTH,
      MEMORY_ADDR_WIDTH => 16,
      MEMORY_LATENCY    => MEMORY_LATENCY,
      DEBUG             => DEBUG)
    port map (
      clk                 => clk,
      rst                 => rst,
      i_memory_req        => o_memory_req,
      i_memory_we         => o_memory_we,
      i_memory_addr       => o_memory_addr,
      i_memory_write_data => o_memory_wdata,
      o_memory_read_data  => i_memory_rdata,
      o_memory_valid      => i_memory_done);

  i_req              <= porta_req;
  i_wen              <= porta_we;
  i_addr             <= porta_addr;
  i_do_write_through <= porta_wthrough;
  i_wdata            <= porta_write_data;
end architecture test;

configuration acache_tb_test_cfg of acache_tb is
  for test
  end for;
end acache_tb_test_cfg;
