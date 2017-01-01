-------------------------------------------------------------------------------
-- Title      : Testbench for design "SinglePort_Associative_Cache"
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : SinglePort_Associative_Cache_tb.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-30
-- Last update: 2016-12-30
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
-- It is supposed that :
--   NB_LINES = 2
--   DATAS_PER_LINE = 2
--   NB_WAY = 2
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

use work.cache_defs.all;

-------------------------------------------------------------------------------

entity SinglePort_Associative_Cache_tb is

end entity SinglePort_Associative_Cache_tb;

-------------------------------------------------------------------------------

architecture ways_N_associative of SinglePort_Associative_Cache_tb is

  -- component generics
  constant MEMORY_LATENCY : integer := 3;
  constant DEBUG          : boolean := true;

  -- component ports
  signal clk                      : std_logic                                 := '1';
  signal clkena                   : std_logic                                 := '1';
  signal rst                      : std_logic                                 := '1';
  signal i_porta_req              : std_logic                                 := '0';
  signal i_porta_we               : std_logic;
  signal i_porta_addr             : addr_t                                    := (others => '0');
  signal i_porta_do_write_through : std_logic;
  signal i_porta_write_data       : std_logic_vector(DATA_WIDTH - 1 downto 0) := (others => 'X');
  signal o_porta_read_data        : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal o_porta_valid            : std_logic;

  signal o_memory_req        : std_logic := '0';
  signal o_memory_we         : std_logic := '0';
  signal o_memory_addr       : addr_t;
  signal o_memory_write_data : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal i_memory_read_data  : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal i_memory_valid      : std_logic;
  signal o_dbg_state         : cache_state;
  signal o_dbg_cstats        : cache_stats_t;

  signal porta_req        : std_logic := '0';
  signal porta_we         : std_logic;
  signal porta_addr       : addr_t    := (others => '0');
  signal porta_wthrough   : std_logic;
  signal porta_write_data : data_t;

  signal cls_req     : cls_op;
  signal cls_creq    : cache_request_t;
  signal cls_cresp   : cache_response_t;
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

  function cache_state_name(c : cache_state) return string is
  begin
    case c is
      when s_idle             => return "idle";
      when s_searching        => return "searching";
      when s_prepare_flushing => return "prepare_flush";
      when s_flush_outer      => return "flush_outer";
      when s_flushing         => return "flushing";
      when s_refill_memory    => return "refill_memory";
      when s_refill_cache     => return "refill_cache";
      when s_writethrough     => return "writethrough";
      when s_write_allocate   => return "write_allocate";
    end case;
  end function cache_state_name;

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

  function test_report_header(num_test : natural; num_action : unsigned; clk_after_req : natural;
                              state    : cache_state)
    return string is
  begin
    return "[" & testname(num_test) & ":" & integer'image(to_integer(num_action)) & "]" &
      "[" & cache_state_name(state) & "]" &
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
  --- Refill Memory        : MEMORY_LATENCY * nb_to_refill + 3 cycles
  --- Refill Cache         : nb_to_refill + 1
  --- TOTAL                : 4 + nb_to_refill + nb_to_refill * MEMORY_LATENCY

  -- Write Allocate cycles : 1 cycle

  type latency_kind is (read_hit, write_hit, write_hit_through,
                        read_miss, write_miss, write_miss_through);

  function estimate_latency(lk : latency_kind; nb_refill : natural; nb_flush : natural)
    return natural is
    variable lat : natural := 0;
  begin
    lat := lat + 1;                     -- searched
    case lk is
      when read_hit =>
      when write_hit =>
        lat := lat + 1;                 -- write allocate
      when read_miss =>
        lat := lat + 4 + nb_refill + nb_refill * MEMORY_LATENCY;  -- refill
        if nb_flush > 0 then
          lat := lat + 4 + nb_flush + nb_flush * MEMORY_LATENCY;  -- flush
        end if;
      when write_miss =>
        if nb_flush > 0 then
          lat := lat + 4 + nb_flush + nb_flush * MEMORY_LATENCY;  -- flush
        end if;
        lat := lat + 1;                 -- write allocate
      when write_hit_through =>
        lat := lat + 1;                 -- write allocate
        lat := lat + 1 + 1 + 1 * MEMORY_LATENCY;  -- write through
      when write_miss_through =>
        if nb_flush > 0 then
          lat := lat + 4 + nb_flush + nb_flush * MEMORY_LATENCY;  -- flush
        end if;
        lat := lat + 1;                 -- write allocate
        lat := lat + 1 + 1 + 1 * MEMORY_LATENCY;  -- write through
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

begin  -- architecture ways_N_associative

  -- component instantiation
  DUT : entity work.SinglePort_Associative_Cache(rtl)
    generic map (DEBUG => DEBUG)
    port map (
      clk                      => clk,
      rst                      => rst,
      i_porta_req              => i_porta_req,
      i_porta_we               => i_porta_we,
      i_porta_addr             => i_porta_addr,
      i_porta_do_write_through => i_porta_do_write_through,
      i_porta_write_data       => i_porta_write_data,
      o_porta_read_data        => o_porta_read_data,
      o_porta_valid            => o_porta_valid,
      -- Carry-over signals
      o_creq                   => cls_creq,
      i_cresp                  => cls_cresp,
      -- Debug
      o_dbg_state              => o_dbg_state,
      o_dbg_cstats             => o_dbg_cstats);

  -- reset
  rst <= '0'                  after 12 ps;
  -- clock generation
  clk <= (clkena and not clk) after 5 ps;

  requestor_handler : process(clk, requestor, o_porta_valid, o_porta_read_data)
    variable queried_addr    : addr_t;
    variable queried_we      : std_ulogic;
    variable expecting_valid : integer := -1;
    variable expecting_data  : data_t;
    variable clk_after_req   : integer := -1;
  begin
    if falling_edge(clk) then
      if clk_after_req >= 0 then
        clk_after_req := clk_after_req + 1;
      end if;
      if expecting_valid > 0 then
        expecting_valid := expecting_valid - 1;
      end if;

      if clk_after_req > 0 and expecting_valid >= 0 then
        if o_porta_valid = '1' and expecting_valid > 0 then
          report test_report_header(test_num, test_action, clk_after_req, o_dbg_state) &
            "Unexpected cache response too early, remaining " &
            integer'image(expecting_valid) & " cycles, " &
            integer'image(clk_after_req) & " cycles after request" severity error;
        end if;

        if o_porta_valid = '1' and expecting_valid = 0 then
          if queried_we = '0' then
            if expecting_data = o_porta_read_data then
              report test_report_header(test_num, test_action, clk_after_req, o_dbg_state) &
                "Cache responded as expected @" & to_hstring(queried_addr) &
                " => " & to_hstring(expecting_data) severity note;
              clk_after_req := -1;
            else
              report test_report_header(test_num, test_action, clk_after_req, o_dbg_state) &
                "Cache responded wrong data in expected cycle @" & to_hstring(queried_addr) &
                " => " & to_hstring(o_porta_read_data) & " while expecting " &
                to_hstring(expecting_data) severity error;
            end if;
            clk_after_req := -1;
          end if;

          if queried_we = '1' then
            report test_report_header(test_num, test_action, clk_after_req, o_dbg_state) &
              "Cache accepted write as expected @" & to_hstring(queried_addr) &
              " <= " & to_hstring(porta_write_data) severity note;
            clk_after_req := -1;
          end if;
        end if;

        if o_porta_valid = '0' and expecting_valid = 0 then
          report test_report_header(test_num, test_action, clk_after_req, o_dbg_state) &
            "Cache didn't respond in time @" severity error;
          clk_after_req := -1;
        end if;
      end if;

      if requestor.req = '1' then
        clk_after_req    := 0;
        queried_addr     := requestor.addr;
        queried_we       := requestor.we;
        expecting_valid  := to_integer(requestor.expected_valid_latency);
        expecting_data   := requestor.expected_data;
        porta_req        <= '1';
        porta_addr       <= requestor.addr;
        porta_we         <= requestor.we;
        porta_write_data <= requestor.data;
        porta_wthrough   <= requestor.wthrough;
      end if;

      if requestor.req = '1' then
      elsif clk_after_req > 0 then
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
            action_latency := estimate_latency(read_miss, 1, 0);
            request_read(16#0004#, action_latency, 16#0104#, requestor);
          when 2 =>
            action_latency := estimate_latency(read_miss, 1, 0);
            request_read(16#0008#, action_latency, 16#0108#, requestor);
          when 3 =>
            action_latency := estimate_latency(read_miss, 1, 0);
            request_read(16#000c#, action_latency, 16#010c#, requestor);
          when 4 =>
            action_latency := estimate_latency(read_miss, 1, 0);
            request_read(16#0010#, action_latency, 16#0110#, requestor);
          when 5 =>
            action_latency := estimate_latency(read_miss, 1, 0);
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
            action_latency := estimate_latency(read_miss, 1, 0);
            request_read(16#0024#, action_latency, 16#0124#, requestor);
          when 2 =>
            action_latency := estimate_latency(read_miss, 1, 0);
            request_read(16#0034#, action_latency, 16#0134#, requestor);
          when 3 =>
            action_latency := estimate_latency(read_miss, 1, 0);
            request_read(16#0044#, action_latency, 16#0144#, requestor);
          when 4 =>
            action_latency := estimate_latency(read_miss, 1, 0);
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
            action_latency := estimate_latency(write_hit, 0, 0);
            request_write(16#0004#, 16#0204#, action_latency, false, requestor);
          when 2 =>
            action_latency := estimate_latency(write_hit, 0, 0);
            request_write(16#0008#, 16#0208#, action_latency, false, requestor);
          when 3 =>
            action_latency := estimate_latency(write_hit, 0, 0);
            request_write(16#000c#, 16#020c#, action_latency, false, requestor);
          when 4 =>
            action_latency := estimate_latency(write_hit, 0, 0);
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
            action_latency := estimate_latency(write_miss, 0, 1);
            request_write(16#0024#, 16#0224#, action_latency, false, requestor);
          when 2 =>
            -- Way will be flushed as there are 2 dirty data in it
            action_latency := estimate_latency(write_miss, 0, 2);
            request_write(16#0034#, 16#0234#, action_latency, false, requestor);
          when 3 =>
            -- Way will be flushed as there are dirty data in them
            action_latency := estimate_latency(write_miss, 0, 1);
            request_write(16#0044#, 16#0244#, action_latency, false, requestor);
          when 4 =>
            -- Way will be flushed as there are dirty data in them
            action_latency := estimate_latency(write_miss, 0, 1);
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
            action_latency := estimate_latency(write_miss, 0, 1);
            request_write(16#0034#, 16#0334#, action_latency, false, requestor);
          when 4 =>
            action_latency := estimate_latency(read_hit, 0, 0);
            request_read(16#0024#, action_latency, 16#0324#, requestor);
          when 5 =>
            action_latency := estimate_latency(read_hit, 0, 0);
            request_read(16#0034#, action_latency, 16#0334#, requestor);
          when 6 =>
            -- Flush 0020, 0024, refill 0044
            action_latency := estimate_latency(read_miss, 1, 2);
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
          --
          -- From there, writethrough 034 should make this line non dirty
          --
          when 7 =>
            action_latency := estimate_latency(write_hit_through, 0, 0);
            request_write(16#0034#, 16#0434#, action_latency, true, requestor);
          when 8 =>
            -- Previous writethrough should remove the need for flushing (0034)
            action_latency := estimate_latency(read_miss, 1, 0);
            request_read(16#0004#, action_latency, 16#0204#, requestor);
          when 9 =>
            -- As 0040 should still be dirty, there is a flush + refill (0040)
            action_latency := estimate_latency(read_miss, 1, 1);
            request_read(16#0014#, action_latency, 16#0214#, requestor);
          when 10 =>
            action_latency := estimate_latency(write_hit, 1, 1);
            request_write(16#0000#, 16#0400#, action_latency, false, requestor);
          when 11 =>
            action_latency := estimate_latency(write_miss_through, 1, 1);
            request_write(16#0040#, 16#0440#, action_latency, true, requestor);
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

  reporter : process(clk, rst, i_porta_req, i_porta_addr, o_porta_valid, o_porta_read_data)
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
      elsif nb_clk = 300 then
        test_num <= 4;
        report_cache_stats(o_dbg_cstats);
      elsif nb_clk = 400 then
        test_num <= 5;
        report_cache_stats(o_dbg_cstats);
      elsif nb_clk = 500 then
        test_num <= 6;
        report_cache_stats(o_dbg_cstats);
      elsif nb_clk > 600 then
        test_num <= 7;
        report_cache_stats(o_dbg_cstats);
      end if;
      nb_clk := nb_clk + 1;

      if test_num = 7 then
        clkena <= '0';
        report "Ending simulation";
      end if;

      if rst = '0' and rising_edge(clk) then
        if i_porta_req = '1' and i_porta_we = '0' then
          report test_report_header(test_num, test_action, clk_after_req, o_dbg_state) &
            "[" & cache_state_name(o_dbg_state) &
            "] read request @" & to_hstring(i_porta_addr);
        end if;

        if i_porta_req = '1' and i_porta_we = '1' then
          report test_report_header(test_num, test_action, clk_after_req, o_dbg_state) &
            "[" & cache_state_name(o_dbg_state) &
            "] write request @" & to_hstring(i_porta_addr) & "<= " &
            to_hstring(i_porta_write_data);
        end if;
      end if;
    end if;
  end process reporter;

  cls : entity work.cache_line_streamer
    generic map (
      ADDR_WIDTH           => ADDR_WIDTH,
      DATA_WIDTH           => DATA_WIDTH,
      DATAS_PER_LINE_WIDTH => DATAS_PER_LINE_WIDTH)
    port map (
      clk     => clk,
      rst     => rst,
      i_creq  => cls_creq,
      o_cresp => cls_cresp,

      o_memory_req   => o_memory_req,
      o_memory_we    => o_memory_we,
      o_memory_addr  => o_memory_addr,
      o_memory_wdata => o_memory_write_data,
      i_memory_rdata => i_memory_read_data,
      i_memory_done  => i_memory_valid);

  -- memory simulator
  Simulated_Memory_1 : entity work.Simulated_Memory
    generic map (
      ADDR_WIDTH        => ADDR_WIDTH,
      DATA_WIDTH        => DATA_WIDTH,
      MEMORY_ADDR_WIDTH => 16,
      MEMORY_LATENCY    => MEMORY_LATENCY,
      DEBUG             => DEBUG)
    port map (
      clk                 => clk,
      rst                 => rst,
      i_memory_req        => o_memory_req,
      i_memory_we         => o_memory_we,
      i_memory_addr       => o_memory_addr,
      i_memory_write_data => o_memory_write_data,
      o_memory_read_data  => i_memory_read_data,
      o_memory_valid      => i_memory_valid);

  i_porta_req              <= porta_req;
  i_porta_we               <= porta_we;
  i_porta_addr             <= porta_addr;
  i_porta_do_write_through <= porta_wthrough;
  i_porta_write_data       <= porta_write_data;


end architecture ways_N_associative;

-------------------------------------------------------------------------------

configuration SinglePort_Associative_Cache_tb_ways_N_associative_cfg of SinglePort_Associative_Cache_tb is
  for ways_N_associative
  end for;
end SinglePort_Associative_Cache_tb_ways_N_associative_cfg;

-------------------------------------------------------------------------------

