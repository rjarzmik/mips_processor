-------------------------------------------------------------------------------
-- Title      : Instructions Provider
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : Instruction_Provider.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-03
-- Last update: 2016-12-10
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-12-03  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cpu_defs.all;
use work.instruction_defs.instr_tag_t;

-------------------------------------------------------------------------------

entity Instruction_Provider is

  generic (
    ADDR_WIDTH : integer;
    DATA_WIDTH : integer
    );

  port (
    clk                      : in  std_logic;
    rst                      : in  std_logic;
    -- control
    --- kill_req = 1 implies that the instructions at i_next_pc and i_next_next_pc
    --- should be killed, ie. o_valid sould be '0' for them.
    kill_req                 : in  std_logic;
    --- stall_req = 1 implies nothing is latched
    stall_req                : in  std_logic;
    -- program counters
    --- i_next_next_pc must get i_next_pc when o_do_step_pc is set
    i_next_pc                : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_next_pc_instr_tag      : in  instr_tag_t;
    i_next_next_pc           : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_next_next_pc_instr_tag : in  instr_tag_t;
    o_pc                     : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_instr_tag              : out instr_tag_t;
    o_data                   : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_valid                  : out std_logic;
    o_do_step_pc             : out std_logic;
    -- L2 connections
    o_L2c_req                : out std_logic;
    o_L2c_addr               : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_L2c_read_data          : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    i_L2c_valid              : in  std_logic;
    -- Debug signal
    --- Current fetching address accessed in the instruction cache
    o_dbg_fetching           : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_dbg_fetching_itag      : out instr_tag_t
    );

end entity Instruction_Provider;

-------------------------------------------------------------------------------

architecture str of Instruction_Provider is
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  subtype data_t is std_logic_vector(DATA_WIDTH - 1 downto 0);

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  signal next_pc       : addr_t;
  signal after_pc      : addr_t;
  signal next_pc_itag  : instr_tag_t;
  signal after_pc_itag : instr_tag_t;

  --- Cache query management
  signal cache_query_addr     : addr_t;
  signal cache_query_itag     : instr_tag_t;
  signal cache_latched_pc     : addr_t;
  signal cache_latched_itag   : instr_tag_t;
  signal cache_response_data  : data_t;
  signal cache_response_valid : std_logic;  -- true if data for
                                            -- @cache_latched_pc is valid
  signal cache_killer         : boolean;  -- true if the cache response should be killed

  --- Cache outputs
  signal out_pc    : addr_t;
  signal out_data  : data_t;
  signal out_valid : std_logic;
  signal out_itag  : instr_tag_t;

begin  -- architecture str
  l1c : entity work.Instruction_Cache(rtl)
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      DATA_WIDTH => DATA_WIDTH
      )
    port map (
      clk             => clk,
      rst             => rst,
      -- cache query and response
      addr            => cache_query_addr,
      data            => cache_response_data,
      data_valid      => cache_response_valid,
      -- signal carry over L2 connections
      o_L2c_req       => o_L2c_req,
      o_L2c_addr      => o_L2c_addr,
      i_L2c_read_data => i_L2c_read_data,
      i_L2c_valid     => i_L2c_valid
      );

  --- PC handling
  o_do_step_pc  <= '1' when (cache_response_valid = '1' and stall_req = '0' and not cache_killer) or kill_req = '1' else '0';
  next_pc       <= i_next_pc;
  after_pc      <= i_next_next_pc;
  next_pc_itag  <= i_next_pc_instr_tag;
  after_pc_itag <= i_next_next_pc_instr_tag;

  --- Cache inputs
  --- Cache is forced on next_pc on stall or kill, and makes only a "predictive"
  --- after_pc fetch in the optimal workflow to sustain a cadence of 1 cycle.
  cache_query_addr <= next_pc when
                      cache_response_valid = '0' or stall_req = '1' or cache_killer
                      else after_pc;
  cache_query_itag <= next_pc_itag when
                      cache_response_valid = '0' or stall_req = '1' or cache_killer
                      else after_pc_itag;

  cache_inputs : process(clk, rst)
  begin
    if rst = '1' then
      cache_killer <= false;
    elsif rising_edge(clk) then
      --- If killing while cache_response_valid = '1', cache_query = i_next_next_pc.
      --- In this case, when the future cache response should be killed.
      if kill_req = '1' and cache_response_valid = '1' then
        cache_killer <= true;
      elsif kill_req = '1' and cache_response_valid = '0' then
        --- If killing while cache_response_valid = '0', cache_query = i_next_pc.
        --- In this case, when the future cache response should be killed, and
        --- the next after it.
        cache_killer <= true;

      end if;
      --- on next cache move forward, disarm the cache killer
      if kill_req = '0' and cache_response_valid = '1' then
        cache_killer <= false;
      end if;
    end if;
  end process cache_inputs;

  cache_outputs : process(clk, rst)
  begin
    if rst = '1' then
    elsif rising_edge(clk) then
      cache_latched_pc   <= cache_query_addr;
      cache_latched_itag <= cache_query_itag;
    end if;
  end process cache_outputs;

  --- Outputs
  outputs : process(clk, rst, kill_req)
  begin
    if rst = '1' then
    elsif rising_edge(clk) then
      if kill_req = '1' or cache_killer then
        out_pc    <= cache_latched_pc;
        out_itag  <= cache_latched_itag;
        out_valid <= '0';
        if cache_response_valid = '1' then
          out_data <= cache_response_data;
        else
          out_data <= (others => 'X');
        end if;
      elsif stall_req = '1' then
      elsif kill_req = '0' then
        out_pc    <= cache_latched_pc;
        out_itag  <= cache_latched_itag;
        out_data  <= cache_response_data;
        out_valid <= cache_response_valid;
      end if;
    end if;
  end process outputs;

  --- Cache outputs
  o_pc        <= out_pc;
  o_data      <= out_data;
  o_valid     <= out_valid;
  o_instr_tag <= out_itag;

  --- Debug output
  o_dbg_fetching <= cache_query_addr;
  o_dbg_fetching_itag <= cache_query_itag;

-----------------------------------------------------------------------------
-- Component instantiations
-----------------------------------------------------------------------------
end architecture str;

-------------------------------------------------------------------------------
