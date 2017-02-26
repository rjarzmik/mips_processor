-------------------------------------------------------------------------------
-- Title      : Fetch stage synthesis test
-- Project    : Cache implementations
-------------------------------------------------------------------------------
-- File       : Fetch_Syn.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2017-02-17
-- Last update: 2017-02-22
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- Copyright (c) 2017 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2017-02-17  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.instruction_defs.all;

entity Fetch_Syn is
  generic (
    ADDR_WIDTH : integer := 32;
    DATA_WIDTH : integer := 32;
    STEP       : natural := 4
    );

  port (
    clk       : in std_logic;
    rst       : in std_logic;
    stall_req : in std_logic;           -- stall current instruction
    kill_req  : in std_logic;           -- kill current instruction

    o_pc_instr                 : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_instruction              : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_instr_tag                : out instr_tag_t;
    o_mispredict_kill_pipeline : out std_logic;
    -- L2 connections
    o_l2c_req                  : out std_logic;
    o_l2c_we                   : out std_logic;
    o_l2c_addr                 : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_l2c_rdata                : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_l2c_wdata                : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    i_l2c_done                 : in  std_logic;
    -- Writeback feedback signals
    i_is_jump                  : in  std_logic;
    i_jump_target              : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_commited_instr_tag       : in  instr_tag_t
    );
end entity Fetch_Syn;

architecture str of Fetch_Syn is
  signal ri_l2c_rdata          : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal ri_l2c_done           : std_logic;
  signal ri_is_jump            : std_logic;
  signal ri_jump_target        : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal ri_commited_instr_tag : instr_tag_t;
begin  -- architecture str
  Fetch_1 : entity work.Fetch(simple)
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      DATA_WIDTH => DATA_WIDTH,
      STEP       => STEP)
    port map (
      clk                        => clk,
      rst                        => rst,
      stall_req                  => stall_req,
      kill_req                   => kill_req,
      o_pc_instr                 => o_pc_instr,
      o_instruction              => o_instruction,
      o_instr_tag                => o_instr_tag,
      o_mispredict_kill_pipeline => o_mispredict_kill_pipeline,
      o_l2c_req                  => o_l2c_req,
      o_l2c_we                   => o_l2c_we,
      o_l2c_addr                 => o_l2c_addr,
      i_l2c_rdata                => i_l2c_rdata,
      o_l2c_wdata                => o_l2c_wdata,
      i_l2c_done                 => i_l2c_done,
      i_is_jump                  => i_is_jump,
      i_jump_target              => ri_jump_target,
      i_commited_instr_tag       => ri_commited_instr_tag);

  process(clk)
  begin
    if rising_edge(clk) then
      ri_l2c_rdata          <= i_l2c_rdata;
      ri_l2c_done           <= i_l2c_done;
      ri_is_jump            <= i_is_jump;
      ri_jump_target        <= i_jump_target;
      ri_commited_instr_tag <= i_commited_instr_tag;
    end if;
  end process;

end architecture str;
