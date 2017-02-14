-------------------------------------------------------------------------------
-- Title      : MIPS CPU synthesis entity
-- Project    : Cache implementations
-------------------------------------------------------------------------------
-- File       : MIPS_CPU_Syn.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2017-01-04
-- Last update: 2017-02-14
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Entity to test the MIPS CPU Synthesis
-------------------------------------------------------------------------------
-- Copyright (c) 2017 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2017-01-04  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.cpu_defs.all;
use work.cache_defs.all;
use work.instruction_defs.instr_tag_t;
use work.instruction_defs.INSTR_TAG_FIRST_VALID;
use work.instruction_prediction.prediction_t;

entity MIPS_CPU_Syn is

  generic (
    ADDR_WIDTH           : integer  := 32;
    DATA_WIDTH           : integer  := 32;
    NB_REGISTERS_GP      : positive := 32;  -- r0 to r31
    NB_REGISTERS_SPECIAL : positive := 2;   -- mflo and mfhi
    DEBUG                : boolean  := false
    );

  port (
    clk              : in  std_logic;
    rst              : in  std_logic;
    -- outer mem interface
    o_memory_req     : out std_logic;
    o_memory_we      : out std_logic;
    o_memory_addr    : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_memory_rdata   : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_memory_wdata   : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    i_memory_done    : in  std_logic;
    -- Temprorary Data Memory interface
    o_mem_addr       : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_mem_rd_valid   : in  std_logic;
    i_mem_rd_data    : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_mem_wr_en      : out std_logic;
    o_mem_word_width : out std_logic;
    o_mem_wr_data    : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    i_mem_wr_ack     : in  std_logic
    );

end entity MIPS_CPU_Syn;

architecture str of MIPS_CPU_Syn is

begin  -- architecture str

  MIPS_CPU_1 : entity work.MIPS_CPU
    generic map (
      ADDR_WIDTH           => ADDR_WIDTH,
      DATA_WIDTH           => DATA_WIDTH,
      NB_REGISTERS_GP      => NB_REGISTERS_GP,
      NB_REGISTERS_SPECIAL => NB_REGISTERS_SPECIAL)
    port map (
      clk     => clk,
      rst     => rst,

      o_mem_addr       => o_mem_addr,
      i_mem_rd_valid   => i_mem_rd_valid,
      i_mem_rd_data    => i_mem_rd_data,
      o_mem_wr_en      => o_mem_wr_en,
      o_mem_word_width => o_mem_word_width,
      o_mem_wr_data    => o_mem_wr_data,
      i_mem_wr_ack     => i_mem_wr_ack,

      o_memory_req   => o_memory_req,
      o_memory_we    => o_memory_we,
      o_memory_addr  => o_memory_addr,
      o_memory_wdata => o_memory_wdata,
      i_memory_rdata => i_memory_rdata,
      i_memory_done  => i_memory_done);

end architecture str;
