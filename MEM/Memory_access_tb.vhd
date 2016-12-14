-------------------------------------------------------------------------------
-- Title      : Testbench for design "Memory_access"
-- Project    : 
-------------------------------------------------------------------------------
-- File       : Memory_access_tb.vhd
-- Author     : Simon Desfarges
-- Company    : 
-- Created    : 2016-11-28
-- Last update: 2016-12-14
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Very simple testbench for the memory stage module.
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-28  1.0      simon   Created

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cpu_defs.all;
-------------------------------------------------------------------------------

entity Memory_access_tb is

end entity Memory_access_tb;

-------------------------------------------------------------------------------


architecture tb of Memory_access_tb is

  -- component generics
  constant ADDR_WIDTH : integer := 32;
  constant DATA_WIDTH : integer := 32;

  -- component ports
  signal clk              : std_logic := '0';
  signal rst              : std_logic := '1';
  signal stall_req        : std_logic;
  signal kill_req         : std_logic := '0';
  signal o_exception      : std_logic;
  signal i_reg1           : register_port_type;
  signal i_reg2           : register_port_type;
  signal i_mem_op         : memory_op_type;
  signal i_mem_data       : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal i_is_jump        : std_logic;
  signal i_jump_target    : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal o_reg1           : register_port_type;
  signal o_reg2           : register_port_type;
  signal o_jump_target    : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal o_is_jump        : std_logic;
  signal o_stage1_reg1    : register_port_type;
  signal o_stage1_reg2    : register_port_type;
  signal o_stage2_reg1    : register_port_type;
  signal o_stage2_reg2    : register_port_type;
  signal i_mem_rd_valid   : std_logic;
  signal i_mem_rd_data    : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal o_mem_wr_en      : std_logic;
  signal o_mem_word_width : std_logic;
  signal i_mem_wr_ack     : std_logic;
  signal o_mem_addr       : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal o_mem_wr_data    : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal o_need_stall     : std_logic;

  signal i_dbg_mem_pc : std_logic_vector(ADDR_WIDTH -1 downto 0);
  signal o_dbg_mem_pc : std_logic_vector(ADDR_WIDTH -1 downto 0);

begin  -- architecture tb

  -- component instantiation
  DUT : entity work.Memory_access
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      DATA_WIDTH => DATA_WIDTH)
    port map (
      clk              => clk,
      rst              => rst,
      stall_req        => stall_req,
      kill_req         => kill_req,
      o_exception      => o_exception,
      i_reg1           => i_reg1,
      i_reg2           => i_reg2,
      i_mem_op         => i_mem_op,
      i_mem_data       => i_mem_data,
      i_is_jump        => i_is_jump,
      i_jump_target    => i_jump_target,
      o_reg1           => o_reg1,
      o_reg2           => o_reg2,
      o_jump_target    => o_jump_target,
      o_is_jump        => o_is_jump,
      o_stage1_reg1    => o_stage1_reg1,
      o_stage1_reg2    => o_stage1_reg2,
      o_stage2_reg1    => o_stage2_reg1,
      o_stage2_reg2    => o_stage2_reg2,
      i_mem_rd_valid   => i_mem_rd_valid,
      i_mem_rd_data    => i_mem_rd_data,
      o_mem_wr_en      => o_mem_wr_en,
      o_mem_word_width => o_mem_word_width,
      i_mem_wr_ack     => i_mem_wr_ack,
      o_mem_addr       => o_mem_addr,
      o_mem_wr_data    => o_mem_wr_data,
      o_need_stall     => o_need_stall,
      i_dbg_mem_pc     => i_dbg_mem_pc,
      o_dbg_mem_pc     => o_dbg_mem_pc
      );

  -- clock generation
  clk <= not clk after 10 ns;

  -- waveform generation
  WaveGen_Proc : process
  begin
    -- constant signals
    stall_req     <= '0';
    kill_req      <= '0';
    i_is_jump     <= '0';
    i_jump_target <= X"01234567";

    rst <= '1';

    i_reg2.idx  <= 8;
    i_reg2.we   <= '0';
    i_reg2.data <= X"FEDCBA98";

    i_reg1.idx  <= 5;
    i_reg1.we   <= '1';
    i_reg1.data <= (others => '0');

    -- none, loadw, storew, load8, load8_signextend32, store8
    i_mem_op   <= none;
    i_mem_data <= (others => '0');

    wait until clk = '1';
    wait until clk = '1';
    rst      <= '0';
    -- Testing mem_op == none...
    i_mem_op <= none;

    i_reg2.idx  <= 1;
    i_reg2.we   <= '0';
    i_reg2.data <= X"00000001";

    i_reg1.idx  <= 5;
    i_reg1.we   <= '1';
    i_reg1.data <= (others => '0');
    wait until clk = '1';

    i_reg2.idx  <= 2;
    i_reg2.we   <= '1';
    i_reg2.data <= X"00000002";

    i_reg1.idx  <= 3;
    i_reg1.we   <= '0';
    i_reg1.data <= X"AAAAAAAA";

    wait until clk = '1';
    -- Testing mem_op == loadw...
    i_mem_op <= loadw;

    i_reg2.idx  <= 2;
    i_reg2.we   <= not i_reg2.we;
    i_reg2.data <= X"00000002";

    i_reg1.idx  <= 4;
    i_reg1.we   <= '1';
    i_reg1.data <= X"01234567";

    wait until clk = '1';
    -- Testing mem_op == none...
    i_mem_op <= none;

    i_reg2.idx  <= 1;
    i_reg2.we   <= '0';
    i_reg2.data <= X"00000001";

    i_reg1.idx  <= 5;
    i_reg1.we   <= '1';
    i_reg1.data <= (others => '0');

    wait until clk = '1';
    -- Testing mem_op == load8...
    i_mem_op <= load8;

    i_reg2.idx  <= 1;
    i_reg2.we   <= '1';
    i_reg2.data <= X"00011111";

    i_reg1.idx  <= 4;
    i_reg1.we   <= '1';
    i_reg1.data <= X"89ABCDEF";
    wait until clk = '1';
    -- Testing mem_op == load8_signextend32...
    i_mem_op    <= load8_signextend32;

    i_reg2.idx  <= 2;
    i_reg2.we   <= '0';
    i_reg2.data <= X"00011111";

    i_reg1.idx  <= 6;
    i_reg1.we   <= '1';
    i_reg1.data <= X"01234567";

    wait until clk = '1';
    -- Testing mem_op == storew...
    i_mem_op <= storew;

    i_reg2.idx  <= 2;
    i_reg2.we   <= '0';
    i_reg2.data <= X"00011111";

    i_reg1.idx  <= 3;
    i_reg1.we   <= '0';
    i_reg1.data <= X"0000FFFF";
    i_mem_data  <= X"ABABABAB";
    wait until clk = '1';               -- 1 cycle stall
    wait until clk = '1';
    i_mem_op    <= none;
    wait until clk = '1';
    wait until clk = '1';
    wait until clk = '1';
    -- Testing mem_op == store8...
    i_mem_op    <= store8;

    i_reg2.idx  <= 2;
    i_reg2.we   <= '0';
    i_reg2.data <= X"00011111";

    i_reg1.idx  <= 1;
    i_reg1.we   <= '0';
    i_reg1.data <= X"FFFF0000";
    i_mem_data  <= X"DFDFDFDF";
    wait until clk = '1';
    wait until clk = '1';               -- 1 cycle stall
    -- Testing mem_op == load8...
    i_mem_op    <= load8;

    i_reg2.idx  <= 1;
    i_reg2.we   <= '1';
    i_reg2.data <= X"00011111";

    i_reg1.idx  <= 4;
    i_reg1.we   <= '1';
    i_reg1.data <= X"89ABCDEF";
    stall_req   <= '1';
    wait until clk = '1';
    wait until clk = '1';
    wait until clk = '1';
    wait until clk = '1';
    stall_req   <= '0';

    wait until clk = '1';
    i_mem_op  <= none;
    i_reg1.we <= '0';
    report "end of simulation";
    wait;
  end process WaveGen_Proc;

  -- purpose: memory
  -- type   : sequential
  -- inputs : clk, rst
  -- outputs: 
  mem : process (clk, rst) is
  begin  -- process mem
    if rst = '1' then                   -- asynchronous reset (active low)
      i_mem_rd_valid <= '0';
      i_mem_rd_data  <= (others => '0');
      i_mem_wr_ack   <= '0';
    elsif rising_edge(clk) then         -- rising clock edge
      i_mem_wr_ack <= '0';
      if o_mem_wr_en = '1' then
        i_mem_rd_valid <= '0';
        i_mem_rd_data  <= i_mem_rd_data;
        assert i_mem_wr_ack = '0' report "Invalid transaction" severity error;
        i_mem_wr_ack   <= '1';
      else
        -- copy rd @ to data and change data order (ABCD -> DCBA)
        i_mem_rd_data <= o_mem_addr(3 downto 0) &
                         o_mem_addr(7 downto 4) &
                         o_mem_addr(11 downto 8) &
                         o_mem_addr(15 downto 12) &
                         o_mem_addr(19 downto 16) &
                         o_mem_addr(23 downto 20) &
                         o_mem_addr(27 downto 24) &
                         o_mem_addr(31 downto 28);

        i_mem_rd_valid <= '1';
      end if;
    end if;
  end process mem;

  -- purpose: handles the PC
  -- type   : sequential
  -- inputs : clk, rst
  -- outputs:
  dbg_pc : process (clk, rst) is
  begin  -- process dbg_pc
    if rst = '1' then                   -- asynchronous reset (active low)
      i_dbg_mem_pc <= (others => '0');
    elsif rising_edge(clk) then         -- rising clock edge
      if stall_req = '0' and o_need_stall = '0' then
        i_dbg_mem_pc <= std_logic_vector(unsigned(i_dbg_mem_pc) + 4);
      end if;
    end if;
  end process dbg_pc;
end architecture tb;

-------------------------------------------------------------------------------

