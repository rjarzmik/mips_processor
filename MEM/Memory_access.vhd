-------------------------------------------------------------------------------
-- Title      : Memory access stage
-- Project    : 
-------------------------------------------------------------------------------
-- File       : memory_access.vhd
-- Author     : Simon Desfarges
-- Company    : 
-- Created    : 2016-11-24
-- Last update: 2016-12-14
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
-- This is the memory access pipeline stage.
-- Depending on operation to do (load/store), does the corresponding memory
-- access (ie write or read the L1 cache). This module outputs the read memory
-- data to store in the register. It outputs a 'stall' signal for earlier
-- stages in case of memory latency > 1 clock cycle.
--
-- Concerning Stores, the latency is minimum 2 cycles: 1 to set @, data, we and
-- 1 to get the write acknoledgement. In my opinion, this part should be done
-- in WB stage, because 'store' is a kind of 'commit'. This instruction is not
-- really killable at this stage as a store is atomic and lasts more than 1
-- cycle. In a first approach, this stage should not need kill (may need it
-- when exception occurs).
-- Another optimization which can reduce the latency is to reverse the "WR ACK"
-- logic: instead of ACK the write, use a 'MEM BUSY' signal. It jumps to '1' if
-- the memory cache is busy (ie cache miss). If the cache is not busy, the
-- write en transaction is registered (need to maintain the data to write for
-- only one cycle). The 'need_stall' signal is set to 1 if we have a store
-- instruction AND the cache is not busy. The miss is hidden by others
-- instructions.
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-24  1.0      simon   Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.cpu_defs.all;

-------------------------------------------------------------------------------


entity Memory_access is
  generic (
    ADDR_WIDTH : integer := 32;
    DATA_WIDTH : integer := 32
    );
  port (
    clk         : in  std_logic;        -- input clk
    rst         : in  std_logic;        -- input async reset
    stall_req   : in  std_logic;
    kill_req    : in  std_logic;
    o_exception : out std_logic;  -- An exception happened in the module. The
    -- only 'supported' exception is when a store
    -- is killed.

    i_reg1     : in register_port_type;  -- Memory address to access
    i_reg2     : in register_port_type;  -- not used, to be fwded
    i_mem_op   : in memory_op_type;
    i_mem_data : in std_logic_vector(DATA_WIDTH - 1 downto 0);  -- Data to
                                        -- store in memory

-- Carry-over signals
    i_is_jump     : in std_logic;       -- not used, to be fwded
    i_jump_target : in std_logic_vector(ADDR_WIDTH - 1 downto 0);  -- not used, to be fwded

    o_reg1        : out register_port_type;
    o_reg2        : out register_port_type;
    o_jump_target : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_is_jump     : out std_logic;

    -- Control hazard outputs. Needed to check RAW hazards
    o_stage1_reg1 : out register_port_type;
    o_stage1_reg2 : out register_port_type;
    o_stage2_reg1 : out register_port_type;
    o_stage2_reg2 : out register_port_type;

    -- Memory interface
    i_mem_rd_valid   : in  std_logic;
    i_mem_rd_data    : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_mem_wr_en      : out std_logic;
    o_mem_word_width : out std_logic;   -- Actual width of the word to be
    -- written. 1 -> 32 bits, 0 -> 8 bits (LSB)
    i_mem_wr_ack     : in  std_logic;
    o_mem_addr       : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_mem_wr_data    : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_need_stall     : out std_logic;

    -- debug signals
    i_dbg_mem_pc : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_dbg_mem_pc : out std_logic_vector(ADDR_WIDTH - 1 downto 0)
    );

end entity Memory_access;

architecture rtl of Memory_access is
  -- This first version has a minimal latency of 3 clk: 1 clk to put input @ to
  -- memory bus @, 1 clk to retrieve corresponding data from memory and a last
  -- one to register it on the output. The
  -- o_need_stall signal is not registered because we need to stall the earlier
  -- stages with no delay if memory access has a latency > 1 clk.

  signal r1_reg1   : register_port_type;
  signal r1_reg2   : register_port_type;
  signal r1_mem_op : memory_op_type;

  signal r1_is_jump     : std_logic;
  signal r1_jump_target : std_logic_vector(ADDR_WIDTH - 1 downto 0);

  signal r2_reg1   : register_port_type;
  signal r2_reg2   : register_port_type;
  signal r2_mem_op : memory_op_type;

  signal r2_is_jump     : std_logic;
  signal r2_jump_target : std_logic_vector(ADDR_WIDTH - 1 downto 0);

  signal r1_dbg_mem_pc : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal r2_dbg_mem_pc : std_logic_vector(ADDR_WIDTH - 1 downto 0);

  -- Memory interface internal use.
  signal s_mem_wr_en      : std_logic;
  signal s_mem_word_width : std_logic;
  signal s_mem_addr       : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal s_mem_wr_data    : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal s_need_stall     : std_logic;
  signal s_mem_op_load    : std_logic;
  signal s_mem_op_store   : std_logic;

begin  -- architecture rtl

  -- Output internal registers for the control hazard module.
  o_stage1_reg1 <= r1_reg1;
  o_stage1_reg2 <= r1_reg2;
  o_stage2_reg1 <= r2_reg1;
  o_stage2_reg2 <= r2_reg2;

  o_mem_wr_en      <= s_mem_wr_en;
  o_mem_word_width <= s_mem_word_width;
  o_mem_addr       <= s_mem_addr;
  o_mem_wr_data    <= s_mem_wr_data;
  o_need_stall     <= s_need_stall;

  -- stall if there is a read mem access taking longer than 1 clk or a wr
  -- access and data not ACKed.
  s_need_stall <= (not rst)
                  and (
                    (s_mem_op_load and not i_mem_rd_valid)
                    or (s_mem_op_store and not i_mem_wr_ack)
                    );

  with r1_mem_op select
    s_mem_op_load <=
    '0' when none | storew | store8,
    '1' when others;

  with r1_mem_op select
    s_mem_op_store <=
    '1' when storew | store8,
    '0' when others;

  -- purpose: because of memory latency we need to register inpout
  -- signal to easily retrieve them for output.
  -- type   : sequential
  -- inputs : clk, rst
  -- outputs: 
  internal_regs : process (clk, rst) is
  begin  -- process internal regs
    if rst = '1' then                   -- asynchronous reset (active high)
      r1_mem_op      <= none;
      r1_jump_target <= (others => '0');
      r1_is_jump     <= '0';
      r1_reg1.we     <= '0';
      r1_reg1.idx    <= 0;
      r1_reg1.data   <= (others => '0');
      r1_reg2.we     <= '0';
      r1_reg2.idx    <= 0;
      r1_reg2.data   <= (others => '0');
      r2_mem_op      <= none;
      r2_jump_target <= (others => '0');
      r2_is_jump     <= '0';
      r2_reg1.we     <= '0';
      r2_reg1.idx    <= 0;
      r2_reg1.data   <= (others => '0');
      r2_reg2.we     <= '0';
      r2_reg2.idx    <= 0;
      r2_reg2.data   <= (others => '0');
    elsif rising_edge(clk) then         -- rising clock edge
      if kill_req = '1' then
        r1_mem_op      <= none;
        r1_jump_target <= (others => '0');
        r1_is_jump     <= '0';
        r1_reg1.we     <= '0';
        r1_reg1.idx    <= 0;
        r1_reg1.data   <= (others => '0');
        r1_reg2.we     <= '0';
        r1_reg2.idx    <= 0;
        r1_reg2.data   <= (others => '0');
        r2_mem_op      <= none;
        r2_jump_target <= (others => '0');
        r2_is_jump     <= '0';
        r2_reg1.we     <= '0';
        r2_reg1.idx    <= 0;
        r2_reg1.data   <= (others => '0');
        r2_reg2.we     <= '0';
        r2_reg2.idx    <= 0;
        r2_reg2.data   <= (others => '0');
      elsif stall_req = '0' then
        if s_need_stall = '0' then
          r1_reg1.we   <= i_reg1.we;
          r1_reg1.idx  <= i_reg1.idx;
          r1_reg1.data <= i_reg1.data;
          r1_reg2.we   <= i_reg2.we;
          r1_reg2.idx  <= i_reg2.idx;
          r1_reg2.data <= i_reg2.data;

          r1_mem_op <= i_mem_op;

          r1_is_jump     <= i_is_jump;
          r1_jump_target <= i_jump_target;

          r2_reg1.we   <= r1_reg1.we;
          r2_reg1.idx  <= r1_reg1.idx;
          r2_reg1.data <= r1_reg1.data;
          r2_reg2.we   <= r1_reg2.we;
          r2_reg2.idx  <= r1_reg2.idx;
          r2_reg2.data <= r1_reg2.data;

          r2_mem_op <= r1_mem_op;

          r2_is_jump     <= r1_is_jump;
          r2_jump_target <= r1_jump_target;
        end if;
      end if;
    end if;
  end process internal_regs;

  -- purpose: According to memory_operation, do the right memory access
  -- type   : sequential
  -- inputs : clk, rst
  -- outputs: 
  process (clk, rst) is
  begin  -- process
    if rst = '1' then                   -- asynchronous reset (active high)
      s_mem_wr_en      <= '0';
      s_mem_word_width <= '0';
      s_mem_addr       <= (others => '0');
      s_mem_wr_data    <= (others => '0');
    elsif rising_edge(clk) then         -- rising clock edge
      if kill_req = '1' then
        s_mem_wr_en <= '0';
      elsif stall_req = '0' then
        if s_need_stall = '0' then
          case i_mem_op is
            when loadw =>
              s_mem_addr  <= i_reg1.data;
              s_mem_wr_en <= '0';
            when load8 =>
              s_mem_addr  <= i_reg1.data;
              s_mem_wr_en <= '0';
            when load8_signextend32 =>
              s_mem_addr  <= i_reg1.data;
              s_mem_wr_en <= '0';
            when storew =>
              s_mem_addr       <= i_reg1.data;
              s_mem_wr_data    <= i_mem_data;
              s_mem_wr_en      <= '1';
              s_mem_word_width <= '1';
            when store8 =>
              s_mem_addr                             <= i_reg1.data;
              s_mem_wr_data(DATA_WIDTH - 1 downto 8) <= (others => '0');
              s_mem_wr_data(7 downto 0)              <= i_mem_data(7 downto 0);
              s_mem_wr_en                            <= '1';
              s_mem_word_width                       <= '0';
            when others =>
              s_mem_wr_en <= '0';
          end case;
        end if;
      end if;
    end if;
  end process;

  -- purpose: Outputs Mem datas in registers. Masks/sign extend datas.
  -- type   : sequential
  -- inputs : clk, rst
  -- outputs:
  process (clk, rst) is
  begin
    if rst = '1' then
      o_reg1.idx  <= 0;
      o_reg1.we   <= '0';
      o_reg1.data <= (others => '0');

      o_reg2.idx  <= 0;
      o_reg2.we   <= '0';
      o_reg2.data <= (others => '0');

      o_jump_target <= (others => '0');
      o_is_jump     <= '0';
      o_exception   <= '0';

    elsif rising_edge(clk) then
      o_exception <= '0';
      if kill_req = '1' then             -- outputs a NOP
        o_reg1.we <= '0';
        o_reg2.we <= '0';
        o_is_jump <= '0';
        if (r2_mem_op = store8
            or r2_mem_op = storew) then  -- For now a store cannot be killed.
                                         -- (because it is hard to roll-back the
                                         -- memory transaction).
          o_exception <= '1';
        end if;
      elsif stall_req = '0' then
        o_reg1.idx <= r2_reg1.idx;

        if i_mem_rd_valid = '1'
          and (r2_mem_op = load8_signextend32
               or r2_mem_op = load8
               or r2_mem_op = loadw)
        then
          o_reg1.we <= '1';
        else
          o_reg1.we <= r2_reg1.we;
        end if;

        if i_mem_rd_valid = '1' then
          case r2_mem_op is
            when loadw =>
              o_reg1.data <= i_mem_rd_data;
            when load8 =>
              o_reg1.data (DATA_WIDTH - 1 downto 8) <= (others => '0');
              o_reg1.data (7 downto 0)              <= i_mem_rd_data (7 downto 0);
            when load8_signextend32 =>
              o_reg1.data (DATA_WIDTH - 1 downto 8) <= (others => i_mem_rd_data(7));
              o_reg1.data (7 downto 0)              <= i_mem_rd_data (7 downto 0);
            when others =>
              o_reg1.data <= r2_reg1.data;
          end case;
        else
          o_reg1.data <= r2_reg1.data;
        end if;

        o_reg2        <= r2_reg2;
        o_jump_target <= r2_jump_target;
        o_is_jump     <= r2_is_jump;

      end if;
    end if;
  end process;

  debug : process (clk, rst) is
  begin  -- process debug
    if rst = '1' then                   -- asynchronous reset (active low)
      r1_dbg_mem_pc <= (others => '0');
      r2_dbg_mem_pc <= (others => '0');
      o_dbg_mem_pc  <= (others => '0');
    elsif rising_edge(clk) then         -- rising clock edge
      if kill_req = '1' then
        r1_dbg_mem_pc <= (others => 'X');
        r2_dbg_mem_pc <= (others => 'X');
        o_dbg_mem_pc  <= (others => 'X');
      elsif stall_req = '0' then
        r1_dbg_mem_pc <= i_dbg_mem_pc;
        r2_dbg_mem_pc <= r1_dbg_mem_pc;
        o_dbg_mem_pc  <= r2_dbg_mem_pc;
      end if;
    end if;
  end process debug;
end architecture rtl;
