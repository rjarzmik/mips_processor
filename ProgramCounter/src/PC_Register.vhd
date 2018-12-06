-------------------------------------------------------------------------------
-- Title      : Program Counter
-- Project    : 
-------------------------------------------------------------------------------
-- File       : PC_Register.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-13
-- Last update: 2018-12-05
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: The MIPS processor program counter
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-13  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cpu_defs.all;

entity PC_Register is

  generic (
    ADDR_WIDTH : integer := 32;
    STEP       : integer := 4
    );

  port (
    clk          : in  std_logic;
    rst          : in  std_logic;
    stall_pc     : in  std_logic;
    -- jump_target: should appear on o_pc on clk rising edge if stall_pc = '0'
    jump_pc      : in  std_logic;
    jump_target  : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_current_pc : out std_logic_vector(ADDR_WIDTH - 1 downto 0)
   -- Debug
    );

end entity PC_Register;

-------------------------------------------------------------------------------
------------ Architecture simple: always step the PC, no prediction -----------
-------------------------------------------------------------------------------
architecture simple of PC_Register is
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);

  component PC_Adder is
    generic (
      ADDR_WIDTH : integer;
      STEP       : integer);
    port (
      current_pc : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
      next_pc    : out std_logic_vector(ADDR_WIDTH - 1 downto 0));
  end component PC_Adder;

  signal pc         : addr_t := (others => '0');
  signal pc_stepped : addr_t := (others => '0');

begin
  pc_addr : PC_Adder
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      STEP       => STEP)
    port map (
      current_pc => pc,
      next_pc    => pc_stepped);

  process(clk, stall_pc, jump_pc, jump_target, pc_stepped)
  begin
    if rst = '1' then
      pc <= std_logic_vector(to_signed(-STEP, pc'length));
    else
      if stall_pc = '0' and rising_edge(clk) then
        if jump_pc = '1' then
          pc <= jump_target;
        else
          pc <= pc_stepped;
        end if;
      end if;
    end if;
  end process;

  o_current_pc <= pc;
end architecture simple;

architecture pipelined of PC_Register is
begin
end architecture pipelined;
