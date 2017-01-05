-------------------------------------------------------------------------------
-- Title      : Register File
-- Project    : 
-------------------------------------------------------------------------------
-- File       : RegisterFile.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-12
-- Last update: 2017-01-05
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: MIPS Register File, 32 registers
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-12  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

-------------------------------------------------------------------------------

entity RegisterFile is

  generic (
    DATA_WIDTH        : positive := 32;
    NB_GP_REGISTERS   : positive := 32;  -- r0 to r31
    NB_GPDW_REGISTERS : positive := 1    -- double width registers :
                                         -- mf(mflo and mfhi)
    );

  port (
    clk           : in  std_logic;
    rst           : in  std_logic;
    stall_req     : in  std_logic;
    a_idx         : in  natural range 0 to NB_GP_REGISTERS + NB_GPDW_REGISTERS - 1;
    b_idx         : in  natural range 0 to NB_GP_REGISTERS + NB_GPDW_REGISTERS - 1;
    -- Writeback register
    rwb_reg_we    : in  std_logic;
    rwb_reg_idx   : in  natural range 0 to NB_GP_REGISTERS + NB_GPDW_REGISTERS - 1;
    rwb_reg_wdata : in  std_logic_vector(DATA_WIDTH * 2 - 1 downto 0);
    -- Output read registers, set on clk rising edge
    q_a           : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    q_b           : out std_logic_vector(DATA_WIDTH - 1 downto 0)
    );

end entity RegisterFile;

-------------------------------------------------------------------------------

architecture rtl of RegisterFile is
  constant NB_REGS : natural := NB_GP_REGISTERS + NB_GPDW_REGISTERS;

  type gp_array is array (0 to NB_GP_REGISTERS - 1) of
    std_logic_vector(DATA_WIDTH - 1 downto 0);
  type gpdw_array is array (0 to NB_GPDW_REGISTERS - 1) of
    std_logic_vector(DATA_WIDTH * 2 - 1 downto 0);
  signal gpregs   : gp_array   := (others => (others => '0'));
  signal gpdwregs : gpdw_array := (others => (others => '0'));

begin  -- architecture rtl

  process(rst, clk)
  begin
    if rst = '1' then
      q_a <= (others => 'X');
      q_b <= (others => 'X');
    elsif rising_edge(clk) then
      if rwb_reg_we = '1' then
        if rwb_reg_idx < NB_GP_REGISTERS then
          gpregs(rwb_reg_idx) <= rwb_reg_wdata(DATA_WIDTH - 1 downto 0);
        else
          gpdwregs(rwb_reg_idx) <= rwb_reg_wdata;
        end if;
      end if;

      if stall_req = '0' then
        if a_idx < NB_GP_REGISTERS then
          q_a <= gpregs(a_idx);
        else
          if (a_idx mod 2) = 0 then
            q_a <= gpdwregs(a_idx / 2)(DATA_WIDTH - 1 downto 0);
          else
            q_a <= gpdwregs(a_idx / 2)(DATA_WIDTH * 2 - 1 downto DATA_WIDTH);
          end if;
        end if;
        q_b <= gpregs(b_idx);
      end if;
    end if;
  end process;

end architecture rtl;

-------------------------------------------------------------------------------
