-------------------------------------------------------------------------------
-- Title      : Register File
-- Project    : MIPS Processor
-------------------------------------------------------------------------------
-- File       : RegisterFile.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2016-11-12
-- Last update: 2017-02-16
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: MIPS Register File, 32 registers
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2016-11-12  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library rjarzmik;
use rjarzmik.memories.sc_sram;

entity RegisterFile is
  generic (
    DATA_WIDTH        : positive := 32;
    NB_GP_REGISTERS   : positive := 32;      -- r0 to r31
    NB_GPDW_REGISTERS : positive := 2;       -- double width registers :
    DEBUG             : boolean  := false);  -- mf(mflo and mfhi)

  port (
    clk           : in  std_logic;
    rst           : in  std_logic;
    stall_req     : in  std_logic;
    a_idx         : in  natural range 0 to NB_GP_REGISTERS + NB_GPDW_REGISTERS - 1;
    b_idx         : in  natural range 0 to NB_GP_REGISTERS + NB_GPDW_REGISTERS - 1;
    -- Writeback register, setting or clk rising edge
    rwb_reg_we    : in  std_logic;
    rwb_reg_idx   : in  natural range 0 to NB_GP_REGISTERS + NB_GPDW_REGISTERS - 1;
    rwb_reg_wdata : in  std_logic_vector(DATA_WIDTH * 2 - 1 downto 0);
    -- Output read registers, set combinatory
    q_a           : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    q_b           : out std_logic_vector(DATA_WIDTH - 1 downto 0)
    );
end entity RegisterFile;

architecture str of RegisterFile is
  constant nb_gp_bits   : integer := integer(log2(real(NB_GP_REGISTERS)));
  constant nb_gpdw_bits : integer := integer(log2(real(NB_GPDW_REGISTERS)));
  signal r1_idx         : natural range 0 to NB_GP_REGISTERS - 1;
  signal r2_idx         : natural range 0 to NB_GP_REGISTERS - 1;
  signal w_idx          : natural range 0 to NB_GP_REGISTERS - 1;
  signal rdw_idx        : natural range 0 to NB_GPDW_REGISTERS - 1;
  signal wdw_idx        : natural range 0 to NB_GPDW_REGISTERS - 1;
  signal sw1_rdata      : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal sw2_rdata      : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal dw1_rdata      : std_logic_vector(DATA_WIDTH * 2 - 1 downto 0);
  signal dw2sw_rdata    : std_logic_vector(DATA_WIDTH - 1 downto 0);

  signal sw_we : std_logic;
  signal dw_we : std_logic;
  signal wdata : std_logic_vector(DATA_WIDTH - 1 downto 0);

  signal r1_addr  : std_logic_vector(nb_gp_bits - 1 downto 0);
  signal r2_addr  : std_logic_vector(nb_gp_bits - 1 downto 0);
  signal w_addr   : std_logic_vector(nb_gp_bits - 1 downto 0);
  signal rdw_addr : std_logic_vector(nb_gpdw_bits - 1 downto 0);
  signal wdw_addr : std_logic_vector(nb_gpdw_bits - 1 downto 0);

begin  -- architecture rtl
  r1_idx      <= a_idx mod NB_GP_REGISTERS;
  r2_idx      <= b_idx mod NB_GP_REGISTERS;
  w_idx       <= rwb_reg_idx mod NB_GP_REGISTERS;
  rdw_idx     <= (r1_idx / 2) mod NB_GPDW_REGISTERS;
  dw2sw_rdata <= dw1_rdata(DATA_WIDTH - 1 downto 0) when (a_idx mod 2) = 0
                 else dw1_rdata(DATA_WIDTH * 2 - 1 downto DATA_WIDTH);

  sw_we   <= '1' when rwb_reg_we = '1' and rwb_reg_idx < NB_GP_REGISTERS  else '0';
  dw_we   <= '1' when rwb_reg_we = '1' and rwb_reg_idx >= NB_GP_REGISTERS else '0';
  wdw_idx <= (rwb_reg_idx / 2) mod NB_GPDW_REGISTERS;
  wdata   <= rwb_reg_wdata(DATA_WIDTH - 1 downto 0);

  r1_addr  <= std_logic_vector(to_unsigned(r1_idx, r1_addr'length));
  r2_addr  <= std_logic_vector(to_unsigned(r2_idx, r2_addr'length));
  w_addr   <= std_logic_vector(to_unsigned(w_idx, w_addr'length));
  rdw_addr <= std_logic_vector(to_unsigned(rdw_idx, rdw_addr'length));
  wdw_addr <= std_logic_vector(to_unsigned(wdw_idx, wdw_addr'length));

  gprs_1 : sc_sram
    generic map (
      ADDR_WIDTH       => nb_gp_bits,
      DATA_WIDTH       => DATA_WIDTH,
      READ_UNDER_WRITE => false,
      LOOKAHEAD        => true,
      DEBUG_NAME       => "gprs1",
      DEBUG            => DEBUG)
    port map (
      clock => clk,
      raddr => r1_addr,
      waddr => w_addr,
      data  => wdata,
      rren  => '1',
      wren  => sw_we,
      q     => sw1_rdata);

  gprs_2 : sc_sram
    generic map (
      ADDR_WIDTH       => nb_gp_bits,
      DATA_WIDTH       => DATA_WIDTH,
      READ_UNDER_WRITE => false,
      LOOKAHEAD        => true,
      DEBUG_NAME       => "gprs2",
      DEBUG            => DEBUG)
    port map (
      clock => clk,
      raddr => r2_addr,
      waddr => w_addr,
      data  => wdata,
      rren  => '1',
      wren  => sw_we,
      q     => sw2_rdata);

  gpdwrs_1 : sc_sram
    generic map (
      ADDR_WIDTH       => nb_gpdw_bits,
      DATA_WIDTH       => DATA_WIDTH * 2,
      READ_UNDER_WRITE => false,
      LOOKAHEAD        => true,
      DEBUG_NAME       => "gpdwrs",
      DEBUG            => DEBUG)
    port map (
      clock => clk,
      raddr => rdw_addr,
      waddr => wdw_addr,
      data  => rwb_reg_wdata,
      rren  => '1',
      wren  => dw_we,
      q     => dw1_rdata);

  process(rst, a_idx, b_idx, sw1_rdata, sw2_rdata, dw2sw_rdata)
  begin
    if rst = '1' then
      q_a <= (others => 'X');
      q_b <= (others => 'X');
    else
      if a_idx < NB_GP_REGISTERS then
        q_a <= sw1_rdata;
      else
        q_a <= dw2sw_rdata;
      end if;
      if b_idx < NB_GP_REGISTERS then
        q_b <= sw2_rdata;
      else
        q_b <= dw2sw_rdata;
      end if;
    end if;
  end process;

end architecture str;
