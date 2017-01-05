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

--------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity regfile_memory is
  generic (
    DATA_WIDTH : positive;
    NB_ENTRIES : positive
    );
  port (
    clk       : in  std_logic;
    stall_req : in  std_logic;
    ridx      : in  natural range 0 to NB_ENTRIES - 1;
    rdata     : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    we        : in  std_logic;
    widx      : in  natural range 0 to NB_ENTRIES - 1;
    wdata     : in  std_logic_vector(DATA_WIDTH - 1 downto 0)
    );
end entity regfile_memory;

architecture str of regfile_memory is
  type r_array is array (0 to NB_ENTRIES - 1) of
    std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal mem : r_array := (others => (others => '0'));

begin  -- architecture str
  process(clk, stall_req)
  begin
    if rising_edge(clk) then
      if we = '1' then
        mem(widx) <= wdata;
      end if;
      if stall_req = '0' then
        rdata <= mem(ridx);
      end if;
    end if;
  end process;
end architecture str;

library ieee;
use ieee.std_logic_1164.all;

entity RegisterFile is
  generic (
    DATA_WIDTH        : positive := 32;
    NB_GP_REGISTERS   : positive := 32;  -- r0 to r31
    NB_GPDW_REGISTERS : positive := 2    -- double width registers :
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

architecture rtl of RegisterFile is
  signal r1_idx      : natural range 0 to NB_GP_REGISTERS - 1;
  signal r2_idx      : natural range 0 to NB_GP_REGISTERS - 1;
  signal w_idx       : natural range 0 to NB_GP_REGISTERS - 1;
  signal rdw1_idx    : natural range 0 to NB_GPDW_REGISTERS - 1;
  signal wdw_idx     : natural range 0 to NB_GPDW_REGISTERS - 1;
  signal sw1_rdata   : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal sw2_rdata   : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal dw1_rdata   : std_logic_vector(DATA_WIDTH * 2 - 1 downto 0);
  signal dw2sw_rdata : std_logic_vector(DATA_WIDTH - 1 downto 0);

  signal sw_we : std_logic;
  signal dw_we : std_logic;
  signal wdata : std_logic_vector(DATA_WIDTH - 1 downto 0);

begin  -- architecture rtl
  r1_idx      <= a_idx mod NB_GP_REGISTERS;
  r2_idx      <= b_idx mod NB_GP_REGISTERS;
  w_idx       <= rwb_reg_idx mod NB_GP_REGISTERS;
  rdw1_idx    <= (r1_idx / 2) mod NB_GPDW_REGISTERS;
  dw2sw_rdata <= dw1_rdata(DATA_WIDTH - 1 downto 0) when (a_idx mod 2) = 0
                 else dw1_rdata(DATA_WIDTH * 2 - 1 downto DATA_WIDTH);

  sw_we   <= '1' when rwb_reg_we = '1' and rwb_reg_idx < NB_GP_REGISTERS  else '0';
  dw_we   <= '1' when rwb_reg_we = '1' and rwb_reg_idx >= NB_GP_REGISTERS else '0';
  wdw_idx <= (rwb_reg_idx / 2) mod NB_GPDW_REGISTERS;
  wdata   <= rwb_reg_wdata(DATA_WIDTH - 1 downto 0);

  gprs_1 : entity work.regfile_memory
    generic map (
      DATA_WIDTH => DATA_WIDTH,
      NB_ENTRIES => NB_GP_REGISTERS)
    port map (
      clk       => clk,
      stall_req => stall_req,
      ridx      => r1_idx,
      rdata     => sw1_rdata,
      we        => sw_we,
      widx      => w_idx,
      wdata     => wdata);
  gprs_2 : entity work.regfile_memory
    generic map (
      DATA_WIDTH => DATA_WIDTH,
      NB_ENTRIES => NB_GP_REGISTERS)
    port map (
      clk       => clk,
      stall_req => stall_req,
      ridx      => r2_idx,
      rdata     => sw2_rdata,
      we        => sw_we,
      widx      => w_idx,
      wdata     => wdata);
  gpdwrs_1 : entity work.regfile_memory
    generic map (
      DATA_WIDTH => DATA_WIDTH * 2,
      NB_ENTRIES => NB_GPDW_REGISTERS)
    port map (
      clk       => clk,
      stall_req => stall_req,
      ridx      => rdw1_idx,
      rdata     => dw1_rdata,
      we        => dw_we,
      widx      => rdw1_idx,
      wdata     => rwb_reg_wdata);

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

end architecture rtl;

