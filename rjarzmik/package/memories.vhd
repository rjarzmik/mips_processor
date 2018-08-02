-------------------------------------------------------------------------------
-- Title      : FIFOs utilities
-- Project    : FIFO
-------------------------------------------------------------------------------
-- File       : fifos.vhd
-- Author     : Robert Jarzmik <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2017-01-31
-- Last update: 2018-08-02
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Various fifos
-------------------------------------------------------------------------------
-- Copyright (c) 2017 Robert Jarzmik <robert.jarzmik@free.fr>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2017-01-31  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package memories is
  component sc_sram is
    generic (
      ADDR_WIDTH       : positive;
      DATA_WIDTH       : positive;
      READ_UNDER_WRITE : boolean;
      LOOKAHEAD        : boolean;
      DEBUG_NAME       : string := "";
      DEBUG            : boolean);
    port (
      clock : in  std_logic;
      raddr : in  std_logic_vector (ADDR_WIDTH - 1 downto 0);
      waddr : in  std_logic_vector (ADDR_WIDTH - 1 downto 0);
      data  : in  std_logic_vector (DATA_WIDTH - 1 downto 0);
      rren  : in  std_logic;
      wren  : in  std_logic;
      q     : out std_logic_vector (DATA_WIDTH - 1 downto 0));
  end component sc_sram;

  component sc_dp_sram is
    generic (
      ADDR_WIDTH       : positive;
      DATA_WIDTH       : positive;
      READ_UNDER_WRITE : boolean;
      LOOKAHEAD        : boolean;
      DEBUG_NAME       : string;
      DEBUG            : boolean);
    port (
      clock  : in  std_logic;
      raddr1 : in  std_logic_vector (ADDR_WIDTH - 1 downto 0) := (others => '0');
      raddr2 : in  std_logic_vector (ADDR_WIDTH - 1 downto 0) := (others => '0');
      waddr  : in  std_logic_vector (ADDR_WIDTH - 1 downto 0);
      data   : in  std_logic_vector (DATA_WIDTH - 1 downto 0);
      rren1  : in  std_logic;
      rren2  : in  std_logic;
      wren   : in  std_logic;
      q1     : out std_logic_vector (DATA_WIDTH - 1 downto 0) := (others => '0');
      q2     : out std_logic_vector (DATA_WIDTH - 1 downto 0) := (others => '0'));
  end component sc_dp_sram;

  component sc_dualw_sram is
    generic (
      ADDR_WIDTH       : positive;
      DATA_WIDTH       : positive;
      WIDTH_WIDENER    : natural;
      READ_UNDER_WRITE : boolean;
      LOOKAHEAD        : boolean;
      DEBUG_NAME       : string;
      DEBUG            : boolean);
    port (
      clock      : in  std_logic;
      wide       : in  std_logic;
      raddr      : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
      waddr      : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
      data_small : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
      data_wide  : in  std_logic_vector(DATA_WIDTH * 2**WIDTH_WIDENER - 1 downto 0);
      rren       : in  std_logic;
      wren       : in  std_logic;
      q_small    : out std_logic_vector(DATA_WIDTH - 1 downto 0)                    := (others => '0');
      q_wide     : out std_logic_vector(DATA_WIDTH * 2**WIDTH_WIDENER - 1 downto 0) := (others => '0'));
  end component sc_dualw_sram;
end package memories;
