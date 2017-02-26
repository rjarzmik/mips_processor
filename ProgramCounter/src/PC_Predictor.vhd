-------------------------------------------------------------------------------
-- Title      : Predicts the next program counters values to be used
-- Project    : MIPS Processor
-------------------------------------------------------------------------------
-- File       : PC_Predictor.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2016-12-10
-- Last update: 2018-08-03
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
--   Predicts the next program counter.
-- Behavior :
--   - on next clk rising edge :
--     prediction_addr <= prediction(saddr)
--     prediction_disrupt <= prediction(saddr)
-------------------------------------------------------------------------------
-- Copyright (c) 2017 Robert Jarzmik <robert.jarzmik@free.fr>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2016-12-10  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity PC_Predictor is
  generic (
    ADDR_WIDTH : integer;
    STEP       : integer
    );

  port (
    clk                  : in  std_logic;
    current              : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    -- Output prediction
    o_prediction_addr    : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_prediction_disrupt : out std_logic
    );
end entity PC_Predictor;

architecture noprediction of PC_Predictor is
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);

  signal predict_addr : addr_t;
begin
  predictor : process(clk, current)
  begin
    if rising_edge(clk) then
      predict_addr <= std_logic_vector(unsigned(current) + STEP);
    end if;
  end process predictor;

  o_prediction_addr <= predict_addr;
  o_prediction_disrupt <= '0';

end architecture noprediction;
