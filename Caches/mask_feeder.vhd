-------------------------------------------------------------------------------
-- Title      : Feeds all first set bits of a mask
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : mask_feeder.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-21
-- Last update: 2016-12-21
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
--   Mask outputing the first set bit to clear on fbitset.
--   Each cycle, bclr bit is cleared if bclrena is set.
--   Priority of operation :
--     1) sclr : latch sdata
--     2) bclrean : clear one bit
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-12-21  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

-------------------------------------------------------------------------------

entity mask_feeder is

  generic (
    WIDTH : natural := 8
    );

  port (
    clk : in std_logic;

    sclr  : in std_logic;
    sdata : in std_logic_vector(WIDTH - 1 downto 0);

    bclrena : in std_logic;
    bclr    : in natural range 0 to WIDTH - 1;

    fbitset  : out natural range 0 to WIDTH - 1;
    allclear : out std_logic;

    dbg_data : out std_logic_vector(WIDTH - 1 downto 0)
    );

end entity mask_feeder;

-------------------------------------------------------------------------------

architecture str of mask_feeder is

  constant ZERO_DATA : std_logic_vector(WIDTH - 1 downto 0) := (others => '0');
  signal data        : std_logic_vector(WIDTH - 1 downto 0);

  function get_fbs(vect : std_logic_vector) return natural is
    variable hlen : natural;
    variable hzero : std_logic_vector(vect'length / 2 - 1 downto 0);
    variable vlow : std_logic_vector(vect'length / 2 - 1 downto 0);
    variable vhigh : std_logic_vector(vect'length / 2 - 1 downto 0);
  begin
    hzero := (others => '0');
    if vect'length = 2 then
      if vect(1) = '1' then
        return 1;
      else
        return 0;
      end if;
    else
      hlen := vect'length / 2;
      vlow := vect(hlen - 1 downto 0);
      vhigh := vect(vect'length - 1 downto hlen);
      if vhigh /= hzero then
        return hlen + get_fbs(vhigh);
      else
        return get_fbs(vlow);
      end if;
    end if;
  end function get_fbs;

begin  -- architecture str

  process(clk, sclr, bclrena, data)
    variable next_data : std_logic_vector(WIDTH - 1 downto 0);
  begin
    if rising_edge(clk) then
      if sclr = '1' then
        data <= sdata;
        fbitset <= get_fbs(sdata);
      elsif bclrena = '1' then
        data(bclr) <= '0';
        next_data := data;
        next_data(bclr) := '0';
        fbitset <= get_fbs(next_data);
      end if;
    end if;

    if data = ZERO_DATA then
      allclear <= '1';
    else
      allclear <= '0';
    end if;

    dbg_data <= data;
  end process;

end architecture str;

-------------------------------------------------------------------------------
