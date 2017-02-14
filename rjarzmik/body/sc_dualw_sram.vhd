-------------------------------------------------------------------------------
-- Title      : Single clock dual width static RAM
-- Project    : Memory
-------------------------------------------------------------------------------
-- File       : sc_dualw_sram.vhd
-- Author     : Robert Jarzmik <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2017-02-02
-- Last update: 2017-02-13
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
--   The single clock SRAM provides memory read and write operations.
--   The read data output is registered.
--   The biggest data width must be a perfect power-of-2 multiple of DATA_WIDTH.
--   The ADDR_WIDTH is relative to DATA_WIDTH, ie. each address stores one
--   DATA_WIDTH word.
--
-- Mode of operation :
--  - if LOOKAHEAD = false, on clock rise and rren = '1', q is registered with
--    the value of the element at raddr
--  - if LOOKAHEAD = true, q always shows the element at raddr, and it is
--    assumed raddr is registered
--
--  Read operation
--    - assert rren = '1' and raddr
--    - on next clock rising edge, q has the read data
--  Write operation
--    - assert wren = '1', waddr and data
--    - on next clock rising edge, data is written to memory
--
-- Bypassing, aka. read under write behavior :
--   If reading and writting to the same address in the same clock cycle, the
--   registered q output will be either :
--     - the old memory data if READ_UNDER_WRITE = false
--     - the data from "data" input bypassed to q if READ_UNDER_WRITE = true
--
-- Dual width :
--   - either wide = '0', and writes occur on DATA_WIDTH basis on waddr, from
--     data_small
--   - or wide = '1', and writes occur on data_wide aligned on the
--     power-of-2 multiple
-------------------------------------------------------------------------------
-- Copyright (c) 2017 Robert Jarzmik <robert.jarzmik@free.fr>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2017-02-02  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library rjarzmik;
use rjarzmik.memories.sc_sram;
use rjarzmik.slv_utils.slv_is_x;

entity sc_dualw_sram is
  generic
    (
      ADDR_WIDTH       : positive;
      DATA_WIDTH       : positive;
      WIDTH_WIDENER    : natural;
      READ_UNDER_WRITE : boolean  := true;
      LOOKAHEAD        : boolean  := false;
      DEBUG_NAME       : string   := "";
      DEBUG            : boolean  := false
      );
  port
    (
      clock      : in  std_logic;
      wide       : in  std_logic;
      raddr      : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
      waddr      : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
      data_small : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
      data_wide  : in  std_logic_vector(DATA_WIDTH * 2**WIDTH_WIDENER - 1 downto 0);
      rren       : in  std_logic;
      wren       : in  std_logic;
      q_small    : out std_logic_vector(DATA_WIDTH - 1 downto 0)                    := (others => '0');
      q_wide     : out std_logic_vector(DATA_WIDTH * 2**WIDTH_WIDENER - 1 downto 0) := (others => '0')
      );
end entity sc_dualw_sram;

architecture str of sc_dualw_sram is
  signal wren_mask : std_logic_vector(0 to 2**WIDTH_WIDENER - 1);
  signal q         : std_logic_vector(DATA_WIDTH * 2**WIDTH_WIDENER - 1 downto 0);
  signal data      : std_logic_vector(DATA_WIDTH * 2**WIDTH_WIDENER - 1 downto 0);

  function get_sub_addr(addr : std_logic_vector(ADDR_WIDTH - 1 downto 0))
    return natural is
  begin
    if not slv_is_x(addr) and WIDTH_WIDENER > 0 then
      return to_integer(unsigned(addr(WIDTH_WIDENER - 1 downto 0)));
    else
      return 0;
    end if;
  end function get_sub_addr;
                          
begin  -- architecture str

  masker : process(wide, waddr, wren) is
    variable subaddr : natural;
  begin
    subaddr := get_sub_addr(waddr);
    for i in 0 to 2**WIDTH_WIDENER - 1 loop
      if subaddr = i or wide = '1' then
        wren_mask(i) <= wren;
      else
        wren_mask(i) <= '0';
      end if;
    end loop;
  end process masker;

  q_p : process(q, raddr, waddr)
    variable subaddr : natural;
  begin
    if not slv_is_x(raddr) then
      subaddr := get_sub_addr(raddr);
      q_wide  <= q;
      q_small <= q(DATA_WIDTH * (subaddr + 1) - 1 downto DATA_WIDTH * subaddr);
    end if;
  end process q_p;

  data_p : process(wide, waddr, data_wide, data_small)
  begin
    if not slv_is_x(waddr) then
      if wide = '1' then
        data <= data_wide;
      else
        for i in 0 to 2**WIDTH_WIDENER - 1 loop
          data(DATA_WIDTH * (i + 1) - 1 downto DATA_WIDTH * i) <= data_small;
        end loop;
      end if;
    end if;
  end process data_p;

  scs : for i in 0 to 2**WIDTH_WIDENER - 1 generate
    sram : sc_sram
      generic map (
        ADDR_WIDTH       => ADDR_WIDTH - WIDTH_WIDENER,
        DATA_WIDTH       => DATA_WIDTH,
        READ_UNDER_WRITE => READ_UNDER_WRITE,
        LOOKAHEAD        => LOOKAHEAD,
        DEBUG_NAME       => DEBUG_NAME & "_d" & integer'image(i),
        DEBUG            => DEBUG)
      port map (
        clock => clock,
        raddr => raddr(ADDR_WIDTH - 1 downto WIDTH_WIDENER),
        waddr => waddr(ADDR_WIDTH - 1 downto WIDTH_WIDENER),
        data  => data(DATA_WIDTH * (i + 1) - 1 downto DATA_WIDTH * i),
        rren  => rren,
        wren  => wren_mask(i),
        q     => q(DATA_WIDTH * (i + 1) - 1 downto DATA_WIDTH * i));
  end generate;
end architecture str;
