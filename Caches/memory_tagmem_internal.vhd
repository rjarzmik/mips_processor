-------------------------------------------------------------------------------
-- Title      : Tags memory with arrays implementation
-- Project    : MIPS processor implementation, compatible MIPS-1
-------------------------------------------------------------------------------
-- File       : memory_tagmem_internal.vhd
-- Author     : Robert Jarzmik (Intel)  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-15
-- Last update: 2016-12-30
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-12-15  1.0      rjarzmik        Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cache_defs.tag_entry_t;
use work.cache_defs.TAG_ENTRY_EMPTY;

-------------------------------------------------------------------------------

entity memory_tagmem_internal is
  generic
    (
      ADDR_WIDTH : integer := 7;
      DEBUG_IDX  : natural := 0;
      DEBUG      : boolean := false
      );
  port
    (
      clock : in  std_logic;
      raddr : in  std_logic_vector (ADDR_WIDTH - 1 downto 0);
      waddr : in  std_logic_vector (ADDR_WIDTH - 1 downto 0);
      data  : in  tag_entry_t;
      rren  : in  std_logic;
      wren  : in  std_logic;
      q     : out tag_entry_t := TAG_ENTRY_EMPTY
      );

end entity memory_tagmem_internal;

architecture infer of memory_tagmem_internal is
  constant blen : natural := ((TAG_ENTRY_EMPTY.tag'length +
                               TAG_ENTRY_EMPTY.valids'length +
                               TAG_ENTRY_EMPTY.dirtys'length +
                               TAG_ENTRY_EMPTY.ctxt'length + 7) / 8) * 8;

  type mem_block_t is array(0 to 2**ADDR_WIDTH - 1) of
    std_logic_vector (blen - 1 downto 0);
  constant MEMORY_RESET : mem_block_t := (others => (others => '0'));
  signal memory         : mem_block_t := MEMORY_RESET;

  signal raddr_reg : std_logic_vector (ADDR_WIDTH - 1 downto 0) := (others => '0');
  signal rdata     : tag_entry_t;

  signal bypass_waddr : std_logic_vector (ADDR_WIDTH - 1 downto 0);
  signal bypass_wren  : std_logic;
  signal bypass_wdata : tag_entry_t;

  function to_tag_entry_t(slv : std_logic_vector(blen - 1 downto 0))
    return tag_entry_t is
    variable o : tag_entry_t;
    variable i : natural := 0;
  begin
    o.tag    := slv(i + o.tag'length - 1 downto i);
    i        := i + o.tag'length;
    o.valids := slv(i + o.valids'length - 1 downto i);
    i        := i + o.valids'length;
    o.dirtys := slv(i + o.dirtys'length - 1 downto i);
    i        := i + o.dirtys'length;
    o.ctxt   := slv(i + o.ctxt'length - 1 downto i);
    return o;
  end function to_tag_entry_t;

  function to_std_logic_vector(te : tag_entry_t) return std_logic_vector is
    variable slv : std_logic_vector(blen - 1 downto 0);
    variable i   : natural := 0;
  begin
    slv(i + te.tag'length - 1 downto i)    := te.tag;
    i                                      := i + te.tag'length;
    slv(i + te.valids'length - 1 downto i) := te.valids;
    i                                      := i + te.valids'length;
    slv(i + te.dirtys'length - 1 downto i) := te.dirtys;
    i                                      := i + te.dirtys'length;
    slv(i + te.ctxt'length - 1 downto i)   := te.ctxt;
    return slv;
  end function to_std_logic_vector;

begin  -- architecture str

  process(clock, memory, raddr_reg, waddr, wren, data, rdata,
          bypass_waddr, bypass_wren, bypass_wdata)
  begin
    if rising_edge(clock) then
      if rren = '1' then
        raddr_reg <= raddr;
      end if;

      bypass_wren  <= wren;
      bypass_waddr <= waddr;
      bypass_wdata <= data;

      if wren = '1' then
        memory(to_integer(unsigned(waddr))) <= to_std_logic_vector(data);

        -- pragma translate_off
        if DEBUG then
          report "Tmem(" & integer'image(DEBUG_IDX) & "): [0x" & to_hstring(waddr) & "] <= (" &
            "tag=" & to_hstring(data.tag) & " ctxt=" & to_hstring(data.ctxt) &
            " valids=" & to_bstring(data.valids) &
            " dirtys=" & to_bstring(data.dirtys) & ")";
        end if;
      -- pragma translate_on
      end if;
    end if;

    rdata <= to_tag_entry_t(memory(to_integer(unsigned(raddr_reg))));
    if raddr_reg = bypass_waddr and bypass_wren = '1' then
      q <= bypass_wdata;
    else
      q <= rdata;
    end if;
  end process;

end architecture infer;

