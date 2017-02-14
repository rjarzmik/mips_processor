-------------------------------------------------------------------------------
-- Title      : Memory that is simulated with predefined values
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : Simulated_Memory.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
--            : Simon Desfarges <simon.desfarges@free.fr>
-- Company    : 
-- Created    : 2016-11-20
-- Last update: 2017-02-14
-- Platform   : 
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description: Simulates a constant latency memory.
--              The memory content is loaded from a file, with init_ram(), and
--              that part requires VHDL 2008 compliance.
--              If the rom is hard encoded in this file, the file should be
--              VHDL'93 compliant.
--
--              It is assumed that a "memory_data.txt" file is available, and
--              that is contains lines of data as would have been generated by
--              hexdump -e '"%08x\n"' bin_opcodes.raw > memory_data.txt
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-20  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library std;
use std.textio.all;

-------------------------------------------------------------------------------

entity Simulated_Memory is

  generic (
    ADDR_WIDTH        : integer := 32;
    DATA_WIDTH        : integer := 32;
    MEMORY_LATENCY    : positive := 1;
    MEMORY_ADDR_WIDTH : natural := 7;
    MEMORY_FILE       : string  := "memory_data.txt";
    DEBUG             : boolean := false
    );

  port (
    clk : in std_logic;
    rst : in std_logic;

    i_memory_req        : in  std_logic;
    i_memory_we         : in  std_logic;
    i_memory_addr       : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_memory_write_data : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_memory_read_data  : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_memory_valid      : out std_logic
    );

end entity Simulated_Memory;

-------------------------------------------------------------------------------

architecture rtl of Simulated_Memory is

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  subtype data_t is std_logic_vector(DATA_WIDTH - 1 downto 0);

  type memory is array(0 to 2**(MEMORY_ADDR_WIDTH - DATA_WIDTH / 8) - 1) of
    data_t;
  --constant rom : memory := (
    --x"24040011",  --   0:       24040011        li      a0,17
    --x"2c820002",  --   4:       2c820002        sltiu   v0,a0,2
    --x"1440000b",  --   8:       1440000b        bnez    v0,38 <fibo_flat+0x38>
    --x"24030001",  --   c:       24030001        li      v1,1
    --x"00003021",  --  10:       00003021        move    a2,zero
    --x"08000008",  --  14:       08000008        j       20 <fibo_flat+0x20>
    --x"24050001",  --  18:       24050001        li      a1,1
    --x"00402821",  --  1c:       00402821        move    a1,v0
    --x"24630001",  --  20:       24630001        addiu   v1,v1,1
    --x"00c51021",  --  24:       00c51021        addu    v0,a2,a1
    --x"1483fffc",  --  28:       1483fffc        bne     a0,v1,1c <fibo_flat+0x1c>
    --x"00a03021",  --  2c:       00a03021        move    a2,a1
    --x"03e00008",  --  30:       03e00008        jr      ra
    --x"00200825",  --  34:       00200825        move    at,at
    --x"03e00008",  --  38:       03e00008        jr      ra
    --x"00801021",  --  3c:       00801021        move    v0,a0
    --others => (others => '0')
  --);
  --function init_ram_data_offsets_addr(step: natural; ofs : natural) return memory is
  --  variable o : memory;
  --  variable d : natural;
  --begin
  --  for i in o'range loop
  --    for j in 0 to (DATA_WIDTH / step) - 1 loop
  --      d := (i * DATA_WIDTH / 8 + ofs + j * step / 8); -- mod 2**memory(0)'length;
  --      o(i)(step * (j + 1) - 1 downto step * j) := std_logic_vector(to_unsigned(d, step));
  --    end loop;
  --  end loop;
  --  return o;
  --end function init_ram_data_offsets_addr;
  --signal ram : memory := init_ram_data_offsets_addr(32, 16#0100#);

  impure function init_ram(FileName : string)
    return memory is

    variable tmp         : memory := (others => (others => '0'));
    file FileHandle      : text open read_mode is FileName;
    variable CurrentLine : line;
    variable TempWord    : bit_vector(DATA_WIDTH - 1 downto 0);
    variable good        : boolean;

  begin
    for addr_pos in 0 to 2**(MEMORY_ADDR_WIDTH - DATA_WIDTH / 8) - 1 loop
      exit when endfile(FileHandle);

      good := false;
      while not good and not endfile(FileHandle) loop
        readline(FileHandle, CurrentLine);
        hread(CurrentLine, TempWord, good);
      end loop;

      tmp(addr_pos) := To_StdLogicVector(TempWord);
    end loop;
    return tmp;
  end init_ram;
  signal ram : memory := init_ram(MEMORY_FILE);

  type state_t is (idle, read_done, write_done, latency_wait);

  function get_done_state(req : std_logic; we : std_logic) return state_t is
  begin
    if req = '1' then
      if we = '0' then
        return read_done;
      else
        return write_done;
      end if;
    else
      return idle;
    end if;
  end function get_done_state;

  function read_ram(addr : addr_t;
                    signal mem : in memory) return data_t is
  begin
    return mem((to_integer(unsigned(addr)) / (DATA_WIDTH / 8)));
  end function read_ram;

  procedure write_ram(addr : addr_t; wdata : data_t;
                      signal mem : out memory) is
  begin
    mem((to_integer(unsigned(addr)) / (DATA_WIDTH / 8))) <= wdata;
  end procedure write_ram;

  procedure do_memory_op(addr : addr_t; we : std_logic;
                         rdata : out data_t;
                         wdata : in data_t;
                         signal mem : inout memory) is
  begin
    if we = '0' then
      rdata := read_ram(addr, mem);

      -- pragma translate_off
      if DEBUG then
        report "Simulated_Memory: read[0x" & to_hstring(addr) &
          "] => 0x" & to_hstring(rdata);
      end if;
      -- pragma translate_on
    else
      write_ram(addr, wdata, mem);

      -- pragma translate_off
      if DEBUG then
          report "Simulated_Memory: write [0x" & to_hstring(addr) &
            "] <= 0x" & to_hstring(wdata);
      end if;
      -- pragma translate_on
    end if;
  end procedure do_memory_op;

begin  -- architecture rtl

  handler : process(rst, clk, i_memory_req, i_memory_we, i_memory_addr)
    variable mreq   : std_logic;
    variable mwe    : std_logic;
    variable maddr  : addr_t;
    variable rdata  : data_t;
    variable wdata  : data_t;
    variable valid  : std_logic;
    variable state  : state_t := idle;
    variable waits  : natural;
  begin

    if rst = '1' then
      valid := '0';
      rdata := (others => 'X');
    else
      case state is
        when idle =>
          if MEMORY_LATENCY = 0 and i_memory_req = '1' then
            valid := '1';
            do_memory_op(i_memory_addr, i_memory_we, rdata, wdata, ram);
          elsif MEMORY_LATENCY = 1 and i_memory_req = '1' then
            if rising_edge(clk) then
              do_memory_op(i_memory_addr, i_memory_we, rdata, wdata, ram);
              valid := '1';
              state := get_done_state(i_memory_req, i_memory_we);
            end if;
          elsif MEMORY_LATENCY > 1 and i_memory_req = '1' then
            if rising_edge(clk) then
              state := latency_wait;
              maddr := i_memory_addr;
              mwe   := i_memory_we;
              wdata := i_memory_write_data;
              waits := MEMORY_LATENCY - 1;
              valid := '0';
              rdata := (others => 'X');
            end if;
          end if;

        when read_done | write_done =>
          if MEMORY_LATENCY = 1 and i_memory_req = '1' then
            if rising_edge(clk) then
              do_memory_op(i_memory_addr, i_memory_we, rdata, wdata, ram);
              valid := '1';
              state := get_done_state(i_memory_req, i_memory_we);
            end if;
          elsif MEMORY_LATENCY = 1 and i_memory_req = '0' then
            if rising_edge(clk) then
              state := idle;
            end if;
          elsif MEMORY_LATENCY > 1 and i_memory_req = '1' then
            if rising_edge(clk) then
              state := latency_wait;
              maddr := i_memory_addr;
              mwe   := i_memory_we;
              wdata := i_memory_write_data;
              waits := MEMORY_LATENCY - 1;
              valid := '0';
              rdata := (others => 'X');
            end if;
          elsif MEMORY_LATENCY > 1 and i_memory_req = '0' then
            if rising_edge(clk) then
              state := idle;
            end if;
          end if;

        when latency_wait =>
          valid := '0';
          rdata := (others => 'X');
          if rising_edge(clk) then
            waits := waits - 1;
            if waits = 0 then
              state := get_done_state('1', mwe);
              valid := '1';
              do_memory_op(maddr, mwe, rdata, wdata, ram);
            end if;
          end if;
      end case;
    end if;

    o_memory_read_data <= rdata;
    o_memory_valid <= valid;
  end process handler;

end architecture rtl;

-------------------------------------------------------------------------------
