-------------------------------------------------------------------------------
-- Title      : Decode and Issue instruction
-- Project    : 
-------------------------------------------------------------------------------
-- File       : Decode.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-12
-- Last update: 2017-01-05
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Decode and Issue a MIPS instruction
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-12  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cpu_defs.all;
use work.instruction_defs.all;

-------------------------------------------------------------------------------

entity Decode is

  generic (
    ADDR_WIDTH           : integer  := 32;
    DATA_WIDTH           : integer  := 32;
    NB_REGISTERS         : positive := 34;
    NB_REGISTERS_SPECIAL : positive := 2;
    REG_IDX_MFLO         : natural  := 32;
    REG_IDX_MFHI         : natural  := 33
    );

  port (
    clk            : in  std_logic;
    rst            : in  std_logic;
    stall_req      : in  std_logic;     -- stall current instruction
    kill_req       : in  std_logic;     -- kill current instruction
    i_instruction  : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    i_pc           : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_instr_tag    : in  instr_tag_t;
    --- Writeback input
    i_rwb_reg1     : in  register_port_type;
    i_rwb_reg2     : in  register_port_type;
    --- Bypass input
    i_bp_reg1      : in  register_port_type;
    i_bp_reg2      : in  register_port_type;
    --- Outputs
    o_alu_op       : out alu_op_type;
    o_reg1         : out register_port_type;
    o_reg2         : out register_port_type;
    o_jump_target  : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_jump_op      : out jump_type;
    o_mem_data     : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_mem_op       : out memory_op_type;
    o_divide_0     : out std_logic;  -- if set, a division attempt will be a X/0
    o_instr_tag    : out instr_tag_t;
    --- Control and bypass outputs
    o_src_reg1_idx : out natural range 0 to NB_REGISTERS - 1;
    o_src_reg2_idx : out natural range 0 to NB_REGISTERS - 1;
    -- Debug signal
    i_dbg_di_pc    : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_dbg_di_pc    : out std_logic_vector(ADDR_WIDTH - 1 downto 0)
    );

  constant op_rtype : std_logic_vector(5 downto 0) := "000000";

  constant op_addi  : std_logic_vector(5 downto 0) := "001000";
  constant op_addiu : std_logic_vector(5 downto 0) := "001001";
  constant op_slti  : std_logic_vector(5 downto 0) := "001010";
  constant op_sltiu : std_logic_vector(5 downto 0) := "001011";
  constant op_andi  : std_logic_vector(5 downto 0) := "001100";
  constant op_ori   : std_logic_vector(5 downto 0) := "001101";
  constant op_xori  : std_logic_vector(5 downto 0) := "001110";

  constant op_lui : std_logic_vector(5 downto 0) := "001111";
  constant op_lb  : std_logic_vector(5 downto 0) := "100000";
  constant op_lw  : std_logic_vector(5 downto 0) := "100011";
  constant op_lbu : std_logic_vector(5 downto 0) := "100100";
  constant op_sb  : std_logic_vector(5 downto 0) := "101000";
  constant op_sw  : std_logic_vector(5 downto 0) := "101011";

  constant op_beq  : std_logic_vector(5 downto 0) := "000100";
  constant op_bne  : std_logic_vector(5 downto 0) := "000101";
  constant op_blez : std_logic_vector(5 downto 0) := "000110";
  constant op_bgtz : std_logic_vector(5 downto 0) := "000111";
  constant op_bltz : std_logic_vector(5 downto 0) := "000001";

  constant op_j    : std_logic_vector(5 downto 0) := "000010";
  constant op_jalr : std_logic_vector(5 downto 0) := "000011";

  constant func_nop  : std_logic_vector(5 downto 0) := "000000";
  constant func_mul  : std_logic_vector(5 downto 0) := "011000";
  constant func_mulu : std_logic_vector(5 downto 0) := "011001";
  constant func_div  : std_logic_vector(5 downto 0) := "011010";
  constant func_divu : std_logic_vector(5 downto 0) := "011011";
  constant func_add  : std_logic_vector(5 downto 0) := "100000";
  constant func_addu : std_logic_vector(5 downto 0) := "100001";
  constant func_sub  : std_logic_vector(5 downto 0) := "100010";
  constant func_subu : std_logic_vector(5 downto 0) := "100011";
  constant func_slt  : std_logic_vector(5 downto 0) := "101010";
  constant func_sltu : std_logic_vector(5 downto 0) := "101011";
  constant func_and  : std_logic_vector(5 downto 0) := "100100";
  constant func_or   : std_logic_vector(5 downto 0) := "100101";
  constant func_nor  : std_logic_vector(5 downto 0) := "100111";
  constant func_xor  : std_logic_vector(5 downto 0) := "101000";
  constant func_jr   : std_logic_vector(5 downto 0) := "001000";
  constant func_jalr : std_logic_vector(5 downto 0) := "001001";
  constant func_mfhi : std_logic_vector(5 downto 0) := "010000";
  constant func_mflo : std_logic_vector(5 downto 0) := "010010";

end entity Decode;

-------------------------------------------------------------------------------

architecture rtl of Decode is
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  subtype data_t is std_logic_vector(DATA_WIDTH - 1 downto 0);

  alias ra : std_logic_vector(DATA_WIDTH - 1 downto 0) is o_reg1.data;
  alias rb : std_logic_vector(DATA_WIDTH - 1 downto 0) is o_reg2.data;

  signal alu_op : alu_op_type;

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  signal op_code      : std_logic_vector(5 downto 0);
  signal func         : std_logic_vector(5 downto 0);
  signal rsi          : natural range 0 to NB_REGISTERS - 1;
  signal rti          : natural range 0 to NB_REGISTERS - 1;
  signal rdi          : natural range 0 to NB_REGISTERS - 1;
  signal rfile_rs     : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal rfile_rt     : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal immediate    : signed(DATA_WIDTH / 2 - 1 downto 0);
  signal next_pc      : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal pc_displace  : std_logic_vector(25 downto 0);
  signal decode_error : std_logic;

  type reg_src is (bypassed_regfile, zero, immediate_unsigned, immediate_signextend);
  signal ra_src                : reg_src;
  signal rb_src                : reg_src;
  signal bp_rs                 : register_port_type;
  signal bp_rt                 : register_port_type;
  constant r0                  : data_t := (others => '0');
  signal rimmediate            : std_logic_vector(DATA_WIDTH / 2 - 1 downto 0);
  signal rimmediate_unsigned   : data_t;
  signal rimmediate_signextend : data_t;

  type jt_t is (jt_rs, jt_rt, jt_absolute, jt_pcrelative);
  signal jt_src             : jt_t;
  signal jt_addr_absolute   : addr_t;
  signal jt_addr_pcrelative : addr_t;
  signal jt_mux_addr        : addr_t;
  signal jt_mux_reg         : addr_t;

  signal rwb_reg_wdata : std_logic_vector(DATA_WIDTH * 2 - 1 downto 0);
begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------

  rwb_reg_wdata <= i_rwb_reg2.data & i_rwb_reg1.data;
  rfile : entity work.RegisterFile
    generic map (
      DATA_WIDTH           => DATA_WIDTH,
      NB_GP_REGISTERS      => NB_REGISTERS - NB_REGISTERS_SPECIAL,
      NB_GPDW_REGISTERS    => NB_REGISTERS_SPECIAL)
    port map (
      clk           => clk,
      rst           => rst,
      stall_req     => stall_req,
      a_idx         => rsi,
      b_idx         => rti,
      q_a           => rfile_rs,
      q_b           => rfile_rt,
      rwb_reg_we    => i_rwb_reg1.we,
      rwb_reg_idx   => i_rwb_reg1.idx,
      rwb_reg_wdata => rwb_reg_wdata
      );

  next_pc <= std_logic_vector(unsigned(i_pc) + 4);

  op_code <= i_instruction(31 downto 26);
  func    <= i_instruction(5 downto 0);

  immediate <= (others => '0') when rst = '1' else signed(i_instruction(15 downto 0));

  instr_tag_p : process(clk, rst, kill_req, stall_req, op_code, func,
                        i_instr_tag)
    variable is_br   : boolean;
    variable is_jr   : boolean;
    variable is_jump : boolean;
    variable itag    : instr_tag_t;
  begin
    itag := i_instr_tag;

    is_br := (op_code = op_beq) or (op_code = op_bne) or (op_code = op_blez) or
             (op_code = op_bgtz) or (op_code = op_bltz);
    is_jr   := (op_code = op_rtype) and (func = func_jr);
    is_jump := (op_code = op_j or op_code = op_jalr);

    itag := get_instr_change_is_branch(
      get_instr_change_is_ja(
        get_instr_change_is_jr(itag, is_jr), is_jump), is_br);

    if rst = '1' then
      o_instr_tag <= INSTR_TAG_NONE;
    elsif rising_edge(clk) then
      if kill_req = '1' then
        o_instr_tag <= INSTR_TAG_NONE;
      elsif stall_req = '1' then
      else
        o_instr_tag <= itag;
      end if;
    end if;
  end process instr_tag_p;

  alu : process(clk, rst, kill_req, stall_req, op_code, func, alu_op)
  begin
    if rst = '1' then
      alu_op <= all_zero;
    elsif rising_edge(clk) then
      if kill_req = '1' then
        alu_op <= all_zero;
      elsif stall_req = '1' then
      else
        case op_code is
          when op_beq | op_bne | op_blez | op_bgtz | op_bltz =>
            alu_op <= substract;
          when op_lw | op_lbu | op_lb | op_sw | op_sb =>
            alu_op <= add;
          when op_lui =>
            alu_op <= add;
          when op_addi | op_addiu => alu_op <= add;
          when op_slti | op_sltiu => alu_op <= slt;
          when op_andi            => alu_op <= log_and;
          when op_ori             => alu_op <= log_or;
          when op_xori            => alu_op <= log_xor;
          when op_rtype =>
            case func is
              when func_mul | func_mulu => alu_op <= multiply;
              when func_div | func_divu => alu_op <= divide;
              when func_add | func_addu => alu_op <= add;
              when func_sub | func_subu => alu_op <= substract;
              when func_slt | func_sltu => alu_op <= slt;
              when func_and             => alu_op <= log_and;
              when func_or              => alu_op <= log_or;
              when func_nor             => alu_op <= log_nor;
              when func_xor             => alu_op <= log_xor;
              when others               => alu_op <= all_zero;
            end case;
          when others => alu_op <= all_zero;
        end case;
      end if;
    end if;
    o_alu_op <= alu_op;
  end process alu;

  registers : process(clk, stall_req, op_code, func, i_instruction, rimmediate,
                      ra_src, rb_src, bp_rs, bp_rt, rfile_rs, rfile_rt,
                      rimmediate_unsigned, rimmediate_signextend)
    variable asrc, bsrc : reg_src;
  begin
    rsi <= to_integer(unsigned(i_instruction(25 downto 21)));
    rti <= to_integer(unsigned(i_instruction(20 downto 16)));
    rdi <= to_integer(unsigned(i_instruction(15 downto 11)));

    rimmediate_signextend(immediate'length - 1 downto 0) <= rimmediate;
    rimmediate_signextend(rimmediate_signextend'length - 1 downto rimmediate'length)
      <= (others => rimmediate(rimmediate'length - 1));
    rimmediate_unsigned(rimmediate'length - 1 downto 0) <= rimmediate;
    rimmediate_unsigned(rimmediate_unsigned'length - 1 downto rimmediate'length)
      <= (others => '0');

    if rising_edge(clk) and stall_req = '0' then
      rimmediate <= i_instruction(rimmediate'length - 1 downto 0);

      bp_rs <= i_bp_reg1;
      bp_rt <= i_bp_reg2;

      case op_code is
        when op_rtype =>
          asrc := bypassed_regfile;
          bsrc := bypassed_regfile;
        when op_addi | op_addiu | op_slti | op_sltiu |
          op_andi | op_ori | op_xori =>
          asrc := bypassed_regfile;
          bsrc := immediate_signextend;
        when op_lw | op_lbu | op_lb | op_sw | op_sb =>
          asrc := bypassed_regfile;
          bsrc := immediate_unsigned;
        when op_lui =>
          asrc := zero;
          bsrc := immediate_unsigned;
        when others =>
          asrc := bypassed_regfile;
          bsrc := bypassed_regfile;
      end case;
      ra_src <= asrc;
      rb_src <= bsrc;
    end if;

    if ra_src = bypassed_regfile or ra_src = zero then
      if ra_src = bypassed_regfile then
        if bp_rs.we = '1' then
          ra <= bp_rs.data;
        else
          ra <= rfile_rs;
        end if;
      else
        ra <= r0;
      end if;
    else
      ra <= r0;
    end if;

    if rb_src = bypassed_regfile or rb_src = zero then
      if rb_src = bypassed_regfile then
        if bp_rt.we = '1' then
          rb <= bp_rt.data;
        else
          rb <= rfile_rt;
        end if;
      else
        rb <= r0;
      end if;
    else
      if rb_src = immediate_unsigned then
        rb <= rimmediate_unsigned;
      else
        rb <= rimmediate_signextend;
      end if;
    end if;
  end process registers;

  rtargets : process(rst, clk, kill_req, stall_req, op_code, func, rsi, rti, rdi)
    variable reg1_we  : std_ulogic;
    variable reg1_idx : natural range 0 to NB_REGISTERS - 1;
    variable reg2_we  : std_ulogic;
    variable reg2_idx : natural range 0 to NB_REGISTERS - 1;
  begin
    reg1_we  := '0';
    reg2_we  := '0';
    reg1_idx := 0;
    reg2_idx := 0;

    case op_code is
      when op_rtype =>
        case func is
          when func_mul | func_mulu | func_div | func_divu =>
            reg1_we  := '1';
            reg1_idx := REG_IDX_MFLO;
            reg2_we  := '1';
            reg2_idx := REG_IDX_MFHI;
          when func_nop =>
          when others =>
            reg1_we  := '1';
            reg1_idx := rdi;
        end case;
      when op_addi | op_addiu | op_slti | op_sltiu |
        op_andi | op_ori | op_xori =>
        reg1_we  := '1';
        reg1_idx := rti;
      when op_lw | op_lbu | op_lb =>
        reg1_we  := '1';
        reg1_idx := rti;
      when op_lui =>
        reg1_we  := '1';
        reg1_idx := rti;
      when op_jalr =>
        reg1_we  := '1';
        reg1_idx := NB_REGISTERS - NB_REGISTERS_SPECIAL - 1;
      when others =>
    end case;

    if rst = '1' then
      o_reg1.we      <= '0';
      o_reg2.we      <= '0';
      o_reg1.idx     <= 0;
      o_reg2.idx     <= 0;
    else
      if rising_edge(clk) then
        if kill_req = '1' then
          o_reg1.we      <= '0';
          o_reg2.we      <= '0';
          o_reg1.idx     <= 0;
          o_reg2.idx     <= 0;
          o_src_reg1_idx <= 0;
          o_src_reg2_idx <= 0;
        elsif stall_req = '1' then
        else
          o_reg1.we      <= reg1_we;
          o_reg2.we      <= reg2_we;
          o_reg1.idx     <= reg1_idx;
          o_reg2.idx     <= reg2_idx;
        end if;
      end if;
    end if;
    o_src_reg1_idx <= rsi;
    o_src_reg2_idx <= rti;
  end process rtargets;

  memory_p : process(rst, clk, kill_req, stall_req, op_code, rimmediate_signextend)
    variable mo : memory_op_type;
  begin
    case op_code is
      when op_lw  => mo := loadw;
      when op_lbu => mo := load8;
      when op_lb  => mo := load8_signextend32;
      when op_sw  => mo := storew;
      when op_sb  => mo := store8;
      when others => mo := none;
    end case;

    if rst = '1' then
      o_mem_op <= none;
    elsif rising_edge(clk) then
      if kill_req = '1' then
        o_mem_op <= none;
      elsif stall_req = '1' then
      else
        o_mem_op <= mo;
      end if;
    end if;

    o_mem_data <= rimmediate_signextend;
  end process memory_p;

  jumper_op : process(rst, clk, stall_req, kill_req, op_code, func)
    variable jump_op : jump_type;
  begin
    case op_code is
      when op_rtype =>
        if func = func_jr or func = func_jalr then
          jump_op := always;
        else
          jump_op := none;
        end if;
      when op_beq =>
        jump_op := zero;
      when op_bne =>
        jump_op := non_zero;
      when op_blez =>
        jump_op := lesser_or_zero;
      when op_bgtz =>
        jump_op := greater;
      when op_bltz =>
        jump_op := lesser;
      when others =>
        jump_op := none;
    end case;

    if rst = '1' then
      o_jump_op <= none;
    elsif rising_edge(clk) then
      if kill_req = '1' then
        o_jump_op <= none;
      elsif stall_req = '1' then
      else
        o_jump_op <= jump_op;
      end if;
    end if;
  end process jumper_op;

  jumper_target : process(clk, stall_req, op_code, func, i_instruction,
                          next_pc, jt_mux_reg, jt_src,
                          jt_addr_pcrelative, jt_addr_absolute)
    variable src : jt_t;
  begin
    pc_displace <= i_instruction(23 downto 0) & b"00";

    case op_code is
      when op_rtype =>
        if func = func_jr then
          src := jt_rs;
        else
          src := jt_absolute;
        end if;
      when op_beq | op_bne | op_blez | op_bgtz | op_bltz =>
        src := jt_pcrelative;
      when others =>
        src := jt_absolute;
    end case;

    if rising_edge(clk) and stall_req = '0' then
      jt_src           <= src;
      jt_addr_absolute <= next_pc(ADDR_WIDTH - 1 downto pc_displace'length) & pc_displace;
      jt_addr_pcrelative <= std_logic_vector(unsigned(next_pc) +
                                             unsigned(resize(immediate * 4, ADDR_WIDTH)));
    end if;

    if jt_src = jt_absolute then
      jt_mux_reg <= jt_addr_absolute;
    else
      jt_mux_reg <= jt_addr_pcrelative;
    end if;

    if jt_src = jt_rs then
      o_jump_target <= ra;
    else
      o_jump_target <= jt_mux_reg;
    end if;
  end process jumper_target;

  debug : process(rst, clk, stall_req, kill_req)
  begin
    if rst = '1' then
      o_dbg_di_pc <= (others => 'X');
    elsif rising_edge(clk) and kill_req = '1' then
      o_dbg_di_pc <= (others => 'X');
    elsif rising_edge(clk) and stall_req = '1' then
    elsif rising_edge(clk) then
      o_dbg_di_pc <= i_dbg_di_pc;
    end if;
  end process debug;

end architecture rtl;
