-------------------------------------------------------------------------------
-- Title      : MIPS CPU definitions
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : cpu_defs.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-15
-- Last update: 2018-12-04
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: MIPS CPU general definitions
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-15  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

-------------------------------------------------------------------------------

package cpu_defs is
  constant REGISTER_WIDTH     : integer := 32;
  constant NB_REGISTERS_MAX   : natural := 35;

  attribute enum_encoding                   : string;
  type jump_type is (none, always, zero, non_zero, lesser_or_zero, greater, lesser, greater_or_zero);
  attribute enum_encoding of jump_type      : type is "sequential";
  type alu_op_type is (all_zero, add, substract, multiply, divide, log_and, log_or, log_xor, log_nor, slt);
  attribute enum_encoding of alu_op_type    : type is "sequential";
  type memory_op_type is (none, loadw, storew, load8, load8_signextend32, store8);
  attribute enum_encoding of memory_op_type : type is "sequential";

  type register_port_type is record
    we   : std_logic;
    idx  : natural range 0 to NB_REGISTERS_MAX;
    data : std_logic_vector(REGISTER_WIDTH - 1 downto 0);
  end record register_port_type;
  constant REG_IDLE : register_port_type :=
    (we => '0', idx => 0, data => (others => 'X'));

  subtype prediction_t is std_ulogic_vector(1 downto 0);
  subtype instr_tag_t is std_ulogic_vector(REGISTER_WIDTH - 1 downto 0);

end package cpu_defs;
