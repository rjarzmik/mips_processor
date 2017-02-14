-------------------------------------------------------------------------------
-- Title      : std_logic_vector utilities
-- Project    : slv
-------------------------------------------------------------------------------
-- File       : slv_utils.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2017-01-30
-- Last update: 2017-02-18
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Various utilities
-------------------------------------------------------------------------------
-- Copyright (c) 2017 Robert Jarzmik <robert.jarzmik@free.fr>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2017-01-30  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library rjarzmik;
package body slv_utils is
  function and_reduce(a : std_ulogic_vector) return std_ulogic is
    variable q : std_ulogic := '1';
  begin
    for i in a'range loop
      q := q and a(i);
    end loop;
    return q;
  end function and_reduce;

  function or_reduce(a : std_ulogic_vector) return std_ulogic is
    variable q : std_ulogic := '0';
  begin
    for i in a'range loop
      q := q or a(i);
    end loop;
    return q;
  end function or_reduce;

  function get_row(slm : slv_2d; row : natural) return std_logic_vector is
    variable slv : std_logic_vector(slm'high(2) downto slm'low(2));
  begin
    for i in slv'range loop
      slv(i) := slm(row, i);
    end loop;
    return slv;
  end function get_row;

  procedure assign_row(signal slm : out slv_2d; slv : in std_logic_vector;
                       constant row : natural) is
  begin
    for i in slv'range loop
      slm(row, i) <= slv(i);
    end loop;
  end procedure assign_row;

  function to_slv_2d(slv : std_logic_vector; rows : positive; cols : positive) return slv_2d is
    variable slm : slv_2d(rows - 1 downto 0, cols - 1 downto 0);
  begin
    for i in 0 to rows - 1 loop
      for j in 0 to cols - 1 loop
        slm(i, j) := slv((i * cols) + j);
      end loop;
    end loop;
    return slm;
  end function;

  function slv_is_x(slv : std_logic_vector) return boolean is
    variable o : boolean;
  begin
    o := not slv(0) = '0' and not slv(0) = '1';
    -- pragma translate_off
    for i in slv'range loop
      o := o or not (slv(i) = '0' or slv(i) = '1');
    end loop;
    -- pragma translate_on
    return o;
  end function slv_is_x;

  function slv_is_x(slv : unsigned) return boolean is
  begin
    return slv_is_x(std_logic_vector(slv));
  end;
end package body slv_utils;
