-------------------------------------------------------------------------------
-- Title      : Instruction predicting
-- Project    : 
-------------------------------------------------------------------------------
-- File       : Instruction_prediction.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-07
-- Last update: 2016-12-07
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-12-07  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

-------------------------------------------------------------------------------

package instruction_prediction is
  constant NB_PREDICTIONS : positive := 4;
  constant ADDR_WIDTH : integer := 32;

  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  type prediction_t is record
    valid       : boolean;
    pc          : addr_t;
    next_pc     : addr_t;
    is_ja_jr    : boolean;
    is_branch   : boolean;
    take_branch : natural range 0 to 3;  -- 0:never, 1:no, 2:yes, 3:always
  end record;
  type predictions_t is array(0 to NB_PREDICTIONS - 1) of prediction_t;

  function is_prediction_hit(i_address   : addr_t;
                             predictions : predictions_t) return boolean;
  function get_prediction(i_address   : addr_t;
                          predictions : predictions_t) return prediction_t;

end package instruction_prediction;

package body instruction_prediction is
  function is_prediction_hit(i_address   : addr_t;
                             predictions : predictions_t)
    return boolean is
    variable found : boolean := false;
  begin
    for i in predictions'range loop
      found := found or (predictions(i).valid and predictions(i).pc = i_address);
    end loop;
    return found;
  end function is_prediction_hit;

  function get_prediction(i_address   : addr_t;
                          predictions : predictions_t)
    return prediction_t is
    variable found : prediction_t;
  begin
    for i in predictions'range loop
      if predictions(i).valid and predictions(i).pc = i_address then
        found := predictions(i);
      end if;
    end loop;
    return found;
  end function get_prediction;

end package body instruction_prediction;
