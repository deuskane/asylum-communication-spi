-------------------------------------------------------------------------------
-- Title      : tb_SPI_models_tests_pkg
-- Project    : SPI
-------------------------------------------------------------------------------
-- File       : tb_SPI_models_tests_pkg.vhd
-- Author     : mrosiere
-------------------------------------------------------------------------------
-- Description: Scenarios of the flash model testbench : names (valid items of
--              TESTS_LIST), models they apply to, and dispatch (run_test) to
--              tb_SPI_models_read_pkg and tb_SPI_models_multi_io_pkg
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-20  1.0      mrosiere Created (split of sim/models/tb_SPI)
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

library bitvis_vip_sbi;
use     bitvis_vip_sbi.sbi_bfm_pkg.all;

library asylum;
use     asylum.string_pkg.all;

library work;
use     work.tb_SPI_models_bfm_pkg.all;
use     work.tb_SPI_models_read_pkg.all;
use     work.tb_SPI_models_multi_io_pkg.all;

package tb_SPI_models_tests_pkg is

  -- Names of the scenarios (valid items of TESTS_LIST)
  constant C_TESTS            : string := "read,fast_read,dual_read,dual_io_read,quad_read,quad_io_read,quad_io_read_cont";

  -- Scenarios supported by every model, the others need s25fl512s
  constant C_TESTS_ALL_MODELS : string := "read,fast_read";

  -- true if the scenario name applies to the flash model
  function applies
    (constant name     : in    string
    ;constant model    : in    string
    ) return boolean;

  -- Run the scenario name
  procedure run_test
    (constant name     : in    string
    ;signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    );

end package tb_SPI_models_tests_pkg;

package body tb_SPI_models_tests_pkg is

  function applies
    (constant name     : in    string
    ;constant model    : in    string
    ) return boolean is
  begin
    return list_contains(C_TESTS_ALL_MODELS, name) or string_eq(model, "s25fl512s");
  end function;

  procedure run_test
    (constant name     : in    string
    ;signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ) is
  begin
    if    string_eq(name, "read")
    then
      test_read             (clk, sbi_if);
    elsif string_eq(name, "fast_read")
    then
      test_fast_read        (clk, sbi_if);
    elsif string_eq(name, "dual_read")
    then
      test_dual_read        (clk, sbi_if);
    elsif string_eq(name, "dual_io_read")
    then
      test_dual_io_read     (clk, sbi_if);
    elsif string_eq(name, "quad_read")
    then
      test_quad_read        (clk, sbi_if);
    elsif string_eq(name, "quad_io_read")
    then
      test_quad_io_read     (clk, sbi_if);
    elsif string_eq(name, "quad_io_read_cont")
    then
      test_quad_io_read_cont(clk, sbi_if);
    else
      alert(TB_ERROR, "Unknown test " & name, C_TB_SCOPE);
    end if;
  end procedure;

end package body tb_SPI_models_tests_pkg;
