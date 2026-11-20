-------------------------------------------------------------------------------
-- Title      : tb_SPI_tests_pkg
-- Project    : SPI
-------------------------------------------------------------------------------
-- File       : tb_SPI_tests_pkg.vhd
-- Author     : mrosiere
-------------------------------------------------------------------------------
-- Description: Scenarios of tb_SPI : names (valid items of TESTS_LIST) and
--              dispatch (run_test) to the packages of the scenarios
--              (tb_SPI_reset_pkg, tb_SPI_modes_pkg, tb_SPI_cmd_pkg)
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-20  1.0      mrosiere Created (split of tb_SPI)
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
use     work.tb_SPI_bfm_pkg.all;
use     work.tb_SPI_reset_pkg.all;
use     work.tb_SPI_modes_pkg.all;
use     work.tb_SPI_cmd_pkg.all;

package tb_SPI_tests_pkg is

  -- Names of the scenarios (valid items of TESTS_LIST)
  constant C_TESTS : string := "reset_values,single,dual,quad,stop,cs_high,keep,loopback,disable,status";

  -- Run the scenario name
  procedure run_test
    (constant name     : in    string
    ;signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    );

end package tb_SPI_tests_pkg;

package body tb_SPI_tests_pkg is

  procedure run_test
    (constant name     : in    string
    ;signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    ) is
  begin
    if    string_eq(name, "reset_values")
    then
      test_reset_values(clk, sbi_if, pads);
    elsif string_eq(name, "single")
    then
      test_single      (clk, sbi_if, tb_req, tb_rsp, pads);
    elsif string_eq(name, "dual")
    then
      test_dual        (clk, sbi_if, tb_req, tb_rsp, pads);
    elsif string_eq(name, "quad")
    then
      test_quad        (clk, sbi_if, tb_req, tb_rsp, pads);
    elsif string_eq(name, "stop")
    then
      test_stop        (clk, sbi_if, tb_req, tb_rsp, pads);
    elsif string_eq(name, "cs_high")
    then
      test_cs_high     (clk, sbi_if, tb_req, tb_rsp, pads);
    elsif string_eq(name, "keep")
    then
      test_keep        (clk, sbi_if, tb_req, tb_rsp, pads);
    elsif string_eq(name, "loopback")
    then
      test_loopback    (clk, sbi_if, tb_req, tb_rsp, pads);
    elsif string_eq(name, "disable")
    then
      test_disable     (clk, sbi_if, tb_req, tb_rsp, pads);
    elsif string_eq(name, "status")
    then
      test_status      (clk, sbi_if, tb_req, tb_rsp, pads);
    else
      alert(TB_ERROR, "Unknown test " & name, C_TB_SCOPE);
    end if;
  end procedure;

end package body tb_SPI_tests_pkg;
