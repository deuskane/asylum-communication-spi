-------------------------------------------------------------------------------
-- Title      : tb_SPI_reset_pkg
-- Project    : SPI
-------------------------------------------------------------------------------
-- File       : tb_SPI_reset_pkg.vhd
-- Author     : mrosiere
-------------------------------------------------------------------------------
-- Description: Scenarios reset_values and disable of tb_SPI, from reset :
--              * reset_values : reset values of the registers, pads driven
--                               by the master in reset
--              * disable      : spi_enable = 0 aborts the transaction (CS#
--                               driven high, SCLK at CPOL) and drops the
--                               commands written while disabled
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
use     asylum.SPI_csr_pkg.all;

library work;
use     work.tb_SPI_bfm_pkg.all;

package tb_SPI_reset_pkg is

  -- Reset values of cfg, status and prescaler; spi_enable = 0 : CS# and SCLK
  -- pads driven (CS# high, SCLK at CPOL), IO pads disabled
  procedure test_reset_values
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   pads     : in    pads_t
    );

  -- spi_enable = 0 during a transaction : abort (CS# driven high, SCLK at
  -- CPOL), the commands and TX bytes written while disabled are dropped
  procedure test_disable
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    );

end package tb_SPI_reset_pkg;

package body tb_SPI_reset_pkg is

  procedure test_reset_values
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   pads     : in    pads_t
    ) is
  begin
    check_reg(SPI_CFG      , x"00"          , "Reset value cfg"                                    , clk, sbi_if);
    check_reg(SPI_STATUS   , x"15"          , "Reset value status (tx_empty, rx_empty, cmd_empty)", clk, sbi_if);
    check_reg(SPI_PRESCALER, C_PRESCALER_RST, "Reset value prescaler (PRESCALER_RATIO)"           , clk, sbi_if);
    check_value(pads.cs_b_oe, '1'  , ERROR, "spi_enable=0 : CS pad enabled (master in reset)"  , C_TB_SCOPE);
    check_value(pads.cs_b_o , '1'  , ERROR, "spi_enable=0 : CS driven inactive (high)"         , C_TB_SCOPE);
    check_value(pads.sclk_oe, '1'  , ERROR, "spi_enable=0 : SCLK pad enabled (master in reset)", C_TB_SCOPE);
    check_value(pads.sclk_o , '0'  , ERROR, "spi_enable=0 : SCLK driven to CPOL (0)"           , C_TB_SCOPE);
    check_value(pads.io_oe  , x"00", ERROR, "spi_enable=0 : IO pads disabled"                  , C_TB_SCOPE);
  end procedure;

  procedure test_disable
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    ) is
    constant C_SEED     : natural := 1;
    variable v_nb_trans : natural := tb_rsp.slave_nb_trans;
  begin
    configure('1', '1', 1, '0', clk, sbi_if, tb_req, pads);
    slave_setup(1, false, C_SEED, tb_req);
    -- Open a transaction (command without last) then disable the SPI
    write_reg(SPI_CMD , cmd(SPI_CMD_SIZE_SINGLE_RAW, true, false, false, 1), "Command TX 1 byte, not last", clk, sbi_if);
    write_reg(SPI_DATA, pattern(C_SEED, 0), "TX byte 0", clk, sbi_if);
    cycles(100, clk);
    check_value(pads.cs_b, '0', ERROR, "Transaction open (CS# active)", C_TB_SCOPE);
    write_reg(SPI_CFG, "00000" & "11" & '0', "Disable SPI (CPOL=1, CPHA=1)", clk, sbi_if);
    wait_end_of_transaction(v_nb_trans, "Abort by spi_enable = 0", tb_rsp, pads);
    check_value(tb_rsp.slave_nb_bytes, 1, ERROR, "Abort by spi_enable = 0 : 1 byte transfered", C_TB_SCOPE);
    check_value(pads.cs_b_oe, '1', ERROR, "spi_enable = 0 : CS pad enabled"         , C_TB_SCOPE);
    check_value(pads.cs_b_o , '1', ERROR, "spi_enable = 0 : CS driven inactive (high)", C_TB_SCOPE);
    check_value(pads.sclk_oe, '1', ERROR, "spi_enable = 0 : SCLK pad enabled"       , C_TB_SCOPE);
    check_value(pads.sclk_o , '1', ERROR, "spi_enable = 0 : SCLK driven to CPOL (1)", C_TB_SCOPE);
    -- Commands and TX bytes written while disabled are dropped (no blocking write)
    write_reg(SPI_CMD , cmd(SPI_CMD_SIZE_SINGLE_RAW, true, false, true, 1), "Command TX 1 byte while disabled", clk, sbi_if);
    write_reg(SPI_DATA, pattern(C_SEED, 1), "TX byte while disabled", clk, sbi_if);
    for i in 1 to 10 loop
      write_reg(SPI_DATA, pattern(C_SEED, 2), "TX byte while disabled (more than DEPTH_TX)", clk, sbi_if);
    end loop;
    write_reg(SPI_CFG, "00000" & "11" & '1', "Enable SPI (CPOL=1, CPHA=1)", clk, sbi_if);
    cycles(200, clk);
    check_value(tb_rsp.slave_nb_trans, v_nb_trans, ERROR, "No transaction after re-enable : commands written while disabled are dropped", C_TB_SCOPE);
    check_value(pads.cs_b, '1', ERROR, "CS inactive after re-enable", C_TB_SCOPE);
  end procedure;

end package body tb_SPI_reset_pkg;
