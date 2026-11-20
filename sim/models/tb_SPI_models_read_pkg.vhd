-------------------------------------------------------------------------------
-- Title      : tb_SPI_models_read_pkg
-- Project    : SPI
-------------------------------------------------------------------------------
-- File       : tb_SPI_models_read_pkg.vhd
-- Author     : mrosiere
-------------------------------------------------------------------------------
-- Description: Scenarios of the flash model testbench supported by every
--              model, each one from reset :
--              * read      : Read (0x03), 4 bytes at 0x000005, prescaler 2
--              * fast_read : Fast Read (0x0B), 16 bytes at 0x000010, one
--                            dummy byte, prescaler 0
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
use     asylum.SPI_csr_pkg.all;

library work;
use     work.tb_SPI_models_bfm_pkg.all;

package tb_SPI_models_read_pkg is

  -- Read (0x03) of 4 bytes at 0x000005, prescaler 2 (SCLK = clk / 6)
  procedure test_read
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    );

  -- Fast Read (0x0B) of 16 bytes at 0x000010, prescaler 0 (SCLK = clk / 2)
  procedure test_fast_read
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    );

end package tb_SPI_models_read_pkg;

package body tb_SPI_models_read_pkg is

  procedure test_read
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ) is
  begin
    configure(2, clk, sbi_if);

    log(ID_SEQUENCER, "Perform Single Read (0x03) - At address 0x000005 (24b)", C_TB_SCOPE);
    write_reg(SPI_CMD , (   SPI_CMD_CFG_CONFIG_RAW
                         or SPI_CMD_LAST_DISABLE_RAW
                         or SPI_CMD_ENABLE_TX_ENABLE_RAW
                         or SPI_CMD_ENABLE_RX_DISABLE_RAW
                         or SPI_CMD_SIZE_SINGLE_RAW
                         or x"03"), "SPI TX 4 Bytes", clk, sbi_if);
    write_reg(SPI_DATA, x"03", "SPI Instruction 0x03", clk, sbi_if);
    write_reg(SPI_DATA, x"00", "SPI Address 0x000005", clk, sbi_if);
    write_reg(SPI_DATA, x"00", "SPI Address 0x000005", clk, sbi_if);
    write_reg(SPI_DATA, x"05", "SPI Address 0x000005", clk, sbi_if);
    write_reg(SPI_CMD , (   SPI_CMD_CFG_CONFIG_RAW
                         or SPI_CMD_LAST_DISABLE_RAW
                         or SPI_CMD_ENABLE_TX_DISABLE_RAW
                         or SPI_CMD_ENABLE_RX_ENABLE_RAW
                         or SPI_CMD_SIZE_SINGLE_RAW
                         or x"03"), "SPI RX 4 Bytes", clk, sbi_if);
    check_read(16#05#, 4, clk, sbi_if);
    write_reg(SPI_CMD , (   SPI_CMD_CFG_CONFIG_RAW
                         or SPI_CMD_LAST_ENABLE_RAW
                         or SPI_CMD_ENABLE_TX_DISABLE_RAW
                         or SPI_CMD_ENABLE_RX_DISABLE_RAW
                         or SPI_CMD_SIZE_SINGLE_RAW
                         or x"00"), "Stop command", clk, sbi_if);
  end procedure;

  procedure test_fast_read
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ) is
  begin
    configure(0, clk, sbi_if);

    log(ID_SEQUENCER, "Perform Fast Read (0x0B) - At address 0x000010 (24b)", C_TB_SCOPE);
    write_reg(SPI_CMD , x"23", "SPI TX 4 Bytes"             , clk, sbi_if);
    write_reg(SPI_DATA, x"0B", "SPI Instruction 0x0B"       , clk, sbi_if);
    write_reg(SPI_DATA, x"00", "SPI Address 0x000010"       , clk, sbi_if);
    write_reg(SPI_DATA, x"00", "SPI Address 0x000010"       , clk, sbi_if);
    write_reg(SPI_DATA, x"10", "SPI Address 0x000010"       , clk, sbi_if);
    write_reg(SPI_CMD , x"00", "Dummy Cycle"                , clk, sbi_if);
    write_reg(SPI_CMD , x"10", "SPI RX 1 Bytes without last", clk, sbi_if);
    write_reg(SPI_CMD , x"CE", "SPI RX 15 Bytes with last"  , clk, sbi_if);
    check_read(16#10#, 16, clk, sbi_if);
  end procedure;

end package body tb_SPI_models_read_pkg;
