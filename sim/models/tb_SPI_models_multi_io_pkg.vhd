-------------------------------------------------------------------------------
-- Title      : tb_SPI_models_multi_io_pkg
-- Project    : SPI
-------------------------------------------------------------------------------
-- File       : tb_SPI_models_multi_io_pkg.vhd
-- Author     : mrosiere
-------------------------------------------------------------------------------
-- Description: Dual and quad read scenarios of the flash model testbench,
--              s25fl512s only, prescaler 0, each one from reset :
--              * dual_read         : DOR  (0x3B) read 1-1-2, 16 bytes
--              * dual_io_read      : DIOR (0xBB) read 1-2-2, 16 bytes
--              * quad_read         : QOR  (0x6B) read 1-1-4, 16 bytes
--              * quad_io_read      : QIOR (0xEB) read 1-4-4, 16 bytes
--              * quad_io_read_cont : QIOR in continuous mode : a read with
--                                    the instruction then a read without
--                                    it, 16 bytes each
--              The quad scenarios first set CR1.QUAD (quad_enable).
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

package tb_SPI_models_multi_io_pkg is

  -- DOR (0x3B) of 16 bytes at 0x000020
  procedure test_dual_read
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    );

  -- DIOR (0xBB) of 16 bytes at 0x000046
  procedure test_dual_io_read
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    );

  -- QOR (0x6B) of 16 bytes at 0x000030
  procedure test_quad_read
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    );

  -- QIOR (0xEB) of 16 bytes at 0x000046
  procedure test_quad_io_read
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    );

  -- QIOR (0xEB) in continuous mode : 16 bytes at 0x000066 with the
  -- instruction (mode 0xA0 : continuous), then 16 bytes at 0x000086 without
  -- it (mode 0x00 : back to the standard mode)
  procedure test_quad_io_read_cont
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    );

end package tb_SPI_models_multi_io_pkg;

package body tb_SPI_models_multi_io_pkg is

  -- QIOR address (TX quad 3 bytes) and mode byte, dummy cycles, RX quad
  -- 4 + 12 bytes
  procedure qior_address
    (constant addr     : in    std_logic_vector(8-1 downto 0)
    ;constant mode     : in    std_logic_vector(8-1 downto 0)
    ;signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ) is
  begin
    write_reg(SPI_CMD , x"2b", "SPI TX 4 Bytes"                    , clk, sbi_if);
    write_reg(SPI_DATA, x"00", "SPI Address 0x0000" & to_hstring(addr), clk, sbi_if);
    write_reg(SPI_DATA, x"00", "SPI Address 0x0000" & to_hstring(addr), clk, sbi_if);
    write_reg(SPI_DATA, addr , "SPI Address 0x0000" & to_hstring(addr), clk, sbi_if);
    write_reg(SPI_DATA, mode , "Mode 0x" & to_hstring(mode)        , clk, sbi_if);
    write_reg(SPI_CMD , x"09", "Dummy Cycle"                       , clk, sbi_if);
    write_reg(SPI_CMD , x"1b", "SPI RX 4 Bytes"                    , clk, sbi_if);
    write_reg(SPI_CMD , x"CB", "SPI RX 12 Bytes with last"         , clk, sbi_if);
  end procedure;

  procedure test_dual_read
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ) is
  begin
    configure(0, clk, sbi_if);

    log(ID_SEQUENCER, "Perform DOR (0x3B) - At address 0x000020 (24b)", C_TB_SCOPE);
    write_reg(SPI_CMD , x"23", "SPI TX 4 Bytes"           , clk, sbi_if);
    write_reg(SPI_DATA, x"3B", "SPI Instruction 0x3B"     , clk, sbi_if);
    write_reg(SPI_DATA, x"00", "SPI Address 0x000020"     , clk, sbi_if);
    write_reg(SPI_DATA, x"00", "SPI Address 0x000020"     , clk, sbi_if);
    write_reg(SPI_DATA, x"20", "SPI Address 0x000020"     , clk, sbi_if);
    write_reg(SPI_CMD , x"00", "Dummy Cycle"              , clk, sbi_if);
    write_reg(SPI_CMD , x"17", "SPI RX 4 Bytes"           , clk, sbi_if);
    write_reg(SPI_CMD , x"CB", "SPI RX 12 Bytes with last", clk, sbi_if);
    check_read(16#20#, 16, clk, sbi_if);
  end procedure;

  procedure test_dual_io_read
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ) is
  begin
    configure(0, clk, sbi_if);

    log(ID_SEQUENCER, "Perform DIOR (0xBB) - At address 0x000046 (24b)", C_TB_SCOPE);
    write_reg(SPI_CMD , x"20", "SPI TX 1 Bytes"           , clk, sbi_if);
    write_reg(SPI_DATA, x"BB", "SPI Instruction 0xBB"     , clk, sbi_if);
    write_reg(SPI_CMD , x"26", "SPI TX 3 Bytes"           , clk, sbi_if);
    write_reg(SPI_DATA, x"00", "SPI Address 0x000046"     , clk, sbi_if);
    write_reg(SPI_DATA, x"00", "SPI Address 0x000046"     , clk, sbi_if);
    write_reg(SPI_DATA, x"46", "SPI Address 0x000046"     , clk, sbi_if);
    write_reg(SPI_CMD , x"04", "Dummy Cycle"              , clk, sbi_if);
    write_reg(SPI_CMD , x"17", "SPI RX 4 Bytes"           , clk, sbi_if);
    write_reg(SPI_CMD , x"CB", "SPI RX 12 Bytes with last", clk, sbi_if);
    check_read(16#46#, 16, clk, sbi_if);
  end procedure;

  procedure test_quad_read
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ) is
  begin
    configure(0, clk, sbi_if);
    quad_enable(clk, sbi_if);

    log(ID_SEQUENCER, "Perform QOR (0x6B) - At address 0x000030 (24b)", C_TB_SCOPE);
    write_reg(SPI_CMD , x"23", "SPI TX 4 Bytes"           , clk, sbi_if);
    write_reg(SPI_DATA, x"6B", "SPI Instruction 0x6B"     , clk, sbi_if);
    write_reg(SPI_DATA, x"00", "SPI Address 0x000030"     , clk, sbi_if);
    write_reg(SPI_DATA, x"00", "SPI Address 0x000030"     , clk, sbi_if);
    write_reg(SPI_DATA, x"30", "SPI Address 0x000030"     , clk, sbi_if);
    write_reg(SPI_CMD , x"00", "Dummy Cycle"              , clk, sbi_if);
    write_reg(SPI_CMD , x"1b", "SPI RX 4 Bytes"           , clk, sbi_if);
    write_reg(SPI_CMD , x"CB", "SPI RX 12 Bytes with last", clk, sbi_if);
    check_read(16#30#, 16, clk, sbi_if);
  end procedure;

  procedure test_quad_io_read
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ) is
  begin
    configure(0, clk, sbi_if);
    quad_enable(clk, sbi_if);

    log(ID_SEQUENCER, "Perform QIOR (0xEB) - At address 0x000046 (24b)", C_TB_SCOPE);
    write_reg(SPI_CMD , x"20", "SPI TX 1 Bytes"           , clk, sbi_if);
    write_reg(SPI_DATA, x"EB", "SPI Instruction 0xEB"     , clk, sbi_if);
    qior_address(x"46", x"00", clk, sbi_if);
    check_read(16#46#, 16, clk, sbi_if);
  end procedure;

  procedure test_quad_io_read_cont
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ) is
  begin
    configure(0, clk, sbi_if);
    quad_enable(clk, sbi_if);

    log(ID_SEQUENCER, "Perform QIOR (0xEB) in continous mode - At address 0x000066 and 0x000086 (24b)", C_TB_SCOPE);
    write_reg(SPI_CMD , x"20", "SPI TX 1 Bytes"           , clk, sbi_if);
    write_reg(SPI_DATA, x"EB", "SPI Instruction 0xEB"     , clk, sbi_if);
    qior_address(x"66", x"A0", clk, sbi_if);
    check_read(16#66#, 16, clk, sbi_if);
    -- Continuous mode : no instruction; mode 0x00 leaves the continuous mode
    -- after this read (the flash is not reset between the scenarios)
    qior_address(x"86", x"00", clk, sbi_if);
    check_read(16#86#, 16, clk, sbi_if);
  end procedure;

end package body tb_SPI_models_multi_io_pkg;
