-------------------------------------------------------------------------------
-- Title      : tb_SPI_cmd_pkg
-- Project    : SPI
-------------------------------------------------------------------------------
-- File       : tb_SPI_cmd_pkg.vhd
-- Author     : mrosiere
-------------------------------------------------------------------------------
-- Description: Scenarios keep, loopback and status of tb_SPI, each one from
--              reset :
--              * keep     : KEEP command mode (6-bit nb_bytes), a KEEP
--                           command with last is not a STOP
--              * loopback : MISO = MOSI inside spi_master
--              * status   : FIFO flags and busy, master stalled by a full RX
--                           FIFO
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

package tb_SPI_cmd_pkg is

  -- KEEP command mode : nb_bytes on 6 bits, enable_rx/tx of the previous
  -- command; a dummy KEEP command with last is not decoded as a STOP
  procedure test_keep
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    );

  -- Loopback : the RX bytes are the TX bytes (MISO = MOSI inside spi_master)
  procedure test_loopback
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    );

  -- Status register : FIFO flags and busy; the master waits (busy) while the
  -- RX FIFO is full
  procedure test_status
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    );

end package tb_SPI_cmd_pkg;

package body tb_SPI_cmd_pkg is

  procedure test_keep
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    ) is
    variable v_nb_trans : natural := tb_rsp.slave_nb_trans;
  begin
    configure('0', '0', 1, '0', clk, sbi_if, tb_req, pads);
    slave_setup(1, true, 1, tb_req);
    -- CONFIG : TX single 1 byte (not last), KEEP : same config, 9 bytes (nb_bytes[5:0] = 8), last
    write_reg(SPI_CMD, cmd(SPI_CMD_SIZE_SINGLE_RAW, true, false, false, 1), "CONFIG command TX single 1 byte", clk, sbi_if);
    write_reg(SPI_CMD, SPI_CMD_CFG_KEEP_RAW or SPI_CMD_LAST_ENABLE_RAW or "00001000", "KEEP command 9 bytes (nb_bytes=8), last", clk, sbi_if);
    for i in 0 to 9 loop
      write_reg(SPI_DATA, pattern(1, i), "TX byte " & to_string(i), clk, sbi_if);
    end loop;
    wait_end_of_transaction(v_nb_trans, "KEEP mode", tb_rsp, pads);
    check_value(tb_rsp.slave_nb_bytes, 10, ERROR, "KEEP mode : 1 + 9 bytes in the transaction", C_TB_SCOPE);
    check_mosi(10, 1, 0, "KEEP mode", tb_rsp);

    -- CONFIG dummy (enable_tx = enable_rx = 0) 1 byte, then KEEP 3 bytes with last :
    -- the KEEP command must not be decoded as a STOP
    slave_setup(1, false, 2, tb_req);
    write_reg(SPI_CMD, cmd(SPI_CMD_SIZE_SINGLE_RAW, false, false, false, 1), "CONFIG command dummy 1 byte", clk, sbi_if);
    write_reg(SPI_CMD, SPI_CMD_CFG_KEEP_RAW or SPI_CMD_LAST_ENABLE_RAW or "00000010", "KEEP command dummy 3 bytes (nb_bytes=2), last", clk, sbi_if);
    wait_end_of_transaction(v_nb_trans, "KEEP dummy with last", tb_rsp, pads);
    check_value(tb_rsp.slave_nb_bytes, 4, ERROR, "KEEP dummy with last : 1 + 3 dummy bytes (not a STOP)", C_TB_SCOPE);
  end procedure;

  procedure test_loopback
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    ) is
    variable v_nb_trans : natural := tb_rsp.slave_nb_trans;
  begin
    configure('0', '1', 0, '1', clk, sbi_if, tb_req, pads);
    slave_setup(1, false, 1, tb_req);
    write_reg(SPI_CMD, cmd(SPI_CMD_SIZE_SINGLE_RAW, true, true, true, 3), "Loopback command TX/RX 3 bytes", clk, sbi_if);
    for i in 0 to 2 loop
      write_reg(SPI_DATA, pattern(1, i), "Loopback TX byte " & to_string(i), clk, sbi_if);
    end loop;
    for i in 0 to 2 loop
      check_reg(SPI_DATA, pattern(1, i), "Loopback RX byte " & to_string(i), clk, sbi_if);
    end loop;
    wait_end_of_transaction(v_nb_trans, "Loopback", tb_rsp, pads);
  end procedure;

  procedure test_status
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    ) is
    constant C_RATIO    : natural := 1;
    variable v_nb_trans : natural := tb_rsp.slave_nb_trans;
  begin
    configure('0', '0', C_RATIO, '0', clk, sbi_if, tb_req, pads);
    check_reg(SPI_STATUS, x"15", "Status idle (tx_empty, rx_empty, cmd_empty)", clk, sbi_if);

    -- TX FIFO full without command : not busy
    slave_setup(1, false, 1, tb_req);
    for i in 0 to 7 loop
      write_reg(SPI_DATA, pattern(1, i), "Status : TX byte " & to_string(i), clk, sbi_if);
    end loop;
    check_reg(SPI_STATUS, x"16", "Status TX FIFO full (tx_full, rx_empty, cmd_empty), not busy", clk, sbi_if);
    -- 2 commands of 4 bytes : busy until CS# is released
    write_reg(SPI_CMD, cmd(SPI_CMD_SIZE_SINGLE_RAW, true, false, false, 4), "Status : command TX 4 bytes, not last", clk, sbi_if);
    write_reg(SPI_CMD, cmd(SPI_CMD_SIZE_SINGLE_RAW, true, false, true , 4), "Status : command TX 4 bytes, last", clk, sbi_if);
    poll_status(x"40", x"40", "Status busy during the transaction", clk, sbi_if);
    poll_status(x"40", x"00", "Status not busy at the end of the transaction", clk, sbi_if);
    check_value(pads.cs_b, '1', ERROR, "Status not busy : CS# released", C_TB_SCOPE);
    wait_end_of_transaction(v_nb_trans, "Status TX", tb_rsp, pads);
    check_value(tb_rsp.slave_nb_bytes, 8, ERROR, "Status TX : 8 bytes in the transaction", C_TB_SCOPE);
    check_mosi(8, 1, 0, "Status TX", tb_rsp);
    check_reg(SPI_STATUS, x"15", "Status idle after the TX transaction", clk, sbi_if);

    -- RX FIFO full (8 bytes) + 1 byte in spi_master : the master waits (busy)
    -- with the 10th byte until the RX FIFO is read
    slave_setup(1, true, 2, tb_req);
    write_reg(SPI_CMD, cmd(SPI_CMD_SIZE_SINGLE_RAW, false, true, false, 4), "Status : command RX 4 bytes, not last", clk, sbi_if);
    write_reg(SPI_CMD, cmd(SPI_CMD_SIZE_SINGLE_RAW, false, true, false, 4), "Status : command RX 4 bytes, not last", clk, sbi_if);
    write_reg(SPI_CMD, cmd(SPI_CMD_SIZE_SINGLE_RAW, false, true, true , 2), "Status : command RX 2 bytes, last", clk, sbi_if);
    poll_status(x"0C", x"08", "Status RX FIFO full", clk, sbi_if);
    cycles(20*(C_RATIO+1), clk);
    check_reg(SPI_STATUS, x"59", "Status RX FIFO full, master waiting (tx_empty, rx_full, cmd_empty, busy)", clk, sbi_if);
    check_value(pads.cs_b, '0', ERROR, "Status RX FIFO full : CS# still active", C_TB_SCOPE);
    for i in 0 to 9 loop
      poll_status(x"04", x"00", "Status RX byte " & to_string(i) & " available", clk, sbi_if);
      check_reg(SPI_DATA, pattern(2+100, i), "Status : RX byte " & to_string(i), clk, sbi_if);
    end loop;
    poll_status(x"40", x"00", "Status not busy at the end of the RX transaction", clk, sbi_if);
    wait_end_of_transaction(v_nb_trans, "Status RX", tb_rsp, pads);
    check_value(tb_rsp.slave_nb_bytes, 10, ERROR, "Status RX : 10 bytes in the transaction", C_TB_SCOPE);
    check_reg(SPI_STATUS, x"15", "Status idle after the RX transaction", clk, sbi_if);
  end procedure;

end package body tb_SPI_cmd_pkg;
