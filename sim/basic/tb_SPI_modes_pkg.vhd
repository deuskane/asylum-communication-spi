-------------------------------------------------------------------------------
-- Title      : tb_SPI_modes_pkg
-- Project    : SPI
-------------------------------------------------------------------------------
-- File       : tb_SPI_modes_pkg.vhd
-- Author     : mrosiere
-------------------------------------------------------------------------------
-- Description: Scenarios of tb_SPI run in the 4 CPOL/CPHA modes, each mode
--              with a different prescaler (0, 1, 2, 5), each scenario from
--              reset :
--              * single  : single full duplex 1 and 4 bytes, MOSI and MISO
--                          bytes, SCLK period and half period
--              * dual    : dual TX only (slave checks the lanes) and RX only
--                          (slave drives the lanes)
--              * quad    : quad TX only and RX only
--              * stop    : commands without last keep CS low, STOP command
--              * cs_high : 2 transactions back to back, CS# high time
--                          >= CS_HIGH_MIN_CYCLES
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-20  1.0      mrosiere Created (split of tb_SPI)
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

library bitvis_vip_sbi;
use     bitvis_vip_sbi.sbi_bfm_pkg.all;

library asylum;
use     asylum.SPI_csr_pkg.all;

library work;
use     work.tb_SPI_bfm_pkg.all;

package tb_SPI_modes_pkg is

  -- Single full duplex transfers of 1 and 4 bytes in the 4 modes
  procedure test_single
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    );

  -- Dual TX only and RX only transfers of 3 bytes in the 4 modes
  procedure test_dual
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    );

  -- Quad TX only and RX only transfers of 4 bytes in the 4 modes
  procedure test_quad
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    );

  -- Several commands in one transaction (last = 0) then STOP, in the 4 modes
  procedure test_stop
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    );

  -- 2 transactions queued back to back in the command FIFO : CS# high guard,
  -- in the 4 modes
  procedure test_cs_high
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    );

end package tb_SPI_modes_pkg;

package body tb_SPI_modes_pkg is

  -- Prescaler ratio of the SPI mode
  function mode_ratio
    (constant mode     : in    natural
    ) return natural is
  begin
    case mode is
      when 0      => return 0;
      when 1      => return 1;
      when 2      => return 2;
      when others => return 5;
    end case;
  end function;

  -- Configure the SPI mode (CPOL = mode/2, CPHA = mode mod 2) and its
  -- prescaler
  procedure configure_mode
    (constant mode     : in    natural
    ;signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   pads     : in    pads_t
    ) is
    constant C_CPOL : std_logic := to_unsigned(mode / 2  , 1)(0);
    constant C_CPHA : std_logic := to_unsigned(mode mod 2, 1)(0);
  begin
    log(ID_SEQUENCER, "SPI mode " & to_string(mode) & " (CPOL=" & to_string(C_CPOL) & ", CPHA=" & to_string(C_CPHA) & "), prescaler " & to_string(mode_ratio(mode)), C_TB_SCOPE);
    configure(C_CPOL, C_CPHA, mode_ratio(mode), '0', clk, sbi_if, tb_req, pads);
  end procedure;

  -- Single full duplex transfer of nb bytes, CS released at the end
  procedure single
    (constant nb       : in    natural
    ;constant ratio    : in    natural
    ;constant seed     : in    natural
    ;variable nb_trans : inout natural
    ;constant msg      : in    string
    ;signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    ) is
  begin
    slave_setup(1, true, seed, tb_req);
    write_reg(SPI_CMD, cmd(SPI_CMD_SIZE_SINGLE_RAW, true, true, true, nb), msg & " : command", clk, sbi_if);
    for i in 0 to nb-1 loop
      write_reg(SPI_DATA, pattern(seed, i), msg & " : TX byte " & to_string(i), clk, sbi_if);
    end loop;
    for i in 0 to nb-1 loop
      check_reg(SPI_DATA, pattern(seed+100, i), msg & " : RX byte " & to_string(i), clk, sbi_if);
    end loop;
    wait_end_of_transaction(nb_trans, msg, tb_rsp, pads);
    check_value(tb_rsp.slave_nb_bytes, nb, ERROR, msg & " : number of bytes seen by the slave", C_TB_SCOPE);
    check_mosi(nb, seed, 0, msg, tb_rsp);
    check_value(tb_rsp.slave_period, 2*(ratio+1)*C_CLK_PERIOD, ERROR, msg & " : SCLK period = 2*(ratio+1) clk_i periods"    , C_TB_SCOPE);
    check_value(tb_rsp.slave_half  ,   (ratio+1)*C_CLK_PERIOD, ERROR, msg & " : SCLK half period = (ratio+1) clk_i periods", C_TB_SCOPE);
  end procedure;

  -- Multi lanes TX only
  procedure multi_tx
    (constant size     : in    std_logic_vector(7 downto 0)
    ;constant lanes    : in    natural
    ;constant nb       : in    natural
    ;constant ratio    : in    natural
    ;constant seed     : in    natural
    ;variable nb_trans : inout natural
    ;constant msg      : in    string
    ;signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    ) is
  begin
    slave_setup(lanes, false, seed, tb_req);
    write_reg(SPI_CMD, cmd(size, true, false, true, nb), msg & " : command", clk, sbi_if);
    for i in 0 to nb-1 loop
      write_reg(SPI_DATA, pattern(seed, i), msg & " : TX byte " & to_string(i), clk, sbi_if);
    end loop;
    wait_end_of_transaction(nb_trans, msg, tb_rsp, pads);
    check_value(tb_rsp.slave_nb_bytes, nb, ERROR, msg & " : number of bytes seen by the slave", C_TB_SCOPE);
    check_mosi(nb, seed, 0, msg, tb_rsp);
    check_value(tb_rsp.slave_period, 2*(ratio+1)*C_CLK_PERIOD, ERROR, msg & " : SCLK period", C_TB_SCOPE);
  end procedure;

  -- Multi lanes RX only
  procedure multi_rx
    (constant size     : in    std_logic_vector(7 downto 0)
    ;constant lanes    : in    natural
    ;constant nb       : in    natural
    ;constant seed     : in    natural
    ;variable nb_trans : inout natural
    ;constant msg      : in    string
    ;signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    ) is
  begin
    slave_setup(lanes, true, seed, tb_req);
    write_reg(SPI_CMD, cmd(size, false, true, true, nb), msg & " : command", clk, sbi_if);
    for i in 0 to nb-1 loop
      check_reg(SPI_DATA, pattern(seed+100, i), msg & " : RX byte " & to_string(i), clk, sbi_if);
    end loop;
    wait_end_of_transaction(nb_trans, msg, tb_rsp, pads);
    check_value(tb_rsp.slave_nb_bytes, nb, ERROR, msg & " : number of bytes seen by the slave", C_TB_SCOPE);
  end procedure;

  procedure test_single
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    ) is
    variable v_nb_trans : natural := tb_rsp.slave_nb_trans;
  begin
    for mode in 0 to 3 loop
      configure_mode(mode, clk, sbi_if, tb_req, pads);
      single(1, mode_ratio(mode), 2*mode  , v_nb_trans, "Mode " & to_string(mode) & " single 1 byte" , clk, sbi_if, tb_req, tb_rsp, pads);
      single(4, mode_ratio(mode), 2*mode+1, v_nb_trans, "Mode " & to_string(mode) & " single 4 bytes", clk, sbi_if, tb_req, tb_rsp, pads);
    end loop;
  end procedure;

  procedure test_dual
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    ) is
    variable v_nb_trans : natural := tb_rsp.slave_nb_trans;
  begin
    for mode in 0 to 3 loop
      configure_mode(mode, clk, sbi_if, tb_req, pads);
      multi_tx(SPI_CMD_SIZE_DUAL_RAW, 2, 3, mode_ratio(mode), 2*mode  , v_nb_trans, "Mode " & to_string(mode) & " dual TX 3 bytes", clk, sbi_if, tb_req, tb_rsp, pads);
      multi_rx(SPI_CMD_SIZE_DUAL_RAW, 2, 3,                   2*mode+1, v_nb_trans, "Mode " & to_string(mode) & " dual RX 3 bytes", clk, sbi_if, tb_req, tb_rsp, pads);
    end loop;
  end procedure;

  procedure test_quad
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    ) is
    variable v_nb_trans : natural := tb_rsp.slave_nb_trans;
  begin
    for mode in 0 to 3 loop
      configure_mode(mode, clk, sbi_if, tb_req, pads);
      multi_tx(SPI_CMD_SIZE_QUAD_RAW, 4, 4, mode_ratio(mode), 2*mode  , v_nb_trans, "Mode " & to_string(mode) & " quad TX 4 bytes", clk, sbi_if, tb_req, tb_rsp, pads);
      multi_rx(SPI_CMD_SIZE_QUAD_RAW, 4, 4,                   2*mode+1, v_nb_trans, "Mode " & to_string(mode) & " quad RX 4 bytes", clk, sbi_if, tb_req, tb_rsp, pads);
    end loop;
  end procedure;

  procedure test_stop
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    ) is
    variable v_nb_trans : natural := tb_rsp.slave_nb_trans;
  begin
    for mode in 0 to 3 loop
      configure_mode(mode, clk, sbi_if, tb_req, pads);
      slave_setup(1, true, mode, tb_req);
      -- 2 bytes TX only, then 2 bytes RX only, CS kept low
      write_reg(SPI_CMD, cmd(SPI_CMD_SIZE_SINGLE_RAW, true , false, false, 2), "Mode " & to_string(mode) & " : command TX 2 bytes, not last", clk, sbi_if);
      write_reg(SPI_DATA, pattern(mode, 0), "Mode " & to_string(mode) & " : TX byte 0", clk, sbi_if);
      write_reg(SPI_DATA, pattern(mode, 1), "Mode " & to_string(mode) & " : TX byte 1", clk, sbi_if);
      write_reg(SPI_CMD, cmd(SPI_CMD_SIZE_SINGLE_RAW, false, true , false, 2), "Mode " & to_string(mode) & " : command RX 2 bytes, not last", clk, sbi_if);
      check_reg(SPI_DATA, pattern(mode+100, 2), "Mode " & to_string(mode) & " : RX byte 2", clk, sbi_if);
      check_reg(SPI_DATA, pattern(mode+100, 3), "Mode " & to_string(mode) & " : RX byte 3", clk, sbi_if);
      -- All the bytes are transfered : CS must stay active
      cycles(20*(mode_ratio(mode)+1), clk);
      check_value(pads.cs_b, '0', ERROR, "Mode " & to_string(mode) & " : CS kept active after commands without last", C_TB_SCOPE);
      check_value(tb_rsp.slave_nb_trans, v_nb_trans, ERROR, "Mode " & to_string(mode) & " : transaction not finished", C_TB_SCOPE);
      check_value(pads.sclk, tb_req.slave_cpol, ERROR, "Mode " & to_string(mode) & " : SCLK idle between commands", C_TB_SCOPE);
      -- STOP : last = 1, enable_rx = enable_tx = 0
      write_reg(SPI_CMD, SPI_CMD_CFG_CONFIG_RAW or SPI_CMD_LAST_ENABLE_RAW, "Mode " & to_string(mode) & " : STOP command", clk, sbi_if);
      wait_end_of_transaction(v_nb_trans, "Mode " & to_string(mode) & " multi commands + STOP", tb_rsp, pads);
      check_value(tb_rsp.slave_nb_bytes, 4, ERROR, "Mode " & to_string(mode) & " : number of bytes in the transaction", C_TB_SCOPE);
      check_mosi(2, mode, 0, "Mode " & to_string(mode) & " multi commands + STOP", tb_rsp);
    end loop;
  end procedure;

  procedure test_cs_high
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    ) is
    variable v_nb_trans : natural := tb_rsp.slave_nb_trans;
  begin
    for mode in 0 to 3 loop
      configure_mode(mode, clk, sbi_if, tb_req, pads);
      slave_setup(1, false, mode, tb_req);
      write_reg(SPI_DATA, pattern(mode, 0), "Mode " & to_string(mode) & " : TX byte 0", clk, sbi_if);
      write_reg(SPI_DATA, pattern(mode, 1), "Mode " & to_string(mode) & " : TX byte 1", clk, sbi_if);
      write_reg(SPI_CMD, cmd(SPI_CMD_SIZE_SINGLE_RAW, true, false, true, 1), "Mode " & to_string(mode) & " : command 1 TX 1 byte, last", clk, sbi_if);
      write_reg(SPI_CMD, cmd(SPI_CMD_SIZE_SINGLE_RAW, true, false, true, 1), "Mode " & to_string(mode) & " : command 2 TX 1 byte, last", clk, sbi_if);
      wait_end_of_transaction(v_nb_trans, "Mode " & to_string(mode) & " back to back transactions (1st)", tb_rsp, pads);
      wait_end_of_transaction(v_nb_trans, "Mode " & to_string(mode) & " back to back transactions (2nd)", tb_rsp, pads);
      check_value(tb_rsp.cs_high_last >= C_CS_HIGH_MIN*C_CLK_PERIOD, ERROR, "Mode " & to_string(mode) & " : CS# high " & to_string(tb_rsp.cs_high_last / C_CLK_PERIOD) & " clk cycles >= CS_HIGH_MIN_CYCLES", C_TB_SCOPE);
      check_value(tb_rsp.slave_mosi(0), pattern(mode, 1), ERROR, "Mode " & to_string(mode) & " : MOSI byte of the 2nd transaction", C_TB_SCOPE);
    end loop;
  end procedure;

end package body tb_SPI_modes_pkg;
