-------------------------------------------------------------------------------
-- Title      : tb_SPI_models_bfm_pkg
-- Project    : SPI
-------------------------------------------------------------------------------
-- File       : tb_SPI_models_bfm_pkg.vhd
-- Author     : mrosiere
-------------------------------------------------------------------------------
-- Description: Helpers of the scenarios of the flash model testbench
--              (sim/models/tb_SPI.vhd) : register accesses (SBI BFM),
--              configuration, check of the bytes read from the flash, quad
--              enable of s25fl512s, reset pulse.
--              The flash models are not reset : a scenario sets what it
--              needs (quad enable) and leaves the flash in its standard
--              mode (no continuous read left open).
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-20  1.0      mrosiere Created (helpers of the tb sequencer)
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

package tb_SPI_models_bfm_pkg is

  constant C_TB_SCOPE   : string   := "TB_SPI";
  constant C_POWER_UP   : time     := 800 us;  -- Power-up time of the flash models
  constant C_POLL_MAX   : positive := 10000;   -- Max number of status register polls

  -- Blocking FIFO registers : an access may wait for the SPI transfer
  constant C_SBI_CFG    : t_sbi_bfm_config := (
    max_wait_cycles            => 1000,
    max_wait_cycles_severity   => failure,
    use_fixed_wait_cycles_read => false,
    fixed_wait_cycles_read     => 0,
    clock_period               => -1 ns,
    clock_period_margin        => 0 ns,
    clock_margin_severity      => TB_ERROR,
    setup_time                 => -1 ns,
    hold_time                  => -1 ns,
    bfm_sync                   => SYNC_ON_CLOCK_ONLY,
    match_strictness           => MATCH_EXACT,
    id_for_bfm                 => ID_BFM,
    id_for_bfm_wait            => ID_BFM_WAIT,
    id_for_bfm_poll            => ID_BFM_POLL,
    use_ready_signal           => true
  );

  -- Register write
  procedure write_reg
    (constant addr     : in    unsigned
    ;constant data     : in    std_logic_vector
    ;constant msg      : in    string
    ;signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    );

  -- Register read and check
  procedure check_reg
    (constant addr     : in    unsigned
    ;constant data     : in    std_logic_vector
    ;constant msg      : in    string
    ;signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    );

  -- Wait the power-up time of the flash, set the prescaler and enable the
  -- SPI (CPOL = CPHA = 0)
  procedure configure
    (constant ratio    : in    natural
    ;signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    );

  -- Check nb bytes read from the flash from the address addr : the memory
  -- file holds addr+1 at the address addr
  procedure check_read
    (constant addr     : in    natural
    ;constant nb       : in    natural
    ;signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    );

  -- s25fl512s : WREN, WRR CR1.QUAD = 1, WRDI, RDSR1 polled until WIP = 0
  procedure quad_enable
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    );

  -- Reset pulse, released on a falling edge
  procedure do_reset
    (signal   clk      : in    std_logic
    ;signal   arst_b   : out   std_logic
    );

end package tb_SPI_models_bfm_pkg;

package body tb_SPI_models_bfm_pkg is

  procedure write_reg
    (constant addr     : in    unsigned
    ;constant data     : in    std_logic_vector
    ;constant msg      : in    string
    ;signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ) is
  begin
    sbi_write(addr_value => addr
             ,data_value => data
             ,msg        => msg
             ,clk        => clk
             ,sbi_if     => sbi_if
             ,scope      => C_TB_SCOPE
             ,config     => C_SBI_CFG
             );
  end procedure;

  procedure check_reg
    (constant addr     : in    unsigned
    ;constant data     : in    std_logic_vector
    ;constant msg      : in    string
    ;signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ) is
  begin
    sbi_check(addr_value  => addr
             ,data_exp    => data
             ,msg         => msg
             ,clk         => clk
             ,sbi_if      => sbi_if
             ,alert_level => error
             ,scope       => C_TB_SCOPE
             ,config      => C_SBI_CFG
             );
  end procedure;

  procedure configure
    (constant ratio    : in    natural
    ;signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ) is
  begin
    if now < C_POWER_UP
    then
      log(ID_SEQUENCER, "Wait the power-up time of the flash", C_TB_SCOPE);
      wait for C_POWER_UP - now;
      wait until rising_edge(clk);
    end if;
    write_reg(SPI_PRESCALER, std_logic_vector(to_unsigned(ratio, 8)), "Set prescaler " & to_string(ratio), clk, sbi_if);
    check_reg(SPI_PRESCALER, std_logic_vector(to_unsigned(ratio, 8)), "Read back prescaler"              , clk, sbi_if);
    write_reg(SPI_CFG      , x"01", "Enable SPI"              , clk, sbi_if);
    check_reg(SPI_CFG      , x"01", "Read back enabled config", clk, sbi_if);
  end procedure;

  procedure check_read
    (constant addr     : in    natural
    ;constant nb       : in    natural
    ;signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ) is
  begin
    for i in 0 to nb-1 loop
      check_reg(SPI_DATA, std_logic_vector(to_unsigned((addr+i+1) mod 256, 8)), "Read Byte @ 0x" & to_hstring(to_unsigned(addr+i, 24)), clk, sbi_if);
    end loop;
  end procedure;

  procedure quad_enable
    (signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ) is
    variable v_sr1 : std_logic_vector(8-1 downto 0);
  begin
    log(ID_SEQUENCER, "WREN Enable write", C_TB_SCOPE);
    write_reg(SPI_CMD , x"60", "SPI TX 1 Byte with last" , clk, sbi_if);
    write_reg(SPI_DATA, x"06", "SPI Instruction 0x06"    , clk, sbi_if);

    log(ID_SEQUENCER, "WRR Enable QUAD - CR Bit 1 = 1", C_TB_SCOPE);
    write_reg(SPI_CMD , x"62", "SPI TX 3 Bytes with last", clk, sbi_if);
    write_reg(SPI_DATA, x"01", "SPI Instruction 0x01"    , clk, sbi_if);
    write_reg(SPI_DATA, x"00", "Status Register"         , clk, sbi_if);
    write_reg(SPI_DATA, x"02", "Configuration Register"  , clk, sbi_if);

    log(ID_SEQUENCER, "WRDI Disable write", C_TB_SCOPE);
    write_reg(SPI_CMD , x"60", "SPI TX 1 Bytes with last", clk, sbi_if);
    write_reg(SPI_DATA, x"04", "SPI Instruction 0x04"    , clk, sbi_if);

    log(ID_SEQUENCER, "RDSR1 until WIP is 0", C_TB_SCOPE);
    write_reg(SPI_CMD , x"20", "SPI TX 1 Byte"           , clk, sbi_if);
    write_reg(SPI_DATA, x"05", "SPI Instruction 0x05"    , clk, sbi_if);

    -- Poll SR1 until WIP (bit 0) is 0, with a bounded number of polls
    v_sr1 := x"FF";
    for i in 1 to C_POLL_MAX loop
      write_reg(SPI_CMD, x"10", "SPI RX 1 Bytes", clk, sbi_if);
      sbi_read(addr_value => SPI_DATA
              ,data_value => v_sr1
              ,msg        => "SR1 -> WIP"
              ,clk        => clk
              ,sbi_if     => sbi_if
              ,scope      => C_TB_SCOPE
              ,config     => C_SBI_CFG
              );
      exit when v_sr1(0) = '0';
    end loop;
    check_value(v_sr1(0), '0', ERROR, "SR1.WIP cleared after at most " & to_string(C_POLL_MAX) & " polls", C_TB_SCOPE);

    write_reg(SPI_CMD, x"40", "Stop command", clk, sbi_if);
  end procedure;

  procedure do_reset
    (signal   clk      : in    std_logic
    ;signal   arst_b   : out   std_logic
    ) is
  begin
    arst_b <= '0';
    wait for 100 ns;
    wait until falling_edge(clk);
    arst_b <= '1';
    wait until rising_edge(clk);
  end procedure;

end package body tb_SPI_models_bfm_pkg;
