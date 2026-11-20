-------------------------------------------------------------------------------
-- Title      : tb_SPI_bfm_pkg
-- Project    : SPI
-------------------------------------------------------------------------------
-- File       : tb_SPI_bfm_pkg.vhd
-- Author     : mrosiere
-------------------------------------------------------------------------------
-- Description: Helpers of the tb_SPI scenarios : constants of the bench,
--              register accesses (SBI BFM), DUT configuration, requests to
--              the slave model of the bench and checks of its results, reset
--              pulse.
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-20  1.0      mrosiere Created (helpers of the tb_SPI sequencer)
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

package tb_SPI_bfm_pkg is

  constant C_TB_SCOPE      : string  := "TB_SPI";
  constant C_CLK_PERIOD    : time    := 20 ns;
  constant C_PRESCALER_RST : std_logic_vector(7 downto 0) := x"0F";
  constant C_CS_HIGH_MIN   : natural := 10;  -- CS_HIGH_MIN_CYCLES of the DUT
  constant C_NB_MAX        : natural := 64;

  type     byte_array_t is array (natural range <>) of std_logic_vector(7 downto 0);

  -- Blocking FIFO registers : an access may wait for the SPI transfer
  constant C_SBI_CFG       : t_sbi_bfm_config := (
    max_wait_cycles            => 100000,
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

  -- Configuration of the slave model, set by the sequencer
  type tb_req_t is record
    slave_cpol     : std_logic;
    slave_cpha     : std_logic;
    slave_lanes    : natural;                     -- 1 : single, 2 : dual, 4 : quad
    slave_drive    : boolean;                     -- Drive MISO (single) or the lanes (dual/quad)
    slave_miso     : byte_array_t(0 to C_NB_MAX-1);
  end record;

  constant C_TB_REQ_INIT   : tb_req_t := (
    slave_cpol     => '0',
    slave_cpha     => '0',
    slave_lanes    => 1,
    slave_drive    => true,
    slave_miso     => (others => (others => '0'))
  );

  -- Results of the slave model (updated at the end of each transaction) and
  -- of the CS# monitor (one process drives each element)
  type tb_rsp_t is record
    slave_mosi     : byte_array_t(0 to C_NB_MAX-1);
    slave_nb_bytes : natural;
    slave_nb_trans : natural;
    slave_period   : time;                        -- SCLK period (2 first leading edges)
    slave_half     : time;                        -- First leading to first trailing edge
    cs_high_last   : time;                        -- Last CS# high time between 2 transactions
  end record;

  -- SPI pads seen by the scenarios : bus levels and DUT outputs
  type pads_t is record
    sclk           : std_logic;                   -- Bus
    cs_b           : std_logic;                   -- Bus
    sclk_o         : std_logic;
    sclk_oe        : std_logic;
    cs_b_o         : std_logic;
    cs_b_oe        : std_logic;
    io_oe          : std_logic_vector(8-1 downto 0);
  end record;

  -- Data patterns
  function pattern
    (constant seed     : in    natural
    ;constant i        : in    natural
    ) return std_logic_vector;

  -- Command register value
  function cmd
    (constant size     : in    std_logic_vector(7 downto 0)
    ;constant tx       : in    boolean
    ;constant rx       : in    boolean
    ;constant last     : in    boolean
    ;constant nb       : in    natural
    ) return std_logic_vector;

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

  -- Poll the status register until (status and mask) = value
  procedure poll_status
    (constant mask     : in    std_logic_vector(7 downto 0)
    ;constant value    : in    std_logic_vector(7 downto 0)
    ;constant msg      : in    string
    ;signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    );

  -- Wait n rising edges
  procedure cycles
    (constant n        : in    natural
    ;signal   clk      : in    std_logic
    );

  -- Configure CPOL/CPHA, prescaler and loopback (master reset during the
  -- change) and the CPOL/CPHA of the slave model
  procedure configure
    (constant cpol     : in    std_logic
    ;constant cpha     : in    std_logic
    ;constant ratio    : in    natural
    ;constant loopback : in    std_logic
    ;signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   pads     : in    pads_t
    );

  -- Prepare the slave for the next transaction : lanes, drive, MISO bytes
  -- pattern(seed+100, i)
  procedure slave_setup
    (constant lanes    : in    natural
    ;constant drive    : in    boolean
    ;constant seed     : in    natural
    ;signal   tb_req   : inout tb_req_t
    );

  -- Wait the end of the next transaction (CS rises) : nb_trans is the number
  -- of transactions seen so far, incremented
  procedure wait_end_of_transaction
    (variable nb_trans : inout natural
    ;constant msg      : in    string
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    );

  -- Check the bytes received by the slave : pattern(seed, i) at first+i
  procedure check_mosi
    (constant nb       : in    natural
    ;constant seed     : in    natural
    ;constant first    : in    natural
    ;constant msg      : in    string
    ;signal   tb_rsp   : in    tb_rsp_t
    );

  -- Reset pulse, released on a falling edge
  procedure do_reset
    (signal   clk      : in    std_logic
    ;signal   arst_b   : out   std_logic
    );

end package tb_SPI_bfm_pkg;

package body tb_SPI_bfm_pkg is

  function pattern
    (constant seed     : in    natural
    ;constant i        : in    natural
    ) return std_logic_vector is
  begin
    return std_logic_vector(to_unsigned((seed*37 + i*53 + 16#5A#) mod 256, 8));
  end function;

  function cmd
    (constant size     : in    std_logic_vector(7 downto 0)
    ;constant tx       : in    boolean
    ;constant rx       : in    boolean
    ;constant last     : in    boolean
    ;constant nb       : in    natural
    ) return std_logic_vector is
    variable v : std_logic_vector(7 downto 0);
  begin
    v := SPI_CMD_CFG_CONFIG_RAW or size or std_logic_vector(to_unsigned(nb-1, 8));
    if tx   then v := v or SPI_CMD_ENABLE_TX_ENABLE_RAW; end if;
    if rx   then v := v or SPI_CMD_ENABLE_RX_ENABLE_RAW; end if;
    if last then v := v or SPI_CMD_LAST_ENABLE_RAW;      end if;
    return v;
  end function;

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

  procedure poll_status
    (constant mask     : in    std_logic_vector(7 downto 0)
    ;constant value    : in    std_logic_vector(7 downto 0)
    ;constant msg      : in    string
    ;signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ) is
    variable v_status : std_logic_vector(7 downto 0);
  begin
    for i in 1 to 1000 loop
      sbi_read(addr_value => SPI_STATUS
              ,data_value => v_status
              ,msg        => msg
              ,clk        => clk
              ,sbi_if     => sbi_if
              ,scope      => C_TB_SCOPE
              ,config     => C_SBI_CFG
              );
      exit when (v_status and mask) = value;
    end loop;
    check_value(v_status and mask, value, ERROR, msg & " : status polling", C_TB_SCOPE);
  end procedure;

  procedure cycles
    (constant n        : in    natural
    ;signal   clk      : in    std_logic
    ) is
  begin
    for i in 1 to n loop
      wait until rising_edge(clk);
    end loop;
  end procedure;

  procedure configure
    (constant cpol     : in    std_logic
    ;constant cpha     : in    std_logic
    ;constant ratio    : in    natural
    ;constant loopback : in    std_logic
    ;signal   clk      : in    std_logic
    ;signal   sbi_if   : inout t_sbi_if
    ;signal   tb_req   : inout tb_req_t
    ;signal   pads     : in    pads_t
    ) is
    variable v_cfg : std_logic_vector(7 downto 0);
  begin
    write_reg(SPI_CFG      , x"00", "Disable SPI (spi_master reset)", clk, sbi_if);
    tb_req.slave_cpol <= cpol;
    tb_req.slave_cpha <= cpha;
    write_reg(SPI_PRESCALER, std_logic_vector(to_unsigned(ratio, 8)), "Set prescaler " & to_string(ratio), clk, sbi_if);
    check_reg(SPI_PRESCALER, std_logic_vector(to_unsigned(ratio, 8)), "Read back prescaler", clk, sbi_if);
    v_cfg := "0000" & loopback & cpha & cpol & '1';
    write_reg(SPI_CFG      , v_cfg, "Enable SPI CPOL=" & to_string(cpol) & " CPHA=" & to_string(cpha) & " loopback=" & to_string(loopback), clk, sbi_if);
    check_reg(SPI_CFG      , v_cfg, "Read back cfg", clk, sbi_if);
    cycles(2, clk);
    check_value(pads.cs_b, '1' , ERROR, "CS inactive after enable", C_TB_SCOPE);
    check_value(pads.sclk, cpol, ERROR, "SCLK idle level is CPOL" , C_TB_SCOPE);
  end procedure;

  procedure slave_setup
    (constant lanes    : in    natural
    ;constant drive    : in    boolean
    ;constant seed     : in    natural
    ;signal   tb_req   : inout tb_req_t
    ) is
  begin
    tb_req.slave_lanes <= lanes;
    tb_req.slave_drive <= drive;
    for i in 0 to C_NB_MAX-1 loop
      tb_req.slave_miso(i) <= pattern(seed+100, i);
    end loop;
    wait for 0 ns;
  end procedure;

  procedure wait_end_of_transaction
    (variable nb_trans : inout natural
    ;constant msg      : in    string
    ;signal   tb_rsp   : in    tb_rsp_t
    ;signal   pads     : in    pads_t
    ) is
  begin
    nb_trans := nb_trans + 1;
    if tb_rsp.slave_nb_trans /= nb_trans
    then
      wait until tb_rsp.slave_nb_trans = nb_trans for 1 ms;
    end if;
    check_value(tb_rsp.slave_nb_trans, nb_trans, ERROR, msg & " : end of transaction (CS rises)", C_TB_SCOPE);
    check_value(pads.cs_b, '1', ERROR, msg & " : CS inactive", C_TB_SCOPE);
  end procedure;

  procedure check_mosi
    (constant nb       : in    natural
    ;constant seed     : in    natural
    ;constant first    : in    natural
    ;constant msg      : in    string
    ;signal   tb_rsp   : in    tb_rsp_t
    ) is
  begin
    for i in 0 to nb-1 loop
      check_value(tb_rsp.slave_mosi(first+i), pattern(seed, i), ERROR, msg & " : MOSI byte " & to_string(first+i), C_TB_SCOPE);
    end loop;
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

end package body tb_SPI_bfm_pkg;
