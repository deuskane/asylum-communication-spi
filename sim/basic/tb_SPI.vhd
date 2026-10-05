-------------------------------------------------------------------------------
-- Title      : tb_SPI
-- Project    : SPI
-------------------------------------------------------------------------------
-- File       : tb_SPI.vhd
-- Author     : mrosiere
-------------------------------------------------------------------------------
-- Description: UVVM/SBI self-checking testbench of sbi_SPI with a SPI slave
--              model written in the bench.
--              For the 4 CPOL/CPHA modes (each with a different prescaler) :
--              * single full duplex : MOSI bytes checked by the slave, MISO
--                bytes returned by the slave checked in the RX FIFO
--              * dual and quad : TX only (slave checks the lanes) and RX
--                only (slave drives the lanes)
--              * CS kept low between commands without last, STOP command
--              * SCLK idle level on CS edges, SCLK period
--                f_sclk = f_clk / (2*(ratio+1))
--              Plus : reset values, KEEP command mode (6-bit nb_bytes),
--              loopback, spi_enable = 0 keeps the master in reset.
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2025-05-29  1.0      mrosiere Created
-- 2026-10-05  2.0      mrosiere UVVM self-checking bench on sbi_SPI with a
--                               SPI slave model (replace the spi_master bench)
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

library bitvis_vip_sbi;
use     bitvis_vip_sbi.sbi_bfm_pkg.all;

library asylum;
use     asylum.sbi_pkg.all;
use     asylum.spi_pkg.all;
use     asylum.SPI_csr_pkg.all;

entity tb_SPI is
end tb_SPI;

architecture sim of tb_SPI is

  constant C_SCOPE         : string  := "TB_SPI";
  constant C_CLK_PERIOD    : time    := 20 ns;
  constant C_PRESCALER_RST : std_logic_vector(7 downto 0) := x"0F";

  type     byte_array_t is array (natural range <>) of std_logic_vector(7 downto 0);
  constant C_NB_MAX        : natural := 64;

  -- DUT
  signal   clk_i           : std_logic := '0';
  signal   clk_ena         : boolean   := true;
  signal   arst_b_i        : std_logic := '0';

  signal   sbi_ini         : sbi_ini_t(addr (SPI_ADDR_WIDTH-1 downto 0),
                                       wdata(SPI_DATA_WIDTH-1 downto 0));
  signal   sbi_tgt         : sbi_tgt_t(rdata(SPI_DATA_WIDTH-1 downto 0));
  signal   sbi_if          : t_sbi_if(addr (SPI_ADDR_WIDTH-1 downto 0),
                                      wdata(SPI_DATA_WIDTH-1 downto 0),
                                      rdata(SPI_DATA_WIDTH-1 downto 0));

  signal   sclk_o          : std_logic;
  signal   sclk_oe_o       : std_logic;
  signal   cs_b_o          : std_logic;
  signal   cs_b_oe_o       : std_logic;
  signal   io_o            : std_logic_vector(8-1 downto 0);
  signal   io_i            : std_logic_vector(8-1 downto 0);
  signal   io_oe_o         : std_logic_vector(8-1 downto 0);

  -- SPI bus (with pull-up)
  signal   sclk            : std_logic;
  signal   cs_b            : std_logic;
  signal   io_bus          : std_logic_vector(8-1 downto 0);

  -- Slave model configuration (set by the sequencer)
  signal   slave_cpol      : std_logic := '0';
  signal   slave_cpha      : std_logic := '0';
  signal   slave_lanes     : natural   := 1;     -- 1 : single, 2 : dual, 4 : quad
  signal   slave_drive     : boolean   := true;  -- drive MISO (single) or the lanes (dual/quad)
  signal   slave_miso      : byte_array_t(0 to C_NB_MAX-1) := (others => (others => '0'));
  signal   slave_io        : std_logic_vector(8-1 downto 0) := (others => 'Z');

  -- Slave model results (updated at the end of each transaction)
  signal   slave_mosi      : byte_array_t(0 to C_NB_MAX-1) := (others => (others => '0'));
  signal   slave_nb_bytes  : natural := 0;
  signal   slave_nb_trans  : natural := 0;
  signal   slave_period    : time    := 0 ns;    -- SCLK period (2 first leading edges)
  signal   slave_half      : time    := 0 ns;    -- first leading to first trailing edge

  -- Data patterns
  function pattern(seed : natural; i : natural) return std_logic_vector is
  begin
    return std_logic_vector(to_unsigned((seed*37 + i*53 + 16#5A#) mod 256, 8));
  end function;

begin

  clock_generator(clk_i, clk_ena, C_CLK_PERIOD, "TB Clock");

  -----------------------------------------------------------------------------
  -- DUT
  -----------------------------------------------------------------------------
  dut : sbi_SPI
    generic map (
      NAME                  => "SPI"
     ,USER_DEFINE_PRESCALER => true
     ,PRESCALER_RATIO       => C_PRESCALER_RST
     ,DEPTH_CMD             => 4
     ,DEPTH_TX              => 8
     ,DEPTH_RX              => 8
     ,HANDLE_HOLD_WP        => false
    )
    port map (
      clk_i      => clk_i
     ,arst_b_i   => arst_b_i
     ,sbi_ini_i  => sbi_ini
     ,sbi_tgt_o  => sbi_tgt
     ,sclk_o     => sclk_o
     ,sclk_oe_o  => sclk_oe_o
     ,cs_b_o     => cs_b_o
     ,cs_b_oe_o  => cs_b_oe_o
     ,io_o       => io_o
     ,io_i       => io_i
     ,io_oe_o    => io_oe_o
    );

  sbi_ini.cs                              <= sbi_if.cs;
  sbi_ini.addr                            <= std_logic_vector(sbi_if.addr(SPI_ADDR_WIDTH-1 downto 0));
  sbi_ini.re                              <= sbi_if.rena;
  sbi_ini.we                              <= sbi_if.wena;
  sbi_ini.wdata                           <= sbi_if.wdata(SPI_DATA_WIDTH-1 downto 0);
  sbi_if.ready                            <= sbi_tgt.ready;
  sbi_if.rdata(SPI_DATA_WIDTH-1 downto 0) <= sbi_tgt.rdata;

  -----------------------------------------------------------------------------
  -- SPI bus : pads with pull-up
  -----------------------------------------------------------------------------
  sclk   <= to_x01(sclk_o) when sclk_oe_o = '1' else '1';
  cs_b   <= to_x01(cs_b_o) when cs_b_oe_o = '1' else '1';

  gen_io : for k in io_bus'range generate
    io_bus(k) <= io_o(k) when io_oe_o(k) = '1' else 'Z';  -- master
    io_bus(k) <= slave_io(k);                              -- slave
    io_bus(k) <= 'H';                                      -- pull-up
  end generate gen_io;

  io_i   <= to_x01(io_bus);

  -----------------------------------------------------------------------------
  -- SPI slave model
  -- Sample edge : leading  edge if CPHA = 0, trailing edge if CPHA = 1
  -- Shift  edge : trailing edge if CPHA = 0, leading  edge if CPHA = 1
  --               (CPHA = 0 : first bits driven when CS falls)
  -- MSB first, io(lanes-1) carries the most significant bit of each group
  -----------------------------------------------------------------------------
  p_slave : process
    variable v_lanes    : natural;
    variable v_bitcnt   : natural;
    variable v_byte_idx : natural;
    variable v_out_bits : natural;
    variable v_rx       : std_logic_vector(7 downto 0);
    variable v_mosi     : byte_array_t(0 to C_NB_MAX-1);
    variable v_leading  : boolean;
    variable v_sample   : boolean;
    variable v_nb_lead  : natural;
    variable v_nb_trail : natural;
    variable v_t_lead1  : time;
    variable v_t_lead2  : time;
    variable v_t_trail1 : time;

    procedure drive_next is
      variable v_byte : std_logic_vector(7 downto 0);
      variable v_pos  : natural;
    begin
      if slave_drive then
        v_byte := slave_miso((v_out_bits/8) mod C_NB_MAX);
        v_pos  := 7 - (v_out_bits mod 8);
        if v_lanes = 1 then
          slave_io(SPI_IO_MISO) <= v_byte(v_pos);
        else
          for l in 0 to v_lanes-1 loop
            slave_io(l) <= v_byte(v_pos - (v_lanes-1) + l);
          end loop;
        end if;
      end if;
      v_out_bits := v_out_bits + v_lanes;
    end procedure;

  begin
    slave_io <= (others => 'Z');
    wait until cs_b = '0';

    check_value(sclk, slave_cpol, ERROR, "SPI slave : SCLK at idle level (CPOL) when CS falls", C_SCOPE, ID_NEVER);

    v_lanes    := slave_lanes;
    v_bitcnt   := 0;
    v_byte_idx := 0;
    v_out_bits := 0;
    v_nb_lead  := 0;
    v_nb_trail := 0;
    v_t_lead1  := 0 ns;
    v_t_lead2  := 0 ns;
    v_t_trail1 := 0 ns;
    v_mosi     := (others => (others => '0'));

    if slave_cpha = '0' then
      drive_next;
    end if;

    loop
      wait on sclk, cs_b;
      exit when cs_b /= '0';

      if sclk'event then
        v_leading := (sclk /= slave_cpol);
        v_sample  := (v_leading and slave_cpha = '0') or (not v_leading and slave_cpha = '1');

        if v_leading then
          v_nb_lead := v_nb_lead + 1;
          if v_nb_lead = 1 then v_t_lead1 := now; end if;
          if v_nb_lead = 2 then v_t_lead2 := now; end if;
        else
          v_nb_trail := v_nb_trail + 1;
          if v_nb_trail = 1 then v_t_trail1 := now; end if;
        end if;

        if v_sample then
          if v_lanes = 1 then
            v_rx := v_rx(6 downto 0) & io_i(SPI_IO_MOSI);
          else
            v_rx := v_rx(7-v_lanes downto 0) & io_i(v_lanes-1 downto 0);
          end if;
          v_bitcnt := v_bitcnt + v_lanes;
          if v_bitcnt = 8 then
            v_mosi(v_byte_idx mod C_NB_MAX) := v_rx;
            v_byte_idx := v_byte_idx + 1;
            v_bitcnt   := 0;
          end if;
        else
          drive_next;
        end if;
      end if;
    end loop;

    -- End of transaction (CS rises)
    check_value(sclk    , slave_cpol, ERROR, "SPI slave : SCLK at idle level (CPOL) when CS rises", C_SCOPE, ID_NEVER);
    check_value(v_bitcnt, 0         , ERROR, "SPI slave : no partial byte when CS rises", C_SCOPE, ID_NEVER);
    slave_io       <= (others => 'Z');
    slave_mosi     <= v_mosi;
    slave_nb_bytes <= v_byte_idx;
    slave_period   <= v_t_lead2  - v_t_lead1;
    slave_half     <= v_t_trail1 - v_t_lead1;
    slave_nb_trans <= slave_nb_trans + 1;
  end process p_slave;

  -----------------------------------------------------------------------------
  -- Sequencer
  -----------------------------------------------------------------------------
  p_main : process
    variable v_sbi_config : t_sbi_bfm_config := C_SBI_BFM_CONFIG_DEFAULT;
    variable v_nb_trans   : natural := 0;
    variable v_cmd        : std_logic_vector(7 downto 0);
    variable v_cfg        : std_logic_vector(7 downto 0);
    variable v_ratio      : natural;
    variable v_seed       : natural := 0;

    procedure write_reg(addr : unsigned; data : std_logic_vector; msg : string) is
    begin
      sbi_write(addr, data, msg, clk_i, sbi_if, C_SCOPE, shared_msg_id_panel, v_sbi_config);
    end procedure;

    procedure check_reg(addr : unsigned; data : std_logic_vector; msg : string) is
    begin
      sbi_check(addr, data, msg, clk_i, sbi_if, ERROR, C_SCOPE, shared_msg_id_panel, v_sbi_config);
    end procedure;

    -- Command register value
    function cmd(size  : std_logic_vector(7 downto 0);
                 tx    : boolean;
                 rx    : boolean;
                 last  : boolean;
                 nb    : natural) return std_logic_vector is
      variable v : std_logic_vector(7 downto 0);
    begin
      v := SPI_CMD_CFG_CONFIG_RAW or size or std_logic_vector(to_unsigned(nb-1, 8));
      if tx   then v := v or SPI_CMD_ENABLE_TX_ENABLE_RAW; end if;
      if rx   then v := v or SPI_CMD_ENABLE_RX_ENABLE_RAW; end if;
      if last then v := v or SPI_CMD_LAST_ENABLE_RAW;      end if;
      return v;
    end function;

    -- Prepare the slave for the next transaction
    procedure slave_setup(lanes : natural; drive : boolean; seed : natural) is
    begin
      slave_lanes <= lanes;
      slave_drive <= drive;
      for i in 0 to C_NB_MAX-1 loop
        slave_miso(i) <= pattern(seed+100, i);
      end loop;
      wait for 0 ns;
    end procedure;

    -- Wait the end of the next transaction (CS rises)
    procedure wait_end_of_transaction(msg : string) is
    begin
      v_nb_trans := v_nb_trans + 1;
      if slave_nb_trans /= v_nb_trans then
        wait until slave_nb_trans = v_nb_trans for 1 ms;
      end if;
      check_value(slave_nb_trans, v_nb_trans, ERROR, msg & " : end of transaction (CS rises)");
      check_value(cs_b, '1', ERROR, msg & " : CS inactive");
    end procedure;

    -- Check the bytes received by the slave
    procedure check_mosi(nb : natural; seed : natural; first : natural; msg : string) is
    begin
      for i in 0 to nb-1 loop
        check_value(slave_mosi(first+i), pattern(seed, i), ERROR, msg & " : MOSI byte " & to_string(first+i));
      end loop;
    end procedure;

    -- Configure CPOL/CPHA and prescaler (master reset during the change)
    procedure configure(cpol : std_logic; cpha : std_logic; ratio : natural; loopback : std_logic := '0') is
    begin
      write_reg(SPI_CFG      , x"00", "Disable SPI (spi_master reset)");
      slave_cpol <= cpol;
      slave_cpha <= cpha;
      write_reg(SPI_PRESCALER, std_logic_vector(to_unsigned(ratio, 8)), "Set prescaler " & to_string(ratio));
      check_reg(SPI_PRESCALER, std_logic_vector(to_unsigned(ratio, 8)), "Read back prescaler");
      v_cfg := "0000" & loopback & cpha & cpol & '1';
      write_reg(SPI_CFG      , v_cfg, "Enable SPI CPOL=" & to_string(cpol) & " CPHA=" & to_string(cpha) & " loopback=" & to_string(loopback));
      check_reg(SPI_CFG      , v_cfg, "Read back cfg");
      wait until rising_edge(clk_i);
      wait until rising_edge(clk_i);
      check_value(cs_b, '1' , ERROR, "CS inactive after enable");
      check_value(sclk, cpol, ERROR, "SCLK idle level is CPOL");
    end procedure;

    -- Single full duplex transfer of nb bytes, CS released at the end
    procedure test_single(nb : natural; msg : string) is
    begin
      v_seed := v_seed + 1;
      slave_setup(1, true, v_seed);
      write_reg(SPI_CMD, cmd(SPI_CMD_SIZE_SINGLE_RAW, true, true, true, nb), msg & " : command");
      for i in 0 to nb-1 loop
        write_reg(SPI_DATA, pattern(v_seed, i), msg & " : TX byte " & to_string(i));
      end loop;
      for i in 0 to nb-1 loop
        check_reg(SPI_DATA, pattern(v_seed+100, i), msg & " : RX byte " & to_string(i));
      end loop;
      wait_end_of_transaction(msg);
      check_value(slave_nb_bytes, nb, ERROR, msg & " : number of bytes seen by the slave");
      check_mosi(nb, v_seed, 0, msg);
      check_value(slave_period, 2*(v_ratio+1)*C_CLK_PERIOD, ERROR, msg & " : SCLK period = 2*(ratio+1) clk_i periods");
      check_value(slave_half  ,   (v_ratio+1)*C_CLK_PERIOD, ERROR, msg & " : SCLK half period = (ratio+1) clk_i periods");
    end procedure;

    -- Multi lanes TX only
    procedure test_multi_tx(size : std_logic_vector(7 downto 0); lanes : natural; nb : natural; msg : string) is
    begin
      v_seed := v_seed + 1;
      slave_setup(lanes, false, v_seed);
      write_reg(SPI_CMD, cmd(size, true, false, true, nb), msg & " : command");
      for i in 0 to nb-1 loop
        write_reg(SPI_DATA, pattern(v_seed, i), msg & " : TX byte " & to_string(i));
      end loop;
      wait_end_of_transaction(msg);
      check_value(slave_nb_bytes, nb, ERROR, msg & " : number of bytes seen by the slave");
      check_mosi(nb, v_seed, 0, msg);
      check_value(slave_period, 2*(v_ratio+1)*C_CLK_PERIOD, ERROR, msg & " : SCLK period");
    end procedure;

    -- Multi lanes RX only
    procedure test_multi_rx(size : std_logic_vector(7 downto 0); lanes : natural; nb : natural; msg : string) is
    begin
      v_seed := v_seed + 1;
      slave_setup(lanes, true, v_seed);
      write_reg(SPI_CMD, cmd(size, false, true, true, nb), msg & " : command");
      for i in 0 to nb-1 loop
        check_reg(SPI_DATA, pattern(v_seed+100, i), msg & " : RX byte " & to_string(i));
      end loop;
      wait_end_of_transaction(msg);
      check_value(slave_nb_bytes, nb, ERROR, msg & " : number of bytes seen by the slave");
    end procedure;

    -- Several commands in one transaction (last = 0) then STOP
    procedure test_stop(msg : string) is
    begin
      v_seed := v_seed + 1;
      slave_setup(1, true, v_seed);
      -- 2 bytes TX only, then 2 bytes RX only, CS kept low
      write_reg(SPI_CMD, cmd(SPI_CMD_SIZE_SINGLE_RAW, true , false, false, 2), msg & " : command TX 2 bytes, not last");
      write_reg(SPI_DATA, pattern(v_seed, 0), msg & " : TX byte 0");
      write_reg(SPI_DATA, pattern(v_seed, 1), msg & " : TX byte 1");
      write_reg(SPI_CMD, cmd(SPI_CMD_SIZE_SINGLE_RAW, false, true , false, 2), msg & " : command RX 2 bytes, not last");
      check_reg(SPI_DATA, pattern(v_seed+100, 2), msg & " : RX byte 2");
      check_reg(SPI_DATA, pattern(v_seed+100, 3), msg & " : RX byte 3");
      -- All the bytes are transfered : CS must stay active
      for i in 1 to 20*(v_ratio+1) loop
        wait until rising_edge(clk_i);
      end loop;
      check_value(cs_b, '0', ERROR, msg & " : CS kept active after commands without last");
      check_value(slave_nb_trans, v_nb_trans, ERROR, msg & " : transaction not finished");
      check_value(sclk, slave_cpol, ERROR, msg & " : SCLK idle between commands");
      -- STOP : last = 1, enable_rx = enable_tx = 0
      write_reg(SPI_CMD, SPI_CMD_CFG_CONFIG_RAW or SPI_CMD_LAST_ENABLE_RAW, msg & " : STOP command");
      wait_end_of_transaction(msg);
      check_value(slave_nb_bytes, 4, ERROR, msg & " : number of bytes in the transaction");
      check_mosi(2, v_seed, 0, msg);
    end procedure;

  begin
    v_sbi_config.max_wait_cycles := 100000;

    sbi_if   <= init_sbi_if_signals(SPI_ADDR_WIDTH, SPI_DATA_WIDTH);
    arst_b_i <= '0';
    wait for 100 ns;
    arst_b_i <= '1';
    wait until rising_edge(clk_i);

    log(ID_SEQUENCER, "Reset released, starting SPI basic test", C_SCOPE);

    ------------------------------------------------
    log(ID_LOG_HDR, "1. Reset values", C_SCOPE);
    ------------------------------------------------
    check_reg(SPI_CFG      , x"00"          , "Reset value cfg");
    check_reg(SPI_PRESCALER, C_PRESCALER_RST, "Reset value prescaler (PRESCALER_RATIO)");
    check_value(cs_b_oe_o, '0', ERROR, "spi_enable=0 : CS pad disabled (master in reset)");
    check_value(sclk_oe_o, '0', ERROR, "spi_enable=0 : SCLK pad disabled (master in reset)");
    check_value(io_oe_o  , x"00", ERROR, "spi_enable=0 : IO pads disabled");

    ------------------------------------------------
    -- 2. The 4 modes, with a different prescaler
    ------------------------------------------------
    for mode in 0 to 3 loop
      case mode is
        when 0      => v_ratio := 0;
        when 1      => v_ratio := 1;
        when 2      => v_ratio := 2;
        when others => v_ratio := 5;
      end case;
      log(ID_LOG_HDR, "2." & to_string(mode) & " SPI mode " & to_string(mode) & " (CPOL=" & to_string(mode/2) & ", CPHA=" & to_string(mode mod 2) & "), prescaler " & to_string(v_ratio), C_SCOPE);
      if mode >= 2 then
        configure('1', to_unsigned(mode mod 2, 1)(0), v_ratio);
      else
        configure('0', to_unsigned(mode mod 2, 1)(0), v_ratio);
      end if;

      test_single  (1                         , "Mode " & to_string(mode) & " single 1 byte");
      test_single  (4                         , "Mode " & to_string(mode) & " single 4 bytes");
      test_multi_tx(SPI_CMD_SIZE_DUAL_RAW, 2, 3, "Mode " & to_string(mode) & " dual TX 3 bytes");
      test_multi_rx(SPI_CMD_SIZE_DUAL_RAW, 2, 3, "Mode " & to_string(mode) & " dual RX 3 bytes");
      test_multi_tx(SPI_CMD_SIZE_QUAD_RAW, 4, 4, "Mode " & to_string(mode) & " quad TX 4 bytes");
      test_multi_rx(SPI_CMD_SIZE_QUAD_RAW, 4, 4, "Mode " & to_string(mode) & " quad RX 4 bytes");
      test_stop    (                          "Mode " & to_string(mode) & " multi commands + STOP");
    end loop;

    ------------------------------------------------
    log(ID_LOG_HDR, "3. KEEP command mode : nb_bytes on 6 bits", C_SCOPE);
    ------------------------------------------------
    v_ratio := 1;
    configure('0', '0', v_ratio);
    v_seed := v_seed + 1;
    slave_setup(1, true, v_seed);
    -- CONFIG : TX single 1 byte (not last), KEEP : same config, 9 bytes (nb_bytes[5:0] = 8), last
    write_reg(SPI_CMD, cmd(SPI_CMD_SIZE_SINGLE_RAW, true, false, false, 1), "CONFIG command TX single 1 byte");
    write_reg(SPI_CMD, SPI_CMD_CFG_KEEP_RAW or SPI_CMD_LAST_ENABLE_RAW or "00001000", "KEEP command 9 bytes (nb_bytes=8), last");
    for i in 0 to 9 loop
      write_reg(SPI_DATA, pattern(v_seed, i), "TX byte " & to_string(i));
    end loop;
    wait_end_of_transaction("KEEP mode");
    check_value(slave_nb_bytes, 10, ERROR, "KEEP mode : 1 + 9 bytes in the transaction");
    check_mosi(10, v_seed, 0, "KEEP mode");

    ------------------------------------------------
    log(ID_LOG_HDR, "4. Loopback (MISO = MOSI inside spi_master)", C_SCOPE);
    ------------------------------------------------
    configure('0', '1', 0, '1');
    v_seed := v_seed + 1;
    slave_setup(1, false, v_seed);
    write_reg(SPI_CMD, cmd(SPI_CMD_SIZE_SINGLE_RAW, true, true, true, 3), "Loopback command TX/RX 3 bytes");
    for i in 0 to 2 loop
      write_reg(SPI_DATA, pattern(v_seed, i), "Loopback TX byte " & to_string(i));
    end loop;
    for i in 0 to 2 loop
      check_reg(SPI_DATA, pattern(v_seed, i), "Loopback RX byte " & to_string(i));
    end loop;
    wait_end_of_transaction("Loopback");

    ------------------------------------------------
    log(ID_LOG_HDR, "5. spi_enable = 0 : no transfer", C_SCOPE);
    ------------------------------------------------
    write_reg(SPI_CFG, x"00", "Disable SPI");
    write_reg(SPI_CMD, cmd(SPI_CMD_SIZE_SINGLE_RAW, false, false, true, 1), "STOP-like command while disabled");
    for i in 1 to 200 loop
      wait until rising_edge(clk_i);
    end loop;
    check_value(slave_nb_trans, v_nb_trans, ERROR, "No transaction while spi_enable = 0");
    check_value(cs_b_oe_o, '0', ERROR, "CS pad disabled while spi_enable = 0");

    log(ID_LOG_HDR, "Simulation Finished", C_SCOPE);
    report_alert_counters(FINAL);
    std.env.stop;
    wait;
  end process p_main;

end sim;
