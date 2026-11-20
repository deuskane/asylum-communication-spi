-------------------------------------------------------------------------------
-- Title      : tb_SPI
-- Project    : SPI
-------------------------------------------------------------------------------
-- File       : tb_SPI.vhd
-- Author     : mrosiere
-------------------------------------------------------------------------------
-- Description: UVVM/SBI self-checking testbench of sbi_SPI with a SPI slave
--              model written in the bench.
--              TESTS_LIST     : scenarios run in sequence, separated by ','
--                               (tb_SPI_tests_pkg), each one after a reset
--                               pulse ; "" : build and elaboration only
--              RX_LATE_SAMPLE : RX_LATE_SAMPLE of the DUT
--              SLAVE_TV_NS    : the slave drives its outputs SLAVE_TV_NS
--                               after the shift edge (RX_LATE_SAMPLE : MISO
--                               round trip of one SCLK period)
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2025-05-29  1.0      mrosiere Created
-- 2026-10-05  2.0      mrosiere UVVM self-checking bench on sbi_SPI with a
--                               SPI slave model (replace the spi_master bench)
-- 2026-11-20  2.1      mrosiere TESTS_LIST, scenarios in tb_SPI_<group>_pkg,
--                               helpers in tb_SPI_bfm_pkg, reset pulse
--                               before each scenario
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
use     asylum.string_pkg.all;
use     asylum.spi_pkg.all;
use     asylum.SPI_csr_pkg.all;

library work;
use     work.tb_SPI_bfm_pkg.all;
use     work.tb_SPI_tests_pkg.all;

entity tb_SPI is
  generic (
    TESTS_LIST     : string  := "";     -- Scenarios, e.g. "single,status"
    RX_LATE_SAMPLE : boolean := false;  -- RX_LATE_SAMPLE of the DUT
    SLAVE_TV_NS    : natural := 0       -- Slave output valid time after the shift edge (ns)
  );
end tb_SPI;

architecture sim of tb_SPI is

  constant C_SCOPE         : string  := "TB_SPI";
  constant C_SLAVE_TV      : time    := SLAVE_TV_NS * 1 ns;

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

  signal   slave_io        : std_logic_vector(8-1 downto 0) := (others => 'Z');

  -- Slave model configuration (set by the sequencer), results of the slave
  -- model and of the CS# monitor, pads seen by the scenarios
  signal   tb_req          : tb_req_t := C_TB_REQ_INIT;
  signal   tb_rsp          : tb_rsp_t := (slave_mosi     => (others => (others => '0')),
                                          slave_nb_bytes => 0,
                                          slave_nb_trans => 0,
                                          slave_period   => 0 ns,
                                          slave_half     => 0 ns,
                                          cs_high_last   => 0 ns);
  signal   pads            : pads_t;

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
     ,RX_LATE_SAMPLE        => RX_LATE_SAMPLE
     ,HANDLE_HOLD_WP        => false
     ,CS_HIGH_MIN_CYCLES    => C_CS_HIGH_MIN
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

  pads   <= (sclk    => sclk
            ,cs_b    => cs_b
            ,sclk_o  => sclk_o
            ,sclk_oe => sclk_oe_o
            ,cs_b_o  => cs_b_o
            ,cs_b_oe => cs_b_oe_o
            ,io_oe   => io_oe_o
            );

  -----------------------------------------------------------------------------
  -- CS# high time between 2 transactions (tSHSL) >= CS_HIGH_MIN_CYCLES
  -----------------------------------------------------------------------------
  p_cs_high : process
    variable v_t_rise : time;
  begin
    wait until cs_b = '1';
    v_t_rise := now;
    wait until cs_b = '0';
    tb_rsp.cs_high_last <= now - v_t_rise;
    if now - v_t_rise < C_CS_HIGH_MIN*C_CLK_PERIOD then
      alert(ERROR, "CS# high during " & to_string((now - v_t_rise) / C_CLK_PERIOD) & " clk cycles between 2 transactions, less than CS_HIGH_MIN_CYCLES", C_SCOPE);
    end if;
  end process p_cs_high;

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
      if tb_req.slave_drive then
        v_byte := tb_req.slave_miso((v_out_bits/8) mod C_NB_MAX);
        v_pos  := 7 - (v_out_bits mod 8);
        if v_lanes = 1 then
          slave_io(SPI_IO_MISO) <= transport v_byte(v_pos) after C_SLAVE_TV;
        else
          for l in 0 to v_lanes-1 loop
            slave_io(l) <= transport v_byte(v_pos - (v_lanes-1) + l) after C_SLAVE_TV;
          end loop;
        end if;
      end if;
      v_out_bits := v_out_bits + v_lanes;
    end procedure;

  begin
    slave_io <= (others => 'Z');
    wait until cs_b = '0';

    check_value(sclk, tb_req.slave_cpol, ERROR, "SPI slave : SCLK at idle level (CPOL) when CS falls", C_SCOPE, ID_NEVER);

    v_lanes    := tb_req.slave_lanes;
    v_bitcnt   := 0;
    v_byte_idx := 0;
    v_out_bits := 0;
    v_nb_lead  := 0;
    v_nb_trail := 0;
    v_t_lead1  := 0 ns;
    v_t_lead2  := 0 ns;
    v_t_trail1 := 0 ns;
    v_mosi     := (others => (others => '0'));

    if tb_req.slave_cpha = '0' then
      drive_next;
    end if;

    loop
      wait on sclk, cs_b;
      exit when cs_b /= '0';

      if sclk'event then
        v_leading := (sclk /= tb_req.slave_cpol);
        v_sample  := (v_leading and tb_req.slave_cpha = '0') or (not v_leading and tb_req.slave_cpha = '1');

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
    check_value(sclk    , tb_req.slave_cpol, ERROR, "SPI slave : SCLK at idle level (CPOL) when CS rises", C_SCOPE, ID_NEVER);
    check_value(v_bitcnt, 0         , ERROR, "SPI slave : no partial byte when CS rises", C_SCOPE, ID_NEVER);
    slave_io              <= (others => 'Z');
    tb_rsp.slave_mosi     <= v_mosi;
    tb_rsp.slave_nb_bytes <= v_byte_idx;
    tb_rsp.slave_period   <= v_t_lead2  - v_t_lead1;
    tb_rsp.slave_half     <= v_t_trail1 - v_t_lead1;
    tb_rsp.slave_nb_trans <= tb_rsp.slave_nb_trans + 1;
  end process p_slave;

  -----------------------------------------------------------------------------
  -- Sequencer
  -----------------------------------------------------------------------------
  p_sequencer : process
  begin
    log(ID_LOG_HDR, "RX_LATE_SAMPLE = " & to_string(RX_LATE_SAMPLE) & ", SLAVE_TV_NS = " & to_string(SLAVE_TV_NS) & ", TESTS_LIST : " & TESTS_LIST, C_SCOPE);

    -- Every name is checked before the first scenario
    for i in 1 to list_length(TESTS_LIST) loop
      if not list_contains(C_TESTS, list_item(TESTS_LIST, i))
      then
        alert(TB_ERROR, "Unknown test """ & list_item(TESTS_LIST, i) & """ (tests : " & C_TESTS & ")", C_SCOPE);
      end if;
    end loop;

    for i in 1 to list_length(TESTS_LIST) loop
      -- Reset pulse, bus inactive (the slave model is idle while CS# is
      -- high) : each scenario starts from reset
      sbi_if <= init_sbi_if_signals(SPI_ADDR_WIDTH, SPI_DATA_WIDTH);
      do_reset(clk_i, arst_b_i);

      log(ID_LOG_HDR, "TEST " & integer'image(i) & "/" & integer'image(list_length(TESTS_LIST)) & " : " & list_item(TESTS_LIST, i), C_SCOPE);
      run_test(list_item(TESTS_LIST, i), clk_i, sbi_if, tb_req, tb_rsp, pads);
    end loop;

    report_alert_counters(FINAL);
    std.env.stop;
    wait;
  end process p_sequencer;

end sim;
