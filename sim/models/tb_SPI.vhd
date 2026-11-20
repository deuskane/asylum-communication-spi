-------------------------------------------------------------------------------
-- Title      : tb_SPI
-- Project    : SPI
-------------------------------------------------------------------------------
-- File       : tb_SPI.vhd
-- Author     : mrosiere
-- Company    :
-- Created    : 2025-05-31
-- Last update: 2026-08-04
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: UVVM/SBI testbench for the SBI SPI DUT with a flash model
--              TESTS_LIST : scenarios run in sequence, separated by ','
--                           (tb_SPI_models_tests_pkg), each one after a
--                           reset pulse ; "" : build and elaboration only
--              MODEL      : flash model (the dual and quad scenarios need
--                           s25fl512s)
-------------------------------------------------------------------------------
-- Copyright (c) 2025
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2025-05-31  1.0      mrosiere Created
-- 2026-08-01  1.1      mrosiere Use UVVM SBI VIP and clock generator
-- 2026-11-20  1.2      mrosiere TESTS_LIST, scenarios in
--                               tb_SPI_models_<group>_pkg, helpers in
--                               tb_SPI_models_bfm_pkg, reset pulse before
--                               each scenario
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
use     asylum.string_pkg.all;
use     asylum.techmap_pkg.all;

library work;
use     work.tb_SPI_models_bfm_pkg.all;
use     work.tb_SPI_models_tests_pkg.all;

entity tb is
  generic (
    TESTS_LIST : string := "";       -- Scenarios, e.g. "read,fast_read"
    MODEL      : string := "m25p40"  -- m25p40 s25fl064p
  );
end tb;

architecture sim of tb is

  constant C_SCOPE         : string := "TB_SPI";
  constant HANDLE_HOLD_WP  : boolean := true;

  signal clk_i             : std_logic := '0';
  signal clk_ena           : boolean   := true;
  signal arst_b_i          : std_logic := '0';

  signal sbi_ini           : sbi_ini_t(addr (SPI_ADDR_WIDTH-1 downto 0),
                                       wdata(SPI_DATA_WIDTH-1 downto 0));
  signal sbi_tgt           : sbi_tgt_t(rdata(SPI_DATA_WIDTH-1 downto 0));
  signal sbi_if            : t_sbi_if(addr (SPI_ADDR_WIDTH-1 downto 0),
                                      wdata(SPI_DATA_WIDTH-1 downto 0),
                                      rdata(SPI_DATA_WIDTH-1 downto 0));

  signal sclk_o            : std_logic;
  signal sclk_oe_o         : std_logic;
  signal cs_b_o            : std_logic;
  signal cs_b_oe_o         : std_logic;
  signal io_o              : std_logic_vector(8-1 downto 0);
  signal io_i              : std_logic_vector(8-1 downto 0);
  signal io_oe_o           : std_logic_vector(8-1 downto 0);
  
  signal SCLK              : std_logic;
  signal CS_B              : std_logic;
  signal MOSI              : std_logic;
  signal MISO              : std_logic;
  signal RSTNeg            : std_logic;
  signal WPNeg             : std_logic;
  signal HOLDNeg           : std_logic;

begin

  clock_generator(clk_i, clk_ena, 20 ns, "TB Clock");

  dut : sbi_SPI
    generic map (
      NAME                  => "SPI"
     ,USER_DEFINE_PRESCALER => true
     ,PRESCALER_RATIO       => x"0F"
     ,DEPTH_CMD             => 4
     ,DEPTH_TX              => 4
     ,DEPTH_RX              => 4
     ,HANDLE_HOLD_WP        => HANDLE_HOLD_WP
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

  RSTNeg   <= '1'; -- PULL UP
  WPNeg    <= 'H'; -- PULL UP
  HOLDNeg  <= 'H'; -- PULL UP
  SCLK     <= 'H'; -- PULL UP
  CS_B     <= 'H'; -- PULL UP
  MISO     <= 'H'; -- PULL UP
  MOSI     <= 'H'; -- PULL UP

  IOBUF_SCLK : iobuf
    port map
     (buf_io     => SCLK
     ,d_i        => sclk_o
     ,d_o        => open
     ,oe_i       => sclk_oe_o
     ,ie_i       => '0'
  );

  IOBUF_CS_B : iobuf
    port map
     (buf_io     => CS_B
     ,d_i        => cs_b_o
     ,d_o        => open
     ,oe_i       => cs_b_oe_o
     ,ie_i       => '0'
  );

  IOBUF_MOSI : iobuf
    port map
     (buf_io     => MOSI
     ,d_i        => io_o    (SPI_IO_MOSI)
     ,d_o        => io_i    (SPI_IO_MOSI)
     ,oe_i       => io_oe_o (SPI_IO_MOSI)
     ,ie_i       => '1'
  );
  
  IOBUF_MISO : iobuf
    port map
     (buf_io     => MISO
     ,d_i        => io_o    (SPI_IO_MISO)
     ,d_o        => io_i    (SPI_IO_MISO)
     ,oe_i       => io_oe_o (SPI_IO_MISO)
     ,ie_i       => '1'
  );
  
  IOBUF_HOLDNeg : iobuf
    port map
     (buf_io     => HOLDNeg
     ,d_i        => io_o    (SPI_IO_HOLD_B)
     ,d_o        => io_i    (SPI_IO_HOLD_B)
     ,oe_i       => io_oe_o (SPI_IO_HOLD_B)
     ,ie_i       => '1'
  );

  IOBUF_WPNeg : iobuf
    port map
     (buf_io     => WPNeg
     ,d_i        => io_o    (SPI_IO_WP_B)
     ,d_o        => io_i    (SPI_IO_WP_B)
     ,oe_i       => io_oe_o (SPI_IO_WP_B)
     ,ie_i       => '1'
  );
  
  gen_at25df161:
  if MODEL = "at25df161" 
  generate
    mem : entity work.at25df161(vhdl_behavioral)
      generic map (
        mem_file_name  => "memory.mem"
       ,otp_file_name  => "none"
       ,UserPreload    => true
       ,TimingChecksOn => true
       ,MsgOn          => true
       ,XOn            => true
      )
      PORT MAP
      (
        SCK     => SCLK
       ,SI      => MOSI
       ,CSNeg   => CS_B
       ,HOLDNeg => HOLDNeg
       ,WPNeg   => WPNeg
       ,SO      => MISO
      );
  end generate;

 gen_cy15b104qs:
  if MODEL = "cy15b104qs" 
  generate
    mem : entity work.cy15b104qs(vhdl_behavioral_static_memory_allocation)
      generic map (
        mem_file_name  => "memory.mem"
       ,otp_file_name  => "none"
       ,UserPreload    => true
       ,TimingChecksOn => true
       ,MsgOn          => true
       ,XOn            => true
       ,TimingModel    => "CY15B104QSN-108SXI"
      )
      PORT MAP
      (
        SCK     => SCLK
       ,SI      => MOSI
       ,CSNeg   => CS_B
       ,WPNeg   => WPNeg
       ,SO      => MISO
       ,RESETNeg=> RSTNeg
      );
  end generate;

  gen_cy15v104qs:
  if MODEL = "cy15v104qs" 
  generate
    mem : entity work.cy15v104qs(vhdl_behavioral_static_memory_allocation)
      generic map (
        mem_file_name  => "memory.mem"
       ,otp_file_name  => "none"
       ,UserPreload    => true
       ,TimingChecksOn => true
       ,MsgOn          => true
       ,XOn            => true
       ,TimingModel    => "CY15V104QSN-108SXI"
      )
      PORT MAP
      (
        SCK     => SCLK
       ,SI      => MOSI
       ,CSNeg   => CS_B
       ,WPNeg   => WPNeg
       ,SO      => MISO
       ,RESETNeg=> RSTNeg

      );
  end generate;

  gen_m25p40:
  if MODEL = "m25p40" 
  generate
    mem : entity work.m25p40(vhdl_behavioral)
      generic map (
        mem_file_name  => "memory.mem"
       ,UserPreload    => true
       ,DebugInfo      => true
       ,TimingChecksOn => true
       ,MsgOn          => true
       ,XOn            => true
      )
      port map (
        D       => MOSI
       ,Q       => MISO
       ,C       => SCLK
       ,SNeg    => CS_B
       ,WNeg    => WPNeg
       ,HOLDNeg => HOLDNeg
      );
  end generate;

  gen_m45pe80:
  if MODEL = "m45pe80" 
  generate
    mem : entity work.m45pe80(vhdl_behavioral)
      generic map (
        mem_file_name  => "memory.mem"
       ,UserPreload    => true
       ,TimingChecksOn => true
       ,MsgOn          => true
       ,XOn            => true
      )
      port map (
        D       => MOSI
       ,Q       => MISO
       ,C       => SCLK
       ,SNeg    => CS_B
       ,WNeg    => WPNeg
       ,ResetNeg=> RSTNeg
      );
  end generate;

  gen_s25fl064p:
  if MODEL = "s25fl064p" 
  generate
    mem : entity work.s25fl064p(vhdl_behavioral)
      generic map (
        mem_file_name  => "memory.mem"
       ,otp_file_name  => "none"
       ,UserPreload    => true
       ,TimingChecksOn => true
       ,MsgOn          => true
       ,XOn            => true
      )
      PORT MAP
      (
        SCK     => SCLK
       ,SI      => MOSI
       ,CSNeg   => CS_B
       ,HOLDNeg => HOLDNeg
       ,WPNeg   => WPNeg
       ,SO      => MISO
      );
  end generate;

  gen_s25fl512s:
  if MODEL = "s25fl512s" 
  generate
    mem : entity work.s25fl512s(vhdl_behavioral_static_memory_allocation)
      generic map (
        mem_file_name  => "memory.mem"
       ,otp_file_name  => "none"
       ,UserPreload    => true
       ,TimingChecksOn => true
       ,MsgOn          => true
       ,XOn            => true
       ,TimingModel    => "S25FL512SAGMFI010_F_30pF"
       ,LongTimming    => false
      )
      PORT MAP
      (
        SCK     => SCLK
       ,SI      => MOSI
       ,CSNeg   => CS_B
       ,HOLDNeg => HOLDNeg
       ,WPNeg   => WPNeg
       ,SO      => MISO
       ,RSTNeg  => RSTNeg
      );
  end generate;

  gen_s35hl256t:
  if MODEL = "s35hl256t" 
  generate
    mem : entity work.s35hl256t(vhdl_behavioral)
      generic map (
        mem_file_name  => "memory.mem"
       ,otp_file_name  => "none"
       ,UserPreload    => true
       ,TimingChecksOn => true
       ,MsgOn          => true
       ,XOn            => true
       ,TimingModel    => "S35HL256TDPBHI010_15pF"
      )
      PORT MAP
      (
        SCK         => SCLK
       ,SI          => MOSI
       ,CSNeg       => CS_B
       ,IO3RESETNeg => HOLDNeg
       ,WPNeg       => WPNeg
       ,SO          => MISO
       ,RESETNeg    => RSTNeg
      );
  end generate;

  sbi_ini.cs                               <= sbi_if.cs;
  sbi_ini.addr                             <= std_logic_vector(sbi_if.addr(SPI_ADDR_WIDTH-1 downto 0));
  sbi_ini.re                               <= sbi_if.rena;
  sbi_ini.we                               <= sbi_if.wena;
  sbi_ini.wdata                            <= sbi_if.wdata(SPI_DATA_WIDTH-1 downto 0);
  sbi_if.ready                             <= sbi_tgt.ready;
  sbi_if.rdata(SPI_DATA_WIDTH-1 downto 0)  <= sbi_tgt.rdata;

  -- HOLD# and WP# must be inactive (high) while CS# is high (reset, idle, CS# edges)
  p_check_hold_wp : process (clk_i) is
  begin
    if rising_edge(clk_i) and HANDLE_HOLD_WP and to_x01(CS_B) = '1'
    then
      if to_x01(HOLDNeg) /= '1' then
        alert(ERROR, "HOLD# active while CS# is high", C_SCOPE);
      end if;
      if to_x01(WPNeg) /= '1' then
        alert(ERROR, "WP# active while CS# is high", C_SCOPE);
      end if;
    end if;
  end process p_check_hold_wp;

  -----------------------------------------------------------------------------
  -- Sequencer
  -----------------------------------------------------------------------------
  p_sequencer : process
  begin
    log(ID_LOG_HDR, "MODEL = " & MODEL & ", TESTS_LIST : " & TESTS_LIST, C_SCOPE);

    -- Every name is checked before the first scenario
    for i in 1 to list_length(TESTS_LIST) loop
      if not list_contains(C_TESTS, list_item(TESTS_LIST, i))
      then
        alert(TB_ERROR, "Unknown test """ & list_item(TESTS_LIST, i) & """ (tests : " & C_TESTS & ")", C_SCOPE);
      elsif not applies(list_item(TESTS_LIST, i), MODEL)
      then
        alert(TB_ERROR, "Test """ & list_item(TESTS_LIST, i) & """ needs MODEL = s25fl512s", C_SCOPE);
      end if;
    end loop;

    for i in 1 to list_length(TESTS_LIST) loop
      -- Reset pulse, bus inactive : each scenario starts from reset (the
      -- flash model is not reset)
      sbi_if <= init_sbi_if_signals(SPI_ADDR_WIDTH, SPI_DATA_WIDTH);
      do_reset(clk_i, arst_b_i);

      log(ID_LOG_HDR, "TEST " & integer'image(i) & "/" & integer'image(list_length(TESTS_LIST)) & " : " & list_item(TESTS_LIST, i), C_SCOPE);
      run_test(list_item(TESTS_LIST, i), clk_i, sbi_if);
    end loop;

    report_alert_counters(FINAL);
    std.env.stop;
    wait;
  end process p_sequencer;

end sim;
