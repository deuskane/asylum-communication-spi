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
-- Description: UVVM/SBI testbench for the SBI SPI DUT
-------------------------------------------------------------------------------
-- Copyright (c) 2025
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2025-05-31  1.0      mrosiere Created
-- 2026-08-01  1.1      mrosiere Use UVVM SBI VIP and clock generator
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
use     asylum.techmap_pkg.all;

entity tb is
  generic (
    MODEL : string := "m25p40" -- m25p40 s25fl064p
  );
end tb;

architecture sim of tb is

  constant C_SCOPE         : string := "TB_SPI";
  constant SPI_ADDR_WIDTH  : natural := 2;
  constant SPI_DATA_WIDTH  : natural := 8;

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
  signal mosi_o            : std_logic;
  signal mosi_oe_o         : std_logic;
  signal miso_i            : std_logic;

  signal SCLK              : std_logic;
  signal CS_B              : std_logic;
  signal MOSI              : std_logic;
  signal MISO              : std_logic;
  signal RSTNeg            : std_logic;
  signal WPNeg             : std_logic;
  signal HOLDNeg           : std_logic;

begin

  arst_b_i <= '0', '1' after 100 ns;

  clock_generator(clk_i, clk_ena, 20 ns, "TB Clock");

  dut : sbi_SPI
    generic map (
      NAME                  => "SPI"
     ,USER_DEFINE_PRESCALER => true
     ,PRESCALER_RATIO       => x"0F"
     ,DEPTH_CMD             => 4
     ,DEPTH_TX              => 4
     ,DEPTH_RX              => 4
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
     ,mosi_o     => mosi_o
     ,mosi_oe_o  => mosi_oe_o
     ,miso_i     => miso_i
    );

  RSTNeg   <= '1'; -- PULL UP
  WPNeg    <= '1'; -- PULL UP
  HOLDNeg  <= '1'; -- PULL UP
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
     ,d_i        => mosi_o
     ,d_o        => open
     ,oe_i       => mosi_oe_o
     ,ie_i       => '0'
  );
  
  IOBUF_MISO : iobuf
    port map
     (buf_io     => MISO
     ,d_i        => '0'
     ,d_o        => miso_i
     ,oe_i       => '0'
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

  process
    
    variable V_SBI_BFM_CONFIG_SPI : t_sbi_bfm_config := C_SBI_BFM_CONFIG_DEFAULT;
    
  begin

    -- BFM Configuration
    V_SBI_BFM_CONFIG_SPI.max_wait_cycles            := 1000;

    sbi_if <= init_sbi_if_signals(SPI_ADDR_WIDTH, SPI_DATA_WIDTH);
    wait until arst_b_i = '1';
    wait until rising_edge(clk_i);

    log(ID_SEQUENCER, "Reset released, starting SPI SBI test", C_SCOPE);

    wait for 800 us;

    log(ID_LOG_HDR, "Configuration of the Prescaler (Divide by 6)", C_SCOPE);
    sbi_write(addr_value => SPI_PRESCALER, data_value => x"02", msg => "Set prescaler", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_PRESCALER, data_exp   => x"02", msg => "Read back prescaler", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

    log(ID_LOG_HDR, "Configure and verify SPI control registers", C_SCOPE);
    sbi_write(addr_value => SPI_CFG      , data_value => x"01", msg => "Enable SPI", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_CFG      , data_exp   => x"01", msg => "Read back enabled config", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);


    log(ID_LOG_HDR, "Perform Single Read (0x03) - At address 0x000005 (24b)", C_SCOPE);

    sbi_write(addr_value => SPI_CMD      , data_value => x"23", msg => "SPI TX 4 Bytes",           clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_write(addr_value => SPI_DATA     , data_value => x"03", msg => "SPI Instruction 0x03",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_write(addr_value => SPI_DATA     , data_value => x"00", msg => "SPI Address 0x000005",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_write(addr_value => SPI_DATA     , data_value => x"00", msg => "SPI Address 0x000005",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_write(addr_value => SPI_DATA     , data_value => x"05", msg => "SPI Address 0x000005",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

    sbi_write(addr_value => SPI_CMD      , data_value => x"13", msg => "SPI RX 4 Bytes",           clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"06", msg => "Read Byte @ 0x000005",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"07", msg => "Read Byte @ 0x000006",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"08", msg => "Read Byte @ 0x000007",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"09", msg => "Read Byte @ 0x000008",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

    sbi_write(addr_value => SPI_CMD      , data_value => x"40", msg => "Stop command",             clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

    log(ID_LOG_HDR, "Configuration of the Prescaler (Divide by 2)", C_SCOPE);
    sbi_write(addr_value => SPI_PRESCALER, data_value => x"00", msg => "Set prescaler",            clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

    log(ID_LOG_HDR, "Perform Fast Read (0x0B) - At address 0x000010 (24b)", C_SCOPE);

    sbi_write(addr_value => SPI_CMD      , data_value => x"23", msg => "SPI TX 4 Bytes",           clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_write(addr_value => SPI_DATA     , data_value => x"0B", msg => "SPI Instruction 0x0B",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_write(addr_value => SPI_DATA     , data_value => x"00", msg => "SPI Address 0x000010",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_write(addr_value => SPI_DATA     , data_value => x"00", msg => "SPI Address 0x000010",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_write(addr_value => SPI_DATA     , data_value => x"10", msg => "SPI Address 0x000010",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

    sbi_write(addr_value => SPI_CMD      , data_value => x"00", msg => "Dummy Cycle",              clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

    sbi_write(addr_value => SPI_CMD      , data_value => x"10", msg => "SPI RX 1 Bytes without last", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_write(addr_value => SPI_CMD      , data_value => x"c6", msg => "SPI RX 7 Bytes with last", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"11", msg => "Read Byte @ 0x000010",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"12", msg => "Read Byte @ 0x000011",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"13", msg => "Read Byte @ 0x000012",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"14", msg => "Read Byte @ 0x000013",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"15", msg => "Read Byte @ 0x000014",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"16", msg => "Read Byte @ 0x000015",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"17", msg => "Read Byte @ 0x000016",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"18", msg => "Read Byte @ 0x000017",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

    for i in 1 to 10 
    loop
      wait until rising_edge(clk_i);
    end loop;

    report "[TB_SPI] All SBI SPI checks passed";
    report_alert_counters(FINAL);
    std.env.stop;
    wait;
  end process;

end sim;
