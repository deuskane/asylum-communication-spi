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

  process
    
    variable V_SBI_BFM_CONFIG_SPI : t_sbi_bfm_config := C_SBI_BFM_CONFIG_DEFAULT;
    variable rdata                : std_logic_vector(8-1 downto 0);

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
    sbi_write(addr_value => SPI_CMD      , data_value => x"ce", msg => "SPI RX 15 Bytes with last",clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"11", msg => "Read Byte @ 0x000010",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"12", msg => "Read Byte @ 0x000011",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"13", msg => "Read Byte @ 0x000012",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"14", msg => "Read Byte @ 0x000013",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"15", msg => "Read Byte @ 0x000014",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"16", msg => "Read Byte @ 0x000015",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"17", msg => "Read Byte @ 0x000016",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"18", msg => "Read Byte @ 0x000017",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"19", msg => "Read Byte @ 0x000018",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"1a", msg => "Read Byte @ 0x000019",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"1b", msg => "Read Byte @ 0x00001a",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"1c", msg => "Read Byte @ 0x00001b",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"1d", msg => "Read Byte @ 0x00001c",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"1e", msg => "Read Byte @ 0x00001d",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"1f", msg => "Read Byte @ 0x00001e",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"20", msg => "Read Byte @ 0x00001f",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

    if MODEL = "s25fl512s" 
    then

      -----------------------------------------------------------------------------------
      -- Read 1-1-2
      -----------------------------------------------------------------------------------
      log(ID_LOG_HDR, "Perform DOR (0x3B) - At address 0x000020 (24b)", C_SCOPE);

      sbi_write(addr_value => SPI_CMD      , data_value => x"23", msg => "SPI TX 4 Bytes",           clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"3B", msg => "SPI Instruction 0x3B",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"00", msg => "SPI Address 0x000020",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"00", msg => "SPI Address 0x000020",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"20", msg => "SPI Address 0x000020",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
  
      sbi_write(addr_value => SPI_CMD      , data_value => x"00", msg => "Dummy Cycle",              clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
  
      sbi_write(addr_value => SPI_CMD      , data_value => x"17", msg => "SPI RX 4 Bytes",           clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_CMD      , data_value => x"CB", msg => "SPI RX 12 Bytes with last",clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"21", msg => "Read Byte @ 0x000020",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"22", msg => "Read Byte @ 0x000021",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"23", msg => "Read Byte @ 0x000022",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"24", msg => "Read Byte @ 0x000023",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"25", msg => "Read Byte @ 0x000024",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"26", msg => "Read Byte @ 0x000025",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"27", msg => "Read Byte @ 0x000026",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"28", msg => "Read Byte @ 0x000027",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"29", msg => "Read Byte @ 0x000028",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"2a", msg => "Read Byte @ 0x000029",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"2b", msg => "Read Byte @ 0x00002a",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"2c", msg => "Read Byte @ 0x00002b",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"2d", msg => "Read Byte @ 0x00002c",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"2e", msg => "Read Byte @ 0x00002d",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"2f", msg => "Read Byte @ 0x00002e",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"30", msg => "Read Byte @ 0x00002f",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

      -----------------------------------------------------------------------------------
      -- Read 1-2-2
      -----------------------------------------------------------------------------------
      log(ID_LOG_HDR, "Perform DIOR (0xBB) - At address 0x000040 (24b)", C_SCOPE);

      sbi_write(addr_value => SPI_CMD      , data_value => x"20", msg => "SPI TX 1 Bytes",           clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"BB", msg => "SPI Instruction 0xBB",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_CMD      , data_value => x"26", msg => "SPI TX 3 Bytes",           clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"00", msg => "SPI Address 0x000040",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"00", msg => "SPI Address 0x000040",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"46", msg => "SPI Address 0x000040",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
  
      sbi_write(addr_value => SPI_CMD      , data_value => x"04", msg => "Dummy Cycle",              clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
  
      sbi_write(addr_value => SPI_CMD      , data_value => x"17", msg => "SPI RX 4 Bytes",           clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_CMD      , data_value => x"CB", msg => "SPI RX 12 Bytes with last",clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"47", msg => "Read Byte @ 0x000046",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"48", msg => "Read Byte @ 0x000047",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"49", msg => "Read Byte @ 0x000048",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"4a", msg => "Read Byte @ 0x000049",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"4b", msg => "Read Byte @ 0x00004a",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"4c", msg => "Read Byte @ 0x00004b",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"4d", msg => "Read Byte @ 0x00004c",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"4e", msg => "Read Byte @ 0x00004d",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"4f", msg => "Read Byte @ 0x00004e",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"50", msg => "Read Byte @ 0x00004f",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"51", msg => "Read Byte @ 0x000050",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"52", msg => "Read Byte @ 0x000051",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"53", msg => "Read Byte @ 0x000052",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"54", msg => "Read Byte @ 0x000053",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"55", msg => "Read Byte @ 0x000054",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"56", msg => "Read Byte @ 0x000055",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

      -----------------------------------------------------------------------------------
      -- Read 1-1-4
      -----------------------------------------------------------------------------------

      log(ID_LOG_HDR, "WREN Enable write", C_SCOPE);
      sbi_write(addr_value => SPI_CMD      , data_value => x"60", msg => "SPI TX 1 Byte with last", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"06", msg => "SPI Instruction 0x06",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

      log(ID_LOG_HDR, "WRR Enable QUAD - CR Bit 1 = 1", C_SCOPE);
      sbi_write(addr_value => SPI_CMD      , data_value => x"62", msg => "SPI TX 3 Bytes with last", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"01", msg => "SPI Instruction 0x01",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"00", msg => "Status Register",          clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"02", msg => "Configuration Register",   clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

      log(ID_LOG_HDR, "WRDI Disable write", C_SCOPE);
      sbi_write(addr_value => SPI_CMD      , data_value => x"60", msg => "SPI TX 1 Bytes with last", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"04", msg => "SPI Instruction 0x04",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

      log(ID_LOG_HDR, "RDSR1 until WIP is 0", C_SCOPE);
      sbi_write(addr_value => SPI_CMD      , data_value => x"20", msg => "SPI TX 1 Byte",            clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"05", msg => "SPI Instruction 0x05",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

      -- Poll SR1 until WIP (bit) is 0: loop while rdata /= x"00"
      rdata := x"FF";
      while rdata /= x"00" loop
        -- small delay between polls
        --wait for 1 us;
        sbi_write(addr_value => SPI_CMD      , data_value => x"10", msg => "SPI RX 1 Bytes",           clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
        sbi_read (addr_value => SPI_DATA     , data_value => rdata, msg => "SR1 -> WIP",               clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      end loop;
  
      sbi_write(addr_value => SPI_CMD      , data_value => x"40", msg => "Stop command",             clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      
      log(ID_LOG_HDR, "Perform QOR (0x6B) - At address 0x000020 (24b)", C_SCOPE);

      sbi_write(addr_value => SPI_CMD      , data_value => x"23", msg => "SPI TX 4 Bytes",           clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"6B", msg => "SPI Instruction 0x3B",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"00", msg => "SPI Address 0x000030",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"00", msg => "SPI Address 0x000030",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"30", msg => "SPI Address 0x000030",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
  
      sbi_write(addr_value => SPI_CMD      , data_value => x"00", msg => "Dummy Cycle",              clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
  
      sbi_write(addr_value => SPI_CMD      , data_value => x"1b", msg => "SPI RX 4 Bytes",           clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_CMD      , data_value => x"CB", msg => "SPI RX 12 Bytes with last",clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"31", msg => "Read Byte @ 0x000030",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"32", msg => "Read Byte @ 0x000031",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"33", msg => "Read Byte @ 0x000032",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"34", msg => "Read Byte @ 0x000033",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"35", msg => "Read Byte @ 0x000034",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"36", msg => "Read Byte @ 0x000035",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"37", msg => "Read Byte @ 0x000036",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"38", msg => "Read Byte @ 0x000037",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"39", msg => "Read Byte @ 0x000038",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"3a", msg => "Read Byte @ 0x000039",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"3b", msg => "Read Byte @ 0x00003a",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"3c", msg => "Read Byte @ 0x00003b",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"3d", msg => "Read Byte @ 0x00003c",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"3e", msg => "Read Byte @ 0x00003d",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"3f", msg => "Read Byte @ 0x00003e",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"40", msg => "Read Byte @ 0x00003f",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

      -----------------------------------------------------------------------------------
      -- Read 1-4-4
      -----------------------------------------------------------------------------------
      log(ID_LOG_HDR, "Perform QIOR (0xEB) - At address 0x000046 (24b)", C_SCOPE);

      sbi_write(addr_value => SPI_CMD      , data_value => x"20", msg => "SPI TX 1 Bytes",           clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"EB", msg => "SPI Instruction 0xEB",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_CMD      , data_value => x"2b", msg => "SPI TX 4 Bytes",           clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"00", msg => "SPI Address 0x000046",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"00", msg => "SPI Address 0x000046",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"46", msg => "SPI Address 0x000046",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"00", msg => "Mode 0",                   clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

      sbi_write(addr_value => SPI_CMD      , data_value => x"09", msg => "Dummy Cycle",              clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
  
      sbi_write(addr_value => SPI_CMD      , data_value => x"1b", msg => "SPI RX 4 Bytes",           clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_CMD      , data_value => x"CB", msg => "SPI RX 12 Bytes with last",clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"47", msg => "Read Byte @ 0x000046",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"48", msg => "Read Byte @ 0x000047",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"49", msg => "Read Byte @ 0x000048",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"4a", msg => "Read Byte @ 0x000049",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"4b", msg => "Read Byte @ 0x00004a",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"4c", msg => "Read Byte @ 0x00004b",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"4d", msg => "Read Byte @ 0x00004c",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"4e", msg => "Read Byte @ 0x00004d",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"4f", msg => "Read Byte @ 0x00004e",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"50", msg => "Read Byte @ 0x00004f",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"51", msg => "Read Byte @ 0x000050",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"52", msg => "Read Byte @ 0x000051",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"53", msg => "Read Byte @ 0x000052",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"54", msg => "Read Byte @ 0x000053",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"55", msg => "Read Byte @ 0x000054",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"56", msg => "Read Byte @ 0x000055",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

      -----------------------------------------------------------------------------------
      -- Read 1-4-4 in continous mode
      -----------------------------------------------------------------------------------
      log(ID_LOG_HDR, "Perform QIOR (0xEB) in continous mode - At address 0x000066 and 0x000086 (24b)", C_SCOPE);

      sbi_write(addr_value => SPI_CMD      , data_value => x"20", msg => "SPI TX 1 Bytes",           clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"EB", msg => "SPI Instruction 0xEB",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_CMD      , data_value => x"2b", msg => "SPI TX 4 Bytes",           clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"00", msg => "SPI Address 0x000066",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"00", msg => "SPI Address 0x000066",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"66", msg => "SPI Address 0x000066",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"a0", msg => "Mode 0",                   clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

      sbi_write(addr_value => SPI_CMD      , data_value => x"09", msg => "Dummy Cycle",              clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
  
      sbi_write(addr_value => SPI_CMD      , data_value => x"1b", msg => "SPI RX 4 Bytes",           clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_CMD      , data_value => x"CB", msg => "SPI RX 12 Bytes with last",clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"67", msg => "Read Byte @ 0x000066",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"68", msg => "Read Byte @ 0x000067",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"69", msg => "Read Byte @ 0x000068",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"6a", msg => "Read Byte @ 0x000069",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"6b", msg => "Read Byte @ 0x00006a",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"6c", msg => "Read Byte @ 0x00006b",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"6d", msg => "Read Byte @ 0x00006c",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"6e", msg => "Read Byte @ 0x00006d",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"6f", msg => "Read Byte @ 0x00006e",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"70", msg => "Read Byte @ 0x00006f",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"71", msg => "Read Byte @ 0x000070",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"72", msg => "Read Byte @ 0x000071",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"73", msg => "Read Byte @ 0x000072",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"74", msg => "Read Byte @ 0x000073",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"75", msg => "Read Byte @ 0x000074",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"76", msg => "Read Byte @ 0x000075",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

      sbi_write(addr_value => SPI_CMD      , data_value => x"2b", msg => "SPI TX 4 Bytes",           clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"00", msg => "SPI Address 0x000086",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"00", msg => "SPI Address 0x000086",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"86", msg => "SPI Address 0x000086",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_DATA     , data_value => x"a0", msg => "Mode 0",                   clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

      sbi_write(addr_value => SPI_CMD      , data_value => x"09", msg => "Dummy Cycle",              clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
  
      sbi_write(addr_value => SPI_CMD      , data_value => x"1b", msg => "SPI RX 4 Bytes",           clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_write(addr_value => SPI_CMD      , data_value => x"CB", msg => "SPI RX 12 Bytes with last",clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"87", msg => "Read Byte @ 0x000086",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"88", msg => "Read Byte @ 0x000087",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"89", msg => "Read Byte @ 0x000088",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"8a", msg => "Read Byte @ 0x000089",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"8b", msg => "Read Byte @ 0x00008a",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"8c", msg => "Read Byte @ 0x00008b",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"8d", msg => "Read Byte @ 0x00008c",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"8e", msg => "Read Byte @ 0x00008d",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"8f", msg => "Read Byte @ 0x00008e",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"90", msg => "Read Byte @ 0x00008f",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"91", msg => "Read Byte @ 0x000090",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"92", msg => "Read Byte @ 0x000091",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"93", msg => "Read Byte @ 0x000092",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"94", msg => "Read Byte @ 0x000093",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"95", msg => "Read Byte @ 0x000094",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
      sbi_check(addr_value => SPI_DATA     , data_exp   => x"96", msg => "Read Byte @ 0x000095",     clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

      end if;

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
