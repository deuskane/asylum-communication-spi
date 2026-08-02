-------------------------------------------------------------------------------
-- Title      : tb_SPI
-- Project    : SPI
-------------------------------------------------------------------------------
-- File       : tb_SPI.vhd
-- Author     : mrosiere
-- Company    :
-- Created    : 2025-05-31
-- Last update: 2026-08-01
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
  RSTNeg   <= '1';
  WPNeg    <= '1';
  HOLDNeg  <= '1';

  clock_generator(clk_i, clk_ena, 20 ns, "TB Clock");

  dut : sbi_SPI
    generic map (
      NAME                  => "SPI"
     ,USER_DEFINE_PRESCALER => false
     ,PRESCALER_RATIO       => x"02"
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

  sbi_ini.cs                               <= sbi_if.cs;
  sbi_ini.addr                             <= std_logic_vector(sbi_if.addr(SPI_ADDR_WIDTH-1 downto 0));
  sbi_ini.re                               <= sbi_if.rena;
  sbi_ini.we                               <= sbi_if.wena;
  sbi_ini.wdata                            <= sbi_if.wdata(SPI_DATA_WIDTH-1 downto 0);
  sbi_if.ready                             <= sbi_tgt.ready;
  sbi_if.rdata(SPI_DATA_WIDTH-1 downto 0) <= sbi_tgt.rdata;

  process
    
    variable V_SBI_BFM_CONFIG_SPI : t_sbi_bfm_config := C_SBI_BFM_CONFIG_DEFAULT;
    
  begin

    -- BFM Configuration
    V_SBI_BFM_CONFIG_SPI.max_wait_cycles            := 1000;

    sbi_if <= init_sbi_if_signals(SPI_ADDR_WIDTH, SPI_DATA_WIDTH);
    wait until arst_b_i = '1';
    wait until rising_edge(clk_i);

    log(ID_SEQUENCER, "Reset released, starting SPI SBI test", C_SCOPE);

    log(ID_LOG_HDR, "Configuration of the Prescaler (Divide by 32)", C_SCOPE);
    sbi_write(addr_value => SPI_PRESCALER, data_value => x"0F", msg => "Set prescaler", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    --sbi_check(addr_value => SPI_PRESCALER, data_exp   => x"0F", msg => "Read back prescaler", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

    log(ID_LOG_HDR, "Configure and verify SPI control registers", C_SCOPE);
    sbi_write(addr_value => SPI_CFG      , data_value => x"01", msg => "Enable SPI", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_CFG      , data_exp   => x"01", msg => "Read back enabled config", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

    wait for 800 us;


    sbi_write(addr_value => SPI_CMD      , data_value => x"83", msg => "Write command register", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_write(addr_value => SPI_DATA     , data_value => x"03", msg => "SPI Instruction 0x03", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_write(addr_value => SPI_DATA     , data_value => x"00", msg => "SPI Address 0x000005", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_write(addr_value => SPI_DATA     , data_value => x"00", msg => "SPI Address 0x000005", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_write(addr_value => SPI_DATA     , data_value => x"05", msg => "SPI Address 0x000005", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

    sbi_write(addr_value => SPI_CMD      , data_value => x"63", msg => "Write command register", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"06", msg => "Read Byte @ 0x000005", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"07", msg => "Read Byte @ 0x000006", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"08", msg => "Read Byte @ 0x000007", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);
    sbi_check(addr_value => SPI_DATA     , data_exp   => x"09", msg => "Read Byte @ 0x000008", clk => clk_i, sbi_if => sbi_if, config => V_SBI_BFM_CONFIG_SPI);

--       cfg(clk_i,dut_ifi,dut_ifo,'0','0',X"0F");
-- 
--       wait for 800 us;
--       
--       -- Read Instruction
--       cmd(clk_i,dut_ifi,dut_ifo,'1','0','0',X"3");
--       tx(clk_i,dut_ifi,dut_ifo,X"03");
--       -- Read Address
--       tx(clk_i,dut_ifi,dut_ifo,X"00");
--       tx(clk_i,dut_ifi,dut_ifo,X"00");
--       tx(clk_i,dut_ifi,dut_ifo,X"05");
--       -- Read Data
--       cmd(clk_i,dut_ifi,dut_ifo,'0','1','1',X"3");
-- --    tx(clk_i,dut_ifi,dut_ifo,X"00");
-- --    tx(clk_i,dut_ifi,dut_ifo,X"00");
-- --    tx(clk_i,dut_ifi,dut_ifo,X"00");
-- --    tx(clk_i,dut_ifi,dut_ifo,X"00");
-- 
--     rx(clk_i,dut_ifi,dut_ifo,X"06");
--     rx(clk_i,dut_ifi,dut_ifo,X"07");
--     rx(clk_i,dut_ifi,dut_ifo,X"08");
--     rx(clk_i,dut_ifi,dut_ifo,X"09");
-- 
--     
    report "[TB_SPI] All SBI SPI checks passed";
    report_alert_counters(FINAL);
    std.env.stop;
    wait;
  end process;

end sim;
