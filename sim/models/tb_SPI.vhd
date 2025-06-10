-------------------------------------------------------------------------------
-- Title      : tb_SPI
-- Project    : SPI
-------------------------------------------------------------------------------
-- File       : tb_SPI.vhd
-- Author     : mrosiere
-- Company    : 
-- Created    : 2025-05-31
-- Last update: 2025-06-10
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2025
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2025-05-31  1.0      mrosiere Created
-------------------------------------------------------------------------------

library IEEE;
use     IEEE.STD_LOGIC_1164.ALL;
use     IEEE.numeric_std.ALL;
use     work.tb_SPI_pkg.all;
 
entity tb is
end tb;
 
architecture sim of tb is
  -----------------------------------------------------
  -- Test signals
  -----------------------------------------------------
  signal test_en                 : boolean   := false;
  signal test_done               : std_logic := '0';
  signal test_ok                 : std_logic := '0';
                                 
  constant PRESCALER_WIDTH       : integer := 8;
                                 
                                 
  -- Déclaration des signaux     
  signal  dut_if                : spi_master_if_t(cfg_prescaler_ratio_i(PRESCALER_WIDTH-1 downto 0),
                                                  cmd_nb_bytes_i       (3 downto 0));
  signal  RSTNeg                : std_logic;
  signal  WPNeg                 : std_logic;
  signal  HOLDNeg               : std_logic;

  procedure run
    (constant n     : in positive;          -- nb cycle
     constant pol   : in string := "pos"
     ) is
    
  begin
    run(n,pol,dut_if.clk_i);
  end run;
  
begin
  ------------------------------------------------
  -- DUT
  ------------------------------------------------
  -- Instanciation du contrôleur SPI_master
  dut: entity work.spi_master(rtl)
    port map
    (clk_i                => dut_if.clk_i
    ,arst_b_i             => dut_if.arst_b_i
    ,tx_tdata_i           => dut_if.tx_tdata_i 
    ,tx_tvalid_i          => dut_if.tx_tvalid_i
    ,tx_tready_o          => dut_if.tx_tready_o
    ,rx_tdata_o           => dut_if.rx_tdata_o 
    ,rx_tvalid_o          => dut_if.rx_tvalid_o
    ,rx_tready_i          => dut_if.rx_tready_i
    ,cfg_cpol_i           => dut_if.cfg_cpol_i     
    ,cfg_cpha_i           => dut_if.cfg_cpha_i     
    ,cfg_prescaler_ratio_i=> dut_if.cfg_prescaler_ratio_i
    ,cmd_tvalid_i         => dut_if.cmd_tvalid_i
    ,cmd_tready_o         => dut_if.cmd_tready_o
    ,cmd_last_transfer_i  => dut_if.cmd_last_transfer_i
    ,cmd_enable_tx_i      => dut_if.cmd_enable_tx_i
    ,cmd_enable_rx_i      => dut_if.cmd_enable_rx_i
    ,cmd_nb_bytes_i       => dut_if.cmd_nb_bytes_i
    ,sclk_o               => dut_if.sclk_o     
    ,cs_b_o               => dut_if.cs_b_o     
    ,mosi_o               => dut_if.mosi_o     
    ,miso_i               => dut_if.miso_i
    );

  ------------------------------------------------
  -- Memory Model
  ------------------------------------------------
  RSTNeg  <= '1';
  WPNeg   <= '1';
  HOLDNeg <= '1';

--  mem : entity work.s25fl512s(vhdl_behavioral_static_memory_allocation)
--    generic map
--    (TimingModel       => "S25FL512SAGMFI010_F_30pF"
--    ,mem_file_name     => "memory.mem"
--    ,otp_file_name     => "memoryOTP.mem"
--    ,UserPreload       => True
--    ,TimingChecksOn    => True
--    ,MsgOn             => True
--    ,XOn               => True
--     )
--    port map
--    (SI            => mosi_o -- serial data input/IO0
--    ,SO            => miso_i -- serial data output/IO1
--    ,SCK           => sclk_o -- serial clock input
--    ,CSNeg         => cs_b_o -- chip select input
--    ,RSTNeg        => RSTNeg -- hardware reset pin
--    ,WPNeg         => WPNeg  -- write protect input/IO2
--    ,HOLDNeg       => HOLDNeg-- hold input/IO3
--     );
--


  mem : entity work.m25p40(vhdl_behavioral)
      generic map
      (mem_file_name     => "memory.mem"
      ,UserPreload       => True
      ,DebugInfo         => True
      ,TimingChecksOn    => True
      ,MsgOn             => True
      ,XOn               => True
       )
      port map
      (D             => dut_if.mosi_o -- serial data input/IO0
      ,Q             => dut_if.miso_i -- serial data output/IO1
      ,C             => dut_if.sclk_o -- serial clock input
      ,SNeg          => dut_if.cs_b_o -- chip select input
      ,WNeg          => WPNeg  -- write protect input/IO2
      ,HOLDNeg       => HOLDNeg-- hold input/IO3
       );
    
  
  ------------------------------------------------
  -- Clock process
  ------------------------------------------------
  gen_clk: process is
    begin  -- process gen_clk
      dut_if.clk_i <= '0';

      while test_done = '0'
      loop
        wait for 5 ns;

        dut_if.clk_i <= not dut_if.clk_i;
      end loop;

      wait; -- end process
    end process gen_clk;  
  
  ------------------------------------------------
  -- Test process
  ------------------------------------------------
  -- purpose: Testbench process
  -- type   : combinational
  -- inputs : 
    -- outputs: All dut design with clk
    
  tb_gen: process is

    -------------------------------------------------------
    -- cfg
    -------------------------------------------------------
    procedure cfg
      (constant cpol   : in std_logic;
       constant cpha   : in std_logic;
       constant prescaler_ratio_i : in std_logic_vector
       ) is

    begin
      
      report "[TESTBENCH] Configure CPOL "& std_logic'image(cpol) & " - CPHA "& std_logic'image(cpol) & " - Prescaler Ratio 0x" & to_hstring(prescaler_ratio_i);
      dut_if.arst_b_i    <= '0';
      dut_if.cfg_cpol_i  <= cpol;
      dut_if.cfg_cpha_i  <= cpha;
      dut_if.cfg_prescaler_ratio_i <= prescaler_ratio_i;
      run(1,"pos",dut_if.clk_i);
      dut_if.arst_b_i    <= '1';
      run(1,"pos",dut_if.clk_i);
    end cfg;

    -------------------------------------------------------
    -- cmd
    -------------------------------------------------------
    procedure cmd
      (constant cmd_enable_tx_i      : in  std_logic;
       constant cmd_enable_rx_i      : in  std_logic;
       constant cmd_last_transfer_i  : in  std_logic;
       constant cmd_nb_bytes_i       : in  std_logic_vector
       ) is
      
    begin
      report "[TESTBENCH] Command";

      while dut_if.cmd_tready_o = '0'
      loop
        run(1);
      end loop;
      
      dut_if.cmd_tvalid_i <= '1';

      report "[TESTBENCH] Command TX "& std_logic'image(cmd_enable_tx_i) & " - RX "& std_logic'image(cmd_enable_rx_i) & " - Last " & std_logic'image(cmd_last_transfer_i) & " nb bytes " & to_hstring(cmd_nb_bytes_i);

      
      dut_if.cmd_last_transfer_i  <= cmd_last_transfer_i;
      dut_if.cmd_enable_rx_i      <= cmd_enable_rx_i    ;
      dut_if.cmd_enable_tx_i      <= cmd_enable_tx_i    ;
      dut_if.cmd_nb_bytes_i       <= cmd_nb_bytes_i     ;
      wait until dut_if.cmd_tready_o = '0';
      dut_if.cmd_tvalid_i <= '0';
      dut_if.cmd_last_transfer_i  <= '0';
      dut_if.cmd_enable_rx_i      <= '0';
      dut_if.cmd_enable_tx_i      <= '0';
      dut_if.cmd_nb_bytes_i       <= (others => '0')     ;
    end cmd;

    -------------------------------------------------------
    -- tx_1byte
    -------------------------------------------------------
    procedure tx_1byte
      (constant byte0   : in std_logic_vector(8-1 downto 0)
       ) is
      
    begin
      report "[TESTBENCH] TX 1 byte " & to_hstring(byte0);

      dut_if.tx_tvalid_i <= '1';
      dut_if.tx_tdata_i  <= byte0;
      wait until dut_if.tx_tready_o = '0';
      dut_if.tx_tvalid_i <= '0';
      dut_if.tx_tdata_i  <= X"00";
    end tx_1byte;

    
  begin  -- process tb_gen
    report "[TESTBENCH] Test begin";

    -- Réinitialisation
    run(10);
    dut_if.tx_tvalid_i            <= '0';

    cfg('0','0',X"0F");

    wait for 800 us;
    
    -- Read Instruction
    cmd('1','0','0',X"3");
    tx_1byte(X"03");
    -- Read Address
    tx_1byte(X"00");
    tx_1byte(X"00");
    tx_1byte(X"05");
    -- Read Data
    cmd('0','1','1',X"3");
--    tx_1byte(X"00");
--    tx_1byte(X"00");
--    tx_1byte(X"00");
--    tx_1byte(X"00");
    
    wait;
  end process;

  p_rx: process is
    -------------------------------------------------------
    -- rx_1byte
    -------------------------------------------------------
    procedure rx_1byte
      (constant byte0   : in std_logic_vector(8-1 downto 0)
       ) is
      
    begin
      report "[TESTBENCH] RX 1 byte";

      dut_if.rx_tready_i <= '1';
      while dut_if.rx_tvalid_o = '0'
      loop
        run(1);
      end loop;
      assert dut_if.rx_tdata_o = byte0 report "[TESTBENCH] Invalid Rx data " & to_hstring(dut_if.rx_tdata_o) & " != " & to_hstring(byte0) severity note;

      run(1);
    end rx_1byte;


  begin  -- process tb_gen
    report "[TESTBENCH] RX Test Begin";

    dut_if.rx_tready_i  <= '0';
    run(100);
 
    rx_1byte(X"06");
    rx_1byte(X"07");
    rx_1byte(X"08");
    rx_1byte(X"09");
    
    report "[TESTBENCH] RX Test End";
    test_ok   <= '1';
    run(100);
    test_done <= '1';
    wait;
  end process;

  
    gen_test_done: process (test_done) is
    begin  -- process gen_test_done
      if test_done'event and test_done = '1' then  -- rising clock edge
        if test_ok = '1' then
          report "[TESTBENCH] Test OK";
        else
          report "[TESTBENCH] Test KO" severity failure;
        end if;
        
      end if;
    end process gen_test_done;

end sim;
