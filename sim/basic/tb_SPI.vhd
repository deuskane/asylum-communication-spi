-------------------------------------------------------------------------------
-- Title      : tb_SPI
-- Project    : SPI
-------------------------------------------------------------------------------
-- File       : tb_SPI.vhd
-- Author     : mrosiere
-- Company    : 
-- Created    : 2025-05-29
-- Last update: 2025-06-07
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2025
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2025-05-29  1.0      mrosiere Created
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
  signal test_en               : boolean   := false;
  signal test_done             : std_logic := '0';
  signal test_ok               : std_logic := '0';
                              
  constant PRESCALER_WIDTH     : integer := 8;

  
  -- Déclaration des signaux
  signal  dut_if                : spi_master_if_t(cfg_prescaler_ratio_i(PRESCALER_WIDTH-1 downto 0));

  procedure run
    (constant n     : in positive;          -- nb cycle
     constant pol   : in string := "pos"
     ) is
    
  begin
    run(n,pol,dut_if.clk_i);
  end run;

  
begin
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
      ,last_transfer_i      => dut_if.last_transfer_i
      ,enable_rx_i          => dut_if.enable_rx_i
      ,sclk_o               => dut_if.sclk_o     
      ,cs_b_o               => dut_if.cs_b_o     
      ,mosi_o               => dut_if.mosi_o     
      ,miso_i               => dut_if.miso_i
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

    variable x : std_logic_vector(1 downto 0) := "10";
    
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
    -- tx_1byte
    -------------------------------------------------------
    procedure tx_1byte
      (constant byte0   : in std_logic_vector(8-1 downto 0)
       ) is
      
    begin
      report "[TESTBENCH] TX 1 byte";

      dut_if.tx_tvalid_i <= '1';
      dut_if.tx_tdata_i  <= byte0;
      wait until dut_if.tx_tready_o = '0';
      dut_if.tx_tvalid_i <= '0';
      dut_if.tx_tdata_i  <= X"00";
      wait until dut_if.tx_tready_o = '1';
    end tx_1byte;

    procedure tx_2byte
      (constant byte0   : in std_logic_vector(8-1 downto 0);
       constant byte1   : in std_logic_vector(8-1 downto 0)
       ) is
      
    begin
      report "[TESTBENCH] TX 2 bytes in burst";

      dut_if.tx_tvalid_i <= '1';
      dut_if.tx_tdata_i  <= byte0;
      wait until dut_if.tx_tready_o = '0';
      dut_if.tx_tvalid_i <= '1';
      dut_if.tx_tdata_i  <= byte1;
      wait until dut_if.tx_tready_o = '1';
      wait until dut_if.tx_tready_o = '0';
      dut_if.tx_tvalid_i <= '0';
      dut_if.tx_tdata_i  <= X"00";
      wait until dut_if.tx_tready_o = '1';
    end tx_2byte;


  begin  -- process tb_gen
    report "[TESTBENCH] Test Begin";
    dut_if.arst_b_i              <= '1';

    -- Réinitialisation
    run(10);
    dut_if.enable_rx_i           <= '1';
    dut_if.last_transfer_i       <= '0';
    dut_if.rx_tready_i           <= '1';

    for cpol in 0 to 1 loop
      for cpha in 0 to 1 loop
        run(100);
        cfg(x(cpol),x(cpha),X"00");
        run(100);
        tx_1byte(X"55");
        run(100);
        tx_2byte(X"0F",X"F0");
        
      end loop;  -- cpha
    end loop;  -- cpol
    
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
