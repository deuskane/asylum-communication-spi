-------------------------------------------------------------------------------
-- Title      : tb_SPI
-- Project    : SPI
-------------------------------------------------------------------------------
-- File       : tb_SPI.vhd
-- Author     : mrosiere
-- Company    : 
-- Created    : 2025-05-29
-- Last update: 2026-01-17
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
library asylum;
use     asylum.spi_pkg.all;
library work;
use     work.tb_SPI_pkg.all;

entity tb is
end tb;
 
architecture sim of tb is
  -----------------------------------------------------
  -- Test signals
  -----------------------------------------------------
  signal   test_en             : boolean   := false;
  signal   test_done           : std_logic := '0';
  signal   test_ok             : std_logic := '0';
                              
  constant PRESCALER_WIDTH     : integer := 8;
  
  -- Déclaration des signaux
  signal  clk_i                  : std_logic;
  signal  dut_ifi                : spi_master_ifi_t(cfg_prescaler_ratio_i(PRESCALER_WIDTH-1 downto 0),
                                                   cmd_nb_bytes_i       (3 downto 0));
  signal  dut_ifo                : spi_master_ifo_t;

  procedure run
    (constant n     : in positive;          -- nb cycle
     constant pol   : in string := "pos"
     ) is
    
  begin
    run(n,pol,clk_i);
  end run;
  
begin
    -- Instanciation du contrôleur SPI_master
    dut: spi_master
      port map
      (clk_i                => clk_i
      ,arst_b_i             => dut_ifi.arst_b_i
      ,tx_tdata_i           => dut_ifi.tx_tdata_i 
      ,tx_tvalid_i          => dut_ifi.tx_tvalid_i
      ,tx_tready_o          => dut_ifo.tx_tready_o
      ,rx_tdata_o           => dut_ifo.rx_tdata_o 
      ,rx_tvalid_o          => dut_ifo.rx_tvalid_o
      ,rx_tready_i          => dut_ifi.rx_tready_i
      ,cfg_cpol_i           => dut_ifi.cfg_cpol_i     
      ,cfg_cpha_i           => dut_ifi.cfg_cpha_i     
      ,cfg_prescaler_ratio_i=> dut_ifi.cfg_prescaler_ratio_i
      ,cfg_loopback_i       => dut_ifi.cfg_loopback_i
      ,cmd_tvalid_i         => dut_ifi.cmd_tvalid_i
      ,cmd_tready_o         => dut_ifo.cmd_tready_o
      ,cmd_tlast_i          => dut_ifi.cmd_tlast_i
      ,cmd_enable_tx_i      => dut_ifi.cmd_enable_tx_i
      ,cmd_enable_rx_i      => dut_ifi.cmd_enable_rx_i
      ,cmd_nb_bytes_i       => dut_ifi.cmd_nb_bytes_i
      ,sclk_o               => dut_ifo.sclk_o     
      ,cs_b_o               => dut_ifo.cs_b_o     
      ,mosi_o               => dut_ifo.mosi_o     
      ,miso_i               => dut_ifo.mosi_o
      );

  ------------------------------------------------
  -- Clock process
  ------------------------------------------------
  gen_clk: process is
    begin  -- process gen_clk
      clk_i <= '0';

      while test_done = '0'
      loop
        wait for 5 ns;

        clk_i <= not clk_i;
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
   

  begin  -- process tb_gen
             
    report "[TESTBENCH] Test Begin";
    dut_ifi.arst_b_i              <= '1';

    -- Réinitialisation
    run(10);
    dut_ifi.rx_tready_i           <= '1';
    dut_ifi.tx_tvalid_i           <= '0';
    dut_ifi.cmd_tvalid_i          <= '0';

    for cpol in 0 to 1 loop
      for cpha in 0 to 1 loop
        run(100);
        cfg(clk_i,dut_ifi,dut_ifo,x(cpol),x(cpha),X"00");
        run(100);
        cmd(clk_i,dut_ifi,dut_ifo,'1','1','1',X"3");
        run(100);
        tx(clk_i,dut_ifi,dut_ifo,X"55");
        run(100);
        tx(clk_i,dut_ifi,dut_ifo,X"0F");
        tx(clk_i,dut_ifi,dut_ifo,X"F0");
        
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
