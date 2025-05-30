-------------------------------------------------------------------------------
-- Title      : tb_GPIO_bidir
-- Project    : GPIO
-------------------------------------------------------------------------------
-- File       : tb_GPIO_bidir.vhd
-- Author     : mrosiere
-- Company    : 
-- Created    : 2017-03-25
-- Last update: 2025-05-30
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2022-07-11  1.1      mrosiere DATA_OE_<INIT/FORCE> in NB_IO
-- 2017-03-25  1.0      mrosiere Created
-------------------------------------------------------------------------------

library IEEE;
use     IEEE.STD_LOGIC_1164.ALL;
use     IEEE.numeric_std.ALL;
 
entity tb is
end tb;
 
architecture sim of tb is
  -----------------------------------------------------
  -- Test signals
  -----------------------------------------------------
  signal test_en   : boolean   := false;
  signal test_done : std_logic := '0';
  signal test_ok   : std_logic := '0';

  constant PRESCALER_WIDTH : integer := 8;

  
  -- Déclaration des signaux
  signal clk_i         : std_logic := '0';
  signal arst_b_i      : std_logic := '1';

  signal tx_tdata_i    :std_logic_vector(7 downto 0);
  signal tx_tvalid_i   :std_logic;
  signal tx_tready_o   :std_logic;
  signal rx_tdata_o    :std_logic_vector(7 downto 0);
  signal rx_tvalid_o   :std_logic;
  signal rx_tready_i   :std_logic;
  signal cpol_i        :std_logic;
  signal cpha_i        :std_logic;
  signal prescaler_i   :std_logic_vector(PRESCALER_WIDTH-1 downto 0);
  signal sclk_o        :std_logic;
  signal cs_b_o        :std_logic;
  signal mosi_o        :std_logic;
  signal miso_i        :std_logic;


  -------------------------------------------------------
  -- xrun
  -------------------------------------------------------
  procedure xrun
    (constant n     : in positive;           -- nb cycle
     constant pol   : in string;
     signal   clk_i : in std_logic
     ) is
    
  begin
    for i in 0 to n-1
    loop
      if (pol="pos")
      then
        wait until rising_edge(clk_i);
      else
        wait until falling_edge(clk_i);
      end if;
      
    end loop;  -- i
  end xrun;

  -------------------------------------------------------
  -- run
  -------------------------------------------------------
  procedure run
    (constant n     : in positive;          -- nb cycle
     constant pol   : in string := "pos"
     ) is
    
  begin
    xrun(n,"pos",clk_i);
  end run;

  
begin
    -- Instanciation du contrôleur SPI_master
    dut: entity work.spi_master(rtl)
      port map
      (clk_i          => clk_i
      ,arst_b_i       => arst_b_i
      ,tx_tdata_i     => tx_tdata_i 
      ,tx_tvalid_i    => tx_tvalid_i
      ,tx_tready_o    => tx_tready_o
      ,rx_tdata_o     => rx_tdata_o 
      ,rx_tvalid_o    => rx_tvalid_o
      ,rx_tready_i    => rx_tready_i
      ,cpol_i         => cpol_i     
      ,cpha_i         => cpha_i     
      ,prescaler_i    => prescaler_i
      ,sclk_o         => sclk_o     
      ,cs_b_o         => cs_b_o     
      ,mosi_o         => mosi_o     
      ,miso_i         => miso_i     
       );

  ------------------------------------------------
  -- Clock process
  ------------------------------------------------
  clk_i <= not test_done and not clk_i after 5 ns;
  
  ------------------------------------------------
  -- Test process
  ------------------------------------------------
  -- purpose: Testbench process
  -- type   : combinational
  -- inputs : 
    -- outputs: All dut design with clk
    
  tb_gen: process is
  begin  -- process tb_gen
    report "[TESTBENCH] Test Begin";

    -- Réinitialisation
    run(10);
    arst_b_i    <= '0';

    prescaler_i <= X"04";
    cpol_i      <= '0';
    cpha_i      <= '0';
    rx_tready_i <= '1';
    miso_i      <= '0';
    run(10);
    arst_b_i <= '1';

    
    report "[TESTBENCH] (CPOL 0 - CPHA 0) Transmission d'un seul byte";
    run(100);
    tx_tvalid_i <= '1';
    tx_tdata_i  <= X"55";
    wait until tx_tready_o = '0';
    tx_tvalid_i <= '0';
    tx_tdata_i  <= X"00";
    wait until tx_tready_o = '1';


    report "[TESTBENCH] (CPOL 0 - CPHA 0) Transmission de 2 bytes";
    run(100);
    tx_tvalid_i <= '1';
    tx_tdata_i  <= X"F0";
    wait until tx_tready_o = '0';
    tx_tvalid_i <= '1';
    tx_tdata_i  <= X"0F";
    wait until tx_tready_o = '1';
    wait until tx_tready_o = '0';
    tx_tvalid_i <= '0';
    tx_tdata_i  <= X"00";
    wait until tx_tready_o = '1';

    report "[TESTBENCH] (CPOL 0 - CPHA 1) Transmission d'1 byte ";
    run(100);
    arst_b_i    <= '0';
    cpol_i      <= '0';
    cpha_i      <= '1';
    run(1);
    arst_b_i    <= '1';
    run(1);

    tx_tvalid_i <= '1';
    tx_tdata_i  <= X"3C";
    wait until tx_tready_o = '0';
    tx_tvalid_i <= '0';
    tx_tdata_i  <= X"00";
    wait until tx_tready_o = '1';

    report "[TESTBENCH] (CPOL 0 - CPHA 1) Transmission de 2 bytes";
    run(100);
    tx_tvalid_i <= '1';
    tx_tdata_i  <= X"F0";
    wait until tx_tready_o = '0';
    tx_tvalid_i <= '1';
    tx_tdata_i  <= X"0F";
    wait until tx_tready_o = '1';
    wait until tx_tready_o = '0';
    tx_tvalid_i <= '0';
    tx_tdata_i  <= X"00";
    wait until tx_tready_o = '1';

    report "[TESTBENCH] (CPOL 1 - CPHA 0) Transmission d'1 byte ";
    run(100);
    arst_b_i    <= '0';
    cpol_i      <= '1';
    cpha_i      <= '0';
    run(1);
    arst_b_i    <= '1';
    run(1);

    tx_tvalid_i <= '1';
    tx_tdata_i  <= X"3C";
    wait until tx_tready_o = '0';
    tx_tvalid_i <= '0';
    tx_tdata_i  <= X"00";
    wait until tx_tready_o = '1';

    report "[TESTBENCH] (CPOL 1 - CPHA 0) Transmission de 2 bytes";
    run(100);
    tx_tvalid_i <= '1';
    tx_tdata_i  <= X"F0";
    wait until tx_tready_o = '0';
    tx_tvalid_i <= '1';
    tx_tdata_i  <= X"0F";
    wait until tx_tready_o = '1';
    wait until tx_tready_o = '0';
    tx_tvalid_i <= '0';
    tx_tdata_i  <= X"00";
    wait until tx_tready_o = '1';

    report "[TESTBENCH] (CPOL 1 - CPHA 1) Transmission d'1 byte ";
    run(100);
    arst_b_i    <= '0';
    cpol_i      <= '1';
    cpha_i      <= '1';
    run(1);
    arst_b_i    <= '1';
    run(1);

    tx_tvalid_i <= '1';
    tx_tdata_i  <= X"3C";
    wait until tx_tready_o = '0';
    tx_tvalid_i <= '0';
    tx_tdata_i  <= X"00";
    wait until tx_tready_o = '1';

    report "[TESTBENCH] (CPOL 1 - CPHA 1) Transmission de 2 bytes";
    run(100);
    tx_tvalid_i <= '1';
    tx_tdata_i  <= X"F0";
    wait until tx_tready_o = '0';
    tx_tvalid_i <= '1';
    tx_tdata_i  <= X"0F";
    wait until tx_tready_o = '1';
    wait until tx_tready_o = '0';
    tx_tvalid_i <= '0';
    tx_tdata_i  <= X"00";
    wait until tx_tready_o = '1';

    
--    -- **Test 3 : SPI avec CPOL=1, CPHA=1**
--    run(10);
--    cpol <= '1';
--    cpha <= '1';
--    tx_tvalid <= '1';
--    tx_tdata <= "11110000";
--    wait until tx_tready = '0';
--    tx_tvalid <= '0';
--    tx_tdata <= "00000000";
--    wait until tx_tready = '1';
--    assert rx_tvalid = '1' and rx_tdata = "11110000"
--      report "Test 3 réussi - Configuration CPOL=1, CPHA=1 validée."
--      severity note;
--    
--    -- **Test 4 : Désactivation de SS après transmission**
--    run(10);
--    disable_ss <= '1';
--    tx_tvalid <= '1';
--    tx_tdata <= "00001111";
--    wait until tx_tready = '0';
--    tx_tvalid <= '0';
--    tx_tdata <= "00000000";
--    wait until tx_tready = '1';
--    assert ss = '1'
--      report "Test 4 réussi - SS désactivé après transaction."
--      severity note;
--    
--    -- **Test 5 : Désactivation de SCLK après transmission**
--    run(10);
--    disable_sclk <= '1';
--    tx_tvalid <= '1';
--    tx_tdata <= "00000000";
--    wait until tx_tready = '0';
--    wait until tx_tready = '1';
--    assert sclk = cpol
--      report "Test 5 réussi - SCLK désactivé après transaction."
--      severity note;
--    
--    -- **Test 6 : Vérification d'un cas d'erreur (pas de donnée TX)**
--    run(10);
--    tx_tvalid <= '0';
--    run(100);
--    assert rx_tvalid = '0'
--      report "Test 6 réussi - Pas de donnée reçue sans TX valid."
--      severity note;
--    
    -- **Fin du test**
    report "Tous les tests SPI terminés !";

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
