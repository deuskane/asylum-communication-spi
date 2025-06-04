-------------------------------------------------------------------------------
-- Title      : tb_SPI
-- Project    : SPI
-------------------------------------------------------------------------------
-- File       : tb_SPI.vhd
-- Author     : mrosiere
-- Company    : 
-- Created    : 2025-05-31
-- Last update: 2025-06-04
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
  signal disable_cs_i  : std_logic;
  signal sclk_o        :std_logic;
  signal cs_b_o        :std_logic;
  signal mosi_o        :std_logic;
  signal miso_i        :std_logic;
  signal RSTNeg         : std_logic;
  signal WPNeg         : std_logic;
  signal HOLDNeg       : std_logic;
  
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
  ------------------------------------------------
  -- DUT
  ------------------------------------------------
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
    ,disable_cs_i   => disable_cs_i
    ,sclk_o         => sclk_o     
    ,cs_b_o         => cs_b_o     
    ,mosi_o         => mosi_o     
    ,miso_i         => miso_i
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
      (D             => mosi_o -- serial data input/IO0
      ,Q             => miso_i -- serial data output/IO1
      ,C             => sclk_o -- serial clock input
      ,SNeg          => cs_b_o -- chip select input
      ,WNeg          => WPNeg  -- write protect input/IO2
      ,HOLDNeg       => HOLDNeg-- hold input/IO3
       );
    
  
  ------------------------------------------------
  -- Clock process
  ------------------------------------------------
  clk_i <= not test_done and not clk_i after 10 ns;
  
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
       constant cpha   : in std_logic
       ) is

    begin
      
      
      report "[TESTBENCH] Configure CPOL "& std_logic'image(cpol) & " - CPHA "& std_logic'image(cpol);
      arst_b_i    <= '0';
      cpol_i      <= cpol;
      cpha_i      <= cpha;
      run(1);
      arst_b_i    <= '1';
      run(1);
    end cfg;

    
    -------------------------------------------------------
    -- tx_1byte
    -------------------------------------------------------
    procedure tx_1byte
      (constant byte0   : in std_logic_vector(8-1 downto 0)
       ) is
      
    begin
      report "[TESTBENCH] TX 1 byte";

      tx_tvalid_i <= '1';
      tx_tdata_i  <= byte0;
      wait until tx_tready_o = '0';
      tx_tvalid_i <= '0';
      tx_tdata_i  <= X"00";
      wait until tx_tready_o = '1';
    end tx_1byte;


  begin  -- process tb_gen
    report "[TESTBENCH] Test Begin";

    -- Réinitialisation
    run(10);
    prescaler_i  <= X"0F";
    rx_tready_i  <= '1';

    cfg('0','0');

    wait for 800 us;
    
    -- SFDP Instruction
    disable_cs_i <= '0';
    tx_1byte(X"03");
    -- SFDP Address
    tx_1byte(X"00");
    tx_1byte(X"00");
    tx_1byte(X"00");
    -- SFDP Data
    tx_1byte(X"00");
    tx_1byte(X"00");
    tx_1byte(X"00");
    disable_cs_i <= '1';
    tx_1byte(X"00");
    
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
