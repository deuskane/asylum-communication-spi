-------------------------------------------------------------------------------
-- Title      : tb_SPI
-- Project    : SPI
-------------------------------------------------------------------------------
-- File       : tb_SPI.vhd
-- Author     : mrosiere
-- Company    : 
-- Created    : 2025-05-29
-- Last update: 2025-06-18
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

package tb_SPI_pkg is

  type spi_master_ifo_t is record
    tx_tready_o          : std_logic;
    rx_tdata_o           : std_logic_vector(8-1 downto 0);
    rx_tvalid_o          : std_logic;
    cmd_tready_o         : std_logic;
    sclk_o               : std_logic;
    sclk_oe_o            : std_logic;
    cs_b_o               : std_logic;
    cs_b_oe_o            : std_logic;
    mosi_o               : std_logic;
    mosi_oe_o            : std_logic;
  end record spi_master_ifo_t;

  type spi_master_ifi_t is record
    clk_i                : std_logic;
    arst_b_i             : std_logic;
    tx_tdata_i           : std_logic_vector(8-1 downto 0);
    tx_tvalid_i          : std_logic;
    rx_tready_i          : std_logic;
    cmd_tvalid_i         : std_logic;
    cmd_last_transfer_i  : std_logic;
    cmd_enable_tx_i      : std_logic;
    cmd_enable_rx_i      : std_logic;
    cmd_nb_bytes_i       : std_logic_vector;
    cfg_cpol_i           : std_logic;
    cfg_cpha_i           : std_logic;
    cfg_prescaler_ratio_i: std_logic_vector;
    miso_i               : std_logic;
  end record spi_master_ifi_t;

  procedure xrun
    (constant n     : in positive;           -- nb cycle
     constant pol   : in string;
     signal   clk   : in std_logic
     );
  procedure run
    (constant n     : in positive;          -- nb cycle
     constant pol   : in string := "pos";
     signal   clk_i : in std_logic
     ) ;

  procedure cfg
    (signal   clk_i             : in    std_logic;
     signal   dut_ifi           : out   spi_master_ifi_t;
     signal   dut_ifo           : in    spi_master_ifo_t;
     constant cpol              : in    std_logic;
     constant cpha              : in    std_logic;
     constant prescaler_ratio_i : in    std_logic_vector
     );

  procedure cmd
    (signal   clk_i             : in    std_logic;
     signal   dut_ifi           : out   spi_master_ifi_t;
     signal   dut_ifo           : in    spi_master_ifo_t;
     constant cmd_enable_tx_i      : in    std_logic;
     constant cmd_enable_rx_i      : in    std_logic;
     constant cmd_last_transfer_i  : in    std_logic;
     constant cmd_nb_bytes_i       : in    std_logic_vector
     );

  procedure tx
    (signal   clk_i             : in    std_logic;
     signal   dut_ifi           : out   spi_master_ifi_t;
     signal   dut_ifo           : in    spi_master_ifo_t;
     constant byte0  : in std_logic_vector(8-1 downto 0)
     );

  procedure rx
    (signal   clk_i             : in    std_logic;
     signal   dut_ifi           : out   spi_master_ifi_t;
     signal   dut_ifo           : in    spi_master_ifo_t;
     constant byte0  : in std_logic_vector(8-1 downto 0)
     );
  
end package tb_SPI_pkg;


package body tb_SPI_pkg is

  -------------------------------------------------------
  -- xrun
  -------------------------------------------------------
  procedure xrun
    (constant n     : in positive;           -- nb cycle
     constant pol   : in string;
     signal   clk   : in std_logic
     ) is
    
  begin
    for i in 0 to n-1
    loop
      if (pol="pos")
      then
        wait until rising_edge(clk);
      else
        wait until falling_edge(clk);
      end if;
      
    end loop;  -- i
  end xrun;

  -------------------------------------------------------
  -- run
  -------------------------------------------------------
  procedure run
    (constant n     : in positive;          -- nb cycle
     constant pol   : in string := "pos";
     signal   clk_i : in std_logic
     ) is
    
  begin
    xrun(n,"pos",clk_i);
  end run;

  -------------------------------------------------------
  -- cfg
  -------------------------------------------------------
  procedure cfg
    (signal   clk_i             : in    std_logic;
     signal   dut_ifi           : out   spi_master_ifi_t;
     signal   dut_ifo           : in    spi_master_ifo_t;
     constant cpol              : in    std_logic;
     constant cpha              : in    std_logic;
     constant prescaler_ratio_i : in    std_logic_vector
     ) is

  begin
    
    report "[TESTBENCH] Configure CPOL "& std_logic'image(cpol) & " - CPHA "& std_logic'image(cpol) & " - Prescaler Ratio 0x" & to_hstring(prescaler_ratio_i);
    dut_ifi.arst_b_i              <= '0';
    dut_ifi.cfg_cpol_i            <= cpol;
    dut_ifi.cfg_cpha_i            <= cpha;
    dut_ifi.cfg_prescaler_ratio_i <= prescaler_ratio_i;
    run(1,"pos",clk_i);
    dut_ifi.arst_b_i              <= '1';
    run(1,"pos",clk_i);
  end cfg;

  -------------------------------------------------------
  -- cmd
  -------------------------------------------------------
  procedure cmd
    (signal   clk_i             : in    std_logic;
     signal   dut_ifi           : out   spi_master_ifi_t;
     signal   dut_ifo           : in    spi_master_ifo_t;
     constant cmd_enable_tx_i      : in    std_logic;
     constant cmd_enable_rx_i      : in    std_logic;
     constant cmd_last_transfer_i  : in    std_logic;
     constant cmd_nb_bytes_i       : in    std_logic_vector
     ) is
    
  begin
    report "[TESTBENCH] Command";

    report "[TESTBENCH] Command TX "& std_logic'image(cmd_enable_tx_i) & " - RX "& std_logic'image(cmd_enable_rx_i) & " - Last " & std_logic'image(cmd_last_transfer_i) & " - #bytes " & to_hstring(cmd_nb_bytes_i);

    
    dut_ifi.cmd_tvalid_i         <= '1';
    dut_ifi.cmd_last_transfer_i  <= cmd_last_transfer_i;
    dut_ifi.cmd_enable_rx_i      <= cmd_enable_rx_i    ;
    dut_ifi.cmd_enable_tx_i      <= cmd_enable_tx_i    ;
    dut_ifi.cmd_nb_bytes_i       <= cmd_nb_bytes_i     ;

    run(1,"pos",clk_i);
    
    -- Wait FIFO empty
    while dut_ifo.cmd_tready_o = '0'
    loop
      run(1,"pos",clk_i);
    end loop;
      
    dut_ifi.cmd_tvalid_i         <= '0';
    dut_ifi.cmd_last_transfer_i  <= '0';
    dut_ifi.cmd_enable_rx_i      <= '0';
    dut_ifi.cmd_enable_tx_i      <= '0';
    --dut_ifi.cmd_nb_bytes_i       <= (others => '0')     ;
  end cmd;
  

  -------------------------------------------------------
  -- tx
  -------------------------------------------------------
  procedure tx
    (signal   clk_i             : in    std_logic;
     signal   dut_ifi           : out   spi_master_ifi_t;
     signal   dut_ifo           : in    spi_master_ifo_t;
     constant byte0  : in std_logic_vector(8-1 downto 0)
     ) is
    
  begin

    report "[TESTBENCH] TX "&to_hstring(byte0);

    dut_ifi.tx_tvalid_i <= '1';
    dut_ifi.tx_tdata_i  <= byte0;

    run(1,"pos",clk_i);

    -- Wait FIFO empty
    while dut_ifo.tx_tready_o = '0'
    loop
      run(1,"pos",clk_i);
    end loop;

    dut_ifi.tx_tvalid_i <= '0';
    dut_ifi.tx_tdata_i  <= X"00";
    
  end tx;

  -------------------------------------------------------
  -- tx
  -------------------------------------------------------
  procedure rx
    (signal   clk_i             : in    std_logic;
     signal   dut_ifi           : out   spi_master_ifi_t;
     signal   dut_ifo           : in    spi_master_ifo_t;
     constant byte0  : in std_logic_vector(8-1 downto 0)
     ) is
    
  begin
    
      report "[TESTBENCH] RX 1 byte";


      dut_ifi.rx_tready_i <= '1';

      -- Wait FIFO empty
      while dut_ifo.rx_tvalid_o = '0'
      loop
        run(1,"pos",clk_i);
      end loop;

      report "[TESTBENCH] RX 1 byte ... "&to_hstring(dut_ifo.rx_tdata_o);
      
      assert dut_ifo.rx_tdata_o = byte0 report "[TESTBENCH] Invalid Rx data " & to_hstring(dut_ifo.rx_tdata_o) & " != " & to_hstring(byte0) severity error;

      run(1,"pos",clk_i);

  end rx;

end package body tb_SPI_pkg;
