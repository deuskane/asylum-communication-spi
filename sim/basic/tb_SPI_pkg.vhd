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

package tb_SPI_pkg is

  type spi_master_if_t is record
    clk_i                : std_logic;
    arst_b_i             : std_logic;
    tx_tdata_i           : std_logic_vector(8-1 downto 0);
    tx_tvalid_i          : std_logic;
    tx_tready_o          : std_logic;
    rx_tdata_o           : std_logic_vector(8-1 downto 0);
    rx_tvalid_o          : std_logic;
    rx_tready_i          : std_logic;
    last_transfer_i      : std_logic;
    enable_rx_i          : std_logic;
    cfg_cpol_i           : std_logic;
    cfg_cpha_i           : std_logic;
    cfg_prescaler_ratio_i: std_logic_vector;
    sclk_o               : std_logic;
    sclk_oe_o            : std_logic;
    cs_b_o               : std_logic;
    cs_b_oe_o            : std_logic;
    mosi_o               : std_logic;
    mosi_oe_o            : std_logic;
    miso_i               : std_logic;
  end record spi_master_if_t;

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


  

end package body tb_SPI_pkg;
