-------------------------------------------------------------------------------
-- Title      : pbi_GPIO
-- Project    : PicoSOC
-------------------------------------------------------------------------------
-- File       : pbi_GPIO.vhd
-- Author     : Mathieu Rosiere
-- Company    : 
-- Created    : 2017-03-30
-- Last update: 2025-06-13
-- Platform   : 
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- Copyright (c) 2017
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2025-06-13  0.1      mrosiere Created
-------------------------------------------------------------------------------

library IEEE;
use     IEEE.STD_LOGIC_1164.ALL;
use     IEEE.numeric_std.ALL;
library work;
library work;
use     work.pbi_pkg.all;
use     work.SPI_csr_pkg.all;

entity pbi_SPI is
  generic(
    USER_DEFINE_PRESCALER : boolean; -- Parameters to use the enable the User define Prescaler
    PRESCALER_RATIO       : std_logic_vector(8-1 downto 0) -- Default value for prescaler ratio
    );
  port   (
    clk_i            : in    std_logic;
    arst_b_i         : in    std_logic; -- asynchronous reset

    -- Bus
    pbi_ini_i        : in    pbi_ini_t;
    pbi_tgt_o        : out   pbi_tgt_t;
    
    -- SPI Interface
    sclk_o           : out std_logic;
    sclk_oe_o        : out std_logic;
    cs_b_o           : out std_logic;
    cs_b_oe_o        : out std_logic;
    mosi_o           : out std_logic;
    mosi_oe_o        : out std_logic;
    miso_i           : in  std_logic
    );

end entity pbi_SPI;

architecture rtl of pbi_SPI is

  signal sw2hw                  : SPI_sw2hw_t;
  signal hw2sw                  : SPI_hw2sw_t;

begin  -- architecture rtl

  ins_csr : entity work.SPI_registers(rtl)
  generic map(
    USER_DEFINE_PRESCALER => USER_DEFINE_PRESCALER,
    PRESCALER_RATIO       => PRESCALER_RATIO      
    )
  port map(
    clk_i     => clk_i           ,
    arst_b_i  => arstn_i         ,
    pbi_ini_i => pbi_ini_i       ,
    pbi_tgt_o => pbi_tgt_o       ,
    sw2hw_o   => sw2hw           ,
    hw2sw_i   => hw2sw   
  );

  ins_spi_master : entity work.spi_master(rtl)
    generic map(
      PRESCALER_WIDTH      => 8
      )
    port map
    ( clk_i                 => clk_i
     ,arst_b_i              => sw2hw.cfg.spi_enable
     ,tx_tvalid_i           => sw2hw.data.valid
     ,tx_tready_o           => hw2sw.data.ready
     ,tx_tdata_i            => sw2hw.data.value
     ,rx_tvalid_o           => hw2sw.data.valid
     ,rx_tready_i           => sw2hw.data.ready
     ,rx_tdata_o            => hw2sw.data.value
     ,cmd_tvalid_i          => sw2hw.cmd.valid
     ,cmd_tready_o          => hw2sw.cmd.ready
     ,cmd_last_transfer_i   => sw2hw.cmd.last
     ,cmd_enable_rx_i       => sw2hw.cmd.enable_rx
     ,cmd_enable_tx_i       => sw2hw.cmd.enable_tx
     ,cmd_nb_bytes_i        => sw2hw.cmd.nb_bytes
     ,cfg_cpol_i            => sw2hw.cfg.cpol
     ,cfg_cpha_i            => sw2hw.cfg.cpha
     ,cfg_prescaler_ratio_i => sw2hw.prescaler.ratio
     ,sclk_o                => sclk_o   
     ,sclk_oe_o             => sclk_oe_o
     ,cs_b_o                => cs_b_o   
     ,cs_b_oe_o             => cs_b_oe_o
     ,mosi_o                => mosi_o   
     ,mosi_oe_o             => mosi_oe_o
     ,miso_i                => miso_i   
    );
  
end architecture rtl;
