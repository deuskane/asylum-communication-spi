-------------------------------------------------------------------------------
-- Title      : sbi_GPIO
-- Project    : PicoSOC
-------------------------------------------------------------------------------
-- File       : sbi_GPIO.vhd
-- Author     : Mathieu Rosiere
-- Company    : 
-- Created    : 2017-03-30
-- Last update: 2025-11-22
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
-- 2025-11-22  1.0      mrosiere Use sbi instead pbi
-------------------------------------------------------------------------------

library IEEE;
use     IEEE.STD_LOGIC_1164.ALL;
use     IEEE.numeric_std.ALL;
use     ieee.std_logic_textio.all;
use     std.textio.all;

library asylum;
use     asylum.sbi_pkg.all;
use     asylum.spi_pkg.all;
use     asylum.SPI_csr_pkg.all;

entity sbi_SPI is
  generic(
    USER_DEFINE_PRESCALER : boolean;                        -- Parameters to use the enable the User define Prescaler
    PRESCALER_RATIO       : std_logic_vector(8-1 downto 0); -- Default value for prescaler ratio
    DEPTH_CMD             : natural := 0;
    DEPTH_TX              : natural := 0;
    DEPTH_RX              : natural := 0;

    FILENAME_CMD          : string  := "dump_spi_cmd.txt";
    FILENAME_TX           : string  := "dump_spi_tx.txt";
    FILENAME_RX           : string  := "dump_spi_rx.txt"

    );
  port   (
    clk_i            : in  std_logic;
    arst_b_i         : in  std_logic; -- asynchronous reset

    -- Bus
    sbi_ini_i        : in  sbi_ini_t;
    sbi_tgt_o        : out sbi_tgt_t;
    
    -- SPI Interface
    sclk_o           : out std_logic;
    sclk_oe_o        : out std_logic;
    cs_b_o           : out std_logic;
    cs_b_oe_o        : out std_logic;
    mosi_o           : out std_logic;
    mosi_oe_o        : out std_logic;
    miso_i           : in  std_logic
    );

end entity sbi_SPI;

architecture rtl of sbi_SPI is

-- synthesis translate_off
  file     file_cmd               : text open write_mode is FILENAME_CMD;
  file     file_tx                : text open write_mode is FILENAME_TX;
  file     file_rx                : text open write_mode is FILENAME_RX;
-- synthesis translate_on

  signal   sw2hw                  : SPI_sw2hw_t;
  signal   hw2sw                  : SPI_hw2sw_t;
           
  signal   miso                   : std_logic;
  signal   mosi                   : std_logic;

  alias    tx_tvalid              : std_logic        is sw2hw.data.valid;
  alias    tx_tready              : std_logic        is hw2sw.data.ready;
  alias    tx_tdata               : std_logic_vector is sw2hw.data.value;
                                                     
  alias    rx_tvalid              : std_logic        is hw2sw.data.valid;
  alias    rx_tready              : std_logic        is sw2hw.data.ready;
  alias    rx_tdata               : std_logic_vector is hw2sw.data.value;

  alias    cmd_tvalid             : std_logic        is sw2hw.cmd.valid;
  alias    cmd_tready             : std_logic        is hw2sw.cmd.ready;
  alias    cmd_last               : std_logic        is sw2hw.cmd.last(0)     ;
  alias    cmd_enable_rx          : std_logic        is sw2hw.cmd.enable_rx(0);
  alias    cmd_enable_tx          : std_logic        is sw2hw.cmd.enable_tx(0);
  alias    cmd_nb_bytes           : std_logic_vector is sw2hw.cmd.nb_bytes    ;

  
begin  -- architecture rtl

  ins_csr : SPI_registers
  generic map(
    USER_DEFINE_PRESCALER => USER_DEFINE_PRESCALER,
    PRESCALER_RATIO       => PRESCALER_RATIO,
    DEPTH_CMD             => DEPTH_CMD,
    DEPTH_TX              => DEPTH_TX,
    DEPTH_RX              => DEPTH_RX
    )
  port map(
    clk_i     => clk_i           ,
    arst_b_i  => arst_b_i        ,
    sbi_ini_i => sbi_ini_i       ,
    sbi_tgt_o => sbi_tgt_o       ,
    sw2hw_o   => sw2hw           ,
    hw2sw_i   => hw2sw   
  );

  ins_spi_master : spi_master
    generic map(
      PRESCALER_WIDTH      => 8
      )
    port map
    ( clk_i                 => clk_i
     ,arst_b_i              => sw2hw.cfg.spi_enable(0)
     ,tx_tvalid_i           => sw2hw.data.valid
     ,tx_tready_o           => hw2sw.data.ready
     ,tx_tdata_i            => sw2hw.data.value
     ,rx_tvalid_o           => hw2sw.data.valid
     ,rx_tready_i           => sw2hw.data.ready
     ,rx_tdata_o            => hw2sw.data.value
     ,cmd_tvalid_i          => sw2hw.cmd.valid
     ,cmd_tready_o          => hw2sw.cmd.ready
     ,cmd_tlast_i           => sw2hw.cmd.last(0)
     ,cmd_enable_rx_i       => sw2hw.cmd.enable_rx(0)
     ,cmd_enable_tx_i       => sw2hw.cmd.enable_tx(0)
     ,cmd_nb_bytes_i        => sw2hw.cmd.nb_bytes
     ,cfg_cpol_i            => sw2hw.cfg.cpol(0)
     ,cfg_cpha_i            => sw2hw.cfg.cpha(0)
     ,cfg_prescaler_ratio_i => sw2hw.prescaler.ratio
     ,sclk_o                => sclk_o   
     ,sclk_oe_o             => sclk_oe_o
     ,cs_b_o                => cs_b_o   
     ,cs_b_oe_o             => cs_b_oe_o
     ,mosi_o                => mosi   
     ,mosi_oe_o             => mosi_oe_o
     ,miso_i                => miso   
    );

  -- Loopback
  miso   <= mosi when sw2hw.cfg.loopback = "1" else
            miso_i;

  mosi_o <= mosi;

-- synthesis translate_off
  process (clk_i) is
    variable line_buffer : line;

    
  begin  -- process

    if rising_edge(clk_i)
    then
     
      if (tx_tvalid and tx_tready)
      then
        write    (line_buffer, tx_tdata);
        write    (line_buffer, string'(" - 0x"));
        write    (line_buffer, to_hstring(tx_tdata));
        write    (line_buffer, string'(" - "));
        write    (line_buffer, character'val(to_integer(unsigned(tx_tdata))));
        writeline(file_tx, line_buffer);
      end if;
      
      if (rx_tvalid and rx_tready)
      then
        write    (line_buffer, rx_tdata);
        write    (line_buffer, string'(" - 0x"));
        write    (line_buffer, to_hstring(rx_tdata));
        write    (line_buffer, string'(" - "));
        write    (line_buffer, character'val(to_integer(unsigned(rx_tdata))));
        writeline(file_rx, line_buffer);
      end if;

     if (cmd_tvalid and cmd_tready)
     then
       write    (line_buffer, cmd_enable_tx);
       write    (line_buffer, string'(" - "));
       write    (line_buffer, cmd_enable_rx);
       write    (line_buffer, string'(" - "));
       write    (line_buffer, cmd_last);
       write    (line_buffer, string'(" - 0x"));
       write    (line_buffer, to_hstring(cmd_nb_bytes));
       writeline(file_cmd, line_buffer);
     end if;
      
    end if;
  end process;
-- synthesis translate_on
end architecture rtl;
