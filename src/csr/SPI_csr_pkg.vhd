-- Generated VHDL Package for SPI

library IEEE;
use     IEEE.STD_LOGIC_1164.ALL;
use     IEEE.NUMERIC_STD.ALL;

--==================================
-- Module      : SPI
-- Description : CSR for SPI
-- Width       : 8
--==================================

package SPI_csr_pkg is

  --==================================
  -- Register    : data
  -- Description : Write : data to tansmit, Read : data to receive
  -- Address     : 0x0
  -- Width       : 8
  -- Sw Access   : rw
  -- Hw Access   : rw
  -- Hw Type     : fifo
  --==================================
  type SPI_data_sw2hw_t is record
    ready : std_logic;
    valid : std_logic;
  --==================================
  -- Field       : value
  -- Description : Data TX or Data RX
  -- Width       : 8
  --==================================
    value : std_logic_vector(8-1 downto 0);
    rx_empty : std_logic;
    rx_full  : std_logic;
    tx_empty : std_logic;
    tx_full  : std_logic;
  end record SPI_data_sw2hw_t;

  type SPI_data_hw2sw_t is record
    ready : std_logic;
    valid : std_logic;
  --==================================
  -- Field       : value
  -- Description : Data TX or Data RX
  -- Width       : 8
  --==================================
    value : std_logic_vector(8-1 downto 0);
  end record SPI_data_hw2sw_t;

  --==================================
  -- Register    : cmd
  -- Description : Command FIFO
  -- Address     : 0x1
  -- Width       : 8
  -- Sw Access   : wo
  -- Hw Access   : ro
  -- Hw Type     : fifo
  --==================================
  type SPI_cmd_sw2hw_t is record
    valid : std_logic;
  --==================================
  -- Field       : nb_bytes
  -- Description : Transfert Length in bytes
  -- Width       : 5
  --==================================
    nb_bytes : std_logic_vector(5-1 downto 0);
  --==================================
  -- Field       : last
  -- Description : Last Transfert - 0 : not last cs keep active after transfer, 1 : last packet to transfer cs go inactive after transfer. SPECIAL CASE if last = enable_rx = enable_tx = 0 then stop the transfert
  -- Width       : 1
  --==================================
    last : std_logic_vector(1-1 downto 0);
  --==================================
  -- Field       : enable_rx
  -- Description : Push in RX FIFO - 0 : don't push in RX FIFO, 1 : push in RX FIFO when receive byte
  -- Width       : 1
  --==================================
    enable_rx : std_logic_vector(1-1 downto 0);
  --==================================
  -- Field       : enable_tx
  -- Description : POP from TX FIFO - 0 : don't pop TX FIFO and keep mosi_oe to 0, 1 pop TX FIFO and mosi_oe_o is 1 during the transfert
  -- Width       : 1
  --==================================
    enable_tx : std_logic_vector(1-1 downto 0);
    rx_empty : std_logic;
    rx_full  : std_logic;
  end record SPI_cmd_sw2hw_t;

  type SPI_cmd_hw2sw_t is record
    ready : std_logic;
  end record SPI_cmd_hw2sw_t;

  --==================================
  -- Register    : cfg
  -- Description : SPI Configuration Register
  -- Address     : 0x2
  -- Width       : 4
  -- Sw Access   : rw
  -- Hw Access   : ro
  -- Hw Type     : reg
  --==================================
  type SPI_cfg_sw2hw_t is record
    re : std_logic;
    we : std_logic;
  --==================================
  -- Field       : spi_enable
  -- Description : 0 : Parity is even, 1 : Parity is odd
  -- Width       : 1
  --==================================
    spi_enable : std_logic_vector(1-1 downto 0);
  --==================================
  -- Field       : cpol
  -- Description : Clock Polarity
  -- Width       : 1
  --==================================
    cpol : std_logic_vector(1-1 downto 0);
  --==================================
  -- Field       : cpha
  -- Description : Clock Phase
  -- Width       : 1
  --==================================
    cpha : std_logic_vector(1-1 downto 0);
  --==================================
  -- Field       : loopback
  -- Description : 0 : MISO is connected to SPI MISO, 1 MISO is connected to MOSI
  -- Width       : 1
  --==================================
    loopback : std_logic_vector(1-1 downto 0);
  end record SPI_cfg_sw2hw_t;

  --==================================
  -- Register    : prescaler
  -- Description : SPI Clock Prescaler. SCLK Frequency is CLK / 2*(prescaler+1)
  -- Address     : 0x3
  -- Width       : 8
  -- Sw Access   : rw
  -- Hw Access   : ro
  -- Hw Type     : reg
  --==================================
  type SPI_prescaler_sw2hw_t is record
    re : std_logic;
    we : std_logic;
  --==================================
  -- Field       : ratio
  -- Description : Baud Tick Counter Max
  -- Width       : 8
  --==================================
    ratio : std_logic_vector(8-1 downto 0);
  end record SPI_prescaler_sw2hw_t;

  ------------------------------------
  -- Structure SPI_t
  ------------------------------------
  type SPI_sw2hw_t is record
    data : SPI_data_sw2hw_t;
    cmd : SPI_cmd_sw2hw_t;
    cfg : SPI_cfg_sw2hw_t;
    prescaler : SPI_prescaler_sw2hw_t;
  end record SPI_sw2hw_t;

  type SPI_hw2sw_t is record
    data : SPI_data_hw2sw_t;
    cmd : SPI_cmd_hw2sw_t;
  end record SPI_hw2sw_t;

  constant SPI_ADDR_WIDTH : natural := 2;
  constant SPI_DATA_WIDTH : natural := 8;

end package SPI_csr_pkg;
