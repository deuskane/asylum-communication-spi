-- Generated VHDL Package for SPI

library IEEE;
use     IEEE.STD_LOGIC_1164.ALL;
use     IEEE.NUMERIC_STD.ALL;

library asylum;
use     asylum.sbi_pkg.all;
--==================================
-- Module      : SPI
-- Description : CSR for SPI
-- Width       : 8
--==================================

package SPI_csr_pkg is

  ------------------------------------
  -- Global Constants
  ------------------------------------

  constant SPI_ADDR_WIDTH : natural := 2;
  constant SPI_DATA_WIDTH : natural := 8;

  --==================================
  -- Register    : data
  -- Description : Write : data to tansmit, Read : data to receive
  -- Address     : 0x0
  -- Width       : 8
  -- Sw Access   : rw
  -- Hw Access   : rw
  -- Hw Type     : fifo
  --==================================
  constant SPI_DATA : unsigned(SPI_ADDR_WIDTH-1 downto 0) := to_unsigned(0, SPI_ADDR_WIDTH);

  type SPI_data_sw2hw_t is record
    ready : std_logic;
    valid : std_logic;
  --==================================
  -- Field       : value
  -- Description : Data TX or Data RX
  -- Width       : 8
  --==================================
    value : std_logic_vector(8-1 downto 0);
    sw2hw_empty : std_logic;
    sw2hw_full  : std_logic;
    hw2sw_empty : std_logic;
    hw2sw_full  : std_logic;
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
  constant SPI_CMD : unsigned(SPI_ADDR_WIDTH-1 downto 0) := to_unsigned(1, SPI_ADDR_WIDTH);

  type SPI_cmd_sw2hw_t is record
    valid : std_logic;
  --==================================
  -- Field       : nb_bytes
  -- Description : Transfert Length in bytes (N+1)
  -- Width       : 2
  --==================================
    nb_bytes : std_logic_vector(2-1 downto 0);
  --==================================
  -- Field       : size
  -- Description : Transfert Size : 1/2/4/8
  -- Width       : 2
  --==================================
    size : std_logic_vector(2-1 downto 0);
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
  --==================================
  -- Field       : last
  -- Description : Last Transfert - 0 : not last cs keep active after transfer, 1 : last packet to transfer cs go inactive after transfer. SPECIAL CASE if last = enable_rx = enable_tx = 0 then stop the transfert
  -- Width       : 1
  --==================================
    last : std_logic_vector(1-1 downto 0);
  --==================================
  -- Field       : cfg
  -- Description : Configuration - 0 : configure enable_tx/enable_rx and size, 1 : replace enable_tx/enable_rx and size by nb_bytes[5:2]
  -- Width       : 1
  --==================================
    cfg : std_logic_vector(1-1 downto 0);
    sw2hw_empty : std_logic;
    sw2hw_full  : std_logic;
  end record SPI_cmd_sw2hw_t;

  type SPI_cmd_hw2sw_t is record
    ready : std_logic;
  end record SPI_cmd_hw2sw_t;

-- Enum        : cmd.size.SINGLE
-- Description : SPI SINGLE Legacy (MISO/MOSI) mode
constant SPI_CMD_SIZE_SINGLE     : std_logic_vector(2-1 downto 0) := "00";
constant SPI_CMD_SIZE_SINGLE_RAW : std_logic_vector(8-1 downto 0) := "00000000";

-- Enum        : cmd.size.DUAL
-- Description : SPI DUAL (2b) mode
constant SPI_CMD_SIZE_DUAL     : std_logic_vector(2-1 downto 0) := "01";
constant SPI_CMD_SIZE_DUAL_RAW : std_logic_vector(8-1 downto 0) := "00000100";

-- Enum        : cmd.size.QUAD
-- Description : SPI QUAD (4b) mode
constant SPI_CMD_SIZE_QUAD     : std_logic_vector(2-1 downto 0) := "10";
constant SPI_CMD_SIZE_QUAD_RAW : std_logic_vector(8-1 downto 0) := "00001000";

-- Enum        : cmd.size.OCTAL
-- Description : SPI OCTAL (8b) mode
constant SPI_CMD_SIZE_OCTAL     : std_logic_vector(2-1 downto 0) := "11";
constant SPI_CMD_SIZE_OCTAL_RAW : std_logic_vector(8-1 downto 0) := "00001100";

-- Enum        : cmd.enable_rx.DISABLE
-- Description : don't push in RX FIFO
constant SPI_CMD_ENABLE_RX_DISABLE     : std_logic_vector(1-1 downto 0) := "0";
constant SPI_CMD_ENABLE_RX_DISABLE_RAW : std_logic_vector(8-1 downto 0) := "00000000";

-- Enum        : cmd.enable_rx.ENABLE
-- Description : push in RX FIFO when receive byte
constant SPI_CMD_ENABLE_RX_ENABLE     : std_logic_vector(1-1 downto 0) := "1";
constant SPI_CMD_ENABLE_RX_ENABLE_RAW : std_logic_vector(8-1 downto 0) := "00010000";

-- Enum        : cmd.enable_tx.DISABLE
-- Description : don't pop TX FIFO and keep mosi_oe to 0
constant SPI_CMD_ENABLE_TX_DISABLE     : std_logic_vector(1-1 downto 0) := "0";
constant SPI_CMD_ENABLE_TX_DISABLE_RAW : std_logic_vector(8-1 downto 0) := "00000000";

-- Enum        : cmd.enable_tx.ENABLE
-- Description : pop TX FIFO and mosi_oe_o is 1 during the transfert
constant SPI_CMD_ENABLE_TX_ENABLE     : std_logic_vector(1-1 downto 0) := "1";
constant SPI_CMD_ENABLE_TX_ENABLE_RAW : std_logic_vector(8-1 downto 0) := "00100000";

-- Enum        : cmd.last.DISABLE
-- Description : not last cs keep active after transfer
constant SPI_CMD_LAST_DISABLE     : std_logic_vector(1-1 downto 0) := "0";
constant SPI_CMD_LAST_DISABLE_RAW : std_logic_vector(8-1 downto 0) := "00000000";

-- Enum        : cmd.last.ENABLE
-- Description : last packet to transfer cs go inactive after transfer. SPECIAL CASE if last = enable_rx = enable_tx = 0 then stop the transfert
constant SPI_CMD_LAST_ENABLE     : std_logic_vector(1-1 downto 0) := "1";
constant SPI_CMD_LAST_ENABLE_RAW : std_logic_vector(8-1 downto 0) := "01000000";

-- Enum        : cmd.cfg.CONFIG
-- Description : configure enable_tx/enable_rx and size
constant SPI_CMD_CFG_CONFIG     : std_logic_vector(1-1 downto 0) := "0";
constant SPI_CMD_CFG_CONFIG_RAW : std_logic_vector(8-1 downto 0) := "00000000";

-- Enum        : cmd.cfg.KEEP
-- Description : replace enable_tx/enable_rx and size by nb_bytes[5:2]
constant SPI_CMD_CFG_KEEP     : std_logic_vector(1-1 downto 0) := "1";
constant SPI_CMD_CFG_KEEP_RAW : std_logic_vector(8-1 downto 0) := "10000000";

  --==================================
  -- Register    : cfg
  -- Description : SPI Configuration Register
  -- Address     : 0x2
  -- Width       : 4
  -- Sw Access   : rw
  -- Hw Access   : ro
  -- Hw Type     : reg
  --==================================
  constant SPI_CFG : unsigned(SPI_ADDR_WIDTH-1 downto 0) := to_unsigned(2, SPI_ADDR_WIDTH);

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
  constant SPI_PRESCALER : unsigned(SPI_ADDR_WIDTH-1 downto 0) := to_unsigned(3, SPI_ADDR_WIDTH);

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

  ------------------------------------
  -- Component
  ------------------------------------
component SPI_registers is
  generic (
    MODULE_NAME :  string := "" -- Name of the module
   ;USER_DEFINE_PRESCALER : boolean -- Parameters to use the enable the User define Prescaler
   ;PRESCALER_RATIO : std_logic_vector(7 downto 0) -- Default value for prescaler ratio
   ;DEPTH_TX : natural -- Depth of FIFO TX (SW2HW)
   ;DEPTH_RX : natural -- Depth of FIFO RX (HW2SW)
   ;DEPTH_CMD : natural -- Depth of FIFO Command (SW2HW)
  );
  port (
    -- Clock and Reset
    clk_i      : in  std_logic
   ;arst_b_i   : in  std_logic
    -- Bus
   ;sbi_ini_i  : in  sbi_ini_t
   ;sbi_tgt_o  : out sbi_tgt_t
    -- CSR
   ;sw2hw_o    : out SPI_sw2hw_t
   ;hw2sw_i    : in  SPI_hw2sw_t
  );
end component SPI_registers;


end package SPI_csr_pkg;
