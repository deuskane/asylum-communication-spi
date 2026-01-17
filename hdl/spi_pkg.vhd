library IEEE;
use     IEEE.STD_LOGIC_1164.ALL;
use     IEEE.NUMERIC_STD.ALL;
library asylum;
use     asylum.sbi_pkg.all;

package spi_pkg is
-- [COMPONENT_INSERT][BEGIN]
component spi_master is
  generic (
    PRESCALER_WIDTH      : integer := 8
    );
  port (
    -- Clock & Reset
    clk_i                : in  std_logic;
    arst_b_i             : in  std_logic;

    -- Data From AXI Stream To SPI
    tx_tvalid_i          : in  std_logic;
    tx_tready_o          : out std_logic;
    tx_tdata_i           : in  std_logic_vector(8-1 downto 0);

    -- Data From SPI To AXI Stream
    rx_tvalid_o          : out std_logic;
    rx_tready_i          : in  std_logic;
    rx_tdata_o           : out std_logic_vector(8-1 downto 0);
    
    -- Command
    cmd_tvalid_i         : in  std_logic;
    cmd_tready_o         : out std_logic;
    cmd_tlast_i          : in  std_logic;
    cmd_enable_rx_i      : in  std_logic;
    cmd_enable_tx_i      : in  std_logic;
    cmd_nb_bytes_i       : in  std_logic_vector;

    -- Configuration
    cfg_cpol_i           : in  std_logic;
    cfg_cpha_i           : in  std_logic;
    cfg_prescaler_ratio_i: in  std_logic_vector(PRESCALER_WIDTH-1 downto 0);
    cfg_loopback_i       : in  std_logic;
    
    -- SPI Interface
    sclk_o               : out std_logic;
    sclk_oe_o            : out std_logic;
    cs_b_o               : out std_logic;
    cs_b_oe_o            : out std_logic;
    mosi_o               : out std_logic;
    mosi_oe_o            : out std_logic;
    miso_i               : in  std_logic
    );
end component spi_master;

component sbi_SPI is
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

end component sbi_SPI;

-- [COMPONENT_INSERT][END]

end spi_pkg;
