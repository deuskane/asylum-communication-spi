-------------------------------------------------------------------------------
-- Title      : sbi_SPI wrapper
-- Project    : Asylum SPI
-------------------------------------------------------------------------------
-- File       : sbi_SPI_wrapper.vhd
-- Author     : Mathieu Rosière
-- Company    :
-- Created    : 2026-10-13
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
--   Constrain the unbounded SBI records before connecting them to the DUT.
--   This avoids width mismatches between the generic SBI record type and the
--   SPI-specific address/data widths used in the CSR block.
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;

library asylum;
use     asylum.sbi_pkg.all;
use     asylum.spi_pkg.all;
use     asylum.SPI_csr_pkg.all;

entity sbi_SPI_wrapper is
  generic(
    NAME                  : string  := "";
    USER_DEFINE_PRESCALER : boolean := true;
    PRESCALER_RATIO       : std_logic_vector(8-1 downto 0) := (others => '0');
    DEPTH_CMD             : natural := 8;
    DEPTH_TX              : natural := 8;
    DEPTH_RX              : natural := 8;
    HANDLE_HOLD_WP        : boolean := false;
    FILENAME_CMD          : string  := "dump_spi_cmd.txt";
    FILENAME_TX           : string  := "dump_spi_tx.txt";
    FILENAME_RX           : string  := "dump_spi_rx.txt"
  );
  port(
    clk_i      : in  std_logic;
    arst_b_i   : in  std_logic;

    sbi_ini_i  : in  sbi_ini_t(addr (SPI_ADDR_WIDTH-1 downto 0),
                               wdata(SPI_DATA_WIDTH-1 downto 0));
    sbi_tgt_o  : out sbi_tgt_t(rdata(SPI_DATA_WIDTH-1 downto 0));

    sclk_o     : out std_logic;
    sclk_oe_o  : out std_logic;
    cs_b_o     : out std_logic;
    cs_b_oe_o  : out std_logic;
    io_o       : out std_logic_vector(8-1 downto 0);
    io_i       : in  std_logic_vector(8-1 downto 0);
    io_oe_o    : out std_logic_vector(8-1 downto 0)
  );
end entity sbi_SPI_wrapper;

architecture rtl of sbi_SPI_wrapper is

begin

  ins_dut : sbi_SPI
    generic map(
      NAME                  => NAME,
      USER_DEFINE_PRESCALER => USER_DEFINE_PRESCALER,
      PRESCALER_RATIO       => PRESCALER_RATIO,
      DEPTH_CMD             => DEPTH_CMD,
      DEPTH_TX              => DEPTH_TX,
      DEPTH_RX              => DEPTH_RX,
      HANDLE_HOLD_WP        => HANDLE_HOLD_WP,
      FILENAME_CMD          => FILENAME_CMD,
      FILENAME_TX           => FILENAME_TX,
      FILENAME_RX           => FILENAME_RX
    )
    port map(
      clk_i     => clk_i,
      arst_b_i  => arst_b_i,
      sbi_ini_i => sbi_ini_i,
      sbi_tgt_o => sbi_tgt_o,
      sclk_o    => sclk_o,
      sclk_oe_o => sclk_oe_o,
      cs_b_o    => cs_b_o,
      cs_b_oe_o => cs_b_oe_o,
      io_o      => io_o,
      io_i      => io_i,
      io_oe_o   => io_oe_o
    );

end architecture rtl;
