-------------------------------------------------------------------------------
-- Title      : sbi_SPI_synth_top
-- Project    : Asylum
-------------------------------------------------------------------------------
-- File       : sbi_SPI_synth_top.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- Synthesis check top (lint_ghdl, lint_ghdl_synth) : sbi_SPI (and so
-- spi_master) with flat and constrained SBI ports (sbi_SPI has unconstrained
-- SBI ports and cannot be a synthesis top). SBI address / data width :
-- SBI_ADDR_WIDTH / SBI_DATA_WIDTH. The synthesizable generics are forwarded,
-- with the defaults of the configuration of asylum-soc-picosoc
-- (USER_DEFINE_PRESCALER and PRESCALER_RATIO have no default in sbi_SPI).
-- NAME and FILENAME_* are simulation only, not connected.
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-10-20  1.0      mrosiere Created
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
library asylum;
use     asylum.sbi_pkg.all;

entity sbi_SPI_synth_top is
  generic (
    USER_DEFINE_PRESCALER : boolean                        := true;
    PRESCALER_RATIO       : std_logic_vector(8-1 downto 0) := x"00";
    DEPTH_CMD             : natural := 0;
    DEPTH_TX              : natural := 0;
    DEPTH_RX              : natural := 0;
    HANDLE_HOLD_WP        : boolean := true
  );
  port (
    clk_i         : in  std_logic;
    arst_b_i      : in  std_logic;

    -- Bus (SBI)
    cs_i          : in  std_logic;
    re_i          : in  std_logic;
    we_i          : in  std_logic;
    addr_i        : in  std_logic_vector(SBI_ADDR_WIDTH-1 downto 0);
    wdata_i       : in  std_logic_vector(SBI_DATA_WIDTH-1 downto 0);
    ready_o       : out std_logic;
    rdata_o       : out std_logic_vector(SBI_DATA_WIDTH-1 downto 0);

    -- SPI Interface
    sclk_o        : out std_logic;
    sclk_oe_o     : out std_logic;
    cs_b_o        : out std_logic;
    cs_b_oe_o     : out std_logic;
    io_o          : out std_logic_vector(8-1 downto 0);
    io_i          : in  std_logic_vector(8-1 downto 0);
    io_oe_o       : out std_logic_vector(8-1 downto 0)
  );
end entity sbi_SPI_synth_top;

architecture rtl of sbi_SPI_synth_top is

  signal sbi_ini       : sbi_ini_t(addr (SBI_ADDR_WIDTH-1 downto 0),
                                   wdata(SBI_DATA_WIDTH-1 downto 0));
  signal sbi_tgt       : sbi_tgt_t(rdata(SBI_DATA_WIDTH-1 downto 0));

begin

  sbi_ini.cs        <= cs_i;
  sbi_ini.re        <= re_i;
  sbi_ini.we        <= we_i;
  sbi_ini.addr      <= addr_i;
  sbi_ini.wdata     <= wdata_i;
  ready_o           <= sbi_tgt.ready;
  rdata_o           <= sbi_tgt.rdata;

  ins_sbi_SPI : entity asylum.sbi_SPI
    generic map (
      USER_DEFINE_PRESCALER => USER_DEFINE_PRESCALER,
      PRESCALER_RATIO       => PRESCALER_RATIO,
      DEPTH_CMD             => DEPTH_CMD,
      DEPTH_TX              => DEPTH_TX,
      DEPTH_RX              => DEPTH_RX,
      HANDLE_HOLD_WP        => HANDLE_HOLD_WP
    )
    port map (
      clk_i                 => clk_i,
      arst_b_i              => arst_b_i,
      sbi_ini_i             => sbi_ini,
      sbi_tgt_o             => sbi_tgt,
      sclk_o                => sclk_o,
      sclk_oe_o             => sclk_oe_o,
      cs_b_o                => cs_b_o,
      cs_b_oe_o             => cs_b_oe_o,
      io_o                  => io_o,
      io_i                  => io_i,
      io_oe_o               => io_oe_o
    );

end architecture rtl;
