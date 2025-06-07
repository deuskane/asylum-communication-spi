-- Generated VHDL Module for SPI


library IEEE;
use     IEEE.STD_LOGIC_1164.ALL;
use     IEEE.NUMERIC_STD.ALL;

library work;
use     work.SPI_csr_pkg.ALL;
library work;
use     work.pbi_pkg.all;

--==================================
-- Module      : SPI
-- Description : CSR for SPI
-- Width       : 8
--==================================
entity SPI_registers is
  generic (
    USER_DEFINE_PRESCALER : boolean -- Parameters to use the enable the User define Prescaler
   ;PRESCALER_RATIO : std_logic_vector(7 downto 0) -- Default value for prescaler ratio
  );
  port (
    -- Clock and Reset
    clk_i      : in  std_logic;
    arst_b_i   : in  std_logic;
    -- Bus
    pbi_ini_i  : in  pbi_ini_t;
    pbi_tgt_o  : out pbi_tgt_t;
    -- CSR
    sw2hw_o    : out SPI_sw2hw_t;
    hw2sw_i    : in  SPI_hw2sw_t
  );
end entity SPI_registers;

architecture rtl of SPI_registers is

  signal   sig_wcs   : std_logic;
  signal   sig_we    : std_logic;
  signal   sig_waddr : std_logic_vector(pbi_ini_i.addr'length-1 downto 0);
  signal   sig_wdata : std_logic_vector(pbi_ini_i.wdata'length-1 downto 0);
  signal   sig_wbusy : std_logic;

  signal   sig_rcs   : std_logic;
  signal   sig_re    : std_logic;
  signal   sig_raddr : std_logic_vector(pbi_ini_i.addr'length-1 downto 0);
  signal   sig_rdata : std_logic_vector(pbi_tgt_o.rdata'length-1 downto 0);
  signal   sig_rbusy : std_logic;

  signal   sig_busy  : std_logic;

  constant INIT_data : std_logic_vector(8-1 downto 0) :=
             "00000000" -- value
           ;
  signal   data_wcs       : std_logic;
  signal   data_we        : std_logic;
  signal   data_wdata     : std_logic_vector(8-1 downto 0);
  signal   data_wdata_sw  : std_logic_vector(8-1 downto 0);
  signal   data_wdata_hw  : std_logic_vector(8-1 downto 0);
  signal   data_wbusy     : std_logic;

  signal   data_rcs       : std_logic;
  signal   data_re        : std_logic;
  signal   data_rdata     : std_logic_vector(8-1 downto 0);
  signal   data_rdata_sw  : std_logic_vector(8-1 downto 0);
  signal   data_rdata_hw  : std_logic_vector(8-1 downto 0);
  signal   data_rbusy     : std_logic;

  constant INIT_cmd : std_logic_vector(8-1 downto 0) :=
             "00000" -- nb_byte
           & "0" -- last
           & "0" -- enable_rx
           & "0" -- enable_tx
           ;
  signal   cmd_wcs       : std_logic;
  signal   cmd_we        : std_logic;
  signal   cmd_wdata     : std_logic_vector(8-1 downto 0);
  signal   cmd_wdata_sw  : std_logic_vector(8-1 downto 0);
  signal   cmd_wdata_hw  : std_logic_vector(8-1 downto 0);
  signal   cmd_wbusy     : std_logic;

  signal   cmd_rcs       : std_logic;
  signal   cmd_re        : std_logic;
  signal   cmd_rdata     : std_logic_vector(8-1 downto 0);
  signal   cmd_rdata_sw  : std_logic_vector(8-1 downto 0);
  signal   cmd_rdata_hw  : std_logic_vector(8-1 downto 0);
  signal   cmd_rbusy     : std_logic;

  constant INIT_cfg : std_logic_vector(4-1 downto 0) :=
             "0" -- spi_enable
           & "0" -- cpol
           & "0" -- cpha
           & "0" -- loopback
           ;
  signal   cfg_wcs       : std_logic;
  signal   cfg_we        : std_logic;
  signal   cfg_wdata     : std_logic_vector(8-1 downto 0);
  signal   cfg_wdata_sw  : std_logic_vector(4-1 downto 0);
  signal   cfg_wdata_hw  : std_logic_vector(4-1 downto 0);
  signal   cfg_wbusy     : std_logic;

  signal   cfg_rcs       : std_logic;
  signal   cfg_re        : std_logic;
  signal   cfg_rdata     : std_logic_vector(8-1 downto 0);
  signal   cfg_rdata_sw  : std_logic_vector(4-1 downto 0);
  signal   cfg_rdata_hw  : std_logic_vector(4-1 downto 0);
  signal   cfg_rbusy     : std_logic;

  constant INIT_prescaler : std_logic_vector(8-1 downto 0) :=
             PRESCALER_RATIO -- ratio
           ;
  signal   prescaler_wcs       : std_logic;
  signal   prescaler_we        : std_logic;
  signal   prescaler_wdata     : std_logic_vector(8-1 downto 0);
  signal   prescaler_wdata_sw  : std_logic_vector(8-1 downto 0);
  signal   prescaler_wdata_hw  : std_logic_vector(8-1 downto 0);
  signal   prescaler_wbusy     : std_logic;

  signal   prescaler_rcs       : std_logic;
  signal   prescaler_re        : std_logic;
  signal   prescaler_rdata     : std_logic_vector(8-1 downto 0);
  signal   prescaler_rdata_sw  : std_logic_vector(8-1 downto 0);
  signal   prescaler_rdata_hw  : std_logic_vector(8-1 downto 0);
  signal   prescaler_rbusy     : std_logic;

begin  -- architecture rtl

  -- Interface 
  sig_wcs   <= pbi_ini_i.cs;
  sig_we    <= pbi_ini_i.we;
  sig_waddr <= pbi_ini_i.addr;
  sig_wdata <= pbi_ini_i.wdata;

  sig_rcs   <= pbi_ini_i.cs;
  sig_re    <= pbi_ini_i.re;
  sig_raddr <= pbi_ini_i.addr;
  pbi_tgt_o.rdata <= sig_rdata;
  pbi_tgt_o.busy <= sig_busy;

  sig_busy  <= sig_wbusy when sig_we = '1' else
               sig_rbusy when sig_re = '1' else
               '0';

  gen_data: if (True)
  generate
  --==================================
  -- Register    : data
  -- Description : Write : data to tansmit, Read : data to receive
  -- Address     : 0x0
  -- Width       : 8
  -- Sw Access   : rw
  -- Hw Access   : rw
  -- Hw Type     : fifo
  --==================================
  --==================================
  -- Field       : value
  -- Description : Data TX or Data RX
  -- Width       : 8
  --==================================


    data_rcs     <= '1' when     (sig_raddr(SPI_ADDR_WIDTH-1 downto 0) = std_logic_vector(to_unsigned(0,SPI_ADDR_WIDTH))) else '0';
    data_re      <= sig_rcs and sig_re and data_rcs;
    data_rdata   <= (
      0 => data_rdata_sw(0), -- value(0)
      1 => data_rdata_sw(1), -- value(1)
      2 => data_rdata_sw(2), -- value(2)
      3 => data_rdata_sw(3), -- value(3)
      4 => data_rdata_sw(4), -- value(4)
      5 => data_rdata_sw(5), -- value(5)
      6 => data_rdata_sw(6), -- value(6)
      7 => data_rdata_sw(7), -- value(7)
      others => '0');

    data_wcs     <= '1' when       (sig_waddr(SPI_ADDR_WIDTH-1 downto 0) = std_logic_vector(to_unsigned(0,SPI_ADDR_WIDTH)))   else '0';
    data_we      <= sig_wcs and sig_we and data_wcs;
    data_wdata   <= sig_wdata;
    data_wdata_sw(7 downto 0) <= data_wdata(7 downto 0); -- value
    data_wdata_hw(7 downto 0) <= hw2sw_i.data.value; -- value
    sw2hw_o.data.value <= data_rdata_hw(7 downto 0); -- value

    ins_data : entity work.csr_fifo(rtl)
      generic map
        (WIDTH         => 8
        ,BLOCKING_READ => True
        ,BLOCKING_WRITE => True
        )
      port map
        (clk_i         => clk_i
        ,arst_b_i      => arst_b_i
        ,sw_wd_i       => data_wdata_sw
        ,sw_rd_o       => data_rdata_sw
        ,sw_we_i       => data_we
        ,sw_re_i       => data_re
        ,sw_rbusy_o    => data_rbusy
        ,sw_wbusy_o    => data_wbusy
        ,hw_tx_valid_i => hw2sw_i.data.valid
        ,hw_tx_ready_o => sw2hw_o.data.ready
        ,hw_tx_data_i  => data_wdata_hw
        ,hw_rx_valid_o => sw2hw_o.data.valid
        ,hw_rx_ready_i => hw2sw_i.data.ready
        ,hw_rx_data_o  => data_rdata_hw
        );

  end generate gen_data;

  gen_data_b: if not (True)
  generate
    data_rcs     <= '0';
    data_rbusy   <= '0';
    data_rdata   <= (others => '0');
    data_wcs      <= '0';
    data_wbusy    <= '0';
    sw2hw_o.data.value <= "00000000";
    sw2hw_o.data.ready <= '0';
    sw2hw_o.data.valid <= '0';
  end generate gen_data_b;

  gen_cmd: if (True)
  generate
  --==================================
  -- Register    : cmd
  -- Description : Command FIFO
  -- Address     : 0x1
  -- Width       : 8
  -- Sw Access   : wo
  -- Hw Access   : wo
  -- Hw Type     : fifo
  --==================================
  --==================================
  -- Field       : nb_byte
  -- Description : NB Byte to TX/RX
  -- Width       : 5
  --==================================

  --==================================
  -- Field       : last
  -- Description : 0 : not last cs keep active after transfer, 1 : last packet to transfer cs go inactive after transfer
  -- Width       : 1
  --==================================

  --==================================
  -- Field       : enable_rx
  -- Description : if 1 then push receive byte into rx fifo else not
  -- Width       : 1
  --==================================

  --==================================
  -- Field       : enable_tx
  -- Description : if 1 then mosi_oe_o is 1 else mosi_oe_o = 0
  -- Width       : 1
  --==================================


    cmd_rcs     <= '0';
    cmd_re      <= '0';
    cmd_rbusy   <= '0';
    cmd_rdata   <= (others=>'0');

    cmd_wcs     <= '1' when       (sig_waddr(SPI_ADDR_WIDTH-1 downto 0) = std_logic_vector(to_unsigned(1,SPI_ADDR_WIDTH)))   else '0';
    cmd_we      <= sig_wcs and sig_we and cmd_wcs;
    cmd_wdata   <= sig_wdata;
    cmd_wdata_sw(4 downto 0) <= cmd_wdata(4 downto 0); -- nb_byte
    cmd_wdata_sw(5 downto 5) <= cmd_wdata(5 downto 5); -- last
    cmd_wdata_sw(6 downto 6) <= cmd_wdata(6 downto 6); -- enable_rx
    cmd_wdata_sw(7 downto 7) <= cmd_wdata(7 downto 7); -- enable_tx
    cmd_wdata_hw(4 downto 0) <= hw2sw_i.cmd.nb_byte; -- nb_byte
    cmd_wdata_hw(5 downto 5) <= hw2sw_i.cmd.last; -- last
    cmd_wdata_hw(6 downto 6) <= hw2sw_i.cmd.enable_rx; -- enable_rx
    cmd_wdata_hw(7 downto 7) <= hw2sw_i.cmd.enable_tx; -- enable_tx

    ins_cmd : entity work.csr_fifo(rtl)
      generic map
        (WIDTH         => 8
        ,BLOCKING_READ => True
        ,BLOCKING_WRITE => True
        )
      port map
        (clk_i         => clk_i
        ,arst_b_i      => arst_b_i
        ,sw_wd_i       => cmd_wdata_sw
        ,sw_rd_o       => cmd_rdata_sw
        ,sw_we_i       => cmd_we
        ,sw_re_i       => cmd_re
        ,sw_rbusy_o    => cmd_rbusy
        ,sw_wbusy_o    => cmd_wbusy
        ,hw_tx_valid_i => hw2sw_i.cmd.valid
        ,hw_tx_ready_o => open
        ,hw_tx_data_i  => cmd_wdata_hw
        ,hw_rx_valid_o => sw2hw_o.cmd.valid
        ,hw_rx_ready_i => '1'
        ,hw_rx_data_o  => open
        );

  end generate gen_cmd;

  gen_cmd_b: if not (True)
  generate
    cmd_rcs     <= '0';
    cmd_rbusy   <= '0';
    cmd_rdata   <= (others => '0');
    cmd_wcs      <= '0';
    cmd_wbusy    <= '0';
    sw2hw_o.cmd.valid <= '0';
  end generate gen_cmd_b;

  gen_cfg: if (True)
  generate
  --==================================
  -- Register    : cfg
  -- Description : SPI Configuration Register
  -- Address     : 0x2
  -- Width       : 4
  -- Sw Access   : rw
  -- Hw Access   : ro
  -- Hw Type     : reg
  --==================================
  --==================================
  -- Field       : spi_enable
  -- Description : 0 : Parity is even, 1 : Parity is odd
  -- Width       : 1
  --==================================

  --==================================
  -- Field       : cpol
  -- Description : Clock Polarity
  -- Width       : 1
  --==================================

  --==================================
  -- Field       : cpha
  -- Description : Clock Phase
  -- Width       : 1
  --==================================

  --==================================
  -- Field       : loopback
  -- Description : 0 : MISO is connected to SPI MISO, 1 MISO is connected to MOSI
  -- Width       : 1
  --==================================


    cfg_rcs     <= '1' when     (sig_raddr(SPI_ADDR_WIDTH-1 downto 0) = std_logic_vector(to_unsigned(2,SPI_ADDR_WIDTH))) else '0';
    cfg_re      <= sig_rcs and sig_re and cfg_rcs;
    cfg_rdata   <= (
      0 => cfg_rdata_sw(0), -- spi_enable(0)
      1 => cfg_rdata_sw(1), -- cpol(0)
      2 => cfg_rdata_sw(2), -- cpha(0)
      3 => cfg_rdata_sw(3), -- loopback(0)
      others => '0');

    cfg_wcs     <= '1' when       (sig_waddr(SPI_ADDR_WIDTH-1 downto 0) = std_logic_vector(to_unsigned(2,SPI_ADDR_WIDTH)))   else '0';
    cfg_we      <= sig_wcs and sig_we and cfg_wcs;
    cfg_wdata   <= sig_wdata;
    cfg_wdata_sw(0 downto 0) <= cfg_wdata(0 downto 0); -- spi_enable
    cfg_wdata_sw(1 downto 1) <= cfg_wdata(1 downto 1); -- cpol
    cfg_wdata_sw(2 downto 2) <= cfg_wdata(2 downto 2); -- cpha
    cfg_wdata_sw(3 downto 3) <= cfg_wdata(3 downto 3); -- loopback
    sw2hw_o.cfg.spi_enable <= cfg_rdata_hw(0 downto 0); -- spi_enable
    sw2hw_o.cfg.cpol <= cfg_rdata_hw(1 downto 1); -- cpol
    sw2hw_o.cfg.cpha <= cfg_rdata_hw(2 downto 2); -- cpha
    sw2hw_o.cfg.loopback <= cfg_rdata_hw(3 downto 3); -- loopback

    ins_cfg : entity work.csr_reg(rtl)
      generic map
        (WIDTH         => 4
        ,INIT          => INIT_cfg
        ,MODEL         => "rw"
        )
      port map
        (clk_i         => clk_i
        ,arst_b_i      => arst_b_i
        ,sw_wd_i       => cfg_wdata_sw
        ,sw_rd_o       => cfg_rdata_sw
        ,sw_we_i       => cfg_we
        ,sw_re_i       => cfg_re
        ,sw_rbusy_o    => cfg_rbusy
        ,sw_wbusy_o    => cfg_wbusy
        ,hw_wd_i       => (others => '0')
        ,hw_rd_o       => cfg_rdata_hw
        ,hw_we_i       => '0'
        ,hw_sw_re_o    => sw2hw_o.cfg.re
        ,hw_sw_we_o    => sw2hw_o.cfg.we
        );

  end generate gen_cfg;

  gen_cfg_b: if not (True)
  generate
    cfg_rcs     <= '0';
    cfg_rbusy   <= '0';
    cfg_rdata   <= (others => '0');
    cfg_wcs      <= '0';
    cfg_wbusy    <= '0';
    sw2hw_o.cfg.spi_enable <= "0";
    sw2hw_o.cfg.cpol <= "0";
    sw2hw_o.cfg.cpha <= "0";
    sw2hw_o.cfg.loopback <= "0";
    sw2hw_o.cfg.re <= '0';
    sw2hw_o.cfg.we <= '0';
  end generate gen_cfg_b;

  gen_prescaler: if (USER_DEFINE_PRESCALER)
  generate
  --==================================
  -- Register    : prescaler
  -- Description : SPI Clock Prescaler. SCLK Frequency is CLK / 2*(prescaler+1)
  -- Address     : 0x3
  -- Width       : 8
  -- Sw Access   : rw
  -- Hw Access   : ro
  -- Hw Type     : reg
  --==================================
  --==================================
  -- Field       : ratio
  -- Description : Baud Tick Counter Max
  -- Width       : 8
  --==================================


    prescaler_rcs     <= '1' when     (sig_raddr(SPI_ADDR_WIDTH-1 downto 0) = std_logic_vector(to_unsigned(3,SPI_ADDR_WIDTH))) else '0';
    prescaler_re      <= sig_rcs and sig_re and prescaler_rcs;
    prescaler_rdata   <= (
      0 => prescaler_rdata_sw(0), -- ratio(0)
      1 => prescaler_rdata_sw(1), -- ratio(1)
      2 => prescaler_rdata_sw(2), -- ratio(2)
      3 => prescaler_rdata_sw(3), -- ratio(3)
      4 => prescaler_rdata_sw(4), -- ratio(4)
      5 => prescaler_rdata_sw(5), -- ratio(5)
      6 => prescaler_rdata_sw(6), -- ratio(6)
      7 => prescaler_rdata_sw(7), -- ratio(7)
      others => '0');

    prescaler_wcs     <= '1' when       (sig_waddr(SPI_ADDR_WIDTH-1 downto 0) = std_logic_vector(to_unsigned(3,SPI_ADDR_WIDTH)))   else '0';
    prescaler_we      <= sig_wcs and sig_we and prescaler_wcs;
    prescaler_wdata   <= sig_wdata;
    prescaler_wdata_sw(7 downto 0) <= prescaler_wdata(7 downto 0); -- ratio
    sw2hw_o.prescaler.ratio <= prescaler_rdata_hw(7 downto 0); -- ratio

    ins_prescaler : entity work.csr_reg(rtl)
      generic map
        (WIDTH         => 8
        ,INIT          => INIT_prescaler
        ,MODEL         => "rw"
        )
      port map
        (clk_i         => clk_i
        ,arst_b_i      => arst_b_i
        ,sw_wd_i       => prescaler_wdata_sw
        ,sw_rd_o       => prescaler_rdata_sw
        ,sw_we_i       => prescaler_we
        ,sw_re_i       => prescaler_re
        ,sw_rbusy_o    => prescaler_rbusy
        ,sw_wbusy_o    => prescaler_wbusy
        ,hw_wd_i       => (others => '0')
        ,hw_rd_o       => prescaler_rdata_hw
        ,hw_we_i       => '0'
        ,hw_sw_re_o    => sw2hw_o.prescaler.re
        ,hw_sw_we_o    => sw2hw_o.prescaler.we
        );

  end generate gen_prescaler;

  gen_prescaler_b: if not (USER_DEFINE_PRESCALER)
  generate
    prescaler_rcs     <= '0';
    prescaler_rbusy   <= '0';
    prescaler_rdata   <= (others => '0');
    prescaler_wcs      <= '0';
    prescaler_wbusy    <= '0';
    sw2hw_o.prescaler.ratio <= PRESCALER_RATIO;
    sw2hw_o.prescaler.re <= '0';
    sw2hw_o.prescaler.we <= '0';
  end generate gen_prescaler_b;

  sig_wbusy <= 
    data_wbusy when data_wcs = '1' else
    cmd_wbusy when cmd_wcs = '1' else
    cfg_wbusy when cfg_wcs = '1' else
    prescaler_wbusy when prescaler_wcs = '1' else
    '0'; -- Bad Address, no busy
  sig_rbusy <= 
    data_rbusy when data_rcs = '1' else
    cmd_rbusy when cmd_rcs = '1' else
    cfg_rbusy when cfg_rcs = '1' else
    prescaler_rbusy when prescaler_rcs = '1' else
    '0'; -- Bad Address, no busy
  sig_rdata <= 
    data_rdata when data_rcs = '1' else
    cmd_rdata when cmd_rcs = '1' else
    cfg_rdata when cfg_rcs = '1' else
    prescaler_rdata when prescaler_rcs = '1' else
    (others => '0'); -- Bad Address, return 0
end architecture rtl;
