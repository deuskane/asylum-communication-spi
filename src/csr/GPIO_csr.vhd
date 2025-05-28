-- Generated VHDL Module for GPIO


library IEEE;
use     IEEE.STD_LOGIC_1164.ALL;
use     IEEE.NUMERIC_STD.ALL;

library work;
use     work.GPIO_csr_pkg.ALL;
library work;
use     work.pbi_pkg.all;

--==================================
-- Module      : GPIO
-- Description : CSR for General Purpose I/O
-- Width       : 8
--==================================
entity GPIO_registers is
  generic (
    DATA_OE_INIT : std_logic_vector -- Direction of the IO after a reset
  );
  port (
    -- Clock and Reset
    clk_i      : in  std_logic;
    arst_b_i   : in  std_logic;
    -- Bus
    pbi_ini_i  : in  pbi_ini_t;
    pbi_tgt_o  : out pbi_tgt_t;
    -- CSR
    sw2hw_o    : out GPIO_sw2hw_t;
    hw2sw_i    : in  GPIO_hw2sw_t
  );
end entity GPIO_registers;

architecture rtl of GPIO_registers is

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

  constant INIT_data_oe : std_logic_vector(8-1 downto 0) :=
             DATA_OE_INIT -- value
           ;
  signal   data_oe_wcs       : std_logic;
  signal   data_oe_we        : std_logic;
  signal   data_oe_wdata     : std_logic_vector(8-1 downto 0);
  signal   data_oe_wdata_sw  : std_logic_vector(8-1 downto 0);
  signal   data_oe_wdata_hw  : std_logic_vector(8-1 downto 0);
  signal   data_oe_wbusy     : std_logic;

  signal   data_oe_rcs       : std_logic;
  signal   data_oe_re        : std_logic;
  signal   data_oe_rdata     : std_logic_vector(8-1 downto 0);
  signal   data_oe_rdata_sw  : std_logic_vector(8-1 downto 0);
  signal   data_oe_rdata_hw  : std_logic_vector(8-1 downto 0);
  signal   data_oe_rbusy     : std_logic;

  constant INIT_data_in : std_logic_vector(8-1 downto 0) :=
             "00000000" -- value
           ;
  signal   data_in_wcs       : std_logic;
  signal   data_in_we        : std_logic;
  signal   data_in_wdata     : std_logic_vector(8-1 downto 0);
  signal   data_in_wdata_sw  : std_logic_vector(8-1 downto 0);
  signal   data_in_wdata_hw  : std_logic_vector(8-1 downto 0);
  signal   data_in_wbusy     : std_logic;

  signal   data_in_rcs       : std_logic;
  signal   data_in_re        : std_logic;
  signal   data_in_rdata     : std_logic_vector(8-1 downto 0);
  signal   data_in_rdata_sw  : std_logic_vector(8-1 downto 0);
  signal   data_in_rdata_hw  : std_logic_vector(8-1 downto 0);
  signal   data_in_rbusy     : std_logic;

  constant INIT_data_out : std_logic_vector(8-1 downto 0) :=
             "00000000" -- value
           ;
  signal   data_out_wcs       : std_logic;
  signal   data_out_we        : std_logic;
  signal   data_out_wdata     : std_logic_vector(8-1 downto 0);
  signal   data_out_wdata_sw  : std_logic_vector(8-1 downto 0);
  signal   data_out_wdata_hw  : std_logic_vector(8-1 downto 0);
  signal   data_out_wbusy     : std_logic;

  signal   data_out_rcs       : std_logic;
  signal   data_out_re        : std_logic;
  signal   data_out_rdata     : std_logic_vector(8-1 downto 0);
  signal   data_out_rdata_sw  : std_logic_vector(8-1 downto 0);
  signal   data_out_rdata_hw  : std_logic_vector(8-1 downto 0);
  signal   data_out_rbusy     : std_logic;

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
  -- Description : data - with data_oe mask apply
  -- Address     : 0x0
  -- Width       : 8
  -- Sw Access   : rw
  -- Hw Access   : rw
  -- Hw Type     : ext
  --==================================
  --==================================
  -- Field       : value
  -- Description : Data with data_oe with mask apply
  -- Width       : 8
  --==================================


    data_rcs     <= '1' when     (sig_raddr(GPIO_ADDR_WIDTH-1 downto 0) = std_logic_vector(to_unsigned(0,GPIO_ADDR_WIDTH))) else '0';
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

    data_wcs      <= '0';
    data_we       <= '0';
    data_wbusy    <= '0';
    data_wdata    <= (others=>'0');
    data_wdata_sw <= (others=>'0');
    data_wdata_hw(7 downto 0) <= hw2sw_i.data.value; -- value
    sw2hw_o.data.value <= data_rdata_hw(7 downto 0); -- value

    ins_data : entity work.csr_ext(rtl)
      generic map
        (WIDTH         => 8
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
        ,hw_wd_i       => data_wdata_hw
        ,hw_rd_o       => data_rdata_hw
        ,hw_we_i       => hw2sw_i.data.we
        ,hw_sw_re_o    => sw2hw_o.data.re
        ,hw_sw_we_o      => open
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
    sw2hw_o.data.re <= '0';
  end generate gen_data_b;

  gen_data_oe: if (True)
  generate
  --==================================
  -- Register    : data_oe
  -- Description : GPIO Direction
  -- Address     : 0x1
  -- Width       : 8
  -- Sw Access   : rw
  -- Hw Access   : ro
  -- Hw Type     : reg
  --==================================
  --==================================
  -- Field       : value
  -- Description : GPIO Direction : 0 input, 1 output
  -- Width       : 8
  --==================================


    data_oe_rcs     <= '1' when     (sig_raddr(GPIO_ADDR_WIDTH-1 downto 0) = std_logic_vector(to_unsigned(1,GPIO_ADDR_WIDTH))) else '0';
    data_oe_re      <= sig_rcs and sig_re and data_oe_rcs;
    data_oe_rdata   <= (
      0 => data_oe_rdata_sw(0), -- value(0)
      1 => data_oe_rdata_sw(1), -- value(1)
      2 => data_oe_rdata_sw(2), -- value(2)
      3 => data_oe_rdata_sw(3), -- value(3)
      4 => data_oe_rdata_sw(4), -- value(4)
      5 => data_oe_rdata_sw(5), -- value(5)
      6 => data_oe_rdata_sw(6), -- value(6)
      7 => data_oe_rdata_sw(7), -- value(7)
      others => '0');

    data_oe_wcs     <= '1' when       (sig_waddr(GPIO_ADDR_WIDTH-1 downto 0) = std_logic_vector(to_unsigned(1,GPIO_ADDR_WIDTH)))   else '0';
    data_oe_we      <= sig_wcs and sig_we and data_oe_wcs;
    data_oe_wdata   <= sig_wdata;
    data_oe_wdata_sw(7 downto 0) <= data_oe_wdata(7 downto 0); -- value
    sw2hw_o.data_oe.value <= data_oe_rdata_hw(7 downto 0); -- value

    ins_data_oe : entity work.csr_reg(rtl)
      generic map
        (WIDTH         => 8
        ,INIT          => INIT_data_oe
        ,MODEL         => "rw"
        )
      port map
        (clk_i         => clk_i
        ,arst_b_i      => arst_b_i
        ,sw_wd_i       => data_oe_wdata_sw
        ,sw_rd_o       => data_oe_rdata_sw
        ,sw_we_i       => data_oe_we
        ,sw_re_i       => data_oe_re
        ,sw_rbusy_o    => data_oe_rbusy
        ,sw_wbusy_o    => data_oe_wbusy
        ,hw_wd_i       => (others => '0')
        ,hw_rd_o       => data_oe_rdata_hw
        ,hw_we_i       => '0'
        ,hw_sw_re_o    => sw2hw_o.data_oe.re
        ,hw_sw_we_o    => sw2hw_o.data_oe.we
        );

  end generate gen_data_oe;

  gen_data_oe_b: if not (True)
  generate
    data_oe_rcs     <= '0';
    data_oe_rbusy   <= '0';
    data_oe_rdata   <= (others => '0');
    data_oe_wcs      <= '0';
    data_oe_wbusy    <= '0';
    sw2hw_o.data_oe.value <= DATA_OE_INIT;
    sw2hw_o.data_oe.re <= '0';
    sw2hw_o.data_oe.we <= '0';
  end generate gen_data_oe_b;

  gen_data_in: if (True)
  generate
  --==================================
  -- Register    : data_in
  -- Description : GPIO Input
  -- Address     : 0x2
  -- Width       : 8
  -- Sw Access   : ro
  -- Hw Access   : rw
  -- Hw Type     : reg
  --==================================
  --==================================
  -- Field       : value
  -- Description : Input Data of GPIO
  -- Width       : 8
  --==================================


    data_in_rcs     <= '1' when     (sig_raddr(GPIO_ADDR_WIDTH-1 downto 0) = std_logic_vector(to_unsigned(2,GPIO_ADDR_WIDTH))) else '0';
    data_in_re      <= sig_rcs and sig_re and data_in_rcs;
    data_in_rdata   <= (
      0 => data_in_rdata_sw(0), -- value(0)
      1 => data_in_rdata_sw(1), -- value(1)
      2 => data_in_rdata_sw(2), -- value(2)
      3 => data_in_rdata_sw(3), -- value(3)
      4 => data_in_rdata_sw(4), -- value(4)
      5 => data_in_rdata_sw(5), -- value(5)
      6 => data_in_rdata_sw(6), -- value(6)
      7 => data_in_rdata_sw(7), -- value(7)
      others => '0');

    data_in_wcs      <= '0';
    data_in_we       <= '0';
    data_in_wbusy    <= '0';
    data_in_wdata    <= (others=>'0');
    data_in_wdata_sw <= (others=>'0');
    data_in_wdata_hw(7 downto 0) <= hw2sw_i.data_in.value; -- value
    sw2hw_o.data_in.value <= data_in_rdata_hw(7 downto 0); -- value

    ins_data_in : entity work.csr_reg(rtl)
      generic map
        (WIDTH         => 8
        ,INIT          => INIT_data_in
        ,MODEL         => "ro"
        )
      port map
        (clk_i         => clk_i
        ,arst_b_i      => arst_b_i
        ,sw_wd_i       => data_in_wdata_sw
        ,sw_rd_o       => data_in_rdata_sw
        ,sw_we_i       => data_in_we
        ,sw_re_i       => data_in_re
        ,sw_rbusy_o    => data_in_rbusy
        ,sw_wbusy_o    => data_in_wbusy
        ,hw_wd_i       => data_in_wdata_hw
        ,hw_rd_o       => data_in_rdata_hw
        ,hw_we_i       => hw2sw_i.data_in.we
        ,hw_sw_re_o    => sw2hw_o.data_in.re
        ,hw_sw_we_o      => open
        );

  end generate gen_data_in;

  gen_data_in_b: if not (True)
  generate
    data_in_rcs     <= '0';
    data_in_rbusy   <= '0';
    data_in_rdata   <= (others => '0');
    data_in_wcs      <= '0';
    data_in_wbusy    <= '0';
    sw2hw_o.data_in.value <= "00000000";
    sw2hw_o.data_in.re <= '0';
  end generate gen_data_in_b;

  gen_data_out: if (True)
  generate
  --==================================
  -- Register    : data_out
  -- Description : GPIO Output
  -- Address     : 0x3
  -- Width       : 8
  -- Sw Access   : rw
  -- Hw Access   : ro
  -- Hw Type     : reg
  --==================================
  --==================================
  -- Field       : value
  -- Description : Output Data of GPIO
  -- Width       : 8
  --==================================


    data_out_rcs     <= '1' when     (sig_raddr(GPIO_ADDR_WIDTH-1 downto 0) = std_logic_vector(to_unsigned(3,GPIO_ADDR_WIDTH))) else '0';
    data_out_re      <= sig_rcs and sig_re and data_out_rcs;
    data_out_rdata   <= (
      0 => data_out_rdata_sw(0), -- value(0)
      1 => data_out_rdata_sw(1), -- value(1)
      2 => data_out_rdata_sw(2), -- value(2)
      3 => data_out_rdata_sw(3), -- value(3)
      4 => data_out_rdata_sw(4), -- value(4)
      5 => data_out_rdata_sw(5), -- value(5)
      6 => data_out_rdata_sw(6), -- value(6)
      7 => data_out_rdata_sw(7), -- value(7)
      others => '0');

    data_out_wcs     <= '1' when       (sig_waddr(GPIO_ADDR_WIDTH-1 downto 0) = std_logic_vector(to_unsigned(0,GPIO_ADDR_WIDTH)))   or (sig_waddr(GPIO_ADDR_WIDTH-1 downto 0) = std_logic_vector(to_unsigned(3,GPIO_ADDR_WIDTH)))   else '0';
    data_out_we      <= sig_wcs and sig_we and data_out_wcs;
    data_out_wdata   <= sig_wdata;
    data_out_wdata_sw(7 downto 0) <= data_out_wdata(7 downto 0); -- value
    sw2hw_o.data_out.value <= data_out_rdata_hw(7 downto 0); -- value

    ins_data_out : entity work.csr_reg(rtl)
      generic map
        (WIDTH         => 8
        ,INIT          => INIT_data_out
        ,MODEL         => "rw"
        )
      port map
        (clk_i         => clk_i
        ,arst_b_i      => arst_b_i
        ,sw_wd_i       => data_out_wdata_sw
        ,sw_rd_o       => data_out_rdata_sw
        ,sw_we_i       => data_out_we
        ,sw_re_i       => data_out_re
        ,sw_rbusy_o    => data_out_rbusy
        ,sw_wbusy_o    => data_out_wbusy
        ,hw_wd_i       => (others => '0')
        ,hw_rd_o       => data_out_rdata_hw
        ,hw_we_i       => '0'
        ,hw_sw_re_o    => sw2hw_o.data_out.re
        ,hw_sw_we_o    => sw2hw_o.data_out.we
        );

  end generate gen_data_out;

  gen_data_out_b: if not (True)
  generate
    data_out_rcs     <= '0';
    data_out_rbusy   <= '0';
    data_out_rdata   <= (others => '0');
    data_out_wcs      <= '0';
    data_out_wbusy    <= '0';
    sw2hw_o.data_out.value <= "00000000";
    sw2hw_o.data_out.re <= '0';
    sw2hw_o.data_out.we <= '0';
  end generate gen_data_out_b;

  sig_wbusy <= 
    data_wbusy when data_wcs = '1' else
    data_oe_wbusy when data_oe_wcs = '1' else
    data_in_wbusy when data_in_wcs = '1' else
    data_out_wbusy when data_out_wcs = '1' else
    '0'; -- Bad Address, no busy
  sig_rbusy <= 
    data_rbusy when data_rcs = '1' else
    data_oe_rbusy when data_oe_rcs = '1' else
    data_in_rbusy when data_in_rcs = '1' else
    data_out_rbusy when data_out_rcs = '1' else
    '0'; -- Bad Address, no busy
  sig_rdata <= 
    data_rdata when data_rcs = '1' else
    data_oe_rdata when data_oe_rcs = '1' else
    data_in_rdata when data_in_rcs = '1' else
    data_out_rdata when data_out_rcs = '1' else
    (others => '0'); -- Bad Address, return 0
end architecture rtl;
