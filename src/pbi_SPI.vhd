-------------------------------------------------------------------------------
-- Title      : pbi_GPIO
-- Project    : PicoSOC
-------------------------------------------------------------------------------
-- File       : pbi_GPIO.vhd
-- Author     : Mathieu Rosiere
-- Company    : 
-- Created    : 2017-03-30
-- Last update: 2025-05-14
-- Platform   : 
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- Copyright (c) 2017
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2025-05-14  0.3      mrosiere Delete parameters DATA_OE_FORCE,
--                               csr use DATA_OE_INIT
-- 2025-03-05  0.2      mrosiere use csr from regtool
-- 2017-03-30  0.1      mrosiere Created
-------------------------------------------------------------------------------

library IEEE;
use     IEEE.STD_LOGIC_1164.ALL;
use     IEEE.numeric_std.ALL;
library work;
library work;
use     work.pbi_pkg.all;
use     work.GPIO_csr_pkg.all;

entity pbi_GPIO is
  generic(
    NB_IO            : natural:=8;     -- Number of IO. Must be <= SIZE_DATA
    DATA_OE_INIT     : std_logic_vector; -- Direction of the IO after a reset
    IT_ENABLE        : boolean:=false    -- GPIO can generate interruption
    );
  port   (
    clk_i            : in    std_logic;
    cke_i            : in    std_logic;
    arstn_i          : in    std_logic; -- asynchronous reset

    -- Bus
    pbi_ini_i        : in    pbi_ini_t;
    pbi_tgt_o        : out   pbi_tgt_t;
    
    -- To/From IO
    data_i           : in    std_logic_vector (NB_IO-1     downto 0);
    data_o           : out   std_logic_vector (NB_IO-1     downto 0);
    data_oe_o        : out   std_logic_vector (NB_IO-1     downto 0);

    -- To/From IT Ctrl
    interrupt_o      : out   std_logic;
    interrupt_ack_i  : in    std_logic
    );

end entity pbi_GPIO;

architecture rtl of pbi_GPIO is

  signal sw2hw                  : GPIO_sw2hw_t;
  signal hw2sw                  : GPIO_hw2sw_t;

begin  -- architecture rtl

  ins_csr : entity work.GPIO_registers(rtl)
  generic map(
    DATA_OE_INIT     => DATA_OE_INIT 
    )
  port map(
    clk_i     => clk_i           ,
    arst_b_i  => arstn_i         ,
    pbi_ini_i => pbi_ini_i       ,
    pbi_tgt_o => pbi_tgt_o       ,
    sw2hw_o   => sw2hw           ,
    hw2sw_i   => hw2sw   
  );

  ins_GPIO : entity work.GPIO(rtl)
  generic map(
    NB_IO            => NB_IO          ,
    IT_ENABLE        => IT_ENABLE    
    )
  port map(
    clk_i            => clk_i          ,
    cke_i            => cke_i          ,
    arstn_i          => arstn_i        ,
    data_i           => data_i         ,
    data_o           => data_o         ,
    data_oe_o        => data_oe_o      ,
    interrupt_o      => interrupt_o    ,
    interrupt_ack_i  => interrupt_ack_i,
    sw2hw_i          => sw2hw          ,
    hw2sw_o          => hw2sw 
    );
  
end architecture rtl;
