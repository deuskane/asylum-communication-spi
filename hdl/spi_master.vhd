-------------------------------------------------------------------------------
-- Title      : SPI_master
-- Project    : PicoSOC
-------------------------------------------------------------------------------
-- File       : SPI_master.vhd
-- Author     : Mathieu Rosiere
-- Company    : 
-- Created    : 2025-05-17
-- Last update: 2025-06-25
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
-- It's a SPI_master component
-------------------------------------------------------------------------------
-- Copyright (c) 2025
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2025-05-17  0.1      mrosiere Created
-------------------------------------------------------------------------------

library IEEE;
use     IEEE.STD_LOGIC_1164.ALL;
use     IEEE.numeric_std.ALL;
library work;
use     work.math_pkg.all;
 
entity spi_master is
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

    -- SPI Interface
    sclk_o               : out std_logic;
    sclk_oe_o            : out std_logic;
    cs_b_o               : out std_logic;
    cs_b_oe_o            : out std_logic;
    mosi_o               : out std_logic;
    mosi_oe_o            : out std_logic;
    miso_i               : in  std_logic
    );
end entity spi_master;
 
architecture rtl of spi_master is
 
    type   state_t is (IDLE, START, TRANSFER, POSTAMBLE, DONE);
    signal state_r            : state_t;

    signal state_is_IDLE      : std_logic;
    signal state_is_START     : std_logic;
    signal state_is_TRANSFER  : std_logic;
    signal state_is_POSTAMBLE : std_logic;
    signal state_is_DONE      : std_logic;

    signal sclk_r             : std_logic;
    signal sclk_oe_r          : std_logic;
    signal mosi_r             : std_logic;
    signal mosi_oe_r          : std_logic;
    signal cs_b_r             : std_logic;
    signal cs_b_oe_r          : std_logic;
    signal prescaler_cnt_r    : unsigned (PRESCALER_WIDTH-1 downto 0);
    signal prescaler_is_min   : std_logic;
    signal bit_sample         : std_logic;
    signal bit_shift          : std_logic;
    signal cnt_bit_r          : unsigned (3 downto 0);
    signal cnt_byte_r         : unsigned (cmd_nb_bytes_i'range);
    signal data_r             : std_logic_vector(8-1 downto 0);
    signal tx_tready_r        : std_logic;
    signal rx_tdata_r         : std_logic_vector(8-1 downto 0);
    signal rx_tvalid_r        : std_logic;
    signal cmd_tready_r       : std_logic;
    signal cmd_tlast_r        : std_logic;
    signal cmd_enable_rx_r    : std_logic;
    signal cmd_enable_tx_r    : std_logic;
    signal cmd_nb_bytes_r     : unsigned (cmd_nb_bytes_i'range);

    signal cycle_phase_r      : std_logic;
    signal cycle_phase0_r     : std_logic;
    signal cycle_phase1_r     : std_logic;
begin

  -----------------------------------------------------------------------------
  -- Prescaler
  -----------------------------------------------------------------------------
  --                     
  -- Prescaler counter   X 3 X 2 X 1 X 0 X 3 X 2 X 1 X 0 X 3 X 2
  --                                  ___             ___
  -- Prescaler min      _____________/   \___________/   \______
  --                                      ________________
  -- cycle_phase_r          _____________/                \_____
  --                                     ___                
  -- cycle_phase0_r        _____________/   \______________________
  --                                                     ___
  -- cycle_phase1_r        _____________________________/   \______
                                          
  -- In this architecture, SAMPLE & SHIFT phase is fix.
  -- SCLK Depend of this signals

  -- The prescaler is "free-running".
  -- TODO : reduce the power cumsoption if active only during transfert
  process(clk_i,arst_b_i)
  begin
    if arst_b_i = '0'
    then
      prescaler_cnt_r  <= (others => '0');

      cycle_phase_r    <= '0';
      cycle_phase0_r   <= '0';
      cycle_phase1_r   <= '0';
      
    elsif rising_edge(clk_i)
    then
      cycle_phase0_r   <= '0';
      cycle_phase1_r   <= '0';

      if prescaler_is_min = '1'
      then
        prescaler_cnt_r <= unsigned(cfg_prescaler_ratio_i);
        cycle_phase_r   <= not cycle_phase_r;
        cycle_phase0_r  <= '1' when cycle_phase_r='0' else
                           '0';
        cycle_phase1_r  <= '1' when cycle_phase_r='1' else
                           '0';
      else
        prescaler_cnt_r <= prescaler_cnt_r - 1;
      end if;
    end if;
  end process;

  prescaler_is_min <= '1' when unsigned(prescaler_cnt_r) = 0 else
                      '0';
                                          
  bit_sample       <= cycle_phase0_r;
  bit_shift        <= cycle_phase1_r;
  
  -----------------------------------------------------------------------------
  -- FSM
  -----------------------------------------------------------------------------
  process(clk_i,arst_b_i)
  begin
    if arst_b_i = '0'
    then
      state_r     <= IDLE;
      cs_b_r      <= '1'; -- CS Inactive
      cs_b_oe_r   <= '0'; -- Inactive pad
      sclk_r      <= '0';
      sclk_oe_r   <= '0'; -- Inactive pad
      mosi_r      <= '0';
      mosi_oe_r   <= '0'; -- Inactive pad
      tx_tready_r <= '0'; -- Never Ready during reset
      rx_tvalid_r <= '0'; -- Never Valid during reset (compliance with AXI-STREAM Protocol Specification)
      cmd_tready_r<= '0'; -- Never Ready during reset
      cnt_bit_r   <= (others => '0');
      cnt_byte_r  <= (others => '0');
      rx_tdata_r  <= (others => '0');
      
    elsif rising_edge(clk_i)
    then
      cs_b_oe_r   <= '1'; -- Active pad
      sclk_oe_r   <= '1'; -- Active pad
      cmd_tready_r<= '0'; -- tready is always reset because tready is set to 1 when tvalid is 1
      tx_tready_r <= '0'; -- tready is always reset because tready is set to 1 when tvalid is 1

      -- RX FIFO Managment
      if (rx_tvalid_r = '1' and rx_tready_i = '1')
      then
        rx_tvalid_r <= '0';
      end if;
                                          
      case state_r is
        -----------------------------------------------------------------------
        -- IDLE State
        -- In IDLE State, no SPI transmision (CS_B = 1)
        -- Wait New Command from AXIS
        -----------------------------------------------------------------------
        when IDLE =>
                                         
          -- Wait to Receive new command
          if cmd_tvalid_i = '1'
          then
            -- Ack the axistream transfert
            cmd_tready_r        <= '1';
            -- Save the Command
            cmd_tlast_r         <= cmd_tlast_i        ;
            cmd_enable_rx_r     <= cmd_enable_rx_i    ;
            cmd_enable_tx_r     <= cmd_enable_tx_i    ;
            cmd_nb_bytes_r      <= unsigned(cmd_nb_bytes_i);

            if (cmd_tlast_i     = '1' and
                cmd_enable_rx_i = '0' and
                cmd_enable_tx_i = '0')
            then
              -- STOP SPI Transaction
              state_r            <= DONE;
            else
              -- START SPI Transaction
              state_r            <= START;
            end if;
          end if;
          
        -----------------------------------------------------------------------
        -- START State
        -- The set the CS_B
        -- Depending the Command, Active the MOSI and the Wait the TX FIFO
        -----------------------------------------------------------------------
        when START =>
          if (bit_sample = '1')
          then
            -- CS_B is active
            cs_b_r    <= '0';
            -- Reset the counter bit
            cnt_bit_r <= (others => '0');

            -- Need TX ? Active MOSI oe pad
            if cmd_enable_tx_r = '1'
            then
              -- Need TX
              --  * Active PAD
              --  * Wait Data
              
              mosi_oe_r <= '1'; -- Active pad
              
              -- Wait TX Data
              if tx_tvalid_i = '1'
              then
                state_r   <= TRANSFER;
                
                -- Ack the axistream transfert
                tx_tready_r <= '1';              
                -- Save the Data
                data_r      <= tx_tdata_i;
              end if;
            else
              -- Don't Need TX
              --  * Disable PAD
              
              mosi_oe_r <= '0'; -- Inactive pad
              state_r   <= TRANSFER;
            end if;
          end if;

        -----------------------------------------------------------------------
        -- TRANSFERT State
        -- Send bit per bit the data (MSB First)
        -- 
        -----------------------------------------------------------------------
        when TRANSFER =>
          -- Bit Shift Phase
          if (bit_shift = '1')
          then
            -- MSB First
            mosi_r    <= data_r(7);

            -- Special Case :
            -- If CPHA = 0, then sample into the first clock edge
            -- So shift the clock
            if not (cfg_cpha_i = '0' and cnt_bit_r = 0)
            then
              sclk_r    <= not sclk_r;
            end if;

            -- If CPHA = 0, then the clock is shifted, then missing one edge
            if ((cfg_cpha_i = '0') and (cnt_bit_r = 8))
            then
              sclk_r    <= not sclk_r;
              state_r   <= POSTAMBLE;
            end if;
            
          end if;
          
          -- Bit Sample Phase
          if (bit_sample = '1')
          then
            sclk_r    <= not sclk_r;
            data_r    <= data_r(6 downto 0) & miso_i;
            cnt_bit_r <= cnt_bit_r + 1;

            if ((cfg_cpha_i = '1') and (cnt_bit_r = 7))
            then
              state_r   <= POSTAMBLE;
            end if;
            
          end if;

        -----------------------------------------------------------------------
        -- POSTAMBLE State
        -- Write in RX fifo (if possible)
        -- Check if last word or stop transfert  
        -----------------------------------------------------------------------
        when POSTAMBLE =>
          -- Push in fifo rx
          if not ((cmd_enable_rx_r = '1') and rx_tvalid_r = '1')
          then
            -- WARNING : OVERWRITE FIFO
            if (cmd_enable_rx_r = '1')
            then
              rx_tvalid_r <= '1'; -- Valid
              rx_tdata_r  <= data_r;
            end if;

            -- Last BYTE ?
            if (cnt_byte_r = cmd_nb_bytes_r)
            then
              cnt_byte_r   <= (others => '0');

              -- After byte disable cs or not
              if (cmd_tlast_r = '1')
              then
                -- Finish Transaction, CS go to inactive
                state_r      <= DONE;
              else
                -- Finish Transfer, continue transaction (CS is again active) and wait Command
                state_r      <= IDLE;
              end if;

            else
              -- Not Last Byte, continue transfert
              cnt_byte_r  <= cnt_byte_r+1;
              state_r     <= START;
            end if;
          end if;     
        -----------------------------------------------------------------------
        -- DONE State
        -- Unset the CS_B
        -----------------------------------------------------------------------
        when DONE =>
          if (bit_sample = '1')
          then
            cs_b_r      <= '1';
            mosi_oe_r   <= '0';
            state_r     <= IDLE;
          end if;
      end case;

    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Debug State
  -----------------------------------------------------------------------------
  state_is_IDLE      <= '1' when state_r = IDLE      else '0';
  state_is_START     <= '1' when state_r = START     else '0';
  state_is_TRANSFER  <= '1' when state_r = TRANSFER  else '0';
  state_is_POSTAMBLE <= '1' when state_r = POSTAMBLE else '0';
  state_is_DONE      <= '1' when state_r = DONE      else '0';

  -----------------------------------------------------------------------------
  -- Output assignments
  -----------------------------------------------------------------------------
  sclk_o       <= sclk_r xor cfg_cpol_i; -- need cgate
  mosi_o       <= mosi_r;
  cs_b_o       <= cs_b_r;
               
  sclk_oe_o    <= sclk_oe_r;
  mosi_oe_o    <= mosi_oe_r;
  cs_b_oe_o    <= cs_b_oe_r;
               
  tx_tready_o  <= tx_tready_r;
  rx_tdata_o   <= rx_tdata_r ;
  rx_tvalid_o  <= rx_tvalid_r;
  cmd_tready_o <= cmd_tready_r;
end architecture rtl;
 
