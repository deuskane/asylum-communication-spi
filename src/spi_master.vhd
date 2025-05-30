-------------------------------------------------------------------------------
-- Title      : SPI_master
-- Project    : PicoSOC
-------------------------------------------------------------------------------
-- File       : SPI_master.vhd
-- Author     : Mathieu Rosiere
-- Company    : 
-- Created    : 2025-05-17
-- Last update: 2025-05-30
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
 
entity spi_master is
  generic (
    PRESCALER_WIDTH : integer := 8
    );
  port (
    -- Clock & Reset
    clk_i           : in  std_logic;
    arst_b_i        : in  std_logic;

    -- Data From AXI Stream
    tx_tdata_i      : in  std_logic_vector(8-1 downto 0);
    tx_tvalid_i     : in  std_logic;
    tx_tready_o     : out std_logic;

    -- Data From AXI Stream
    rx_tdata_o      : out std_logic_vector(8-1 downto 0);
    rx_tvalid_o     : out std_logic;
    rx_tready_i     : in  std_logic;
    
    -- Configuration
    cpol_i          : in  std_logic;
    cpha_i          : in  std_logic;
    prescaler_i     : in  std_logic_vector(PRESCALER_WIDTH-1 downto 0);

    -- SPI Interface
    sclk_o          : out std_logic;
    cs_b_o          : out std_logic;
    mosi_o          : out std_logic;
    miso_i          : in  std_logic
    );
end entity spi_master;
 
architecture rtl of spi_master is
 
    type   state_t is (IDLE, START, TRANSFER, DONE);
    signal state_r            : state_t;

    signal sclk_r             : std_logic;
    signal mosi_r             : std_logic;
    signal cs_b_r             : std_logic;
    signal prescaler_cnt_r    : unsigned(PRESCALER_WIDTH-1 downto 0);
    signal prescaler_is_min   : std_logic;
    signal bit_sample         : std_logic;
    signal bit_shift          : std_logic;
    signal bit_cnt_r          : unsigned(3 downto 0);
    signal data_r             : std_logic_vector(8-1 downto 0);
    signal tx_tready_r        : std_logic;
    signal rx_tdata_r         : std_logic_vector(8-1 downto 0);
    signal rx_tvalid_r        : std_logic;

    signal cycle_phase_r      : std_logic;
    signal cycle_phase0_r     : std_logic;
    signal cycle_phase1_r     : std_logic;
begin

  -----------------------------------------------------------------------------
  -- Prescaler
  -----------------------------------------------------------------------------
  -- SCLK is divide by 2*(prescaler_i+1)
  -- SCLK is the ouput of Register
  
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
        prescaler_cnt_r <= unsigned(prescaler_i);
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

  --                     
  -- Prescaler counter   X 3 X 2 X 1 X 0 X 3 X 2 X 1 X 0 X 3 X 2
  --                                  ___             ___
  -- Prescaler min      _____________/   \___________/   \______
  --                                      ________________
  -- SCLK                   _____________/                \_____
  --                                                  ___
  -- Bit Sample         _____________________________/   \______
  --                                  ___                
  -- Bit Shift          _____________/   \______________________

  
  prescaler_is_min <= '1' when unsigned(prescaler_cnt_r) = 0 else
                      '0';

  -- CPHA 0 : sampled in first  edge
  -- CPHA 1 : sampled in second edge
  
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
      sclk_r       <= cpol_i;
      mosi_r      <= '0';
      tx_tready_r <= '1'; -- Always Ready during reset
      bit_cnt_r   <= (others => '0');
      rx_tdata_r  <= (others => '0');
      rx_tvalid_r <= '0';

    elsif rising_edge(clk_i)
    then
      case state_r is
        -----------------------------------------------------------------------
        -- IDLE State
        -- In IDLE State, no SPI transmision (CS_B = 1)
        -- Wait New transaction from AXIS
        -----------------------------------------------------------------------
        when IDLE =>
          -- Wait to Receive new request
          if tx_tvalid_i = '1'
          then
            -- Load data in TX buffer
            data_r      <= tx_tdata_i;

            -- Ack the axistream transfert
            tx_tready_r <= '0';
            state_r     <= START;
          end if;
          
        -----------------------------------------------------------------------
        -- START State
        -- The set the CS_B
        -----------------------------------------------------------------------
        when START =>
          if (bit_sample = '1')
          then
            cs_b_r    <= '0';
            state_r   <= TRANSFER;
            bit_cnt_r <= (others => '0');
          end if;
        when TRANSFER =>
          if (bit_cnt_r < 8)
          then
            if (bit_shift = '1')
            then
              mosi_r    <= data_r(7);


              if not (cpha_i = '0' and bit_cnt_r = 0)
              then
                sclk_r    <= not sclk_r;
              end if;
                
            end if;
            
            if (bit_sample = '1')
            then
              sclk_r    <= not sclk_r;
              data_r    <= data_r(6 downto 0) & miso_i;
              bit_cnt_r <= bit_cnt_r + 1;
            end if;
          else
            rx_tdata_r  <= data_r;
            rx_tvalid_r <= '1';
            state_r     <= DONE;
          end if;
          
        when DONE =>
          if (bit_shift = '1') and (cpha_i = '0')
          then
            sclk_r    <= not sclk_r;
          end if;


          if (bit_sample = '1')
          then
            cs_b_r      <= '1';
            tx_tready_r <= '1'; -- Ready
            state_r     <= IDLE;
          end if;
      end case;

      if (rx_tvalid_r = '1' and rx_tready_i = '1')
      then
        rx_tvalid_r <= '0';
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Output assignments
  -----------------------------------------------------------------------------
  sclk_o      <= sclk_r;
  mosi_o      <= mosi_r;
  cs_b_o      <= cs_b_r;

  tx_tready_o <= tx_tready_r;
  rx_tdata_o  <= rx_tdata_r ;
  rx_tvalid_o <= rx_tvalid_r;
end architecture rtl;
 
