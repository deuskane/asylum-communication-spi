-------------------------------------------------------------------------------
--  File Name: cy15b104qs.vhd
-------------------------------------------------------------------------------
--  Copyright (C) 2017-2019 Free Model Foundry; http://www.FreeModelFoundry.com
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License version 2 as
--  published by the Free Software Foundation.
--
--  MODIFICATION HISTORY:
--
--  version: |  author:     | mod date:   |  changes made:
--    V1.0     M.Stojanovic   17 Apr 13     Inital Release
--    V1.1     M.Stojanovic   17 Oct 18     Update to datasheet 002-18293 Rev. *B
--    V1.2     B.Barac        18 Nov 06     Update to datasheet 002-18293 Rev. *G
--                                           1. Removed WRR, and Added WRSR
--                                           2. Added Volotile registers
--                                           3. Latency cycle update
--                                           4. Timing changes
--                                           5. Fixed some bugs in read/write
--                                              from memory
--    V1.3     B.Barac        19 Dec 03     Changing code and header to alling with
--                                          cypress f-ram
--    V1.4     B.Barac        19 Dec 26     Update to datasheet 002-18293 Rev. *J
-------------------------------------------------------------------------------
--  PART DESCRIPTION:
--
--  Library:    FRAM
--  Technology: FRAM MEMORY
--  Part:       CY15B104QS
--
--   Description: 4 Megabit Serial F-RAM Memory
--
-------------------------------------------------------------------------------
--  Comments :
--      For correct simulation, simulator resolution should be set to 1 ps
--      A device ordering (trim) option determines whether a feature is enabled
--      or not, or provide relevant parameters:
--        -15th character in TimingModel determines if enhanced high
--         performance option is available
--            (0,2) General Market
--
--------------------------------------------------------------------------------
--  Known Bugs:
--
--------------------------------------------------------------------------------
LIBRARY IEEE;   USE IEEE.std_logic_1164.ALL;
                USE STD.textio.ALL;
--                USE IEEE.VITAL_timing.ALL;
--                USE IEEE.VITAL_primitives.ALL;
LIBRARY IEEE_dummy;
USE     IEEE_dummy.vital_timing.ALL;     
USE     IEEE_dummy.vital_primitives.ALL;                 

LIBRARY FMF;    USE FMF.gen_utils.ALL;
                USE FMF.conversions.ALL;
-------------------------------------------------------------------------------
-- ENTITY DECLARATION
-------------------------------------------------------------------------------
ENTITY cy15b104qs IS
    GENERIC (
    ---------------------------------------------------------------------------
    -- TIMING GENERICS:
    ---------------------------------------------------------------------------
        -- tipd delays: interconnect path delays (delay between components)
        --    There should be one for each IN or INOUT pin in the port list
        --    They are given default values of zero delay.
        tipd_SCK                : VitalDelayType01  := VitalZeroDelay01;
        tipd_SI                 : VitalDelayType01  := VitalZeroDelay01;
        tipd_SO                 : VitalDelayType01  := VitalZeroDelay01;
        tipd_CSNeg              : VitalDelayType01  := VitalZeroDelay01;
        tipd_RESETNeg           : VitalDelayType01  := VitalZeroDelay01;
        tipd_WPNeg              : VitalDelayType01  := VitalZeroDelay01;

        -- tpd delays: propagation delays (pin-to-pin delay within a component)
        tpd_SCK_SO              : VitalDelayType01Z := UnitDelay01Z; -- tV
        tpd_CSNeg_SO_normal_rd  : VitalDelayType01Z := UnitDelay01Z; -- tDIS
        tpd_CSNeg_SO_ddr_rd     : VitalDelayType01Z := UnitDelay01Z; -- tDIS

        -- tsetup values: setup times
        --   setup time is minimum time before the referent signal edge the
        --   input should be stable
        tsetup_CSNeg_SCK_normal_rd : VitalDelayType := UnitDelay; -- tCSS /
        tsetup_CSNeg_SCK_ddr_rd    : VitalDelayType := UnitDelay; -- tCSS /
        tsetup_SI_SCK_normal_rd    : VitalDelayType := UnitDelay; -- tSU:DAT /
        tsetup_SI_SCK_ddr_rd       : VitalDelayType := UnitDelay; -- tSU:DAT /
        tsetup_WPNeg_CSNeg         : VitalDelayType := UnitDelay; -- tWPS \
        tsetup_RESETNeg_CSNeg      : VitalDelayType := UnitDelay; -- tRS

        -- thold values: hold times
        --   hold time is minimum time the input should be present stable
        --   after the referent signal edge
        thold_CSNeg_SCK_normal_rd0 : VitalDelayType := UnitDelay; -- tCSH /
        thold_CSNeg_SCK_normal_rd3 : VitalDelayType := UnitDelay; -- tCSH /
        thold_SI_SCK_normal_rd    : VitalDelayType := UnitDelay; -- tHD:DAT /
        thold_CSNeg_SCK_ddr_rd    : VitalDelayType := UnitDelay; -- tCSH /
        thold_SI_SCK_ddr_rd       : VitalDelayType := UnitDelay; -- tHD:DAT /
        thold_WPNeg_CSNeg         : VitalDelayType := UnitDelay; -- tWPH /
        thold_CSNeg_RESETNeg      : VitalDelayType := UnitDelay; -- tRH

        --tpw values: pulse width
        tpw_SCK_normal_rd      : VitalDelayType := UnitDelay;
        tpw_SCK_ddr_rd         : VitalDelayType := UnitDelay;
        tpw_CSNeg_posedge      : VitalDelayType := UnitDelay; -- tCS
        tpw_CSNeg_rst_quad_posedge : VitalDelayType := UnitDelay; -- tCS
        tpw_CSNeg_wip_posedge  : VitalDelayType := UnitDelay; -- tCS
        tpw_RESETNeg_negedge   : VitalDelayType := UnitDelay; -- tRP
        tpw_RESETNeg_posedge   : VitalDelayType := UnitDelay; -- tRS

        -- tperiod min (calculated as 1/max freq)
        tperiod_SCK_normal_rd  : VitalDelayType := UnitDelay; --fSCK=108MHz
        tperiod_SCK_ddr_rd     : VitalDelayType := UnitDelay; --fSCK=54MHz

        -- tdevice values: values for internal delays
        --timing values that are internal to the model and not associated
        --with any port.
        -- VCC (min) to CS# Low
        tdevice_PU              : VitalDelayType := 450 us;  --tPU
        -- CRC setup time
        tdevice_CRCSETUP        : VitalDelayType := 440 ms;
        -- CRC suspend latency
        tdevice_CRCSL           : VitalDelayType := 100 us;   --tCRCSL
        -- CRC Resume to next suspend
        tdevice_CRCRL           : VitalDelayType := 100 us;
        -- RESET# Low to CS# Low
        tdevice_RPH             : VitalDelayType := 450 us;  --tRPH
        -- RESET# Low to CS# Low
        tdevice_SRESET            : VitalDelayType := 100 us;  --Sreset
        
        
        -- CS# High before HW Reset (Quad mode and Reset Feature are enabled)
        
        
        tdevice_CS_spi          : VitalDelayType := 40 ns;    -- SPI
        
        tdevice_CS_dpi          : VitalDelayType := 75 ns;    -- DPI & !mem
        
        tdevice_CS_dpi1         : VitalDelayType := 40 ns;    -- DPI & !xpi
        
        tdevice_CS_dpi2         : VitalDelayType := 55 ns;    -- DPI & xpi
        
        tdevice_CS_qpi          : VitalDelayType := 110 ns;   -- QPI & MEM = '0'
        
        tdevice_CS_qpi1         : VitalDelayType := 90 ns;    -- QPI & !xpi
        
        tdevice_CS_qpi2         : VitalDelayType := 110 ns;   -- QPI & xpi
        
        tdevice_CS_ddr          : VitalDelayType := 40 ns;    -- SPI DDR
        
        tdevice_CS_ddr_1        : VitalDelayType := 110 ns ;  -- QPI & MEM = '0' DDR
        
        tdevice_CS_ddr_2        : VitalDelayType := 90 ns;    -- QPI & !xpi DDR
        
        tdevice_CS_ddr_3        : VitalDelayType := 110 ns;   -- QPI & xpi DDR
        
--         tdevice_CS              : VitalDelayType := 110 ns;   --tCS
--         CS# High to Power Down Mode
        tdevice_DPD             : VitalDelayType := 3 us;   -- tDPD
        -- CS# High to StandBy mode without Electronic Signature read
        tdevice_RES             : VitalDelayType := 2 us;  --tRES
        -- CS# High to Hibernate Mode
        tdevice_HBN             : VitalDelayType := 3 us;   -- tHBN
        -- CS# Low to ready for access
        tdevice_REC             : VitalDelayType := 450 us;  --tREC
    ---------------------------------------------------------------------------
    -- CONTROL GENERICS:
    ---------------------------------------------------------------------------
        -- generic control parameters
        InstancePath      : STRING    := DefaultInstancePath;
        TimingChecksOn    : BOOLEAN   := DefaultTimingChecks;
        MsgOn             : BOOLEAN   := DefaultMsgOn;
        XOn               : BOOLEAN   := DefaultXon;
        -- memory file to be loaded
        mem_file_name     : STRING    := "cy15b104qs.mem";
        otp_file_name     : STRING    := "cy15b104qsOTP.mem";

        UserPreload       : BOOLEAN   := FALSE; --TRUE;
        LongTimming       : BOOLEAN   := TRUE;

        BootConfig        : BOOLEAN   := TRUE;

        -- For FMF SDF technology file usage
        TimingModel       : STRING
    );
    PORT (
        -- Data Inputs/Outputs
        SI                : INOUT std_ulogic := 'U'; -- serial data input/IO0
        SO                : INOUT std_ulogic := 'U'; -- serial data output/IO1
        -- Controls
        SCK               : IN    std_ulogic := 'U'; -- serial clock input
        CSNeg             : IN    std_ulogic := 'U'; -- chip select input
        WPNeg             : INOUT std_ulogic := 'U'; -- write protect input/IO2
        RESETNeg          : INOUT std_ulogic := 'U'  -- hardware reset pin/IO3
    );

    ATTRIBUTE VITAL_LEVEL0 of cy15b104qs : ENTITY IS TRUE;
END cy15b104qs;

-------------------------------------------------------------------------------
-- ARCHITECTURE DECLARATION
-------------------------------------------------------------------------------
ARCHITECTURE vhdl_behavioral_static_memory_allocation of cy15b104qs IS
    ATTRIBUTE VITAL_LEVEL0 OF
    vhdl_behavioral_static_memory_allocation : ARCHITECTURE IS TRUE;

    ---------------------------------------------------------------------------
    -- CONSTANT AND SIGNAL DECLARATION
    ---------------------------------------------------------------------------
    --Declaration of constants - memory characteristics
        -- The constant declared here are used to enable the creation of models
        -- of memories within a family with a minimum amount of editing

    CONSTANT PartID        : STRING  := "cy15b104qs";
    CONSTANT MaxData       : NATURAL := 16#FF#;        --255;
    CONSTANT MemSize       : NATURAL := 16#7FFFF#;
    CONSTANT SecNumUni     : NATURAL :=  255;
    CONSTANT SecSize       : NATURAL := 16#7FF#;
    CONSTANT AddrRANGE     : NATURAL := 16#7FFFF#;
    CONSTANT HiAddrBit     : NATURAL := 23;
    CONSTANT OTPSize       : NATURAL := 1023;
    CONSTANT OTPLoAddr     : NATURAL := 16#00#;
    CONSTANT OTPHiAddr     : NATURAL := 16#FF#;
    CONSTANT BYTE          : NATURAL := 8;

    --Manufacturer Identification
    CONSTANT Manuf_ID      : NATURAL := 16#01#;
    CONSTANT DeviceID      : NATURAL := 16#19#;
    --Electronic Signature
    CONSTANT ESignature    : NATURAL := 16#19#;
    --Device ID
    --Manufacturer Identification && Memory Type && Memory Capacity
    CONSTANT Jedec_ID      : NATURAL := 16#01#; -- first byte of Device ID
    CONSTANT DeviceID1     : NATURAL := 16#02#;
    CONSTANT DeviceID2     : NATURAL := 16#20#;
    CONSTANT ExtendedBytes : NATURAL := 16#4D#;
    CONSTANT ExtendedID    : NATURAL := 16#00#;
    CONSTANT DieRev        : NATURAL := 16#00#;
    CONSTANT MaskRev       : NATURAL := 16#00#;
    SIGNAL UID             :std_logic_vector(63 DOWNTO 0) := (OTHERS => '0');

    -- Declaration of signals that will hold the delayed values of ports
    SIGNAL SI_ipd          : std_ulogic := 'U';
    SIGNAL SO_ipd          : std_ulogic := 'U';
    SIGNAL SCK_ipd         : std_ulogic := 'U';
    SIGNAL CSNeg_ipd       : std_ulogic := 'U';
    SIGNAL RESETNeg_ipd    : std_ulogic := 'U';
    SIGNAL WPNeg_ipd       : std_ulogic := 'U';

    SIGNAL RESETNeg_pullup : std_ulogic := 'U';
    SIGNAL WPNeg_pullup    : std_ulogic := 'U';

    -- internal delays
    SIGNAL PU_in           : std_ulogic := '0';
    SIGNAL PU_out          : std_ulogic := '0';
    SIGNAL CRCSETUP_in     : std_ulogic := '0';
    SIGNAL CRCSETUP_out    : std_ulogic := '0';
    SIGNAL CRCSL_in        : std_ulogic := '0';
    SIGNAL CRCSL_out       : std_ulogic := '0';
    SIGNAL CRCRL_in        : std_ulogic := '0';
    SIGNAL CRCRL_out       : std_ulogic := '0';
    SIGNAL sSTART_T1       : std_ulogic := '0'; --Start TimeOut
    SIGNAL START_T1_in     : std_ulogic := '0';
    SIGNAL RPH_in          : std_ulogic := '0';
    SIGNAL RPH_out         : std_ulogic := '0';
    SIGNAL CS_in           : std_ulogic := '0';
    SIGNAL CS_out          : std_ulogic := '1';
    SIGNAL DPD_in          : std_ulogic := '0';
    SIGNAL DPD_out         : std_ulogic := '0';
    SIGNAL RES_in          : std_ulogic := '0';
    SIGNAL RES_out         : std_ulogic := '0';
    SIGNAL HBN_in          : std_ulogic := '0';
    SIGNAL HBN_out         : std_ulogic := '0';
    SIGNAL REC_in          : std_ulogic := '0';
    SIGNAL REC_out         : std_ulogic := '0';

    SIGNAL SECURE_OPN      : std_logic := '0';
    
    SIGNAL mode3                    : std_logic := '0';

    ---------------------------------------------------------------------------
    -- Memory data initial value.
    ---------------------------------------------------------------------------
    SHARED VARIABLE max_data     : NATURAL := 16#FF#;
    SHARED VARIABLE tdevice_CS  : time := 40 ns;
     

    SHARED    VARIABLE data_out       : std_logic_vector(7 downto 0);
   SHARED  VARIABLE Data_in_8   : std_logic_vector(7 downto 0) := (others => '1');

BEGIN
    ---------------------------------------------------------------------------
    -- Internal Delays
    ---------------------------------------------------------------------------
    -- Artificial VITAL primitives to incorporate internal delays
    -- Because a tdevice generics is used, there must be a VITAL_primitives
    -- assotiated with them
    CRCSETUP :VitalBuf(CRCSETUP_out,CRCSETUP_in,(tdevice_CRCSETUP,UnitDelay));
    CRCSL  : VitalBuf(CRCSL_out,   CRCSL_in,  (tdevice_CRCSL   ,UnitDelay));
    CRCRL  : VitalBuf(CRCRL_out,   CRCRL_in,  (tdevice_CRCRL   ,UnitDelay));
    RPH    : VitalBuf(RPH_out,     RPH_in,    (tdevice_RPH     ,UnitDelay));
--     CS     : VitalBuf(CS_out,      CS_in,     (tdevice_CS      ,UnitDelay));
    PU     : VitalBuf(PU_out,      PU_in,     (tdevice_PU      ,UnitDelay));
    DPD    : VitalBuf(DPD_out,     DPD_in,    (tdevice_DPD     ,UnitDelay));
    HBN    : VitalBuf(HBN_out,     HBN_in,    (tdevice_HBN     ,UnitDelay));

    ---------------------------------------------------------------------------
    -- Wire Delays
    ---------------------------------------------------------------------------
    WireDelay : BLOCK
    BEGIN

        w_1 : VitalWireDelay (SI_ipd,      SI,      tipd_SI);
        w_2 : VitalWireDelay (SO_ipd,      SO,      tipd_SO);
        w_3 : VitalWireDelay (SCK_ipd,     SCK,     tipd_SCK);
        w_4 : VitalWireDelay (CSNeg_ipd,   CSNeg,   tipd_CSNeg);
        w_5 : VitalWireDelay (RESETNeg_ipd,RESETNeg,tipd_RESETNeg);
        w_6 : VitalWireDelay (WPNeg_ipd,   WPNeg,   tipd_WPNeg);

    END BLOCK WireDelay;

    ---------------------------------------------------------------------------
    -- Main Behavior Block
    ---------------------------------------------------------------------------
    Behavior: BLOCK

        PORT (
            SIIn           : IN    std_ulogic := 'U';
            SIOut          : OUT   std_ulogic := 'U';
            SOIn           : IN    std_logic  := 'U';
            SOut           : OUT   std_logic  := 'U';
            SCK            : IN    std_ulogic := 'U';
            CSNeg          : IN    std_ulogic := 'U';
            WPNegIn        : IN    std_ulogic := 'U';
            WPNegOut       : OUT   std_ulogic := 'U';
            RESETNegIn     : IN    std_ulogic := 'U';
            RESETNegOut    : OUT   std_ulogic := 'U'
        );

        PORT MAP (
             SIIn       => SI_ipd,
             SIOut      => SI,
             SOIn       => SO_ipd,
             SOut       => SO,
             SCK        => SCK_ipd,
             CSNeg      => CSNeg_ipd,
             WPNegIn    => WPNeg_ipd,
             WPNegOut   => WPNeg,
             RESETNegIn  => RESETNeg_ipd,
             RESETNegOut => RESETNeg
        );

        -- State Machine : State_Type
        TYPE state_type IS (IDLE,
                            RESET_STATE,
                            WRITE_SR,
                            WRITE_ALL_REG,
                            OTP_PG,
                            CRC_Calc,
                            CRC_SUSP,
                            DP_DOWN,
                            HIBERNATE
                            );
         -- CS# Signaling Reset states
        TYPE sigres_type IS (SIGRES_IDLE,
                              SIGRES_FIRST_FE,
                              SIGRES_FIRST_RE,
                              SIGRES_SECOND_FE,
                              SIGRES_SECOND_RE,
                              SIGRES_THIRD_FE,
                              SIGRES_THIRD_RE,
                              SIGRES_FOURTH_FE,
                              SIGRES_FOURTH_RE,
                              SIGRES_NOT_A_RESET
                              );
        SHARED VARIABLE sigres_state : sigres_type;

        -- Instruction Type
        TYPE instruction_type IS ( NONE,
                                   WREN,       -- Write Enable
                                   WRDI,       -- Write Disable
                                   WRSR,        -- Write Register
                                   WRAR,       -- Write Any Register
                                   RDAR,       -- Read Any Register
                                   RDSR1,      -- Read Status Register 1
                                   RDSR2,      -- Read Status Register 2
                                   RDCR1,      -- Read Configuration Register 1
                                   RDCR2,      -- Read Configuration Register 2
                                   RDCR4,      -- Read Configuration Register 4
                                   RDCR5,      -- Read Configuration Register 5
                                   RDID,       -- Read ID JEDEC
                                   RUID,       -- Read Unique ID
                                   WRSN,       -- Write Serial Number
                                   RDSN,       -- Read Serial Number
                                   ECCRD,      -- ECC Read
                                   CLECC,      -- Clear ECC Status Register
                                   CRCC,       -- CRC Calculation (3Byte Address)
                                   RBCRC,      -- Read Bus CRC
                                   READ,       -- Read Normal (3Byte Address)
                                   FAST_READ,  -- Fast Read (3Byte Address)
                                   DDRFR,      -- Fast Read DDR (3Byte Address)
                                   DOR,        -- Read Dual Out (3Byte Address)
                                   DIOR,       -- Read Dual I/O (3Byte Address)
                                   QOR,        -- Read Quad Out (3Byte Address)
                                   QIOR,       -- Read Quad I/O (3Byte Address)
                                   DDRQIOR,    -- Read DDR Quad I/O (3Byte)
                                   WRITE_MEM,  -- Write in memory
                                   DDRWRITE,   -- DDR Write in memory
                                   FAST_WRITE, -- Fast Write in memory
                                   DDR_FAST_WRITE, -- DDR Fast Write in memory
                                   DIW,        -- Dual Input Write
                                   DIOW,       -- Dual I/O Write
                                   QIW,        -- Quad Input Write
                                   QIOW,       -- Quad I/O Write
                                   DDRQIOW,    -- DDR Quad I/O Write
                                   SSWR,       -- Special Sector Write
                                   SSRD,       -- Special Sector Read
                                   EPCS,       -- Erase/Program CRC Suspend
                                   EPCR,       -- Erase/Program CRC Resume
                                   RSTEN,      -- Software Reset Enable
                                   RSTCMD,     -- Software Reset
                                   HBN,        -- Hibernate Mode
                                   DPD         -- DPD Enter
                                );

        TYPE WByteType IS ARRAY (0 TO AddrRANGE) OF INTEGER RANGE -1 TO MaxData;
        -- f-ram Memory Array
        TYPE MemArray IS ARRAY (0 TO AddrRANGE) OF INTEGER RANGE -1 TO MaxData;
        -- OTP Memory Array
        TYPE OTPArray IS ARRAY (OTPLoAddr TO OTPHiAddr) OF INTEGER
                                                    RANGE -1 TO MaxData;
        TYPE IDregtype IS ARRAY  (0 TO 7) OF std_logic_vector(7 downto 0);

        -- Main Memory
        SHARED VARIABLE Mem           : MemArray  := (OTHERS => MaxData);
        -- OTP Sector
        SHARED VARIABLE OTPMem        : OTPArray  := (OTHERS => MaxData);

        -- Programming Buffer
        SIGNAL WByte                 : WByteType := (OTHERS => MaxData);

        -- states
        SIGNAL current_state         : state_type := RESET_STATE;
        SIGNAL next_state            : state_type := RESET_STATE;
        
        SIGNAL next_sigres_state     : sigres_type := SIGRES_IDLE;
        SIGNAL current_sigres_state  : sigres_type := SIGRES_IDLE;

        SIGNAL Instruct              : instruction_type;
        --zero delay signal
        SIGNAL SOut_zd               : std_logic := 'Z';
        SIGNAL SIOut_zd              : std_logic := 'Z';
        SIGNAL RESETNegOut_zd        : std_logic := 'Z';
        SIGNAL WPNegOut_zd           : std_logic := 'Z';
        --HOLD delay on output data
        SIGNAL SOut_z                : std_logic := 'Z';
        SIGNAL SIOut_z               : std_logic := 'Z';
        -- powerup
        SIGNAL PoweredUp             : std_logic := '0';

        -----------------------------------------------------------------------
        -- Registers
        -----------------------------------------------------------------------
        --     ***  Status Register 1  ***

        -- Status Register 1
        
         SIGNAL MEM_CHECK       : std_logic := '0';
         SIGNAL XIP_CHECK        : std_logic := '0';
         SIGNAL DUAL_CHECK       : std_logic := '0';
         SIGNAL DDR_CHECK       : std_logic := '0';
         SIGNAL reset_HARD       : std_logic := '0';
         SIGNAL reset_SOFT      : std_logic := '0';
         SIGNAL reset_SIG       : std_logic := '0';
         SIGNAL  reset_check : std_logic_vector(1 downto 0)   := (others => '0');

        SIGNAL SR1_V       : std_logic_vector(7 downto 0)   := (others => '0');
        
        SIGNAL SR1_NV       : std_logic_vector(7 downto 0)   := (others => '0');

        SIGNAL SR1_in    : std_logic_vector(7 downto 0)   := (others => '0');

        -- Status Register Write Disable Bit
        ALIAS SRWD      :std_logic IS SR1_V(7);
        -- Status Register TBPROT bit
        ALIAS TBPROT    :std_logic IS SR1_V(5);
        -- Status Register Block Protection Bits
        ALIAS BP2       :std_logic IS SR1_V(4);
        ALIAS BP1       :std_logic IS SR1_V(3);
        ALIAS BP0       :std_logic IS SR1_V(2);
        -- Status Register Write Enable Latch Bit
        ALIAS WEL       :std_logic IS SR1_V(1);
        -- Status Register Write In Progress Bit
        ALIAS WIP       :std_logic IS SR1_V(0);

        -- Volatile Status Register 2
        SIGNAL SR2_V   : std_logic_vector(7 downto 0)
                                                := (others => '0');
        -- CRC Suspend
        ALIAS CRCS      :std_logic IS SR2_V(4);
        -- CRC Abort
        ALIAS CRCA      :std_logic IS SR2_V(3);

        -- Nonvolatile Configuration Register 1
        SIGNAL CR1_in   : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SIGNAL CR1_NV   : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SIGNAL CR1_V	: std_logic_vector(7 downto 0)
                                                := (others => '0');
        -- Configuration Register QUAD bit
        ALIAS QUAD      :std_logic IS CR1_V(1);

        -- Nonvolatile Configuration Register 2
        SIGNAL CR2_NV   : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SIGNAL CR2_V   : std_logic_vector(7 downto 0)
                                                := (others => '0');

        -- Configuration Register 2 QPI bit
        ALIAS  QPI    :std_logic IS CR2_V(6);

        -- Configuration Register 2 DPI bit
        ALIAS  DPI    :std_logic IS CR2_V(4);

        -- Nonvolatile Configuration Register 4
        SIGNAL CR4_NV   : std_logic_vector(7 downto 0)
                                                := "00001000";
        SIGNAL CR4_V   : std_logic_vector(7 downto 0)
                                                := "00001000";

        -- Nonvolatile Configuration Register 5
        SIGNAL CR5_NV   : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SIGNAL CR5_V   : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SIGNAL REGS_PROTECTED : std_logic;

        -- Serial Number Register
        SIGNAL ID_reg : IDregtype := ("00000000",
                                      "00000000",
                                      "00000000",
                                      "00000000",
                                      "00000110",
                                      "10000000",
                                      "00000000",
                                      "00000000");

        -- Serial Number Register
        SIGNAL SERNUM_reg : std_logic_vector(63 downto 0)
                                                := (others => '0');

        --      ***  Address Trap Register  ***
        SHARED VARIABLE ADDTRAP_reg    : std_logic_vector(31 downto 0)
                                                := (others => '0');
        SIGNAL ADDTRAP_reg_in          : std_logic_vector(31 downto 0)
                                                := (others => '0');
        SHARED VARIABLE CRC_reg        : std_logic_vector(31 downto 0)
                                                := (others => '0');
        SIGNAL CRC_reg_in              : std_logic_vector(31 downto 0)
                                                := (others => '0');

        SHARED VARIABLE WRAR_reg_in    : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SHARED VARIABLE RDAR_reg       : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SHARED VARIABLE ECC_reg        : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SHARED VARIABLE EDC_reg        : std_logic_vector(15 downto 0)
                                                := (others => '0');

        SHARED VARIABLE CRC_Start_Addr_reg: NATURAL RANGE 0 TO AddrRANGE := 0;
        SHARED VARIABLE CRC_End_Addr_reg  : NATURAL RANGE 0 TO AddrRANGE := 0;

        --Command Register
        SIGNAL write              : std_logic := '0';
        SIGNAL cfg_write          : std_logic := '0';
        SIGNAL read_out           : std_logic := '0';

        SIGNAL dual               : boolean   := false;
        SIGNAL rd_slow            : boolean   := false;
        SIGNAL ddr                : boolean   := false;
        SIGNAL any_read           : boolean   := false;

        SIGNAL oe                 : boolean   := false;
        SIGNAL oe_z               : boolean   := false;
        
        SIGNAL Data_Byte          : std_logic := '0'; -- added

        --FSM control signals
        SIGNAL PDONE              : std_logic := '1'; --Prog. Done
        SIGNAL PSTART             : std_logic := '0'; --Start Programming
        SIGNAL PGSUSP             : std_logic := '0'; --Suspend Program
        SIGNAL PGRES              : std_logic := '0'; --Resume Program

        SIGNAL RES_TO_SUS_TIME    : std_logic := '0';--Resume to Suspend Flag

        SIGNAL WDONE              : std_logic := '1'; --Write operation Done
        SIGNAL WSTART             : std_logic := '0'; --Start Write operation

        SIGNAL CRCSTART           : std_logic := '0'; --CRC calc Start
        SIGNAL CRCDONE            : std_logic := '1'; --CRC calc Done
        SIGNAL CRCSUSP            : std_logic := '0'; --CRC Suspend
        SIGNAL CRCRES             : std_logic := '0'; --CRC Resume

        --reset timing
        SIGNAL RST                 : std_logic := '0';
        SIGNAL RST_HARD            : std_logic := '0';
        SIGNAL RST1                : std_logic := '0';
        SIGNAL RST_SIG             : std_logic := '1'; --new
        SIGNAL reseted             : std_logic := '0'; --Reset Timing Control
        SIGNAL RST_in              : std_logic := '0';
        SIGNAL RST_out             : std_logic := '1';
        SIGNAL SWRST_in            : std_logic := '0';
        SIGNAL SWRST_out           : std_logic := '1';
        SIGNAL RESET_EN            : std_logic := '0';
        SIGNAL reset_act           : boolean;
        SIGNAL rst_quad            : boolean;
        SIGNAL rst_not_quad        : boolean;
        SIGNAL double              : boolean;
        SIGNAL dpd_act             : boolean; 
        SIGNAL DEBUG_CHECK            : std_logic := '0';
        
        SIGNAL INITIAL_CONFIG     : std_logic := '0';

        SHARED VARIABLE SecAddr_pgm   : NATURAL RANGE 0 TO SecNumUni := 0;


        SHARED VARIABLE read_cnt  : NATURAL := 0;
        SHARED VARIABLE byte_cnt  : NATURAL := 1;
        SHARED VARIABLE read_addr : NATURAL;

        SIGNAL change_addr        : std_logic := '0';
        SIGNAL Address            : NATURAL;

        -- Sector is protect if Sec_Prot(SecNum) = '1'
        SHARED VARIABLE Sec_Prot  : std_logic_vector(SecNumUni downto 0) :=
                                                   (OTHERS => '0');

        SIGNAL change_BP          : std_logic := '0';
        SHARED VARIABLE BP_bits   : std_logic_vector(2 downto 0) := "000";

        SIGNAL Byte_number        : NATURAL RANGE 0 TO AddrRANGE := 0; 

        TYPE bus_cycle_type IS (STAND_BY,
                                OPCODE_BYTE,
                                ADDRESS_BYTES,
                                DUMMY_BYTES,
                                MODE_BYTE,
                                DATA_BYTES
                                );
        SHARED VARIABLE bus_cycle_state    : bus_cycle_type;

        SHARED VARIABLE Latency_code       : NATURAL;
        SHARED VARIABLE Register_Latency   : NATURAL;
        SHARED VARIABLE opcode_cnt         : NATURAL := 0;
        SHARED VARIABLE addr_cnt           : NATURAL := 0;
        SHARED VARIABLE mode_cnt           : NATURAL := 0;
        SHARED VARIABLE dummy_cnt          : NATURAL := 0;
        SHARED VARIABLE data_cnt           : NATURAL := 0;
        SHARED VARIABLE ZERO_DETECTED      : std_logic;

        SHARED VARIABLE CRC_ACT            : std_logic := '0';--CRC Active
        SHARED VARIABLE CRC_RD_SETUP       : std_logic := '0';--CRC read setup
        SHARED VARIABLE crc_in  : std_logic_vector(15 downto 0);
        SHARED VARIABLE crc_out : std_logic_vector(31 downto 0);
        SHARED VARIABLE crc_tmp : std_logic;

        SIGNAL RES_TO_SUSP_TIME            : std_ulogic := '0';

        -- timing check violation
        SIGNAL Viol               : X01 := '0';

    BEGIN

    -----------------------------------------------------------------------
    -- Register related logic
    -----------------------------------------------------------------------
    REGS_PROTECTED  <= SRWD AND (NOT WPNeg_ipd);

    ---------------------------------------------------------------------------
    --Power Up time
    ---------------------------------------------------------------------------

    PoweredUp <= '1' AFTER tdevice_PU;
    reset_act <= CR2_NV(5)='1' AND ((QUAD='0' AND QPI='0') OR CSNeg='1'); -- reset_act <= CR2_NV(5)='1' AND (QUAD='0' OR (QUAD='1' AND CSNeg='1'));
    rst_quad <= TRUE WHEN (CR2_V(5) = '1') AND (CSNeg = '1')  ELSE  FALSE; -- TRUE WHEN (CR2_NV(5) = '1') AND (QUAD = '1')  ELSE  FALSE;
    rst_not_quad <= CR2_V(5) = '1' AND QUAD = '0' AND QPI = '0';
    
    MODE_3: PROCESS(CSNeg, ddr)
    BEGIN
            IF ((falling_edge(CSNeg) OR rising_edge(CSNeg)) AND ddr = true AND SCK = '1') THEN
                mode3 <= '1';
            ELSIF ((falling_edge(CSNeg) OR rising_edge(CSNeg)) AND SCK = '0') THEN
                mode3 <= '0';
            END IF;
        
    END PROCESS MODE_3;
    
    TimingModelSel: PROCESS
    BEGIN
        IF TimingModel(15)='0' OR TimingModel(15)='2' OR
        TimingModel(15)='3' THEN
            SECURE_OPN <= '0';
        ELSIF TimingModel(15)='Y' OR TimingModel(15)='y' OR
        TimingModel(15)='Z' OR TimingModel(15)='z' THEN
            SECURE_OPN <= '1';
        END IF;
        WAIT;
    END PROCESS;

    CSTIME: PROCESS( QPI, DPI, QUAD, DDR_CHECK, MEM_CHECK, XIP_CHECK, DUAL_CHECK)
    BEGIN
        IF (PoweredUp = '1') THEN
            IF (QPI = '0' AND DPI = '0' AND QUAD = '0' AND DUAL_CHECK = '0' AND DDR_CHECK = '0') THEN
                tdevice_CS := tdevice_CS_spi;
            ELSIF (QPI = '0' AND( DPI  = '1' OR DUAL_CHECK  = '1') AND QUAD = '0' AND MEM_CHECK = '0' AND DDR_CHECK = '0' ) THEN
                tdevice_CS := tdevice_CS_dpi;
            ELSIF (QPI = '0' AND ( DPI = '1' OR (DUAL_CHECK = '1'  AND QUAD = '0')) AND MEM_CHECK = '1' AND XIP_CHECK = '0' AND DDR_CHECK = '0') THEN
                tdevice_CS := tdevice_CS_dpi1;
            ELSIF (QPI = '0' AND ( DPI = '1' OR (DUAL_CHECK = '1'  AND QUAD = '0')) AND MEM_CHECK = '1' AND XIP_CHECK = '1' AND DDR_CHECK = '0') THEN
                tdevice_CS := tdevice_CS_dpi2;
            ELSIF ((QPI = '1' OR QUAD = '1') AND DPI = '0' AND DUAL_CHECK = '0' AND  DDR_CHECK = '0' AND MEM_CHECK = '0') THEN
                tdevice_CS := tdevice_CS_qpi;
            ELSIF ((QPI = '1' OR QUAD = '1')AND DPI = '0' AND DUAL_CHECK = '0' AND DDR_CHECK = '0' AND MEM_CHECK = '1' AND XIP_CHECK = '0') THEN
                tdevice_CS := tdevice_CS_qpi1;
            ELSIF ((QPI = '1' OR QUAD = '1') AND DPI = '0' AND DUAL_CHECK = '0' AND DDR_CHECK = '0' AND MEM_CHECK = '1' AND XIP_CHECK = '1') THEN
                tdevice_CS := tdevice_CS_qpi2;
            ELSIF (QPI = '0' AND DPI = '0' AND QUAD = '0' AND DDR_CHECK = '1') THEN
                tdevice_CS := tdevice_CS_ddr;
            ELSIF ((QPI = '1' OR QUAD = '1') AND DPI = '0' AND DUAL_CHECK = '0' AND DDR_CHECK = '1' AND MEM_CHECK = '0') THEN
                tdevice_CS := tdevice_CS_ddr_1;
            ELSIF ((QPI = '1' OR QUAD = '1') AND DPI = '0' AND DUAL_CHECK = '0' AND DDR_CHECK = '1' AND MEM_CHECK = '1' AND XIP_CHECK = '0') THEN
                tdevice_CS := tdevice_CS_ddr_2;
            ELSIF ((QPI = '1' OR QUAD = '1') AND DPI = '0' AND DUAL_CHECK = '0' AND DDR_CHECK = '1' AND MEM_CHECK = '1' AND XIP_CHECK = '1') THEN
                tdevice_CS := tdevice_CS_ddr_3;
            ELSE 
                tdevice_CS := tdevice_CS_spi;
            END IF;
        END IF;
    END PROCESS;

    ---------------------------------------------------------------------------
    -- VITAL Timing Checks Procedures
    ---------------------------------------------------------------------------
    VITALTimingCheck: PROCESS(SIIn, SOIn, SCK_ipd, CSNeg_ipd, RESETNeg_ipd,
                              WPNegIn)

        -- Timing Check Variables
        -- Setup/Hold Checks variables
        VARIABLE Tviol_CSNeg_SCK  : X01 := '0';
        VARIABLE TD_CSNeg_SCK     : VitalTimingDataType;

        VARIABLE Tviol_SI_SCK            : X01 := '0';
        VARIABLE TD_SI_SCK               : VitalTimingDataType;

        VARIABLE Tviol_SI_SCK_ddr_R      : X01 := '0';
        VARIABLE TD_SI_SCK_ddr_R         : VitalTimingDataType;

        VARIABLE Tviol_SI_SCK_ddr_F      : X01 := '0';
        VARIABLE TD_SI_SCK_ddr_F         : VitalTimingDataType;

        VARIABLE Tviol_SO_SCK            : X01 := '0';
        VARIABLE TD_SO_SCK               : VitalTimingDataType;

        VARIABLE Tviol_SO_SCK_ddr_R      : X01 := '0';
        VARIABLE TD_SO_SCK_ddr_R         : VitalTimingDataType;

        VARIABLE Tviol_SO_SCK_ddr_F      : X01 := '0';
        VARIABLE TD_SO_SCK_ddr_F         : VitalTimingDataType;

        VARIABLE Tviol_WPNeg_SCK         : X01 := '0';
        VARIABLE TD_WPNeg_SCK            : VitalTimingDataType;

        VARIABLE Tviol_WPNeg_SCK_ddr_R   : X01 := '0';
        VARIABLE TD_WPNeg_SCK_ddr_R      : VitalTimingDataType;

        VARIABLE Tviol_WPNeg_SCK_ddr_F   : X01 := '0';
        VARIABLE TD_WPNeg_SCK_ddr_F      : VitalTimingDataType;

        VARIABLE Tviol_RESETNeg_SCK      : X01 := '0';
        VARIABLE TD_RESETNeg_SCK         : VitalTimingDataType;

        VARIABLE Tviol_RESETNeg_SCK_ddr_R   : X01 := '0';
        VARIABLE TD_RESETNeg_SCK_ddr_R      : VitalTimingDataType;

        VARIABLE Tviol_RESETNeg_SCK_ddr_F   : X01 := '0';
        VARIABLE TD_RESETNeg_SCK_ddr_F      : VitalTimingDataType;

        VARIABLE Tviol_WPNeg_CSNeg_setup    : X01 := '0';
        VARIABLE TD_WPNeg_CSNeg_setup       : VitalTimingDataType;

        VARIABLE Tviol_WPNeg_CSNeg_hold     : X01 := '0';
        VARIABLE TD_WPNeg_CSNeg_hold        : VitalTimingDataType;

        VARIABLE Tviol_RESETNeg_CSNeg       : X01 := '0';
        VARIABLE TD_RESETNeg_CSNeg          : VitalTimingDataType;

        VARIABLE Tviol_CSNeg_RESETNeg       : X01 := '0';
        VARIABLE TD_CSNeg_RESETNeg          : VitalTimingDataType;

        --Pulse Width and Period Check Variables
        VARIABLE Pviol_SCK_rd     : X01 := '0';
        VARIABLE PD_SCK_rd        : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_SCK_ddr    : X01 := '0';
        VARIABLE PD_SCK_ddr       : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_CSNeg      : X01 := '0';
        VARIABLE PD_CSNeg         : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_CSNeg_rst_quad : X01 := '0';
        VARIABLE PD_CSNeg_rst_quad    : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_CSNeg_wip    : X01 := '0';
        VARIABLE PD_CSNeg_wip       : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_RESETNeg     : X01 := '0';
        VARIABLE PD_RESETNeg        : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Violation          : X01 := '0';

    BEGIN
    ---------------------------------------------------------------------------
    -- Timing Check Section
    ---------------------------------------------------------------------------
        IF (TimingChecksOn) THEN

        -- Setup/Hold Check between CS# and SCK, mode0
        VitalSetupHoldCheck (
            TestSignal      => CSNeg_ipd,
            TestSignalName  => "CS#",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_CSNeg_SCK_normal_rd,
            SetupLow        => tsetup_CSNeg_SCK_normal_rd,
            HoldHigh        => thold_CSNeg_SCK_normal_rd0,
            HoldLow         => thold_CSNeg_SCK_normal_rd0,
            CheckEnabled    => PoweredUp='1' AND SIOut_zd /= SIIn AND ddr = false,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_CSNeg_SCK,
            Violation       => Tviol_CSNeg_SCK
        );
        
         -- Setup/Hold Check between CS# and SCK, mode3
        VitalSetupHoldCheck (
            TestSignal      => CSNeg_ipd,
            TestSignalName  => "CS#",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_CSNeg_SCK_normal_rd,
            SetupLow        => tsetup_CSNeg_SCK_normal_rd,
            HoldHigh        => thold_CSNeg_SCK_normal_rd3,
            HoldLow         => thold_CSNeg_SCK_normal_rd3,
            CheckEnabled    => PoweredUp='1' AND SIOut_zd /= SIIn AND ddr = false,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_CSNeg_SCK,
            Violation       => Tviol_CSNeg_SCK
        );
        
        -- Setup/Hold Check between CS# and SCK, ddr
        VitalSetupHoldCheck (
            TestSignal      => CSNeg_ipd,
            TestSignalName  => "CS#",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_CSNeg_SCK_ddr_rd,
            SetupLow        => tsetup_CSNeg_SCK_ddr_rd,
            HoldHigh        => thold_CSNeg_SCK_ddr_rd,
            HoldLow         => thold_CSNeg_SCK_ddr_rd,
            CheckEnabled    => PoweredUp='1' AND SIOut_zd /= SIIn AND ddr = true,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_CSNeg_SCK,
            Violation       => Tviol_CSNeg_SCK
        );

        -- Hold Check between CSNeg and RESETNeg
        VitalSetupHoldCheck (
            TestSignal      => CSNeg,
            TestSignalName  => "CSNeg",
            RefSignal       => RESETNeg,
            RefSignalName   => "RESETNeg",
            HoldHigh        => thold_CSNeg_RESETNeg,
            CheckEnabled    => CR2_NV(5)='1' AND QUAD='0',
            RefTransition   => '\',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_CSNeg_RESETNeg,
            Violation       => Tviol_CSNeg_RESETNeg
        );

        -- Setup/Hold Check between SI and SCK, SDR mode
        VitalSetupHoldCheck (
            TestSignal      => SIIn,
            TestSignalName  => "SI",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK_normal_rd,
            SetupLow        => tsetup_SI_SCK_normal_rd,
            HoldHigh        => thold_SI_SCK_normal_rd,
            HoldLow         => thold_SI_SCK_normal_rd,
            CheckEnabled    => PoweredUp='1' AND SIOut_zd /= SIIn,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_SI_SCK,
            Violation       => Tviol_SI_SCK
        );

        -- Setup/Hold Check between SI and SCK, DDR mode
        VitalSetupHoldCheck (
            TestSignal      => SIIn,
            TestSignalName  => "SI",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK_ddr_rd,
            SetupLow        => tsetup_SI_SCK_ddr_rd,
            HoldHigh        => thold_SI_SCK_ddr_rd,
            HoldLow         => thold_SI_SCK_ddr_rd,
            CheckEnabled    => PoweredUp='1' AND double AND
            SIOut_zd/=SIIn,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_SI_SCK_ddr_R,
            Violation       => Tviol_SI_SCK_ddr_R
        );

        -- Setup/Hold Check between SI and SCK, DDR mode
        VitalSetupHoldCheck (
            TestSignal      => SIIn,
            TestSignalName  => "SI",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK_ddr_rd,
            SetupLow        => tsetup_SI_SCK_ddr_rd,
            HoldHigh        => thold_SI_SCK_ddr_rd,
            HoldLow         => thold_SI_SCK_ddr_rd,
            CheckEnabled    => PoweredUp='1' AND double AND
            SIOut_zd/=SIIn,
            RefTransition   => '\',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_SI_SCK_ddr_F,
            Violation       => Tviol_SI_SCK_ddr_F
        );

        -- Setup/Hold Check between SO and SCK, SDR mode
        VitalSetupHoldCheck (
            TestSignal      => SOIn,
            TestSignalName  => "SO",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK_normal_rd,
            SetupLow        => tsetup_SI_SCK_normal_rd,
            HoldHigh        => thold_SI_SCK_normal_rd,
            HoldLow         => thold_SI_SCK_normal_rd,
            CheckEnabled    => PoweredUp='1' AND SOut_zd /= SOIn,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_SO_SCK,
            Violation       => Tviol_SO_SCK
        );

        -- Setup/Hold Check between SO and SCK, DDR mode
        VitalSetupHoldCheck (
            TestSignal      => SOIn,
            TestSignalName  => "SO",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK_ddr_rd,
            SetupLow        => tsetup_SI_SCK_ddr_rd,
            HoldHigh        => thold_SI_SCK_ddr_rd,
            HoldLow         => thold_SI_SCK_ddr_rd,
            CheckEnabled    => PoweredUp='1' AND double AND
            SOut_zd /= SOIn,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_SO_SCK_ddr_R,
            Violation       => Tviol_SO_SCK_ddr_R
        );

        -- Setup/Hold Check between SO and SCK, DDR mode
        VitalSetupHoldCheck (
            TestSignal      => SOIn,
            TestSignalName  => "SO",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK_ddr_rd,
            SetupLow        => tsetup_SI_SCK_ddr_rd,
            HoldHigh        => thold_SI_SCK_ddr_rd,
            HoldLow         => thold_SI_SCK_ddr_rd,
            CheckEnabled    => PoweredUp='1' AND double AND
            SOut_zd /= SOIn,
            RefTransition   => '\',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_SO_SCK_ddr_F,
            Violation       => Tviol_SO_SCK_ddr_F
        );

        -- Setup/Hold Check between WPNeg and SCK, SDR mode
        VitalSetupHoldCheck (
            TestSignal      => WPNegIn,
            TestSignalName  => "WPNeg",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK_normal_rd,
            SetupLow        => tsetup_SI_SCK_normal_rd,
            HoldHigh        => thold_SI_SCK_normal_rd,
            HoldLow         => thold_SI_SCK_normal_rd,
            CheckEnabled    => PoweredUp='1' AND
            WPNegOut_zd /= WPNegIn  AND QUAD='1',
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_WPNeg_SCK,
            Violation       => Tviol_WPNeg_SCK
        );

        -- Setup/Hold Check between WPNeg and SCK, DDR mode
        VitalSetupHoldCheck (
            TestSignal      => WPNegIn,
            TestSignalName  => "WPNeg",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK_ddr_rd,
            SetupLow        => tsetup_SI_SCK_ddr_rd,
            HoldHigh        => thold_SI_SCK_ddr_rd,
            HoldLow         => thold_SI_SCK_ddr_rd,
            CheckEnabled    => PoweredUp='1' AND double AND
            WPNegOut_zd /= WPNegIn  AND QUAD='1',
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_WPNeg_SCK_ddr_R,
            Violation       => Tviol_WPNeg_SCK_ddr_R
        );

        -- Setup/Hold Check between WPNeg and SCK, DDR mode
        VitalSetupHoldCheck (
            TestSignal      => WPNegIn,
            TestSignalName  => "WPNeg",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK_ddr_rd,
            SetupLow        => tsetup_SI_SCK_ddr_rd,
            HoldHigh        => thold_SI_SCK_ddr_rd,
            HoldLow         => thold_SI_SCK_ddr_rd,
            CheckEnabled    => PoweredUp='1' AND double AND
            WPNegOut_zd /= WPNegIn AND QUAD='1',
            RefTransition   => '\',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_WPNeg_SCK_ddr_F,
            Violation       => Tviol_WPNeg_SCK_ddr_F
        );

        -- Setup/Hold Check between RESETNeg and SCK, SDR mode
        VitalSetupHoldCheck (
            TestSignal      => RESETNegIn,
            TestSignalName  => "RESETNeg",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK_normal_rd,
            SetupLow        => tsetup_SI_SCK_normal_rd,
            HoldHigh        => thold_SI_SCK_normal_rd,
            HoldLow         => thold_SI_SCK_normal_rd,
            CheckEnabled    => PoweredUp='1' AND
            RESETNegOut_zd /= RESETNegIn AND QUAD='1' AND CSNeg='0',
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_WPNeg_SCK,
            Violation       => Tviol_RESETNeg_SCK
        );

        -- Setup/Hold Check between RESETNeg and SCK, DDR mode
        VitalSetupHoldCheck (
            TestSignal      => RESETNegIn,
            TestSignalName  => "RESETNeg",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK_ddr_rd,
            SetupLow        => tsetup_SI_SCK_ddr_rd,
            HoldHigh        => thold_SI_SCK_ddr_rd,
            HoldLow         => thold_SI_SCK_ddr_rd,
            CheckEnabled    => PoweredUp='1' AND double AND
            RESETNegOut_zd /= RESETNegIn AND QUAD='1' AND CSNeg='0',
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_RESETNeg_SCK_ddr_R,
            Violation       => Tviol_RESETNeg_SCK_ddr_R
        );

        -- Setup/Hold Check between RESETNeg and SCK, DDR mode
        VitalSetupHoldCheck (
            TestSignal      => RESETNegIn,
            TestSignalName  => "RESETNeg",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK_ddr_rd,
            SetupLow        => tsetup_SI_SCK_ddr_rd,
            HoldHigh        => thold_SI_SCK_ddr_rd,
            HoldLow         => thold_SI_SCK_ddr_rd,
            CheckEnabled    => PoweredUp='1' AND double AND
            RESETNegOut_zd /= RESETNegIn AND QUAD='1' AND CSNeg='0',
            RefTransition   => '\',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_RESETNeg_SCK_ddr_F,
            Violation       => Tviol_RESETNeg_SCK_ddr_F
        );

        -- Setup Check between WP# and CS# \
        VitalSetupHoldCheck (
            TestSignal      => WPNegIn,
            TestSignalName  => "WP#",
            RefSignal       => CSNeg_ipd,
            RefSignalName   => "CS#",
            SetupHigh       => tsetup_WPNeg_CSNeg,
            CheckEnabled    => SRWD='1' AND QUAD='0',
            RefTransition   => '\',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_WPNeg_CSNeg_setup,
            Violation       => Tviol_WPNeg_CSNeg_setup
        );

        -- Hold Check between WP# and CS# /
        VitalSetupHoldCheck (
            TestSignal      => WPNegIn,
            TestSignalName  => "WP#",
            RefSignal       => CSNeg_ipd,
            RefSignalName   => "CS#",
            HoldHigh        => thold_WPNeg_CSNeg,
            CheckEnabled    => SRWD = '1' AND QUAD = '0',
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_WPNeg_CSNeg_hold,
            Violation       => Tviol_WPNeg_CSNeg_hold
        );

        -- Setup Check between RESETNeg and CSNeg, DDR mode
        VitalSetupHoldCheck (
            TestSignal      => RESETNeg,
            TestSignalName  => "RESETNeg",
            RefSignal       => CSNeg,
            RefSignalName   => "CSNeg",
            SetupHigh       => tsetup_RESETNeg_CSNeg,
            CheckEnabled    => CR2_NV(5)='1' AND QUAD='0',
            RefTransition   => '\',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_RESETNeg_CSNeg,
            Violation       => Tviol_RESETNeg_CSNeg
        );

        --Pulse Width and Period Check Variables
        -- Pulse Width Check SCK for READ, serial mode
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            PulseWidthLow   =>  tpw_SCK_normal_rd,
            PulseWidthHigh  =>  tpw_SCK_normal_rd,
            PeriodData      =>  PD_SCK_rd,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK_rd,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  rd_slow);

        -- Pulse Width Check SCK for DDR mode
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            PulseWidthLow   =>  tpw_SCK_ddr_rd,
            PulseWidthHigh  =>  tpw_SCK_ddr_rd,
            PeriodData      =>  PD_SCK_ddr,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK_ddr,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  ddr);

        -- Pulse Width Check CS# for READ, serial mode
        VitalPeriodPulseCheck (
            TestSignal      =>  CSNeg_ipd,
            TestSignalName  =>  "CS#",
            PulseWidthHigh  =>  tpw_CSNeg_posedge,
            PeriodData      =>  PD_CSNeg,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_CSNeg,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  any_read);

        -- Pulse Width Check CS# for QUAD mode
        VitalPeriodPulseCheck (
            TestSignal      =>  CSNeg_ipd,
            TestSignalName  =>  "CS#",
            PulseWidthHigh  =>  tpw_CSNeg_rst_quad_posedge,
            PeriodData      =>  PD_CSNeg_rst_quad,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_CSNeg_rst_quad,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  rst_quad);

        -- Pulse Width Check CS# for Program/Erase, serial mode
        VitalPeriodPulseCheck (
            TestSignal      =>  CSNeg_ipd,
            TestSignalName  =>  "CS#",
            PulseWidthHigh  =>  tpw_CSNeg_wip_posedge,
            PeriodData      =>  PD_CSNeg_wip,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_CSNeg_wip,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  WIP = '1');

        -- Pulse Width Check RESETNeg
        VitalPeriodPulseCheck (
            TestSignal        => RESETNeg_ipd,
            TestSignalName    => "RESETNeg",
            PulseWidthLow     => tpw_RESETNeg_negedge,
            PulseWidthHigh    => tpw_RESETNeg_posedge,
            CheckEnabled      => reset_act,
            HeaderMsg         => InstancePath & PartID,
            PeriodData        => PD_RESETNeg,
            Violation         => Pviol_RESETNeg);

        -- Period Check SCK for READ, serial mode
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            Period          =>  tperiod_SCK_normal_rd,
            PeriodData      =>  PD_SCK_rd,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK_rd,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  rd_slow);

        -- Period Check SCK for DUAL READ, serial mode
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            Period          =>  tperiod_SCK_ddr_rd,
            PeriodData      =>  PD_SCK_ddr,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK_ddr,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  ddr);

        Violation :=   Tviol_CSNeg_SCK OR
                       Tviol_CSNeg_RESETNeg OR
                       Tviol_SI_SCK OR
                       Tviol_SI_SCK_ddr_R OR
                       Tviol_SI_SCK_ddr_F OR
                       Tviol_SO_SCK OR
                       Tviol_SO_SCK_ddr_R OR
                       Tviol_SO_SCK_ddr_F OR
                       Tviol_WPNeg_SCK OR
                       Tviol_WPNeg_SCK_ddr_R OR
                       Tviol_WPNeg_SCK_ddr_F OR
                       Tviol_RESETNeg_SCK OR
                       Tviol_RESETNeg_SCK_ddr_R OR
                       Tviol_RESETNeg_SCK_ddr_F OR
                       Tviol_WPNeg_CSNeg_setup OR
                       Tviol_WPNeg_CSNeg_hold OR
                       Tviol_RESETNeg_CSNeg OR
                       Pviol_SCK_rd OR
                       Pviol_SCK_ddr OR
                       Pviol_CSNeg OR
                       Pviol_CSNeg_rst_quad OR
                       Pviol_CSNeg_wip OR
                       Pviol_RESETNeg;

            Viol <= Violation;

            ASSERT Violation = '0'
                REPORT InstancePath & partID & ": simulation may be" &
                    " inaccurate due to timing violations"
                SEVERITY WARNING;

        END IF;
    END PROCESS VITALTimingCheck;

    ----------------------------------------------------------------------------
    -- sequential process for FSM state transition
    ----------------------------------------------------------------------------

   -- CS# Signaling Reset state machine
        CSNegSignalingReset: PROCESS(CSNeg_ipd, SI_ipd, SCK_ipd)

        BEGIN

        CASE current_sigres_state IS
             WHEN SIGRES_IDLE   =>
                -- Start check once CSNeg is asserted
                -- For first CS# assertion data needs to be 1'b0.
                IF falling_edge(CSNeg_ipd) AND SI_ipd = '0' THEN
                   next_sigres_state <= SIGRES_FIRST_FE;
                END IF;
             WHEN SIGRES_FIRST_FE   => -- 1st falling edge occured
                -- Data needs to be constant zero during and at the end of
                -- memory selection - check if this is the case
                IF rising_edge(CSNeg_ipd) AND SI_ipd = '0' THEN
                   next_sigres_state <= SIGRES_FIRST_RE;
                -- SI data cannot toggle during memory selection
                -- SCK cannot toggle during memory selection
                ELSIF ((rising_edge(SCK_ipd) OR falling_edge(SCK_ipd) OR
                      (SI_ipd = '1')) AND (CSNeg_ipd = '0')) THEN
                   next_sigres_state <= SIGRES_NOT_A_RESET;
                END IF;
             WHEN SIGRES_FIRST_RE   => -- 1st rising edge occured
                -- For second CS# assertion data needs to be 1'b1.
                IF falling_edge(CSNeg_ipd) AND SI_ipd = '1' THEN
                   next_sigres_state <= SIGRES_SECOND_FE;
                -- SI data cannot toggle during memory selection
                -- SCK cannot toggle during memory selectio
                ELSIF ((rising_edge(SCK_ipd) OR falling_edge(SCK_ipd) OR
                      (SI_ipd = '0')) AND (CSNeg_ipd = '0')) THEN
                   next_sigres_state <= SIGRES_NOT_A_RESET;
                END IF;
             WHEN SIGRES_SECOND_FE   => -- 2nd falling edge occured
                -- Data needs to be constant one during and at the end of
                -- memory selection - check if this is the case
                IF rising_edge(CSNeg_ipd) AND SI_ipd = '1' THEN
                   next_sigres_state <= SIGRES_SECOND_RE;
                -- SI data cannot toggle during memory selection
                -- SCK cannot toggle during memory selectio
                ELSIF ((rising_edge(SCK_ipd) OR falling_edge(SCK_ipd) OR
                      (SI_ipd = '0')) AND (CSNeg_ipd = '0')) THEN
                   next_sigres_state <= SIGRES_NOT_A_RESET;
                END IF;
             WHEN SIGRES_SECOND_RE   => -- 2nd rising edge occured
                -- For 3rd CS# assertion data needs to be 1'b0.
                IF falling_edge(CSNeg_ipd) AND SI_ipd = '0' THEN
                   next_sigres_state <= SIGRES_THIRD_FE;
                -- SI data cannot toggle during memory selection
                -- SCK cannot toggle during memory selectio
                ELSIF ((rising_edge(SCK_ipd) OR falling_edge(SCK_ipd) OR
                      (SI_ipd = '1')) AND (CSNeg_ipd = '0')) THEN
                   next_sigres_state <= SIGRES_NOT_A_RESET;
                END IF;
        
         WHEN SIGRES_THIRD_FE   => -- 3rd falling edge occured
                -- Data needs to be constant one during and at the end of
                -- memory selection - check if this is the case
                IF rising_edge(CSNeg_ipd) AND SI_ipd = '0' THEN
                   next_sigres_state <= SIGRES_THIRD_RE;
                -- SI data cannot toggle during memory selection
                -- SCK cannot toggle during memory selectio
                ELSIF ((rising_edge(SCK_ipd) OR falling_edge(SCK_ipd) OR
                      (SI_ipd = '1')) AND (CSNeg_ipd = '0')) THEN
                   next_sigres_state <= SIGRES_NOT_A_RESET;
                END IF;

        WHEN SIGRES_THIRD_RE =>   -- 3nd rising edge occured
         -- Data needs to be constant one during and at the end of
                -- memory selection - check if this is the case
                IF falling_edge(CSNeg_ipd) AND SI_ipd = '1' THEN
                   next_sigres_state <= SIGRES_FOURTH_FE;
                -- SI data cannot toggle during memory selection
                -- SCK cannot toggle during memory selectio
                ELSIF ((rising_edge(SCK_ipd) OR falling_edge(SCK_ipd) OR
                      (SI_ipd = '0')) AND (CSNeg_ipd = '0')) THEN
                   next_sigres_state <= SIGRES_NOT_A_RESET;
                END IF;
        

        WHEN SIGRES_FOURTH_FE =>    -- 4th falling edge occured
               -- Data needs to be constant one during and at the end of
                -- memory selection - check if this is the case
                IF rising_edge(CSNeg_ipd) AND SI_ipd = '1' THEN
                   RST_SIG <= '0', '1' AFTER 10 ns;
                   next_sigres_state <= SIGRES_FOURTH_RE;
                -- SI data cannot toggle during memory selection
                -- SCK cannot toggle during memory selectio
                ELSIF ((rising_edge(SCK_ipd) OR falling_edge(SCK_ipd) OR
                      (SI_ipd = '0')) AND (CSNeg_ipd = '0')) THEN
                   next_sigres_state <= SIGRES_NOT_A_RESET;
                END IF;
        

        WHEN SIGRES_FOURTH_RE =>    -- 4th risig edge occured
        
             -- Final state - reset memory

        

        WHEN SIGRES_NOT_A_RESET   =>
                IF CSNeg_ipd = '1' THEN
                   next_sigres_state <= SIGRES_IDLE;
                END IF;
         END CASE;
        END PROCESS; 
    
  
   RST_HARD <= RESETNeg AFTER 199 ns;
   CS_Out <= CS_in AFTER tdevice_CS;
   
   RST <= RST_SIG OR RST_HARD;

    StateTransition1: PROCESS(next_state, PoweredUp, RST, RST_out, SWRST_out,
                                RESETNeg, write, RST_SIG, RST_HARD)
    BEGIN
        IF PoweredUp = '1' THEN
            
            IF (((RESETNegIn='0' AND reset_act) OR
            (rising_edge(RESETNeg) AND reset_act)) AND falling_edge(RST_HARD))
                OR RST_SIG = '0' THEN
            -- no state transition while RESET# low
                current_state <= RESET_STATE;
--                 current_sigres_state  <= SIGRES_IDLE;
                RST_in <= '1', '0' AFTER 1 ns;
                reseted <= '0';
            ELSIF (NOT reset_act OR (RESETNegIn='1' AND reset_act)) AND
            RST_out = '1' AND SWRST_out = '1' THEN
                current_state <= next_state;
                reseted <= '1';
            END IF;

            IF falling_edge(write) THEN
                IF (Instruct = RSTCMD AND RESET_EN = '1') THEN
                -- no state transition while RESET is in progress
                    current_state <= RESET_STATE;
                    SWRST_in <= '1', '0' AFTER 1 ns;
                    reseted <= '0';
                END IF;
            END IF;
        END IF;
    END PROCESS;
    
    StateTransition2: PROCESS(next_sigres_state, PoweredUp, RST, write)
    BEGIN
        IF PoweredUp = '1' THEN
            IF falling_edge(RST) THEN
            -- no state transition while RESET# low
                current_sigres_state  <= SIGRES_IDLE;
            ELSE
                current_sigres_state  <= next_sigres_state;
            END IF;

            IF falling_edge(write) THEN
                IF Instruct = RSTEN THEN
                -- no state transition while RESET is in progress
                current_sigres_state  <= SIGRES_IDLE;
                END IF;
            END IF;
        END IF;
    END PROCESS;

    -- Timing control for the Hardware Reset
    Threset1: PROCESS(RST_in)
    BEGIN
        IF rising_edge(RST_in) THEN
            reset_HARD <= '1', '0' AFTER 10 ns;
            RST_out <= '0', '1' AFTER (tdevice_RPH - 200 ns);
        END IF;
    END PROCESS;
    
    -- Timing control for the SIgress
    Threset3: PROCESS(RST_SIG)
    BEGIN
        IF rising_edge(RST_SIG) THEN
            reset_SIG <= '1', '0' AFTER 10 ns;
        END IF;
    END PROCESS;
    
    -- Timing control for the Software Reset
    Threset2: PROCESS(SWRST_in)
    BEGIN
        IF rising_edge(SWRST_in) THEN
            reset_SOFT <= '1', '0' AFTER 10 ns;
            SWRST_out <= '0', '1' AFTER (tdevice_SRESET);
        END IF;
    END PROCESS;
    
    Threset4: PROCESS(reset_SOFT, reset_SIG, reset_HARD)
    BEGIN
            IF  reset_HARD = '1' THEN
                reset_check(1 DOWNTO 0) <= "01";
            ELSIF reset_SOFT = '1' THEN
                reset_check(1 DOWNTO 0) <= "10";
            ELSIF reset_SIG = '1' THEN
                reset_check(1 DOWNTO 0) <= "11";
            END IF;
    END PROCESS;

    ---------------------------------------------------------------------------
    --  Write cycle decode
    ---------------------------------------------------------------------------

    BusCycleDecode : PROCESS(SCK_ipd, CSNeg_ipd)

        TYPE quad_data_type IS ARRAY (0 TO 1023) OF INTEGER RANGE 0 TO 15;

        VARIABLE bit_cnt            : NATURAL := 0;
        VARIABLE Data_in            : std_logic_vector(4095 downto 0) 
                                                    := (others => '1');
        

        VARIABLE opcode             : std_logic_vector(7 downto 0);
        VARIABLE opcode_in          : std_logic_vector(7 downto 0);
        VARIABLE opcode_tmp         : std_logic_vector(7 downto 0);
        VARIABLE addr_bytes         : std_logic_vector(23 downto 0);
        VARIABLE Address_in         : std_logic_vector(23 downto 0);
        VARIABLE mode_bytes         : std_logic_vector(7 downto 0);
        VARIABLE mode_in            : std_logic_vector(7 downto 0);
        VARIABLE quad_data_in       : quad_data_type;
        VARIABLE quad_nybble        : std_logic_vector(3 downto 0) := "0000";
        VARIABLE Quad_slv           : std_logic_vector(3 downto 0);
        VARIABLE Byte_slv           : std_logic_vector(7 downto 0) := "00000000";
        VARIABLE Quad_int           : INTEGER;
        VARIABLE sect               : INTEGER;

        VARIABLE CLK_PER            : time;
        VARIABLE LAST_CLK           : time;
        VARIABLE Check_freq         : boolean := FALSE;

    BEGIN

        IF rising_edge(CSNeg_ipd) AND bus_cycle_state /= DATA_BYTES THEN
            bus_cycle_state := STAND_BY;
        ELSE

            CASE bus_cycle_state IS
                WHEN STAND_BY =>
                    IF falling_edge(CSNeg_ipd) THEN
                        Instruct  <= NONE;
                        write     <= '1';
                        cfg_write <= '0';
                        opcode_cnt:= 0;
                        addr_cnt  := 0;
                        mode_cnt  := 0;
                        dummy_cnt := 0;
                        data_cnt  := 0;
                        CLK_PER   := 0 ns;
                        LAST_CLK  := 0 ns;
                        Data_in   := (others => '1');
                        Data_in_8 := (others => '1');
                        ZERO_DETECTED := '0';
                        bus_cycle_state := OPCODE_BYTE;
                    END IF;

                WHEN OPCODE_BYTE =>
                    data_cnt  := 0;
                    IF rising_edge(SCK_ipd) THEN

                        CLK_PER  := NOW - LAST_CLK;
                        LAST_CLK := NOW;

                        IF CSNeg_ipd = '0' THEN

                            Latency_code := to_nat(CR1_V(7 downto 4));
                            Register_Latency := to_nat(CR5_V(7 downto 6));

                            IF QPI = '1' THEN
                                opcode_in(4*opcode_cnt)   := RESETNegIn;
                                opcode_in(4*opcode_cnt+1) := WPNegIn;
                                opcode_in(4*opcode_cnt+2) := SOIn;
                                opcode_in(4*opcode_cnt+3) := SIIn;
                            ELSIF DPI = '1' THEN
                                opcode_in(2*opcode_cnt)   := SOIn;
                                opcode_in(2*opcode_cnt+1) := SIIn;
                            ELSE
                                opcode_in(opcode_cnt) := SIIn;
                            END IF;
                            opcode_cnt := opcode_cnt + 1;

                            IF (QPI = '1' AND opcode_cnt = BYTE/4) OR
                            (DPI = '1' AND opcode_cnt = BYTE/2) OR
                            opcode_cnt = BYTE THEN
                                --MSB first
                                FOR I IN 7 DOWNTO 0 LOOP
                                    opcode(i) := opcode_in(7-i);
                                END LOOP;
                                CASE opcode IS
                                    WHEN "00000001"  => --01h
                                        Instruct <= WRSR;
                                        IF WEL = '1' THEN
                                           bus_cycle_state := DATA_BYTES;
                                           MEM_CHECK <= '0';
                                           XIP_CHECK <=  '0';
                                           DUAL_CHECK <= '0';
                                          DDR_CHECK <= '0';
                                        ELSE 
                                           bus_cycle_state :=STAND_BY;
                                        END IF;
                                    WHEN "00000010"  => --02h
                                        Instruct <= WRITE_MEM;
                                        IF WEL = '1' THEN
                                            bus_cycle_state := ADDRESS_BYTES;
                                            MEM_CHECK <= '1';
                                            XIP_CHECK <=  '0';
                                            DUAL_CHECK <= '0';
                                           DDR_CHECK <= '0';
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "00000011"  => --03h
                                        Instruct <= READ;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := TRUE;
                                        MEM_CHECK <= '1';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "00000100"  => --04h
                                        Instruct <= WRDI;
                                        bus_cycle_state := DATA_BYTES;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "00000101"  => --05h
                                        Instruct <= RDSR1;
                                        bus_cycle_state := DUMMY_BYTES;
                                        Check_freq := TRUE;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "00000110"  => --06h
                                        Instruct <= WREN;
                                        bus_cycle_state := DATA_BYTES;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                        DDR_CHECK <= '0';

                                    WHEN "00000111"  => --07h
                                        Instruct <= RDSR2;
                                        bus_cycle_state := DUMMY_BYTES;
                                        Check_freq := TRUE;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "00001011"  => --0Bh
                                        Instruct <= FAST_READ;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := TRUE;
                                        MEM_CHECK <= '1';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "00001101"  => --0Dh
                                        Instruct <= DDRFR;
                                        IF QPI = '1' THEN
                                            bus_cycle_state := ADDRESS_BYTES;
                                            Check_freq := TRUE;
                                            MEM_CHECK <= '1';
                                            XIP_CHECK <=  '0';
                                            DUAL_CHECK <= '0';
                                            DDR_CHECK <= '1';
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "00011001"  => --19h
                                        Instruct <= ECCRD;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := TRUE;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "00011011"  => --1Bh
                                        Instruct <= CLECC;
                                        bus_cycle_state := DATA_BYTES;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "00110010"  => --32h
                                        Instruct <= QIW;
                                        IF DPI = '1' OR QPI = '1' OR WEL = '0' THEN
                                    --Command not supported in QPI mode
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                            MEM_CHECK <= '1';
                                            XIP_CHECK <=  '0';
                                            DUAL_CHECK <= '0';
                                           DDR_CHECK <= '0';
                                        END IF;

                                    WHEN "00110101"  => --35h
                                        Instruct <= RDCR1;
                                        bus_cycle_state := DUMMY_BYTES;
                                        Check_freq := TRUE;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "00111011"  => --3Bh
                                        Instruct <= DOR;
                                        IF DPI = '1' OR QPI = '1' THEN
                                    --Command not supported in QPI mode
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                            Check_freq := TRUE;
                                            MEM_CHECK <= '1';
                                            XIP_CHECK <=  '0';
                                            DUAL_CHECK <= '1';
                                           DDR_CHECK <= '0';
                                        END IF;
                                        
                                    WHEN "00111111"  => --3Fh
                                        Instruct <= RDCR2;
                                        bus_cycle_state := DUMMY_BYTES;
                                        Check_freq := TRUE;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "01000010"  => --42h
                                        Instruct <= SSWR;
                                        IF WEL = '1' THEN
                                            bus_cycle_state := ADDRESS_BYTES;
                                            MEM_CHECK <= '1';
                                            XIP_CHECK <=  '0';
                                            DUAL_CHECK <= '0';
                                           DDR_CHECK <= '0';
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "01000101"  => --45h
                                        Instruct <= RDCR4;
                                        bus_cycle_state := DUMMY_BYTES;
                                        Check_freq := TRUE;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "01001011"  => --4Bh
                                        Instruct <= SSRD;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        MEM_CHECK <= '1';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "01001100"  => --4Ch
                                        Instruct <= RUID;
                                        bus_cycle_state := DUMMY_BYTES;
                                        Check_freq := TRUE;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "01011011"  => --5Bh
                                        Instruct <= CRCC;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "01011110"  => --5Eh
                                        Instruct <= RDCR5;
                                        bus_cycle_state := DUMMY_BYTES;
                                        Check_freq := TRUE;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "01100100"  => --64h
                                        Instruct <= RBCRC;
                                        bus_cycle_state := DATA_BYTES;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "01100101"  => --65h
                                        Instruct <= RDAR;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        --Check_freq := TRUE;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "01100110"  => --66h
                                        Instruct <= RSTEN;
                                        bus_cycle_state := DATA_BYTES;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';                                        

                                    WHEN "01101011"  => --6Bh
                                        Instruct <= QOR;
                                        IF DPI = '1' OR QPI = '1' THEN
                                    --Command not supported in QPI mode
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                            Check_freq := TRUE;
                                            MEM_CHECK <= '1';
                                            XIP_CHECK <=  '0';
                                            DUAL_CHECK <= '0';
                                           DDR_CHECK <= '0';
                                        END IF;

                                    WHEN "01110001"  => --71h
                                        Instruct <= WRAR;
                                        IF WEL = '1' THEN
                                            bus_cycle_state := ADDRESS_BYTES;
                                            MEM_CHECK <= '0';
                                            XIP_CHECK <=  '0';
                                            DUAL_CHECK <= '0';
                                           DDR_CHECK <= '0';
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "01110101"  => --75h
                                        Instruct <= EPCS;
                                        bus_cycle_state := DATA_BYTES;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "01111010"  => --7Ah
                                        Instruct <= EPCR;
                                        bus_cycle_state := DATA_BYTES;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "10011001"  => --99h
                                        Instruct <= RSTCMD;
                                        bus_cycle_state := DATA_BYTES;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "10011111"  => --9Fh
                                        Instruct <= RDID;
                                        bus_cycle_state := DUMMY_BYTES;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "10100001"  => --A1h
                                        Instruct <= DIOW;
                                        IF DPI = '1' OR QPI = '1' OR WEL = '0' THEN
                                    --Command not supported in QPI mode
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                            MEM_CHECK <= '1';
                                            XIP_CHECK <=  '0';
                                            DUAL_CHECK <= '1';
                                           DDR_CHECK <= '0';
                                        END IF;

                                    WHEN "10100010"  => --A2h
                                        Instruct <= DIW;
                                        IF DPI = '1' OR QPI = '1' OR WEL = '0' THEN
                                    --Command not supported in QPI mode
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                            MEM_CHECK <= '1';
                                            XIP_CHECK <=  '0';
                                            DUAL_CHECK <= '1';
                                           DDR_CHECK <= '0';
                                        END IF;

                                    WHEN "10111001" => -- B9h
                                        Instruct <= DPD;
                                        bus_cycle_state := DATA_BYTES;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "10111010"  => --BAh
                                        Instruct <= HBN;
                                        bus_cycle_state := DATA_BYTES;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "10111011"  => --BBh
                                        Instruct <= DIOR;
                                        IF DPI = '1' OR QPI = '1' THEN
                                          ASSERT false
                                              REPORT "Command not supported in QPI nor DPI mode: DIOR"
                                              SEVERITY warning;
                                    --Command not supported in QPI mode
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                            Check_freq := TRUE;
                                            MEM_CHECK <= '1';
                                            XIP_CHECK <=  '0';
                                            DUAL_CHECK <= '1';
                                           DDR_CHECK <= '0';
                                        END IF;

                                    WHEN "11000010"  => --C2h
                                        Instruct <= WRSN;
                                        IF WEL = '1' THEN
                                            bus_cycle_state := DATA_BYTES;
                                            MEM_CHECK <= '0';
                                            XIP_CHECK <=  '0';
                                            DUAL_CHECK <= '0';
                                           DDR_CHECK <= '0';
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "11000011"  => --C3h
                                        Instruct <= RDSN;
                                        bus_cycle_state := DUMMY_BYTES;
                                        Check_freq := TRUE;
                                        MEM_CHECK <= '0';
                                        XIP_CHECK <=  '0';
                                        DUAL_CHECK <= '0';
                                       DDR_CHECK <= '0';

                                    WHEN "11010001"  => --D1h
                                        Instruct <= DDRQIOW;
                                        IF DPI = '1' OR QPI = '1' OR WEL = '0' THEN
                                    --Command not supported in QPI mode
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                            MEM_CHECK <= '1';
                                            XIP_CHECK <=  '0';
                                            DUAL_CHECK <= '1';
                                           DDR_CHECK <= '0';
                                        END IF;

                                    WHEN "11010010"  => --D2h
                                        Instruct <= QIOW;
                                        IF DPI = '1' OR QPI = '1' OR WEL = '0' THEN
                                    --Command not supported in QPI mode
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                            MEM_CHECK <= '1';
                                            XIP_CHECK <=  '0';
                                            DUAL_CHECK <= '0';
                                           DDR_CHECK <= '0';
                                        END IF;

                                    WHEN "11011010"  => --DAh
                                        Instruct <= FAST_WRITE;
                                        IF WEL = '1' THEN
                                            bus_cycle_state := ADDRESS_BYTES;
                                            MEM_CHECK <= '1';
                                            XIP_CHECK <=  '0';
                                            DUAL_CHECK <= '0';
                                           DDR_CHECK <= '0';
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "11011101"  => --DDh
                                        Instruct <= DDR_FAST_WRITE;
                                        IF WEL = '1' THEN
                                            bus_cycle_state := ADDRESS_BYTES;
                                            MEM_CHECK <= '1';
                                            XIP_CHECK <=  '0';
                                            DUAL_CHECK <= '0';
                                           DDR_CHECK <= '1';
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "11011110"  => --DEh
                                        Instruct <= DDRWRITE;
                                        IF WEL = '1' THEN
                                            bus_cycle_state := ADDRESS_BYTES;
                                            MEM_CHECK <= '1';
                                            XIP_CHECK <=  '0';
                                            DUAL_CHECK <= '0';
                                           DDR_CHECK <= '1';
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "11101011"  => --EBh
                                        Instruct <= QIOR;
                                        IF QPI = '0' AND QUAD = '0' THEN
                                    --Command not supported in DPI mode
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                            MEM_CHECK <= '1';
                                            XIP_CHECK <=  '0';
                                            DUAL_CHECK <= '0';
                                           DDR_CHECK <= '0';
                                            Check_freq := TRUE;
                                        END IF;

                                    WHEN "11101101"  => --EDh
                                        Instruct <= DDRQIOR;
                                         IF QPI = '0' AND QUAD = '0' THEN
                                    --Command not supported in DPI mode
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                            Check_freq := TRUE;
                                            MEM_CHECK <= '1';
                                            XIP_CHECK <=  '0';
                                            DUAL_CHECK <= '0';
                                           DDR_CHECK <= '1';
                                        END IF;

                                    WHEN others =>
                                        null;

                                END CASE;
                            END IF;
                        END IF;
                    END IF;

                WHEN ADDRESS_BYTES =>
                dummy_cnt := 0;
                    IF rising_edge(SCK_ipd) AND CSNeg_ipd = '0' THEN

                        -------------------------------------
                        -- Check_freq start
                        -------------------------------------
                        IF Check_freq THEN
                            IF (Instruct=FAST_READ AND QPI = '0') OR
                            Instruct=DOR OR Instruct=QOR THEN
                                IF (CLK_PER<9.25 ns AND Latency_code>=0) THEN --108MHz
                                    ASSERT FALSE
                                    REPORT "More wait states are required for " &
                                        "this clock frequency value. " &
                                        "Instruction: " & instruction_type'image(Instruct)
                                    SEVERITY warning;
                                END IF;
                                Check_freq := FALSE;
                            END IF;
                            IF (Instruct=FAST_READ AND DPI = '1') OR
                            Instruct=DIOR THEN
                                IF (CLK_PER<18.181 ns AND Latency_code=0) OR--55MHz
                                (CLK_PER<14.285 ns AND Latency_code=1) OR--70MHz
                                (CLK_PER<12.5 ns AND Latency_code=2) OR--80MHz
                                (CLK_PER<10.526 ns AND Latency_code=3) OR --95MHz
                                (CLK_PER<9.25 ns AND Latency_code>=4) THEN --108MHz
                                    ASSERT FALSE
                                    REPORT "More wait states are required for " &
                                        "this clock frequency value. " &
                                        "Instruction: " & instruction_type'image(Instruct)
                                    SEVERITY warning;
                                END IF;
                                Check_freq := FALSE;
                            END IF;

                            IF ((Instruct=FAST_READ OR Instruct=QIOR)
                            AND QPI = '1') OR (Instruct=QIOR AND QUAD = '1')THEN
                                IF (CLK_PER<100 ns AND Latency_code=0) OR--10MHz
                                (CLK_PER<40 ns AND Latency_code=1) OR--25MHz
                                (CLK_PER<25 ns AND Latency_code=2) OR--40MHz
                                (CLK_PER<18.181 ns AND Latency_code=3) OR--55MHz
                                (CLK_PER<14.285 ns AND Latency_code=4) OR--70MHz
                                (CLK_PER<12.5 ns AND Latency_code=5) OR--80MHz
                                (CLK_PER<10.526 ns AND Latency_code=6) OR --95MHz
                                (CLK_PER<9.25 ns AND Latency_code>=7) THEN--108MHz
                                    ASSERT FALSE
                                    REPORT "More wait states are required for " &
                                        "this clock frequency value. " &
                                        "Instruction: " & instruction_type'image(Instruct)
                                    SEVERITY warning;
                                END IF;
                                Check_freq := FALSE;
                            END IF;

                            IF (Instruct=READ OR Instruct=ECCRD OR Instruct=SSRD) AND QPI='0' THEN
                                IF (CLK_PER<25 ns AND Latency_code=0) OR--40MHz
                                (CLK_PER<18.181 ns AND Latency_code=1) OR--55MHz
                                (CLK_PER<14.285 ns AND Latency_code=2) OR--70MHz
                                (CLK_PER<12.5 ns AND Latency_code=3) OR--80MHz
                                (CLK_PER<10.526 ns AND Latency_code=4) OR --95MHz
                                (CLK_PER<9.25 ns AND Latency_code>=5)THEN--108MHz
                                    ASSERT FALSE
                                    REPORT "More wait states are required for " &
                                        "this clock frequency value. " &
                                        "Instruction: " & instruction_type'image(Instruct)
                                    SEVERITY warning;
                                END IF;
                                Check_freq := FALSE;
                            END IF;

                            IF (Instruct=READ OR Instruct=ECCRD OR Instruct=SSRD) AND DPI='1' THEN
                                IF (CLK_PER<40 ns AND Latency_code=2) OR--25MHz
                                (CLK_PER<25 ns AND Latency_code=3) OR--40MHz
                                (CLK_PER<18.181 ns AND Latency_code=4) OR--55MHz
                                (CLK_PER<14.285 ns AND Latency_code=5) OR--70MHz
                                (CLK_PER<12.5 ns AND Latency_code=6) OR--80MHz
                                (CLK_PER<10.526 ns AND Latency_code=7) OR --95MHz 
                                (CLK_PER<9.25 ns AND Latency_code>=8) THEN--108MHz
                                    ASSERT FALSE
                                    REPORT "More wait states are required for " &
                                        "this clock frequency value. " &
                                        "Instruction: " & instruction_type'image(Instruct)
                                    SEVERITY warning;
                                END IF;
                                Check_freq := FALSE;
                            END IF;

                            IF (Instruct=READ OR Instruct=ECCRD OR Instruct=SSRD) AND QPI='1' THEN
                                IF (CLK_PER<100 ns AND Latency_code=2) OR--10MHz
                                (CLK_PER<40 ns AND Latency_code=3) OR--25MHz
                                (CLK_PER<25 ns AND Latency_code=4) OR--40MHz
                                (CLK_PER<18.181 ns AND Latency_code=5) OR--55MHz
                                (CLK_PER<14.285 ns AND Latency_code=6) OR--70MHz
                                (CLK_PER<12.5 ns AND Latency_code=7) OR--80MHz
                                (CLK_PER<10.526 ns AND Latency_code=8) OR --95MHz
                                (CLK_PER<9.25 ns AND Latency_code>=9) THEN--108MHz
                                    ASSERT FALSE
                                    REPORT "More wait states are required for " &
                                        "this clock frequency value. " &
                                        "Instruction: " & instruction_type'image(Instruct)
                                    SEVERITY warning;
                                END IF;
                                Check_freq := FALSE;
                            END IF;

                            IF (Instruct=DDRFR AND QPI='1') OR Instruct=DDRQIOR THEN
                                IF (CLK_PER<100 ns AND Latency_code=2) OR--10MHz
                                (CLK_PER<40 ns AND Latency_code=3) OR--25MHz
                                (CLK_PER<30 ns AND Latency_code=4) OR--33MHz
                                (CLK_PER<25 ns AND Latency_code=5) OR--40MHz
                                (CLK_PER<20 ns AND Latency_code=6) OR --50MHz
                                (CLK_PER<18.518 ns AND Latency_code>=7) THEN--54MHz
                                    ASSERT FALSE
                                    REPORT "More wait states are required for " &
                                        "this clock frequency value. " &
                                        "Instruction: " & instruction_type'image(Instruct)
                                    SEVERITY warning;
                                END IF;
                                Check_freq := FALSE;
                            END IF;

                            IF Instruct = RDSR1 OR Instruct = RDSR2 OR
                            Instruct = RDCR1 OR Instruct = RDCR2 OR
                            Instruct = RDCR4 OR Instruct = RDCR5 OR
                            Instruct = RDAR OR Instruct = RUID OR
                            Instruct = RDID OR Instruct = RDSN THEN
                                IF (CLK_PER<20 ns AND Register_Latency=0) OR--50MHz
                                (CLK_PER<9.25 ns AND Register_Latency>=1) THEN--108MHz
                                    ASSERT FALSE
                                    REPORT "More wait states are required for " &
                                        "this clock frequency value. " &
                                        "Instruction: " & instruction_type'image(Instruct)
                                    SEVERITY warning;
                                END IF;
                                Check_freq := FALSE;
                            END IF;
                        END IF; -- Check_freq
                        -------------------------------------

                        -------------------------------------
                        -- Instruction Addresses
                        -------------------------------------

                        IF Instruct = DDRFR OR Instruct = DDRQIOR OR
                           Instruct = DDRWRITE OR Instruct = DDR_FAST_WRITE THEN
                            double <= TRUE;
                        ELSE
                            double <= FALSE;
                        END IF;

                        IF (Instruct = QIOR OR Instruct = QIOW) THEN
                        -- Instruction + 3 Bytes Address + Dummy Byte
                            Address_in(4*addr_cnt) := RESETNegIn;
                            Address_in(4*addr_cnt+1) := WPNegIn;
                            Address_in(4*addr_cnt+2) := SOIn;
                            Address_in(4*addr_cnt+3) := SIIn;
                            read_cnt := 0;
                            addr_cnt := addr_cnt + 1;
                            IF addr_cnt = (3*BYTE)/4 THEN
                                addr_cnt := 0;
                                FOR I IN 23 DOWNTO 0 LOOP
                                    addr_bytes(23-i) := Address_in(i);
                                END LOOP;
                                addr_bytes(23 DOWNTO 20) := "0000";
                                Address <= to_nat(addr_bytes);
                                change_addr <= '1','0' AFTER 1 ns;
                                bus_cycle_state := MODE_BYTE;
                            END IF;
                        ELSIF (Instruct = DOR OR Instruct = QOR OR
                        Instruct = DIW OR Instruct = QIW) THEN
                            Address_in(addr_cnt) := SIIn;
                            addr_cnt := addr_cnt + 1;
                            IF addr_cnt = 3*BYTE THEN
                                addr_cnt := 0;
                                FOR I IN 23 DOWNTO 0 LOOP
                                    addr_bytes(23-i) := Address_in(i);
                                END LOOP;
                                addr_bytes(23 DOWNTO 20) := "0000";
                                Address <= to_nat(addr_bytes);
                                change_addr <= '1','0' AFTER 1 ns;
                                bus_cycle_state := MODE_BYTE;
                            END IF;
                        ELSIF Instruct = DIOR OR Instruct = DIOW THEN
                        -- DUAL I/O High Performance Read(3 Bytes Addr)
                            Address_in(2*addr_cnt) := SOIn;
                            Address_in(2*addr_cnt+1) := SIIn;
                            read_cnt := 0;
                            addr_cnt := addr_cnt + 1;
                            IF addr_cnt = (3*BYTE)/2 THEN
                                addr_cnt := 0;
                                FOR I IN 23 DOWNTO 0 LOOP
                                    addr_bytes(23-i) := Address_in(i);
                                END LOOP;
                                addr_bytes(23 downto 20):="0000";
                                Address <= to_nat(addr_bytes);
                                change_addr <= '1','0' AFTER 1 ns;
                                bus_cycle_state := MODE_BYTE;
                            END IF;
                        ELSIF (Instruct = DDRQIOR AND (QUAD = '1' OR QPI = '1')) OR 
                             (Instruct = DDRQIOW AND QUAD = '1') THEN
                            Address_in(4*addr_cnt) := RESETNegIn;
                            Address_in(4*addr_cnt+1) := WPNegIn;
                            Address_in(4*addr_cnt+2) := SOIn;
                            Address_in(4*addr_cnt+3) := SIIn;
                            read_cnt := 0;
                            addr_cnt := addr_cnt + 1;
                        ELSIF Instruct = DDRFR OR Instruct = DDRWRITE OR
                        Instruct = DDR_FAST_WRITE THEN
                            IF QPI = '1' THEN
                                Address_in(4*addr_cnt) := RESETNegIn;
                                Address_in(4*addr_cnt+1) := WPNegIn;
                                Address_in(4*addr_cnt+2) := SOIn;
                                Address_in(4*addr_cnt+3) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                            END IF;
                        ELSIF Instruct = FAST_READ OR Instruct = FAST_WRITE THEN
                        -- Instruction + 3 Bytes Address + Dummy Byte
                            IF QPI = '1' THEN
                                Address_in(4*addr_cnt) := RESETNegIn;
                                Address_in(4*addr_cnt+1) := WPNegIn;
                                Address_in(4*addr_cnt+2) := SOIn;
                                Address_in(4*addr_cnt+3) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = (3*BYTE)/4 THEN
                                    addr_cnt := 0;
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    addr_bytes(23 DOWNTO 20) := "0000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    bus_cycle_state := MODE_BYTE;
                                END IF;
                            ELSIF DPI = '1' THEN
                                Address_in(2*addr_cnt) := SOIn;
                                Address_in(2*addr_cnt+1) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = (3*BYTE)/2 THEN
                                    addr_cnt := 0;
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    addr_bytes(23 DOWNTO 20) := "0000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    bus_cycle_state := MODE_BYTE;
                                END IF;
                            ELSE
                                Address_in(addr_cnt) := SIIn;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = 3*BYTE THEN
                                    addr_cnt := 0;
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    addr_bytes(23 DOWNTO 20) := "0000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    bus_cycle_state := MODE_BYTE;
                                END IF;
                            END IF;
                        ELSIF Instruct = SSWR OR Instruct = WRITE_MEM OR
                        Instruct = WRAR THEN
                            IF QPI = '1' THEN
                                Address_in(4*addr_cnt) := RESETNegIn;
                                Address_in(4*addr_cnt+1) := WPNegIn;
                                Address_in(4*addr_cnt+2) := SOIn;
                                Address_in(4*addr_cnt+3) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = (3*BYTE)/4 THEN
                                    addr_cnt := 0;
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    addr_bytes(23 DOWNTO 20) := "0000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    bus_cycle_state := DATA_BYTES;
                                END IF;
                            ELSIF DPI = '1' THEN
                                Address_in(2*addr_cnt) := SOIn;
                                Address_in(2*addr_cnt+1) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = (3*BYTE)/2 THEN
                                    addr_cnt := 0;
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    addr_bytes(23 DOWNTO 20) := "0000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    bus_cycle_state := DATA_BYTES;
                                END IF;
                            ELSE
                                Address_in(addr_cnt) := SIIn;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = 3*BYTE THEN
                                    addr_cnt := 0;
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    addr_bytes(23 DOWNTO 20) := "0000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    bus_cycle_state := DATA_BYTES;
                                END IF;
                            END IF;
                        ELSIF Instruct = CRCC THEN
                        -- Instruction + 3 Bytes Address
                            IF QPI = '1' THEN
                                Address_in(4*(addr_cnt MOD 6)) := RESETNegIn;
                                Address_in(4*(addr_cnt MOD 6)+1) := WPNegIn;
                                Address_in(4*(addr_cnt MOD 6)+2) := SOIn;
                                Address_in(4*(addr_cnt MOD 6)+3) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = (3*BYTE)/4 THEN
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                END IF;
                                IF addr_cnt = (6*BYTE)/4 THEN
                                    addr_cnt := 0;
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    bus_cycle_state := DATA_BYTES;
                                END IF;
                            ELSIF DPI = '1' THEN
                                Address_in(2*(addr_cnt MOD 12)) := SOIn;
                                Address_in(2*(addr_cnt MOD 12)+1) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = (3*BYTE)/2 THEN
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                END IF;
                                IF addr_cnt = (6*BYTE)/2 THEN
                                    addr_cnt := 0;
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    bus_cycle_state := DATA_BYTES;
                                END IF;
                            ELSE
                                Address_in(addr_cnt MOD 24) := SIIn;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = 3*BYTE THEN
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                END IF;
                                IF addr_cnt = 6*BYTE THEN
                                    addr_cnt := 0;
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    bus_cycle_state := DATA_BYTES;
                                END IF;
                            END IF;
                            CRC_End_Addr_reg := to_nat(addr_bytes);
                        ELSIF Instruct = READ THEN
                        -- Instruction + 3 Bytes Address + Dummy Byte
                            IF QPI = '1' THEN
                                Address_in(4*addr_cnt) := RESETNegIn;
                                Address_in(4*addr_cnt+1) := WPNegIn;
                                Address_in(4*addr_cnt+2) := SOIn;
                                Address_in(4*addr_cnt+3) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = (3*BYTE)/4 THEN
                                    addr_cnt := 0;
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    addr_bytes(23 DOWNTO 20) := "0000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF Register_Latency = 0 THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                END IF;
                            ELSIF DPI = '1' THEN
                                Address_in(2*addr_cnt) := SOIn;
                                Address_in(2*addr_cnt+1) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = (3*BYTE)/2 THEN
                                    addr_cnt := 0;
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    addr_bytes(23 DOWNTO 20) := "0000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF Register_Latency = 0 THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                END IF;
                            ELSE
                                Address_in(addr_cnt) := SIIn;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = 3*BYTE THEN
                                    addr_cnt := 0;
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    addr_bytes(23 DOWNTO 20) := "0000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF Register_Latency = 0 THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                END IF;
                            END IF;
                        ELSE  -- All other instructions
                        -- Instruction + 3 Bytes Address + Dummy Byte
                            IF QPI = '1' THEN
                                Address_in(4*addr_cnt) := RESETNegIn;
                                Address_in(4*addr_cnt+1) := WPNegIn;
                                Address_in(4*addr_cnt+2) := SOIn;
                                Address_in(4*addr_cnt+3) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = (3*BYTE)/4 THEN
                                    addr_cnt := 0;
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    addr_bytes(23 DOWNTO 20) := "0000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF Register_Latency = 0 THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                END IF;
                            ELSIF DPI = '1' THEN
                                Address_in(2*addr_cnt) := SOIn;
                                Address_in(2*addr_cnt+1) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = (3*BYTE)/2 THEN
                                    addr_cnt := 0;
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    addr_bytes(23 DOWNTO 20) := "0000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF Register_Latency = 0 THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                END IF;
                            ELSE
                                Address_in(addr_cnt) := SIIn;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = 3*BYTE THEN
                                    addr_cnt := 0;
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    addr_bytes(23 DOWNTO 20) := "0000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF Register_Latency = 0 THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;

                    ELSIF falling_edge(SCK_ipd) AND CSNeg_ipd = '0' THEN
                        IF ((Instruct = DDRFR) OR (Instruct = DDRWRITE) OR
                        (Instruct = DDR_FAST_WRITE)) THEN
                        
                            IF (QPI = '1') THEN
                            
                                Address_in(4*addr_cnt)   := RESETNegIn;
                                Address_in(4*addr_cnt+1) := WPNegIn;
                                Address_in(4*addr_cnt+2) := SOIn;
                                Address_in(4*addr_cnt+3) := SIIn;
                                read_cnt := 0;
                                IF (addr_cnt /= 0) THEN
                                
                                    addr_cnt := addr_cnt + 1;
                                END IF;
                                IF (addr_cnt = 3*BYTE/4) THEN
                                
                                    addr_cnt := 0;
                                    FOR I IN 23 DOWNTO 0 LOOP
                                      addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    addr_bytes(23 DOWNTO 20) := "0000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;                        
                                IF (Instruct = DDRWRITE) THEN
                                    bus_cycle_state := DATA_BYTES;
                                ELSE
                                    bus_cycle_state := MODE_BYTE;
                                END IF;
                                
                               END IF;    
                            END IF;
                        --END IF;
                        ELSIF Instruct = DDRQIOR AND (QUAD = '1' OR QPI = '1') THEN
                        
                        --Quad I/O DDR Read Mode (3 Bytes Address)
                            Address_in(4*addr_cnt)   := RESETNegIn;
                            Address_in(4*addr_cnt+1) := WPNegIn;
                            Address_in(4*addr_cnt+2) := SOIn;
                            Address_in(4*addr_cnt+3) := SIIn;
                            IF (addr_cnt /= 0) THEN
                            
                                addr_cnt := addr_cnt + 1;
                            END IF;
                            read_cnt := 0;
                            IF (addr_cnt = 3*BYTE/4) THEN
                            
                                addr_cnt := 0;
                                FOR I IN 23 DOWNTO 0 LOOP
                                  addr_bytes(23-i) := Address_in(i);
                                END LOOP;
                                addr_bytes(23 DOWNTO 20) := "0000";
                                Address <= to_nat(addr_bytes);
                                change_addr <= '1','0' AFTER 1 ns;
                                
                                bus_cycle_state := MODE_BYTE;
                             END IF;
                        --END IF;
                        ELSIF ((Instruct = DDRQIOW) AND QUAD='1') THEN
                        
                            Address_in(4*addr_cnt)   := RESETNegIn;
                            Address_in(4*addr_cnt+1) := WPNegIn;
                            Address_in(4*addr_cnt+2) := SOIn;
                            Address_in(4*addr_cnt+3) := SIIn;
                            IF (addr_cnt /= 0) THEN
                            
                                addr_cnt := addr_cnt + 1;
                            END IF;
                            read_cnt := 0;
                            IF (addr_cnt = 3*BYTE/4) THEN
                            
                                addr_cnt := 0;
                                FOR I IN 23 DOWNTO 0 LOOP
                                  addr_bytes(23-i) := Address_in(i);
                                END LOOP;
                                addr_bytes(23 DOWNTO 20) := "0000";
                                Address <= to_nat(addr_bytes);
                                change_addr <= '1','0' AFTER 1 ns;
                                
                                bus_cycle_state := MODE_BYTE;
                            END IF;
                        
                        ELSIF (Instruct = CRCC) THEN
                        
                            IF (((addr_cnt = 3*BYTE/4) AND QPI = '1') OR
                                ((addr_cnt = 3*BYTE/2) AND QPI = '1') OR
                                ((addr_cnt = 3*BYTE) AND (QPI = '0'))) THEN
                                CRC_Start_Addr_reg := Address;
                            END IF;
                        END IF; 
                   END IF; --end of ADDRESS_BYTES

                WHEN MODE_BYTE =>
                    data_cnt := 0;
                    dummy_cnt := 0;
                    IF rising_edge(SCK_ipd) AND CSNeg = '0' THEN
                        IF Instruct = DIOR OR Instruct = DIOW OR
                        (Instruct = FAST_READ AND DPI = '1') THEN
                            mode_in(2*mode_cnt)   := SOIn;
                            mode_in(2*mode_cnt+1) := SIIn;
                            mode_cnt := mode_cnt + 1;
                            IF mode_cnt = BYTE/2 THEN
                                mode_cnt := 0;
                                FOR I IN 7 DOWNTO 0 LOOP
                                    mode_bytes(i) := mode_in(7-i);
                                END LOOP;
                                IF Latency_code = 0 OR Instruct = DIOW THEN
                                    bus_cycle_state := DATA_BYTES;
                                ELSE
                                    bus_cycle_state := DUMMY_BYTES;
                                END IF;
                            END IF;
                        ELSIF (Instruct = QIOR AND (QUAD = '1' OR QPI = '1')) OR
                        (Instruct = FAST_READ AND QPI = '1') THEN
                            mode_in(4*mode_cnt)   := RESETNegIn;
                            mode_in(4*mode_cnt+1) := WPNegIn;
                            mode_in(4*mode_cnt+2) := SOIn;
                            mode_in(4*mode_cnt+3) := SIIn;
                            mode_cnt := mode_cnt + 1;
                            IF mode_cnt = BYTE/4 THEN
                                mode_cnt := 0;
                                FOR I IN 7 DOWNTO 0 LOOP
                                    mode_bytes(i) := mode_in(7-i);
                                END LOOP;
                                IF Latency_code = 0 THEN
                                    bus_cycle_state := DATA_BYTES;
                                ELSE
                                    bus_cycle_state := DUMMY_BYTES;
                                END IF;
                            END IF;
                        ELSIF Instruct = QIOW AND QUAD = '1' THEN
                            mode_in(4*mode_cnt)   := RESETNegIn;
                            mode_in(4*mode_cnt+1) := WPNegIn;
                            mode_in(4*mode_cnt+2) := SOIn;
                            mode_in(4*mode_cnt+3) := SIIn;
                            mode_cnt := mode_cnt + 1;
                            IF mode_cnt = BYTE/4 THEN
                                mode_cnt := 0;
                                FOR I IN 7 DOWNTO 0 LOOP
                                    mode_bytes(i) := mode_in(7-i);
                                END LOOP;
                                bus_cycle_state := DATA_BYTES;
                            END IF;
                        ELSIF Instruct = DIW OR Instruct = QIW OR
                        (Instruct = FAST_READ AND QPI = '0')THEN
                            mode_in(mode_cnt) := SIIn;
                            mode_cnt := mode_cnt + 1;
                            IF mode_cnt = BYTE THEN
                                mode_cnt := 0;
                                FOR I IN 7 DOWNTO 0 LOOP
                                    mode_bytes(i) := mode_in(7-i);
                                END LOOP;
                                IF Instruct /= FAST_READ THEN
                                    bus_cycle_state := DATA_BYTES;
                                ELSE
                                    IF Latency_code = 0 THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                END IF;
                            END IF;
                        ELSIF Instruct = DOR OR Instruct = QOR THEN
                            mode_in(mode_cnt) := SIIn;
                            mode_cnt := mode_cnt + 1;
                            IF mode_cnt = BYTE THEN
                                mode_cnt := 0;
                                FOR I IN 7 DOWNTO 0 LOOP
                                    mode_bytes(i) := mode_in(7-i);
                                END LOOP;
                                IF Latency_code = 0 THEN
                                    bus_cycle_state := DATA_BYTES;
                                ELSE
                                    bus_cycle_state := DUMMY_BYTES;
                                END IF;
                            END IF;
                        ELSIF Instruct = FAST_WRITE THEN
                          IF QPI = '1' THEN
                                mode_in(4*mode_cnt)   := RESETNegIn;
                                mode_in(4*mode_cnt+1) := WPNegIn;
                                mode_in(4*mode_cnt+2) := SOIn;
                                mode_in(4*mode_cnt+3) := SIIn;
                                mode_cnt := mode_cnt + 1;
                                IF mode_cnt = BYTE/4 THEN
                                   mode_cnt := 0;
                                   FOR I IN 7 DOWNTO 0 LOOP
                                     mode_bytes(i) := mode_in(7-i);
                                   END LOOP;
                                   bus_cycle_state := DATA_BYTES;
                                END IF;
                          ELSIF DPI = '1' THEN
                                 mode_in(2*mode_cnt)   := SOIn;
                                 mode_in(2*mode_cnt+1) := SIIn;
                                 mode_cnt := mode_cnt + 1;
                                 IF mode_cnt = BYTE/2 THEN
                                     mode_cnt := 0;
                                     FOR I IN 7 DOWNTO 0 LOOP
                                       mode_bytes(i) := mode_in(7-i);
                                     END LOOP;
                                    IF Latency_code = 0 OR Instruct = DIOW THEN
                                         bus_cycle_state := DATA_BYTES;
                                    ELSE
                                         bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                 END IF;
                          ELSE
                                mode_in(mode_cnt) := SIIn;
                                mode_cnt := mode_cnt + 1;
                                IF mode_cnt = BYTE THEN
                                    mode_cnt := 0;
                                     FOR I IN 7 DOWNTO 0 LOOP
                                        mode_bytes(i) := mode_in(7-i);
                                     END LOOP;
--                                 END IF;
                                IF Latency_code = 0  THEN --IF Latency_code = 0 OR Instruct = DIOW
                                    bus_cycle_state := DATA_BYTES;
                                ELSE
                                    bus_cycle_state := DUMMY_BYTES;
                                END IF;
                                END IF;
                           END IF;
                        ELSIF Instruct = DDRFR THEN
                                mode_in(0) := RESETNegIn;
                                mode_in(1) := WPNegIn;
                                mode_in(2) := SOIn;
                                mode_in(3) := SIIn;
                        ELSIF ((Instruct = DDRQIOR AND (QUAD = '1' OR QPI = '1')) 
                               OR (Instruct = DDRQIOW AND QUAD = '1')) THEN
                            mode_in(0) := RESETNegIn;
                            mode_in(1) := WPNegIn;
                            mode_in(2) := SOIn;
                            mode_in(3) := SIIn;
                        ELSIF (Instruct = DDR_FAST_WRITE) THEN
                            IF QPI = '1' THEN
                                mode_in(4) := RESETNegIn;
                                mode_in(5) := WPNegIn;
                                mode_in(6) := SOIn;
                                mode_in(7) := SIIn;
                            ELSIF DPI = '1' THEN
                                mode_in(4*mode_cnt+2) := SOIn;
                                mode_in(4*mode_cnt+3) := SIIn;
                            ELSE
                                mode_in(2*mode_cnt+1) := SIIn;
                            END IF;
--                          END IF;
                        dummy_cnt := 0;
                       END IF;
                    ELSIF falling_edge(SCK_ipd) AND CSNeg = '0' THEN 
                        IF (Instruct = DDRFR) THEN
                        
                            IF QPI = '1' THEN
                            
                                mode_in(4) := RESETNegIn;
                                mode_in(5) := WPNegIn;
                                mode_in(6) := SOIn;
                                mode_in(7) := SIIn;
                                FOR I IN 7 DOWNTO 0 LOOP
                                         mode_bytes(i) := mode_in(7-i);
                                END LOOP;                                
                                IF Latency_code = 0  THEN --IF Latency_code = 0 OR Instruct = DIOW
                                    bus_cycle_state := DATA_BYTES;
                                    read_out <= '1', '0' AFTER 1 ns;
                                ELSE
                                    bus_cycle_state := DUMMY_BYTES;
                                END IF;
                            END IF; 
                        ELSIF Instruct = DDRQIOR  AND (QPI = '1' OR QUAD= '1') THEN
                        
                            mode_in(4) := RESETNegIn;
                            mode_in(5) := WPNegIn;
                            mode_in(6) := SOIn;
                            mode_in(7) := SIIn;
                            FOR I IN 7 DOWNTO 0 LOOP
                                         mode_bytes(i) := mode_in(7-i);
                            END LOOP;

                            IF Latency_code = 0 THEN
                                bus_cycle_state := DATA_BYTES;
                                read_out <= '1', '0' AFTER 1 ns;
                            ELSE
                                bus_cycle_state := DUMMY_BYTES;
                            END IF;
                        ELSIF  (Instruct = DDRQIOW AND QUAD= '1') THEN
                        
                            mode_in(4) := RESETNegIn;
                            mode_in(5) := WPNegIn;
                            mode_in(6) := SOIn;
                            mode_in(7) := SIIn;
                            FOR I IN 7 DOWNTO 0 LOOP
                                         mode_bytes(i) := mode_in(7-i);
                            END LOOP;

                                bus_cycle_state := DATA_BYTES;
                                
                        ELSIF (Instruct = DDR_FAST_WRITE) THEN
                            IF QPI = '1' THEN
                                mode_in(4) := RESETNegIn;
                                mode_in(5) := WPNegIn;
                                mode_in(6) := SOIn;
                                mode_in(7) := SIIn;
                                bus_cycle_state := DATA_BYTES;
                            ELSIF DPI = '1' THEN
                                mode_in(4*mode_cnt+2) := SOIn;
                                mode_in(4*mode_cnt+3) := SIIn;
                                mode_cnt := mode_cnt + 1;
                                IF (mode_cnt = BYTE/4) THEN
                                    mode_cnt := 0;
                                    FOR I IN 7 DOWNTO 0 LOOP
                                      mode_bytes(i) := mode_in(7-i);
                                    END LOOP;
                                bus_cycle_state := DATA_BYTES;
                                END IF;
                            ELSE
                                mode_in(2*mode_cnt+1) := SIIn;
                                mode_cnt := mode_cnt + 1;
                                IF (mode_cnt = BYTE/2) THEN
                                    mode_cnt := 0;
                                    FOR I IN 7 DOWNTO 0 LOOP
                                      mode_bytes(i) := mode_in(7-i);
                                    END LOOP;
                                bus_cycle_state := DATA_BYTES;
                                END IF;
                            END IF;
                        END IF;       --end of MODE_BYTE
                    END IF;

                WHEN DUMMY_BYTES =>
                    IF rising_edge(SCK_ipd) AND CSNeg = '0' THEN
                        
                         
                        dummy_cnt := dummy_cnt + 1;


                    ELSIF falling_edge(SCK_ipd) AND CSNeg = '0' THEN
                        dummy_cnt := dummy_cnt + 1;

                        IF ((Instruct = RDSR1) OR (Instruct = RDSR2) OR
                            (Instruct = RDCR1) OR (Instruct = RDCR2) OR
                            (Instruct = RDCR4) OR (Instruct = RDCR5) OR
                            (Instruct = RDID)  OR (Instruct = RDSN)  OR
                            (Instruct = RUID)  OR (Instruct = RDAR)) THEN
                        
                            IF (Register_Latency = dummy_cnt/2) THEN
                            
                                bus_cycle_state := DATA_BYTES;
                                read_out <= '1', '0' AFTER 1 ns;
                                dummy_cnt := 0;
                            END IF;

                        ELSIF (Latency_code = dummy_cnt/2) THEN
                            
                                bus_cycle_state := DATA_BYTES;
                                read_out <= '1', '0' AFTER 1 ns;
                                dummy_cnt := 0;
                        END IF;
                    END IF;

                WHEN DATA_BYTES =>

                    Check_freq := FALSE;

                    IF rising_edge(SCK_ipd) AND CSNeg = '0' THEN
                        IF Instruct = DDRQIOR OR Instruct = DDRFR THEN
                            read_out <= '1', '0' AFTER 1 ns;
                        END IF;
                        IF (Instruct = DDRWRITE OR Instruct = DDR_FAST_WRITE
                             OR Instruct = DDRQIOW ) THEN
                             
                                    quad_nybble := RESETNegIn & WPNegIn & SOIn & SIIn;
                                    Data_in_8(7 - 4*(data_cnt MOD 2)) := RESETNegIn;
                                    Data_in_8(6 - 4*(data_cnt MOD 2)) := WPNegIn;
                                    Data_in_8(5 - 4*(data_cnt MOD 2)) := SOIn;
                                    Data_in_8(4 - 4*(data_cnt MOD 2)) := SIIn;
                                    data_cnt := data_cnt + 1;
                            ELSIF (Instruct = WRITE_MEM OR Instruct = FAST_WRITE) THEN
                               IF QPI = '1' THEN
                                       quad_nybble := RESETNegIn & WPNegIn & SOIn & SIIn;
                                       Data_in_8(7 - 4*(data_cnt MOD 2)) := RESETNegIn;
                                       Data_in_8(6 - 4*(data_cnt MOD 2)) := WPNegIn;
                                       Data_in_8(5 - 4*(data_cnt MOD 2)) := SOIn;
                                       Data_in_8(4 - 4*(data_cnt MOD 2)) := SIIn;
                                       bit_cnt := 0;
                                     IF ((data_cnt MOD 2) = 1) THEN
                                           Data_in_8(3 DOWNTO 0) := quad_nybble;
                                           Data_Byte <= '1';
                                           sect := Address / (SecSize+1);
                                           IF Sec_Prot(sect)= '0' THEN
                                               Mem(Address) := to_nat(Data_in_8);
                                           END IF;
                                           IF ((Address) = MemSize) THEN
                                               Address <= 0;
                                           ELSE
                                               Address <= Address + 1;
                                           END IF;
                                           Byte_slv := Data_in_8;
                                     ELSE
                                           Data_in_8(7 DOWNTO 4) := quad_nybble;
                                           Data_Byte <= '0';
                                     END IF;
                                           data_cnt := data_cnt + 1;                    
                            ELSIF DPI = '1' THEN
--                                     Data_in(data_cnt) := SIIn;
                                    Data_in_8(7 - 2*(data_cnt MOD 4)) := SOIn;
                                    Data_in_8(6 - 2*(data_cnt MOD 4)) := SIIn;
                                    bit_cnt := 0;
                                    IF ((data_cnt MOD 4) = 3) THEN
                                        Data_Byte <= '1';
                                        sect := Address / (SecSize+1);
                                        IF Sec_Prot(sect)= '0' THEN
                                            Mem(Address) := to_nat(Data_in_8);
                                        END IF;
                                        IF (Address = MemSize) THEN
                                            Address <= 0;
                                        ELSE
                                        
                                            Address <= Address + 1;
                                        END IF;
                                        Byte_slv := Data_in_8;

                                    ELSE
                                    
                                        Data_Byte <= '0';
                                    END IF;
                                    data_cnt := data_cnt + 1;
   
                            ELSE
                                

                                    Data_in_8(7 - (data_cnt MOD 8)) := SIIn;
                                    data_cnt := data_cnt + 1;
                                    bit_cnt := 0;
                                    IF ((data_cnt MOD 8) = 0) THEN
                                    
                                        Data_Byte <= '1';
                                        sect := Address / (SecSize+1);
                                        IF Sec_Prot(sect)= '0' THEN
                                            Mem(Address) := to_nat(Data_in_8);
                                        END IF;
                                        IF ((Address) = MemSize) THEN
                                        
                                            Address <= 0;
                                        ELSE
                                        
                                            Address <= Address + 1;
                                        END IF;
                                        Byte_slv := Data_in_8;

                                    ELSE
                                    
                                        Data_Byte <= '0';
                                    END IF;

                            END IF;
                        ELSIF (Instruct = SSWR) THEN
                        
                            IF (QPI = '1') THEN
                            
                                quad_nybble := RESETNegIn & WPNegIn & SOIn & SIIn;
                                data_cnt := data_cnt + 1;
                                bit_cnt := 0;
                                IF ((data_cnt MOD 2) = 0) THEN
                                
                                    Data_in_8(7 downto 4) := quad_nybble;
                                    Data_Byte <= '1';
                                    IF (Address <= 16#FF#) THEN
                                    
                                        OTPMem(Address) := to_nat(Data_in_8);
                                        Address <= Address + 1;
                                    END IF;
                                    Byte_slv := Data_in_8;
                                ELSE
                                
                                    Data_in_8(3 downto 0) := quad_nybble;
                                    Data_Byte <= '0';
                                END IF;
                                
                            ELSIF (DPI = '1') THEN
                            

                                Data_in_8(7 - 2*(data_cnt MOD 4)) := SOIn;
                                Data_in_8(6 - 2*(data_cnt MOD 4)) := SIIn;
                                bit_cnt := 0;
                                IF ((data_cnt MOD 4) = 3) THEN
                                
                                    Data_Byte <= '1';
                                    IF (Address <= 16#FF#) THEN
                                    
                                        OTPMem(Address) := to_nat(Data_in_8);
                                        Address <= Address + 1;
                                    END IF;
                                    Byte_slv := Data_in_8;
                                ELSE
                                
                                    Data_Byte <= '0';

                                END IF;
                                
                                data_cnt := data_cnt + 1;
                            ELSE
                            
                                Data_in(data_cnt) := SIIn;
                                Data_in_8(7 - (data_cnt MOD 8)) := SIIn;
                                data_cnt := data_cnt + 1;
                                bit_cnt := 0;
                                IF ((data_cnt MOD 8) = 0) THEN
                                
                                    Data_Byte <= '1';
                                    IF (Address <= 16#FF#) THEN
                                    
                                        OTPMem(Address) := to_nat(Data_in_8);
                                        Address <= Address + 1;
                                    END IF;
                                    Byte_slv := Data_in_8;
                                ELSE
                                
                                    Data_Byte <= '0';

                                END IF;
                             END IF;
                        ELSIF ((Instruct = DIW) OR (Instruct = DIOW) ) THEN
                        

                            Data_in_8(7 - 2*(data_cnt MOD 4)) := SOIn;
                            Data_in_8(6 - 2*(data_cnt MOD 4)) := SIIn;
                            bit_cnt := 0;
                            IF ((data_cnt MOD 4) = 3) THEN
                            
                                Data_Byte <= '1';
                                sect := Address / (SecSize+1);
                                IF Sec_Prot(sect)= '0' THEN
                                    Mem(Address) := to_nat(Data_in_8);
                                END IF;
                                IF (Address = MemSize) THEN
                                
                                    Address <= 0;
                                ELSE
                                
                                    Address <= Address + 1;
                                END IF;
                                Byte_slv := Data_in_8;
                            
                            ELSE
                            
                                Data_Byte <= '0';
                            END IF;
                            
                            data_cnt := data_cnt + 1;
                        
                        ELSIF ( (Instruct = QIW) OR (Instruct = QIOW)) THEN
                        
                                      quad_nybble := RESETNegIn & WPNegIn & SOIn & SIIn;
                                       Data_in_8(7 - 4*(data_cnt MOD 2)) := RESETNegIn;
                                       Data_in_8(6 - 4*(data_cnt MOD 2)) := WPNegIn;
                                       Data_in_8(5 - 4*(data_cnt MOD 2)) := SOIn;
                                       Data_in_8(4 - 4*(data_cnt MOD 2)) := SIIn;
                                       bit_cnt := 0;
                                   IF ((data_cnt MOD 2) = 1) THEN
                                           Data_in_8(3 DOWNTO 0) := quad_nybble;
                                           Data_Byte <= '1';
                                           sect := Address / (SecSize+1);
                                           IF Sec_Prot(sect)= '0' THEN
                                               Mem(Address) := to_nat(Data_in_8);
                                           END IF;
                                           IF ((Address) = MemSize) THEN
                                               Address <= 0;
                                           ELSE
                                               Address <= Address + 1;
                                           END IF;
                                           Byte_slv := Data_in_8;
                                   ELSE
                                           Data_in_8(7 DOWNTO 4) := quad_nybble;
                                           Data_Byte <= '0';
                                   END IF;
                                           data_cnt := data_cnt + 1;

                        ELSIF (Instruct = WRSR OR Instruct = WRAR) THEN
                           IF QPI = '1' THEN
                             Data_in_8(7 - 4*(data_cnt MOD 2)) := RESETNegIn;
                             Data_in_8(6 - 4*(data_cnt MOD 2)) := WPNegIn;
                             Data_in_8(5 - 4*(data_cnt MOD 2)) := SOIn;
                             Data_in_8(4 - 4*(data_cnt MOD 2)) := SIIn;
                             data_cnt := data_cnt + 1;
                           ELSIF DPI = '1' THEN
                             Data_in_8(7 - 2*(data_cnt MOD 4)) := SOIn;
                             Data_in_8(6 - 2*(data_cnt MOD 4)) := SIIn;
                             data_cnt := data_cnt + 1;
                           ELSE
                             Data_in_8(7 - (data_cnt MOD 8)) := SIIn;
                             data_cnt := data_cnt + 1;
                           END IF;
                        END IF;
                    END IF;
                    
                    
                    IF falling_edge(SCK_ipd) AND CSNeg_ipd = '0' THEN
                      IF ((((Instruct = DDRQIOR) OR (Instruct = QIOR)) AND 
                            (QPI = '1' OR QUAD= '1')) OR
                            (Instruct = READ)     OR (Instruct = FAST_READ) OR
                            (Instruct = RDSR1)    OR (Instruct = RDSR2) OR
                            (Instruct = RDCR1)    OR (Instruct = RDCR2) OR
                            (Instruct = RDCR4)    OR (Instruct = RDCR5) OR
                            (Instruct = RUID)     OR (Instruct = RDID)  OR
                            (Instruct = DIOR)     OR (Instruct = RDSN)  OR
                            (Instruct = SSRD)     OR (Instruct = DOR)   OR
                            (Instruct = QOR)      OR (Instruct = DDRFR) OR
                            (Instruct = ECCRD)    OR (Instruct = RDAR)) THEN
                        
                            read_out <= '1', '0' AFTER 1 ns;
                        --END IF;
--                        ELSIF (Instruct = CRCC) THEN
--                             CRC_End_Addr_reg := Address;
                       ELSIF (Instruct = DDRWRITE) OR (Instruct = DDR_FAST_WRITE) OR
                       (Instruct = DDRQIOW) THEN
                                       quad_nybble := RESETNegIn & WPNegIn & SOIn & SIIn;
                                       Data_in_8(7 - 4*(data_cnt MOD 2)) := RESETNegIn;
                                       Data_in_8(6 - 4*(data_cnt MOD 2)) := WPNegIn;
                                       Data_in_8(5 - 4*(data_cnt MOD 2)) := SOIn;
                                       Data_in_8(4 - 4*(data_cnt MOD 2)) := SIIn;
                                       bit_cnt := 0;
                                   IF ((data_cnt MOD 2) = 1) THEN
                                           Data_in_8(3 DOWNTO 0) := quad_nybble;
                                           Data_Byte <= '1';
                                           sect := Address / (SecSize+1);
                                           IF Sec_Prot(sect)= '0' THEN
                                               Mem(Address) := to_nat(Data_in_8);
                                           END IF;
                                           IF ((Address) = MemSize) THEN
                                               Address <= 0;
                                           ELSE
                                               Address <= Address + 1;
                                           END IF;
                                           Byte_slv := Data_in_8;
                                   ELSE
                                           Data_Byte <= '0';
                                   END IF;
                                           data_cnt := data_cnt + 1;
                    END IF; --end of DATA_BYTES
             END IF;

        IF rising_edge(CSNeg_ipd) THEN
                
            IF (mode_bytes(7 downto 4) = "1010" AND
                        (Instruct = FAST_WRITE OR Instruct = DIW OR
                        Instruct = DIOW OR Instruct = QIW OR
                        Instruct = QIOW OR Instruct = FAST_READ OR
                        Instruct = DOR OR Instruct = DIOR OR
                        Instruct = QOR OR Instruct = QIOR)) OR
                        (mode_bytes = "10100101" AND
                        (Instruct = DDR_FAST_WRITE OR
                        Instruct = DDRQIOW OR Instruct = DDRFR OR
                        Instruct = DDRQIOR)) THEN
                            bus_cycle_state := ADDRESS_BYTES;
                        XIP_CHECK <=  '1';
            
            ELSE 
                    bus_cycle_state := STAND_BY;
                    XIP_CHECK <=  '0';
            END IF;
                        
                   CASE Instruct IS
                            WHEN WREN | WRDI | DPD | HBN |
                            RSTEN | RSTCMD | 
                            EPCS | EPCR | CRCC =>
                                IF data_cnt = 0 THEN
                                    write <= '0';
                                END IF;
                            
                            WHEN SSWR =>
                                    write <= '0';
                              
                                
                                
                            WHEN READ =>
                                IF dpd_act THEN
                                    write <= '0';
                                END IF;

                            WHEN WRSR =>
                                IF QPI = '1' THEN
                                    IF data_cnt = 2 THEN
                                       write <= '0';
                                       SR1_in <= Data_in_8;
                                    END IF;
                                ELSIF  DPI = '1' THEN
                                     IF data_cnt = 4 THEN
                                        write <= '0';
                                        SR1_in <= Data_in_8;
                                     END IF;
                                ELSE
                                      IF data_cnt = 8 THEN
                                        write <= '0';
                                        SR1_in <= Data_in_8;
                                       END IF;
                                 END IF;
                                 
                            WHEN WRAR =>
                                IF QPI = '1' THEN
                                    IF data_cnt = 2 THEN
                                        write <= '0';
                                        WRAR_reg_in := Data_in_8;
                                    END IF;    
                                ELSIF DPI = '1' THEN
                                    IF data_cnt = 4 THEN
                                        write <= '0';
                                        WRAR_reg_in := Data_in_8;
                                    END IF;
                                ELSE
                                    IF data_cnt = 8 THEN
                                        write <= '0';
                                        WRAR_reg_in := Data_in_8;
                                    END IF;
                                END IF;
                                
                            
                            WHEN WRSN =>
                                IF QPI = '1' THEN
                                    IF data_cnt = 16 THEN
                                        write <= '0';
                                        FOR J IN 15 DOWNTO 0 LOOP
                                            Quad_slv := to_slv(quad_data_in(15-J), 4);
                                            SERNUM_reg(4*J+3 downto 4*J)
                                                    <= Quad_slv;
                                        END LOOP;
                                    END IF;
                                ELSIF DPI = '1' THEN
                                    IF data_cnt = 32 THEN
                                        write <= '0';
                                        FOR J IN 31 DOWNTO 0 LOOP
                                            SERNUM_reg(2*J+1 downto 2*J)
                                                    <= SOIn & SIIn;
                                        END LOOP;
                                    END IF;
                                ELSE
                                    IF data_cnt = 64 THEN
                                        write <= '0';
                                        FOR J IN 1 TO 8 LOOP
                                            FOR K IN 1 TO 8 LOOP
                                                SERNUM_reg(J*8-K) <=
                                                    Data_in(8*(J-1)+K-1);
                                            END LOOP;
                                        END LOOP;
                                    END IF;
                                END IF;
                                
                             WHEN others =>
                                null;
                                
                    END CASE;
                END IF;
            END CASE;
        END IF; -- end of rising_edge_CSNeg_ipd

    END PROCESS BusCycleDecode;

    ---------------------------------------------------------------------------
    -- Timing control for the Write Status Register
    ---------------------------------------------------------------------------
    WriteTime : PROCESS(WSTART, reseted)
        VARIABLE wob      : time;
    BEGIN
        IF LongTimming THEN
            wob  := 1 ns;
        ELSE
            wob  := 1 ns;
        END IF;
        IF rising_edge(reseted) THEN
            WDONE <= '1';  -- reset done, programing terminated
        ELSIF reseted = '1' THEN
            IF rising_edge(WSTART) AND WDONE = '1' THEN
                WDONE <= '0', '1' AFTER wob;
            END IF;
        END IF;

    END PROCESS WriteTime;

    ---------------------------------------------------------------------------
    -- Timing control for the Program Operation
    ---------------------------------------------------------------------------
    ProgTime : PROCESS(PSTART, reseted)
        VARIABLE pob      : time;
    BEGIN
        pob  := 1 ns;

        IF rising_edge(reseted) THEN
            PDONE <= '1';  -- reset done, programing terminated
        ELSIF reseted = '1' THEN
            IF rising_edge(PSTART) AND PDONE = '1' THEN
                PDONE <= '0', '1' AFTER pob;
            END IF;
        END IF;

    END PROCESS ProgTime;

    ---------------------------------------------------------------------------
    -- Timing control for the suspend process
    ---------------------------------------------------------------------------
    Start_T1_time : PROCESS (START_T1_in)
    BEGIN
        IF rising_edge(START_T1_in) THEN
            IF CRC_ACT = '1' THEN
                sSTART_T1 <= '0', '1' AFTER tdevice_CRCSL;
            END IF;
        ELSE
            sSTART_T1 <= '0';
        END IF;
    END PROCESS Start_T1_time;

    ---------------------------------------------------------------------------
    -- Timing control for the CRC calculation
    ---------------------------------------------------------------------------
    CRCTime : PROCESS (reseted, CRCSTART)
        VARIABLE elapsed_crc  : time;
        VARIABLE crc_duration : time;
        VARIABLE start_crc    : time;
    BEGIN
        IF rising_edge(reseted) THEN
            CRCDONE <= '1';
        ELSIF reseted = '1' THEN
            IF rising_edge(CRCSTART) AND CRCDONE = '1' THEN
                crc_duration := tdevice_CRCSETUP;
                elapsed_crc := 0 ns;
                CRCDONE <= '0' , '1' AFTER crc_duration;
                start_crc := NOW;
            ELSIF rising_edge(CRCSUSP) AND CRCDONE = '0' THEN
                elapsed_crc  := NOW - start_crc;
                crc_duration := crc_duration - elapsed_crc;
                CRCDONE <= '0';
            ELSIF rising_edge(CRCRES) AND CRCDONE = '0' THEN
                start_crc := NOW;
                CRCDONE <= '0', '1' AFTER crc_duration;
            END IF;
        END IF;
    END PROCESS CRCTime;

    CheckCEOnPowerUP :PROCESS(CSNeg_ipd)
    BEGIN
        IF (PoweredUp = '0' AND falling_edge(CSNeg_ipd)) THEN
            REPORT InstancePath & partID &
            ": Device is selected during Power Up"
            SEVERITY WARNING;
        END IF;
    END PROCESS;

    ---------------------------------------------------------------------------
    -- Main Behavior Process
    -- combinational process for next state generation
    ---------------------------------------------------------------------------

    StateGen :PROCESS(PoweredUp, write, WDONE, RST_out, PDONE,
                      CRCDONE, sSTART_T1, HBN_out, REC_out,
                      SWRST_out, RESETNeg, DPD_out, RES_out, reset_check, reset_HARD,
                         reset_SOFT, reset_SIG)

    VARIABLE sect      : NATURAL RANGE 0 TO SecNumUni;

    BEGIN

        IF rising_edge(PoweredUp) AND SWRST_out = '1' AND RST_out = '1' THEN
            next_state <= IDLE;
        ELSIF PoweredUp = '1' THEN
            IF RST_out = '0' THEN
                next_state <= current_state;
            ELSIF ((falling_edge(write)) AND (Instruct = RSTCMD)) THEN
                next_state <= IDLE;
            ELSE
                CASE current_state IS
                    WHEN RESET_STATE =>
                        IF rising_edge(RST_out) OR rising_edge(SWRST_out) THEN
                            next_state <= IDLE;
                        END IF;

                    WHEN IDLE =>
                        IF falling_edge(write) THEN
                            IF Instruct=WRSR AND WEL='1' AND
                            not(SRWD='1' AND WPNegIn='0' AND QUAD='0') THEN
                                next_state <= IDLE;
                            ELSIF Instruct=WRAR AND WEL='1' AND
                            not(SRWD='1' AND WPNegIn='0' AND QUAD='0' AND
                                (Address=16#0000# OR
                                Address=16#0002#)) THEN
                            -- can not execute if WEL bit is zero or Hardware
                            -- Protection Mode is entered and SR1NV,SR1V,CR1NV or
                            -- CR1V is selected (no error is set)
                                IF Address = 16#0001# OR
                                Address = 16#0004# OR
                                Address = 16#0007# OR
                                ((Address>16#0008#) AND
                                (Address<16#0040#)) OR
                                ((Address>16#0041#) AND
                                (Address<16#0089#)) OR
                                ((Address>16#008F#) AND
                                (Address<16#0095#)) OR
                                (Address>16#0098#) THEN
                                    ASSERT FALSE
                                    REPORT "WARNING: Undefined location " &
                                        "selected. Command is ignored! "
                                    SEVERITY WARNING;
                                ELSIF ((Address>16#0094#) AND
                                (Address<16#0099#)) THEN --CRC
                                    ASSERT FALSE
                                    REPORT "WARNING: CRC register cannot be " &
                                        "written by the WRAR command. " &
                                        "Command is ignored! "
                                    SEVERITY WARNING;
                                ELSE -- Protection mode not selected
                                    next_state <= IDLE;
                                END IF;
                            ELSIF Instruct = SSWR AND WEL = '1' THEN
                                IF (Address + Byte_number) <= OTPHiAddr THEN
                                    -- Program within valid OTP Range
                                    next_state <=  IDLE;
                                END IF;
                            ELSIF Instruct = CRCC THEN
                                IF (Address >= CRC_Start_Addr_reg + 4) THEN
                                -- Condition for entering CRC_calc state is not complete
                                -- it needs to have comparison of Addr to EndAddr
                                -- Check datasheet for table of state transitions
                                    next_state <= CRC_Calc;
                                ELSE
                                    next_state <= IDLE;
                                END IF;
                            ELSIF Instruct = EPCS THEN
                                next_state <= CRC_SUSP;
                            ELSE
                                next_state <= IDLE;
                            END IF;
                        ELSIF rising_edge(DPD_out) THEN
                            next_state <= DP_DOWN;
                        ELSIF rising_edge(HBN_out) THEN
                            next_state <= HIBERNATE;
                        END IF;

                    WHEN WRITE_SR       =>
                        IF rising_edge(WDONE) THEN
                            next_state <= IDLE;
                        END IF;

                    WHEN WRITE_ALL_REG       =>
                        IF rising_edge(WDONE) THEN
                            next_state <= IDLE;
                        END IF;

                    WHEN OTP_PG         =>
                        IF rising_edge(PDONE) THEN
                            next_state <= IDLE;
                        END IF;

                    WHEN CRC_Calc =>
                        IF Instruct = EPCS OR rising_edge(sSTART_T1) THEN
                            next_state <= CRC_SUSP;
                        END IF;

                        IF rising_edge(CRCDONE) THEN
                            next_state <= IDLE;
                        END IF;

                    WHEN CRC_SUSP      =>
                        IF falling_edge(write) THEN
                            IF Instruct = EPCR THEN
                                next_state <= CRC_Calc;
                            ELSIF Instruct = RSTEN THEN
                                next_state <= RESET_STATE;
                            END IF;
                        END IF;

                    WHEN DP_DOWN =>
                        IF rising_edge(RES_out) AND Instruct = READ THEN
                            next_state <= IDLE;
                        END IF;

                    WHEN HIBERNATE =>
                        IF rising_edge(REC_out) AND Instruct = READ THEN
                            next_state <= IDLE;
                        END IF;

                END CASE;
            END IF;
        END IF;

    END PROCESS StateGen;

    ReadEnable: PROCESS (read_out)
    BEGIN
        oe_z <= rising_edge(read_out) AND PoweredUp = '1';

        IF read_out'EVENT AND read_out = '0' AND PoweredUp = '1' THEN
            oe   <= TRUE, FALSE AFTER 1 ns;
        END IF;
    END PROCESS ReadEnable;

    ---------------------------------------------------------------------------
    --FSM Output generation and general funcionality
    ---------------------------------------------------------------------------
    Functional : PROCESS(write,current_state, PoweredUp, WDONE, CRCDONE, PDONE,
                         oe, oe_z, HBN_out, REC_in, REC_out, Instruct, sSTART_T1,
                         change_addr, DPD_out, RES_in, RES_out, CSNeg, reseted, reset_HARD,
                         reset_SOFT, reset_SIG, reset_check)

        VARIABLE WData          : WByteType:= (OTHERS => MaxData);

        VARIABLE AddrLo         : NATURAL;
        VARIABLE AddrHi         : NATURAL;
        VARIABLE Addr           : NATURAL;
        VARIABLE Addr_pgm       : NATURAL;
        VARIABLE Addr_ers       : NATURAL;
        VARIABLE Addr_pgm_tmp   : NATURAL;
        VARIABLE Addr_idcfi     : NATURAL;
        VARIABLE Addr_id_reg    : NATURAL;

        VARIABLE old_bit        : std_logic_vector(7 downto 0);
        VARIABLE new_bit        : std_logic_vector(7 downto 0);
        VARIABLE old_int        : INTEGER RANGE -1 to MaxData;
        VARIABLE new_int        : INTEGER RANGE -1 to MaxData;
        VARIABLE wr_cnt         : NATURAL RANGE 0 TO AddrRANGE;

        VARIABLE sect           : NATURAL RANGE 0 TO SecNumUni;
        VARIABLE cnt            : NATURAL RANGE 0 TO AddrRANGE := 0;

        PROCEDURE READ_ALL_REG(
            VARIABLE   RDAR_reg : INOUT std_logic_vector(7 downto 0);
            VARIABLE   Addr     : NATURAL) IS
        BEGIN
            IF Addr = 16#000000# OR Addr = 16#070000# THEN
                RDAR_reg := SR1_V;
            ELSIF Addr = 16#000001# OR Addr = 16#070001# THEN
                RDAR_reg := SR2_V;
            ELSIF Addr = 16#000002# OR Addr = 16#070002# THEN
                RDAR_reg := CR1_V;
            ELSIF Addr = 16#000003# OR Addr = 16#070003# THEN
                RDAR_reg := CR2_V;
            ELSIF Addr = 16#000005# OR Addr = 16#070005# THEN
                RDAR_reg := CR4_V;
            ELSIF Addr = 16#000006# OR Addr = 16#070006# THEN
                RDAR_reg := CR5_V;
            ELSIF Addr = 16#000089# OR Addr = 16#070089# THEN
                RDAR_reg := ECC_reg;
            ELSIF Addr = 16#00008A# OR Addr = 16#07008A# THEN
                RDAR_reg := EDC_reg(7 downto 0);
            ELSIF Addr = 16#00008B# OR Addr = 16#07008B# THEN
                RDAR_reg := EDC_reg(15 downto 8);
            ELSIF Addr = 16#00008E# OR Addr = 16#07008E# THEN
                RDAR_reg := ADDTRAP_reg(7 downto 0);
            ELSIF Addr = 16#00008F# OR Addr = 16#07008F# THEN
                RDAR_reg := ADDTRAP_reg(15 downto 8);
            ELSIF Addr = 16#000040# OR Addr = 16#070040# THEN
                RDAR_reg := ADDTRAP_reg(23 downto 16);
            ELSIF Addr = 16#000041# OR Addr = 16#070041# THEN
                RDAR_reg := ADDTRAP_reg(31 downto 24);
            ELSIF Addr = 16#000095# OR Addr = 16#070095# THEN
                RDAR_reg := CRC_reg(7 downto 0);
            ELSIF Addr = 16#000096# OR Addr = 16#070096# THEN
                RDAR_reg := CRC_reg(15 downto 8);
            ELSIF Addr = 16#000097# OR Addr = 16#070097# THEN
                RDAR_reg := CRC_reg(23 downto 16);
            ELSIF Addr = 16#000098# OR Addr = 16#070098# THEN
                RDAR_reg := CRC_reg(31 downto 24);
            ELSE
                RDAR_reg := "XXXXXXXX";
            END IF;
        END READ_ALL_REG;

    BEGIN

        -----------------------------------------------------------------------
        -- Functionality Section
        -----------------------------------------------------------------------

        IF Instruct'EVENT THEN
            read_cnt := 0;
            byte_cnt := 1;
            rd_slow  <= false;
            dual     <= false;
            ddr      <= false;
            any_read <= false;
            Addr_idcfi := 0;
        END IF;

        IF rising_edge(PoweredUp) THEN
            -- the default condition after power-up
            -- During POR,the non-volatile version of the registers is copied to
            -- volatile version to provide the default state of the volatile
            -- register
            
            BP_bits := SR1_V(4) & SR1_V(3) & SR1_V(2);
            change_BP <= '1', '0' AFTER 1 ns;

            CRC_ACT      := '0';
            CRC_RD_SETUP := '0';
        END IF;

        IF change_addr'EVENT THEN
            read_addr := Address;
        END IF;

        IF DPD_out'EVENT AND DPD_out = '1' THEN
            DPD_in <= '0';
        END IF;

        IF RES_in'EVENT AND RES_in = '1' THEN
            RES_out <= '0', '1' AFTER tdevice_RES;
        END IF;

        IF HBN_out'EVENT AND HBN_out = '1' THEN
            HBN_in <= '0';
        END IF;

        IF REC_in'EVENT AND REC_in = '1' THEN
            REC_out <= '0', '1' AFTER tdevice_REC;
        END IF;

        CASE current_state IS

            WHEN IDLE          =>
--                 reset_check(1 DOWNTO 0) <= "00";
                dpd_act <= false;

                IF falling_edge(write) AND (DPD_in = '0' OR HBN_in = '0') THEN
                    IF Instruct = WREN THEN
                        SR1_V(1) <= '1';
                        DEBUG_CHECK <= '1';
                    ELSIF Instruct = WRDI THEN
                        SR1_V(1) <= '0';
                    ELSIF Instruct=WRSR AND WEL='1' THEN
                          IF REGS_PROTECTED = '0' THEN
                                       SR1_V(1) <= '0'; --WEL
                                       --SRWD bit
                                         SR1_NV(7) <= SR1_in(7);
                                         SR1_NV(5) <= SR1_in(5);--TBPROT
                                
                                         SR1_NV(4) <= SR1_in(4);--BP2
                                         SR1_NV(3) <= SR1_in(3);--BP1
                                         SR1_NV(2) <= SR1_in(2);--BP0
                                   
                                         SR1_V(7)  <= SR1_in(7);
                                         SR1_V(5)  <= SR1_in(5);--TBPROT
                                
                                         SR1_V(4)  <= SR1_in(4);--BP2
                                         SR1_V(3)  <= SR1_in(3);--BP1
                                         SR1_V(2)  <= SR1_in(2);--BP0
                                
                                         BP_bits := SR1_V(4) & SR1_V(3) & SR1_V(2);
                                         change_BP <= '1';
                                         change_BP <= '0' AFTER 1 ns;
                                    END IF;

                    ELSIF Instruct=WRAR AND WEL='1' THEN
                         IF REGS_PROTECTED = '0' THEN
                                  SR1_V(1) <= '0'; -- WEL
                            
                                 IF (Address = 16#000000#) THEN -- SR1_NV; 
                                   IF REGS_PROTECTED = '0' THEN
                                    --SRWD bit
                                    SR1_NV(7) <= SR1_in(7);
                                    SR1_NV(5) <= SR1_in(5);--TBPROT
                                
                                    SR1_NV(4) <= SR1_in(4);--BP2
                                    SR1_NV(3) <= SR1_in(3);--BP1
                                    SR1_NV(2) <= SR1_in(2);--BP0
                                   
                                    SR1_V(7)  <= WRAR_reg_in(7);
                                    SR1_V(5)  <= WRAR_reg_in(5);--TBPROT
                                
                                    SR1_V(4)  <= WRAR_reg_in(4);--BP2
                                    SR1_V(3)  <= WRAR_reg_in(3);--BP1
                                    SR1_V(2)  <= WRAR_reg_in(2);--BP0
                                
                                    BP_bits := SR1_V(4) & SR1_V(3) & SR1_V(2);
                                                                         
                                    change_BP <= '1';
                                    change_BP <= '0' AFTER 1 ns;                      
                                   END IF;
                                  --END IF;
                                ELSIF (Address = 16#070000#) THEN -- SR1_V;
                            
                                  IF REGS_PROTECTED = '0' THEN
                                    --SRWD bit
                                    SR1_V(1) <= '0'; -- WEL
                                    SR1_V(7)  <= WRAR_reg_in(7);
                                    SR1_V(5)  <= WRAR_reg_in(5);--TBPROT
                                
                                    SR1_V(4)  <= WRAR_reg_in(4);--BP2
                                    SR1_V(3)  <= WRAR_reg_in(3);--BP1
                                    SR1_V(2)  <= WRAR_reg_in(2);--BP0
                                
                                    BP_bits := SR1_V(4) & SR1_V(3) & SR1_V(2);
                                                                         
                                    change_BP <= '1';
                                    change_BP <= '0' AFTER 1 ns;      
                                   END IF;
                                ELSIF (Address = 16#000002#) THEN -- CR1_NV;
                                   IF REGS_PROTECTED = '0' THEN
                                    CR1_NV(7 DOWNTO 4) <= WRAR_reg_in(7 DOWNTO 4); --RL(3:0)
                                    CR1_NV(1) <= WRAR_reg_in(1);--QUAD
                                
                                    CR1_V(7 DOWNTO 4) <= WRAR_reg_in(7 DOWNTO 4); --RL(3:0)
                                    CR1_V(1) <= WRAR_reg_in(1);--QUAD
                                   END IF;
                                ELSIF (Address = 16#070002#) THEN -- CR1_V;
                                   IF REGS_PROTECTED = '0' THEN
                                    CR1_V(7 DOWNTO 4) <= WRAR_reg_in(7 DOWNTO 4); --RL_NV(3:0)
                                    CR1_V(1) <= WRAR_reg_in(1);--QUAD
                                   END IF; 
                                ELSIF (Address = 16#000003#) THEN -- CR2_NV;
                                   IF REGS_PROTECTED = '0' THEN
                                    CR2_NV(6)  <= WRAR_reg_in(6); --QPI
                                    CR2_NV(5)  <= WRAR_reg_in(5); --IO3R
                                    CR2_NV(4)  <= WRAR_reg_in(4); --DPI
                                    
                                    CR2_V(6)  <= WRAR_reg_in(6); --QPI
                                    CR2_V(5)  <= WRAR_reg_in(5); --IO3R
                                    CR2_V(4)  <= WRAR_reg_in(4); --DPI
                                   END IF;
                                ELSIF (Address = 16#070003#) THEN -- CR2_V;
                                   IF REGS_PROTECTED = '0' THEN
                                    CR2_V(6)  <= WRAR_reg_in(6); --QPI
                                    CR2_V(5)  <= WRAR_reg_in(5); --IO3R
                                    CR2_V(4)  <= WRAR_reg_in(4); --DPI 
                                   END IF;
                                ELSIF (Address = 16#000005#) THEN -- CR4_NV;
                                   IF REGS_PROTECTED = '0' THEN
                                    CR4_NV(7 DOWNTO 5) <= WRAR_reg_in(7 DOWNTO 5); --OI_O(2:0)
                                    CR4_NV(2) <= WRAR_reg_in(2);--DPDPOR
                                
                                    CR4_V(7 DOWNTO 5) <= WRAR_reg_in(7 DOWNTO 5); --OI_O(2:0)
                                    CR4_V(2) <= WRAR_reg_in(2);--DPDPOR 
                                   END IF;
                                ELSIF (Address = 16#070005#) THEN -- CR4_V;
                                   IF REGS_PROTECTED = '0' THEN
                                    CR4_V(7 DOWNTO 5) <= WRAR_reg_in(7 DOWNTO 5); --OI_O(2:0)
                                    CR4_V(2) <= WRAR_reg_in(2);--DPDPOR
                                   END IF; 
                                ELSIF (Address = 16#000006#) THEN -- CR5_NV;
                                   IF REGS_PROTECTED = '0' THEN
                                    CR5_NV(7 DOWNTO 6) <= WRAR_reg_in(7 DOWNTO 6); --REG_LATENCY_NV(2:0)
                                    CR5_V(7 DOWNTO 6) <= WRAR_reg_in(7 DOWNTO 6);--REG_LATENCY_NV(2:0)
                                   END IF; 
                                ELSIF (Address = 16#070006#) THEN -- CR5_V;
                                   IF REGS_PROTECTED = '0' THEN
                                    CR5_V(7 DOWNTO 6) <= WRAR_reg_in(7 DOWNTO 6); --REG_LATENCY_NV(2:0)   
                                   END IF;
                                END IF;
                              END IF;
                    ELSIF (Instruct = WRITE_MEM OR Instruct = DDRWRITE OR
                    Instruct = FAST_WRITE OR Instruct = DDR_FAST_WRITE OR
                    Instruct = DIW OR Instruct = DIOW OR
                    Instruct = QIW OR Instruct = QIOW OR Instruct = DDRQIOW)
                    AND WEL = '1' THEN
                        SecAddr_pgm := Address/(SecSize+1);
                        IF Sec_Prot(SecAddr_pgm) = '0' THEN
                            PSTART <= '1', '0' AFTER 1 ns;
                            INITIAL_CONFIG <= '1';
                            Addr_pgm := Address;
                            Addr_pgm_tmp := Address;
                            wr_cnt := Byte_number;
                            FOR I IN wr_cnt DOWNTO 0 LOOP
                                IF Viol /= '0' THEN
                                    WData(i) := -1;
                                ELSE
                                    WData(i) := WByte(i);
                                END IF;
                            END LOOP;
                        END IF;
                    ELSIF Instruct = SSWR AND WEL = '1' THEN
                        IF (Address + Byte_number) <= OTPHiAddr THEN
                        -- Program within valid OTP Range
                            SR1_V(1) <= '0';
                            IF (Address<=16#FF#) THEN
                                PSTART <= '1', '0' AFTER 1 ns;
                                Addr_pgm := Address;
                                wr_cnt := Byte_number;
                                FOR I IN wr_cnt DOWNTO 0 LOOP
                                    IF Viol /= '0' THEN
                                        WData(i) := -1;
                                    ELSE
                                        WData(i) := WByte(i);
                                    END IF;
                                END LOOP;
                            ELSE
                                Addr_pgm := 0;
                            END IF;
                        END IF;
                    ELSIF Instruct = CRCC THEN
                        IF (CRC_End_Addr_reg >= CRC_Start_Addr_reg + 4) THEN
                            CRCSTART <= '1', '0' AFTER 1 ns;
                            SR1_V(0) <= '1';  -- WIP
                            CRCA <= '0';
                            CRC_reg := (OTHERS => '0');
                        ELSE
                            -- Abort CRC calculation
                            ASSERT FALSE
                            REPORT "CRC EndAddr is not StartAddr+4 " &
                                    "or greater; CRC calculation is aborted"
                            SEVERITY WARNING;
                            CRCA  <= '1';
                        END IF;
                    ELSIF Instruct = EPCS AND START_T1_in = '0' THEN
                        START_T1_in <= '1';

                    ELSIF (Instruct = DPD) THEN
                        DPD_in  <= '1';

                    ELSIF (Instruct = HBN) THEN
                        HBN_in  <= '1';
                    END IF;

                    IF Instruct = RSTEN THEN
                        RESET_EN <= '1';
                    ELSE
                        RESET_EN <= '0';
                    END IF;

                ELSIF oe_z THEN
                    IF Instruct = READ THEN
                        rd_slow <= true;
                        dual    <= false;
                        ddr     <= false;
                    ELSIF Instruct = DDRQIOR OR Instruct = DDRFR THEN
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= true;
                    ELSIF Instruct = DIOR OR Instruct = QIOR OR Instruct = QOR THEN
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        IF QPI = '1' THEN
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                        ELSE
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                        END IF;
                    END IF;

                ELSIF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1 THEN
                        --Read Status Register 1
                        IF QPI = '1' THEN
                            data_out(7 DOWNTO 0) := SR1_V;
                            RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF DPI = '1' THEN
                            data_out(7 DOWNTO 0) := SR1_V;
                            SOut_zd       <= data_out(7-2*read_cnt);
                            SIOut_zd      <= data_out(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            SOut_zd <= SR1_V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2 THEN
                        --Read Status Register 2
                        IF QPI = '1' THEN
                            data_out(7 DOWNTO 0) := SR2_V;
                            RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF DPI = '1' THEN
                            data_out(7 DOWNTO 0) := SR2_V;
                            SOut_zd       <= data_out(7-2*read_cnt);
                            SIOut_zd      <= data_out(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            SOut_zd <= SR2_V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDCR1 THEN
                        --Read Configuration Register 1
                        IF QPI = '1' THEN
                            data_out(7 DOWNTO 0) := CR1_V;
                            RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF DPI = '1' THEN
                            data_out(7 DOWNTO 0) := CR1_V;
                            SOut_zd       <= data_out(7-2*read_cnt);
                            SIOut_zd      <= data_out(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            SOut_zd <= CR1_V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDCR2 THEN
                        --Read Configuration Register 2
                        IF QPI = '1' THEN
                            data_out(7 DOWNTO 0) := CR2_V;
                            RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF DPI = '1' THEN
                            data_out(7 DOWNTO 0) := CR2_V;
                            SOut_zd       <= data_out(7-2*read_cnt);
                            SIOut_zd      <= data_out(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            SOut_zd <= CR2_V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDCR4 THEN
                        --Read Configuration Register 4
                        IF QPI = '1' THEN
                            data_out(7 DOWNTO 0) := CR4_V;
                            RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF DPI = '1' THEN
                            data_out(7 DOWNTO 0) := CR4_V;
                            SOut_zd       <= data_out(7-2*read_cnt);
                            SIOut_zd      <= data_out(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            SOut_zd <= CR4_V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDCR5 THEN
                        --Read Configuration Register 5
                        IF QPI = '1' THEN
                            data_out(7 DOWNTO 0) := CR5_V;
                            RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF DPI = '1' THEN
                            data_out(7 DOWNTO 0) := CR5_V;
                            SOut_zd       <= data_out(7-2*read_cnt);
                            SIOut_zd      <= data_out(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            SOut_zd <= CR5_V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDAR THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI = '1' THEN
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF DPI = '1' THEN
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            SOut_zd       <= data_out(7-2*read_cnt);
                            SIOut_zd      <= data_out(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = READ THEN
                       -- Read Memory array
                       IF (QPI = '1') THEN
                           rd_slow <= true;
                           dual    <= true;
                           ddr     <= false;
                        
                          IF Mem(read_addr) /= -1 THEN
                                data_out := to_slv(Mem(read_addr),8);
                                RESETNegOut_zd  <= data_out(7-4*read_cnt);
                            WPNegOut_zd     <= data_out(6-4*read_cnt);
                            SOut_zd     <= data_out(5-4*read_cnt);
                            SIOut_zd     <= data_out(4-4*read_cnt);
                           ELSE
                                RESETNegOut_zd<= 'X';
                                WPNegOut_zd <= 'X';
                                SIOut_zd <= 'X';
                                SOut_zd <= 'X';
                           END IF;
                        
                        
                            
                            read_cnt := read_cnt + 1;
                            IF (read_cnt = 2) THEN
                            
                                read_cnt := 0;
                                IF (read_addr = AddrRANGE) THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        ELSIF (DPI = '1') THEN
                        
                            rd_slow <= true;
                            dual    <= true;
                            ddr     <= false;
                             IF Mem(read_addr) /= -1 THEN
                                data_out := to_slv(Mem(read_addr),8);
                               SOut_zd <= data_out(7-2*read_cnt);
                               SIOut_zd <= data_out(6-2*read_cnt);
                            ELSE
                                SOut_zd <= 'X';
                               SIOut_zd <= 'X';
                            END IF;
                            read_cnt := read_cnt + 1;
                            IF (read_cnt = 4) THEN
                            
                                read_cnt := 0;
                                IF (read_addr = AddrRANGE) THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        ELSE
                            rd_slow <= true;
                            dual    <= false;
                            ddr     <= false;
                            IF Mem(read_addr) /= -1 THEN
                                data_out := to_slv(Mem(read_addr),8);
                                SOut_zd <= data_out(7-read_cnt);
                            ELSE
                                SOut_zd <= 'X';
                            END IF;
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                IF read_addr >= AddrRANGE THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                      END IF;
                      
                    ELSIF Instruct = FAST_READ THEN
                       -- Read Memory array
                       IF (QPI = '1') THEN
                           rd_slow <= false;
                           dual    <= true;
                           ddr     <= false;
                        
                          IF Mem(read_addr) /= -1 THEN
                                data_out := to_slv(Mem(read_addr),8);
                                RESETNegOut_zd  <= data_out(7-4*read_cnt);
                            WPNegOut_zd     <= data_out(6-4*read_cnt);
                            SOut_zd     <= data_out(5-4*read_cnt);
                            SIOut_zd     <= data_out(4-4*read_cnt);
                           ELSE
                                RESETNegOut_zd<= 'X';
                                WPNegOut_zd <= 'X';
                                SIOut_zd <= 'X';
                                SOut_zd <= 'X';
                          END IF;
                        
                        
                            
                            read_cnt := read_cnt + 1;
                            IF (read_cnt = 2) THEN
                            
                                read_cnt := 0;
                                IF (read_addr = AddrRANGE) THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        ELSIF (DPI = '1') THEN
                        
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                             IF Mem(read_addr) /= -1 THEN
                                data_out := to_slv(Mem(read_addr),8);
                               SOut_zd <= data_out(7-2*read_cnt);
                               SIOut_zd <= data_out(6-2*read_cnt);
                            ELSE
                                SOut_zd <= 'X';
                               SIOut_zd <= 'X';
                            END IF;
                            read_cnt := read_cnt + 1;
                            IF (read_cnt = 4) THEN
                            
                                read_cnt := 0;
                                IF (read_addr = AddrRANGE) THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        ELSE
                        
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            IF Mem(read_addr) /= -1 THEN
                                data_out := to_slv(Mem(read_addr),8);
                                SOut_zd <= data_out(7-read_cnt);
                            ELSE
                                SOut_zd <= 'X';
                            END IF;
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                IF read_addr >= AddrRANGE THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                      END IF;

                    ELSIF Instruct = DDRFR THEN
                        -- Read Memory array
                        IF (QPI = '1') THEN
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= true;

                            data_out := to_slv(Mem(read_addr),8);
                            RESETNegOut_zd  <= data_out(7-4*read_cnt);
                            WPNegOut_zd     <= data_out(6-4*read_cnt);
                            SOut_zd         <= data_out(5-4*read_cnt);
                            SIOut_zd        <= data_out(4-4*read_cnt);
                            read_cnt        := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                            END IF;
                        END IF;

                    ELSIF Instruct = DIOR OR Instruct = DOR THEN
                        -- Read Memory array
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;

                        data_out := to_slv(Mem(read_addr),8);
                        SOut_zd  <= data_out(7-2*read_cnt);
                        SIOut_zd <= data_out(6-2*read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 4 THEN
                            read_cnt := 0;
                            IF read_addr = AddrRANGE THEN
                                read_addr := 0;
                            ELSE
                                read_addr := read_addr + 1;
                            END IF;
                        END IF;

                    ELSIF (Instruct = QIOR AND (QPI = '1' OR QUAD = '1'))
                            OR (Instruct = DDRQIOR AND (QPI = '1' OR QUAD = '1'))
                            OR (Instruct = QOR AND QUAD='1') THEN
                        IF Instruct = DDRQIOR THEN
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= true;
                        ELSE
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                        END IF;

                        data_out := to_slv(Mem(read_addr),8);
                        RESETNegOut_zd <= data_out(7-4*read_cnt);
                        WPNegOut_zd   <= data_out(6-4*read_cnt);
                        SOut_zd       <= data_out(5-4*read_cnt);
                        SIOut_zd      <= data_out(4-4*read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 2 THEN
                            read_cnt := 0;
                            IF read_addr = AddrRANGE THEN
                                read_addr := 0;
                            ELSE
                                read_addr := read_addr + 1;
                            END IF;
                        END IF;

                    ELSIF Instruct = SSRD THEN
                        IF (read_addr>=OTPLoAddr) AND (read_addr<=OTPHiAddr) THEN
                        -- Read OTP Memory array
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;

                            data_out := to_slv(OTPMem(read_addr),8);
                            IF QPI = '1' THEN
                                RESETNegOut_zd <= data_out(7-4*read_cnt);
                                WPNegOut_zd   <= data_out(6-4*read_cnt);
                                SOut_zd       <= data_out(5-4*read_cnt);
                                SIOut_zd      <= data_out(4-4*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                    read_cnt := 0;
                                    read_addr := read_addr + 1;
                                END IF;
                            ELSIF DPI = '1' THEN
                                SOut_zd       <= data_out(7-2*read_cnt);
                                SIOut_zd      <= data_out(6-2*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 4 THEN
                                    read_cnt := 0;
                                    read_addr := read_addr + 1;
                                END IF;
                            ELSE
                                SOut_zd <= data_out(7-read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        ELSIF (read_addr > OTPHiAddr) THEN
                        --OTP Read operation will not wrap to the
                        --starting address after the OTP address is at
                        --its maximum or Read Password Protection Mode
                        --is selected; instead, the data beyond the
                        --maximum OTP address will be undefined.
                            IF QPI = '1' THEN
                                RESETNegOut_zd <= 'X';
                                WPNegOut_zd    <= 'X';
                                SOut_zd        <= 'X';
                                SIOut_zd       <= 'X';
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                    read_cnt := 0;
                                END IF;
                            ELSIF DPI = '1' THEN
                                SOut_zd       <= 'X';
                                SIOut_zd      <= 'X';
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 4 THEN
                                    read_cnt := 0;
                                END IF;
                            ELSE
                                SOut_zd <= 'X';
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                END IF;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDID THEN
                        IF QPI = '1' THEN
                            IF (Addr_id_reg <= 7) THEN -- RDID does not wrap
                                data_out(7 DOWNTO 0) := ID_reg(Addr_id_reg);
                                RESETNegOut_zd <= data_out(7-4*read_cnt);
                                WPNegOut_zd   <= data_out(6-4*read_cnt);
                                SOut_zd       <= data_out(5-4*read_cnt);
                                SIOut_zd      <= data_out(4-4*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                    read_cnt := 0;
                                    Addr_id_reg := Addr_id_reg+1;
                                END IF;
                            END IF;
                        ELSIF DPI = '1' THEN
                            IF (Addr_id_reg <= 7) THEN -- RDID does not wrap
                                data_out(7 DOWNTO 0) := ID_reg(Addr_id_reg);
                                SOut_zd       <= data_out(7-2*read_cnt);
                                SIOut_zd      <= data_out(6-2*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 4 THEN
                                    read_cnt := 0;
                                    Addr_id_reg := Addr_id_reg+1;
                                END IF;
                            END IF;
                        ELSE
                            IF (Addr_id_reg <= 7) THEN -- RDID does not wrap
                                data_out(7 DOWNTO 0) := ID_reg(Addr_id_reg);
                                SOut_zd       <= data_out(7-read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                    Addr_id_reg := Addr_id_reg+1;
                                END IF;
                            END IF;
                        END IF;

                    ELSIF Instruct = RUID THEN
                        IF QPI = '1' THEN
                            RESETNegOut_zd <= UID((8*byte_cnt-1)-4*read_cnt);
                            WPNegOut_zd   <= UID((8*byte_cnt-2)-4*read_cnt);
                            SOut_zd       <= UID((8*byte_cnt-3)-4*read_cnt);
                            SIOut_zd      <= UID((8*byte_cnt-4)-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                                byte_cnt := byte_cnt + 1;
                                IF byte_cnt = 9 THEN
                                   byte_cnt := 1;
                                END IF;
                            END IF;
                        ELSIF DPI = '1' THEN
                            SOut_zd       <= UID((8*byte_cnt-1)-2*read_cnt);
                            SIOut_zd      <= UID((8*byte_cnt-2)-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                                byte_cnt := byte_cnt + 1;
                                IF byte_cnt = 9 THEN
                                   byte_cnt := 1;
                                END IF;
                            END IF;
                        ELSE
                            SOut_zd       <= UID((8*byte_cnt-1)-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                byte_cnt := byte_cnt + 1;
                                IF byte_cnt = 9 THEN
                                   byte_cnt := 1;
                                END IF;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSN THEN
                        IF QPI = '1' THEN
                            RESETNegOut_zd <= SERNUM_reg((8*byte_cnt-1)-4*read_cnt);
                            WPNegOut_zd   <= SERNUM_reg((8*byte_cnt-2)-4*read_cnt);
                            SOut_zd       <= SERNUM_reg((8*byte_cnt-3)-4*read_cnt);
                            SIOut_zd      <= SERNUM_reg((8*byte_cnt-4)-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                                byte_cnt := byte_cnt + 1;
                                IF byte_cnt = 9 THEN
                                   byte_cnt := 1;
                                END IF;
                            END IF;
                        ELSIF DPI = '1' THEN
                            SOut_zd       <= SERNUM_reg((8*byte_cnt-1)-2*read_cnt);
                            SIOut_zd      <= SERNUM_reg((8*byte_cnt-2)-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                                byte_cnt := byte_cnt + 1;
                                IF byte_cnt = 9 THEN
                                   byte_cnt := 1;
                                END IF;
                            END IF;
                        ELSE
                            SOut_zd      <= SERNUM_reg((8*byte_cnt-1)-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                byte_cnt := byte_cnt + 1;
                                IF byte_cnt = 9 THEN
                                   byte_cnt := 1;
                                END IF;
                            END IF;
                        END IF;

                    ELSIF Instruct = ECCRD THEN
                        IF QPI = '1' THEN
                            RESETNegOut_zd <= ECC_reg(7-4*read_cnt);
                            WPNegOut_zd   <= ECC_reg(6-4*read_cnt);
                            SOut_zd       <= ECC_reg(5-4*read_cnt);
                            SIOut_zd      <= ECC_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF DPI = '1' THEN
                            SOut_zd       <= ECC_reg(7-2*read_cnt);
                            SIOut_zd      <= ECC_reg(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            SOut_zd <= ECC_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;

            WHEN WRITE_SR       =>

            
            WHEN WRITE_ALL_REG       =>
                

            WHEN OTP_PG       =>


            WHEN CRC_Calc =>
                IF QPI = '1' THEN
                    rd_slow <= false;
                    dual    <= true;
                    ddr     <= false;
                ELSE
                    rd_slow <= false;
                    dual    <= false;
                    ddr     <= false;
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1 THEN
                        --Read Status Register 1
                        IF QPI = '1' THEN
                            data_out(7 DOWNTO 0) := SR1_V;
                            RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF DPI = '1' THEN
                            data_out(7 DOWNTO 0) := SR1_V;
                            SOut_zd       <= data_out(7-2*read_cnt);
                            SIOut_zd      <= data_out(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            SOut_zd <= SR1_V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;
                
                CRC_ACT := '1';
                CRC_RD_SETUP := '1';

                IF rising_edge(CRCDONE) THEN
                    crc_out := (OTHERS => '0');
                    FOR i IN CRC_Start_Addr_reg TO CRC_End_Addr_reg LOOP
                        crc_in := to_slv(Mem(i),16);
                        FOR J IN 15 DOWNTO 0 LOOP
                            crc_tmp := crc_in(J) XOR crc_out(31);
                            crc_out(31) := crc_out(30);
                            crc_out(30) := crc_out(29);
                            crc_out(29) := crc_out(28);
                            crc_out(28) := crc_out(27) XOR crc_tmp;
                            crc_out(27) := crc_out(26) XOR crc_tmp;
                            crc_out(26) := crc_out(25) XOR crc_tmp;
                            crc_out(25) := crc_out(24) XOR crc_tmp;
                            crc_out(24) := crc_out(23);
                            crc_out(23) := crc_out(22) XOR crc_tmp;
                            crc_out(22) := crc_out(21) XOR crc_tmp;
                            crc_out(21) := crc_out(20);
                            crc_out(20) := crc_out(19) XOR crc_tmp;
                            crc_out(19) := crc_out(18) XOR crc_tmp;
                            crc_out(18) := crc_out(17) XOR crc_tmp;
                            crc_out(17) := crc_out(16);
                            crc_out(16) := crc_out(15);
                            crc_out(15) := crc_out(14);
                            crc_out(14) := crc_out(13) XOR crc_tmp;
                            crc_out(13) := crc_out(12) XOR crc_tmp;
                            crc_out(12) := crc_out(11);
                            crc_out(11) := crc_out(10) XOR crc_tmp;
                            crc_out(10) := crc_out(9) XOR crc_tmp;
                            crc_out(9) := crc_out(8) XOR crc_tmp;
                            crc_out(8) := crc_out(7) XOR crc_tmp;
                            crc_out(7) := crc_out(6);
                            crc_out(6) := crc_out(5) XOR crc_tmp;
                            crc_out(5) := crc_out(4);
                            crc_out(4) := crc_out(3);
                            crc_out(3) := crc_out(2);
                            crc_out(2) := crc_out(1);
                            crc_out(1) := crc_out(0);
                            crc_out(0) :=  CRC_tmp;
                        END LOOP;
                    END LOOP;
                    CRC_reg := crc_out;
                    SR1_V(0) <= '0';  -- WIP
                END IF;

            WHEN CRC_SUSP       =>
                IF QPI = '1' THEN
                    rd_slow <= false;
                    dual    <= true;
                    ddr     <= false;
                ELSE
                    rd_slow <= false;
                    dual    <= false;
                    ddr     <= false;
                END IF;
                IF sSTART_T1 = '1' AND START_T1_in = '1' THEN
                    START_T1_in <= '0';
                    -- The WIP bit in the Status Register will indicate that
                    -- the device is ready for another operation.
                    SR1_V(0) <= '0';
                    -- The CRC Suspend (CRCS) bit in the Status Register will
                    -- be set to the logical “1” state to indicate that the
                    -- CRC operation has been suspended.
                    CRCS <= '1';
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1 THEN
                        --Read Status Register 1
                        IF QPI = '1' THEN
                            data_out(7 DOWNTO 0) := SR1_V;
                            RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF DPI = '1' THEN
                            data_out(7 DOWNTO 0) := SR1_V;
                            SOut_zd       <= data_out(7-2*read_cnt);
                            SIOut_zd      <= data_out(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            SOut_zd <= SR1_V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2 THEN
                        --Read Status Register 2
                        IF QPI = '1' THEN
                            data_out(7 DOWNTO 0) := SR2_V;
                            RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF DPI = '1' THEN
                            data_out(7 DOWNTO 0) := SR2_V;
                            SOut_zd       <= data_out(7-2*read_cnt);
                            SIOut_zd      <= data_out(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            SOut_zd <= SR2_V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDCR1 THEN
                        --Read Configuration Register 1
                        IF QPI = '1' THEN
                            data_out(7 DOWNTO 0) := CR1_V;
                            RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF DPI = '1' THEN
                            data_out(7 DOWNTO 0) := CR1_V;
                            SOut_zd       <= data_out(7-2*read_cnt);
                            SIOut_zd      <= data_out(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            SOut_zd <= CR1_V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDCR2 THEN
                        --Read Configuration Register 2
                        IF QPI = '1' THEN
                            data_out(7 DOWNTO 0) := CR2_V;
                            RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF DPI = '1' THEN
                            data_out(7 DOWNTO 0) := CR2_V;
                            SOut_zd       <= data_out(7-2*read_cnt);
                            SIOut_zd      <= data_out(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            SOut_zd <= CR2_V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDCR4 THEN
                        --Read Configuration Register 4
                        IF QPI = '1' THEN
                            data_out(7 DOWNTO 0) := CR4_V;
                            RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF DPI = '1' THEN
                            data_out(7 DOWNTO 0) := CR4_V;
                            SOut_zd       <= data_out(7-2*read_cnt);
                            SIOut_zd      <= data_out(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            SOut_zd <= CR4_V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDCR5 THEN
                        --Read Configuration Register 5
                        IF QPI = '1' THEN
                            data_out(7 DOWNTO 0) := CR5_V;
                            RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF DPI = '1' THEN
                            data_out(7 DOWNTO 0) := CR5_V;
                            SOut_zd       <= data_out(7-2*read_cnt);
                            SIOut_zd      <= data_out(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            SOut_zd <= CR5_V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDAR THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI = '1' THEN
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF DPI = '1' THEN
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            SOut_zd       <= data_out(7-2*read_cnt);
                            SIOut_zd      <= data_out(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = READ THEN
                     -- Read Memory array
                       IF (QPI = '1') THEN
                           rd_slow <= true;
                           dual    <= true;
                           ddr     <= false;
                        
                           IF Mem(read_addr) /= -1 THEN
                                data_out := to_slv(Mem(read_addr),8);
                                RESETNegOut_zd  <= data_out(7-4*read_cnt);
                            WPNegOut_zd     <= data_out(6-4*read_cnt);
                            SOut_zd     <= data_out(5-4*read_cnt);
                            SIOut_zd     <= data_out(4-4*read_cnt);
                           ELSE
                                RESETNegOut_zd<= 'X';
                                WPNegOut_zd <= 'X';
                                SIOut_zd <= 'X';
                                SOut_zd <= 'X';
                           END IF;
                        
                        
                            
                            read_cnt := read_cnt + 1;
                            IF (read_cnt = 2) THEN
                                read_cnt := 0;
                                IF (read_addr = AddrRANGE) THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        ELSIF (DPI = '1') THEN
                        
                            rd_slow <= true;
                            dual    <= true;
                            ddr     <= false;
                             IF Mem(read_addr) /= -1 THEN
                                data_out := to_slv(Mem(read_addr),8);
                               SOut_zd <= data_out(7-2*read_cnt);
                               SIOut_zd <= data_out(6-2*read_cnt);
                            ELSE
                                SOut_zd <= 'X';
                               SIOut_zd <= 'X';
                            END IF;
                            read_cnt := read_cnt + 1;
                            IF (read_cnt = 4) THEN
                            
                                read_cnt := 0;
                                IF (read_addr = AddrRANGE) THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        ELSE
                        
                            rd_slow <= true;
                            dual    <= false;
                            ddr     <= false;
                            IF Mem(read_addr) /= -1 THEN
                                data_out := to_slv(Mem(read_addr),8);
                                SOut_zd <= data_out(7-read_cnt);
                            ELSE
                                SOut_zd <= 'X';
                            END IF;
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                IF read_addr >= AddrRANGE THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                      END IF;

                    ELSIF Instruct = FAST_READ THEN
                    -- Read Memory array
                       IF (QPI = '1') THEN
                           rd_slow <= false;
                           dual    <= true;
                           ddr     <= false;
                        
                          IF Mem(read_addr) /= -1 THEN
                                data_out := to_slv(Mem(read_addr),8);
                                RESETNegOut_zd  <= data_out(7-4*read_cnt);
                            WPNegOut_zd     <= data_out(6-4*read_cnt);
                            SOut_zd     <= data_out(5-4*read_cnt);
                            SIOut_zd     <= data_out(4-4*read_cnt);
                           ELSE
                                RESETNegOut_zd<= 'X';
                                WPNegOut_zd <= 'X';
                                SIOut_zd <= 'X';
                                SOut_zd <= 'X';
                           END IF;
                        
                        
                            
                            read_cnt := read_cnt + 1;
                            IF (read_cnt = 2) THEN
                            
                                read_cnt := 0;
                                IF (read_addr = AddrRANGE) THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        ELSIF (DPI = '1') THEN
                        
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                             IF Mem(read_addr) /= -1 THEN
                                data_out := to_slv(Mem(read_addr),8);
                               SOut_zd <= data_out(7-2*read_cnt);
                               SIOut_zd <= data_out(6-2*read_cnt);
                            ELSE
                                SOut_zd <= 'X';
                               SIOut_zd <= 'X';
                            END IF;
                            read_cnt := read_cnt + 1;
                            IF (read_cnt = 4) THEN
                            
                                read_cnt := 0;
                                IF (read_addr = AddrRANGE) THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        ELSE
                        
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            IF Mem(read_addr) /= -1 THEN
                                data_out := to_slv(Mem(read_addr),8);
                                SOut_zd <= data_out(7-read_cnt);
                            ELSE
                                SOut_zd <= 'X';
                            END IF;
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                IF read_addr >= AddrRANGE THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                      END IF;

                    ELSIF Instruct = DOR OR Instruct = DIOR THEN
                        -- Read Memory array
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                            data_out := to_slv(Mem(read_addr),8);
                            SOut_zd <= data_out(7-2*read_cnt);
                            SIOut_zd <= data_out(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                                IF read_addr = AddrRANGE THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;

                    ELSIF (Instruct = QIOR AND (QPI = '1' OR QUAD = '1'))
                            OR (Instruct = DDRQIOR AND (QPI = '1' OR QUAD = '1'))
                            OR (Instruct = QOR AND QUAD='1') THEN
                       IF Instruct = DDRQIOR THEN
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= true;
                        ELSE
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                        END IF;
                            data_out := to_slv(Mem(read_addr),8);
                            RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                                IF read_addr = AddrRANGE THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;

                    ELSIF Instruct = ECCRD THEN
                        IF QPI = '1' THEN
                            RESETNegOut_zd <= ECC_reg(7-4*read_cnt);
                            WPNegOut_zd   <= ECC_reg(6-4*read_cnt);
                            SOut_zd       <= ECC_reg(5-4*read_cnt);
                            SIOut_zd      <= ECC_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF DPI = '1' THEN
                            SOut_zd       <= ECC_reg(7-2*read_cnt);
                            SIOut_zd      <= ECC_reg(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            SOut_zd <= ECC_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDID THEN
                        IF QPI = '1' THEN
                            IF (Addr_id_reg <= 7) THEN -- RDID does not wrap
                                data_out(7 DOWNTO 0) := ID_reg(Addr_id_reg);
                                RESETNegOut_zd <= data_out(7-4*read_cnt);
                                WPNegOut_zd   <= data_out(6-4*read_cnt);
                                SOut_zd       <= data_out(5-4*read_cnt);
                                SIOut_zd      <= data_out(4-4*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                    read_cnt := 0;
                                    Addr_id_reg := Addr_id_reg+1;
                                END IF;
                            END IF;
                        ELSIF DPI = '1' THEN
                            IF (Addr_id_reg <= 7) THEN -- RDID does not wrap
                                data_out(7 DOWNTO 0) := ID_reg(Addr_id_reg);
                                SOut_zd       <= data_out(7-2*read_cnt);
                                SIOut_zd      <= data_out(6-2*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 4 THEN
                                    read_cnt := 0;
                                    Addr_id_reg := Addr_id_reg+1;
                                END IF;
                            END IF;
                        ELSE
                            IF (Addr_id_reg <= 7) THEN -- RDID does not wrap
                                data_out(7 DOWNTO 0) := ID_reg(Addr_id_reg);
                                SOut_zd       <= data_out(7-read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                    Addr_id_reg := Addr_id_reg+1;
                                END IF;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSN THEN
                        IF QPI = '1' THEN
                            RESETNegOut_zd <= SERNUM_reg((8*byte_cnt-1)-4*read_cnt);
                            WPNegOut_zd   <= SERNUM_reg((8*byte_cnt-2)-4*read_cnt);
                            SOut_zd       <= SERNUM_reg((8*byte_cnt-3)-4*read_cnt);
                            SIOut_zd      <= SERNUM_reg((8*byte_cnt-4)-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                                byte_cnt := byte_cnt + 1;
                                IF byte_cnt = 9 THEN
                                   byte_cnt := 1;
                                END IF;
                            END IF;
                        ELSIF DPI = '1' THEN
                            SOut_zd       <= SERNUM_reg((8*byte_cnt-1)-2*read_cnt);
                            SIOut_zd      <= SERNUM_reg((8*byte_cnt-2)-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                                byte_cnt := byte_cnt + 1;
                                IF byte_cnt = 9 THEN
                                   byte_cnt := 1;
                                END IF;
                            END IF;
                        ELSE
                            SOut_zd      <= SERNUM_reg((8*byte_cnt-1)-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                byte_cnt := byte_cnt + 1;
                                IF byte_cnt = 9 THEN
                                   byte_cnt := 1;
                                END IF;
                            END IF;
                        END IF;

                    END IF;
                ELSIF oe_z THEN
                    IF Instruct = READ THEN
                        rd_slow <= true;
                        dual    <= false;
                        ddr     <= false;
                    ELSIF Instruct = DIOR OR Instruct = QIOR THEN
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSIF Instruct = DDRQIOR THEN
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= true;
                    ELSE
                        IF QPI = '1' THEN
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                        ELSE
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                        END IF;
                    END IF;
                END IF;

                IF falling_edge(write) THEN
                    IF Instruct = EPCR THEN
                        CRCS <= '0';
                        SR1_V(0) <= '1'; -- WIP
                        CRCRES  <= '1', '0' AFTER 1 ns;
                        RES_TO_SUSP_TIME <= '1', '0' AFTER tdevice_CRCRL; -- 10 us
                    END IF;

                    IF Instruct = RSTEN THEN
                        RESET_EN <= '1';
                    ELSE
                        RESET_EN <= '0';
                    END IF;

                END IF;

            WHEN DP_DOWN =>
                dpd_act <= true;
                IF oe THEN
                    any_read <= false;
                END IF;

                IF falling_edge(write) THEN
                    IF (Instruct = READ) THEN
                        RES_in <= '1', '0' AFTER 5 ns;
                    ELSE
                        REPORT "Device is in DPD Mode; No instructions allowed"
                        SEVERITY NOTE;
                    END IF;
                END IF;

            WHEN HIBERNATE =>
                IF oe THEN
                    any_read <= false;
                END IF;

                IF falling_edge(write) THEN
                    IF (Instruct = READ) THEN
                        REC_in <= '1', '0' AFTER 5 ns;
                    ELSE
                        REPORT "Device is in HIBERNATE Mode; No instructions allowed"
                        SEVERITY NOTE;
                    END IF;
                END IF;

            WHEN RESET_STATE   =>
            -- During Reset,the non-volatile version of the registers is
            -- copied to volatile version to provide the default state of
            -- the volatile register
            
                IF reset_check = "01" THEN
                    SR1_NV <= (others => '0');
                    SR2_V <= (others => '0');
                    CR1_NV <= (others => '0');
                    CR2_NV  <= (others => '0');
                    CR4_NV  <=  "00001000";
                    CR5_NV   <= (others => '0');
                    ECC_reg := (others => '0');
                    ADDTRAP_reg := (others => '0');
                    EDC_reg := (others => '0');
                    CRC_reg  := (others => '0');
                ELSIF reset_check = "10" THEN
                    SR1_V(1) <= '0';
                    ECC_reg := (others => '0');
                    ADDTRAP_reg := (others => '0');
                    EDC_reg := (others => '0');
                    CRC_reg  := (others => '0');
                ELSIF reset_check = "11" THEN
                    SR1_NV <= (others => '0');
                    SR2_V <= (others => '0');
                    CR1_NV <= (others => '0');
                    CR2_NV  <= (others => '0');
                    CR4_NV  <=  "00001000";
                    CR5_NV   <= (others => '0');
                    ECC_reg := (others => '0');
                    ADDTRAP_reg := (others => '0');
                    EDC_reg := (others => '0');
                    CRC_reg  := (others => '0');
                 END IF;
            
            
            
                SR1_V <= SR1_NV(7 DOWNTO 2) & "00";
                CR1_V <= CR1_NV;
                CR2_V <= CR2_NV;
                CR4_V <= CR4_NV;
                CR5_V <= CR5_NV;
                
                BP_bits := SR1_V(4) & SR1_V(3) & SR1_V(2);
                change_BP <= '1';
                change_BP <= '0' AFTER 1 ns;
                
                CRC_reg := (OTHERS => '0');

                --Loads the Program Buffer with all ones
                WData := (OTHERS => MaxData);

                -- When BPNV is set to '1'. the BP2-0 bits in Status
                -- Register are volatile and will be reseted after
                -- reset command
                BP_bits := SR1_V(4) & SR1_V(3) & SR1_V(2);
                change_BP <= '1', '0' AFTER 1 ns;

                RESET_EN <= '0';

        END CASE;
        IF falling_edge(write) THEN
            IF Instruct = RSTEN AND
            (current_state /= DP_DOWN OR current_state /= HIBERNATE) THEN
                RESET_EN <= '1';
            ELSE
                RESET_EN <= '0';
            END IF;
        END IF;
        --Output Disable Control
        IF (CSNeg_ipd = '1') THEN
            read_cnt        := 0;
            SIOut_zd        <= 'Z';
            RESETNegOut_zd  <= 'Z';
            WPNegOut_zd     <= 'Z';
            SOut_zd         <= 'Z';
        END IF;

        IF QPI = '1' THEN
            dual <= true;
        END IF;

    END PROCESS Functional;

    Protect : PROCESS(change_BP)
    BEGIN
        IF rising_edge(change_BP) THEN

            CASE SR1_V(4 DOWNTO 2) IS
                WHEN "000" =>
                    Sec_Prot := (OTHERS => '0');
                WHEN "001" =>
                    IF TBPROT = '0' THEN
                        Sec_Prot := (OTHERS => '0');
                        Sec_Prot(SecNumUni downto (SecNumUni+1)*63/64)
                                                := (OTHERS => '1');
                    ELSE
                        Sec_Prot := (OTHERS => '0');
                        Sec_Prot((SecNumUni+1)/64-1 downto 0)
                                                := (OTHERS => '1');
                    END IF;

                WHEN "010" =>
                    IF TBPROT = '0' THEN
                        Sec_Prot := (OTHERS => '0');
                        Sec_Prot(SecNumUni downto (SecNumUni+1)*31/32)
                                                := (OTHERS => '1');
                    ELSE
                        Sec_Prot := (OTHERS => '0');
                        Sec_Prot((SecNumUni+1)/32-1 downto 0)
                                                := (OTHERS => '1');
                    END IF;

                WHEN "011" =>
                    IF TBPROT = '0' THEN
                        Sec_Prot := (OTHERS => '0');
                        Sec_Prot(SecNumUni downto (SecNumUni+1)*15/16)
                                                := (OTHERS => '1');
                    ELSE
                        Sec_Prot := (OTHERS => '0');
                        Sec_Prot((SecNumUni+1)/16-1 downto 0)
                                                := (OTHERS => '1');
                    END IF;

                WHEN "100" =>
                    IF TBPROT = '0' THEN
                        Sec_Prot := (OTHERS => '0');
                        Sec_Prot(SecNumUni downto (SecNumUni+1)*7/8)
                                                := (OTHERS => '1');
                    ELSE
                        Sec_Prot := (OTHERS => '0');
                        Sec_Prot((SecNumUni+1)/8-1 downto 0)
                                                := (OTHERS => '1');
                    END IF;

                WHEN "101" =>
                    IF TBPROT = '0' THEN
                        Sec_Prot := (OTHERS => '0');
                        Sec_Prot(SecNumUni downto (SecNumUni+1)*3/4)
                                                := (OTHERS => '1');
                    ELSE
                        Sec_Prot := (OTHERS => '0');
                        Sec_Prot((SecNumUni+1)/4-1 downto 0)
                                                := (OTHERS => '1');
                    END IF;

                WHEN "110" =>
                    IF TBPROT = '0' THEN
                        Sec_Prot := (OTHERS => '0');
                        Sec_Prot(SecNumUni downto (SecNumUni+1)/2)
                                                := (OTHERS => '1');
                    ELSE
                        Sec_Prot := (OTHERS => '0');
                        Sec_Prot((SecNumUni+1)/2-1 downto 0)
                                                := (OTHERS => '1');
                    END IF;

                WHEN OTHERS =>
                    Sec_Prot := (OTHERS => '1');
            END CASE;
        END IF;
    END PROCESS Protect;

    WP_PULL_UP : PROCESS(WPNegIn)
    BEGIN
        IF (QUAD = '0') THEN
            IF (WPNegIn = 'Z') THEN
                WPNeg_pullup <= '1';
            ELSE
                WPNeg_pullup <= WPNegIn;
            END IF;
        END IF;
    END PROCESS WP_PULL_UP;

    RST_PULL_UP : PROCESS(RESETNeg)
    BEGIN
        IF (RESETNeg = 'Z') THEN
            RESETNeg_pullup <= '1';
        ELSE
            RESETNeg_pullup <= RESETNeg;
        END IF;
    END PROCESS RST_PULL_UP;

    ---------------------------------------------------------------------------
    ---- File Read Section - Preload Control
    ---------------------------------------------------------------------------
    MemPreload : PROCESS

        -- text file input variables
        FILE mem_file         : text  is  mem_file_name;
        FILE otp_file         : text  is  otp_file_name;
        VARIABLE ind          : NATURAL RANGE 0 TO AddrRANGE := 0;
        VARIABLE S_ind        : NATURAL RANGE 0 TO SecNumUni := 0;
        VARIABLE index        : NATURAL RANGE 0 TO SecSize   :=0;
        VARIABLE otp_ind      : NATURAL RANGE 16#00# TO 16#FF# := 16#00#;
        VARIABLE buf          : line;
        VARIABLE reported     : NATURAL;

    BEGIN
    ---------------------------------------------------------------------------
    --cy15b104qs memory preload file format
    -----------------------------------
    ---------------------------------------------------------------------------
    --   /       - comment
    --   @aaaaaa - <aaaaaa> stands for address
    --   dd      - <dd> is byte to be written at Mem(aaaaaa++)
    --             (aaaaaa is incremented at every load)
    --   only first 1-5 columns are loaded. NO empty lines !!!!!!!!!!!!!!!!
    ---------------------------------------------------------------------------
         -- memory preload
        IF (mem_file_name /= "none" AND UserPreload ) THEN
            ind := 0;
            reported := 0;
            Mem := (OTHERS => MaxData);
            WHILE (not ENDFILE (mem_file)) LOOP
                READLINE (mem_file, buf);
                IF buf(1) = '/' THEN --comment
                    NEXT;
                ELSIF buf(1) = '@' THEN --address
                    ind := h(buf(2 to 7));
                ELSE
                    IF ind <= AddrRANGE THEN
                        Mem(ind) := h(buf(1 to 2));
                        IF ind < AddrRANGE THEN
                            ind := ind + 1;
                        END IF;
                    ELSIF reported = 0 THEN
                        REPORT " Memory address out of range"
                        SEVERITY warning;
                        reported := 1;
                    END IF;
                END IF;
            END LOOP;
        END IF;

    ---------------------------------------------------------------------------
    --cy15b104qs_otp memory preload file format
    ---------------------------------------------------------------------------
    --   /       - comment
    --   @aaa - <aaa> stands for address
    --   dd      - <dd> is byte to be written at OTPMem(aaa++)
    --             (aaa is incremented at every load)
    --   only first 1-4 columns are loaded. NO empty lines !!!!!!!!!!!!!!!!
    ---------------------------------------------------------------------------

         -- memory preload
        IF (otp_file_name /= "none" AND UserPreload) THEN
            otp_ind := 16#00#;
            OTPMem := (OTHERS => MaxData);
            WHILE (not ENDFILE (otp_file)) LOOP
                READLINE (otp_file, buf);
                IF buf(1) = '/' THEN
                    NEXT;
                ELSIF buf(1) = '@' THEN
                    IF otp_ind > 16#FF# OR otp_ind < 16#00# THEN
                        ASSERT false
                            REPORT "Given preload address is out of" &
                                   "OTP address range"
                            SEVERITY warning;
                    ELSE
                        otp_ind := h(buf(2 to 3)); --address
                    END IF;
                ELSE
                    OTPMem(otp_ind) := h(buf(1 to 2));
                    otp_ind := otp_ind + 1;
                END IF;
            END LOOP;
        END IF;

        WAIT;
    END PROCESS MemPreload;

    ----------------------------------------------------------------------------
    -- Path Delay Section
    ----------------------------------------------------------------------------

    S_Out_PathDelay_Gen : PROCESS(SOut_zd)

            VARIABLE SO_GlitchData : VitalGlitchDataType;
        BEGIN
            VitalPathDelay01Z (
                OutSignal       => SOut,
                OutSignalName   => "SO",
                OutTemp         => SOut_zd,
                Mode            => VitalTransport,
                GlitchData      => SO_GlitchData,
                Paths           => (
                    0 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_SCK_SO),
                        PathCondition   => NOT(ddr)),
                    1 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay   => VitalExtendtofillDelay(tpd_SCK_SO),
                        PathCondition   => (ddr)),
                    2 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_SO_normal_rd,
                        PathCondition   => CSNeg_ipd = '1' AND NOT rst_quad),
                    3 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_SO_ddr_rd,
                        PathCondition   => CSNeg_ipd = '1' AND rst_quad)
                )
            );
        END PROCESS;

    SI_Out_PathDelay : PROCESS(SIOut_zd)

            VARIABLE SI_GlitchData : VitalGlitchDataType;
        BEGIN
            VitalPathDelay01Z (
                OutSignal       => SIOut,
                OutSignalName   => "SI",
                OutTemp         => SIOut_zd,
                Mode            => VitalTransport,
                GlitchData      => SI_GlitchData,
                Paths           => (
                    0 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_SCK_SO),
                        PathCondition => dual AND NOT(ddr)),
                    1 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay   => VitalExtendtofillDelay(tpd_SCK_SO),
                        PathCondition   => dual AND ddr),
                    2 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_SO_normal_rd,
                        PathCondition   => CSNeg_ipd = '1' AND NOT rst_quad
                                            AND dual),
                    3 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_SO_ddr_rd,
                        PathCondition   => CSNeg_ipd = '1' AND rst_quad
                                            AND dual)
                )
            );
        END PROCESS;

    RESET_Out_PathDelay : PROCESS(RESETNegOut_zd)

            VARIABLE RST_GlitchData : VitalGlitchDataType;
        BEGIN
            VitalPathDelay01Z (
                OutSignal       => RESETNegOut,
                OutSignalName   => "RESETNeg",
                OutTemp         => RESETNegOut_zd,
                Mode            => VitalTransport,
                GlitchData      => RST_GlitchData,
                Paths           => (
                    0 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_SCK_SO),
                        PathCondition   =>  not(ddr) AND (QUAD = '1' OR QPI = '1')),
                    1 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_SCK_SO),
                        PathCondition   => ddr AND (QUAD = '1' OR QPI = '1')),
                    2 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_SO_normal_rd,
                        PathCondition   => CSNeg_ipd = '1' AND
                                            NOT rst_quad AND QUAD = '1'),
                    3 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_SO_ddr_rd,
                        PathCondition   => CSNeg_ipd = '1' AND
                                            rst_quad AND QUAD = '1')
                )
            );
        END PROCESS;

    WP_Out_PathDelay : PROCESS(WPNegOut_zd)

            VARIABLE WP_GlitchData : VitalGlitchDataType;
        BEGIN
            VitalPathDelay01Z (
                OutSignal       => WPNegOut,
                OutSignalName   => "WPNeg",
                OutTemp         => WPNegOut_zd,
                Mode            => VitalTransport,
                GlitchData      => WP_GlitchData,
                Paths           => (
                    0 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_SCK_SO),
                        PathCondition   =>  not(ddr) AND (QUAD = '1' OR QPI = '1')),
                    1 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_SCK_SO),
                        PathCondition   => ddr AND (QUAD = '1' OR QPI = '1')),
                    2 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_SO_normal_rd,
                        PathCondition   => CSNeg_ipd = '1' AND
                                            NOT rst_quad AND QUAD = '1'),
                    3 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_SO_ddr_rd,
                        PathCondition   => CSNeg_ipd = '1' AND
                                            rst_quad AND QUAD = '1')
                )
            );
        END PROCESS;

    END BLOCK behavior;
END vhdl_behavioral_static_memory_allocation;
