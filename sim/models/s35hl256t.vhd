-------------------------------------------------------------------------------
--  File Name: s35hl256t.vhd
-------------------------------------------------------------------------------
--  Copyright (C) 2020 Free Model Foundry; https://www.FreeModelFoundry.com
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License version 2 as
--  published by the Free Software Foundation.
--
--  MODIFICATION HISTORY:
--
--  version: |  author:     | mod date:   |  changes made:
--    V1.0      M.Krneta        20 Apr 27    Initial Release 
--                                           rev *C, Revised Mar 24, 2020
-------------------------------------------------------------------------------
--  PART DESCRIPTION:
--
--  Library:    FLASH
--  Technology: FLASH MEMORY
--  Part:       S35HL256T
--
--   Description: 256 Megabit Serial Flash Memory
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
                USE IEEE.VITAL_timing.ALL;
                USE IEEE.VITAL_primitives.ALL;

LIBRARY FMF;    USE FMF.gen_utils.ALL;
                USE FMF.conversions.ALL;
-------------------------------------------------------------------------------
-- ENTITY DECLARATION
-------------------------------------------------------------------------------
ENTITY s35hl256t IS
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
        tipd_IO3RESETNeg        : VitalDelayType01  := VitalZeroDelay01;

        -- tpd delays: propagation delays (pin-to-pin delay within a component)
        tpd_sck_so_timing_30                   : VitalDelayType01Z := UnitDelay01Z; -- tV
        tpd_sck_so_ntb_timing_30               : VitalDelayType01Z := UnitDelay01Z; -- tV
        tpd_sck_so_op_ddr_an_timing_30_cp      : VitalDelayType01Z := UnitDelay01Z; -- tV
        tpd_sck_so_op_ddr_an_ntb_timing_30_cp  : VitalDelayType01Z := UnitDelay01Z; -- tV
        
        tpd_SCK_SO                 : VitalDelayType01Z := UnitDelay01Z; -- tV
        tpd_SCK_SO_op_ddr_cp       : VitalDelayType01Z := UnitDelay01Z; -- tV
        tpd_CSNeg_SO_rst_quad_EQ_1 : VitalDelayType01Z := UnitDelay01Z; -- tDIS
        tpd_CSNeg_SO_rst_quad_EQ_0 : VitalDelayType01Z := UnitDelay01Z; -- tDIS

        -- tsetup values: setup times
        --   setup time is minimum time before the referent signal edge the
        --   input should be stable
        tsetup_CSNeg_SCK       : VitalDelayType := UnitDelay; -- tCSS /
        tsetup_SI_SCK_Negddr_F51M_NOEDGE_POSEDGE       : VitalDelayType := UnitDelay;  --tDSV,tDSZ edge /
        tsetup_SI_SCK_Negddr_NegF51M_NOEDGE_POSEDGE    : VitalDelayType := UnitDelay;
        tsetup_CSNeg_SCK_Negddr_F51M_NOEDGE_POSEDGE       : VitalDelayType := UnitDelay;
        tsetup_CSNeg_SCK_ddr_F51M_NOEDGE_POSEDGE       : VitalDelayType := UnitDelay;
        
        tsetup_SI_SCK           : VitalDelayType := UnitDelay;
--         tsetup_SO_SCK          : VitalDelayType := UnitDelay; -- tSU:DAT /
--         tsetup_S2_SCK          : VitalDelayType := UnitDelay; -- tSU:DAT /
--         tsetup_S3_SCK          : VitalDelayType := UnitDelay; -- tSU:DAT /
        tsetup_SI_SCK_double_noedge_posedge   : VitalDelayType
                                                := UnitDelay; -- tSU:DAT /
        tsetup_SI_SCK_double_noedge_negedge   : VitalDelayType
                                                := UnitDelay; -- tSU:DAT /
        tsetup_WPNeg_CSNeg     : VitalDelayType := UnitDelay; -- tWPS \
        tsetup_RESETNeg_CSNeg  : VitalDelayType := UnitDelay; -- tRS

        -- thold values: hold times
        --   hold time is minimum time the input should be present stable
        --   after the referent signal edge
        thold_CSNeg_SCK        : VitalDelayType := UnitDelay; -- tCSH /
        thold_SI_SCK           : VitalDelayType := UnitDelay;
        thold_SI_SCK_Negddr_F51M_NOEDGE_POSEDGE       : VitalDelayType := UnitDelay;  --tDSV,tDSZ edge /
        thold_SI_SCK_Negddr_NegF51M_NOEDGE_POSEDGE    : VitalDelayType := UnitDelay;
        thold_SI_SCK_ddr_F51M_NOEDGE_POSEDGE    : VitalDelayType := UnitDelay;
        thold_SI_SCK_double_noedge_posedge : VitalDelayType
                                                := UnitDelay; -- tHD:DAT /
        thold_SI_SCK_double_noedge_negedge : VitalDelayType
                                                := UnitDelay; -- tHD:DAT /
        thold_csneg_sck_noedge_negedge : VitalDelayType := UnitDelay;-- tHD:DAT /
        thold_csneg_sck_ntb_ddr_noedge_posedge : VitalDelayType := UnitDelay; -- tHD:DAT /                                        
        thold_WPNeg_CSNeg      : VitalDelayType := UnitDelay; -- tWPH /
        thold_CSNeg_RESETNeg   : VitalDelayType := UnitDelay; -- tRH
        thold_CSNeg_IO3RESETNeg: VitalDelayType := UnitDelay; -- tRH

        --tpw values: pulse width
        tpw_SCK_normal_rd      : VitalDelayType := UnitDelay;
        tpw_SCK_fast_rd        : VitalDelayType := UnitDelay;
        tpw_SCK_fast_rd1       : VitalDelayType := UnitDelay;
        tpw_SCK_ddr_rd         : VitalDelayType := UnitDelay;
        tpw_CSNeg_posedge      : VitalDelayType := UnitDelay; -- tCS
        tpw_CSNeg_rst_quad_posedge : VitalDelayType := UnitDelay; -- tCS
        tpw_CSNeg_RDYBSY_posedge  : VitalDelayType := UnitDelay; -- tCS
        tpw_RESETNeg_negedge   : VitalDelayType := UnitDelay; -- tRP
        tpw_RESETNeg_posedge   : VitalDelayType := UnitDelay; -- tRS
        tpw_IO3RESETNeg_negedge: VitalDelayType := UnitDelay; -- tRP
        tpw_IO3RESETNeg_posedge: VitalDelayType := UnitDelay; -- tRS

        -- tperiod min (calculated as 1/max freq)
        tperiod_SCK_normal_rd  : VitalDelayType := UnitDelay; --fSCK=50MHz
        tperiod_SCK_fast_rd    : VitalDelayType := UnitDelay; --fSCK=166MHz
        tperiod_SCK_fast_rd1   : VitalDelayType := UnitDelay; --fSCK=133MHz
        tperiod_SCK_ddr_rd     : VitalDelayType := UnitDelay; --fSCK=102MHz

        -- tdevice values: values for internal delays
        --timing values that are internal to the model and not associated
        --with any port.
        -- WRR Cycle Time
        tdevice_WRR             : VitalDelayType := 357.5 ms;  --tW
        -- Page Program Operation
        tdevice_PP_256          : VitalDelayType := 1700 us;  --tPP
        -- Page Program Operation
        tdevice_PP_512          : VitalDelayType := 1700 us;  --tPP
        -- Sector Erase Operation
        tdevice_SE4             : VitalDelayType := 335 ms; --tSE
        -- Sector Erase Operation
        tdevice_SE256           : VitalDelayType := 2677 ms; --tSE
        -- Bulk Erase Operation
        tdevice_BE              : VitalDelayType := 348 sec; --tBE
        -- Evaluate Erase Status Time
        tdevice_EES             : VitalDelayType := 51 us;  --tEES
        -- Program/Erase Suspend/Resume Time
        tdevice_PSUSP            : VitalDelayType := 64 us;   --tPSL
        -- Program/Erase Suspend/Resume Time
        tdevice_ESUSP            : VitalDelayType := 64 us;   --tESL
        -- VCC (min) to CS# Low
        tdevice_PU              : VitalDelayType := 450 us;  --tPU
        -- Resume to next Suspend Time
        tdevice_RS              : VitalDelayType := 100 us;  --tRS
        -- DIC setup time
        tdevice_DICSETUP        : VitalDelayType := 17 us;
        -- Sector Erase Count register max time
        tdevice_SEERC           : VitalDelayType := 63 us; 
        -- DIC suspend latency
        tdevice_DICSL           : VitalDelayType := 55 us;   --tDICSL
        -- DIC Resume to next suspend
        tdevice_DICRL           : VitalDelayType := 100 us;
        -- RESET# Low to CS# Low
        tdevice_RPH             : VitalDelayType := 450 us;  --tRPH
        -- CS# High before HW Reset (Quad mode and Reset Feature are enabled)
        tdevice_CS              : VitalDelayType := 50 ns;   --tCS
         -- CS# High to Power Down Mode
        tdevice_DPD          : VitalDelayType := 3 us;   -- tDPD
        -- CS# High to exit Power Down Mode
        tdevice_CSDDPD          : VitalDelayType := 20 ns;   -- tDPD
        -- CS# High to StandBy mode without Electronic Signature read
        tdevice_EXTDPD          : VitalDelayType := 380 us;  --tRES
        -- CS# High to StandBy mode without Electronic Signature read
        tdevice_RES             : VitalDelayType := 60 us;  --tRES
        
        tdevice_CSPOR            : VitalDelayType := 150 us;
    ---------------------------------------------------------------------------
    -- CONTROL GENERICS:
    ---------------------------------------------------------------------------
        -- generic control parameters
        InstancePath      : STRING    := DefaultInstancePath;
        TimingChecksOn    : BOOLEAN   := DefaultTimingChecks;
        MsgOn             : BOOLEAN   := DefaultMsgOn;
        XOn               : BOOLEAN   := DefaultXon;
        -- memory file to be loaded
        mem_file_name     : STRING    := "s35hl256t.mem";
        otp_file_name     : STRING    := "s35hl256tOTP.mem";

        UserPreload       : BOOLEAN   := FALSE; --TRUE;
        LongTimming       : BOOLEAN   := FALSE;

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
        RESETNeg          : INOUT std_ulogic := 'U'; -- hardware reset pin
        WPNeg             : INOUT std_ulogic := 'U'; -- write protect input/IO2
        IO3RESETNeg       : INOUT std_ulogic := 'U'  -- hold input/IO3
    );

    ATTRIBUTE VITAL_LEVEL0 of s35hl256t : ENTITY IS TRUE;
END s35hl256t;

-------------------------------------------------------------------------------
-- ARCHITECTURE DECLARATION
-------------------------------------------------------------------------------


ARCHITECTURE vhdl_behavioral of s35hl256t IS
    ATTRIBUTE VITAL_LEVEL0 OF
    vhdl_behavioral : ARCHITECTURE IS TRUE;

    ---------------------------------------------------------------------------
    -- CONSTANT AND SIGNAL DECLARATION
    ---------------------------------------------------------------------------
    --Declaration of constants - memory characteristics
        -- The constant declared here are used to enable the creation of models
        -- of memories within a family with a minimum amount of editing

    CONSTANT PartID             : STRING  := "s35hl256t";
    CONSTANT MaxData            : NATURAL := 16#FF#;        --255;
    CONSTANT MemSize            : NATURAL := 16#1FFFFFF#;
    CONSTANT SecNumUni          : NATURAL := 127;
    CONSTANT SecNumHyb          : NATURAL := 159;
    CONSTANT SecSize256         : NATURAL := 16#3FFFF#;     --256KB
    CONSTANT SecSize4           : NATURAL := 16#FFF#;       --4KB
    CONSTANT AddrRANGE          : NATURAL := 16#1FFFFFF#;
    CONSTANT HiAddrBit          : NATURAL := 31;
    CONSTANT OTPSize            : NATURAL := 1023;
    CONSTANT OTPLoAddr          : NATURAL := 16#000#;
    CONSTANT OTPHiAddr          : NATURAL := 16#3FF#;
    CONSTANT SFDPLoAddr         : NATURAL := 16#0000#;
    CONSTANT SFDPHiAddr         : NATURAL := 16#0247#;
    CONSTANT SFDPLength         : NATURAL := 16#0247#;
    CONSTANT MDIDLoAddr         : NATURAL := 16#00#;
    CONSTANT MDIDHiAddr         : NATURAL := 16#0F#;
    CONSTANT MDIDLength         : NATURAL := 16#0F#;
    CONSTANT IDLength           : NATURAL := 16#0F#;
    CONSTANT BYTE               : NATURAL := 8;
    CONSTANT CryptoPacketSize   : NATURAL := 839;
    
        -- ECC data unit check
    SHARED VARIABLE ECC_data  : NATURAL := 0;
    SHARED VARIABLE ECC_check     : NATURAL := 0;
    SHARED VARIABLE DEBUG_ADDR     : NATURAL := 0;
    SHARED VARIABLE ECC_ERR     : NATURAL := 0;
    SHARED VARIABLE DEBUG_CHECK     : NATURAL := 0;

    --Manufacturer Identification
    CONSTANT Manuf_ID      : NATURAL := 16#01#;
    CONSTANT DeviceID      : NATURAL := 16#19#;
    --Electronic Signature
    CONSTANT ESignature    : NATURAL := 16#19#;
    --Device ID
    --Manufacturer Identification && Memory Type && Memory Capacity
    CONSTANT Jedec_ID      : NATURAL := 16#01#; -- first byte of Device ID
    CONSTANT DeviceUID     : NATURAL := 16#07#;
    CONSTANT DeviceID1     : NATURAL := 16#02#;
    CONSTANT DeviceID2     : NATURAL := 16#20#;
    CONSTANT ExtendedBytes : NATURAL := 16#4D#;
    CONSTANT ExtendedID    : NATURAL := 16#00#;
    CONSTANT DieRev        : NATURAL := 16#00#;
    CONSTANT MaskRev       : NATURAL := 16#00#;
    CONSTANT HOLD_CSNeg_RSTNeg : TIME := 34800 ns;
    SIGNAL   UID_reg             :std_logic_vector(63 DOWNTO 0) := (OTHERS => '0');
--     SIGNAL MDID_reg        :std_logic_vector(127 DOWNTO 0) := "1111111111111111111111111111111111111111
--                                                                1111111111111111111111111111111111111111
--                                                                1001000000000000001100001111000110100101
--                                                                101100110100" --???

    -- Declaration of signals that will hold the delayed values of ports
    SIGNAL SI_ipd          : std_ulogic := 'U';
    SIGNAL SO_ipd          : std_ulogic := 'U';
    SIGNAL SCK_ipd         : std_ulogic := 'U';
    SIGNAL CSNeg_ipd       : std_ulogic := 'U';
    SIGNAL RESETNeg_ipd    : std_ulogic := 'U';
    SIGNAL WPNeg_ipd       : std_ulogic := 'U';
    SIGNAL IO3RESETNeg_ipd : std_ulogic := 'U';

    SIGNAL IO3RESETNeg_pullup : std_ulogic := 'U';
    SIGNAL RESETNeg_pullup : std_ulogic := 'U';
    SIGNAL WPNeg_pullup    : std_ulogic := 'U';

    -- internal delays
    SIGNAL WRR_in          : std_ulogic := '0';
    SIGNAL WRR_out         : std_ulogic := '0';
    SIGNAL PP_256_in       : std_ulogic := '0';
    SIGNAL PP_256_out      : std_ulogic := '0';
    SIGNAL PP_512_in       : std_ulogic := '0';
    SIGNAL PP_512_out      : std_ulogic := '0';
    SIGNAL SE4_in          : std_ulogic := '0';
    SIGNAL SE4_out         : std_ulogic := '0';
    SIGNAL SE256_in        : std_ulogic := '0';
    SIGNAL SE256_out       : std_ulogic := '0';
    SIGNAL BE_in           : std_ulogic := '0';
    SIGNAL BE_out          : std_ulogic := '0';
    SIGNAL EES_in          : std_ulogic := '0';
    SIGNAL EES_out         : std_ulogic := '0';
    SIGNAL ERSSUSP_in      : std_ulogic := '0';
    SIGNAL ERSSUSP_out     : std_ulogic := '0';
    SIGNAL PRGSUSP_in      : std_ulogic := '0';
    SIGNAL PRGSUSP_out     : std_ulogic := '0';
    SIGNAL ERSSUSP_tmp_in  : std_ulogic := '0';
    SIGNAL ERSSUSP_tmp_out : std_ulogic := '0';
    SIGNAL PRGSUSP_tmp_in  : std_ulogic := '0';
    SIGNAL PRGSUSP_tmp_out : std_ulogic := '0';
    SIGNAL PU_in           : std_ulogic := '0';
    SIGNAL PU_out          : std_ulogic := '0';
    SIGNAL RS_in           : std_ulogic := '0';
    SIGNAL RS_out          : std_ulogic := '0';
    SIGNAL DICSETUP_in     : std_ulogic := '0';
    SIGNAL DICSETUP_out    : std_ulogic := '0';
    SIGNAL DICSL_in        : std_ulogic := '0';
    SIGNAL DICSL_out       : std_ulogic := '0';
    SIGNAL DICRL_in        : std_ulogic := '0';
    SIGNAL DICRL_out       : std_ulogic := '0';
    SIGNAL sSTART_T1       : std_ulogic := '0'; --Start TimeOut
    SIGNAL START_T1_in     : std_ulogic := '0';
    SIGNAL RPH_in          : std_ulogic := '0';
    SIGNAL RPH_out         : std_ulogic := '0';
    SIGNAL CS_in           : std_ulogic := '0';
    SIGNAL CS_out          : std_ulogic := '1';
    SIGNAL PPBERASE_in     : std_ulogic := '0';
    SIGNAL PPBERASE_out    : std_ulogic := '0';
    SIGNAL PASSULCK_in     : std_ulogic := '0';
    SIGNAL PASSULCK_out    : std_ulogic := '0';
    
    --     SIGNAL DPDExt          : std_ulogic := '0';
    SIGNAL DPDExt_in       : std_ulogic := '0';
    SIGNAL DPDExt_out      : std_ulogic := '1';
    SIGNAL DPD_in          : std_ulogic := '0';
--     SIGNAL DPD_out         : std_ulogic := '1';
    SIGNAL DPDEnt_out      : std_ulogic := '0';
    SIGNAL SEERC_START     : std_logic  := '0'; --Sector Erase Start
    SIGNAL SEERC_DONE      : std_logic  := '1'; --Sector Erase Done
    
    SIGNAL CRYPTO_in       : std_logic  := '0'; --Crypto mode flag


    -- Clock period
    SHARED VARIABLE CK_PER        : time      := 0 ns;
    SHARED VARIABLE CK_PERIOD     : time      := 0 ns;
    
    SHARED VARIABLE wob      : time;
    SHARED VARIABLE wob1      : time;
    
    SHARED VARIABLE counter_clock            : std_logic_vector(31 downto 0)
                                                := (others => '0');
    SIGNAL freq51      : std_logic
                                                         := '0';
    SIGNAL non_industrial_temp  : std_ulogic := '0';
    
    SIGNAL timing_30                    : std_logic := '0';

    ---------------------------------------------------------------------------
    -- Memory data initial value.
    ---------------------------------------------------------------------------
    SHARED VARIABLE max_data     : NATURAL := 16#FF#;

BEGIN
    ---------------------------------------------------------------------------
    -- Internal Delays
    ---------------------------------------------------------------------------
    -- Artificial VITAL primitives to incorporate internal delays
    -- Because a tdevice generics is used, there must be a VITAL_primitives
    -- assotiated with them
    WRR    : VitalBuf(WRR_out,     WRR_in,    (tdevice_WRR     ,UnitDelay));
    PP_256 : VitalBuf(PP_256_out,  PP_256_in, (tdevice_PP_256  ,UnitDelay));
    PP_512 : VitalBuf(PP_512_out,  PP_512_in, (tdevice_PP_512  ,UnitDelay));
    SE4    : VitalBuf(SE4_out,     SE4_in,    (tdevice_SE4     ,UnitDelay));
    SE256  : VitalBuf(SE256_out,   SE256_in,  (tdevice_SE256   ,UnitDelay));
    BE     : VitalBuf(BE_out,      BE_in,     (tdevice_BE      ,UnitDelay));
    EES    : VitalBuf(EES_out,     EES_in,    (tdevice_EES     ,UnitDelay));
    ESUSP  : VitalBuf(ERSSUSP_tmp_out, ERSSUSP_tmp_in, (tdevice_ESUSP,UnitDelay));
    PSUSP  : VitalBuf(PRGSUSP_tmp_out, PRGSUSP_tmp_in, (tdevice_PSUSP,UnitDelay));
    DICSL  : VitalBuf(DICSL_out,   DICSL_in,  (tdevice_DICSL   ,UnitDelay));
    DICRL  : VitalBuf(DICRL_out,   DICRL_in,  (tdevice_DICRL   ,UnitDelay));
    RPH    : VitalBuf(RPH_out,     RPH_in,    (tdevice_RPH     ,UnitDelay));
    CS     : VitalBuf(CS_out,      CS_in,     (tdevice_CS      ,UnitDelay));
    PU     : VitalBuf(PU_out,      PU_in,     (tdevice_PU      ,UnitDelay));
    DPD    : VitalBuf(DPDEnt_out,     DPD_in,    (tdevice_DPD     ,UnitDelay));
--     SEERC  : VitalBuf(SEERC_DONE,  SEERC_START, (tdevice_SEERC   ,UnitDelay));

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
        w_7 : VitalWireDelay (IO3RESETNeg_ipd,IO3RESETNeg,tipd_IO3RESETNeg);

    END BLOCK;

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
            RESETNegIn     : IN    std_ulogic := 'U';
            RESETNegOut    : OUT   std_ulogic := 'U';
            WPNegIn        : IN    std_ulogic := 'U';
            WPNegOut       : OUT   std_ulogic := 'U';
            IO3RESETNegIn  : IN    std_ulogic := 'U';
            IO3RESETNegOut : OUT   std_ulogic := 'U'
        );

        PORT MAP (
             SIIn       => SI_ipd,
             SIOut      => SI,
             SOIn       => SO_ipd,
             SOut       => SO,
             SCK        => SCK_ipd,
             CSNeg      => CSNeg_ipd,
             RESETNegIn  => RESETNeg_ipd,
             RESETNegOut => RESETNeg,
             WPNegIn    => WPNeg_ipd,
             WPNegOut   => WPNeg,
             IO3RESETNegIn  => IO3RESETNeg_ipd,
             IO3RESETNegOut => IO3RESETNeg
        );

        -- State Machine : State_Type
        TYPE state_type IS (IDLE,
                            RESET_STATE,
                            PGERS_ERROR,
                            AUTOBOOT,
                            WRITE_SR,
                            WRITE_ALL_REG,
                            PAGE_PG,
                            OTP_PG,
                            PG_SUSP,
                            SECTOR_ERS,
                            BULK_ERS,
                            ERS_SUSP,
                            ERS_SUSP_PG,
                            ERS_SUSP_PG_SUSP,
                            DIC_Calc,
                            DIC_SUSP,
                            DP_DOWN,
                            PASS_PG,
                            PASS_UNLOCK,
                            PPB_PG,
                            PPB_ERS,
                            AUTOBOOT_PG,
                            ASP_PG,
                            PLB_PG,
                            DYB_PG,
                            NVDLR_PG,
                            BLANK_CHECK,
                            EVAL_ERS_STAT,
                            LOCKED_STATE,
                            SEERC
                            );
                            
        TYPE state_type_fsm2 IS (SIGRES_IDLE,
                            SIGRES_FIRST_FE,
                            SIGRES_FIRST_RE,
                            SIGRES_SECOND_FE,
                            SIGRES_SECOND_RE,
                            SIGRES_THIRD_FE,
                            SIGRES_THIRD_RE,
                            SIGRES_NOT_A_RESET
                            );

        -- Instruction Type
        TYPE instruction_type IS ( NONE,
                                   WRENB_0_0,       -- Write Enable
                                   WRDIS_0_0,       -- Write Disable
                                   WRREG_0_1,        -- Write Register
                                   WRENV_0_0,      -- Write Enable for Volatile Status
                                   WRARG_C_1,       -- Write Any Register
                                   CLPEF_0_0,       -- Clear Status Register
                                   RDARG_C_0,       -- Read Any Register
                                   RDSR1_0_0,      -- Read Status Register 1
                                   RDSR2_0_0,      -- Read Status Register 2
                                   RDCR1_0_0,      -- Read Configuration Register 1
                                   RDIDN_0_0,       -- Read ID JEDEC
                                   RDQID_0_0,      -- Read Quad ID
                                   RSFDP_3_0,      -- Read JEDEC SFDP
                                   RDUID_0_0,       -- Read Unique ID
                                   EN4BA_0_0,       -- Enter 4Byte Address Mode
                                   EX4BA_0_0,       -- Exit 4Byte Address Mode
                                   RDECC_C_0,      -- ECC Read
                                   RDECC_4_0,     -- ECC Read (4Byte +)
                                   CLECC_0_0,      -- Clear ECC Status Register
                                   RDDLP_0_0,      -- Read Data Learning Pattern
                                   PRDLP_0_1,     -- Program NVDLP Reg
                                   WRDLP_0_1,      -- Write Volatile DLP Reg
                                   WRAUB_0_1,       -- AutoBoot Register Write
                                   DICHK_4_1,        -- DIC Calculation
                                   RDAY1_C_0,       -- Read Normal (3Byte Address)
                                   RDAY1_4_0,      -- Read Normal (4Byte +)
                                   RDAY2_C_0,       -- Fast Read (3Byte Address)
                                   RDAY2_4_0,       -- Fast Read (4Byte +) --RDAY4_4_0
                                   RDAY3_C_0,       -- Read Dual I/O (3Byte Address) --RDAY4_C_0
                                   RDAY3_4_0,      -- Read Dual I/O (4Byte +) --RDAY8_4_0
                                   RDAY4_C_0,        -- Read Quad Out (3Byte Address) --RDAY3_C_0
                                   RDAY4_4_0,       -- Read Quad Out (4Byte +) --RDAY2_4_0
                                   RDAY5_C_0,       -- Read Quad I/O (3Byte Address) --RDAY0_C_0
                                   RDAY5_4_0,      -- Read Quad I/O (4Byte +) --RDAY9_C_0
                                   RDAY7_C_0,    -- Read DDR Quad I/O (3Byte) --RDAY5_C_0
                                   RDAY7_4_0,   -- Read DDR Quad I/O (4Byte +) --RDAY5_4_0
                                   PRPGE_C_1,         -- Program Page (3Byte Address)
                                   PRPGE_4_1,        -- Program Page (4Byte +)
                                   ERCHP_0_0,         -- Bulk Erase
                                   ER256_C_0,         -- Erase 128/256KB (3Byte)
                                   ER256_4_0,        -- Erase 128/256KB (4Byte +)
                                   ER004_C_0,        -- 4KB-sector Erase (3Byte Addr)
                                   ER004_4_0,       -- 4KB-sector Erase (4Byte Addr)
                                   EVERS_C_0,        -- Evaluate Erase Status
                                   SEERC_C_0,        -- Sector Erase Count
                                   SPEPD_0_0,       -- Erase/Program DIC Suspend
                                   SPEPA_0_0,        -- Erase/Program Suspend
                                   RSEPD_0_0,       -- Erase/Program DIC Resume
                                   RSEPA_0_0,        -- Erase/Program Resume
                                   PRSSR_C_1,       -- OTP Program
                                   RDSSR_C_0,       -- OTP Read
                                   RDDYB_C_0,      -- DYB Read
                                   RDDYB_4_0,     -- DYB Read
                                   WRDYB_C_1,      -- DYB Write
                                   WRDYB_4_1,     -- DYB Write
                                   RDPPB_C_0,      -- PPB Read
                                   RDPPB_4_0,     -- PPB Read
                                   PRPPB_C_0,       -- PPB Program
                                   PRPPB_4_0,      -- PPB Program
                                   ERPPB_0_0,       -- PPB Erase
                                   PRASP_0_1,       -- ASP Program
                                   RDPLB_0_0,      -- PPB Lock Bit Read
                                   WRPLB_0_0,      -- PPB Lock Bit Write
                                   PGPWD_0_1,      -- Password Program
                                   PWDUL_0_1,      -- Password Unlock
                                   SRSTE_0_0,      -- Software Reset Enable
                                   SFRST_0_0,     -- Software Reset
                                   SFRSL_0_0,      -- Legacy Software Reset
                                   ENDPD_0_0,       -- DPD Enter
                                   ENCTM_0_0,       -- Enter Crypto Mode
                                   EXCTM_0_0,       -- Exit Crypto Mode
                                   PKRD1_4_0,       -- Packet Read
                                   PKRD2_4_0,
                                   PKRD3_4_0,
                                   PKRD4_4_0,
                                   PKWR1_4_1,       -- Packet Write
                                   PKWR2_4_1,
                                   PKWR3_4_1,
                                   PKWR4_4_1,
                                   RDHL0_0_0,
                                   RDHL1_0_0
                                );

        TYPE WBCryptoType IS ARRAY (0 TO 839) OF INTEGER RANGE -1 TO MaxData;
        -- Hash values
        TYPE RDHLType IS ARRAY (0 to 31) OF INTEGER RANGE -1 TO MaxData;

        TYPE WByteType IS ARRAY (0 TO 511) OF INTEGER RANGE -1 TO MaxData;
        -- Flash Memory Array
        TYPE MemArray IS ARRAY (0 TO AddrRANGE) OF INTEGER RANGE -1 TO MaxData;
        -- OTP Memory Array
        TYPE OTPArray IS ARRAY (OTPLoAddr TO OTPHiAddr) OF INTEGER
                                                    RANGE -1 TO MaxData;
        --CFI Array (Common Flash Interface Query codes)
        TYPE SFDPtype  IS ARRAY (0 TO SFDPLength) OF INTEGER
                                                    RANGE -1 TO MaxData;
        TYPE MDIDtype  IS ARRAY (0 TO MDIDLength) OF INTEGER
                                                    RANGE -1 TO MaxData;
                                                    

        -- Main Memory
        SHARED VARIABLE Mem           : MemArray  := (OTHERS => MaxData);
        -- OTP Sector
        SHARED VARIABLE OTPMem        : OTPArray  := (OTHERS => MaxData);
        --CFI Array
        --SFDP Array
        SHARED VARIABLE SFDP_array    : SFDPtype   := (OTHERS => 0);
        SHARED VARIABLE MDID_array    : MDIDtype   := (OTHERS => 0);


        SHARED VARIABLE SFDP_array_tmp : std_logic_vector(8*(SFDPLength+1)-1 downto 0);
        SHARED VARIABLE SFDP_tmp : std_logic_vector(7 downto 0);
        
        SHARED VARIABLE MDID_array_tmp : std_logic_vector(8*(MDIDLength+1)-1 downto 0);
        SHARED VARIABLE MDID_tmp : std_logic_vector(7 downto 0);

        -- Programming Buffer
        SIGNAL WByte                 : WByteType    := (OTHERS => MaxData);
        
        SIGNAL WByteCrypto           : WBCryptoType := (OTHERS => MaxData); --Crypto buffer
        SIGNAL RDHL0Buf              : RDHLType := (OTHERS => 16#AA#);
        SIGNAL RDHL1Buf              : RDHLType := (OTHERS => 16#AA#);
        -- states
        SIGNAL current_state         : state_type := RESET_STATE;
        SIGNAL next_state            : state_type := RESET_STATE;
        
        SIGNAL next_sigres_state     : state_type_fsm2 := SIGRES_IDLE;
        SIGNAL current_sigres_state  : state_type_fsm2 := SIGRES_IDLE;

        SIGNAL Instruct              : instruction_type;
        --zero delay signal
        SIGNAL SOut_zd               : std_logic := 'Z';
        SIGNAL SIOut_zd              : std_logic := 'Z';
        SIGNAL RESETNegOut_zd        : std_logic := 'Z';
        SIGNAL WPNegOut_zd           : std_logic := 'Z';
        SIGNAL IO3RESETNegOut_zd     : std_logic := 'Z';
        --HOLD delay on output data
        SIGNAL SOut_z                : std_logic := 'Z';
        SIGNAL SIOut_z               : std_logic := 'Z';
        -- powerup
        SIGNAL PoweredUp             : std_logic := '0';

        -----------------------------------------------------------------------
        -- Registers
        -----------------------------------------------------------------------
        --     ***  Status Register 1  ***

        SIGNAL SR1_in   : std_logic_vector(7 downto 0)   := (others => '0');
        -- Nonvolatile Status Register 1
        SIGNAL  STR1N   : std_logic_vector(7 downto 0)   := (others => '0');

        ALIAS STCFWR_NV      :std_logic IS STR1N(7);
        ALIAS LBPROT2_NV       :std_logic IS STR1N(4);
        ALIAS LBPROT1_NV       :std_logic IS STR1N(3);
        ALIAS LBPROT0_NV       :std_logic IS STR1N(2);

        -- Volatile Status Register 1
        SIGNAL  STR1V   : std_logic_vector(7 downto 0)   := (others => '0');

        -- Status Register Write Disable Bit
        ALIAS STCFWR      :std_logic IS STR1V(7);
        -- Status Register Programming Error Bit
        ALIAS PRGERR     :std_logic IS STR1V(6);
        -- Status Register Erase Error Bit
        ALIAS ERSERR     :std_logic IS STR1V(5);
        -- Status Register Block Protection Bits
        ALIAS LBPROT2       :std_logic IS STR1V(4);
        ALIAS LBPROT1       :std_logic IS STR1V(3);
        ALIAS LBPROT0       :std_logic IS STR1V(2);
        -- Status Register Write Enable Latch Bit
        ALIAS WRPGEN       :std_logic IS STR1V(1);
        -- Status Register Write In Progress Bit
        ALIAS RDYBSY       :std_logic IS STR1V(0);

        -- Volatile Status Register 2
        SIGNAL STR2V   : std_logic_vector(7 downto 0)
                                                := (others => '0');
        -- DIC Suspend
        ALIAS DICRCS      :std_logic IS STR2V(4);
        -- DIC Abort
        ALIAS DICRCA      :std_logic IS STR2V(3);
        -- Erase status
        ALIAS SESTAT     :std_logic IS STR2V(2);
        -- Erase suspend
        ALIAS ERASES        :std_logic IS STR2V(1);
        -- Program suspend
        ALIAS PROGMS        :std_logic IS STR2V(0);

        -- Nonvolatile Configuration Register 1
        SIGNAL CR1_in   : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SIGNAL CR2_in   : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SIGNAL CR3_in   : std_logic_vector(7 downto 0)
                                                := (others => '0');                                        
        SIGNAL CR4_in   : std_logic_vector(7 downto 0)
                                                := (others => '0');                                        
                                                
                                                
                                                
        SIGNAL CFR1N   : std_logic_vector(7 downto 0)
                                                := (others => '0');
        -- Split Parameter Sectors both Top and Bottom
        ALIAS SP4KBS_NV  :std_logic IS CFR1N(6);
        -- Configuration Register TBPROT bit
        ALIAS TBPROT_NV :std_logic IS CFR1N(5);
        -- Configuration Register LOCK bit
        ALIAS PLPROT_NV    :std_logic IS CFR1N(4);
        -- Configuration Register BPNV bit
        ALIAS BPNV_O    :std_logic IS CFR1N(3);
        -- Configuration Register TB4KBS bit
        ALIAS TB4KBS_NV :std_logic IS CFR1N(2);
        -- Configuration Register QUAD bit
        ALIAS QUADIT_NV   :std_logic IS CFR1N(1);
         -- Configuration Register 0 bit
        ALIAS TLPROT_NV   :std_logic IS CFR1N(0);


        --Volatile Configuration Register 1
        SIGNAL CFR1V    : std_logic_vector(7 downto 0)
                                                := (others => '0');
        -- Split Parameter Sectors both Top and Bottom
        ALIAS SPARM     :std_logic IS CFR1V(6);
        -- Configuration Register TBPROT bit
        ALIAS TBPROT    :std_logic IS CFR1V(5);
        -- Configuration Register LOCK bit
        ALIAS PLPROT      :std_logic IS CFR1V(4);
        -- Configuration Register BPNV bit
        ALIAS BPNV      :std_logic IS CFR1V(3);
        -- Configuration Register TB4KBS bit
        ALIAS TB4KBS    :std_logic IS CFR1V(2);
        -- Configuration Register QUAD bit
        ALIAS QUADIT      :std_logic IS CFR1V(1);
        -- Configuration Register TLPROT bit
        ALIAS TLPROT    :std_logic IS CFR1V(0);

        -- Nonvolatile Configuration Register 2
        SIGNAL CFR2N   : std_logic_vector(7 downto 0)
                                                := "00001000";
        -- Volatile Configuration Register 2
        SIGNAL CFR2V    : std_logic_vector(7 downto 0)
                                                := "00001000";
        -- Configuration Register 2 QPI_IT bit
        
        
        ALIAS  QPI_IT    :std_logic IS CFR2V(6);
        SIGNAL STR1V_DPD : std_logic := '0';
        SIGNAL WVREG  : std_logic    := '0'; --Write volatile regs

        -- Nonvolatile Configuration Register 3
        SIGNAL CFR3N   : std_logic_vector(7 downto 0)
                                                := "00000000";
        -- Volatile Configuration Register 3
        SIGNAL CFR3V   : std_logic_vector(7 downto 0)
                                                := "00000000";
        -- Nonvolatile Configuration Register 4
        SIGNAL CFR4N   : std_logic_vector(7 downto 0)
                                                := "00001000";
        -- Volatile Configuration Register 4
        SIGNAL CFR4V   : std_logic_vector(7 downto 0)
                                                := "00001000";
                                                
        ALIAS IOIMPD2       :std_logic IS CFR4V(7);
        ALIAS IOIMPD1       :std_logic IS CFR4V(6);
        ALIAS IOIMPD0       :std_logic IS CFR4V(5);
        --  VDLR Register
        SHARED VARIABLE DLPV    : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SIGNAL DLPV_in            : std_logic_vector(7 downto 0)
                                                := (others => '0');
        -- NVDLR Register
        SHARED VARIABLE DLPN     : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SIGNAL DLPN_in           : std_logic_vector(7 downto 0)
                                                := (others => '0');
        -- ASP Register
        SHARED VARIABLE ASPO        : std_logic_vector(15 downto 0)
                                                     := (others => '1');
        SIGNAL ASPO_in              : std_logic_vector(15 downto 0)
                                                     := (others => '1');
        --Read Password Mode Enable Bit
        ALIAS ASPRDP      :std_logic IS ASPO(5);
        --DYB Lock Boot Bit
        ALIAS ASPDYB    :std_logic IS ASPO(4);
        --PPB OTP Bit
        ALIAS ASPPPB    :std_logic IS ASPO(3);
        -- Password Protection Mode Lock Bit
        ALIAS ASPPWD    :std_logic IS ASPO(2);
        --Persistent Protection Mode Lock Bit
        ALIAS ASPPER    :std_logic IS ASPO(1);
        --Permanent Protection Lock bit
        ALIAS ASPPRM    :std_logic IS ASPO(0);

        --      ***  Password Register  ***
        SHARED VARIABLE PWDO   : std_logic_vector(63 downto 0)
                                                := (others => '1');
        SIGNAL PWDO_in         : std_logic_vector(63 downto 0)
                                                := (others => '1');
        --      ***  PPB Lock Register  ***
        SHARED VARIABLE PPLV           : std_logic_vector(7 downto 0)
                                                := "00000001";
        SIGNAL PPLV_in                 : std_logic_vector(7 downto 0)
                                                := "00000001";
        --Persistent Protection Mode Lock Bit
        ALIAS PPBLCK                  : std_logic IS PPLV(0); 
        SIGNAL PPBLCK_temp            : std_ulogic := '0';
        --      ***  PPB Access Register  ***
        SHARED VARIABLE PPAV          : std_logic_vector(7 downto 0)
                                                := (others => '1');
        SIGNAL PPAV_in                : std_logic_vector(7 downto 0)
                                                := (others => '1');
        -- PPB_bits(Sec)
        SHARED VARIABLE PPB_bits       : std_logic_vector(SecNumHyb downto 0)
                                                := (OTHERS => '1');
        --      ***  DYB Access Register  ***
        SHARED VARIABLE DYAV          : std_logic_vector(7 downto 0)
                                                := (others => '1');
        SIGNAL DYAV_in                : std_logic_vector(7 downto 0)
                                                := (others => '1');
        -- DYB(Sec)
        SHARED VARIABLE DYB_bits       : std_logic_vector(SecNumHyb downto 0)
                                                := (others => '1');
        --      ***  AutoBoot Register  ***
        SHARED VARIABLE ATBN   : std_logic_vector(31 downto 0)
                                                := (others => '0');
        SIGNAL ATBN_in         : std_logic_vector(31 downto 0)
                                                := (others => '0');
        --AutoBoot Enable Bit
        ALIAS ATBTEN       :std_logic IS ATBN(0);

        --      ***  Bank Address Register  ***
        SHARED VARIABLE Bank_Addr_reg  : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SIGNAL Bank_Addr_reg_in        : std_logic_vector(7 downto 0)
                                                := (others => '0');                                                
        --      ***  Pointer Address Registers  ***
        SHARED VARIABLE EFX0O  : std_logic_vector(15 downto 0)
                                                := "0000000000000011";
        SIGNAL EFX0O_in        : std_logic_vector(15 downto 0)
                                                := (others => '0');
        SHARED VARIABLE EFX1O  : std_logic_vector(15 downto 0)
                                                := "0000001111111111";
        SIGNAL EFX1O_in        : std_logic_vector(15 downto 0)
                                                := (others => '0');
        SHARED VARIABLE EFX2O  : std_logic_vector(15 downto 0)
                                                := "0000001111111111";
        SIGNAL EFX2O_in        : std_logic_vector(15 downto 0)
                                                := (others => '0');
        SHARED VARIABLE EFX3O  : std_logic_vector(15 downto 0)
                                                := "0000001111111111";
        SIGNAL EFX3O_in        : std_logic_vector(15 downto 0)
                                                := (others => '0');
        SHARED VARIABLE EFX4O  : std_logic_vector(15 downto 0)
                                                := "0000001111111111";
        SIGNAL EFX4O_in        : std_logic_vector(15 downto 0)
                                                := (others => '0');

        ALIAS GBLSEL      :std_logic IS EFX0O(1);
        ALIAS WRLVEN      :std_logic IS EFX0O(0);
        ALIAS ERGNT1      :std_logic IS EFX1O(1);
        ALIAS EPTEB1      :std_logic IS EFX1O(0);
        ALIAS ERGNT2      :std_logic IS EFX2O(1);
        ALIAS EPTEB2      :std_logic IS EFX2O(0);
        ALIAS ERGNT3      :std_logic IS EFX3O(1);
        ALIAS EPTEB3      :std_logic IS EFX3O(0);
        ALIAS ERGNT4      :std_logic IS EFX4O(1);
        ALIAS EPTEB4      :std_logic IS EFX4O(0);

        ALIAS F51M        :std_logic IS freq51;

        SHARED VARIABLE Negddr_F51M          :std_logic;
        SHARED VARIABLE Negddr_NegF51M       :std_logic;
        SHARED VARIABLE ddr_F51M             :std_logic;
        --      ***  Address Trap Register  ***
        SHARED VARIABLE EATV    : std_logic_vector(31 downto 0)
                                                := (others => '0');
        SIGNAL EATV_in          : std_logic_vector(31 downto 0)
                                                := (others => '0');
        SHARED VARIABLE DCRV        : std_logic_vector(31 downto 0)
                                                := (others => '0');
        SIGNAL DCRV_in              : std_logic_vector(31 downto 0)
                                                := (others => '0');
        TYPE SecArray IS ARRAY (0 TO SecNumHyb) OF   std_logic_vector(23 downto 0);
        --      ***  Sector Erase Count Register  ***
        SIGNAL SECV            : std_logic_vector(23 downto 0)
                                                := (others => '0');
        SHARED VARIABLE SECVIN          : std_logic_vector(31 downto 0)
                                                := (others => '0');
        SIGNAL SECV_in         : SecArray
                                                := (others => (others => '0'));
                                     
        SHARED VARIABLE MPASSREG    : std_logic_vector(SecNumHyb downto 0)
                                                := (others => '0');
                                                
        SHARED VARIABLE WRAR_reg_in    : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SHARED VARIABLE RDAR_reg       : std_logic_vector(7 downto 0)
                                                := (others => '0');

        SHARED VARIABLE ECSV        : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SHARED VARIABLE ECTV        : std_logic_vector(15 downto 0)
                                                := (others => '0');

        SHARED VARIABLE DIC_Start_Addr_reg: NATURAL RANGE 0 TO AddrRANGE := 0;
        SHARED VARIABLE DIC_End_Addr_reg  : NATURAL RANGE 0 TO AddrRANGE := 0;
        -- The Lock Protection Registers for OTP Memory space
        SHARED VARIABLE LOCK_BYTE1 :std_logic_vector(7 downto 0);
        SHARED VARIABLE LOCK_BYTE2 :std_logic_vector(7 downto 0);
        SHARED VARIABLE LOCK_BYTE3 :std_logic_vector(7 downto 0);
        SHARED VARIABLE LOCK_BYTE4 :std_logic_vector(7 downto 0);

        --Command Register
        SIGNAL write              : std_logic := '0';
        SIGNAL cfg_write          : std_logic := '0';
        SIGNAL cfg_write1          : std_logic := '0';
        SIGNAL cfg_write2          : std_logic := '0';
        SIGNAL cfg_write3          : std_logic := '0';
        SIGNAL cfg_write4          : std_logic := '0';
        SIGNAL read_out           : std_logic := '0';

        SIGNAL dual               : boolean   := false;
        SIGNAL rd_fast            : boolean   := true;
        SIGNAL rd_fast1           : boolean   := false;
        SIGNAL rd_slow            : boolean   := false;
        SIGNAL ddr                : boolean   := false;
        SIGNAL any_read           : boolean   := false;

        SIGNAL oe                 : boolean   := false;
        SIGNAL oe_z               : boolean   := false;

        -- Memory Array Configuration
        SIGNAL BottomBoot          : boolean  := BootConfig;
        SIGNAL TopBoot             : boolean  := NOT BootConfig;
        SIGNAL UniformSec          : boolean  := false;

        --FSM control signals
        SIGNAL PDONE              : std_logic := '1'; --Page Prog. Done
        SIGNAL PSTART             : std_logic := '0'; --Start Page Programming
        SIGNAL PGSUSP             : std_logic := '0'; --Suspend Program
        SIGNAL PGRES              : std_logic := '0'; --Resume Program
        
        SHARED VARIABLE WRONG_PASS        : NATURAL := 0; --Wrong password inserted
 
        
        SIGNAL RES_TO_SUS_TIME    : std_logic := '0';--Resume to Suspend Flag

        SIGNAL WDONE              : std_logic := '1'; --Write operation Done
        SIGNAL WSTART             : std_logic := '0'; --Start Write operation

        SIGNAL CSDONE             : std_logic := '1'; --Write volatile bits
        SIGNAL CSSTART            : std_logic := '0'; --Start Write volatile bits

        SIGNAL EESDONE            : std_logic := '1'; --Evaluate Erase Status Done
        SIGNAL EESSTART           : std_logic := '0'; --Start Evaluate Erase Status operation

        SIGNAL ESTART             : std_logic := '0'; --Start Erase operation
        SIGNAL EDONE              : std_logic := '1'; --Erase operation Done
        SIGNAL ESUSP              : std_logic := '0'; --Suspend Erase
        SIGNAL ERES               : std_logic := '0'; --Resume Erase

        SIGNAL DICSTART           : std_logic := '0'; --DIC calc Start
        SIGNAL DICDONE            : std_logic := '1'; --DIC calc Done
        SIGNAL DICSUSP            : std_logic := '0'; --DIC Suspend
        SIGNAL DICRES             : std_logic := '0'; --DIC Resume
        
        
        SIGNAL SRNC               : std_logic := '0';
        SIGNAL RSTRDAct           : std_logic := '0'; 

        SIGNAL BCDONE             : std_logic := '1';

        --reset timing
        SIGNAL RST                 : std_logic := '0';
        SIGNAL RST_HARD            : std_logic := '0';
        SIGNAL RST_SIG             : std_logic := '0';
        SIGNAL reseted             : std_logic := '0'; --Reset Timing Control
        SIGNAL RST_in              : std_logic := '0';
        SIGNAL RST_in_DPD          : std_logic := '0';
        SIGNAL RST_in_HARD         : std_logic := '0'; 
        SIGNAL RST_in_soft         : std_logic := '0';
        SIGNAL RST_out             : std_logic := '1';
        SIGNAL SWRST_in            : std_logic := '0';
        SIGNAL SWRST_out           : std_logic := '1';
        SIGNAL RESET_EN            : std_logic := '0';
        SIGNAL QUAD_QPI            : std_logic := '0';
        SIGNAL reset_act           : boolean;
        SIGNAL rst_quad            : boolean;
        SIGNAL double              : boolean;

        --Flag that mark if ASP Register is already programmed
        SIGNAL ASPOTPFLAG         : BOOLEAN   := FALSE;
        SIGNAL INITIAL_CONFIG     : std_logic := '0';

        SHARED VARIABLE SecAddr_ers   : NATURAL RANGE 0 TO SecNumHyb := 0;
        SHARED VARIABLE SecAddr_pgm   : NATURAL RANGE 0 TO SecNumHyb := 0;

        SHARED VARIABLE pgm_page  : NATURAL;

        SHARED VARIABLE ASP_ProtSE  : NATURAL   := 0;
        SHARED VARIABLE Sec_ProtSE  : NATURAL   := 0;

        --Flag for Password unlock command
        SIGNAL PASS_UNLOCKED      : boolean   := FALSE;
        SIGNAL PASS_TEMP          : std_logic_vector(63 downto 0)
                                                := (others => '1');
        SHARED VARIABLE EHP       : BOOLEAN := FALSE;

        SHARED VARIABLE read_cnt  : NATURAL := 0;
        SHARED VARIABLE byte_cnt  : NATURAL := 1;
        SHARED VARIABLE read_addr : NATURAL;

        SHARED VARIABLE start_delay : NATURAL RANGE 0 TO 7;
        SHARED VARIABLE ABSD        : NATURAL RANGE 0 TO 7;
        SIGNAL start_autoboot       : std_logic := '0';

        SIGNAL change_addr        : std_logic := '0';
        SIGNAL Address            : NATURAL;
        SIGNAL SectorSuspend      : NATURAL RANGE 0 TO SecNumHyb := 0;
        SIGNAL ERS_nosucc         : std_logic_vector(SecNumHyb downto 0)
                                                 := (OTHERS => '0');

        -- Sector is protect if Sec_Prot(SecNum) = '1'
        SHARED VARIABLE Sec_Prot  : std_logic_vector(SecNumHyb downto 0) :=
                                                   (OTHERS => '0');

        SIGNAL change_BP          : std_logic := '0';
        SHARED VARIABLE BP_bits   : std_logic_vector(2 downto 0) := "000";
        SIGNAL change_TB4KBS      : std_logic := '0';

        SIGNAL PageSize           : NATURAL RANGE 0 TO 512;
        
        SIGNAL Byte_number        : NATURAL RANGE 0 TO 839    := 0;

        SIGNAL Byte_numERCHP_0_0r : NATURAL RANGE 0 TO 511    := 0;
        
        TYPE bus_cycle_type IS (STAND_BY,
                                OPCODE_BYTE,
                                ADDRESS_BYTES,
                                DUMMY_BYTES,
                                MODE_BYTE,
                                DATA_BYTES
                                );
        TYPE bus_cycle_type_reset IS (SIGRES_IDLE,
                                SIGRES_FIRST_FE,
                                SIGRES_FIRST_RE,
                                SIGRES_SECOND_FE,
                                SIGRES_SECOND_RE,
                                SIGRES_THIRD_FE,
                                SIGRES_THIRD_RE,
                                SIGRES_NOT_A_RESET
                                );
                                
        SHARED VARIABLE bus_cycle_state          : bus_cycle_type;
        SHARED VARIABLE bus_cycle_state_reset    : bus_cycle_type_reset;
        -- switch between Data bytes and Dummy bytes
        SHARED VARIABLE DummyBytes_act     : X01 := '0';
        SIGNAL dummy_cnt_act_temp          : NATURAL := 0;
        SIGNAL dummy_cnt_act               : NATURAL := 0;
        --Read Password Protection Mode Active flag
        SIGNAL RdPswdProtMode              : std_ulogic := '0';
        --Read Password Protection Mode Support flag
        SIGNAL RdPswdProtEnable            : std_ulogic := '0';

        SHARED VARIABLE Latency_code       : NATURAL;
        SHARED VARIABLE Register_Latency   : NATURAL;
        SHARED VARIABLE WrapLength         : NATURAL RANGE 0 TO 64;
        SHARED VARIABLE opcode_cnt         : NATURAL := 0;
        SHARED VARIABLE addr_cnt           : NATURAL := 0;
        SHARED VARIABLE mode_cnt           : NATURAL := 0;
        SHARED VARIABLE dummy_cnt          : NATURAL := 0;
        SHARED VARIABLE data_cnt           : NATURAL := 0;
        SHARED VARIABLE ZERO_DETECTED      : std_logic;

        SHARED VARIABLE DIC_ACT            : std_logic := '0';--DIC Active
        SHARED VARIABLE DIC_RD_SETUP       : std_logic := '0';--DIC read setup
        SHARED VARIABLE dic_in             : std_logic_vector(15 downto 0);
        SHARED VARIABLE dic_out            : std_logic_vector(31 downto 0);
        SHARED VARIABLE dic_tmp            : std_logic;

        -- Flag for Blank Check
        SIGNAL NOT_BLANK                   : std_ulogic := '0';
        SIGNAL bc_done                     : std_ulogic := '0';

        SIGNAL RES_TO_SUSP_TIME            : std_ulogic := '0';
        SIGNAL res_time                    : time;

        -- timing check violation
        SIGNAL Viol               : X01 := '0';

        FUNCTION ReturnSectorID(ADDR       : NATURAL;
                                BottomBoot : BOOLEAN;
                                TopBoot    : BOOLEAN) RETURN NATURAL IS
            VARIABLE result : NATURAL;
            VARIABLE conv   : NATURAL;
        BEGIN
            conv := ADDR / (SecSize256+1);
            IF BottomBoot AND TopBoot = false THEN
                IF conv=0 AND ADDR<(32*(SecSize4+1)) THEN
                    result := ADDR/(SecSize4+1);
                ELSIF conv=0 AND ADDR>=(32*(SecSize4+1)) THEN
                    result := 32;
                ELSE
                    result := conv + 32;
                END IF;
            ELSIF TopBoot AND BottomBoot = false THEN
                IF conv=127 AND ADDR<(AddrRANGE+1 - 32*(SecSize4+1)) THEN
                    result := 127;
                ELSIF conv=127 AND ADDR>(AddrRANGE - 32*(SecSize4+1)) THEN
                    result := 128 + (ADDR -
                    (AddrRANGE+1 - 32*(SecSize4+1)))/(SecSize4+1);
                ELSE
                    result := conv;
                END IF;
            ELSIF TopBoot AND BottomBoot THEN
                IF conv=0 AND ADDR<(16*(SecSize4+1)) THEN
                    result := ADDR/(SecSize4+1);
                ELSIF conv=0 AND ADDR>=(16*(SecSize4+1)) THEN
                    result := 17;
                ELSIF conv=127 AND ADDR<(AddrRANGE+1 - 16*(SecSize4+1)) THEN
                    result := 143;
                ELSIF conv=127 AND ADDR>(AddrRANGE - 16*(SecSize4+1)) THEN
                    result := 144 + (ADDR -
                    (AddrRANGE+1 - 16*(SecSize4+1)))/(SecSize4+1);
                ELSIF conv>0 AND conv<127 THEN 
                    result := conv + 17;
                END IF;
            ELSE
                result := conv;
            END IF;
            RETURN result;
        END ReturnSectorID;

        FUNCTION ReturnAddrInSect (ADDR       : NATURAL;
                                   BottomBoot : BOOLEAN;
                                   TopBoot    : BOOLEAN) RETURN NATURAL IS
            VARIABLE result : NATURAL;
            VARIABLE conv   : NATURAL;
        BEGIN
            conv := ADDR / (SecSize256+1);
            IF BottomBoot AND TopBoot = false THEN
                IF conv=0 AND ADDR<(32*(SecSize4+1)) THEN
                    result := ADDR mod (SecSize4+1);
                ELSIF conv=0 AND ADDR>=(32*(SecSize4+1)) THEN
                    result := ADDR - 32*(SecSize4+1);
                ELSE
                    result := ADDR mod (SecSize256+1);
                END IF;
            ELSIF TopBoot AND BottomBoot = false THEN
                IF conv=127 AND ADDR<(AddrRANGE+1 - 32*(SecSize4+1)) THEN
                    result := ADDR - 127*(SecSize256+1);
                ELSIF conv=127 AND ADDR>(AddrRANGE - 32*(SecSize4+1)) THEN
                    result := ADDR mod (SecSize4+1);
                ELSE
                    result := ADDR mod (SecSize256+1);
                END IF;
            ELSIF TopBoot AND BottomBoot THEN
                IF conv=0 AND ADDR<(16*(SecSize4+1)) THEN
                    result := ADDR mod (SecSize4+1);
                ELSIF conv=0 AND ADDR>=(16*(SecSize4+1)) THEN
                    result := ADDR - 16*(SecSize4+1);
                ELSIF conv=143 AND ADDR<(AddrRANGE+1 - 16*(SecSize4+1)) THEN
                    result := ADDR - 143*(SecSize256+1);
                ELSIF conv=143 AND ADDR>(AddrRANGE - 16*(SecSize4+1)) THEN
                    result := ADDR mod (SecSize4+1);
                ELSE
                    result := ADDR mod (SecSize256+1);
                END IF;
            ELSE
                result := ADDR mod (SecSize256+1);
            END IF;
            RETURN result;
        END ReturnAddrInSect;

    BEGIN
    ---------------------------------------------------------------------------
    --Power Up time
    ---------------------------------------------------------------------------

    PoweredUp <= '1' AFTER tdevice_PU;
    QUAD_QPI  <=  QUADIT OR QPI_IT;
    reset_act <= CFR2V(5)='1' AND (QUAD_QPI='0' OR (QUAD_QPI='1' AND CSNeg='1'));
    rst_quad  <= TRUE WHEN (CFR2V(5) = '1') AND (QUAD_QPI = '1')  ELSE  FALSE;--(QUADIT OR QPI_IT)

    res_time <= tdevice_RS WHEN LongTimming  ELSE tdevice_RS/10;

    PageSize <= 127 WHEN CFR3V(4)='0' ELSE 511;
    
    timing30: PROCESS(IOIMPD2,IOIMPD1, IOIMPD0)
    BEGIN
            IF (IOIMPD2 = '1' AND IOIMPD1 = '0' AND IOIMPD0 = '1' ) THEN
                timing_30 <= '1';
            ELSE 
                timing_30 <= '0';
            END IF;
            
        
    END PROCESS timing30;
    
    TimingModelSel: PROCESS
    BEGIN
        IF TimingModel(14)='v' OR TimingModel(14)='B' OR
           TimingModel(14)='A' OR TimingModel(14)='M' THEN
            non_industrial_temp <= '0';
        ELSIF TimingModel(15)='I' THEN
            non_industrial_temp <= '1';
        END IF;
        WAIT;
    END PROCESS;


    ReadPasswordProtectionMode: PROCESS(PPBLCK_temp,
    ASPO_in(2), ASPO_in(5))
    BEGIN
        IF (PPBLCK = '0' AND ASPPWD = '0' AND ASPRDP = '0' AND
            RdPswdProtEnable = '1') THEN
            RdPswdProtMode <= '1';
            ATBTEN := '0';
        ELSE
            RdPswdProtMode <= '0';
        END IF;
    END PROCESS;
    ---------------------------------------------------------------------------
    -- autoboot control logic
    ---------------------------------------------------------------------------
    AutoBootControl: PROCESS(SCK_ipd, current_state)
    BEGIN
        IF (current_state = AUTOBOOT) THEN
            IF rising_edge(SCK_ipd) THEN
                IF (start_delay > 0) THEN
                    start_delay := start_delay - 1;
                END IF;
            END IF;

            IF (start_delay = 0) THEN
                start_autoboot <= '1';
            ELSE
                start_autoboot <= '0';
            END IF;
        END IF;
    END PROCESS;
    ---------------------------------------------------------------------------
    -- Frequency check
    ---------------------------------------------------------------------------
    MHZFREQ: PROCESS(ddr, F51M)
    BEGIN
        IF ddr = false THEN
           Negddr_F51M  := F51M;
           Negddr_NegF51M  := NOT F51M ;
--           ddr_F51M  := '0' ;
        ELSE
           ddr_F51M  := F51M ;
           Negddr_NegF51M  := NOT F51M ;
        END IF;
    END PROCESS;

    CK_F :PROCESS(SCK_ipd, CSNeg_ipd)
        VARIABLE   LAST_CK_1         : time := 0 ns;
    BEGIN
        CK_PERIOD  := NOW - LAST_CK_1;
        LAST_CK_1  := NOW;
        IF CSNeg_ipd = '1' THEN
            counter_clock := (others => '0');
        ELSIF counter_clock(2 downto 0) < "111" THEN
            counter_clock := to_slv(to_nat(counter_clock) + 1);
        ELSE
            counter_clock := (others => '1');
        END IF;
        IF ((CK_PERIOD < 20.000 ns) OR (counter_clock(2 downto 0) < "010")) THEN
            freq51 <= '1';
        ELSE
            freq51 <= '0';
        END IF;
    END PROCESS CK_F;
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

        VARIABLE Tviol_CSNeg_IO3RESETNeg    : X01 := '0';
        VARIABLE TD_CSNeg_IO3RESETNeg       : VitalTimingDataType;


        --Pulse Width and Period Check Variables
        VARIABLE Pviol_SCK_rd     : X01 := '0';
        VARIABLE PD_SCK_rd        : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_SCK_fast   : X01 := '0';
        VARIABLE PD_SCK_fast      : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_SCK_fast1  : X01 := '0';
        VARIABLE PD_SCK_fast1     : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_SCK_ddr    : X01 := '0';
        VARIABLE PD_SCK_ddr       : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_CSNeg      : X01 := '0';
        VARIABLE PD_CSNeg         : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_CSNeg_rst_quad : X01 := '0';
        VARIABLE PD_CSNeg_rst_quad    : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_CSNeg_RDYBSY    : X01 := '0';
        VARIABLE PD_CSNeg_RDYBSY       : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_RESETNeg     : X01 := '0';
        VARIABLE PD_RESETNeg        : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_IO3RESETNeg  : X01 := '0';
        VARIABLE PD_IO3RESETNeg     : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Violation          : X01 := '0';

    BEGIN
    ---------------------------------------------------------------------------
    -- Timing Check Section
    ---------------------------------------------------------------------------
        IF (TimingChecksOn) THEN

        -- Setup/Hold Check between CS# and SCK
        VitalSetupHoldCheck (
            TestSignal      => CSNeg_ipd,
            TestSignalName  => "CS#",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_CSNeg_SCK,
            SetupLow        => tsetup_CSNeg_SCK,
            HoldHigh        => thold_CSNeg_SCK,
            HoldLow         => thold_CSNeg_SCK,
            CheckEnabled    => true,
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
            CheckEnabled    => CFR2V(5)='1' AND QUADIT='0',
            RefTransition   => '\',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_CSNeg_RESETNeg,
            Violation       => Tviol_CSNeg_RESETNeg
        );

        -- Hold Check between CSNeg and IO3RESETNeg
        VitalSetupHoldCheck (
            TestSignal      => CSNeg,
            TestSignalName  => "CSNeg",
            RefSignal       => IO3RESETNeg,
            RefSignalName   => "IO3RESETNeg",
            HoldHigh        => thold_CSNeg_IO3RESETNeg,
            CheckEnabled    => CFR2V(5) = '1',
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_CSNeg_IO3RESETNeg,
            Violation       => Tviol_CSNeg_IO3RESETNeg
        );

        -- Setup/Hold Check between SI and SCK, SDR mode
        VitalSetupHoldCheck (
            TestSignal      => SIIn,
            TestSignalName  => "SI",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK,
            SetupLow        => tsetup_SI_SCK,
            HoldHigh        => thold_SI_SCK,
            HoldLow         => thold_SI_SCK,
            CheckEnabled    => PoweredUp='1' AND SIOut_zd /= SIIn,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_SI_SCK,
            Violation       => Tviol_SI_SCK
        );
        
--         -- Setup/Hold Check between SI and SCK, SDR mode ??? ubaci ako treba xD
--         VitalSetupHoldCheck (
--             TestSignal      => SOIn,
--             TestSignalName  => "SO",
--             RefSignal       => SCK_ipd,
--             RefSignalName   => "SCK",
--             SetupHigh       => tsetup_SI_SCK,
--             SetupLow        => tsetup_SI_SCK,
--             HoldHigh        => thold_SI_SCK,
--             HoldLow         => thold_SI_SCK,
--             CheckEnabled    => PoweredUp='1' AND SIOut_zd /= SOIn,
--             RefTransition   => '/',
--             HeaderMsg       => InstancePath & PartID,
--             TimingData      => TD_SI_SCK,
--             Violation       => Tviol_SI_SCK
--         );

        -- Setup/Hold Check between SI and SCK, DDR mode
        VitalSetupHoldCheck (
            TestSignal      => SIIn,
            TestSignalName  => "SI",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK_double_noedge_posedge,
            SetupLow        => tsetup_SI_SCK_double_noedge_posedge,
            HoldHigh        => thold_SI_SCK_double_noedge_posedge,
            HoldLow         => thold_SI_SCK_double_noedge_posedge,
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
            SetupHigh       => tsetup_SI_SCK_double_noedge_posedge,
            SetupLow        => tsetup_SI_SCK_double_noedge_posedge,
            HoldHigh        => thold_SI_SCK_double_noedge_posedge,
            HoldLow         => thold_SI_SCK_double_noedge_posedge,
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
            SetupHigh       => tsetup_SI_SCK_Negddr_NegF51M_NOEDGE_POSEDGE,
            SetupLow        => tsetup_SI_SCK_Negddr_NegF51M_NOEDGE_POSEDGE,
            HoldHigh        => thold_SI_SCK_Negddr_NegF51M_NOEDGE_POSEDGE,
            HoldLow         => thold_SI_SCK_Negddr_NegF51M_NOEDGE_POSEDGE,
            CheckEnabled    => PoweredUp='1' AND SOut_zd /= SOIn AND  Negddr_NegF51M = '1',
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_SO_SCK,
            Violation       => Tviol_SO_SCK
        );
        
        -- Setup/Hold Check between SO and SCK, SDR mode --ADDED
--         VitalSetupHoldCheck (
--             TestSignal      => SOIn,
--             TestSignalName  => "SO",
--             RefSignal       => SCK_ipd,
--             RefSignalName   => "SCK",
--             SetupHigh       => tsetup_SI_SCK_Negddr_NegF51M_NOEDGE_POSEDGE,
--             SetupLow        => tsetup_SI_SCK_Negddr_NegF51M_NOEDGE_POSEDGE,
--             HoldHigh        => thold_SI_SCK_Negddr_NegF51M_NOEDGE_POSEDGE,
--             HoldLow         => thold_SI_SCK_Negddr_NegF51M_NOEDGE_POSEDGE,
--             CheckEnabled    => PoweredUp='1' AND SOut_zd /= SOIn AND CFR4N(7 DOWNTO 5) = "101",
--             RefTransition   => '/',
--             HeaderMsg       => InstancePath & PartID,
--             TimingData      => TD_SO_SCK,
--             Violation       => Tviol_SO_SCK
--         );
        
        -- Setup/Hold Check between SO and SCK, SDR mode
        VitalSetupHoldCheck (
            TestSignal      => SOIn,
            TestSignalName  => "SO",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK_Negddr_F51M_NOEDGE_POSEDGE,
            SetupLow        => tsetup_SI_SCK_Negddr_F51M_NOEDGE_POSEDGE,
            HoldHigh        => thold_SI_SCK_Negddr_F51M_NOEDGE_POSEDGE,
            HoldLow         => thold_SI_SCK_Negddr_F51M_NOEDGE_POSEDGE,
            CheckEnabled    => PoweredUp='1' AND SOut_zd /= SOIn AND Negddr_F51M = '1',
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_SO_SCK,
            Violation       => Tviol_SO_SCK
        );
        
        -- Setup/Hold Check between SO and SCK, SDR mode
        VitalSetupHoldCheck (
            TestSignal      => SOIn,
            TestSignalName  => "SO",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            HoldHigh        => thold_SI_SCK_ddr_F51M_NOEDGE_POSEDGE,
            HoldLow         => thold_SI_SCK_ddr_F51M_NOEDGE_POSEDGE,
            CheckEnabled    => PoweredUp='1' AND SOut_zd /= SOIn AND ddr_F51M = '1',
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_SO_SCK,
            Violation       => Tviol_SO_SCK
        );
        
        -- Setup/Hold Check between SO and SCK, DDR mode --ADDED
--         VitalSetupHoldCheck (
--             TestSignal      => SOIn,
--             TestSignalName  => "SO",
--             RefSignal       => SCK_ipd,
--             RefSignalName   => "SCK",
--             SetupHigh       => tsetup_SI_SCK_double_noedge_posedge,
--             SetupLow        => tsetup_SI_SCK_double_noedge_posedge,
--             HoldHigh        => thold_SI_SCK_double_noedge_posedge,
--             HoldLow         => thold_SI_SCK_double_noedge_negedge,
--             CheckEnabled    => PoweredUp='1' AND double AND 
--             SOut_zd /= SOIn AND CFR4N(7 DOWNTO 5) = "101",
--             RefTransition   => '/',
--             HeaderMsg       => InstancePath & PartID,
--             TimingData      => TD_SO_SCK_ddr_R,
--             Violation       => Tviol_SO_SCK_ddr_R
--         );
        
        -- Setup/Hold Check between SO and SCK, DDR mode
        VitalSetupHoldCheck (
            TestSignal      => SOIn,
            TestSignalName  => "SO",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK_double_noedge_posedge,
            SetupLow        => tsetup_SI_SCK_double_noedge_posedge,
            HoldHigh        => thold_SI_SCK_double_noedge_posedge,
            HoldLow         => thold_SI_SCK_double_noedge_posedge,
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
            SetupHigh       => tsetup_SI_SCK_double_noedge_posedge,
            SetupLow        => tsetup_SI_SCK_double_noedge_posedge,
            HoldHigh        => thold_SI_SCK_double_noedge_posedge,
            HoldLow         => thold_SI_SCK_double_noedge_posedge,
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
            SetupHigh       => tsetup_SI_SCK,
            SetupLow        => tsetup_SI_SCK,
            HoldHigh        => thold_SI_SCK,
            HoldLow         => thold_SI_SCK,
            CheckEnabled    => PoweredUp='1' AND
            WPNegOut_zd /= WPNegIn  AND QUADIT='1',
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
            SetupHigh       => tsetup_SI_SCK_double_noedge_posedge,
            SetupLow        => tsetup_SI_SCK_double_noedge_posedge,
            HoldHigh        => thold_SI_SCK_double_noedge_posedge,
            HoldLow         => thold_SI_SCK_double_noedge_posedge,
            CheckEnabled    => PoweredUp='1' AND double AND
            WPNegOut_zd /= WPNegIn  AND QUADIT='1',
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
            SetupHigh       => tsetup_SI_SCK_double_noedge_posedge,
            SetupLow        => tsetup_SI_SCK_double_noedge_posedge,
            HoldHigh        => thold_SI_SCK_double_noedge_posedge,
            HoldLow         => thold_SI_SCK_double_noedge_posedge,
            CheckEnabled    => PoweredUp='1' AND double AND
            WPNegOut_zd /= WPNegIn AND QUADIT='1',
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
            SetupHigh       => tsetup_SI_SCK,
            SetupLow        => tsetup_SI_SCK,
            HoldHigh        => thold_SI_SCK,
            HoldLow         => thold_SI_SCK,
            CheckEnabled    => PoweredUp='1' AND
            RESETNegOut_zd /= RESETNegIn AND QUADIT='1' AND CSNeg='0',
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
            SetupHigh       => tsetup_SI_SCK_double_noedge_posedge,
            SetupLow        => tsetup_SI_SCK_double_noedge_posedge,
            HoldHigh        => thold_SI_SCK_double_noedge_posedge,
            HoldLow         => thold_SI_SCK_double_noedge_posedge,
            CheckEnabled    => PoweredUp='1' AND double AND
            RESETNegOut_zd /= RESETNegIn AND QUADIT='1' AND CSNeg='0',
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
            SetupHigh       => tsetup_SI_SCK_double_noedge_posedge,
            SetupLow        => tsetup_SI_SCK_double_noedge_posedge,
            HoldHigh        => thold_SI_SCK_double_noedge_posedge,
            HoldLow         => thold_SI_SCK_double_noedge_posedge,
            CheckEnabled    => PoweredUp='1' AND double AND
            RESETNegOut_zd /= RESETNegIn AND QUADIT='1' AND CSNeg='0',
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
            CheckEnabled    => STCFWR='1' AND QUAD_QPI='0',
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
            CheckEnabled    => STCFWR = '1' AND QUAD_QPI = '0',
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
            CheckEnabled    => CFR2V(5)='1' AND QUADIT='0',
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

        -- Pulse Width Check SCK for RDAY2_C_0, serial mode
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            PulseWidthLow   =>  tpw_SCK_fast_rd,
            PulseWidthHigh  =>  tpw_SCK_fast_rd,
            PeriodData      =>  PD_SCK_fast,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK_fast,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  rd_fast);

        -- Pulse Width Check SCK for RDAY2_C_01, serial mode
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            PulseWidthLow   =>  tpw_SCK_fast_rd1,
            PulseWidthHigh  =>  tpw_SCK_fast_rd1,
            PeriodData      =>  PD_SCK_fast1,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK_fast1,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  rd_fast1);

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
            PulseWidthHigh  =>  tpw_CSNeg_RDYBSY_posedge,
            PeriodData      =>  PD_CSNeg_RDYBSY,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_CSNeg_RDYBSY,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  RDYBSY = '1');

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

        -- Pulse Width Check IO3RESETNeg
        VitalPeriodPulseCheck (
            TestSignal        => IO3RESETNeg_ipd,
            TestSignalName    => "IO3RESETNeg",
            PulseWidthLow     => tpw_IO3RESETNeg_negedge,
            PulseWidthHigh    => tpw_IO3RESETNeg_posedge,
            CheckEnabled      => rst_quad,
            HeaderMsg         => InstancePath & PartID,
            PeriodData        => PD_IO3RESETNeg,
            Violation         => Pviol_IO3RESETNeg);

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

        -- Period Check SCK for FAST READ, serial mode
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            Period          =>  tperiod_SCK_fast_rd,
            PeriodData      =>  PD_SCK_fast,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK_fast,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  rd_fast);

        -- Period Check SCK for FAST READ 1, serial mode
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            Period          =>  tperiod_SCK_fast_rd1,
            PeriodData      =>  PD_SCK_fast1,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK_fast1,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  rd_fast1);

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
                       Tviol_CSNeg_IO3RESETNeg OR
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
                       Pviol_SCK_fast OR
                       Pviol_SCK_fast1 OR
                       Pviol_SCK_ddr OR
                       Pviol_CSNeg OR
                       Pviol_CSNeg_rst_quad OR
                       Pviol_CSNeg_RDYBSY OR
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

   RST_HARD <= RESETNeg AFTER 199 ns;
   RST <= RST_SIG OR RST_HARD;
   RST_in <= RST_in_DPD OR RST_in_HARD OR RST_in_soft;

    StateTransition1: PROCESS(next_state, PoweredUp, RST, RST_out, SWRST_out, write, RST_in)
    BEGIN
        IF PoweredUp = '1' THEN
            IF ((IO3RESETNegIn='0' AND reset_act) OR 
            RESETNegIn='0' OR falling_edge(RST)) THEN
            -- no state transition while RESET# low
                current_state <= RESET_STATE;
                RST_in_HARD <= '1', '0' AFTER 1 ns;
                reseted <= '0';
            ELSIF RST_out = '1' AND SWRST_out = '1' THEN
                current_state <= next_state;
                reseted <= '1';
            END IF;

            IF falling_edge(write) THEN
                IF (Instruct = SFRST_0_0 AND RESET_EN = '1') OR Instruct = SFRSL_0_0 THEN
                -- no state transition while RESET is in progress
                    current_state <= RESET_STATE;
                    SWRST_in <= '1', '0' AFTER 1 ns;
                    reseted <= '0';
                    IF ( CFR3V = CFR3N ) THEN
                         SRNC <= '1';
                    ELSE
                         SRNC <= '0';
                    END IF;
                END IF;
             END IF;
         END IF;
        IF falling_edge(RST_in) THEN
            reseted <= '0';
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
                IF (Instruct = SFRST_0_0 AND RESET_EN = '1') OR Instruct = SFRSL_0_0 THEN
                -- no state transition while RESET is in progress
                current_sigres_state  <= SIGRES_IDLE;
                END IF;
            END IF;
        END IF;
    END PROCESS;
    
--     WRONG_PASS <= WRONG_PASS_RST OR WRONG_PASS_UNLOCK;
    -- Timing control for the Hardware Reset
    Threset1: PROCESS(RST_in)
    BEGIN
        IF rising_edge(RST_in) THEN
            WRONG_PASS := 0;
            RST_out <= '0', '1' AFTER (tdevice_RPH - 200 ns);
        END IF;
    END PROCESS;
    -- Timing control for the Software Reset
    Threset2: PROCESS(SWRST_in)
    BEGIN
        IF rising_edge(SWRST_in) THEN
            SWRST_out <= '0', '1' AFTER (tdevice_RPH);
        END IF;
    END PROCESS;
    
    
    ------------------------- DPD control----------------------------------
-----------------------------------------------------------------------
    DPD_CSNeg_Time: PROCESS(CSNeg_ipd, DPDEnt_out, RST)
        VARIABLE DPD_CSNEG_RISING            : time := 0 us;
        VARIABLE DPD_CSNEG_FALLING           : time := 5 us;
    BEGIN
        IF falling_edge(CSNeg_ipd) AND DPDEnt_out = '1' THEN
           DPD_CSNEG_FALLING := NOW;
        ELSIF rising_edge(CSNeg_ipd) AND DPDEnt_out = '1' THEN
           DPD_CSNEG_RISING  := NOW;
        ELSIF DPDEnt_out = '1'THEN
                   DPD_CSNEG_FALLING := 5 us;
                   DPD_CSNEG_RISING  := 0 us;
        END IF;
        IF  (((((DPD_CSNEG_FALLING - DPD_CSNEG_RISING) < tdevice_DPD) AND 
                 CSNeg = '1') OR falling_edge(RST)) AND DPDEnt_out = '1') THEN
            DPDExt_in <= '1';
        ELSE
            DPDExt_in <= '0';
        END IF;
    END PROCESS;

    DPDExtEvent: PROCESS(DPDExt_in, DPD_in)
    BEGIN
          IF rising_edge(DPDExt_in) THEN
              DPDExt_out <= '0', '1' AFTER (tdevice_EXTDPD);
          ELSIF rising_edge(DPD_in) THEN
              DPDExt_out <= '0';
          END IF;
    END PROCESS;

--     DPDEntEvent: PROCESS(DPD_in, DPDExt_out)
--     BEGIN
--               IF rising_edge(DPD_in) THEN
--                   DPDEnt_out <= '0', '1' AFTER (tdevice_DPD);
--               END IF;
--               IF rising_edge(DPDExt_out) THEN
--                   DPDEnt_out <= '1', '0' AFTER 1 ns;
--               END IF;
--     END PROCESS;
------------------------ DPD control end----------------------------------
--------------------------------------------------------------------------

    -- 4kB Erase/Uniform sec architecture
   SecArch: PROCESS(TB4KBS_NV, PoweredUp, CFR3V(3), CFR1V(6))
    BEGIN
        IF CFR3V(3) = '0' THEN
          IF CFR1V(6) = '0' THEN
            IF TB4KBS_NV = '0' THEN
                BottomBoot <= true;
                TopBoot    <= false;
                UniformSec <= false;
            ELSE
                BottomBoot <= false;
                TopBoot    <= true;
                UniformSec <= false;
            END IF;
          ELSE
              UniformSec <= true;
              BottomBoot <= true;
              TopBoot    <= false;
          END IF;
        ELSE
            UniformSec <= true;
            BottomBoot <= false;
            TopBoot    <= false;
        END IF;
    END PROCESS;

   ---------------------------------------------------------------------------
    --  Write cycle decode
    ---------------------------------------------------------------------------

    BusCycleDecode : PROCESS(SCK_ipd, CSNeg_ipd)

        TYPE quad_data_type IS ARRAY (0 TO 1679) OF INTEGER RANGE 0 TO 15;

        VARIABLE bit_cnt            : NATURAL := 0;
        VARIABLE Data_in            : std_logic_vector(6719 downto 0)
                                                    := (others => '1');

        VARIABLE opcode             : std_logic_vector(7 downto 0);
        VARIABLE opcode_in          : std_logic_vector(7 downto 0);
        VARIABLE opcode_tmp         : std_logic_vector(7 downto 0);
        VARIABLE addr_bytes         : std_logic_vector(31 downto 0);
        VARIABLE hiaddr_bytes       : std_logic_vector(31 downto 0);
        VARIABLE Address_in         : std_logic_vector(31 downto 0);
        VARIABLE mode_bytes         : std_logic_vector(7 downto 0);
        VARIABLE mode_in            : std_logic_vector(7 downto 0);
        VARIABLE quad_data_in       : quad_data_type;
        VARIABLE quad_nybble        : std_logic_vector(3 downto 0) := "0000";
        VARIABLE Quad_slv           : std_logic_vector(3 downto 0);
        VARIABLE Byte_slv           : std_logic_vector(7 downto 0) := "00000000";
        VARIABLE Quad_int           : INTEGER;

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
                        ZERO_DETECTED := '0';
                        IF current_state = AUTOBOOT THEN
                            bus_cycle_state := DATA_BYTES;
                        ELSE
                            bus_cycle_state := OPCODE_BYTE;
                        END IF;
                    END IF;

                WHEN OPCODE_BYTE =>
                    IF rising_edge(SCK_ipd) THEN

                        CLK_PER  := NOW - LAST_CLK;
                        LAST_CLK := NOW;
                        IF Check_freq THEN
                            IF Instruct=RDAY2_C_0 OR Instruct=PKRD1_4_0 OR Instruct=PKRD2_4_0 OR
                               Instruct = RDHL0_0_0 OR Instruct = RDHL1_0_0 OR
                            Instruct=PKRD3_4_0 OR Instruct=PKRD4_4_0 OR Instruct=RDSSR_C_0 OR
                            (Instruct=RDARG_C_0 AND (Address < 16#00800000#)) OR
                            ((Instruct=RDECC_C_0 OR Instruct=RDECC_4_0 OR 
                              Instruct=RDAY4_4_0 OR Instruct=RDAY4_C_0 OR
                              Instruct=RDPPB_C_0 OR Instruct=RDPPB_4_0) AND
                               QPI_IT = '0') THEN
                                IF (CLK_PER<20 ns AND Latency_code=0) OR --50MHz
                                (CLK_PER<14.70 ns AND Latency_code=1) OR--68MHz
                                (CLK_PER<12.34 ns AND Latency_code=2) OR--81MHz
                                (CLK_PER<10.75 ns AND Latency_code=3) OR--93MHz
                                (CLK_PER<9.43 ns AND Latency_code=4) OR--106MHz
                                (CLK_PER<8.47 ns AND Latency_code=5) OR--118MHz
                                (CLK_PER<7.63 ns AND Latency_code=6) OR--131MHz
                                (CLK_PER<6.99 ns AND Latency_code=7) OR--143MHz
                                (CLK_PER<6.41 ns AND Latency_code=8) OR--156MHz
                                (CLK_PER<6.02 ns AND Latency_code>=9) --166MHz
                                THEN
                                    ASSERT FALSE
                                    REPORT "More wait states are required for " &
                                        "this clock frequency value"
                                    SEVERITY warning;
                                END IF;
                                Check_freq := FALSE;
                            END IF;
                            IF Instruct=RDAY2_4_0 AND QPI_IT = '0' THEN
                                IF (CLK_PER<6.41 ns AND Latency_code=0) OR--156MHz
                                (CLK_PER<6.02 ns AND Latency_code>=1) --166MHz
                                THEN
                                    ASSERT FALSE
                                    REPORT "More wait states are required for " &
                                        "this clock frequency value"
                                    SEVERITY warning;
                                END IF;
                                Check_freq := FALSE;
                            END IF;

                            IF Instruct=RDAY3_C_0 OR Instruct=RDAY3_4_0 THEN
                                IF (CLK_PER<12.34 ns AND Latency_code=0) OR--81MHz
                                (CLK_PER<10.75 ns AND Latency_code=1) OR--93MHz
                                (CLK_PER<9.43 ns AND Latency_code=2) OR--106MHz
                                (CLK_PER<8.47 ns AND Latency_code=3) OR--118MHz
                                (CLK_PER<7.63 ns AND Latency_code=4) OR--131MHz
                                (CLK_PER<6.99 ns AND Latency_code=5) OR--143MHz
                                (CLK_PER<6.41 ns AND Latency_code=6) OR--156MHz
                                (CLK_PER<6.02 ns AND Latency_code>=7) --166MHz
                                THEN
                                    ASSERT FALSE
                                    REPORT "More wait states are required for " &
                                        "this clock frequency value"
                                    SEVERITY warning;
                                END IF;
                                Check_freq := FALSE;
                            END IF;

                            IF Instruct=RDAY2_4_0 OR Instruct=RDAY5_4_0 OR 
                                ((Instruct=RDPPB_C_0 OR Instruct=RDPPB_4_0 
                                 OR Instruct=RDAY5_C_0) AND QPI_IT='1')  THEN
                                IF (CLK_PER<23.22 ns AND Latency_code=0) OR--43MHz
                                (CLK_PER<17.85 ns AND Latency_code=1) OR--56MHz
                                (CLK_PER<14.70 ns AND Latency_code=2) OR--68MHz
                                (CLK_PER<12.34 ns AND Latency_code=3) OR--81MHz
                                (CLK_PER<10.75 ns AND Latency_code=4) OR--93MHz
                                (CLK_PER<9.43 ns AND Latency_code=5) OR--106MHz
                                (CLK_PER<8.47 ns AND Latency_code=6) OR--118MHz
                                (CLK_PER<7.63 ns AND Latency_code=7) OR--131MHz
                                (CLK_PER<6.99 ns AND Latency_code=8) OR--143MHz
                                (CLK_PER<6.41 ns AND Latency_code=9) OR--156MHz
                                (CLK_PER<6.02 ns AND Latency_code>=10) --166MHz
                                THEN
                                    ASSERT FALSE
                                    REPORT "More wait states are required for " &
                                        "this clock frequency value"
                                    SEVERITY warning;
                                END IF;
                                Check_freq := FALSE;
                            END IF;

                                        
                            IF (Instruct = RDDYB_C_0 OR Instruct = RDDYB_4_0 OR
                                 Instruct = RDSR1_0_0 OR Instruct = RDIDN_0_0 OR 
                                  Instruct = RDQID_0_0 OR
                                 Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR
                                  Instruct = RDDLP_0_0 OR Instruct = RDPLB_0_0) THEN
    
                                IF ((CLK_PER < 20.000 ns AND Register_Latency = 0) OR -- <=50MHz
                                   (CLK_PER <  7.510 ns AND Register_Latency = 1) OR -- <=133MHz
                                   (CLK_PER <  7.510 ns AND Register_Latency = 2) OR -- <=133MHz
                                   (CLK_PER <  6.020 ns AND Register_Latency = 3)) -- <=166MHz
                                 THEN
                                    ASSERT FALSE
                                    REPORT "More wait states are required for " &
                                        "this clock frequency value"
                                    SEVERITY warning;
                                END IF;
                                Check_freq := FALSE;
                            END IF;
                            
                            IF (Instruct = RDARG_C_0 AND (Address >= 16#00800000#)) THEN
                                IF ((CLK_PER < 20.000 ns AND Register_Latency = 0) OR -- <=50MHz
                                   (CLK_PER <  7.510 ns AND Register_Latency = 1)  OR -- <=133MHz
                                   (CLK_PER <  7.510 ns AND Register_Latency = 2)  OR -- <=133MHz
                                   (CLK_PER <  6.020 ns AND Register_Latency = 3)) -- <=166MHz
                                 THEN
                                    ASSERT FALSE
                                    REPORT "More wait states are required for " &
                                        "this clock frequency value"
                                    SEVERITY warning;
                                END IF;
                                Check_freq := FALSE;
                            END IF;

                            
                            IF (Instruct=PKRD1_4_0 OR Instruct=PKRD4_4_0 OR
                                Instruct = RDHL0_0_0 OR Instruct = RDHL1_0_0 OR
                                Instruct=RDSSR_C_0 OR (Instruct=RDARG_C_0 AND (Address < 16#00800000#)) OR
                               Instruct=RDECC_C_0 OR Instruct=RDECC_4_0) AND QPI_IT='1' THEN
                                IF (CLK_PER<55.55 ns AND Latency_code=0) OR--18MHz
                                (CLK_PER<32.25 ns AND Latency_code=1) OR--31MHz
                                (CLK_PER<23.22 ns AND Latency_code=2) OR--43MHz
                                (CLK_PER<17.85 ns AND Latency_code=3) OR--56MHz
                                (CLK_PER<14.70 ns AND Latency_code=4) OR--68MHz
                                (CLK_PER<12.34 ns AND Latency_code=5) OR--81MHz
                                (CLK_PER<10.75 ns AND Latency_code=6) OR--93MHz
                                (CLK_PER<9.43 ns AND Latency_code=7) OR--106MHz
                                (CLK_PER<8.47 ns AND Latency_code=8) OR--118MHz
                                (CLK_PER<7.63 ns AND Latency_code=9) OR--131MHz
                                (CLK_PER<6.99 ns AND Latency_code=10) OR--143MHz
                                (CLK_PER<6.41 ns AND Latency_code=11) OR--156MHz
                                (CLK_PER<6.02 ns AND Latency_code>=12) --166MHz
                                THEN
                                    ASSERT FALSE
                                    REPORT "More wait states are required for " &
                                        "this clock frequency value"
                                    SEVERITY warning;
                                END IF;
                                Check_freq := FALSE;
                            END IF;

                            IF (  Instruct = RDAY7_C_0  OR
                                  Instruct = RDAY7_4_0  OR
                                  Instruct = PKWR4_4_1) THEN
                                IF 
                                (CLK_PER<23.22 ns AND Latency_code<=2) OR--43MHz
                                (CLK_PER<17.85 ns AND Latency_code=3) OR--56MHz
                                (CLK_PER<14.70 ns AND Latency_code=4) OR--68MHz
                                (CLK_PER<12.34 ns AND Latency_code=5) OR--81MHz
                                (CLK_PER<10.75 ns AND Latency_code=6) OR--93MHz
                                (CLK_PER<9.80 ns AND Latency_code>=7)--100MHz
                                THEN
                                    ASSERT FALSE
                                    REPORT "More wait states are required for " &
                                        "this clock frequency value"
                                    SEVERITY warning;
                                END IF;
                                Check_freq := FALSE;
                            END IF;
                        END IF;

                        IF CSNeg_ipd = '0' THEN

                            Latency_code := to_nat(CFR2V(3 downto 0));
                            Register_Latency := to_nat(CFR3V(7 downto 6));

                            -- Wrap Length
                            IF to_nat(CFR4V(1 DOWNTO 0)) = 1 THEN
                                WrapLength := 16;
                            ELSIF to_nat(CFR4V(1 DOWNTO 0)) = 2 THEN
                                WrapLength := 32;
                            ELSIF to_nat(CFR4V(1 DOWNTO 0)) = 3 THEN
                                WrapLength := 64;
                            ELSE
                                WrapLength := 8;
                            END IF;

                            IF QPI_IT = '1' THEN
                                opcode_in(4*opcode_cnt) := IO3RESETNegIn;
                                opcode_in(4*opcode_cnt+1) := WPNegIn;
                                opcode_in(4*opcode_cnt+2) := SOIn;
                                opcode_in(4*opcode_cnt+3):= SIIn;
                            ELSE
                                opcode_in(opcode_cnt) := SIIn;
                            END IF;
                            opcode_cnt := opcode_cnt + 1;

                            IF (QPI_IT = '1' AND opcode_cnt = BYTE/4) OR
                            opcode_cnt = BYTE THEN
                                opcode_cnt := 0;
                                --MSB first
                                FOR I IN 7 DOWNTO 0 LOOP
                                    opcode(i) := opcode_in(7-i);
                                END LOOP;
                                CASE opcode IS
                                    WHEN "00000001"  => --01h
                                        Instruct <= WRREG_0_1;
                                        IF WRPGEN = '1' OR WVREG = '1' THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "00000010"  => --02h
                                        Instruct <= PRPGE_C_1;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := ADDRESS_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "00000011"  => --03h
                                        Instruct <= RDAY1_C_0;
                                        IF QPI_IT = '1' THEN
                                    --Command not supported in QPI_IT mode
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                        END IF;

                                    WHEN "00000100"  => --04h
                                        Instruct <= WRDIS_0_0;
                                        bus_cycle_state := DATA_BYTES;

                                    WHEN "00000101"  => --05h
                                        Instruct <= RDSR1_0_0;
                                        IF Register_Latency <= 1 THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                            bus_cycle_state := DUMMY_BYTES;
                                        END IF;

                                    WHEN "00000110"  => --06h
                                        Instruct <= WRENB_0_0;
                                        bus_cycle_state := DATA_BYTES;

                                    WHEN "00000111"  => --07h
                                        Instruct <= RDSR2_0_0;
                                        IF Register_Latency = 0 OR 
                                        (Register_Latency = 1 AND QPI_IT = '0') THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                        END IF;

                                    WHEN "00001011"  => --0Bh
                                        Instruct <= RDAY2_C_0;
                                        IF QPI_IT = '1' THEN
                                    --Command not supported in QPI_IT mode
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                            Check_freq := TRUE;
                                        END IF;

                                    WHEN "00001100"  => --0Ch
                                        Instruct <= RDAY2_4_0;
                                            bus_cycle_state := ADDRESS_BYTES;
                                            addr_cnt := 0;
                                            Check_freq := TRUE;

                                    WHEN "00010010"  => --12h
                                        Instruct <= PRPGE_4_1;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := ADDRESS_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "00010011"  => --13h
                                        Instruct <= RDAY1_4_0;
                                        IF QPI_IT = '1' THEN
                                    --Command not supported in QPI_IT mode
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                        END IF;

                                    WHEN "00010101"  => --15h
                                        Instruct <= WRAUB_0_1;
                                        data_cnt := 0;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "00011000"  => --18h
                                        Instruct <= RDECC_4_0;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := TRUE;

                                    WHEN "00011001"  => --19h
                                        Instruct <= RDECC_C_0;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := TRUE;

                                    WHEN "00011011"  => --1Bh
                                        Instruct <= CLECC_0_0;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;
                                        
                                    WHEN "00100000"  => --20h
                                        Instruct <= ER004_C_0;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := ADDRESS_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "00100001"  => --21h
                                        Instruct <= ER004_4_0;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := ADDRESS_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "00101111"  => --2Fh
                                        Instruct <= PRASP_0_1;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "00110000"  => --30h
                                        IF CFR3V(2) = '1' THEN
                                            Instruct <= RSEPA_0_0;
                                        ELSE
                                            Instruct <= CLPEF_0_0;
                                        END IF;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF; 

                                    WHEN "00110101"  => --35h
                                        Instruct <= RDCR1_0_0;
                                        read_cnt := 0;
                                        IF Register_Latency = 0 OR 
                                        (Register_Latency = 1 AND QPI_IT = '0') THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                        END IF;

                                    WHEN "01000001"  => --41h
                                        Instruct <= RDDLP_0_0;
                                        read_cnt := 0;
                                        IF Register_Latency = 0 OR 
                                        (Register_Latency = 1 AND QPI_IT = '0') THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                        END IF;

                                    WHEN "01000010"  => --42h
                                        Instruct <= PRSSR_C_1;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := ADDRESS_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "01000011"  => --43h
                                        Instruct <= PRDLP_0_1;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "01001010"  => --4Ah
                                        Instruct <= WRDLP_0_1;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "01001011"  => --4Bh
                                        Instruct <= RDSSR_C_0;
                                        bus_cycle_state := ADDRESS_BYTES;

                                    WHEN "01001100"  => --4Ch
                                        Instruct <= RDUID_0_0;
                                        bus_cycle_state := DUMMY_BYTES;

                                    WHEN "01010000"  => --50h
                                        Instruct <= WRENV_0_0;
                                        bus_cycle_state := DATA_BYTES;

                                    WHEN "01011010"  => --5Ah
                                        Instruct <= RSFDP_3_0;
                                        bus_cycle_state := ADDRESS_BYTES;

                                    WHEN "01011011"  => --5Bh
                                        Instruct <= DICHK_4_1;
                                        bus_cycle_state := ADDRESS_BYTES;

                                    WHEN "01011101"  => --5Dh
                                        Instruct <=  SEERC_C_0;
                                        bus_cycle_state := ADDRESS_BYTES;

                                    WHEN "01100000"  => --60h
                                        Instruct <= ERCHP_0_0;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "01100101"  => --65h
                                        Instruct <= RDARG_C_0;
                                        bus_cycle_state := ADDRESS_BYTES;

                                    WHEN "01100110"  => --66h
                                        Instruct <= SRSTE_0_0;
                                        bus_cycle_state := DATA_BYTES;

                                    WHEN "01101011"  => --6Bh
                                        Instruct <= RDAY4_C_0;
                                        IF QPI_IT = '1' THEN
                                    --Command not supported in QPI_IT mode
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                            Check_freq := TRUE;
                                        END IF;

                                    WHEN "01101100"  => --6Ch
                                        Instruct <= RDAY4_4_0;
                                        IF QPI_IT = '1' THEN
                                    --Command not supported in QPI_IT mode
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                            Check_freq := TRUE;
                                        END IF;

                                    WHEN "01110001"  => --71h
                                        Instruct <= WRARG_C_1;
                                        IF WRPGEN = '1' OR WVREG = '1' THEN
                                            bus_cycle_state := ADDRESS_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "01110101"  => --75h
                                        Instruct <= SPEPD_0_0;
                                        bus_cycle_state := DATA_BYTES;

                                    WHEN "01111010"  => --7Ah
                                        Instruct <= RSEPD_0_0;
                                        bus_cycle_state := DATA_BYTES;

                                    WHEN "10000010"  => --82h
                                        Instruct <= CLPEF_0_0;
                                        bus_cycle_state := DATA_BYTES;

                                    WHEN "10000101"  => --85h
                                        Instruct <= SPEPA_0_0;
                                        bus_cycle_state := DATA_BYTES;

                                    WHEN "10001010"  => --8Ah
                                        Instruct <= RSEPA_0_0;
                                        bus_cycle_state := DATA_BYTES;

                                    WHEN "10011001"  => --99h
                                        Instruct <= SFRST_0_0;
                                        bus_cycle_state := DATA_BYTES;

                                    WHEN "10011111"  => --9Fh
                                        Instruct <= RDIDN_0_0;
                                        IF Register_Latency <= 1 THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                        END IF;

                                    WHEN "10100110"  => --A6h
                                        Instruct <= WRPLB_0_0;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "10100111"  => --A7h
                                        Instruct <= RDPLB_0_0;
                                        IF Register_Latency = 0 OR 
                                        (Register_Latency = 1 AND QPI_IT = '0') THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                        END IF;
                                        
                                    WHEN "10101001"  => --A9h
                                        Instruct <= PKRD2_4_0;
                                        IF CRYPTO_in = '0' THEN
                                            bus_cycle_state := STAND_BY;
                                        ELSIF QUAD_QPI = '1' THEN
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                            Check_freq := true;
                                        END IF;   
                                        
                                    WHEN "10101010"  => --AAh
                                        Instruct <= PKRD3_4_0;
                                        IF CRYPTO_in = '0' THEN
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                            Check_freq := true;
                                        END IF;    
                                        
                                    WHEN "10101100"  => --ACh
                                        Instruct <= PKRD4_4_0;
                                        IF CRYPTO_in = '0' THEN
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                            Check_freq := true;
                                        END IF;     

                                    WHEN "10101111"  => --AFh
                                        Instruct <= RDQID_0_0;
                                        IF Register_Latency = 0 OR 
                                        (Register_Latency = 1 AND QPI_IT = '0') THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                        END IF;

                                    WHEN "10110000"  => --B0h
                                        Instruct <= SPEPA_0_0;
                                        bus_cycle_state := DATA_BYTES;

                                    WHEN "10110111"  => --B7h
                                        Instruct <= EN4BA_0_0;
                                        bus_cycle_state := DATA_BYTES;

                                    WHEN "10111000"  => --B8h
                                        Instruct <= EX4BA_0_0;
                                        bus_cycle_state := DATA_BYTES;

                                    WHEN "10111001" => -- B9h
                                        Instruct <= ENDPD_0_0;
                                        bus_cycle_state := DATA_BYTES;

                                    WHEN "10111011"  => --BBh
                                        Instruct <= RDAY3_C_0;
                                        IF QPI_IT = '1' THEN
                                    --Command not supported in QPI_IT mode
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                            Check_freq := true;
                                        END IF;

                                    WHEN "10111100"  => --BCh
                                        Instruct <= RDAY3_4_0;
                                        IF QPI_IT = '1' THEN
                                    --Command not supported in QPI_IT mode
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                            Check_freq := true;
                                        END IF;


                                    WHEN "11000111"  => --C7h
                                        Instruct <= ERCHP_0_0;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;
                                        
                                    WHEN "11001001"  => --C9h
                                        Instruct <= PKWR2_4_1;
                                        bus_cycle_state := ADDRESS_BYTES;   
                                        
                                    WHEN "11001010"  => --CAh
                                        Instruct <= PKWR3_4_1;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        
                                    WHEN "11001100"  => --CCh
                                        Instruct <= PKWR4_4_1;
                                        bus_cycle_state := ADDRESS_BYTES;    

                                    WHEN "11010000"  => --D0h
                                        Instruct <= EVERS_C_0;
                                        bus_cycle_state := ADDRESS_BYTES;

                                    WHEN "11011000"  => --D8h
                                        Instruct <= ER256_C_0;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := ADDRESS_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "11011100"  => --DCh
                                        Instruct <= ER256_4_0;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := ADDRESS_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "11100000"  => --E0h
                                        Instruct <= RDDYB_4_0;
                                        bus_cycle_state := ADDRESS_BYTES;

                                    WHEN "11100001"  => --E1h
                                        Instruct <= WRDYB_4_1;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := ADDRESS_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "11100010"  => --E2h
                                        Instruct <= RDPPB_4_0;
                                        bus_cycle_state := ADDRESS_BYTES;

                                    WHEN "11100011"  => --E3h
                                        Instruct <= PRPPB_4_0;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := ADDRESS_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "11100100"  => --E4h
                                        Instruct <= ERPPB_0_0;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "11101000"  => --E8h
                                        Instruct <= PGPWD_0_1;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "11101001"  => --E9h
                                        Instruct <= PWDUL_0_1;
                                        bus_cycle_state := DATA_BYTES;

                                    WHEN "11101011"  => --EBh
                                        Instruct <= RDAY5_C_0;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := true;

                                    WHEN "11101100"  => --ECh
                                        Instruct <= RDAY5_4_0;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := true;

                                    WHEN "11101101"  => --EDh
                                        Instruct <= RDAY7_C_0;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := true;

                                    WHEN "11101110"  => --EEh
                                        Instruct <= RDAY7_4_0;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := true;

                                    WHEN "11110000"  => --F0h
                                        Instruct <= SFRSL_0_0;
                                        bus_cycle_state := DATA_BYTES;
                                        
                                    WHEN "11010110"  => --D6h
                                        Instruct <= RDHL0_0_0;
                                        IF CRYPTO_in = '0' THEN
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                        END IF;   
                                    WHEN "11010111"  => --D7h
                                        Instruct <= RDHL1_0_0;
                                        IF CRYPTO_in = '0' THEN
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                        END IF;   

                                    WHEN "11110001"  => --F1h
                                        Instruct <= PKRD1_4_0;
                                        IF CRYPTO_in = '0' THEN
                                            bus_cycle_state := STAND_BY;
                                        ELSE
                                            bus_cycle_state := ADDRESS_BYTES;
                                        END IF;   
                                        
                                    WHEN "11110010"  => --F2h
                                        Instruct <= PKWR1_4_1;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        
                                    WHEN "11110011"  => --F3h
                                        Instruct <= ENCTM_0_0;
                                        bus_cycle_state := DATA_BYTES;
                                        
                                    WHEN "11110100"  => --F4h
                                        Instruct <= EXCTM_0_0;
                                        bus_cycle_state := DATA_BYTES;    

                                    WHEN "11111010"  => --FAh
                                        Instruct <= RDDYB_C_0;
                                        bus_cycle_state := ADDRESS_BYTES;

                                    WHEN "11111011"  => --FBh
                                        Instruct <= WRDYB_C_1;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := ADDRESS_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;

                                    WHEN "11111100"  => --FCh
                                        Instruct <= RDPPB_C_0;
                                        bus_cycle_state := ADDRESS_BYTES;

                                    WHEN "11111101"  => --FDh
                                        Instruct <= PRPPB_C_0;
                                        IF WRPGEN = '1' THEN
                                            bus_cycle_state := ADDRESS_BYTES;
                                        ELSE
                                            bus_cycle_state := STAND_BY;
                                        END IF;
                                    WHEN others =>
                                        null;

                                END CASE;
                            END IF;
                        END IF;
                    END IF;

                WHEN ADDRESS_BYTES =>
                    
                    IF rising_edge(SCK_ipd) AND CSNeg_ipd = '0' THEN
                        IF (  Instruct = RDAY7_C_0  OR
                              Instruct = RDAY7_4_0  OR
                              Instruct = PKWR4_4_1  OR
                              Instruct = PKRD4_4_0) THEN
                            double <= TRUE;
                        ELSE
                            double <= FALSE;
                        END IF;

                        IF ((Instruct = RDAY2_C_0 OR Instruct = RDSSR_C_0
                        OR Instruct = RDECC_C_0 OR Instruct = RDAY4_C_0) AND CFR2V(7)='0') THEN
                        -- Instruction + 3 Bytes Address + Dummy Byte
                            IF QPI_IT = '1' THEN
                                Address_in(4*addr_cnt) := IO3RESETNegIn;
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
                                    addr_bytes(31 downto 24):="00000000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF (Latency_code = 0) THEN
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
                                    addr_bytes(31 downto 24):="00000000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF (Latency_code = 0) THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                END IF;
                            END IF;
                            
                         ELSIF ((Instruct = RDARG_C_0) AND CFR2V(7)='0') THEN
                        -- Instruction + 3 Bytes Address + Dummy Byte
                            IF QPI_IT = '1' THEN
                                Address_in(4*addr_cnt) := IO3RESETNegIn;
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
                                    addr_bytes(31 downto 24):="00000000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF (addr_bytes(23) = '1') THEN --volatile REGS
                                        IF (Register_Latency = 0) THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                            bus_cycle_state := DUMMY_BYTES;
                                        END IF;
                                        
                                    ELSE -- NV REGS 
                                        IF Latency_code = 0 THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                            bus_cycle_state := DUMMY_BYTES;
                                        END IF;
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
                                    addr_bytes(31 downto 24):="00000000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF (addr_bytes(23) = '1') THEN --volatile REGS
                                        IF (Register_Latency = 0) THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                            bus_cycle_state := DUMMY_BYTES;
                                        END IF;
                                        
                                    ELSE -- NV REGS 
                                        IF Latency_code = 0 THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                            bus_cycle_state := DUMMY_BYTES;
                                        END IF;
                                    END IF;
                                END IF;
                            END IF;
                            
                        ELSIF ((Instruct = RDPPB_C_0) AND CFR2V(7)='0') THEN
                        -- Instruction + 3 Bytes Address + Dummy Byte
                            IF QPI_IT = '1' THEN
                                Address_in(4*addr_cnt) := IO3RESETNegIn;
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
                                    addr_bytes(31 downto 24):="00000000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF (Latency_code = 0) THEN
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
                                    addr_bytes(31 downto 24):="00000000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF (Latency_code = 0) THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                END IF;
                            END IF;
                        
                        ELSIF ((Instruct = RDDYB_C_0) AND CFR2V(7)='0') THEN
                        -- Instruction + 3 Bytes Address + Dummy Byte
                            IF QPI_IT = '1' THEN
                                Address_in(4*addr_cnt) := IO3RESETNegIn;
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
                                    addr_bytes(31 downto 24):="00000000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF (Register_Latency = 0) THEN
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
                                    addr_bytes(31 downto 24):="00000000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF (Register_Latency = 0) THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                END IF;
                            END IF;
                        ELSIF  Instruct = RDECC_4_0 OR Instruct = RDAY4_4_0 OR
                               Instruct = PKRD2_4_0 OR Instruct = PKRD1_4_0 OR
                               Instruct = RDHL0_0_0 OR Instruct = RDHL1_0_0 OR
                        ((Instruct = RDAY2_C_0 OR Instruct = RDSSR_C_0      OR
                          Instruct = RDECC_C_0 OR Instruct = RDAY4_C_0)     AND
                          CFR2V(7) = '1') THEN
                        -- Instruction + 4 Bytes Address + Dummy Byte
                            IF QPI_IT = '1' THEN
                                Address_in(4*addr_cnt) := IO3RESETNegIn;
                                Address_in(4*addr_cnt+1) := WPNegIn;
                                Address_in(4*addr_cnt+2) := SOIn;
                                Address_in(4*addr_cnt+3) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = (4*BYTE)/4 THEN
                                    addr_cnt := 0;
                                    FOR I IN 31 DOWNTO 0 LOOP
                                        hiaddr_bytes(31-i) := Address_in(i);
                                    END LOOP;
                                    hiaddr_bytes(31 downto 25):="0000000";
                                    Address <= to_nat(hiaddr_bytes(24 downto 0));
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF (Latency_code = 0) THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                END IF;
                            ELSE
                                Address_in(addr_cnt) := SIIn;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = 4*BYTE THEN
                                    FOR I IN 31 DOWNTO 0 LOOP
                                        hiaddr_bytes(31-i) := Address_in(i);
                                    END LOOP;
                                    hiaddr_bytes(31 downto 25):="0000000";
                                    Address <= to_nat(hiaddr_bytes(24 downto 0));
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF (Latency_code = 0) THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                END IF;
                            END IF;
                        ELSIF  Instruct = RDAY2_4_0 THEN 
                        -- Instruction + 4 Bytes Address + Dummy Byte
                            IF QPI_IT = '1' THEN
                                Address_in(4*addr_cnt) := IO3RESETNegIn;
                                Address_in(4*addr_cnt+1) := WPNegIn;
                                Address_in(4*addr_cnt+2) := SOIn;
                                Address_in(4*addr_cnt+3) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = BYTE THEN
                                    addr_cnt := 0;
                                    FOR I IN 31 DOWNTO 0 LOOP
                                        hiaddr_bytes(31-i) := Address_in(i);
                                    END LOOP;
                                    hiaddr_bytes(31 downto 25):="0000000";
                                    Address <= to_nat(hiaddr_bytes(24 downto 0));
                                    change_addr <= '1','0' AFTER 1 ns;

                                    bus_cycle_state := MODE_BYTE;
      
                                END IF;
                            ELSE
                                Address_in(addr_cnt) := SIIn;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = 4*BYTE THEN
                                    addr_cnt := 0;
                                    FOR I IN 31 DOWNTO 0 LOOP
                                        hiaddr_bytes(31-i) := Address_in(i);
                                    END LOOP;
                                    hiaddr_bytes(31 downto 25):="0000000";
                                    Address <= to_nat(hiaddr_bytes(24 downto 0));
                                    change_addr <= '1','0' AFTER 1 ns;
                                    bus_cycle_state := MODE_BYTE;
                                END IF;
                            END IF;
                        ELSIF ((Instruct = RDARG_C_0) AND CFR2V(7) = '1') THEN
                        -- Instruction + 4 Bytes Address + Dummy Byte
                            IF QPI_IT = '1' THEN
                                Address_in(4*addr_cnt) := IO3RESETNegIn;
                                Address_in(4*addr_cnt+1) := WPNegIn;
                                Address_in(4*addr_cnt+2) := SOIn;
                                Address_in(4*addr_cnt+3) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = (4*BYTE)/4 THEN
                                    addr_cnt := 0;
                                    FOR I IN 31 DOWNTO 0 LOOP
                                        hiaddr_bytes(31-i) := Address_in(i);
                                    END LOOP;
                                    hiaddr_bytes(31 downto 25):="0000000";
                                    Address <= to_nat(hiaddr_bytes(24 downto 0));
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF (Address >= 16#00800000#) THEN --volatile REGS
                                        IF (Register_Latency = 0) THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                            bus_cycle_state := DUMMY_BYTES;
                                        END IF;
                                        
                                    ELSE -- NV REGS 
                                        IF Latency_code = 0 THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                            bus_cycle_state := DUMMY_BYTES;
                                        END IF;
                                    END IF;
                                END IF;
                            ELSE
                                Address_in(addr_cnt) := SIIn;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = 4*BYTE THEN
                                    FOR I IN 31 DOWNTO 0 LOOP
                                        hiaddr_bytes(31-i) := Address_in(i);
                                    END LOOP;
                                    hiaddr_bytes(31 downto 25):="0000000";
                                    Address <= to_nat(hiaddr_bytes(24 downto 0));
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF (Address >= 16#00800000#) THEN --volatile REGS
                                        IF (Register_Latency = 0) THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                            bus_cycle_state := DUMMY_BYTES;
                                        END IF;
                                        
                                    ELSE -- NV REGS 
                                        IF Latency_code = 0 THEN
                                            bus_cycle_state := DATA_BYTES;
                                        ELSE
                                            bus_cycle_state := DUMMY_BYTES;
                                        END IF;
                                    END IF;
                                END IF;
                            END IF;
                        ELSIF (Instruct = RDPPB_4_0 OR (Instruct = RDPPB_C_0 AND CFR2V(7) = '1')) THEN
                        -- Instruction + 4 Bytes Address + Dummy Byte
                            IF QPI_IT = '1' THEN
                                Address_in(4*addr_cnt) := IO3RESETNegIn;
                                Address_in(4*addr_cnt+1) := WPNegIn;
                                Address_in(4*addr_cnt+2) := SOIn;
                                Address_in(4*addr_cnt+3) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = (4*BYTE)/4 THEN
                                    addr_cnt := 0;
                                    FOR I IN 31 DOWNTO 0 LOOP
                                        hiaddr_bytes(31-i) := Address_in(i);
                                    END LOOP;
                                    hiaddr_bytes(31 downto 25):="0000000";
                                    Address <= to_nat(hiaddr_bytes(24 downto 0));
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF (Latency_code = 0) THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                END IF;
                            ELSE
                                Address_in(addr_cnt) := SIIn;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = 4*BYTE THEN
                                    FOR I IN 31 DOWNTO 0 LOOP
                                        hiaddr_bytes(31-i) := Address_in(i);
                                    END LOOP;
                                    hiaddr_bytes(31 downto 25):="0000000";
                                    Address <= to_nat(hiaddr_bytes(24 downto 0));
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF (Latency_code = 0) THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                END IF;
                            END IF;
                        ELSIF (Instruct = RDDYB_4_0) OR ((Instruct = RDDYB_C_0) AND CFR2V(7) = '1') THEN
                        -- Instruction + 4 Bytes Address + Dummy Byte
                            IF QPI_IT = '1' THEN
                                Address_in(4*addr_cnt) := IO3RESETNegIn;
                                Address_in(4*addr_cnt+1) := WPNegIn;
                                Address_in(4*addr_cnt+2) := SOIn;
                                Address_in(4*addr_cnt+3) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = (4*BYTE)/4 THEN
                                    addr_cnt := 0;
                                    FOR I IN 31 DOWNTO 0 LOOP
                                        hiaddr_bytes(31-i) := Address_in(i);
                                    END LOOP;
                                    hiaddr_bytes(31 downto 25):="0000000";
                                    Address <= to_nat(hiaddr_bytes(24 downto 0));
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF (Register_Latency = 0) THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                END IF;
                            ELSE
                                Address_in(addr_cnt) := SIIn;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = 4*BYTE THEN
                                    FOR I IN 31 DOWNTO 0 LOOP
                                        hiaddr_bytes(31-i) := Address_in(i);
                                    END LOOP;
                                    hiaddr_bytes(31 downto 25):="0000000";
                                    Address <= to_nat(hiaddr_bytes(24 downto 0));
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF (Register_Latency = 0) THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                END IF;
                            END IF;
                        ELSIF Instruct = DICHK_4_1  THEN
                        -- Instruction + 4 Bytes Address
                            IF QPI_IT = '1' THEN
                                Address_in(4*(addr_cnt MOD 8)) := IO3RESETNegIn;
                                Address_in(4*(addr_cnt MOD 8)+1) := WPNegIn;
                                Address_in(4*(addr_cnt MOD 8)+2) := SOIn;
                                Address_in(4*(addr_cnt MOD 8)+3) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = (4*BYTE)/4 THEN
                                    FOR I IN 31 DOWNTO 0 LOOP
                                        hiaddr_bytes(31-i) := Address_in(i);
                                    END LOOP;
                                    hiaddr_bytes(31 downto 25):="0000000";
                                    Address <= to_nat(hiaddr_bytes(24 downto 0));
                                    change_addr <= '1','0' AFTER 1 ns;
                                END IF;
                                IF addr_cnt = (8*BYTE)/4 THEN
                                    addr_cnt := 0;
                                    FOR I IN 31 DOWNTO 0 LOOP
                                        hiaddr_bytes(31-i) := Address_in(i);
                                    END LOOP;
                                    hiaddr_bytes(31 downto 25):="0000000";
                                    Address <= to_nat(hiaddr_bytes(24 downto 0));
                                    change_addr <= '1','0' AFTER 1 ns;
                                    bus_cycle_state := DATA_BYTES;
                                END IF;
                            ELSE
                                Address_in(addr_cnt MOD 32) := SIIn;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = 4*BYTE THEN
                                    FOR I IN 31 DOWNTO 0 LOOP
                                        hiaddr_bytes(31-i) := Address_in(i);
                                    END LOOP;
                                    hiaddr_bytes(31 downto 25):="0000000";
                                    Address <= to_nat(hiaddr_bytes(24 downto 0));
                                    change_addr <= '1','0' AFTER 1 ns;
                                END IF;
                                IF addr_cnt = 8*BYTE THEN
                                    addr_cnt := 0;
                                    FOR I IN 31 DOWNTO 0 LOOP
                                        hiaddr_bytes(31-i) := Address_in(i);
                                    END LOOP;
                                    hiaddr_bytes(31 downto 25):="0000000";
                                    Address <= to_nat(hiaddr_bytes(24 downto 0));
                                    change_addr <= '1','0' AFTER 1 ns;
                                    bus_cycle_state := DATA_BYTES;
                                END IF;
                            END IF;
                        ELSIF Instruct = RDAY3_C_0 AND CFR2V(7) = '0' THEN
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
                                addr_bytes(31 downto 24):="00000000";
                                Address <= to_nat(addr_bytes);
                                change_addr <= '1','0' AFTER 1 ns;
                                bus_cycle_state := MODE_BYTE;
                            END IF;
                        ELSIF Instruct = RDAY3_4_0 OR (Instruct = RDAY3_C_0
                        AND CFR2V(7) = '1') THEN
                        -- DUAL I/O High Performance Read(4 Bytes Addr)
                            Address_in(2*addr_cnt) := SOIn;
                            Address_in(2*addr_cnt+1) := SIIn;
                            read_cnt := 0;
                            addr_cnt := addr_cnt + 1;
                            IF addr_cnt = (4*BYTE)/2 THEN
                                addr_cnt := 0;
                                FOR I IN 31 DOWNTO 0 LOOP
                                    hiaddr_bytes(31-i) := Address_in(i);
                                END LOOP;
                                hiaddr_bytes(31 downto 25):="0000000";
                                Address <= to_nat(hiaddr_bytes(24 downto 0));
                                change_addr <= '1','0' AFTER 1 ns;
                                bus_cycle_state := MODE_BYTE;
                            END IF;
                        ELSIF Instruct = RDAY5_C_0 AND CFR2V(7) = '0' THEN
                        -- QUAD I/O High Performance Read(3 Bytes Addr)
                            IF QUADIT = '1' THEN
                                Address_in(4*addr_cnt) := IO3RESETNegIn;
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
                                    addr_bytes(31 downto 24):="00000000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    bus_cycle_state := MODE_BYTE;
                                END IF;
                            ELSE
                                bus_cycle_state := STAND_BY;
                            END IF;
                        ELSIF Instruct = RDAY5_4_0 OR Instruct = PKRD3_4_0 OR
                        (Instruct = RDAY5_C_0 AND CFR2V(7) = '1') THEN
                        -- QUAD I/O High Performance Read(4 Bytes Addr)
                            IF QUADIT = '1' THEN
                                Address_in(4*addr_cnt) := IO3RESETNegIn;
                                Address_in(4*addr_cnt+1) := WPNegIn;
                                Address_in(4*addr_cnt+2) := SOIn;
                                Address_in(4*addr_cnt+3) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = (4*BYTE)/4 THEN
                                    addr_cnt := 0;
                                    FOR I IN 31 DOWNTO 0 LOOP
                                        hiaddr_bytes(31-i) := Address_in(i);
                                    END LOOP;
                                    hiaddr_bytes(31 downto 25):="0000000";
                                    Address <= to_nat(hiaddr_bytes(24 downto 0));
                                    change_addr <= '1','0' AFTER 1 ns;
                                    bus_cycle_state := MODE_BYTE;
                                END IF;
                            ELSE
                                bus_cycle_state := STAND_BY;
                            END IF;
                        ELSIF (Instruct= WRARG_C_1 AND CFR2V(7)='1')  THEN
                        -- Instruction + 4 Bytes Address
                            IF QPI_IT = '1' THEN
                                Address_in(4*addr_cnt) := IO3RESETNegIn;
                                Address_in(4*addr_cnt+1) := WPNegIn;
                                Address_in(4*addr_cnt+2) := SOIn;
                                Address_in(4*addr_cnt+3) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = (4*BYTE)/4 THEN
                                    addr_cnt := 0;
                                    FOR I IN 31 DOWNTO 0 LOOP
                                        hiaddr_bytes(31-i) := Address_in(i);
                                    END LOOP;
                                    hiaddr_bytes(31 downto 25):="0000000";
                                    Address <= to_nat(hiaddr_bytes(24 downto 0));
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF WRPGEN='1' THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSIF (hiaddr_bytes(23) = '1' AND WVREG='1') THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                    
                                 END IF;
                            ELSE
                                Address_in(addr_cnt) := SIIn;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = 4*BYTE THEN
                                    FOR I IN 31 DOWNTO 0 LOOP
                                        hiaddr_bytes(31-i) := Address_in(i);
                                    END LOOP;
                                    hiaddr_bytes(31 downto 25):="0000000";
                                    Address <= to_nat(hiaddr_bytes(24 downto 0));
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF WRPGEN='1' THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSIF (hiaddr_bytes(23) = '1' AND WVREG='1') THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                END IF;
                            END IF;
                         ELSIF (Instruct= WRARG_C_1 AND CFR2V(7)='0')  THEN
                         -- Instruction + 3 Bytes Address
                            IF QPI_IT = '1' THEN
                                Address_in(4*addr_cnt) := IO3RESETNegIn;
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
                                    addr_bytes(31 downto 24):="00000000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF WRPGEN='1' THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSIF (addr_bytes(23) = '1' AND WVREG='1') THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                END IF;
                            ELSE
                                Address_in(addr_cnt) := SIIn;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = 3*BYTE THEN
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    addr_bytes(31 downto 24):="00000000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF WRPGEN='1' THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSIF (addr_bytes(23) = '1' AND WVREG='1') THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                END IF;
                            END IF;
                        ELSIF (Instruct= RDAY1_4_0 OR Instruct= ER256_4_0 OR
                        Instruct= WRDYB_4_1 OR Instruct= PRPPB_4_0 OR
                        Instruct= ER004_4_0 OR Instruct= PRPGE_4_1) OR (
                        (Instruct= RDAY1_C_0 AND CFR2V(7)='1') OR
                        (Instruct= EVERS_C_0  AND CFR2V(7)='1') OR
                        (Instruct= PRPGE_C_1   AND CFR2V(7)='1') OR
                        (Instruct= ER004_C_0  AND CFR2V(7)='1') OR
                        (Instruct= ER256_C_0   AND CFR2V(7)='1') OR
                        (Instruct= SEERC_C_0  AND CFR2V(7)='1') OR
                        (Instruct= PRSSR_C_1 AND CFR2V(7)='1') OR
                        (Instruct= WRDYB_C_1 AND CFR2V(7)='1') OR
                        (Instruct= PRPPB_C_0 AND CFR2V(7)='1') OR
                        Instruct = PKWR1_4_1                   OR
                        Instruct = PKWR2_4_1                   OR
                        Instruct = PKWR3_4_1) THEN
                        -- Instruction + 4 Bytes Address
                            IF ( QPI_IT = '1' OR (Instruct = PKWR3_4_1) ) THEN
                                Address_in(4*addr_cnt) := IO3RESETNegIn;
                                Address_in(4*addr_cnt+1) := WPNegIn;
                                Address_in(4*addr_cnt+2) := SOIn;
                                Address_in(4*addr_cnt+3) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = (4*BYTE)/4 THEN
                                    addr_cnt := 0;
                                    FOR I IN 31 DOWNTO 0 LOOP
                                        hiaddr_bytes(31-i) := Address_in(i);
                                    END LOOP;
                                    hiaddr_bytes(31 downto 25):="0000000";
                                    Address <= to_nat(hiaddr_bytes(24 downto 0));
                                    change_addr <= '1','0' AFTER 1 ns;
                                    bus_cycle_state := DATA_BYTES;
                                END IF;
                            ELSE
                                Address_in(addr_cnt) := SIIn;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = 4*BYTE THEN
                                    FOR I IN 31 DOWNTO 0 LOOP
                                        hiaddr_bytes(31-i) := Address_in(i);
                                    END LOOP;
                                    hiaddr_bytes(31 downto 25):="0000000";
                                    Address <= to_nat(hiaddr_bytes(24 downto 0));
                                    change_addr <= '1','0' AFTER 1 ns;
                                    bus_cycle_state := DATA_BYTES;
                                END IF;
                            END IF;
                        ELSIF Instruct = RDAY7_C_0 AND CFR2V(7) = '0'
                        AND QUADIT = '1' THEN
                    --Quad I/O DDR Read Mode (3 Bytes Address)
                            Address_in(4*addr_cnt) := IO3RESETNegIn;
                            Address_in(4*addr_cnt+1) := WPNegIn;
                            Address_in(4*addr_cnt+2) := SOIn;
                            Address_in(4*addr_cnt+3) := SIIn;
                            opcode_tmp(addr_cnt/2) := SIIn;
                            read_cnt := 0;
                            addr_cnt := addr_cnt + 1;
                        ELSIF ( Instruct = RDAY7_4_0  OR
                                Instruct = PKRD4_4_0  OR
                                Instruct = PKWR4_4_1  OR
                               (Instruct = RDAY7_C_0 AND CFR2V(7) = '1')) AND
                                QUADIT = '1' THEN
                    --Quad I/O DDR Read Mode (4 Bytes Address)
                            Address_in(4*addr_cnt) := IO3RESETNegIn;
                            Address_in(4*addr_cnt+1) := WPNegIn;
                            Address_in(4*addr_cnt+2) := SOIn;
                            Address_in(4*addr_cnt+3) := SIIn;
                            opcode_tmp(addr_cnt/2) := SIIn;
                            read_cnt := 0;
                            addr_cnt := addr_cnt + 1;
                        ELSIF Instruct = RSFDP_3_0 AND CFR2V(7) = '0' THEN
                        -- Instruction + 3 Bytes Address + Dummy Byte
                            IF QPI_IT = '1' THEN
                                Address_in(4*addr_cnt) := IO3RESETNegIn;
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
                                    addr_bytes(31 downto 24):="00000000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    bus_cycle_state := DUMMY_BYTES;
                                END IF;
                            ELSE
                                Address_in(addr_cnt) := SIIn;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = 3*BYTE THEN
                                    addr_cnt := 0;
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    addr_bytes(31 downto 24):="00000000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    bus_cycle_state := DUMMY_BYTES;
                                END IF;
                            END IF;
                        ELSIF CFR2V(7)='0' THEN
                        -- Instruction + 3 Bytes Address
                            IF QPI_IT = '1' THEN
                                Address_in(4*addr_cnt) := IO3RESETNegIn;
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
                                    addr_bytes(31 downto 24):="00000000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    bus_cycle_state := DATA_BYTES;
                                END IF;
                            ELSE
                                Address_in(addr_cnt) := SIIn;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = 3*BYTE THEN
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    addr_bytes(31 downto 24):="00000000";
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    bus_cycle_state := DATA_BYTES;
                                END IF;
                            END IF;
                        END IF;

                    ELSIF falling_edge(SCK_ipd) AND CSNeg_ipd = '0' THEN
                        IF Instruct = RDAY7_C_0 AND CFR2V(7) = '0' AND QUADIT = '1' THEN
                        --Quad I/O DDR Read Mode (3 Bytes Address)
                            Address_in(4*addr_cnt) := IO3RESETNegIn;
                            Address_in(4*addr_cnt+1) := WPNegIn;
                            Address_in(4*addr_cnt+2) := SOIn;
                            Address_in(4*addr_cnt+3) := SIIn;
                            IF addr_cnt /= 0 THEN
                                addr_cnt := addr_cnt + 1;
                            END IF;
                            read_cnt := 0;
                            IF addr_cnt = (3*BYTE)/4 THEN
                                addr_cnt := 0;
                                FOR I IN 23 DOWNTO 0 LOOP
                                    addr_bytes(23-i) := Address_in(i);
                                END LOOP;
                                addr_bytes(31 downto 24):="00000000";
                                Address <= to_nat(addr_bytes);
                                change_addr <= '1','0' AFTER 1 ns;
                                bus_cycle_state := MODE_BYTE;
                            END IF;
                        ELSIF ( Instruct = RDAY7_4_0  OR
                                Instruct = PKRD4_4_0  OR
                                Instruct = PKWR4_4_1  OR
                        (Instruct = RDAY7_C_0 AND CFR2V(7) = '1')) AND QUAD_QPI = '1' THEN
                        --Quad I/O DDR Read Mode (4 Bytes Address)
                            Address_in(4*addr_cnt) := IO3RESETNegIn;
                            Address_in(4*addr_cnt+1) := WPNegIn;
                            Address_in(4*addr_cnt+2) := SOIn;
                            Address_in(4*addr_cnt+3) := SIIn;
                            IF addr_cnt /= 0 THEN
                                addr_cnt := addr_cnt + 1;
                            END IF;
                            read_cnt := 0;
                            IF addr_cnt = (4*BYTE)/4 THEN
                                addr_cnt := 0;
                                FOR I IN 31 DOWNTO 0 LOOP
                                    hiaddr_bytes(31-i) := Address_in(i);
                                END LOOP;
                                hiaddr_bytes(31 downto 25):="0000000";
                                Address <= to_nat(hiaddr_bytes(24 downto 0));
                                change_addr <= '1','0' AFTER 1 ns;
                                IF (Instruct = PKWR4_4_1) THEN
                                    bus_cycle_state := DATA_BYTES;
                                ELSE
                                    bus_cycle_state := MODE_BYTE;
                                END IF;
                            END IF;
                        ELSIF (Instruct = DICHK_4_1) THEN
                            IF addr_cnt = (4*BYTE)/4 OR addr_cnt = 4*BYTE THEN 
                                DIC_Start_Addr_reg := Address;
                            END IF;
                        END IF;
                    END IF;
                    
                    
                WHEN MODE_BYTE =>
                    
                    IF rising_edge(SCK_ipd) AND CSNeg = '0' THEN
                        IF Instruct = RDAY3_C_0 OR Instruct = RDAY3_4_0 THEN
                            mode_in(2*mode_cnt)   := SOIn;
                            mode_in(2*mode_cnt+1) := SIIn;
                            mode_cnt := mode_cnt + 1;
                            IF mode_cnt = BYTE/2 THEN
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
                        ELSIF (Instruct = RDAY2_4_0) AND QPI_IT = '1' THEN 
                            read_cnt := 0;
                            mode_in(4*mode_cnt)   := IO3RESETNegIn;
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
                         ELSIF (Instruct = RDAY2_4_0) AND QPI_IT = '0' THEN  
                            read_cnt := 0;
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
                        ELSIF (Instruct = RDAY5_C_0 OR Instruct = RDAY5_4_0 OR
                        Instruct = PKRD3_4_0) AND QUADIT = '1' THEN
                            mode_in(4*mode_cnt)   := IO3RESETNegIn;
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
                        ELSIF (Instruct = RDAY7_C_0 OR Instruct = RDAY7_4_0 OR
                               Instruct = PKRD4_4_0) AND QUADIT = '1' THEN
                            mode_in(0) := IO3RESETNegIn;
                            mode_in(1) := WPNegIn;
                            mode_in(2) := SOIn;
                            mode_in(3) := SIIn;
                        END IF;
                         dummy_cnt := 0;
                        

                    ELSIF falling_edge(SCK_ipd) AND CSNeg = '0' THEN
                        IF Instruct = RDAY7_C_0 OR
                           Instruct = RDAY7_4_0 OR
                           Instruct = PKRD4_4_0 THEN
                            mode_in(4) := IO3RESETNegIn;
                            mode_in(5) := WPNegIn;
                            mode_in(6) := SOIn;
                            mode_in(7) := SIIn;
                            FOR I IN 7 DOWNTO 0 LOOP
                                mode_bytes(i) := mode_in(7-i);
                            END LOOP;
                            
                            IF DLPV /= "00000000" THEN
                                read_out <= '1', '0' AFTER 1 ns;
                            END IF;
                            IF Latency_code = 0 THEN
                                bus_cycle_state := DATA_BYTES;
                            ELSE
                                bus_cycle_state := DUMMY_BYTES;
                            END IF;
                        END IF;
                    END IF;
                    

                WHEN DUMMY_BYTES =>
                    IF rising_edge(SCK_ipd) AND CSNeg = '0' THEN
                        dummy_cnt := dummy_cnt + 1;
                        IF (Instruct = RDAY7_C_0 OR Instruct = RDAY7_4_0) AND
                        (DLPV /= "00000000") AND (dummy_cnt >= (2*Latency_code-8)) THEN
                            read_out <= '1', '0' AFTER 1 ns;
                        END IF;
                       

                    ELSIF falling_edge(SCK_ipd) AND CSNeg = '0' THEN
--                         IF dummy_cnt /= 0 THEN
                            dummy_cnt := dummy_cnt + 1;
--                         END IF;
                        IF ((Instruct = RDAY7_C_0 OR Instruct = RDAY7_4_0 OR Instruct = PKRD4_4_0) AND
                        (DLPV /= "00000000") AND (dummy_cnt >= (2*Latency_code-8)))
                         OR ((Instruct = RDAY2_4_0 OR Instruct = RDAY4_C_0 
                         OR Instruct = RDAY5_C_0 OR Instruct = RDAY5_4_0 OR Instruct = PKRD3_4_0
                         OR Instruct = RDAY4_4_0 OR Instruct = PKRD2_4_0) AND 
                        (DLPV /= "00000000") AND (dummy_cnt >= (2*Latency_code-16)))
                         THEN
                            read_out <= '1', '0' AFTER 1 ns;
                        END IF;
                        IF Instruct = RSFDP_3_0 THEN
                            IF dummy_cnt = 14 THEN
                                bus_cycle_state := DATA_BYTES;
                            END IF;
                        ELSIF Instruct = RDUID_0_0 THEN
                            IF dummy_cnt = 62 THEN
                                bus_cycle_state := DATA_BYTES;
                            END IF;
                        
                        ELSIF Instruct = RDARG_C_0 THEN
                            IF (Address >= 16#00800000#) THEN --volatile REGS
                                IF Register_Latency = dummy_cnt/2 THEN
                                    bus_cycle_state := DATA_BYTES;
                                    read_out <= '1', '0' AFTER 1 ns;
                                END IF;
                            ELSE -- NV REGS
                                IF Latency_code = dummy_cnt/2 THEN
                                    bus_cycle_state := DATA_BYTES;
                                    read_out <= '1', '0' AFTER 1 ns;
                                END IF;
                            END IF;
                      
                      ELSIF Instruct = RDIDN_0_0 OR 
                        Instruct = RDQID_0_0 OR ( QPI_IT = '0' AND (Instruct = RDSR2_0_0 
                        OR Instruct = RDCR1_0_0 OR Instruct = RDDLP_0_0 OR Instruct = RDPLB_0_0)) THEN
                            IF Register_Latency = dummy_cnt/2+1 AND Register_Latency >= 2 THEN
                                bus_cycle_state := DATA_BYTES;
                                read_out <= '1', '0' AFTER 1 ns;
                            END IF;
                            
                      ELSIF Instruct = RDSR1_0_0 THEN
                          IF Register_Latency = dummy_cnt/2+1 AND Register_Latency >= 2 THEN
                              bus_cycle_state := DATA_BYTES;
                                read_out <= '1', '0' AFTER 1 ns;
                          ELSE
                             bus_cycle_state := DATA_BYTES;
                                read_out <= '1', '0' AFTER 1 ns;
                          END IF;

                      
                      ELSIF  Instruct = RDDYB_C_0 OR Instruct = RDDYB_4_0 OR
                             ( QPI_IT = '1' AND (Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 
                              OR Instruct = RDDLP_0_0 OR Instruct = RDPLB_0_0)) THEN
                            IF (Register_Latency = dummy_cnt/2 AND Register_Latency = 1) OR
                               (Register_Latency = dummy_cnt/2+1 AND Register_Latency > 1) THEN
                                bus_cycle_state := DATA_BYTES;
                                read_out <= '1', '0' AFTER 1 ns;
                            END IF;
                      ELSE
                            IF Latency_code = dummy_cnt/2 THEN
                                bus_cycle_state := DATA_BYTES;
                                read_out <= '1', '0' AFTER 1 ns;
                            END IF;
                        END IF;
                        data_cnt := 0;
                      END IF;
                    

                WHEN DATA_BYTES =>
                    dummy_cnt := 0;
                    IF rising_edge(SCK_ipd) AND CSNeg = '0' THEN
                        IF Instruct = RDAY7_C_0 OR Instruct = RDAY7_4_0 OR Instruct = PKRD4_4_0 THEN
                            read_out <= '1', '0' AFTER 1 ns;
                        END IF;

                        IF (QPI_IT = '0' AND (Instruct = PKWR1_4_1)) THEN
                            IF SIIn/='Z' AND SIIn/='X' THEN
                                Data_in(data_cnt) := SIIn;
                            END IF;
                            data_cnt := data_cnt + 1;

                        ELSIF ( -- 1-1-4 or 1-4-4 (QUADIT Mode)
                                (QUADIT = '1' AND (Instruct = PKWR2_4_1   OR
                                                   Instruct = PKWR3_4_1)) OR

                                -- 4-4-4 (QPI Mode)
                                (QPI_IT = '1' AND (Instruct = PKWR1_4_1   OR
                                             Instruct = PKWR4_4_1))
                              ) THEN
                            quad_nybble := IO3RESETNegIn & WPNegIn & SOIn & SIIn;

                            IF data_cnt > ((CryptoPacketSize+1)*2-1) THEN
                            --In case of quad mode,if more than CryptoPacketSize+1 bytes
                            --are sent to the device previously latched data
                            --are discarded and last 840 data bytes are
                            --guaranteed to be programmed correctly within
                            --the same page.
                                FOR I IN 0 TO (CryptoPacketSize*2) LOOP
                                    quad_data_in(i) := quad_data_in(i+1);
                                END LOOP;
                                quad_data_in((CryptoPacketSize+1)*2-1) :=
                                                    to_nat(quad_nybble);
                                data_cnt := data_cnt +1;
                            ELSE
                                quad_data_in(data_cnt) :=
                                to_nat(quad_nybble);
                                data_cnt := data_cnt +1;
                            END IF;

                        ELSIF Instruct = WRAUB_0_1 AND QPI_IT = '1' THEN
                            IF IO3RESETNegIn /= 'Z' AND IO3RESETNegIn /= 'X' AND
                            WPNegIn /= 'Z' AND WPNegIn /= 'X' AND
                            SOIn /= 'Z' AND SOIn /= 'X' AND
                            SIIn /= 'Z' AND SIIn /= 'X' THEN
                                quad_nybble := IO3RESETNegIn & WPNegIn & SOIn & SIIn;
                            END IF;
                            IF data_cnt < 9 THEN --???
                            --In case of quad mode,if more than PageSize+1 bytes
                            --are sent to the device previously latched data
                            --are discarded and last 256/512 data bytes are
                            --guaranteed to be programmed correctly within
                            --the same page.
                                quad_data_in(data_cnt) :=
                                to_nat(quad_nybble);
                                data_cnt := data_cnt +1;
                            END IF;
                        ELSIF Instruct = PRASP_0_1  AND QPI_IT = '1' THEN
                            IF IO3RESETNegIn /= 'Z' AND IO3RESETNegIn /= 'X' AND
                            WPNegIn /= 'Z' AND WPNegIn /= 'X' AND
                            SOIn /= 'Z' AND SOIn /= 'X' AND
                            SIIn /= 'Z' AND SIIn /= 'X' THEN
                                quad_nybble := IO3RESETNegIn & WPNegIn & SOIn & SIIn;
                            END IF;
                            IF data_cnt < 4 THEN --???
                            --In case of quad mode,if more than PageSize+1 bytes
                            --are sent to the device previously latched data
                            --are discarded and last 256/512 data bytes are
                            --guaranteed to be programmed correctly within
                            --the same page.
                                quad_data_in(data_cnt) :=
                                to_nat(quad_nybble);
                                data_cnt := data_cnt +1;
                            END IF;
                        ELSIF Instruct = PWDUL_0_1  AND QPI_IT = '1' THEN
                            IF IO3RESETNegIn /= 'Z' AND IO3RESETNegIn /= 'X' AND
                            WPNegIn /= 'Z' AND WPNegIn /= 'X' AND
                            SOIn /= 'Z' AND SOIn /= 'X' AND
                            SIIn /= 'Z' AND SIIn /= 'X' THEN
                                quad_nybble := IO3RESETNegIn & WPNegIn & SOIn & SIIn;
                            END IF;
                            IF data_cnt < 17 THEN --???
                            --In case of quad mode,if more than PageSize+1 bytes
                            --are sent to the device previously latched data
                            --are discarded and last 256/512 data bytes are
                            --guaranteed to be programmed correctly within
                            --the same page.
                                quad_data_in(data_cnt) :=
                                to_nat(quad_nybble);
                                data_cnt := data_cnt +1;
                            END IF;

                        ELSIF QPI_IT = '1' THEN
                            IF IO3RESETNegIn /= 'Z' AND IO3RESETNegIn /= 'X' AND
                            WPNegIn /= 'Z' AND WPNegIn /= 'X' AND
                            SOIn /= 'Z' AND SOIn /= 'X' AND
                            SIIn /= 'Z' AND SIIn /= 'X' THEN
                                quad_nybble := IO3RESETNegIn & WPNegIn & SOIn & SIIn;
                            END IF;
                            IF data_cnt > ((PageSize+1)*2-1) THEN
                            --In case of quad mode,if more than PageSize+1 bytes
                            --are sent to the device previously latched data
                            --are discarded and last 256/512 data bytes are
                            --guaranteed to be programmed correctly within
                            --the same page.
                                FOR I IN 0 TO (PageSize*2) LOOP
                                    quad_data_in(i) := quad_data_in(i+1);
                                END LOOP;
                                quad_data_in((PageSize+1)*2-1) :=
                                                    to_nat(quad_nybble);
                                data_cnt := data_cnt +1;
                            ELSE
                                quad_data_in(data_cnt) :=
                                to_nat(quad_nybble);
                                data_cnt := data_cnt +1;
                            END IF;
                        ELSE  -- not QPI_IT
                            IF data_cnt > ((PageSize+1)*8)-1 THEN
                            --In case of serial mode and PRPGE_C_1,
                            -- if more than 512 bytes are sent to the device
                            -- previously latched data are discarded and last
                            -- 512 data bytes are guaranteed to be programmed
                            -- correctly within the same page.
                                IF bit_cnt = 0 THEN
                                    FOR I IN 0 TO (PageSize*BYTE - 1) LOOP
                                        Data_in(i) := Data_in(i+8);
                                    END LOOP;
                                END IF;
                                Data_in(PageSize*BYTE + bit_cnt) := SIIn;
                                bit_cnt := bit_cnt + 1;
                                IF bit_cnt = 8 THEN
                                    bit_cnt := 0;
                                END IF;
                                data_cnt := data_cnt + 1;
                            ELSE
                                IF SIIn/='Z' AND SIIn/='X' THEN
                                    Data_in(data_cnt) := SIIn;
                                END IF;
                                data_cnt := data_cnt + 1;
                                bit_cnt := 0;
                            END IF;
                        END IF;
                    END IF;


                    IF falling_edge(SCK_ipd) AND CSNeg_ipd = '0' THEN
                        IF ((Instruct = RDAY7_C_0 OR Instruct = RDAY7_4_0 OR
                            Instruct = RDAY5_C_0 OR Instruct = RDAY5_4_0 OR
                            Instruct = RDAY4_C_0 OR Instruct = RDAY4_4_0) AND QUADIT = '1') OR 
                            Instruct = RDAY1_C_0 OR Instruct = RDAY1_4_0 OR
                            Instruct = RDAY2_C_0 OR Instruct = RDAY2_4_0 OR 
                            Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR
                            Instruct = RDCR1_0_0 OR Instruct = RDUID_0_0 OR 
                            Instruct = RDSSR_C_0 OR Instruct = RDAY3_C_0 OR 
                            Instruct = RDAY3_4_0 OR Instruct = RDIDN_0_0 OR 
                            Instruct = RDQID_0_0 OR Instruct = RDPPB_C_0 OR 
                            Instruct = RDPPB_4_0 OR Instruct = RDDYB_C_0 OR 
                            Instruct = RDDYB_4_0 OR Instruct = RDECC_C_0 OR 
                            Instruct = RDECC_4_0 OR Instruct = RDDLP_0_0 OR 
                            Instruct = RDPLB_0_0 OR Instruct = RSFDP_3_0 OR 
                            Instruct = RDARG_C_0 OR Instruct = PKRD1_4_0 OR
                            Instruct = PKRD2_4_0 OR Instruct = PKRD3_4_0 OR
                            Instruct = PKRD4_4_0 OR Instruct = RDHL0_0_0 OR
                            Instruct = RDHL1_0_0 THEN
                                read_out <= '1', '0' AFTER 1 ns;

                        ELSIF (QPI_IT = '1' AND Instruct = PKWR4_4_1) THEN
                            quad_nybble := IO3RESETNegIn & WPNegIn & SOIn & SIIn;

                            IF data_cnt > ((CryptoPacketSize+1)*2-1) THEN
                            --In case of quad mode,if more than CryptoPacketSize+1 bytes
                            --are sent to the device previously latched data
                            --are discarded and last 840 data bytes are
                            --guaranteed to be programmed correctly within
                            --the same page.
                                FOR I IN 0 TO (CryptoPacketSize*2) LOOP
                                    quad_data_in(i) := quad_data_in(i+1);
                                END LOOP;
                                quad_data_in((CryptoPacketSize+1)*2-1) :=
                                                    to_nat(quad_nybble);
                                data_cnt := data_cnt +1;
                            ELSE
                                quad_data_in(data_cnt) :=
                                to_nat(quad_nybble);
                                data_cnt := data_cnt +1;
                            END IF;

                        ELSIF Instruct = DICHK_4_1 THEN
                            DIC_End_Addr_reg := Address;
                        END IF;
                    END IF;
 

                    IF rising_edge(CSNeg_ipd) THEN
                         IF (mode_bytes(7 downto 4) = "1010" AND
                        (Instruct = RDAY3_C_0 OR Instruct = RDAY3_4_0 OR Instruct = PKRD3_4_0 OR
                         Instruct = RDAY5_C_0 OR Instruct = RDAY5_4_0 OR Instruct = RDAY2_4_0)) OR 
                        ((mode_bytes(7 downto 0) = "10100101") AND
                         (Instruct = RDAY7_C_0 OR Instruct = RDAY7_4_0 OR Instruct = PKRD4_4_0)) THEN
                            bus_cycle_state := ADDRESS_BYTES;
                        ELSE
                            bus_cycle_state := STAND_BY;
                        END IF;
                        CASE Instruct IS
                            WHEN WRENB_0_0 | WRDIS_0_0 | ERCHP_0_0 | ER256_C_0 | ER256_4_0 | ER004_C_0 | ER004_4_0 | ENDPD_0_0 |
                            CLPEF_0_0 | SRSTE_0_0 | SFRST_0_0 | SFRSL_0_0 | EN4BA_0_0 | EX4BA_0_0 | PRPPB_C_0 | ERPPB_0_0 | SEERC_C_0 |
                            PRPPB_4_0 | WRPLB_0_0 | SPEPA_0_0 | RSEPA_0_0 | EVERS_C_0 | SPEPD_0_0 | RSEPD_0_0 | DICHK_4_1 |
                            WRENV_0_0 | ENCTM_0_0 | EXCTM_0_0 =>
                                IF data_cnt = 0 THEN
                                    write <= '0';
                                END IF;

                            WHEN WRREG_0_1 =>
                                IF QPI_IT = '0' THEN
                                    IF data_cnt = 8 THEN
                                    --If CS# is driven high after eight
                                    --cycles,only the Status Register is
                                    --written to.
                                        write <= '0';
                                        FOR i IN 0 TO 7 LOOP
                                            SR1_in(i) <= Data_in(7-i);
                                        END LOOP;
                                    ELSIF data_cnt = 16 THEN
                                    --After the 16th cycle both the
                                    --Status and Configuration Registers
                                    --are written to.
                                        write <= '0';
                                        cfg_write1 <= '1','0' AFTER wob1;
                                        FOR i IN 0 TO 7 LOOP
                                            SR1_in(i) <= Data_in(7-i);
                                            CR1_in(i) <= Data_in(15-i);
                                        END LOOP;
                                    ELSIF data_cnt = 24 THEN
                                    --After the 16th cycle both the
                                    --Status and Configuration Registers
                                    --are written to.
                                        write <= '0';
                                        cfg_write1 <= '1','0' AFTER wob1;
                                        cfg_write2 <= '1','0' AFTER wob1;
                                        FOR i IN 0 TO 7 LOOP
                                            SR1_in(i) <= Data_in(7-i);
                                            CR1_in(i) <= Data_in(15-i);
                                            CR2_in(i) <= Data_in(23-i);
                                        END LOOP;
                                    ELSIF data_cnt = 32 THEN
                                    --After the 16th cycle both the
                                    --Status and Configuration Registers
                                    --are written to.
                                        write <= '0';
                                        cfg_write1 <= '1','0' AFTER wob1;
                                        cfg_write2 <= '1','0' AFTER wob1;
                                        cfg_write3 <= '1','0' AFTER wob1;
                                        FOR i IN 0 TO 7 LOOP
                                            SR1_in(i) <= Data_in(7-i);
                                            CR1_in(i) <= Data_in(15-i);
                                            CR2_in(i) <= Data_in(23-i);
                                            CR3_in(i) <= Data_in(31-i);
                                        END LOOP;
                                    ELSIF data_cnt = 32 THEN
                                    --After the 16th cycle both the
                                    --Status and Configuration Registers
                                    --are written to.
                                        write <= '0';
                                        cfg_write1 <= '1','0' AFTER wob1;
                                        cfg_write2 <= '1','0' AFTER wob1;
                                        cfg_write3 <= '1','0' AFTER wob1;
                                        cfg_write4 <= '1';
                                        FOR i IN 0 TO 7 LOOP
                                            SR1_in(i) <= Data_in(7-i);
                                            CR1_in(i) <= Data_in(15-i);
                                            CR2_in(i) <= Data_in(23-i);
                                            CR3_in(i) <= Data_in(31-i);
                                            CR4_in(i) <= Data_in(39-i);
                                        END LOOP; 
                                    END IF;
                                ELSE
                                    IF data_cnt = 2 THEN
                                    --If CS# is driven high after eight
                                    --cycles,only the Status Register is
                                    --written to.
                                        write <= '0';
                                        FOR i IN 1 DOWNTO 0 LOOP
                                            Quad_slv := to_slv(quad_data_in(1-i), 4);
                                            IF i = 1 THEN
                                                SR1_in(7 downto 4)
                                                        <= Quad_slv;
                                            ELSIF i = 0 THEN
                                                SR1_in(3 downto 0)
                                                        <= Quad_slv;
                                            END IF;
                                        END LOOP;
                                    ELSIF data_cnt = 4 THEN
                                    --After the 16th cycle both the
                                    --Status and Configuration Registers
                                    --are written to.
                                        write <= '0';
                                        cfg_write1 <= '1','0' AFTER wob1;
                                        FOR i IN 3 DOWNTO 0 LOOP
                                            Quad_slv := to_slv(quad_data_in(3-i), 4);
                                            IF i = 3 THEN
                                                SR1_in(7 downto 4)
                                                        <= Quad_slv;
                                            ELSIF i = 2 THEN
                                                SR1_in(3 downto 0)
                                                    <= Quad_slv;
                                            ELSIF i = 1 THEN
                                                CR1_in(7 downto 4)
                                                    <= Quad_slv;
                                            ELSIF i = 0 THEN
                                                CR1_in(3 downto 0)
                                                    <= Quad_slv;
                                            END IF;
                                        END LOOP;
                                    ELSIF data_cnt = 6 THEN
                                    --After the 24th cycle both the
                                    --Status and Configuration Registers
                                    --are written to.
                                        write <= '0';
                                        cfg_write1 <= '1','0' AFTER wob1;
                                        cfg_write2 <= '1','0' AFTER wob1;
                                        FOR i IN 5 DOWNTO 0 LOOP
                                            Quad_slv := to_slv(quad_data_in(5-i), 4);
                                            IF i = 5 THEN
                                                SR1_in(7 downto 4)
                                                        <= Quad_slv;
                                            ELSIF i = 4 THEN
                                                SR1_in(3 downto 0)
                                                    <= Quad_slv;
                                            ELSIF i = 3 THEN
                                                CR1_in(7 downto 4)
                                                    <= Quad_slv;
                                            ELSIF i = 2 THEN
                                                CR1_in(3 downto 0)
                                                    <= Quad_slv;
                                            ELSIF i = 1 THEN
                                                CR2_in(7 downto 4)
                                                    <= Quad_slv;
                                            ELSIF i = 0 THEN
                                                CR2_in(3 downto 0)
                                                    <= Quad_slv;
                                            END IF;
                                        END LOOP;
                                    ELSIF data_cnt = 8 THEN
                                    --After the 32th cycle both the
                                    --Status and Configuration Registers
                                    --are written to.
                                        write <= '0';
                                        cfg_write1 <= '1','0' AFTER wob1;
                                        cfg_write2 <= '1','0' AFTER wob1;
                                        cfg_write3 <= '1','0' AFTER wob1;
                                        FOR i IN 7 DOWNTO 0 LOOP
                                            Quad_slv := to_slv(quad_data_in(7-i), 4);
                                            IF i = 7 THEN
                                                SR1_in(7 downto 4)
                                                        <= Quad_slv;
                                            ELSIF i = 6 THEN
                                                SR1_in(3 downto 0)
                                                    <= Quad_slv;
                                            ELSIF i = 5 THEN
                                                CR1_in(7 downto 4)
                                                    <= Quad_slv;
                                            ELSIF i = 4 THEN
                                                CR1_in(3 downto 0)
                                                    <= Quad_slv;
                                            ELSIF i = 3 THEN
                                                CR2_in(7 downto 4)
                                                    <= Quad_slv;
                                            ELSIF i = 2 THEN
                                                CR2_in(3 downto 0)
                                                    <= Quad_slv;
                                            ELSIF i = 1 THEN
                                                CR3_in(7 downto 4)
                                                    <= Quad_slv;
                                            ELSIF i = 0 THEN
                                                CR3_in(3 downto 0)
                                                    <= Quad_slv;
                                            END IF;
                                        END LOOP;
                                    ELSIF data_cnt = 10 THEN
                                    --After the 40th cycle both the
                                    --Status and Configuration Registers
                                    --are written to.
                                        write <= '0';
                                        cfg_write1 <= '1','0' AFTER wob1;
                                        cfg_write2 <= '1','0' AFTER wob1;
                                        cfg_write3 <= '1','0' AFTER wob1;
--                                         cfg_write4 <= '1';
                                        
                                        cfg_write4 <= '1','0' AFTER wob1;
                                        FOR i IN 9 DOWNTO 0 LOOP
                                            Quad_slv := to_slv(quad_data_in(9-i), 4);
                                            IF i = 9 THEN
                                                SR1_in(7 downto 4)
                                                        <= Quad_slv;
                                            ELSIF i = 8 THEN
                                                SR1_in(3 downto 0)
                                                    <= Quad_slv;
                                            ELSIF i = 7 THEN
                                                CR1_in(7 downto 4)
                                                    <= Quad_slv;
                                            ELSIF i = 6 THEN
                                                CR1_in(3 downto 0)
                                                    <= Quad_slv;
                                            ELSIF i = 5 THEN
                                                CR2_in(7 downto 4)
                                                    <= Quad_slv;
                                            ELSIF i = 4 THEN
                                                CR2_in(3 downto 0)
                                                    <= Quad_slv;
                                            ELSIF i = 3 THEN
                                                CR3_in(7 downto 4)
                                                    <= Quad_slv;
                                            ELSIF i = 2 THEN
                                                CR3_in(3 downto 0)
                                                    <= Quad_slv;
                                            ELSIF i = 1 THEN
                                                CR4_in(7 downto 4)
                                                    <= Quad_slv;
                                            ELSIF i = 0 THEN
                                                CR4_in(3 downto 0)
                                                    <= Quad_slv;
                                            END IF;
                                        END LOOP;
                                    END IF;
                                END IF;

                            WHEN WRARG_C_1 =>
                                IF QPI_IT = '0' THEN
                                    IF data_cnt = 8 THEN
                                        write <= '0';
                                        FOR i IN 0 TO 7 LOOP
                                            WRAR_reg_in(i) := Data_in(7-i);
                                        END LOOP;
                                    END IF;
                                ELSE
                                    IF data_cnt = 2 THEN
                                        write <= '0';
                                        FOR i IN 1 DOWNTO 0 LOOP
                                            Quad_slv := to_slv(quad_data_in(1-i), 4);
                                            IF i = 1 THEN
                                                WRAR_reg_in(7 downto 4)
                                                        := Quad_slv;
                                            ELSIF i = 0 THEN
                                                WRAR_reg_in(3 downto 0)
                                                        := Quad_slv;
                                            END IF;
                                        END LOOP;
                                    END IF;
                                END IF;

                            WHEN PRPGE_C_1 | PRPGE_4_1 | PRSSR_C_1 =>
                                IF QPI_IT = '0' THEN
                                    IF data_cnt > 0 THEN
                                        IF (data_cnt mod 8) = 0 THEN
                                            write <= '0';
                                            FOR I IN 0 TO PageSize LOOP
                                                FOR J IN 7 DOWNTO 0 LOOP
                                                    IF (Data_in((i*8) + (7-j))
                                                                /= 'X') AND
                                                    (Data_in((i*8) + (7-j))
                                                                /= 'Z') THEN
                                                                                                                                                    
                                                        Byte_slv(j) :=
                                                        Data_in((i*8) + (7-j));
                                                        IF Data_in((i*8) + (7-j))
                                                            = '0' THEN
                                                            ZERO_DETECTED := '1';
                                                        END IF;
                                                    END IF;
                                                END LOOP;
                                                WByte(i) <= to_nat(Byte_slv);
                                            END LOOP;
                                            IF data_cnt > (PageSize+1)*BYTE THEN
                                                Byte_number <= PageSize;
                                            ELSE
                                                Byte_number <= data_cnt/8-1;
                                            END IF;
                                        END IF;
                                    END IF;
                                ELSE
                                    IF data_cnt > 0 THEN
                                        IF (data_cnt mod 2) = 0 THEN
                                            write <= '0';
                                            FOR i IN 0 TO PageSize LOOP
                                                FOR j IN 1 DOWNTO 0 LOOP
                                                Quad_int := quad_data_in((i*2)+(1-j));
                                                Quad_slv := to_slv(Quad_int, 4);
                                                IF j=1 THEN
                                                    Byte_slv(7 downto 4):= Quad_slv;
                                                ELSIF j=0 THEN
                                                    Byte_slv(3 downto 0):= Quad_slv;
                                                END IF; 
                                                END LOOP;
                                                WByte(i) <= to_nat(Byte_slv);
                                            END LOOP;
                                            IF data_cnt > (PageSize+1)*2 THEN
                                                Byte_number <= PageSize;
                                            ELSE
                                                Byte_number <= data_cnt/2-1;
                                            END IF;
                                        END IF;
                                    END IF;
                                END IF;

                            WHEN WRAUB_0_1 =>
                                IF QPI_IT = '0' THEN
                                    IF data_cnt = 32 THEN
                                        write <= '0';
                                        FOR J IN 0 TO 31 LOOP
                                            ATBN_in(J) <= Data_in(31-J);
                                        END LOOP;
                                    END IF;
                                ELSE
                                    IF data_cnt = 8 THEN
                                        write <= '0';
                                        FOR J IN 7 DOWNTO 0 LOOP
                                            Quad_slv := to_slv(quad_data_in(7-J), 4);
                                            ATBN_in(4*J+3 downto 4*J)
                                                    <= Quad_slv;
                                        END LOOP;
                                    END IF;
                                END IF;

                            WHEN PRASP_0_1 =>
                                IF QPI_IT = '0' THEN
                                    IF data_cnt = 16 THEN
                                        write <= '0';
                                        FOR J IN 0 TO 15 LOOP
                                            ASPO_in(J) <=
                                                    Data_in(15-J);
                                        END LOOP;
                                    END IF;
                                ELSE
                                    IF data_cnt = 4 THEN
                                        write <= '0';
                                        FOR J IN 3 DOWNTO 0 LOOP
                                            Quad_slv := to_slv(quad_data_in(3-J), 4);
                                            IF J = 3 THEN
                                                ASPO_in(7 downto 4)
                                                        <= Quad_slv;
                                            ELSIF J = 2 THEN
                                                ASPO_in(3 downto 0)
                                                        <= Quad_slv;
                                            ELSIF J = 1 THEN
                                                ASPO_in(15 downto 12)
                                                        <= Quad_slv;
                                            ELSIF J = 0 THEN
                                                ASPO_in(11 downto 8)
                                                        <= Quad_slv;
                                            END IF;
                                        END LOOP;
                                    END IF;
                                END IF;

                            WHEN PRDLP_0_1 =>
                                IF QPI_IT ='0' THEN
                                    IF data_cnt = 8 THEN
                                        write <= '0';
                                        FOR J IN 0 TO 7 LOOP
                                            DLPN_in(J) <=
                                                Data_in(7-J);
                                        END LOOP;
                                    END IF;
                                ELSE
                                    IF data_cnt = 2 THEN
                                        write <= '0';
                                        FOR J IN 1 DOWNTO 0 LOOP
                                            Quad_slv :=
                                            to_slv(quad_data_in(1-J),4);
                                            IF J = 1 THEN
                                                DLPN_in(7 downto 4)
                                                            <= Quad_slv;
                                            ELSIF J = 0 THEN
                                                DLPN_in(3 downto 0)
                                                            <= Quad_slv;
                                            END IF;
                                        END LOOP;
                                    END IF;
                                END IF;

                            WHEN WRDLP_0_1 =>
                                IF QPI_IT ='0' THEN
                                    IF data_cnt = 8 THEN
                                        write <= '0';
                                        FOR J IN 0 TO 7 LOOP
                                            DLPV_in(J) <=
                                                Data_in(7-J);
                                        END LOOP;
                                    END IF;
                                ELSE
                                    IF data_cnt = 2 THEN
                                        write <= '0';
                                        FOR J IN 1 DOWNTO 0 LOOP
                                            Quad_slv :=
                                            to_slv(quad_data_in(1-J),4);
                                            IF J = 1 THEN
                                                DLPV_in(7 downto 4)
                                                            <= Quad_slv;
                                            ELSIF J = 0 THEN
                                                DLPV_in(3 downto 0)
                                                            <= Quad_slv;
                                            END IF;
                                        END LOOP;
                                    END IF;
                                END IF;

                            WHEN WRDYB_C_1 | WRDYB_4_1 =>
                                IF QPI_IT ='0' THEN
                                    IF data_cnt = 8 THEN
                                        write <= '0';
                                        FOR J IN 0 TO 7 LOOP
                                            DYAV_in(J) <=
                                                Data_in(7-J);
                                        END LOOP;
                                    END IF;
                                ELSE
                                    IF data_cnt = 2 THEN
                                        write <= '0';
                                        FOR J IN 1 DOWNTO 0 LOOP
                                            Quad_slv :=
                                            to_slv(quad_data_in(1-J),4);
                                            IF J = 1 THEN
                                                DYAV_in(7 downto 4)
                                                            <= Quad_slv;
                                            ELSIF J = 0 THEN
                                                DYAV_in(3 downto 0)
                                                            <= Quad_slv;
                                            END IF;
                                        END LOOP;
                                    END IF;
                                END IF;

                            WHEN PGPWD_0_1 =>
                                IF QPI_IT = '0' THEN
                                    IF data_cnt = 64 THEN
                                        write <= '0';
                                        FOR J IN 1 TO 8 LOOP
                                            FOR K IN 1 TO 8 LOOP
                                                PWDO_in(J*8-K) <=
                                                    Data_in(8*(J-1)+K-1);
                                            END LOOP;
                                        END LOOP;
                                    END IF;
                                ELSE
                                    IF data_cnt = 16 THEN
                                        write <= '0';
                                        FOR J IN 15 DOWNTO 0 LOOP
                                            Quad_slv := to_slv(quad_data_in(15-J), 4);
                                            PWDO_in(4*J+3 downto 4*J)
                                                    <= Quad_slv;
                                        END LOOP;
                                    END IF;
                                END IF;

                            WHEN PWDUL_0_1 =>
                                IF QPI_IT = '0' THEN
                                    IF data_cnt = 64 THEN
                                        write <= '0';
                                        FOR J IN 1 TO 8 LOOP
                                            FOR K IN 1 TO 8 LOOP
                                                PASS_TEMP(J*8-K) <=
                                                    Data_in(8*(J-1)+K-1);
                                            END LOOP;
                                        END LOOP;
                                    END IF;
                                ELSE
                                    IF data_cnt = 16 THEN
                                        write <= '0';
                                        FOR J IN 0 TO 7 LOOP
                                          FOR I IN 1 DOWNTO 0 LOOP
                                            Quad_slv := to_slv(quad_data_in(2*(J) + 1 - I), 4);
                                            PASS_TEMP((8*(J)+4*I+3) downto (8*(J)+4*I))
                                                    <= Quad_slv;
                                            END LOOP;
                                        END LOOP;
                                    END IF;
                                END IF;
                                
                            WHEN PKWR1_4_1 =>
                                IF QPI_IT = '0' THEN
                                    IF data_cnt > 0 THEN
                                        IF (data_cnt mod 8) = 0 THEN
                                            write <= '0';
                                            FOR I IN 0 TO CryptoPacketSize LOOP
                                                FOR J IN 7 DOWNTO 0 LOOP
                                                    IF (Data_in((i*8) + (7-j))
                                                                /= 'X') AND
                                                    (Data_in((i*8) + (7-j))
                                                                /= 'Z') THEN
                                                                                                                                                    
                                                        Byte_slv(j) :=
                                                        Data_in((i*8) + (7-j));
                                                    END IF;
                                                END LOOP;
                                                WByteCrypto(i) <= to_nat(Byte_slv);
                                            END LOOP;
                                        END IF;
                                    END IF;
                                ELSE
                                    IF data_cnt > 0 THEN
                                        IF (data_cnt mod 2) = 0 THEN
                                            write <= '0';
                                            FOR i IN 0 TO CryptoPacketSize LOOP
                                                FOR j IN 1 DOWNTO 0 LOOP
                                                Quad_int := quad_data_in((i*2)+(1-j));
                                                Quad_slv := to_slv(Quad_int, 4);
                                                IF j=1 THEN
                                                    Byte_slv(7 downto 4):= Quad_slv;
                                                ELSIF j=0 THEN
                                                    Byte_slv(3 downto 0):= Quad_slv;
                                                END IF; 
                                                END LOOP;
                                                WByteCrypto(i) <= to_nat(Byte_slv);
                                            END LOOP;
                                            IF data_cnt > (CryptoPacketSize+1)*2 THEN
                                                Byte_number <= CryptoPacketSize;
                                            ELSE
                                                Byte_number <= data_cnt/2-1;
                                            END IF;
                                        END IF;
                                    END IF;
                                END IF;    

                            WHEN PKWR2_4_1 |
                                 PKWR3_4_1 |
                                 PKWR4_4_1 =>

                                IF data_cnt > 0 THEN
                                    IF (data_cnt mod 2) = 0 THEN
                                        write <= '0';
                                        FOR i IN 0 TO CryptoPacketSize LOOP
                                            FOR j IN 1 DOWNTO 0 LOOP
                                            Quad_int := quad_data_in((i*2)+(1-j));
                                            Quad_slv := to_slv(Quad_int, 4);
                                            IF j=1 THEN
                                                Byte_slv(7 downto 4):= Quad_slv;
                                            ELSIF j=0 THEN
                                                Byte_slv(3 downto 0):= Quad_slv;
                                            END IF; 
                                            END LOOP;
                                            WByteCrypto(i) <= to_nat(Byte_slv);
                                        END LOOP;
                                        IF data_cnt > (CryptoPacketSize+1)*2 THEN
                                            Byte_number <= CryptoPacketSize;
                                        ELSE
                                            Byte_number <= data_cnt/2-1;
                                        END IF;
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
    -- Timing control for the Page Program
    ---------------------------------------------------------------------------
    ProgTime : PROCESS(PSTART, PGSUSP, PGRES, reseted)
        VARIABLE pob      : time;
        VARIABLE elapsed_pgm  : time;
        VARIABLE start_pgm    : time;
        VARIABLE duration_pgm : time;
    BEGIN
        IF LongTimming THEN
            IF CFR3V(4) = '0' THEN
                pob  := tdevice_PP_256;
            ELSE
                pob  := tdevice_PP_512;
            END IF;
        ELSE
            IF CFR3V(4) = '0' THEN
                pob  := tdevice_PP_256/10;
            ELSE
                pob  := tdevice_PP_512/10;
            END IF;
        END IF;

        IF rising_edge(reseted) THEN
            PDONE <= '1';  -- reset done, programing terminated
        ELSIF reseted = '1' THEN
            IF rising_edge(PSTART) AND PDONE = '1' THEN
                elapsed_pgm := 0 ns;
                start_pgm := NOW;
                PDONE <= '0', '1' AFTER pob;
            ELSIF PGSUSP'EVENT AND PGSUSP = '1' AND PDONE /= '1' THEN
                elapsed_pgm  := NOW - start_pgm;
                duration_pgm := pob - elapsed_pgm;
                PDONE <= '0';
            ELSIF PGRES'EVENT AND PGRES = '1' THEN
                start_pgm := NOW;
                PDONE <= '0', '1' AFTER duration_pgm;
            END IF;
        END IF;

    END PROCESS ProgTime;

    ---------------------------------------------------------------------------
    -- Timing control for the Write Status Register
    ---------------------------------------------------------------------------
    WriteTime : PROCESS(WSTART, reseted)
        
    BEGIN
        IF LongTimming THEN
            wob  := tdevice_WRR;
        ELSE
            wob  := tdevice_WRR / 1000;
        END IF;
        wob1 := wob + 1 ns;
        IF rising_edge(reseted) THEN
            WDONE <= '1';  -- reset done, programing terminated
        ELSIF reseted = '1' THEN
            IF rising_edge(WSTART) AND WDONE = '1' THEN
                WDONE <= '0', '1' AFTER wob;
            END IF;
        END IF;

    END PROCESS WriteTime;

    ---------------------------------------------------------------------------
    -- Timing control for the Write volatile registers bits
    ---------------------------------------------------------------------------
    WriteVolatileBitsTime : PROCESS(CSSTART, reseted)
    BEGIN
        IF rising_edge(reseted) THEN
            CSDONE <= '1';  -- reset done, programing terminated
        ELSIF reseted = '1' THEN
            IF rising_edge(CSSTART) AND CSDONE = '1' THEN
                CSDONE <= '0', '1' AFTER 50 ns;
            END IF;
        END IF;

    END PROCESS WriteVolatileBitsTime;

    ---------------------------------------------------------------------------
    -- Timing control for Evaluate Erase Status
    ---------------------------------------------------------------------------
    EESTime : PROCESS(EESSTART, reseted)
        VARIABLE ees_time      : time;
    BEGIN
        IF LongTimming THEN
            ees_time  := tdevice_EES;
        ELSE
            ees_time  := tdevice_EES / 10;
        END IF;
        IF rising_edge(reseted) THEN
            EESDONE <= '1';  -- reset done, write terminated
        ELSIF reseted = '1' THEN
            IF rising_edge(EESSTART) AND EESDONE = '1' THEN
                EESDONE <= '0', '1' AFTER ees_time;
            END IF;
        END IF;

    END PROCESS EESTime;

    ---------------------------------------------------------------------------
    -- Timing control for block erase operation
    ---------------------------------------------------------------------------
    ErsTime : PROCESS(ESTART, ESUSP, ERES, reseted, SEERC_START)
        VARIABLE seo4     : time;
        VARIABLE seo256   : time;
        VARIABLE secc   : time;
        VARIABLE beo      : time;
        VARIABLE elapsed_ers  : time;
        VARIABLE start_ers    : time;
        VARIABLE duration_ers : time;
    BEGIN
        IF LongTimming THEN
            seo4 := tdevice_SE4;
            seo256 := tdevice_SE256;
            beo := tdevice_BE;
            secc := tdevice_SEERC;
        ELSE
            seo4 := tdevice_SE4 / 100;
            seo256 := tdevice_SE256 / 100;
            beo := tdevice_BE / 1000;
            secc := tdevice_SEERC / 10;
        END IF;

        IF Instruct = ERCHP_0_0 THEN
            duration_ers := beo;
        ELSIF Instruct = ER004_C_0 OR Instruct = ER004_4_0 THEN
            duration_ers := seo4;
        ELSE
            duration_ers := seo256;
        END IF;
        
        IF rising_edge(reseted) THEN
            SEERC_DONE <= '1';
        ELSIF reseted = '1' THEN
            IF rising_edge(SEERC_START) AND SEERC_DONE = '1' THEN
                SEERC_DONE <= '0', '1' AFTER secc;
            END IF;
-- --         ELSIF ESUSP'EVENT AND ESUSP = '1' AND EDONE /= '1' THEN
-- --             elapsed_ers  := NOW - start_ers;
-- --             duration_ers := duration_ers - elapsed_ers;
-- --             EDONE <= '0';
-- --         ELSIF ERES'EVENT AND ERES = '1' THEN
-- --             start_ers := NOW;
-- --             EDONE <= '0', '1' AFTER duration_ers;
        END IF;

        IF rising_edge(reseted) THEN
            EDONE <= '1';
        ELSIF reseted = '1' THEN
            IF rising_edge(ESTART) AND EDONE = '1' THEN
                elapsed_ers := 0 ns;
                EDONE <= '0', '1' AFTER duration_ers;
                start_ers := NOW;
            END IF;
        ELSIF ESUSP'EVENT AND ESUSP = '1' AND EDONE /= '1' THEN
            elapsed_ers  := NOW - start_ers;
            duration_ers := duration_ers - elapsed_ers;
            EDONE <= '0';
        ELSIF ERES'EVENT AND ERES = '1' THEN
            start_ers := NOW;
            EDONE <= '0', '1' AFTER duration_ers;
        END IF;

    END PROCESS ErsTime;

    SuspTime : PROCESS(ERSSUSP_in,PRGSUSP_in)
        VARIABLE susp_time      : time;
    BEGIN
        IF LongTimming THEN
            susp_time  := tdevice_PSUSP;
        ELSE
            susp_time  := tdevice_PSUSP / 10;
        END IF;

        IF rising_edge(ERSSUSP_in) THEN
            ERSSUSP_out <= '0', '1' after susp_time;
        ELSIF falling_edge(ERSSUSP_in) THEN
            ERSSUSP_out <= '0';
        END IF;

        IF rising_edge(PRGSUSP_in) THEN
            PRGSUSP_out <= '0', '1' after susp_time;
        ELSIF falling_edge(ERSSUSP_in) THEN
            PRGSUSP_out <= '0';
        END IF;
    END PROCESS SuspTime;

    ---------------------------------------------------------------------------
    -- Timing control for the suspend process
    ---------------------------------------------------------------------------
    Start_T1_time : PROCESS (START_T1_in)
    BEGIN
        IF rising_edge(START_T1_in) THEN
            IF DIC_ACT = '1' THEN
                sSTART_T1 <= '0', '1' AFTER tdevice_DICSL;
            ELSE
                sSTART_T1 <= '0', '1' AFTER tdevice_ESUSP;
            END IF;
        ELSE
            sSTART_T1 <= '0';
        END IF;
    END PROCESS Start_T1_time;

    ---------------------------------------------------------------------------
    -- Timing control for the DIC calculation
    ---------------------------------------------------------------------------
    DICTime : PROCESS (reseted, DICSTART)
        VARIABLE elapsed_dic  : time;
        VARIABLE dic_duration : time;
        VARIABLE start_dic    : time;
    BEGIN
        IF rising_edge(reseted) THEN
            DICDONE <= '1';
        ELSIF reseted = '1' THEN
            IF rising_edge(DICSTART) AND DICDONE = '1' THEN
                dic_duration := tdevice_DICSETUP;
                elapsed_dic := 0 ns;
                DICDONE <= '0' , '1' AFTER dic_duration;
                start_dic := NOW;
            ELSIF rising_edge(DICSUSP) AND DICDONE = '0' THEN
                elapsed_dic  := NOW - start_dic;
                dic_duration := dic_duration - elapsed_dic;
                DICDONE <= '0';
            ELSIF rising_edge(DICRES) AND DICDONE = '0' THEN
                start_dic := NOW;
                DICDONE <= '0', '1' AFTER dic_duration;
            END IF;
        END IF;
    END PROCESS DICTime;

    PPBEraseTime : PROCESS(PPBERASE_in)
        VARIABLE ppbe_time      : time;
    BEGIN
        IF LongTimming THEN
            ppbe_time  := tdevice_SE256;
        ELSE
            ppbe_time  := tdevice_SE256 / 100;
        END IF;

        IF rising_edge(PPBERASE_in) THEN
            PPBERASE_out <= '0', '1' after ppbe_time;
        ELSIF falling_edge(PPBERASE_in) THEN
            PPBERASE_out <= '0';
        END IF;
    END PROCESS PPBEraseTime;

    PassUlckTime : PROCESS(PASSULCK_in)
        VARIABLE passulck_time      : time;
    BEGIN
        IF LongTimming THEN
            passulck_time := tdevice_PP_256;
        ELSE
            passulck_time := tdevice_PP_256 / 10;
        END IF;

        IF rising_edge(PASSULCK_in) THEN
            PASSULCK_out <= '0', '1' after passulck_time;
        ELSIF falling_edge(PASSULCK_in) THEN
            PASSULCK_out <= '0';
        END IF;
    END PROCESS PassUlckTime;

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

    StateGen :PROCESS(PoweredUp, write, WDONE, PDONE, EDONE,  RST_out, CSDONE,
                      BCDONE, EESDONE, ERSSUSP_out, PRGSUSP_out, PPBERASE_in,
                      DICDONE, sSTART_T1, PASSULCK_in, SWRST_out, RESETNeg, 
                      SEERC_DONE, DPDExt_out,
                      DPDEnt_out)

    VARIABLE sect          : NATURAL RANGE 0 TO SecNumHyb;
    VARIABLE SectorErased  : NATURAL RANGE 0 TO SecNumHyb;

    BEGIN

        IF rising_edge(PoweredUp) AND SWRST_out = '1' AND RST_out = '1' THEN
            WRONG_PASS := 0;
            IF ATBTEN = '1' AND ASPRDP /= '0' THEN
            --Autoboot is enabled and The Read Password feature is not enabled
                next_state <= AUTOBOOT;
                read_cnt    := 0;
                byte_cnt    := 1;
                read_addr   := to_nat(ATBN(31 DOWNTO 9) & "000000000");
                start_delay := to_nat(ATBN(8 DOWNTO 1));
                ABSD        := to_nat(ATBN(8 DOWNTO 1));
                CFR4N(4)    <= '0';
--                 WVREG       <= '0';
            ELSIF (CFR4N(4) = '1' AND SWRST_out = '1' AND RST_out = '1') THEN
                next_state <= DP_DOWN;
            ELSE
                next_state <= IDLE;
            END IF;
        ELSIF PoweredUp = '1' THEN
            IF RST_out= '0' then
                next_state <= current_state;
            ELSIF falling_edge(write) AND Instruct = SFRSL_0_0 THEN
                IF ATBTEN = '1' AND ASPRDP /= '0' THEN
                    read_cnt   := 0;
                    byte_cnt   := 1;
                    read_addr  := to_nat(ATBN(31 DOWNTO 9) & "000000000");
                    start_delay:= to_nat(ATBN(8 DOWNTO 1));
                    ABSD       := to_nat(ATBN(8 DOWNTO 1));
                    next_state <= AUTOBOOT;
                    CFR4N(4)    <= '0';
--                     WVREG       <= '0';
                ELSIF (CFR4N(2) = '1' AND SWRST_out = '1' AND RST_out = '1') THEN
                    next_state <= DP_DOWN;
                ELSIF WRONG_PASS = 0 THEN
                    next_state <= IDLE;
                ELSE
                    next_state <= current_state;
                END IF;
            ELSE
                CASE current_state IS
                    WHEN RESET_STATE =>
                        IF rising_edge(RST_out) OR rising_edge(SWRST_out) THEN
                            IF ATBTEN = '1' AND ASPRDP /= '0'
                            AND RdPswdProtMode = '0' THEN
                                next_state <= AUTOBOOT;
                                read_cnt    := 0;
                                byte_cnt    := 1;
                                read_addr   := to_nat(ATBN(31 DOWNTO 9)&
                                                                "000000000");
                                start_delay := to_nat(ATBN(8 DOWNTO 1));
                                ABSD        := to_nat(ATBN(8 DOWNTO 1));
                                CFR4N(4)    <= '0';
--                                 WVREG       <= '0';
                            ELSIF (CFR4N(2) = '1' AND RST_out = '1') THEN
                                next_state <= DP_DOWN;
                            ELSE
                                next_state <= IDLE;
                            END IF;
                        END IF;

                    WHEN IDLE =>
                        IF falling_edge(write) THEN
                            IF Instruct=WRREG_0_1 AND (WRPGEN='1' OR WVREG='1') AND
                            not(STCFWR='1' AND WPNegIn='0' AND QUAD_QPI='0') THEN
                                    -- can not execute if HPM is entered
                                    -- or if WRPGEN bit is zero
                                IF CR1_in(4)='0' AND CFR1N(4)='1' AND 
                                cfg_write1 = '1' THEN
                                        REPORT "Changing value of Configuration " &
                                            "Register OTP bit from 1 to 0 is " &
                                            "not allowed!!!"
                                        SEVERITY WARNING;
                                END IF;
                                IF (ASPPWD/='1' OR ASPPER/='1') THEN
                            -- Once the protection mode is selected, the OTP
                            -- bits are permanently protected from programming
                                    next_state <= PGERS_ERROR;
                                ELSE
                                    next_state <= WRITE_SR;
                                END IF;
                            ELSIF Instruct=WRARG_C_1 AND (WRPGEN='1' OR WVREG='1') AND
                            not(STCFWR='1' AND WPNegIn='0' AND QUAD_QPI='0' AND
                                (Address=16#00000000# OR
                                Address=16#00000002# OR
                                Address=16#00800000# OR
                                Address=16#00800002#)) THEN
                            -- can not execute if WRPGEN bit is zero or Hardware
                            -- Protection Mode is entered and SR1NV,SR1V,CR1NV or
                            -- CR1V is selected (no error is set)
                                IF Address=16#00000001#  OR
                                ((Address>16#00000005#) AND
                                (Address<16#0000010#)) OR
                                ((Address>16#0000010#) AND
                                (Address<16#0000020#)) OR
                                ((Address>16#0000027#) AND
                                (Address<16#0000030#)) OR
                                ((Address>16#0000031#) AND
                                (Address<16#00800000#)) OR
                                ((Address>16#00800005#) AND
                                (Address<16#0800010#)) OR
                                ((Address>16#0800010#) AND
                                (Address<16#0800040#)) OR
                                ((Address>16#0800045#) AND
                                (Address<16#0800068#)) OR
                                ((Address>16#0800069#) AND
                                (Address<16#0800070#)) OR
                                Address=16#0800078# OR
                                ((Address>16#0800080#) AND
                                (Address<16#0800089#)) OR
                                Address=16#0800094# OR
                                ((Address>16#0800098#) AND
                                (Address<16#080009B#)) OR
                                (Address>16#080009B#) THEN
                                    ASSERT FALSE
                                    REPORT "WARNING: Undefined location " &
                                        "selected. Command is ignored! "
                                    SEVERITY WARNING;
                                ELSIF ((Address>16#0800094#) AND
                                (Address<16#0800099#)) THEN --DICHK_4_1
                                    ASSERT FALSE
                                    REPORT "WARNING: DICHK_4_1 register cannot be " &
                                        "written by the WRAR command. " &
                                        "Command is ignored! "
                                    SEVERITY WARNING;
                                ELSIF Address=16#080009B# THEN --PPLV
                                    ASSERT FALSE
                                    REPORT "WARNING: PPLV register cannot be " &
                                        "written by the WRAR command. " &
                                        "Command is ignored! "
                                    SEVERITY WARNING;
                                ELSIF Address=16#00000002# AND
                                    ((TBPROT_NV='1' AND WRAR_reg_in(5)='0') OR
                                    (TB4KBS_NV='1' AND WRAR_reg_in(2)='0'
                                    AND CFR3V(3)='0') OR
                                    (WRAR_reg_in(3)='0')) THEN
                                ASSERT FALSE
                                    REPORT "WARNING: Writing of OTP bits back " &
                                        "to their default state is ignored " &
                                        "and no error is set! "
                                    SEVERITY WARNING;
                                ELSIF ASPPWD/='1' OR ASPPER/='1' THEN
                                -- Once the protection mode is selected,the OTP
                                -- bits are permanently protected from programming
                                    IF ((WRAR_reg_in(5)='1' OR
                                    (WRAR_reg_in(4)='1') OR
                                    WRAR_reg_in(3)='1' OR
                                    (WRAR_reg_in(2)='1' AND CFR3N(3)='0')) AND
                                    Address =16#00000002#) OR -- CR1NV[5:2]
                                    Address =16#00000003# OR -- CR2NV
                                    Address =16#00000004# OR -- CR3NV
                                    Address =16#00000005# OR -- CR4NV
                                    Address =16#0000010# OR -- NVDLR
                                    Address =16#0000020# OR -- PASS(7:0)
                                    Address =16#0000021# OR -- PASS(15:8)
                                    Address =16#0000022# OR -- PASS(23:16)
                                    Address =16#0000023# OR -- PASS(31:24)
                                    Address =16#0000024# OR -- PASS(39:32)
                                    Address =16#0000025# OR -- PASS(47:40)
                                    Address =16#0000026# OR -- PASS(55:58)
                                    Address =16#0000027# OR -- PASS(63:56)
                                    Address =16#0000030# OR -- ASPR(7:0)
                                    Address =16#0000031# THEN-- ASPR(15:8)
                                        next_state <= PGERS_ERROR;
                                    ELSE
                                        next_state <= WRITE_ALL_REG;
                                    END IF;
                                ELSE -- Protection mode not selected
                                    IF (Address =16#0000030#) OR
                                    (Address =16#0000031#) THEN --ASPR
                                        IF WRAR_reg_in(2)='0' AND
                                        WRAR_reg_in(1)='0'
                                        AND Address =16#0000030# THEN
                                            next_state <= PGERS_ERROR;
                                        ELSE
                                            next_state <= WRITE_ALL_REG;
                                        END IF;
                                    ELSE
                                        next_state <= WRITE_ALL_REG;
                                    END IF;
                                END IF;
                            ELSIF (Instruct=PRPGE_C_1 OR Instruct=PRPGE_4_1) AND WRPGEN='1' THEN
                                sect := ReturnSectorID
                                (Address,BottomBoot,TopBoot);
                                IF Sec_Prot(sect)= '0' AND PPB_bits(sect)= '1' AND
                                    DYB_bits(sect)= '1' THEN
                                    next_state <= PAGE_PG;
                                ELSE
                                    next_state <= PGERS_ERROR;
                                END IF;
                            ELSIF Instruct=PRSSR_C_1 AND WRPGEN = '1' THEN
                                IF (Address + Byte_number) <= OTPHiAddr THEN
                                    -- Program within valid OTP Range
                                    IF ((Address>=16#10# AND Address<=16#FF#)
                                    AND LOCK_BYTE1(Address/32) = '1') OR
                                    ((Address >= 16#100# AND Address<=16#1FF#)
                                    AND LOCK_BYTE2((Address-16#100#)/32) = '1') 
                                    OR ((Address>=16#200# AND Address<=16#2FF#)
                                    AND LOCK_BYTE3((Address-16#200#)/32)='1') 
                                    OR ((Address>=16#300# AND Address<=16#3FF#)
                                    AND LOCK_BYTE4((Address-16#300#)/32)='1') 
                                    THEN
                                        IF TLPROT = '0' THEN
                                            next_state <=  OTP_PG;
                                        ELSE
                                            -- Attempting to program within valid OTP
                                            -- range while TLPROT = 1
                                            next_state <= PGERS_ERROR;
                                        END IF;
                                    ELSIF ZERO_DETECTED = '1' THEN
                                        --Attempting to program any zero in the 16
                                        --lowest bytes or attempting to program any zero
                                        --in locked region
                                        next_state <= PGERS_ERROR;
                                    END IF;
                                END IF;
                            ELSIF (Instruct= ER256_C_0 OR Instruct= ER256_4_0) AND WRPGEN = '1' THEN
                                sect := ReturnSectorID
                                (Address,BottomBoot,TopBoot);
                                IF UniformSec OR (TopBoot AND BottomBoot = false AND (sect < 127)) OR
                                (BottomBoot AND TopBoot = false AND (sect > 31)) OR 
                                (TopBoot AND BottomBoot AND (sect > 15 AND sect < 144)) THEN
                                    IF (Sec_Prot(sect) = '0' AND PPB_bits(sect)='1'
                                    AND DYB_bits(sect)='1') THEN
                                        IF CFR3V(5) = '0' THEN
                                            next_state <=  SECTOR_ERS;
                                        ELSE
                                            next_state <=  BLANK_CHECK;
                                        END IF;
                                    ELSE
                                        next_state <=  PGERS_ERROR;
                                    END IF;
                                ELSIF (TopBoot AND BottomBoot = false AND (sect >= 127)) OR
                                (BottomBoot AND TopBoot = false AND (sect <= 32)) OR 
                                (TopBoot AND BottomBoot AND (sect <= 16 OR sect >= 144)) THEN
                                    IF Sec_ProtSE = 33 AND ASP_ProtSE = 33 THEN
                                    --Sector erase command is applied to a
                                    --256 KB range that includes 4 KB sectors.
                                        IF CFR3V(5) = '0' THEN
                                            next_state <=  SECTOR_ERS;
                                        ELSE
                                            next_state <=  BLANK_CHECK;
                                        END IF;
                                    ELSE
                                        next_state <=  PGERS_ERROR;
                                    END IF;
                                END IF;
                            ELSIF (Instruct=ER004_C_0 OR Instruct=ER004_4_0) AND WRPGEN='1' THEN
                                sect := ReturnSectorID(Address,BottomBoot,TopBoot);
                                IF UniformSec OR (TopBoot AND BottomBoot = false AND (sect < 128)) OR
                                (BottomBoot AND TopBoot = false AND (sect > 31)) OR 
                                (TopBoot AND BottomBoot AND (sect > 15 AND sect < 144)) THEN
                                    REPORT "The instruction is applied to "&
                                    "a sector that is larger than "&
                                    "4 KB. "&
                                    "Instruction is ignored!!!"
                                    SEVERITY warning;
                                ELSE
                                    IF (Sec_Prot(sect) = '0' AND PPB_bits(sect)='1'
                                    AND DYB_bits(sect)='1') THEN
                                        IF CFR3V(5) = '0' THEN
                                            next_state <=  SECTOR_ERS;
                                        ELSE
                                            next_state <=  BLANK_CHECK;
                                        END IF;
                                    ELSE
                                        next_state <=  PGERS_ERROR;
                                    END IF;
                                END IF;
                            ELSIF Instruct = ERCHP_0_0 AND WRPGEN = '1' AND
                            (STR1V(4)='0' AND STR1V(3)='0' AND STR1V(2)='0') THEN
                                IF CFR3V(5) = '0' THEN
                                    next_state <=  BULK_ERS;
                                ELSE
                                    next_state <=  BLANK_CHECK;
                                END IF;
                            ELSIF Instruct = WRAUB_0_1 AND WRPGEN = '1' THEN
                            --Autoboot Register Write Command
                                next_state <= AUTOBOOT_PG;
                            ELSIF (Instruct=PRPPB_C_0 OR Instruct=PRPPB_4_0) AND WRPGEN='1' THEN
                                IF ASPPPB ='1' AND ASPPRM ='1' AND PPBLCK ='1' THEN
                                    next_state <=  PPB_PG;
                                ELSE
                                    next_state <=  PGERS_ERROR;
                                END IF;
                            ELSIF Instruct=ERPPB_0_0 AND WRPGEN='1' THEN
                                IF ASPPPB='1' AND PPBLCK='1' AND ASPPRM='1' THEN
                                    next_state <=  PPB_ERS;
                                ELSE
                                    next_state <=  PGERS_ERROR;
                                END IF;
                            ELSIF Instruct=PRASP_0_1 AND WRPGEN='1' THEN
                                -- ASP Register Program Command
                                IF ASPPWD='1' AND ASPPER='1' THEN
                                    IF ASPO_in(2 downto 1) = "00" THEN
                                        next_state <=  PGERS_ERROR;
                                    ELSE
                                        next_state <=  ASP_PG;
                                    END IF;
                                ELSE
                                    next_state <=  PGERS_ERROR;
                                END IF;
                            ELSIF Instruct = WRPLB_0_0 AND WRPGEN = '1' THEN
                                next_state <= PLB_PG;
                            ELSIF (Instruct=WRDYB_C_1 OR Instruct=WRDYB_4_1) AND WRPGEN='1' THEN
                                IF DYAV_in = "11111111" OR
                                DYAV_in = "00000000" THEN
                                    next_state <= DYB_PG;
                                ELSE
                                    next_state <=  PGERS_ERROR;
                                END IF;
                            ELSIF Instruct = PRDLP_0_1  AND WRPGEN = '1' THEN
                                IF ASPPWD='1' AND ASPPER='1' THEN
                                    next_state <= NVDLR_PG;
                                ELSE
                                    next_state <=  PGERS_ERROR;
                                END IF;
                            ELSIF Instruct = PGPWD_0_1  AND WRPGEN = '1' THEN
                                IF ASPPWD='1' AND ASPPER='1' THEN
                                    next_state <= PASS_PG;
                                ELSE
                                    next_state <= PGERS_ERROR;
                                END IF;
                            ELSIF Instruct=PWDUL_0_1 AND  RDYBSY='0' THEN
                                next_state <= PASS_UNLOCK;
                            ELSIF Instruct = EVERS_C_0 THEN
                                next_state <= EVAL_ERS_STAT;
                            ELSIF Instruct = DICHK_4_1 THEN
                                IF (Address >= DIC_Start_Addr_reg + 3) THEN
                                -- Condition for entering DIC_calc state is not complete
                                -- it needs to have comparison of Addr to EndAddr
                                -- Check datasheet for table of state transitions
                                    next_state <= DIC_Calc;
                                ELSE
                                    next_state <= IDLE;
                                END IF;
                            ELSIF Instruct = SPEPD_0_0 THEN
                                next_state <= DIC_SUSP;
                            -- Reading Sector Erase Count register
                            ELSIF Instruct = SEERC_C_0 AND  RDYBSY='0' THEN
                                sect := ReturnSectorID
                                (Address,BottomBoot,TopBoot);--???
                                next_state <= SEERC;
                            ELSE
                                next_state <= IDLE;
                            END IF;
                        ELSIF (DPDEnt_out = '1') THEN
                            next_state <= DP_DOWN;
                        END IF;

                    WHEN  AUTOBOOT =>
                        IF rising_edge(CSNeg_ipd) THEN
                            next_state <= IDLE;
                        END IF;

                    WHEN WRITE_SR       =>
                        IF rising_edge(WDONE) THEN
                            next_state <= IDLE;
                        END IF;

                    WHEN WRITE_ALL_REG       =>
                        IF rising_edge(WDONE) OR rising_edge(CSDONE) THEN
                            next_state <= IDLE;
                        END IF;

                    WHEN PAGE_PG        =>
                        IF PRGSUSP_out'EVENT AND PRGSUSP_out = '1' THEN
                            next_state <= PG_SUSP;
                        ELSIF rising_edge(PDONE) THEN
                            next_state <= IDLE;
                        END IF;

                    WHEN OTP_PG         =>
                        IF rising_edge(PDONE) THEN
                            next_state <= IDLE;
                        END IF;

                    WHEN PG_SUSP      =>
                        IF falling_edge(write) THEN
                            IF Instruct = RSEPA_0_0 OR Instruct = RSEPD_0_0 THEN
                                next_state <=  PAGE_PG;
                            END IF;
                        END IF;

                    WHEN DIC_Calc =>
                        IF Instruct = SPEPD_0_0 OR rising_edge(sSTART_T1) THEN
                            next_state <= DIC_SUSP;
                        END IF;

                        IF rising_edge(DICDONE) THEN
                            next_state <= IDLE;
                        END IF;

                    WHEN DIC_SUSP      =>
                        IF falling_edge(write) THEN
                            IF Instruct = RSEPD_0_0 THEN
                                next_state <= DIC_Calc;
                            ELSIF Instruct = SFRST_0_0 OR Instruct = SFRSL_0_0 THEN
                                next_state <= RESET_STATE;
                            END IF;
                        END IF;

                    WHEN SECTOR_ERS     =>
                        IF ERSSUSP_out'EVENT AND ERSSUSP_out = '1' THEN
                            next_state <= ERS_SUSP;
                        ELSIF rising_edge(EDONE) THEN
                            next_state <= IDLE;
                        END IF;

                    WHEN BULK_ERS       =>
                        IF rising_edge(EDONE) THEN
                            next_state <= IDLE;
                        END IF;

                    WHEN ERS_SUSP      =>
                        IF falling_edge(write) THEN
                            IF (Instruct = PRPGE_C_1 OR Instruct = PRPGE_4_1) AND
                            WRPGEN='1' AND PRGERR='0' THEN
                                sect := ReturnSectorID(Address,BottomBoot,TopBoot);
                                IF SectorSuspend /= Address/(SecSize256+1) THEN
                                    IF Sec_Prot(sect)='0' AND PPB_bits(sect)='1'
                                    AND DYB_bits(sect)='1' THEN
                                    next_state <=  ERS_SUSP_PG;
                                    END IF;
                                END IF;
                            ELSIF (Instruct = RSEPA_0_0 OR Instruct = RSEPD_0_0) AND PRGERR = '0' THEN
                                next_state <=  SECTOR_ERS;
                            END IF;
                        END IF;

                    WHEN ERS_SUSP_PG         =>
                        IF PRGSUSP_out'EVENT AND PRGSUSP_out = '1' THEN
                            next_state <= ERS_SUSP_PG_SUSP;
                        ELSIF rising_edge(PDONE) THEN
                            next_state <= ERS_SUSP;
                        END IF;

                    WHEN ERS_SUSP_PG_SUSP      =>
                        IF falling_edge(write) THEN
                            IF Instruct = RSEPA_0_0 OR Instruct = RSEPD_0_0 THEN
                                next_state <=  ERS_SUSP_PG;
                            END IF;
                        END IF;

                    WHEN PASS_PG        =>
                        IF rising_edge(PDONE) THEN
                            next_state <= IDLE;
                        END IF;

                    WHEN PASS_UNLOCK    =>
                        IF falling_edge(PASSULCK_in) THEN
                            IF WRONG_PASS = 0 THEN
                                next_state <= IDLE;
                            ELSE
                                next_state <= LOCKED_STATE;
                            END IF;
                        END IF;
                        
                    WHEN LOCKED_STATE   =>

                    WHEN PPB_PG         =>
                        IF rising_edge(PDONE) THEN
                            next_state <= IDLE;
                        END IF;

                    WHEN PPB_ERS        =>
                        IF falling_edge(PPBERASE_in) THEN
                            next_state <= IDLE;
                        END IF;
                        
                    WHEN AUTOBOOT_PG    =>
                        IF rising_edge(PDONE) THEN
                            next_state <= IDLE;
                        END IF;

                    WHEN PLB_PG         =>
                        IF rising_edge(PDONE) THEN
                            next_state <= IDLE;
                        END IF;

                    WHEN DYB_PG         =>
                        IF rising_edge(PDONE) THEN
                            IF ERASES = '1' THEN
                                next_state <= ERS_SUSP;
                            ELSE
                                next_state <= IDLE;
                            END IF;
                        END IF;

                    WHEN ASP_PG         =>
                        IF rising_edge(PDONE) THEN
                            next_state <= IDLE;
                        END IF;

                    WHEN NVDLR_PG         =>
                        IF rising_edge(PDONE) THEN
                            next_state <= IDLE;
                        END IF;

                    WHEN PGERS_ERROR         =>
                        IF falling_edge(write) THEN
                            IF (Instruct=WRDIS_0_0 AND PRGERR='0' AND
                            ERSERR='0') OR (Instruct=EXCTM_0_0 AND PRGERR='0' AND
                            ERSERR='0') THEN
                            -- A Clear Status Register (CLPEF_0_0) followed by a Write
                            -- Disable (WRDIS_0_0) command must be sent to return the
                            -- device to standby state
                                next_state <= IDLE;
                            END IF;
                        END IF;

                    WHEN BLANK_CHECK         =>
                        IF rising_edge(BCDONE) THEN
                            IF NOT_BLANK = '1' THEN
                                IF Instruct=ERCHP_0_0 THEN
                                    next_state <= BULK_ERS;
                                ELSE
                                    next_state <= SECTOR_ERS;
                                END IF;
                            ELSE
                                next_state <= IDLE;
                            END IF;
                        END IF;

                    WHEN EVAL_ERS_STAT         =>
                        IF rising_edge(EESDONE) THEN
                            next_state <= IDLE;
                        END IF;

                    WHEN DP_DOWN =>
                        IF falling_edge(RST) THEN
                            next_state <= RESET_STATE;
                        ELSIF rising_edge(DPDExt_out) THEN
                            next_state <= IDLE;
                        END IF;
                    
                    WHEN SEERC   =>
                        IF rising_edge(SEERC_DONE) THEN
                            next_state <= IDLE;
                        END IF;

                END CASE;
            END IF;
        END IF;
        
    END PROCESS StateGen;
        
    CSNegSignalingResetStateTran : PROCESS(CSNeg_ipd, SI_ipd, SCK_ipd)
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
                IF falling_edge(CSNeg_ipd) AND SI_ipd = '0' THEN
                   next_sigres_state <= SIGRES_SECOND_FE;
                -- SI data cannot toggle during memory selection
                -- SCK cannot toggle during memory selectio
                ELSIF ((rising_edge(SCK_ipd) OR falling_edge(SCK_ipd) OR
                      (SI_ipd = '1')) AND (CSNeg_ipd = '0')) THEN
                   next_sigres_state <= SIGRES_NOT_A_RESET;
                END IF;
             WHEN SIGRES_SECOND_FE   => -- 2nd falling edge occured
                -- Data needs to be constant one during and at the end of
                -- memory selection - check if this is the case
                IF rising_edge(CSNeg_ipd) AND SI_ipd = '0' THEN
                   next_sigres_state <= SIGRES_SECOND_RE;
                -- SI data cannot toggle during memory selection
                -- SCK cannot toggle during memory selectio
                ELSIF ((rising_edge(SCK_ipd) OR falling_edge(SCK_ipd) OR
                      (SI_ipd = '1')) AND (CSNeg_ipd = '0')) THEN
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
             WHEN SIGRES_THIRD_RE   => -- 3rd rising  edge occured
                   -- Final state - reset memory
                RST_SIG <= '1', '0' AFTER 10 ns;
             WHEN SIGRES_NOT_A_RESET   =>
                IF CSNeg_ipd = '1' THEN
                   next_sigres_state <= SIGRES_IDLE;
                END IF;
         END CASE;
    END PROCESS CSNegSignalingResetStateTran;

 
    

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
    Functional : PROCESS(write,current_state, PoweredUp, WDONE, PDONE, EDONE, DICDONE,
                         CSDONE, ERSSUSP_out, PRGSUSP_out, PASSULCK_out, oe, oe_z,
                         PPBERASE_out, BCDONE, EESDONE, Instruct, start_autoboot,
                         sSTART_T1, change_addr,
                         CSNeg, reseted, SEERC_DONE, DPDExt_out, DPD_in)

        VARIABLE WData          : WByteType:= (OTHERS => MaxData);

        VARIABLE AddrLo         : NATURAL;
        VARIABLE AddrHi         : NATURAL;
        VARIABLE Addr           : NATURAL;
        VARIABLE Addr_pgm       : NATURAL;
        VARIABLE Addr_ers       : NATURAL;
        VARIABLE Addr_pgm_tmp   : NATURAL;
        VARIABLE Addr_pgm_ECC   : NATURAL;
        VARIABLE Addr_idcfi     : NATURAL;

        VARIABLE data_out       : std_logic_vector(7 downto 0);

        VARIABLE old_bit        : std_logic_vector(7 downto 0);
        VARIABLE new_bit        : std_logic_vector(7 downto 0);
        VARIABLE old_int        : INTEGER RANGE -1 to MaxData;
        VARIABLE new_int        : INTEGER RANGE -1 to MaxData;
        VARIABLE old_pass       : std_logic_vector(63 downto 0);
        VARIABLE new_pass       : std_logic_vector(63 downto 0);
        VARIABLE old_pass_byte  : std_logic_vector(7 downto 0);
        VARIABLE new_pass_byte  : std_logic_vector(7 downto 0);
        VARIABLE wr_cnt         : NATURAL RANGE 0 TO 511;
        --Data Learning Pattern Enable
        VARIABLE dlp_act        : BOOLEAN   := FALSE;

        VARIABLE sect           : NATURAL RANGE 0 TO SecNumHyb;
        VARIABLE SectorErased   : NATURAL RANGE 0 TO SecNumHyb;
        
        VARIABLE cnt            : NATURAL RANGE 0 TO 512 := 0;
        VARIABLE Instruct_ER004_C_0   : std_logic;

        PROCEDURE ADDRHILO_SEC(
            VARIABLE   AddrLOW  : INOUT NATURAL RANGE 0 to ADDRRange;
            VARIABLE   AddrHIGH : INOUT NATURAL RANGE 0 to ADDRRange;
            VARIABLE   Addr     : NATURAL) IS
            VARIABLE   sec     : NATURAL;
        BEGIN
            IF CFR3V(3) = '0' THEN -- Hybrid Sector Architecture
                IF TB4KBS_NV = '0' THEN -- 4KB Sectors at Bottom
                    IF  Addr/(SecSize256+1) = 0 THEN
                        IF Addr/(SecSize4+1) < 32 AND
                        Instruct_ER004_C_0 = '1' THEN  --4KB Sectors
                            sec := Addr/(SecSize4+1);
                            AddrLOW  := sec*(SecSize4+1);
                            AddrHIGH := sec*(SecSize4+1) + SecSize4;
                        ELSE
                            AddrLOW  := 32*(SecSize4+1);
                            AddrHIGH := SecSize256;
                        END IF;
                    ELSE
                        sec := Addr/(SecSize256+1);
                        AddrLOW  := sec*(SecSize256+1);
                        AddrHIGH := sec*(SecSize256+1) + SecSize256;
                    END IF;
                ELSE -- 4KB Sectors at Top
                    IF Addr/(SecSize256+1) = 127 THEN
                        IF (Addr > (AddrRANGE - 32*(SecSize4+1)))
                        AND Instruct_ER004_C_0='1' THEN --4KB Sectors
                            sec := 128 +
                           (Addr-(AddrRANGE + 1 - 8*(SecSize4+1)))/(SecSize4+1);
                            AddrLOW  := AddrRANGE + 1 - 32*(SecSize4+1) +
                            (sec-128)*(SecSize4+1);
                            AddrHIGH :=AddrLOW + SecSize4;
                        ELSE
                            AddrLOW  := 127*(SecSize256+1);
                            AddrHIGH := AddrRANGE - 32*(SecSize4+1);
                        END IF;
                    ELSE
                        sec := Addr/(SecSize256+1);
                        AddrLOW  := sec*(SecSize256+1);
                        AddrHIGH := AddrLOW + SecSize256;
                    END IF;
                END IF;
            ELSE  -- Uniform Sector Architecture
                sec := Addr/(SecSize256+1);
                AddrLOW  := sec*(SecSize256+1);
                AddrHIGH := sec*(SecSize256+1) + SecSize256;
            END IF;
        END ADDRHILO_SEC;

        PROCEDURE ADDRHILO_PG(
            VARIABLE   AddrLOW  : INOUT NATURAL RANGE 0 to ADDRRange;
            VARIABLE   AddrHIGH : INOUT NATURAL RANGE 0 to ADDRRange;
            VARIABLE   Addr     : NATURAL) IS
            VARIABLE   page     : NATURAL;
        BEGIN
            page     := Addr/(PageSize+1);
            AddrLOW  := Page*(PageSize+1);
            AddrHIGH := AddrLOW + PageSize;
        END ADDRHILO_PG;

        PROCEDURE READ_ALL_REG(
            VARIABLE   RDAR_reg  : INOUT std_logic_vector(7 downto 0);
            VARIABLE   Addr     : NATURAL) IS
        BEGIN
            IF Addr = 16#00000000# THEN
                RDAR_reg := STR1N;
            ELSIF Addr = 16#00000002# THEN
                RDAR_reg := CFR1N;
            ELSIF Addr = 16#00000003# THEN
                RDAR_reg := CFR2N;
            ELSIF Addr = 16#00000004# THEN
                RDAR_reg := CFR3N;
            ELSIF Addr = 16#00000005# THEN
                RDAR_reg := CFR4N;
            ELSIF Addr = 16#0000010# THEN
                RDAR_reg := DLPN;
            ELSIF Addr = 16#0000020# THEN
                IF ASPPWD = '1' THEN
                    RDAR_reg := PWDO(7 downto 0);
                ELSE
                    RDAR_reg := "XXXXXXXX";
                END IF;
            ELSIF Addr = 16#00000021# THEN
                IF ASPPWD = '1' THEN
                    RDAR_reg := PWDO(15 downto 8);
                ELSE
                    RDAR_reg := "XXXXXXXX";
                END IF;
            ELSIF Addr = 16#00000022# THEN
                IF ASPPWD = '1' THEN
                    RDAR_reg := PWDO(23 downto 16);
                ELSE
                    RDAR_reg := "XXXXXXXX";
                END IF;
            ELSIF Addr = 16#00000023# THEN
                IF ASPPWD = '1' THEN
                    RDAR_reg := PWDO(31 downto 24);
                ELSE
                    RDAR_reg := "XXXXXXXX";
                END IF;
            ELSIF Addr = 16#0000024# THEN
                IF ASPPWD = '1' THEN
                    RDAR_reg := PWDO(39 downto 32);
                ELSE
                    RDAR_reg := "XXXXXXXX";
                END IF;
            ELSIF Addr = 16#0000025# THEN
                IF ASPPWD = '1' THEN
                    RDAR_reg := PWDO(47 downto 40);
                ELSE
                    RDAR_reg := "XXXXXXXX";
                END IF;
            ELSIF Addr = 16#0000026# THEN
                IF ASPPWD = '1' THEN
                    RDAR_reg := PWDO(55 downto 48);
                ELSE
                    RDAR_reg := "XXXXXXXX";
                END IF;
            ELSIF Addr = 16#0000027# THEN
                IF ASPPWD = '1' THEN
                    RDAR_reg := PWDO(63 downto 56);
                ELSE
                    RDAR_reg := "XXXXXXXX";
                END IF;
            ELSIF Addr = 16#0000030# THEN
                RDAR_reg := ASPO(7 downto 0);
            ELSIF Addr = 16#0000031# THEN
                RDAR_reg := ASPO(15 downto 8);
            ELSIF Addr = 16#0000042# THEN
                RDAR_reg := ATBN(7 downto 0);
            ELSIF Addr = 16#0000043# THEN
                RDAR_reg := ATBN(15 downto 8);
            ELSIF Addr = 16#0000044# THEN
                RDAR_reg := ATBN(23 downto 16);
            ELSIF Addr = 16#0000045# THEN
                RDAR_reg := ATBN(31 downto 24);
            ELSIF Addr = 16#0000050# THEN
                RDAR_reg := EFX0O(7 downto 0);
--             ELSIF Addr = 16#0000051# THEN
--                 RDAR_reg := EFX0O(15 downto 8);
            ELSIF Addr = 16#0000052# THEN
                RDAR_reg := EFX1O(7 downto 0);
            ELSIF Addr = 16#0000053# THEN
                RDAR_reg := EFX1O(15 downto 8);
            ELSIF Addr = 16#0000054# THEN
                RDAR_reg := EFX2O(7 downto 0);
            ELSIF Addr = 16#0000055# THEN
                RDAR_reg := EFX2O(15 downto 8);
            ELSIF Addr = 16#0000056# THEN
                RDAR_reg := EFX3O(7 downto 0);
            ELSIF Addr = 16#0000057# THEN
                RDAR_reg := EFX3O(15 downto 8);
            ELSIF Addr = 16#0000058# THEN
                RDAR_reg := EFX4O(7 downto 0);
            ELSIF Addr = 16#0000059# THEN
                RDAR_reg := EFX4O(15 downto 8);
            ELSIF Addr = 16#0000079# THEN
--                 RDAR_reg := UID(7 downto 0);
--             ELSIF Addr = 16#000007A# THEN
--                 RDAR_reg := UID(15 downto 8);
--             ELSIF Addr = 16#000007B# THEN
--                 RDAR_reg := UID(23 downto 16);
--             ELSIF Addr = 16#000007C# THEN
--                 RDAR_reg := UID(31 downto 24);
--             ELSIF Addr = 16#000007D# THEN
--                 RDAR_reg := UID(39 downto 32);
--             ELSIF Addr = 16#000007E# THEN
--                 RDAR_reg := UID(47 downto 40);
--             ELSIF Addr = 16#000007F# THEN
--                 RDAR_reg := UID(55 downto 48);
--             ELSIF Addr = 16#0000080# THEN
--                 RDAR_reg := UID(63 downto 56);
            -- VOLOTILE
            ELSIF Addr = 16#00800000# THEN
                RDAR_reg := STR1V;
            ELSIF Addr = 16#00800001# THEN
                RDAR_reg := STR2V;
            ELSIF Addr = 16#00800002# THEN
                RDAR_reg := CFR1V;
            ELSIF Addr = 16#00800003# THEN
                RDAR_reg := CFR2V;
            ELSIF Addr = 16#00800004# THEN
                RDAR_reg := CFR3V;
            ELSIF Addr = 16#00800005# THEN
                RDAR_reg := CFR4V;
            ELSIF Addr = 16#0800010# THEN
                RDAR_reg := DLPV;
            ELSIF Addr = 16#0800089# THEN
                RDAR_reg := ECSV;
            ELSIF Addr = 16#080008A# THEN
                RDAR_reg := ECTV(7 downto 0);
            ELSIF Addr = 16#080008B# THEN
                RDAR_reg := ECTV(15 downto 8);
            ELSIF Addr = 16#080008E# THEN
                RDAR_reg := EATV(7 downto 0);
            ELSIF Addr = 16#080008F# THEN
                RDAR_reg := EATV(15 downto 8);
            ELSIF Addr = 16#0800040# THEN
                RDAR_reg := EATV(23 downto 16);
            ELSIF Addr = 16#0800041# THEN
                RDAR_reg := EATV(31 downto 24);
--             ELSIF Addr = 16#0800090# THEN
--                 RDAR_reg := Bank_Addr_reg;
            ELSIF Addr = 16#0800091# THEN
                RDAR_reg := SECV(7 downto 0);
            ELSIF Addr = 16#0800092# THEN
                RDAR_reg := SECV(15 downto 8);
            ELSIF Addr = 16#0800093# THEN
                RDAR_reg := SECV(23 downto 16);
            ELSIF Addr = 16#0800095# THEN
                RDAR_reg := DCRV(7 downto 0);
            ELSIF Addr = 16#0800096# THEN
                RDAR_reg := DCRV(15 downto 8);
            ELSIF Addr = 16#0800097# THEN
                RDAR_reg := DCRV(23 downto 16);
            ELSIF Addr = 16#0800098# THEN
                RDAR_reg := DCRV(31 downto 24);
            ELSIF Addr = 16#080009B# THEN
                RDAR_reg := PPLV;
            ELSE
                RDAR_reg := "XXXXXXXX";
            END IF;
        END READ_ALL_REG;

        FUNCTION Return_DLP (Latency_code : NATURAL; dummy_cnt : NATURAL)
                                                RETURN BOOLEAN IS
            VARIABLE result : BOOLEAN;
        BEGIN
            IF (Latency_code >= 4) AND (dummy_cnt >= (2*Latency_code-8)) THEN
                result := TRUE;
            ELSE
                result := FALSE;
            END IF;
            RETURN result;
        END Return_DLP;
        
        FUNCTION Return_DLP_SDR (Latency_code : NATURAL; dummy_cnt : NATURAL)
                                                RETURN BOOLEAN IS
            VARIABLE result : BOOLEAN;
        BEGIN
            IF (Latency_code >= 8) AND (dummy_cnt >= 2*(Latency_code-8)) THEN
                result := TRUE;
            ELSE
                result := FALSE;
            END IF;
            RETURN result;
        END Return_DLP_SDR;

    BEGIN

        -----------------------------------------------------------------------
        -- Functionality Section
        -----------------------------------------------------------------------

        IF Instruct'EVENT THEN
            read_cnt := 0;
            byte_cnt := 1;
            rd_fast  <= false;
            rd_fast1 <= false;
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
            STR1V <= STR1N;

            CFR1V <= CFR1N;
            CFR2V <= CFR2N;
            CFR3V <= CFR3N;
            CFR4V <= CFR4N;
--             ECSV(4) := '0'; -- 2 bits ECC detection
--             ECSV(3) := '0'; -- 1 bit ECC correction
            ECTV := "0000000000000000";

            DLPV := DLPN;
            -- As shipped from the factory, all devices default ASP to the
            -- Persistent Protection mode, with all sectors unprotected,
            -- when power is applied. The device programmer or host system must
            -- then choose which sector protection method to use.
            -- For Persistent Protection mode, PPLVOCK defaults to "1"
            PPLV(0) := '1';
            IF ASPDYB='0' THEN
                -- All the DYB power-up in the protected state
                DYB_bits := (OTHERS =>'0');
            ELSE
                -- All the DYB power-up in the unprotected state
                DYB_bits := (OTHERS =>'1');
            END IF;

            BP_bits := STR1V(4) & STR1V(3) & STR1V(2);
            change_BP <= '1', '0' AFTER 1 ns;

            DIC_ACT      := '0';
            DIC_RD_SETUP := '0';
        END IF;

        IF change_addr'EVENT THEN
            read_addr := Address;
        END IF;

        IF DPDExt_out'EVENT AND DPDExt_out = '1' THEN
            DPD_in        <= '0';
        END IF;

        CASE current_state IS

            WHEN IDLE          =>

                ASP_ProtSE := 0;
                Sec_ProtSE := 0;
                IF (BottomBoot AND TopBoot = false) THEN
                    FOR I IN 0 TO 32 LOOP
                        IF PPB_bits(I)='1' AND DYB_bits(I)='1' THEN
                            ASP_ProtSE := ASP_ProtSE + 1;
                        END IF;
                        IF Sec_Prot(I) = '0' THEN
                            Sec_ProtSE := Sec_ProtSE + 1;
                        END IF;
                    END LOOP;
                ELSIF (BottomBoot = false AND TopBoot) THEN
                    FOR I IN 127 TO 287 LOOP
                        IF PPB_bits(I)='1' AND DYB_bits(I)='1' THEN
                            ASP_ProtSE := ASP_ProtSE + 1;
                        END IF;
                        IF Sec_Prot(I) = '0' THEN
                            Sec_ProtSE := Sec_ProtSE + 1;
                        END IF;
                    END LOOP;
                ELSIF (BottomBoot AND TopBoot) THEN
                    FOR I IN 0 TO 16 LOOP
                        IF PPB_bits(I)='1' AND DYB_bits(I)='1' THEN
                            ASP_ProtSE := ASP_ProtSE + 1;
                        END IF;
                        IF Sec_Prot(I) = '0' THEN
                            Sec_ProtSE := Sec_ProtSE + 1;
                        END IF;
                    END LOOP;
                    FOR I IN 143 TO 287 LOOP
                        IF PPB_bits(I)='1' AND DYB_bits(I)='1' THEN
                            ASP_ProtSE := ASP_ProtSE + 1;
                        END IF;
                        IF Sec_Prot(I) = '0' THEN
                            Sec_ProtSE := Sec_ProtSE + 1;
                        END IF;
                    END LOOP;
                    Sec_ProtSE := Sec_ProtSE - 1;
                    ASP_ProtSE := ASP_ProtSE - 1;
                END IF;

                IF falling_edge(write) AND DPD_in ='0' THEN
                    
                    IF Instruct = WRENV_0_0 THEN
                        WVREG <= '1'; -- Write volatile Regs
                    
                    ELSIF Instruct = WRENB_0_0 THEN
                        STR1V(1) <= '1';
                        STR1V_DPD <= '1';
                    ELSIF Instruct = ENCTM_0_0 THEN
                        CRYPTO_in <= '1';    
                    ELSIF Instruct = WRDIS_0_0 THEN
                        STR1V(1) <= '0';
                        WVREG <= '0';
                    ELSIF Instruct = EXCTM_0_0 THEN
                        CRYPTO_in <= '0';     
                    ELSIF (Instruct = WRAUB_0_1 AND WRPGEN = '1') THEN
                        PSTART <= '1', '0' AFTER 1 ns;
                        RDYBSY <= '1';
                        WVREG <= '0';
                        CFR4N(4) <= '0';
                    ELSIF Instruct = EN4BA_0_0 THEN
                        CFR2V(7) <= '1';
                    ELSIF Instruct = EX4BA_0_0 THEN
                        CFR2V(7) <= '0';
                    ELSIF Instruct = EVERS_C_0 THEN
                        sect := ReturnSectorID(Address,BottomBoot,TopBoot);
                        EESSTART <= '1', '0' AFTER 1 ns;
                        STR1V(0) <= '1';  -- RDYBSY
                        STR1V(1) <= '1';  -- WRPGEN
                    ELSIF Instruct=WRREG_0_1 AND (WRPGEN='1' OR WVREG='1')  THEN
                        IF  not(STCFWR='1' AND WPNegIn='0' AND QUAD_QPI='0') THEN
                           
                            IF (ASPPWD/='1' OR ASPPER/='1') THEN
                           -- Once the protection mode is selected, the OTP
                           -- bits are permanently protected from programming
                                STR1V(6) <= '1'; -- PRGERR
                                STR1V(0) <= '1'; -- RDYBSY
                            ELSE
                                WSTART <= '1', '0' AFTER 1 ns;
                                STR1V(0) <= '1';  -- RDYBSY
                            END IF;
                        ELSE
                         -- can not execute if Hardware Protection Mode
                         -- is entered or if WRPGEN bit is zero
                             STR1V(1) <= '0'; -- WRPGEN
                             STR1V_DPD <= '0';
                             WVREG    <= '0'; -- Write volatile regs
                        END IF;

                    ELSIF Instruct=WRARG_C_1 AND (WRPGEN='1' OR WVREG = '1') THEN
                        IF not(STCFWR='1' AND WPNegIn='0' AND QUAD_QPI='0' AND
                        (Address=16#00000000# OR
                         Address=16#00000002# OR
                         Address=16#00800000# OR
                         Address=16#00800002#)) THEN
                        -- can not execute if WRPGEN bit is zero or Hardware
                        -- Protection Mode is entered and SR1NV,SR1V,CR1NV or
                        -- CR1V is selected (no error is set)
                            Addr := Address;
                            IF Address=16#00000001#  OR
                               ((Address>16#00000005#) AND
                               (Address<16#00000010#)) OR
                               ((Address>16#00000010#) AND
                               (Address<16#00000020#)) OR
                               ((Address>16#00000027#) AND
                               (Address<16#00000030#)) OR
                               ((Address>16#00000031#) AND
                               (Address<16#00800000#)) OR
                               ((Address>16#00800005#) AND
                               (Address<16#00800010#)) OR
                               (Address>16#00800010#) THEN
                                STR1V(1) <= '0';  -- WRPGEN
                            ELSIF Address = 16#00000002# AND
                            ((TBPROT_NV='1' AND WRAR_reg_in(5)='0') OR
                             (TB4KBS_NV='1' AND WRAR_reg_in(2)='0'
                             AND CFR3V(3) = '0') OR
                            (WRAR_reg_in(3)='0')) THEN
                                STR1V(1) <= '0';  -- WRPGEN
                                
                             STR1V_DPD <= '0';
                             WVREG    <= '0'; -- Write volatile regs
                            ELSIF (ASPPWD/='1' OR ASPPER/='1') THEN
                            -- Once the protection mode is selected,the OTP
                            -- bits are permanently protected from programming
                                IF ((WRAR_reg_in(5)='1' OR
                                (WRAR_reg_in(4)='1') OR
                                WRAR_reg_in(3)='1' OR
                                (WRAR_reg_in(2)='1' AND CFR3N(3)='0')) AND
                                Address =16#00000002#) OR -- CR1NV[5:2]
                                Address =16#00000003# OR -- CR2NV
                                Address =16#00000004# OR -- CR3NV
                                Address =16#00000005# OR -- CR4NV
                                Address =16#00000010# OR -- NVDLR
                                Address =16#00000020# OR -- PASS(7:0)
                                Address =16#00000021# OR -- PASS(15:8)
                                Address =16#00000022# OR -- PASS(23:16)
                                Address =16#00000023# OR -- PASS(31:24)
                                Address =16#00000024# OR -- PASS(39:32)
                                Address =16#00000025# OR -- PASS(47:40)
                                Address =16#00000026# OR -- PASS(55:58)
                                Address =16#00000027# OR -- PASS(63:56)
                                Address =16#00000030# OR -- ASPR(7:0)
                                Address =16#00000031# THEN-- ASPR(15:8)
                                    STR1V(6) <= '1';  -- PRGERR
                                    STR1V(0) <= '1';  -- RDYBSY
                                ELSE
                                    CSSTART <= '1', '0' AFTER 1 ns;
                                    STR1V(0) <= '1';  -- RDYBSY
                                END IF;
                            ELSE -- Protection Mode not selected
                                IF Address =16#00000030# OR
                                Address =16#00000031# THEN --ASPR
                                    IF WRAR_reg_in(2)='0' AND
                                    WRAR_reg_in(1)='0'
                                    AND Address =16#00000030# THEN
                                        STR1V(6) <= '1';  -- PRGERR
                                        STR1V(0) <= '1';  -- RDYBSY                                        
                                    ELSE
                                        WSTART <= '1', '0' AFTER 1 ns;
                                        STR1V(0) <= '1';  -- RDYBSY
                                    END IF;
                                ELSIF Address = 16#00000000# OR
                                Address = 16#00000010# OR
                                (Address >= 16#00000002# AND
                                Address <= 16#00000005#) OR
                                (Address >= 16#00000020# AND
                                Address <= 16#00000027#) THEN
                                    WSTART <= '1', '0' AFTER 1 ns;
                                    STR1V(0) <= '1';  -- RDYBSY
                                ELSE
                                    CSSTART <= '1', '0' AFTER 1 ns;
                                    STR1V(0) <= '1';  -- RDYBSY
                                END IF;
                            END IF;
                        ELSE
                        -- can not execute if Hardware Protection Mode
                        -- is entered or if WRPGEN bit is zero
                            STR1V(1) <= '0'; -- WRPGEN
                             STR1V_DPD <= '0';
                             WVREG    <= '0'; -- Write volatile regs
                        END IF;

                    ELSIF (Instruct=PRPGE_C_1 OR Instruct=PRPGE_4_1) AND WRPGEN='1' THEN
                        pgm_page := Address/(PageSize+1);
                        SecAddr_pgm := ReturnSectorID
                                                (Address,BottomBoot,TopBoot);
                        IF (Sec_Prot(SecAddr_pgm) = '0' AND
                        PPB_bits(SecAddr_pgm)='1' AND
                        DYB_bits(SecAddr_pgm)='1') THEN
                            PSTART <= '1', '0' AFTER 1 ns;
                            PGSUSP  <= '0';
                            PGRES   <= '0';
                            INITIAL_CONFIG <= '1';
                            STR1V(0) <= '1';  -- RDYBSY
                            Addr_pgm := Address;
                            Addr_pgm_tmp := Address;
                            Addr_pgm_ECC := Address;
                            wr_cnt := Byte_number;
                            FOR I IN wr_cnt DOWNTO 0 LOOP
                                IF Viol /= '0' THEN
                                    WData(i) := -1;
                                ELSE
                                    WData(i) := WByte(i);
                                END IF;
                            END LOOP;
                        ELSE
                        -- PRGERR bit will be set when the user attempts to
                        -- to program within a protected main memory sector
                            STR1V(6) <= '1'; -- PRGERR
                            STR1V(0) <= '1'; -- RDYBSY
                        END IF;

                    ELSIF Instruct = PRSSR_C_1 AND WRPGEN = '1' THEN
                        IF (Address + Byte_number) <= OTPHiAddr THEN
                        -- Program within valid OTP Range
                            IF ((Address>=16#10# AND Address<=16#FF#)
                            AND LOCK_BYTE1(Address/32) = '1') OR
                            ((Address >= 16#100# AND Address<=16#1FF#)
                            AND LOCK_BYTE2((Address-16#100#)/32) = '1')
                            OR ((Address>=16#200# AND Address<=16#2FF#)
                            AND LOCK_BYTE3((Address-16#200#)/32)='1')
                            OR ((Address>=16#300# AND Address<=16#3FF#)
                            AND LOCK_BYTE4((Address-16#300#)/32)='1') THEN
                            -- As long as the TLPROT bit remains cleared to a
                            -- logic '0' the OTP address space is programmable.
                                IF TLPROT = '0' THEN
                                    PSTART <= '1', '0' AFTER 1 ns;
                                    STR1V(0) <= '1'; --RDYBSY
                                    Addr_pgm := Address;
                                    Addr_pgm_ECC := Address;
                                    wr_cnt := Byte_number;
                                    FOR I IN wr_cnt DOWNTO 0 LOOP
                                        IF Viol /= '0' THEN
                                            WData(i) := -1;
                                        ELSE
                                            WData(i) := WByte(i);
                                        END IF;
                                    END LOOP;
                                ELSE
                                -- Attempting to program within valid OTP
                                -- range while TLPROT = 1
                                    STR1V(6) <= '1'; -- PRGERR
                                    STR1V(0) <= '1'; -- RDYBSY
                                END IF;
                            ELSIF ZERO_DETECTED = '1' THEN
                                IF Address > 16#3FF# THEN
                                    ASSERT false
                                        REPORT "Given  address is out of" &
                                            "OTP address range"
                                        SEVERITY warning;
                                ELSE
                                -- Attempting to program any zero in the 16
                                -- lowest bytes or attempting to program any zero
                                -- in locked region
                                    STR1V(6) <= '1'; -- PRGERR
                                    STR1V(0) <= '1'; -- RDYBSY
                                END IF;
                            END IF;
                        END IF;

                    ELSIF (Instruct = ER256_C_0 OR Instruct = ER256_4_0) AND WRPGEN = '1' THEN
                        SecAddr_ers := ReturnSectorID(Address,BottomBoot,TopBoot);
                        SectorSuspend <= Address/(SecSize256+1);
                        Addr_ers := Address;
                        IF UniformSec OR (TopBoot AND BottomBoot = false AND (SecAddr_ers <= 127)) OR
                        (BottomBoot AND TopBoot = false AND (SecAddr_ers >= 32)) OR
                        (BottomBoot AND TopBoot AND (SecAddr_ers >= 16 AND SecAddr_ers <= 143)) THEN
                            IF (Sec_Prot(SecAddr_ers) = '0' AND 
                            PPB_bits(SecAddr_ers)='1' AND
                            DYB_bits(SecAddr_ers)='1') THEN
                                Addr := Address;
                                IF CFR3V(5) = '0' THEN
                                    bc_done <= '0';
                                    ESTART <= '1', '0' AFTER 1 ns;
                                    ESUSP  <= '0';
                                    ERES   <= '0';
                                    INITIAL_CONFIG <= '1';
                                    STR1V(0) <= '1'; --RDYBSY
                                END IF;
                            ELSE
                            -- ERSERR bit will be set when the user attempts to
                            -- erase an individual protected main memory sector
                                STR1V(5)<= '1'; -- ERSERR
                                STR1V(0) <= '1'; -- RDYBSY
                            END IF;
                        ELSIF UniformSec OR (TopBoot AND BottomBoot = false AND (SecAddr_ers >= 127)) OR
                        (BottomBoot AND TopBoot = false AND (SecAddr_ers <= 32)) OR
                        (BottomBoot AND TopBoot AND (SecAddr_ers <= 16 OR SecAddr_ers >= 143)) THEN
                            IF Sec_ProtSE = 33 AND ASP_ProtSE = 33 THEN
                                --Sector erase command is applied to a
                                --256 KB range that includes 4 KB sectors.
                                Addr := Address;
                                IF CFR3V(5) = '0' THEN
                                    bc_done <= '0';
                                    ESTART <= '1', '0' AFTER 1 ns;
                                    ESUSP  <= '0';
                                    ERES   <= '0';
                                    INITIAL_CONFIG <= '1';
                                    STR1V(0) <= '1'; --RDYBSY
                                END IF;
                            ELSE
                            -- ERSERR bit will be set when the user attempts to
                            -- erase an individual protected main memory sector
                                STR1V(5)<= '1'; -- ERSERR
                                STR1V(0) <= '1'; -- RDYBSY
                            END IF;
                        END IF;

                    ELSIF (Instruct = ER004_C_0 OR Instruct = ER004_4_0) AND WRPGEN = '1' THEN
                        SecAddr_ers := ReturnSectorID(Address,BottomBoot,TopBoot);
                        IF UniformSec OR (TopBoot AND BottomBoot = false AND (SecAddr_ers <= 127)) OR
                        (BottomBoot AND TopBoot = false AND (SecAddr_ers >= 32)) OR
                        (BottomBoot AND TopBoot AND (SecAddr_ers >= 16 AND SecAddr_ers <= 143)) THEN
                            STR1V(1) <= '0'; -- WRPGEN
                            STR1V_DPD <= '0'; -- 
                            WVREG     <= '0'; -- Write volatile
                        ELSE
                            IF (Sec_Prot(SecAddr_ers) = '0' AND 
                            PPB_bits(SecAddr_ers)='1' AND
                            DYB_bits(SecAddr_ers)='1') THEN
                            -- A ER004_C_0 instruction applied to a sector
                            -- that has been Write Protected through the
                            -- Block Protect Bits or ASP will not be
                            -- executed and will set the ERSERR status
                                Addr_ers := Address;
                                Instruct_ER004_C_0 := '1';
                                IF CFR3V(5) = '0' THEN
                                    bc_done <= '0';
                                    ESTART <= '1', '0' AFTER 1 ns;
                                    ESUSP  <= '0';
                                    ERES   <= '0';
                                    INITIAL_CONFIG <= '1';
                                    STR1V(0) <= '1'; --RDYBSY
                                END IF;
                            ELSE
                            -- ERSERR bit will be set when the user attempts to
                            -- erase an individual protected main memory sector
                                STR1V(5)<= '1'; -- ERSERR
                                STR1V(0) <= '1'; -- RDYBSY
                            END IF;
                        END IF;

                    ELSIF Instruct = ERCHP_0_0 AND WRPGEN = '1' THEN
                        IF STR1V(4)='0' AND STR1V(3)='0' AND STR1V(2)='0' THEN
                            IF CFR3V(5) = '0' THEN
                                bc_done <= '0';
                                ESTART <= '1', '0' AFTER 1 ns;
                                ESUSP  <= '0';
                                ERES   <= '0';
                                INITIAL_CONFIG <= '1';
                                STR1V(0) <= '1';
                            END IF;
                        ELSE
                        --The Bulk Erase command will not set ERSERR if a
                        --protected sector is found during the command
                        --execution.
                            STR1V(1)   <= '0';--WRPGEN
                            STR1V_DPD <= '0'; -- 
                            WVREG     <= '0'; -- Write Volotil  
                        END IF;

                    ELSIF (Instruct=PRPPB_C_0 OR Instruct=PRPPB_4_0) AND WRPGEN='1' THEN
                        IF ASPPRM='1' AND PPBLCK='1' AND ASPPPB='1' THEN
                            sect := ReturnSectorID(Address,BottomBoot,TopBoot);
                            PSTART <= '1', '0' AFTER 1 ns;
                            STR1V(0) <= '1'; --RDYBSY
                        ELSIF PPBLCK='0' OR (ASPPRM='0') THEN
                            STR1V(6) <= '1'; -- PRGERR
                            STR1V(0) <= '1'; -- RDYBSY
                        END IF;

                    ELSIF Instruct=ERPPB_0_0 AND WRPGEN = '1' THEN
                            IF ASPPPB='1' AND PPBLCK='1' AND ASPPRM='1' THEN
                                PPBERASE_in <= '1';
                                STR1V(0) <= '1'; -- RDYBSY
                            ELSE
                                STR1V(5) <= '1'; -- ERSERR
                                STR1V(0) <= '1'; -- RDYBSY
                            END IF;


                    ELSIF Instruct=PRASP_0_1 AND WRPGEN = '1' THEN
                        IF ASPPWD='1' AND ASPPER='1' THEN -- Protection Mode not selected
                            IF ASPO_in(2)='0' AND ASPO_in(1)='0' THEN
                                STR1V(6) <= '1'; -- PRGERR
                                STR1V(0) <= '1'; -- RDYBSY
                            ELSE
                                PSTART <= '1', '0' AFTER 1 ns;
                                STR1V(0) <= '1'; -- RDYBSY
                            END IF;
                        ELSE
                            STR1V(6) <= '1'; -- PRGERR
                            STR1V(0) <= '1'; -- RDYBSY
                        END IF;

                    ELSIF Instruct = WRPLB_0_0 AND WRPGEN = '1' THEN
                        PSTART <= '1', '0' AFTER 1 ns;
                        STR1V(0) <= '1'; -- RDYBSY

                    ELSIF (Instruct=WRDYB_C_1 OR Instruct=WRDYB_4_1) AND WRPGEN = '1' THEN
                        IF DYAV_in = "11111111" OR DYAV_in = "00000000" THEN
                            sect :=
                            ReturnSectorID(Address,BottomBoot,TopBoot);
                            PSTART <= '1', '0' AFTER 1 ns;
                            STR1V(0) <= '1'; -- RDYBSY
                        ELSE
                            STR1V(6) <= '1'; -- PRGERR
                            STR1V(0) <= '1'; -- RDYBSY
                        END IF;

                    ELSIF Instruct=PRDLP_0_1 AND WRPGEN='1' THEN
                        IF ASPPWD='1' AND ASPPER='1' THEN --Protection Mode not selected
                            PSTART <= '1', '0' AFTER 1 ns;
                            STR1V(0) <= '1'; -- RDYBSY
                        ELSE
                            STR1V(6) <= '1'; -- PRGERR
                            STR1V(0) <= '1'; -- RDYBSY
                        END IF;

                    ELSIF Instruct=WRDLP_0_1 AND WRPGEN='1' THEN
                        DLPV := DLPV_in;
                        STR1V(1) <= '0'; -- WRPGEN
                        STR1V_DPD <= '0';
                         WVREG    <= '0'; -- Write volatile regs

                    ELSIF Instruct = PGPWD_0_1 AND WRPGEN = '1' THEN
                        IF (ASPPWD='1' AND ASPPER='1') THEN--Protection Mode not selected
                            PSTART <= '1', '0' AFTER 5 ns;
                            STR1V(0) <= '1'; -- RDYBSY
                        ELSE
                            STR1V(6) <= '1'; -- PRGERR
                            STR1V(0) <= '1'; -- RDYBSY
                            REPORT "Password programming is not allowed" &
                                   " in Password Protection Mode."
                            SEVERITY warning;
                        END IF;

                    ELSIF Instruct = PWDUL_0_1 THEN
                        IF RDYBSY = '0'  THEN
                            PASSULCK_in <= '1';
                            STR1V(0) <= '1'; -- RDYBSY
                        ELSE
                            REPORT "The PASSU command cannot be accepted" &
                                   " any faster than once every 100us"
                            SEVERITY warning;
                        END IF;

                    ELSIF Instruct = DICHK_4_1 THEN
                        IF (DIC_End_Addr_reg >= DIC_Start_Addr_reg + 3) THEN
                            DICSTART <= '1', '0' AFTER 1 ns;
                            STR1V(0) <= '1';  -- RDYBSY
                            DICRCA <= '0';
                            DCRV := (OTHERS => '0');
                        ELSE
                            -- Abort DIC calculation
                            ASSERT FALSE
                            REPORT "DIC EndAddr is not StartAddr+3 " &
                                    "or greater; DIC calculation is aborted"
                            SEVERITY WARNING;
                            DICRCA  <= '1';
                        END IF;
                    ELSIF Instruct = SPEPD_0_0 AND START_T1_in = '0' THEN
                        START_T1_in <= '1';

                    ELSIF Instruct = CLECC_0_0 THEN
                        ECSV(4) := '0';
                        ECSV(3) := '0';
                        ECTV := "0000000000000000";
                        EATV := "00000000000000000000000000000000";

                    ELSIF Instruct = CLPEF_0_0 THEN
                        STR1V(6) <= '0';-- PRGERR
                        STR1V(5) <= '0';-- ERSERR
                        STR1V(0) <= '0';-- RDYBSY
                    ELSIF Instruct = SEERC_C_0 THEN
                        sect := ReturnSectorID
                                (Address,BottomBoot,TopBoot);
                        SectorErased  := sect;
                        SectorSuspend <= Address/(SecSize256+1);
                        Addr := Address;
                        SEERC_START <= '1', '0' AFTER 5 ns;
                        STR1V(0) <= '1'; --RDYBSY
                        

                    ELSIF (Instruct = ENDPD_0_0) THEN --???
                        DPD_in  <= '1';
                    END IF;
                    

                    IF Instruct = SRSTE_0_0 THEN
                        RESET_EN <= '1';
                    ELSE
                        RESET_EN <= '0';
                    END IF;

                ELSIF oe_z THEN
                    IF Instruct = RDAY1_C_0 OR Instruct = RDAY1_4_0 THEN
                        rd_fast <= false;
                        rd_fast1 <= false;
                        rd_slow <= true;
                        dual    <= false;
                        ddr     <= false;
                    ELSIF Instruct = RDAY7_C_0 OR Instruct = RDAY7_4_0 OR Instruct = PKRD4_4_0 THEN
                        rd_fast <= false;
                        rd_fast1 <= false;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= true;
                    ELSIF ((Instruct = RDAY4_C_0 OR Instruct = RDAY4_4_0 OR Instruct = PKRD2_4_0) 
                           AND QUADIT = '1') THEN
                        rd_fast   <= false;
                        rd_fast1  <= true;
                        rd_slow   <= false;
                        dual      <= true;
                        ddr       <= false;
                    ELSIF Instruct = RDAY3_C_0 OR Instruct = RDAY3_4_0 OR
                    ((Instruct = RDAY5_C_0 OR Instruct = RDAY5_4_0 OR Instruct = PKRD3_4_0) AND QUADIT = '1') THEN
                        rd_fast   <= true;
                        rd_fast1  <= false;
                        rd_slow   <= false;
                        dual      <= true;
                        ddr       <= false;
                     ELSIF Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR
                    Instruct = RDCR1_0_0 OR Instruct = RDARG_C_0 THEN
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                        END IF;
                    ELSE
                        IF QPI_IT = '1' THEN
                            rd_fast <= true;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                        ELSE
                            rd_fast <= true;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                        END IF;
                    END IF;

                ELSIF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDAY1_C_0 OR Instruct = RDAY1_4_0 THEN
                       -- Read Memory array
                        rd_fast <= false;
                        rd_fast1 <= false;
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
                        
                    ELSIF Instruct = RDHL0_0_0 OR Instruct = RDHL1_0_0 THEN
                       -- Read Memory array
                        rd_fast   <= true;
                        rd_fast1  <= false;
                        rd_slow   <= false;
                        dual      <= false;
                        ddr       <= false;

                        IF (QPI_IT = '1') THEN
                            IF Instruct = RDHL0_0_0 THEN
                                data_out := to_slv(RDHL0Buf(read_addr),8);
                            ELSE
                                data_out := to_slv(RDHL1Buf(read_addr),8);
                            END IF;

                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd       <= data_out(6-4*read_cnt);
                            SOut_zd           <= data_out(5-4*read_cnt);
                            SIOut_zd          <= data_out(4-4*read_cnt);

                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                                IF read_addr = 31 THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;

                        ELSE
                            IF Instruct = RDHL0_0_0 THEN
                                data_out := to_slv(RDHL0Buf(read_addr),8);
                            ELSE
                                data_out := to_slv(RDHL1Buf(read_addr),8);
                            END IF;

                            SOut_zd <= data_out(7-read_cnt);

                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                IF read_addr = 31 THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;    
                        END IF;

                    ELSIF Instruct = PKRD1_4_0 THEN
                       -- Read Memory array
                        rd_fast   <= true;
                        rd_fast1  <= false;
                        rd_slow   <= false;
                        dual      <= false;
                        ddr       <= false;

                        IF (QPI_IT = '1') THEN
                            IF WByteCrypto(read_addr) /= -1 THEN
                                data_out := to_slv(WByteCrypto(read_addr),8);

                                IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                                WPNegOut_zd       <= data_out(6-4*read_cnt);
                                SOut_zd           <= data_out(5-4*read_cnt);
                                SIOut_zd          <= data_out(4-4*read_cnt);

                            ELSE
                                IO3RESETNegOut_zd <= 'X';
                                WPNegOut_zd       <= 'X';
                                SOut_zd           <= 'X';
                                SIOut_zd          <= 'X';
                            END IF;

                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                                IF read_addr = AddrRANGE THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;

                                IF (read_addr = CryptoPacketSize+1) THEN
                                    read_addr := 0;
                                END IF;
                            END IF;

                        ELSE
                            IF WByteCrypto(read_addr) /= -1 THEN
                                data_out := to_slv(WByteCrypto(read_addr),8);
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
                        
                    ELSIF (QPI_IT = '1' AND Instruct = RDAY2_4_0) THEN 
                        rd_fast <= true;
                        rd_fast1 <= false;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                         
                         IF bus_cycle_state = DUMMY_BYTES THEN
                            dlp_act := Return_DLP_SDR(Latency_code,dummy_cnt);
                            -- Data Learning Pattern (DLP) is enabled
                            -- Optional DLP
                            IF DLPV /= "00000000" AND dlp_act = true THEN
                                IO3RESETNegOut_zd <= DLPV(7-read_cnt);
                                WPNegOut_zd   <= DLPV(7-read_cnt);
                                SOut_zd       <= DLPV(7-read_cnt);
                                SIOut_zd      <= DLPV(7-read_cnt);
                                dlp_act := FALSE;
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                END IF;
                            END IF;
                        ELSE
                            IF Mem(read_addr) /= -1 THEN
                                data_out := to_slv(Mem(read_addr),8);
                                IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                                WPNegOut_zd   <= data_out(6-4*read_cnt);
                                SOut_zd       <= data_out(5-4*read_cnt);
                                SIOut_zd      <= data_out(4-4*read_cnt);
                            ELSE
                                data_out := to_slv(Mem(read_addr),8);
                                IO3RESETNegOut_zd <= 'X';
                                WPNegOut_zd   <= 'X';
                                SOut_zd       <= 'X';
                                SIOut_zd      <= 'X';
                            END IF;
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                                IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                ELSE
                                    read_addr := read_addr + 1;
                                    IF read_addr MOD WrapLength = 0 THEN
                                        read_addr := read_addr - WrapLength;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                        
                        
                    ELSIF Instruct = RDAY2_C_0 OR Instruct = RDAY2_4_0 THEN 
                       -- Read Memory array
                        rd_fast <= true;
                        rd_fast1 <= false;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                        IF bus_cycle_state = DUMMY_BYTES AND 
                           QPI_IT = '0' AND Instruct = RDAY2_4_0 THEN 
                            dlp_act := Return_DLP_SDR(Latency_code,dummy_cnt);
                            -- Data Learning Pattern (DLP) is enabled
                            -- Optional DLP
                            IF DLPV /= "00000000" AND dlp_act = true THEN
                                SOut_zd       <= DLPV(7-read_cnt);
                                dlp_act := FALSE;
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                END IF;
                            END IF;
                        ELSE
                            IF Mem(read_addr) /= -1 THEN
                                data_out := to_slv(Mem(read_addr),8);
                                SOut_zd <= data_out(7-read_cnt);
                            ELSE
                                SOut_zd <= 'X';
                            END IF;
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                IF CFR4V(4) = '0' THEN
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                ELSE
                                    read_addr := read_addr + 1;
                                    IF read_addr mod WrapLength = 0 THEN
                                        read_addr := read_addr - WrapLength;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDAY3_C_0 OR Instruct = RDAY3_4_0 THEN
                       -- Read Memory array
                        rd_fast <= true;
                        rd_fast1 <= false;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                        data_out := to_slv(Mem(read_addr),8);
                        SOut_zd  <= data_out(7-2*read_cnt);
                        SIOut_zd <= data_out(6-2*read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 4 THEN
                            read_cnt := 0;

                            IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                IF read_addr = AddrRANGE THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            ELSE
                                read_addr := read_addr + 1;
                                IF read_addr MOD WrapLength = 0 THEN
                                    read_addr := read_addr - WrapLength;
                                END IF;
                            END IF;
                        END IF;

                    ELSIF (Instruct = RDAY4_C_0 OR Instruct = RDAY4_4_0) AND QUADIT = '1' THEN 
                         rd_fast <= false;
                         rd_fast1 <= true;
                         rd_slow <= false;
                         dual    <= true;
                         ddr     <= false;
                         IF bus_cycle_state = DUMMY_BYTES THEN
                             dlp_act := Return_DLP_SDR(Latency_code,dummy_cnt);
                             -- Data Learning Pattern (DLP) is enabled
                             -- Optional DLP
                             IF DLPV /= "00000000" AND dlp_act = true THEN
                                 IO3RESETNegOut_zd <= DLPV(7-read_cnt);
                                 WPNegOut_zd   <= DLPV(7-read_cnt);
                                 SOut_zd       <= DLPV(7-read_cnt);
                                 SIOut_zd      <= DLPV(7-read_cnt);
                                 dlp_act := FALSE;
                                 read_cnt := read_cnt + 1;
                                 IF read_cnt = 8 THEN
                                     read_cnt := 0;
                                 END IF;
                             END IF;
                         ELSE
                             data_out := to_slv(Mem(read_addr),8);
                             IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                             WPNegOut_zd   <= data_out(6-4*read_cnt);
                             SOut_zd       <= data_out(5-4*read_cnt);
                             SIOut_zd      <= data_out(4-4*read_cnt);
                             read_cnt := read_cnt + 1;
                             IF read_cnt = 2 THEN
                             read_cnt := 0;
                                IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                   IF read_addr = AddrRANGE THEN
                                       read_addr := 0;
                                   ELSE
                                       read_addr := read_addr + 1;
                                   END IF;
                                ELSE
                                   read_addr := read_addr + 1;
                                   IF read_addr MOD WrapLength = 0 THEN
                                       read_addr := read_addr - WrapLength;
                                   END IF;
                                END IF;
                             END IF;
                         END IF;
                         
                         ELSIF (Instruct = PKRD2_4_0) AND QUAD_QPI = '1' THEN 
                         rd_fast  <= false;
                         rd_fast1 <= true;
                         rd_slow  <= false;
                         dual     <= true;
                         ddr      <= false;

                         data_out := to_slv(WByteCrypto(read_addr),8);
                         IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                         WPNegOut_zd   <= data_out(6-4*read_cnt);
                         SOut_zd       <= data_out(5-4*read_cnt);
                         SIOut_zd      <= data_out(4-4*read_cnt);
                         read_cnt := read_cnt + 1;
                         IF read_cnt = 2 THEN
                         read_cnt := 0;
                            IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                               IF read_addr = AddrRANGE THEN
                                   read_addr := 0;
                               ELSE
                                   read_addr := read_addr + 1;
                               END IF;
                            ELSE
                               read_addr := read_addr + 1;
                               IF read_addr MOD WrapLength = 0 THEN
                                   read_addr := read_addr - WrapLength;
                               END IF;
                            END IF;
                         END IF;

                    ELSIF (Instruct = RDAY5_C_0 OR Instruct = RDAY5_4_0 OR Instruct = PKRD3_4_0 OR
                    Instruct=RDAY7_C_0 OR Instruct=RDAY7_4_0) AND QUADIT='1' THEN
                        IF Instruct = RDAY7_C_0 OR Instruct = RDAY7_4_0 THEN
                            rd_fast <= false;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= true;
                        ELSE
                            rd_fast <= true;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                        END IF;
                        IF bus_cycle_state = DUMMY_BYTES THEN
                            IF (Instruct = RDAY7_C_0 OR Instruct = RDAY7_4_0) THEN
                                dlp_act := Return_DLP(Latency_code,dummy_cnt);
                                -- Data Learning Pattern (DLP) is enabled
                                -- Optional DLP
                                IF DLPV /= "00000000" AND dlp_act = true THEN
                                    IO3RESETNegOut_zd <= DLPV(7-read_cnt);
                                    WPNegOut_zd   <= DLPV(7-read_cnt);
                                    SOut_zd       <= DLPV(7-read_cnt);
                                    SIOut_zd      <= DLPV(7-read_cnt);
                                    dlp_act := FALSE;
                                    read_cnt := read_cnt + 1;
                                    IF read_cnt = 8 THEN
                                        read_cnt := 0;
                                    END IF;
                                END IF;
                            ELSIF (Instruct = RDAY5_4_0 OR Instruct = RDAY5_C_0) THEN
                                dlp_act := Return_DLP_SDR(Latency_code,dummy_cnt);
                                -- Data Learning Pattern (DLP) is enabled
                                -- Optional DLP
                                IF DLPV /= "00000000" AND dlp_act = true THEN
                                    IO3RESETNegOut_zd <= DLPV(7-read_cnt);
                                    WPNegOut_zd   <= DLPV(7-read_cnt);
                                    SOut_zd       <= DLPV(7-read_cnt);
                                    SIOut_zd      <= DLPV(7-read_cnt);
                                    dlp_act := FALSE;
                                    read_cnt := read_cnt + 1;
                                    IF read_cnt = 8 THEN
                                        read_cnt := 0;
                                    END IF;
                                END IF;
                            END IF;
                        ELSE
                            data_out := to_slv(Mem(read_addr),8);
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                                IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                ELSE
                                    read_addr := read_addr + 1;
                                    IF read_addr MOD WrapLength = 0 THEN
                                        read_addr := read_addr - WrapLength;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                        
                    ELSIF (Instruct = PKRD3_4_0 OR Instruct = PKRD4_4_0) AND QUADIT='1' THEN
                        IF Instruct = PKRD4_4_0 THEN
                            rd_fast   <= false;
                            rd_fast1  <= false;
                            rd_slow   <= false;
                            dual      <= true;
                            ddr       <= true;
                        ELSE
                            rd_fast   <= true;
                            rd_fast1  <= false;
                            rd_slow   <= false;
                            dual      <= true;
                            ddr       <= false;
                        END IF;
--                        IF bus_cycle_state = DUMMY_BYTES THEN
--                            IF (Instruct = PKRD4_4_0) THEN
--                                dlp_act := Return_DLP(Latency_code,dummy_cnt);
--                                -- Data Learning Pattern (DLP) is enabled
--                                -- Optional DLP
--                                IF DLPV /= "00000000" AND dlp_act = true THEN
--                                    IO3RESETNegOut_zd <= DLPV(7-read_cnt);
--                                    WPNegOut_zd   <= DLPV(7-read_cnt);
--                                    SOut_zd       <= DLPV(7-read_cnt);
--                                    SIOut_zd      <= DLPV(7-read_cnt);
--                                    dlp_act := FALSE;
--                                    read_cnt := read_cnt + 1;
--                                    IF read_cnt = 8 THEN
--                                        read_cnt := 0;
--                                    END IF;
--                                END IF;
--                            ELSIF (Instruct = PKRD3_4_0) THEN
--                                dlp_act := Return_DLP_SDR(Latency_code,dummy_cnt);
--                                -- Data Learning Pattern (DLP) is enabled
--                                -- Optional DLP
--                                IF DLPV /= "00000000" AND dlp_act = true THEN
--                                    IO3RESETNegOut_zd <= DLPV(7-read_cnt);
--                                    WPNegOut_zd   <= DLPV(7-read_cnt);
--                                    SOut_zd       <= DLPV(7-read_cnt);
--                                    SIOut_zd      <= DLPV(7-read_cnt);
--                                    dlp_act := FALSE;
--                                    read_cnt := read_cnt + 1;
--                                    IF read_cnt = 8 THEN
--                                        read_cnt := 0;
--                                    END IF;
--                                END IF;
--                            END IF;
--                        ELSE
                            data_out := to_slv(WByteCrypto(read_addr),8);
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                                    IF read_addr = CryptoPacketSize THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
--                                ELSE
--                                    read_addr := read_addr + 1;
--                                    IF read_addr MOD WrapLength = 0 THEN
--                                        read_addr := read_addr - WrapLength;
--                                    END IF;
--                                END IF;
                            END IF;
--                        END IF;    

                    ELSIF Instruct = RDSSR_C_0 THEN
                        IF (read_addr>=OTPLoAddr) AND (read_addr<=OTPHiAddr) THEN
                        -- Read OTP Memory array
                            IF QPI_IT = '1' THEN
                                rd_fast <= true;
                                rd_fast1 <= false;
                                rd_slow <= false;
                                dual    <= true;
                                ddr     <= false;
                                data_out := to_slv(OTPMem(read_addr),8);
                                IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                                WPNegOut_zd   <= data_out(6-4*read_cnt);
                                SOut_zd       <= data_out(5-4*read_cnt);
                                SIOut_zd      <= data_out(4-4*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                    read_cnt := 0;
                                    read_addr := read_addr + 1;
                                END IF;
                            ELSE
                                rd_fast <= true;
                                rd_fast1 <= false;
                                rd_slow <= false;
                                dual    <= false;
                                ddr     <= false;
                                data_out := to_slv(OTPMem(read_addr),8);
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
                            IF QPI_IT = '1' THEN
                                IO3RESETNegOut_zd <= 'X';
                                WPNegOut_zd   <= 'X';
                                SOut_zd       <= 'X';
                                SIOut_zd      <= 'X';
                            ELSE
                                SOut_zd <= 'X';
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                END IF;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDIDN_0_0 THEN
                        IF QPI_IT = '1' THEN
                            rd_fast <= true;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            IF (read_addr <= MDIDHiAddr) THEN
                                data_out(7 DOWNTO 0) := to_slv(MDID_array(read_addr),8);
                                IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                                WPNegOut_zd   <= data_out(6-4*read_cnt);
                                SOut_zd       <= data_out(5-4*read_cnt);
                                SIOut_zd      <= data_out(4-4*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                    read_cnt := 0;
                                END IF;
                            END IF;
                        ELSE
                            rd_fast <= true;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            IF (read_addr <= MDIDHiAddr) THEN
                                data_out(7 DOWNTO 0) := to_slv(MDID_array(read_addr),8);
                                SOut_zd       <= data_out(7-read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                END IF;
                            END IF;
                        END IF;
                        
                    ELSIF Instruct = RDUID_0_0 THEN
                        IF QPI_IT = '1' THEN
                            rd_fast <= true;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            IF (Addr_idcfi <= DeviceUID) THEN
                                data_out(7 DOWNTO 0) := UID_reg(8*Addr_idcfi+7 downto 8*Addr_idcfi);
                                IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                                WPNegOut_zd   <= data_out(6-4*read_cnt);
                                SOut_zd       <= data_out(5-4*read_cnt);
                                SIOut_zd      <= data_out(4-4*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                    read_cnt := 0;
                                    Addr_idcfi := Addr_idcfi+1;
                                END IF;
                            END IF;
                        ELSE
                            rd_fast <= true;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                           IF (Addr_idcfi <= DeviceUID) THEN
                                data_out(7 DOWNTO 0) := UID_reg(8*Addr_idcfi+7 downto 8*Addr_idcfi);
                                SOut_zd       <= data_out(7-read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                    Addr_idcfi := Addr_idcfi+1;
                                END IF;
                            END IF;
                        END IF;
                        

                    ELSIF Instruct = RDQID_0_0 AND QUADIT = '1' THEN
                        rd_fast <= true;
                        rd_fast1 <= false;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                        IF (read_addr <= MDIDHiAddr) THEN
                            data_out(7 DOWNTO 0) := to_slv(MDID_array(read_addr),8);
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                                Addr_idcfi := Addr_idcfi+1;
                            END IF;
                        END IF;

                    ELSIF Instruct = RSFDP_3_0 THEN
                        IF QPI_IT = '1' THEN
                            IF (read_addr <= SFDPHiAddr) THEN
                                data_out(7 DOWNTO 0) := to_slv(SFDP_array(read_addr),8);
                                IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                                WPNegOut_zd   <= data_out(6-4*read_cnt);
                                SOut_zd       <= data_out(5-4*read_cnt);
                                SIOut_zd      <= data_out(4-4*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                    read_cnt := 0;
                                    read_addr := read_addr+1;
                                END IF;
                            ELSE
                            -- Continued shifting of output beyond the end of
                            -- the defined ID-CFI address space will
                            -- provide undefined data.
                                IO3RESETNegOut_zd <= 'X';
                                WPNegOut_zd   <= 'X';
                                SOut_zd       <= 'X';
                                SIOut_zd      <= 'X';
                            END IF;
                        ELSE
                            IF (read_addr <= SFDPHiAddr) THEN
                                data_out(7 DOWNTO 0) := to_slv(SFDP_array(read_addr),8);
                                SOut_zd <= data_out(7-read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                    read_addr := read_addr+1;
                                END IF;
                            ELSE
                            -- Continued shifting of output beyond the end of
                            -- the defined ID-CFI address space will
                            -- provide undefined data.
                                SOut_zd       <= 'X';
                            END IF;
                        END IF;

                    ELSIF Instruct = RDDLP_0_0 THEN
                        -- Read DLP
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                        IF QPI_IT = '1' THEN
                            IO3RESETNegOut_zd <= DLPV(7-4*read_cnt);
                            WPNegOut_zd   <= DLPV(6-4*read_cnt);
                            SOut_zd       <= DLPV(5-4*read_cnt);
                            SIOut_zd      <= DLPV(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            SOut_zd <= DLPV(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDECC_C_0 OR Instruct = RDECC_4_0 THEN
                        IF QPI_IT = '1' THEN
                            IO3RESETNegOut_zd <= ECSV(7-4*read_cnt);
                            WPNegOut_zd   <= ECSV(6-4*read_cnt);
                            SOut_zd       <= ECSV(5-4*read_cnt);
                            SIOut_zd      <= ECSV(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            SOut_zd <= ECSV(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDDYB_C_0 OR Instruct = RDDYB_4_0 THEN
                        --Read DYB Access Register
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                        sect := ReturnSectorID(Address,BottomBoot,TopBoot);
  
                        IF DYB_bits(sect) = '1' THEN
                            DYAV(7 downto 0) := "11111111";
                        ELSE
                            DYAV(7 downto 0) := "00000000";
                        END IF;

                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            IO3RESETNegOut_zd <= DYAV(7-4*read_cnt);
                            WPNegOut_zd   <= DYAV(6-4*read_cnt);
                            SOut_zd       <= DYAV(5-4*read_cnt);
                            SIOut_zd      <= DYAV(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= DYAV(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDPPB_C_0 OR Instruct = RDPPB_4_0 THEN
                        --Read PPB Access Register
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                        sect := ReturnSectorID(Address,BottomBoot,TopBoot);
                        IF PPB_bits(sect) = '1' THEN
                            PPAV(7 downto 0) := "11111111";
                        ELSE
                            PPAV(7 downto 0) := "00000000";
                        END IF;
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            IO3RESETNegOut_zd <= PPAV(7-4*read_cnt);
                            WPNegOut_zd   <= PPAV(6-4*read_cnt);
                            SOut_zd       <= PPAV(5-4*read_cnt);
                            SIOut_zd      <= PPAV(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            SOut_zd <= PPAV(7-read_cnt); 
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                               read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDPLB_0_0 THEN
                        --Read PPB Lock Register
                            
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            IO3RESETNegOut_zd <= PPLV(7-4*read_cnt);
                            WPNegOut_zd   <= PPLV(6-4*read_cnt);
                            SOut_zd       <= PPLV(5-4*read_cnt);
                            SIOut_zd      <= PPLV(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= PPLV(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                               read_cnt := 0;
                            END IF;
                        END IF;
                   END IF;
                END IF;

            WHEN AUTOBOOT       =>
                IF start_autoboot = '1' THEN
                    IF (oe) THEN
                        any_read <= true;
                        IF QPI_IT = '1' THEN
                            IF ABSD > 0 THEN      --If ABSD > 0,
                                rd_fast <= false; --max SCK frequency is 104MHz
                            rd_fast1 <= false;
                                rd_slow <= false;
                                dual    <= true;
                                ddr     <= false;
                            ELSE -- If ABSD = 0, max SCK frequency is 50 MHz
                                rd_fast <= false;
                                rd_fast1 <= false;
                                rd_slow <= true;
                                dual    <= false;
                                ddr     <= false;
                            END IF;

                            data_out := to_slv(Mem(read_addr),8);
                            IO3RESETNegOut_zd  <= data_out(7-4*read_cnt);
                            WPNegOut_zd     <= data_out(6-4*read_cnt);
                            SOut_zd         <= data_out(5-4*read_cnt);
                            SIOut_zd        <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                                read_addr := read_addr + 1;
                            END IF;
                        ELSE
                            IF ABSD > 0 THEN      --If ABSD > 0,
                                rd_fast <= true; --max SCK frequency is 133MHz
                                rd_fast1 <= false;
                                rd_slow <= false;
                                dual    <= false;
                                ddr     <= false;
                            ELSE -- If ABSD = 0, max SCK frequency is 50 MHz
                                rd_fast <= false;
                                rd_fast1 <= false;
                                rd_slow <= true;
                                dual    <= false;
                                ddr     <= false;
                            END IF;

                            data_out := to_slv(Mem(read_addr),8);
                            SOut_zd <= data_out(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                read_addr := read_addr + 1;
                            END IF;
                       END IF;
                    ELSIF oe_z THEN
                       IF QPI_IT = '1' THEN
                           IF ABSD > 0 THEN      --If ABSD > 0,
                               rd_fast <= false; --max SCK frequency is 104MHz
                               rd_fast1 <= false;
                               rd_slow <= false;
                               dual    <= true;
                               ddr     <= false;
                           ELSE -- If ABSD = 0, max SCK frequency is 50 MHz
                               rd_fast <= false;
                               rd_fast1 <= false;
                               rd_slow <= true;
                               dual    <= false;
                               ddr     <= false;
                           END IF;
                       ELSE
                           IF ABSD > 0 THEN      --If ABSD > 0,
                               rd_fast <= true; --max SCK frequency is 133MHz
                               rd_fast1 <= false;
                               rd_slow <= false;
                               dual    <= false;
                               ddr     <= false;
                           ELSE -- If ABSD = 0, max SCK frequency is 50 MHz
                               rd_fast <= false;
                               rd_fast1 <= false;
                               rd_slow <= true;
                               dual    <= false;
                               ddr     <= false;
                           END IF;
                       END IF;
                       IO3RESETNegOut_zd <= 'Z';
                       WPNegOut_zd   <= 'Z';
                       SOut_zd       <= 'Z';
                       SIOut_zd      <= 'Z';
                    END IF;
                END IF;

            WHEN WRITE_SR       =>
                IF QPI_IT = '1' THEN
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= true;
                    ddr     <= false;
                ELSE
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= false;
                    ddr     <= false;
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;

                IF WDONE = '1' THEN
                    STR1V(0) <= '0'; -- RDYBSY
                    STR1V(1) <= '0'; -- WRPGEN
                    STR1V_DPD <= '0';
                    WVREG     <= '0'; --Write volatile regs
                    IF WRPGEN = '1' THEN
                        -- STCFWR bit
                        STR1N(7) <= SR1_in(7);
                        STR1V(7) <= SR1_in(7);
                        IF (PLPROT_NV='0') THEN
                            IF TLPROT = '0' THEN
                            -- The TLPROT Bit, when set to 1, locks the current
                            -- state of the LBPROT2-0 bits in Status Register,
                            -- the TBPROT and TB4KBS bits in the Config Register
                            -- As long as the TLPROT bit remains cleared to logic
                            -- '0', the other bits of the Configuration register
                            -- including TLPROT are writeable.
                        
                                    STR1N(4) <= SR1_in(4); -- LBPROT2_NV
                                    STR1N(3) <= SR1_in(3); -- LBPROT1_NV
                                    STR1N(2) <= SR1_in(2); -- LBPROT0_NV
                                    STR1V(4) <= SR1_in(4); -- LBPROT2
                                    STR1V(3) <= SR1_in(3); -- LBPROT1
                                    STR1V(2) <= SR1_in(2); -- LBPROT0
                                
                        
                                BP_bits := STR1V(4) & STR1V(3) & STR1V(2);
                                change_BP <= '1', '0' AFTER 1 ns;
                            END IF;
                        END IF;
                        
                        --CFR1N
                        IF cfg_write1 = '1' THEN
                            IF (PLPROT_NV='0') AND ASPPER = '1' THEN
                                CFR1N(4)  <= CR1_in(4);--PLPROT_NV
                                IF TLPROT = '0' THEN
                                
                                    CFR1N(6)  <= CR1_in(6);--SP4KBS_NV
                        
                                    CFR1N(5)  <= CR1_in(5);--TBPROT_NV
                        
                                    CFR1N(2)  <= CR1_in(2);--TB4KBS_NV
                                    
                                    CFR1V(0)  <= CR1_in(0);--TLPROT
                                    
                                END IF;
                            END IF;
                            CFR1N(1) <= CR1_in(1);  -- QUADIT_NV
                            CFR1V(1)  <= CR1_in(1);  -- QUADIT
                            
                        END IF;
                        
                        IF cfg_write2 = '1' THEN
                        --CFR2N
                            CFR2N(7)  <= CR2_in(7);--
                            CFR2V(7)  <= CR2_in(7);--
                            CFR2N(6)  <= CR2_in(6);--
                            CFR2V(6)  <= CR2_in(6);--
                            CFR2N(5)  <= CR2_in(6);--
                            CFR2V(5)  <= CR2_in(5);--
                            CFR2N(3 DOWNTO 0)  <= CR2_in(3 DOWNTO 0);--
                            CFR2V(3 DOWNTO 0)  <= CR2_in(3 DOWNTO 0);--
                        END IF;
                        
                        IF cfg_write3 = '1' THEN
                        --CFR3N
                            CFR3N(7)  <= CR3_in(7);--
                            CFR3V(7)  <= CR3_in(7);--
                            CFR3N(6)  <= CR3_in(7);--
                            CFR3V(6)  <= CR3_in(6);--
                            CFR3N(5)  <= CR3_in(5);--
                            CFR3V(5)  <= CR3_in(5);--
                            CFR3N(4)  <= CR3_in(4);--
                            CFR3V(4)  <= CR3_in(4);--
                            
                            IF TLPROT = '0' AND ASPPER = '1' THEN
                                CFR3N(3)  <= CR3_in(3);--UNHYSA_NV
                            END IF;
                            
                            CFR3N(2)  <= CR3_in(2);--
                            CFR3V(2)  <= CR3_in(2);--
                            CFR3N(0)  <= CR3_in(0);--
                            CFR3V(0)  <= CR3_in(0);--
                        END IF;
                        
                        IF cfg_write4 = '1' THEN
                        --CFR4N
                            CFR4N(7 DOWNTO 5)  <= CR4_in(7 DOWNTO 5);--
                            CFR4V(7 DOWNTO 5)  <= CR4_in(7 DOWNTO 5);--
                            CFR4N(4)  <= CR4_in(4);--
                            CFR4V(4)  <= CR4_in(4);--
                            CFR4N(3)  <= CR4_in(3);--
                            CFR4V(3)  <= CR4_in(3);--
                            CFR4N(2)  <= CR4_in(2);--
--                             CFR4V(2)  <= CR4_in(2);--
                            CFR4N(1 DOWNTO 0)  <= CR4_in(1 DOWNTO 0);--
                            CFR4V(1 DOWNTO 0)  <= CR4_in(1 DOWNTO 0);--
                            
                        END IF;
                        
                    ELSIF WVREG = '1' THEN
                        -- STCFWR bit
                        STR1V(7) <= SR1_in(7);
                        IF (PLPROT_NV='0') THEN
                            IF TLPROT = '0' THEN
                            -- The TLPROT Bit, when set to 1, locks the current
                            -- state of the LBPROT2-0 bits in Status Register,
                            -- the TBPROT and TB4KBS bits in the Config Register
                            -- As long as the TLPROT bit remains cleared to logic
                            -- '0', the other bits of the Configuration register
                            -- including TLPROT are writeable.
                        
                                    STR1V(4) <= SR1_in(4); -- LBPROT2
                                    STR1V(3) <= SR1_in(3); -- LBPROT1
                                    STR1V(2) <= SR1_in(2); -- LBPROT0
                                
                        
                                BP_bits := STR1V(4) & STR1V(3) & STR1V(2);
                                change_BP <= '1', '0' AFTER 1 ns;
                            END IF;
                        END IF;
                        
                        --CFR1N
                        IF cfg_write1 = '1' THEN
                            IF (PLPROT_NV='0') AND ASPPER = '1' THEN
                                IF TLPROT = '0' THEN
                                
                                    
                                    CFR1V(0)  <= CR1_in(0);--TLPROT
                                    
                                END IF;
                            END IF;
                            CFR1V(1)  <= CR1_in(1);  -- QUADIT
                            
                        END IF;
                        
                        IF cfg_write2 = '1' THEN
                        --CFR2N
                            CFR2V(7)  <= CR2_in(7);--
                            CFR2V(6)  <= CR2_in(6);--
                            CFR2V(5)  <= CR2_in(5);--
                            CFR2V(3 DOWNTO 0)  <= CR2_in(3 DOWNTO 0);--
                        END IF;
                        
                        IF cfg_write3 = '1' THEN
                        --CFR3N
                            CFR3V(7)  <= CR3_in(7);--
                            CFR3V(6)  <= CR3_in(6);--
                            CFR3V(5)  <= CR3_in(5);--
                            CFR3V(4)  <= CR3_in(4);--
                            CFR3V(0)  <= CR3_in(0);--
                        END IF;
                        
                        IF cfg_write4 = '1' THEN
                        --CFR4N
                            CFR4V(7 DOWNTO 5)  <= CR4_in(7 DOWNTO 5);--
                            CFR4V(4)  <= CR4_in(4);--
                            CFR4V(3)  <= CR4_in(3);--
                            CFR4V(1 DOWNTO 0)  <= CR4_in(1 DOWNTO 0);--
                            
                        END IF;
                    END IF;
--                     cfg_write1 <= '1','0' AFTER 1 ns;
--                     cfg_write2 <= '1','0' AFTER 1 ns;
--                     cfg_write3 <= '1','0' AFTER 1 ns;
--                     cfg_write4 <= '1','0' AFTER 1 ns;
                END IF;

            WHEN WRITE_ALL_REG       =>
                IF (Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR 
                    Instruct = RDARG_C_0) THEN
                    IF QPI_IT = '1' THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                ELSIF QPI_IT = '1' THEN
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= true;
                    ddr     <= false;
                ELSE
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= false;
                    ddr     <= false;
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;

                new_pass_byte := WRAR_reg_in;
                IF Addr = 16#0000020# THEN
                    old_pass_byte := PWDO(7 downto 0);
                ELSIF Addr = 16#0000021# THEN
                    old_pass_byte := PWDO(15 downto 8);
                ELSIF Addr = 16#0000022# THEN
                    old_pass_byte := PWDO(23 downto 16);
                ELSIF Addr = 16#0000023# THEN
                    old_pass_byte := PWDO(31 downto 24);
                ELSIF Addr = 16#0000024# THEN
                    old_pass_byte := PWDO(39 downto 32);
                ELSIF Addr = 16#0000025# THEN
                    old_pass_byte := PWDO(47 downto 40);
                ELSIF Addr = 16#0000026# THEN
                    old_pass_byte := PWDO(55 downto 48);
                ELSIF Addr = 16#0000027# THEN
                    old_pass_byte := PWDO(63 downto 56);
                END IF;
                FOR j IN 0 TO 7 LOOP
                    IF old_pass_byte(j) = '0' THEN
                        new_pass_byte(j) := '0';
                    END IF;
                END LOOP;

                IF WDONE = '1' AND CSDONE = '1' THEN
                    STR1V(0) <= '0'; -- RDYBSY
                    STR1V(1) <= '0'; -- WRPGEN
                    STR1V_DPD <= '0';
                     WVREG     <= '0'; --Write volatile regs
                    

                    IF Addr = 16#00000000# THEN -- STR1N;
                        STR1N(7) <= WRAR_reg_in(7);
                        STR1V(7) <= WRAR_reg_in(7);
                        IF PLPROT_NV='0' THEN
                            IF TLPROT = '0' THEN
                                    STR1N(4) <= WRAR_reg_in(4); -- LBPROT2_NV
                                    STR1N(3) <= WRAR_reg_in(3); -- LBPROT1_NV
                                    STR1N(2) <= WRAR_reg_in(2); -- LBPROT0_NV
                                    STR1V(4) <= WRAR_reg_in(4); -- LBPROT2
                                    STR1V(3) <= WRAR_reg_in(3); -- LBPROT1
                                    STR1V(2) <= WRAR_reg_in(2); -- LBPROT0

                                    BP_bits := STR1V(4) & STR1V(3) & STR1V(2);
                                    change_BP <= '1', '0' AFTER 1 ns;
                            END IF;
                        END IF;

                    ELSIF Addr = 16#00000002# THEN -- CFR1N;
                        IF (PLPROT_NV='0') THEN
                            IF TLPROT = '0' THEN
                            
                               IF SP4KBS_NV='0' AND INITIAL_CONFIG='0' THEN
                                    CFR1N(6) <= WRAR_reg_in(6);--SP4KBS_NV
                                    CFR1V(6)  <= WRAR_reg_in(6);--SP4KBS
                                END IF; 
                                
                                IF TBPROT_NV='0' AND INITIAL_CONFIG='0' THEN
                                    CFR1N(5) <= WRAR_reg_in(5);--TBPROT_NV
                                    CFR1V(5)  <= WRAR_reg_in(5);--TBPROT
                                END IF;


                                IF TB4KBS_NV='0' AND INITIAL_CONFIG='0' AND
                                    CFR3V(3)='0' THEN
                                    CFR1N(2) <= WRAR_reg_in(2);--TB4KBS_NV
                                    CFR1V(2)  <= WRAR_reg_in(2);--TB4KBS
                                    change_TB4KBS <= '1', '0' AFTER 1 ns;
                                END IF;
                            END IF;
                        END IF;

                            CFR1N(1) <= WRAR_reg_in(1);  -- QUADIT_NV
                            CFR1V(1)  <= WRAR_reg_in(1);  -- QUADIT

                        IF (PLPROT_NV = '0') THEN
                            CFR1N(4) <= WRAR_reg_in(4); -- PLPROT_NV
                            CFR1V(4)  <= WRAR_reg_in(4); -- PLPROT
                        END IF;

                    ELSIF Addr = 16#00000003# THEN -- CFR2N;
                       IF CFR2N(7) = '0' THEN
                            CFR2N(7) <= WRAR_reg_in(7); --  AL_NV
                            CFR2V(7)  <= WRAR_reg_in(7); --  AL
                        END IF;

                       IF CFR2N(6)='0' AND WRAR_reg_in(6)='1' THEN
                            CFR2N(6) <= WRAR_reg_in(6); --  QA_NV
                            CFR2V(6)  <= WRAR_reg_in(6); --  QA

                        END IF;

                       IF CFR2N(5) = '0' THEN
                            CFR2N(5) <= WRAR_reg_in(5); --  IO3R_NV
                            CFR2V(5)  <= WRAR_reg_in(5); --  IO3R_S
                        END IF;

                       IF CFR2N(3 downto 0) = "1000" THEN
                            CFR2N(3 downto 0) <= WRAR_reg_in(3 downto 0); -- RL_NV(3:0)
                            CFR2V(3 downto 0)  <= WRAR_reg_in(3 downto 0); -- RL(3:0)
                        END IF;

                    ELSIF Addr = 16#00000004# THEN -- CFR3N;
                    
--                         IF CFR3N(7) = '0' THEN
                            CFR3N(7) <= WRAR_reg_in(7); --  
                            CFR3V(7)  <= WRAR_reg_in(7); -- 
--                         END IF;

--                        IF CFR3N(6) = '0' THEN
                            CFR3N(6) <= WRAR_reg_in(6); --
                            CFR3V(6)  <= WRAR_reg_in(6); -- 
--                         END IF;
                        IF CFR3N(5) = '0' THEN
                            CFR3N(5) <= WRAR_reg_in(5); --  BC_NV
                            CFR3V(5)  <= WRAR_reg_in(5); --  BC_V
                        END IF;

                       IF CFR3N(4) = '0' THEN
                            CFR3N(4) <= WRAR_reg_in(4); --  02h_NV
                            CFR3V(4)  <= WRAR_reg_in(4); --  02h_V
                        END IF;

                       IF CFR3N(3) = '0' THEN
                            CFR3N(3) <= WRAR_reg_in(3); --  20_NV
                            CFR3V(3)  <= WRAR_reg_in(3); --  20_V
                        END IF;

                       IF CFR3N(2) = '0' THEN
                            CFR3N(2) <= WRAR_reg_in(2); --  30_NV
                            CFR3V(2)  <= WRAR_reg_in(2); --  30_V
                        END IF;

                       IF CFR3N(0) = '0' THEN
                            CFR3N(0) <= WRAR_reg_in(0); --  F0_NV
                            CFR3V(0)  <= WRAR_reg_in(0); --  F0_V
                        END IF;

                    ELSIF Addr = 16#00000005# THEN -- CFR4N;
                       IF CFR4N(7 downto 5) = "000" THEN
                            CFR4N(7 downto 5) <= WRAR_reg_in(7 downto 5); --  OI_O(2:0)
                            CFR4V(7 downto 5) <= WRAR_reg_in(7 downto 5); --  OI(2:0)
                        END IF;

                       IF CFR4N(4) = '0' THEN
                            CFR4N(4) <= WRAR_reg_in(4); --  WE_O
                            CFR4V(4)  <= WRAR_reg_in(4); --  WE
                        END IF;

                       IF CFR4N(1 downto 0) = "00" THEN
                            CFR4N(1 downto 0) <= WRAR_reg_in(1 downto 0); --  WL_O(1:0)
                            CFR4V(1 downto 0)  <= WRAR_reg_in(1 downto 0); --  WL(1:0)
                        END IF;

                    ELSIF Addr = 16#0000010# THEN -- DLPN;
                        IF DLPN = "00000000" THEN
                            DLPN := WRAR_reg_in;
                            DLPV  := WRAR_reg_in;
                        ELSE
                            REPORT "NVDLR bits already programmed"
                            SEVERITY warning;
                        END IF;

                    ELSIF Addr = 16#0000020# THEN -- PWDO[7:0]
                        PWDO(7 DOWNTO 0) := new_pass_byte;

                    ELSIF Addr = 16#0000021# THEN -- PWDO[15:8]
                        PWDO(15 DOWNTO 8) := new_pass_byte;

                    ELSIF Addr = 16#0000022# THEN -- PWDO[23:16]
                        PWDO(23 DOWNTO 16) := new_pass_byte;

                    ELSIF Addr = 16#0000023# THEN -- PWDO[31:24]
                        PWDO(31 DOWNTO 24) := new_pass_byte;

                    ELSIF Addr = 16#0000024# THEN -- PWDO[39:32]
                        PWDO(39 DOWNTO 32) := new_pass_byte;

                    ELSIF Addr = 16#0000025# THEN -- PWDO[47:40]
                        PWDO(47 DOWNTO 40) := new_pass_byte;

                    ELSIF Addr = 16#0000026# THEN -- PWDO[55:48]
                        PWDO(55 DOWNTO 48) := new_pass_byte;

                    ELSIF Addr = 16#0000027# THEN -- PWDO[63:56]
                        PWDO(63 DOWNTO 56) := new_pass_byte;

                    ELSIF Addr = 16#0000030# THEN -- ASP Register

                            IF ASPDYB = '0' AND WRAR_reg_in(4) = '1' THEN
                                REPORT "ASPDYB bit is already programmed"
                                SEVERITY warning;
                            ELSE
                                ASPO(4) := WRAR_reg_in(4); -- ASPDYB
                            END IF;

                            IF ASPPPB = '0' AND WRAR_reg_in(3) = '1' THEN
                                REPORT "ASPPPB bit is already programmed"
                                SEVERITY warning;
                            ELSE
                                ASPO(3) := WRAR_reg_in(3); --ASPPPB
                            END IF;

                            IF ASPPRM = '0' AND WRAR_reg_in(0) = '1' THEN
                                REPORT "ASPPRM bit is already programmed"
                                SEVERITY warning;
                            ELSE
                                ASPO(0) := WRAR_reg_in(0); --ASPPRM
                            END IF;


                        ASPO(2) := WRAR_reg_in(2); -- ASPPWD
                        ASPO(1) := WRAR_reg_in(1); -- ASPPER

                    ELSIF Addr = 16#0000031# THEN -- ASPO[15:8]
                        REPORT "RFU bits"
                        SEVERITY warning;
                    ELSIF Addr = 16#00800000# THEN --  STR1V
                        -- STCFWR bit
                        STR1V (7) <= WRAR_reg_in(7);

                       IF (PLPROT_NV='0')THEN
                           IF TLPROT = '0' THEN
                             -- The TLPROT Bit, when set to 1, locks the current
                             -- state of the LBPROT2-0 bits in Status Register.
                                    STR1V(4)  <= WRAR_reg_in(4); -- LBPROT2
                                    STR1V(3)  <= WRAR_reg_in(3); -- LBPROT1
                                    STR1V(2)  <= WRAR_reg_in(2); -- LBPROT0

                                    BP_bits := STR1V(4) & STR1V(3) & STR1V(2);

                                    change_BP <= '1', '0' AFTER 1 ns;
                            END IF;
                        END IF;
                    ELSIF Addr = 16#00800001# THEN -- STR2V
                        REPORT "Status Register 2 does not have use r" &
                            "programmable bits, all defined bits are " &
                            "volatile read only status."
                        SEVERITY warning;

                    ELSIF Addr = 16#00800002# THEN -- CFR1V
 
                        -- While Quad All mode is selected (CR2NV[1]=1 or
                        -- CR2V[1]=1) the QUAD bit cannot be cleared to 0.
                            CFR1V(1)  <= WRAR_reg_in(1); --QUADIT


                        IF TLPROT = '0' THEN
                            CFR1V(0) <= WRAR_reg_in(0); -- TLPROT
                        END IF;

                    ELSIF Addr = 16#00800003# THEN -- CFR2V
                        CFR2V(7)   <= WRAR_reg_in(7);   --  AL
                        CFR2V(6)   <= WRAR_reg_in(6);   --  QA

       
                        CFR2V(5)   <= WRAR_reg_in(5);   --  IO3R_S
                        CFR2V(3 downto 0) <= WRAR_reg_in(3 downto 0); --  RL(3:0)

                    ELSIF Addr = 16#00800004# THEN -- CFR3V
                        CFR3V(7)  <= WRAR_reg_in(7); -- 
                        CFR3V(6)  <= WRAR_reg_in(6); --  
                        CFR3V(5)  <= WRAR_reg_in(5); --  BC_V
                        CFR3V(4)  <= WRAR_reg_in(4); --  02h_V
                        CFR3V(3)  <= WRAR_reg_in(3); --  20_V
                        CFR3V(2)  <= WRAR_reg_in(2); --  30_V
                        CFR3V(0)  <= WRAR_reg_in(0); --  F0_V

                    ELSIF Addr = 16#00800005# THEN -- CFR4V
                        CFR4V(7 downto 5) <= WRAR_reg_in(7 downto 5);-- OI(2:0)
                        CFR4V(4)          <= WRAR_reg_in(4);  -- WE
                        CFR4V(3)          <= WRAR_reg_in(3);  -- ECC
                        CFR4V(1 downto 0) <= WRAR_reg_in(1 downto 0); -- WL(1:0)

                    ELSIF Addr = 16#0800010# THEN  -- DLPV
                        DLPV  := WRAR_reg_in;
                    END IF;
                END IF;

            WHEN PAGE_PG       =>
                IF (Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR 
                    Instruct = RDARG_C_0) THEN
                    IF QPI_IT = '1' THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                ELSIF QPI_IT = '1' THEN
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= true;
                    ddr     <= false;
                ELSE
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= false;
                    ddr     <= false;
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;

                IF falling_edge(PDONE) THEN
                    ADDRHILO_PG(AddrLo, AddrHi, Addr_pgm);
                    
                    IF ((Addr_pgm_ECC mod 16) + Byte_number + 1) mod 16 = 0 THEN
                        ECC_check := ((Addr_pgm_ECC mod 16) + Byte_number+1) / 16;
                    ELSE
                        ECC_check := (((Addr_pgm_ECC mod 16) + Byte_number+1) / 16) + 1;
                    END IF;
                    
                    ECC_data := Addr_pgm_ECC - (Addr_pgm_ECC mod 16);
                    cnt :=0;
                    
                    FOR i IN 0 TO (ECC_check*16-1) LOOP
                        IF (Mem(ECC_data + i - cnt) /= MaxData) THEN
                            ECC_ERR := ECC_ERR + 1;
                            IF (ECC_data + i) = AddrHi THEN
                                ECC_data := AddrLo;
                                cnt := i + 1;
                            END IF;
                        END IF;
                    END LOOP;
                    
                    cnt := 0;
                    FOR i IN 0 TO wr_cnt LOOP
                        new_int := WData(i);
                        old_int := Mem(Addr_pgm + i - cnt);

                        IF new_int > -1 THEN
                            new_bit := to_slv(new_int,8);
                            IF old_int > -1 THEN
                                old_bit := to_slv(old_int,8);
                                FOR j IN 0 TO 7 LOOP
                                    IF old_bit(j) = '0' THEN
                                        new_bit(j) := '0';
                                    END IF;
                                END LOOP;
                                new_int := to_nat(new_bit);
                            END IF;
                            WData(i) := new_int;
                        ELSE
                            WData(i) := -1;
                        END IF;

                        Mem(Addr_pgm + i - cnt) :=  -1;
                        IF (Addr_pgm + i) = AddrHi THEN
                            Addr_pgm := AddrLo;
                            cnt := i + 1;
                        END IF;
                    END LOOP;
                    cnt :=0;
                END IF;

                IF rising_edge(PDONE) THEN
                 
                    IF (((CFR4V(3) = '1')  OR (non_industrial_temp = '1')) 
                        AND (ECC_ERR > 0)) THEN
                        STR1V(0) <= '0'; --RDYBSY
                        STR1V(1) <= '0'; --WRPGEN
                        STR1V(6) <= '1'; --PRGERR
                        ECC_ERR  := 0; --
                        REPORT "WARNING: For non-industrial temperatures " &
                               "it is not allowed to have multi-programming " &
                               "without erasing previously the sector! " &
                               "multi-pass programming within the same data unit" &
                               "will result in a Program Error. "
                        SEVERITY WARNING;
                    ELSE
                        STR1V(0) <= '0'; -- RDYBSY
                        STR1V(1) <= '0';  -- WRPGEN
                        ECC_ERR  := 0; --
                    END IF;
                        
                    FOR i IN 0 TO wr_cnt LOOP
                            Mem(Addr_pgm_tmp + i - cnt) :=  WData(i);
                            IF (Addr_pgm_tmp + i) = AddrHi THEN
                                Addr_pgm_tmp := AddrLo;
                                cnt := i + 1;
                            END IF;
                    END LOOP;

                ELSIF falling_edge(write) THEN
                    IF (Instruct = SPEPA_0_0 OR Instruct = SPEPD_0_0) AND PRGSUSP_in = '0' THEN
                        IF RES_TO_SUSP_TIME = '0' THEN
                            PGSUSP <= '1', '0' AFTER 1 ns;
                            PRGSUSP_in <= '1';
                        ELSE
                            ASSERT FALSE
                            REPORT "Minimum for tPRS is not satisfied! " &
                                "PGSP command is ignored"
                            SEVERITY warning;
                        END IF;
                    END IF;
                END IF;

            WHEN PG_SUSP       =>
                IF (Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR 
                    Instruct = RDARG_C_0) THEN
                    IF QPI_IT = '1' THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                ELSIF QPI_IT = '1' THEN
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= true;
                    ddr     <= false;
                ELSE
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= false;
                    ddr     <= false;
                END IF;
                IF PRGSUSP_out = '1' AND PRGSUSP_in = '1' THEN
                    PRGSUSP_in <= '0';
                    -- The RDYBSY bit in the Status Register will indicate that
                    -- the device is ready for another operation.
                    STR1V(0) <= '0';
                    -- The Program Suspend (PS) bit in the Status Register will
                    -- be set to the logical “1” state to indicate that the
                    -- program operation has been suspended.
                    STR2V(0) <= '1';
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDAY1_C_0 OR Instruct = RDAY1_4_0 THEN
                       -- Read Memory array
                        rd_fast <= false;
                        rd_fast1 <= false;
                        rd_slow <= true;
                        dual    <= false;
                        ddr     <= false;
                        IF pgm_page /= (read_addr / (PageSize+1)) THEN
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
                        ELSE
                            SOut_zd <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                IF read_addr = AddrRANGE THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        END IF;
                    ELSIF (QPI_IT = '1' AND Instruct = RDAY2_4_0) THEN 
                        rd_fast <= true;
                        rd_fast1 <= false;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                        IF pgm_page /= (read_addr / (PageSize+1)) THEN
                         
                            IF bus_cycle_state = DUMMY_BYTES THEN
                                dlp_act := Return_DLP_SDR(Latency_code,dummy_cnt);
                                -- Data Learning Pattern (DLP) is enabled
                                -- Optional DLP
                                IF DLPV /= "00000000" AND dlp_act = true THEN
                                    IO3RESETNegOut_zd <= DLPV(7-read_cnt);
                                    WPNegOut_zd   <= DLPV(7-read_cnt);
                                    SOut_zd       <= DLPV(7-read_cnt);
                                    SIOut_zd      <= DLPV(7-read_cnt);
                                    dlp_act := FALSE;
                                    read_cnt := read_cnt + 1;
                                    IF read_cnt = 8 THEN
                                        read_cnt := 0;
                                    END IF;
                                END IF;
                            ELSE
                                data_out := to_slv(Mem(read_addr),8);
                                IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                                WPNegOut_zd   <= data_out(6-4*read_cnt);
                                SOut_zd       <= data_out(5-4*read_cnt);
                                SIOut_zd      <= data_out(4-4*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                    read_cnt := 0;
                                    IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                        IF read_addr = AddrRANGE THEN
                                            read_addr := 0;
                                        ELSE
                                            read_addr := read_addr + 1;
                                        END IF;
                                    ELSE
                                        read_addr := read_addr + 1;
                                        IF read_addr MOD WrapLength = 0 THEN
                                            read_addr := read_addr - WrapLength;
                                        END IF;
                                    END IF;
                                END IF;
                            END IF;
                        ELSE
                            data_out := to_slv(Mem(read_addr),8);
                            IO3RESETNegOut_zd <= 'X';
                            WPNegOut_zd   <= 'X';
                            SOut_zd       <= 'X';
                            SIOut_zd      <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                                IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                ELSE
                                    read_addr := read_addr + 1;
                                    IF read_addr MOD WrapLength = 0 THEN
                                        read_addr := read_addr - WrapLength;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                    ELSIF Instruct=RDAY2_C_0 OR Instruct=RDAY2_4_0 THEN 
                        IF pgm_page /= (read_addr / (PageSize+1)) THEN
                            IF bus_cycle_state = DUMMY_BYTES AND 
                               QPI_IT = '0' AND Instruct = RDAY2_4_0 THEN 
                                dlp_act := Return_DLP_SDR(Latency_code,dummy_cnt);
                                -- Data Learning Pattern (DLP) is enabled
                                -- Optional DLP
                                IF DLPV /= "00000000" AND dlp_act = true THEN
                                    SOut_zd       <= DLPV(7-read_cnt);
                                    dlp_act := FALSE;
                                    read_cnt := read_cnt + 1;
                                    IF read_cnt = 8 THEN
                                        read_cnt := 0;
                                    END IF;
                                END IF;
                            ELSE                    
                                data_out := to_slv(Mem(read_addr),8);
                                SOut_zd <= data_out(7-read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                    IF CFR4V(4) = '0' THEN
                                        IF read_addr = AddrRANGE THEN
                                            read_addr := 0;
                                        ELSE
                                            read_addr := read_addr + 1;
                                        END IF;
                                    ELSE
                                        read_addr := read_addr + 1;
                                        IF read_addr mod WrapLength = 0 THEN
                                            read_addr := read_addr - WrapLength;
                                        END IF;
                                    END IF;
                                END IF;
                            END IF;
                        ELSE
                            SOut_zd <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                IF CFR4V(4) = '0' THEN
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                ELSE
                                    read_addr := read_addr + 1;
                                    IF read_addr mod WrapLength = 0 THEN
                                        read_addr := read_addr - WrapLength;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;

                    ELSIF Instruct=RDAY3_C_0 OR Instruct=RDAY3_4_0 THEN
                        -- Read Memory array
                        rd_fast <= true;
                        rd_fast1 <= false;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                        IF pgm_page /= (read_addr / (PageSize+1)) THEN
                            data_out := to_slv(Mem(read_addr),8);
                            SOut_zd <= data_out(7-2*read_cnt);
                            SIOut_zd <= data_out(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                                IF CFR4V(4) = '0' THEN -- Burst read wrap disabled
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                ELSE
                                    read_addr := read_addr + 1;
                                    IF read_addr mod WrapLength = 0 THEN
                                        read_addr := read_addr - WrapLength;
                                    END IF;
                                END IF;
                            END IF;
                        ELSE
                            SOut_zd <= 'X';
                            SIOut_zd <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                                IF CFR4V(4) = '0' THEN -- Burst read wrap disabled
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                ELSE
                                    read_addr := read_addr + 1;
                                    IF read_addr mod WrapLength = 0 THEN
                                        read_addr := read_addr - WrapLength;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;

                    ELSIF (Instruct = RDAY4_C_0 OR Instruct = RDAY4_4_0) AND QUADIT='1' THEN 
                        -- Read Memory array
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                        IF pgm_page /= (read_addr / (PageSize+1)) THEN
                            IF bus_cycle_state = DUMMY_BYTES THEN
                                 dlp_act := Return_DLP_SDR(Latency_code,dummy_cnt);
                                 -- Data Learning Pattern (DLP) is enabled
                                 -- Optional DLP
                                 IF DLPV /= "00000000" AND dlp_act = true THEN
                                     IO3RESETNegOut_zd <= DLPV(7-read_cnt);
                                     WPNegOut_zd   <= DLPV(7-read_cnt);
                                     SOut_zd       <= DLPV(7-read_cnt);
                                     SIOut_zd      <= DLPV(7-read_cnt);
                                     dlp_act := FALSE;
                                     read_cnt := read_cnt + 1;
                                     IF read_cnt = 8 THEN
                                         read_cnt := 0;
                                     END IF;
                                 END IF;
                             ELSE
                               data_out := to_slv(Mem(read_addr),8);
                               IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                               WPNegOut_zd   <= data_out(6-4*read_cnt);
                               SOut_zd       <= data_out(5-4*read_cnt);
                               SIOut_zd      <= data_out(4-4*read_cnt);
                               read_cnt := read_cnt + 1;
                               IF read_cnt = 2 THEN
                               read_cnt := 0;
                                  IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                     IF read_addr = AddrRANGE THEN
                                         read_addr := 0;
                                     ELSE
                                         read_addr := read_addr + 1;
                                     END IF;
                                  ELSE
                                     read_addr := read_addr + 1;
                                     IF read_addr MOD WrapLength = 0 THEN
                                         read_addr := read_addr - WrapLength;
                                     END IF;
                                  END IF;
                               END IF;
                           END IF;
                        ELSE
                            IO3RESETNegOut_zd <= 'X';
                            WPNegOut_zd   <= 'X';
                            SOut_zd       <= 'X';
                            SIOut_zd      <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                            read_cnt := 0;
                               IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                  IF read_addr = AddrRANGE THEN
                                      read_addr := 0;
                                  ELSE
                                      read_addr := read_addr + 1;
                                  END IF;
                               ELSE
                                  read_addr := read_addr + 1;
                                  IF read_addr MOD WrapLength = 0 THEN
                                      read_addr := read_addr - WrapLength;
                                  END IF;
                               END IF;
                            END IF;
                        END IF;

                    ELSIF (Instruct = RDAY5_C_0 OR Instruct = RDAY5_4_0 OR
                    Instruct=RDAY7_C_0 OR Instruct=RDAY7_4_0) AND QUADIT='1' THEN
                        IF Instruct = RDAY7_C_0 OR Instruct = RDAY7_4_0  THEN
                            rd_fast <= false;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= true;
                        ELSE
                            rd_fast <= true;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                        END IF;
                        IF pgm_page /= (read_addr / (PageSize+1)) THEN
                            IF bus_cycle_state = DUMMY_BYTES THEN
                                IF (Instruct = RDAY7_C_0 OR Instruct = RDAY7_4_0) THEN
                                    dlp_act := Return_DLP(Latency_code,dummy_cnt);
                                    -- Data Learning Pattern (DLP) is enabled
                                    -- Optional DLP
                                    IF DLPV /= "00000000" AND dlp_act = true THEN
                                        IO3RESETNegOut_zd <= DLPV(7-read_cnt);
                                        WPNegOut_zd   <= DLPV(7-read_cnt);
                                        SOut_zd       <= DLPV(7-read_cnt);
                                        SIOut_zd      <= DLPV(7-read_cnt);
                                        dlp_act := FALSE;
                                        read_cnt := read_cnt + 1;
                                        IF read_cnt = 8 THEN
                                            read_cnt := 0;
                                        END IF;
                                    END IF;
                                    ELSIF (Instruct = RDAY5_4_0 OR Instruct = RDAY5_C_0) THEN
                                    dlp_act := Return_DLP_SDR(Latency_code,dummy_cnt);
                                    -- Data Learning Pattern (DLP) is enabled
                                    -- Optional DLP
                                    IF DLPV /= "00000000" AND dlp_act = true THEN
                                        IO3RESETNegOut_zd <= DLPV(7-read_cnt);
                                        WPNegOut_zd   <= DLPV(7-read_cnt);
                                        SOut_zd       <= DLPV(7-read_cnt);
                                        SIOut_zd      <= DLPV(7-read_cnt);
                                        dlp_act := FALSE;
                                        read_cnt := read_cnt + 1;
                                        IF read_cnt = 8 THEN
                                            read_cnt := 0;
                                        END IF;
                                    END IF;
                                END IF;
                            ELSE
                                data_out := to_slv(Mem(read_addr),8);
                                IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                                WPNegOut_zd   <= data_out(6-4*read_cnt);
                                SOut_zd       <= data_out(5-4*read_cnt);
                                SIOut_zd      <= data_out(4-4*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                    read_cnt := 0;
                                    IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                        IF read_addr = AddrRANGE THEN
                                            read_addr := 0;
                                        ELSE
                                            read_addr := read_addr + 1;
                                        END IF;
                                    ELSE
                                        read_addr := read_addr + 1;
                                        IF read_addr MOD WrapLength = 0 THEN
                                            read_addr := read_addr - WrapLength;
                                        END IF;
                                    END IF;
                                END IF;
                            END IF;
                        ELSE
                            IO3RESETNegOut_zd <= 'X';
                            WPNegOut_zd   <= 'X';
                            SOut_zd <= 'X';
                            SIOut_zd <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                                IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                ELSE
                                    read_addr := read_addr + 1;
                                    IF read_addr MOD WrapLength = 0 THEN
                                        read_addr := read_addr - WrapLength;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    IF Instruct = RDAY1_C_0 OR Instruct = RDAY1_4_0 THEN
                        rd_fast <= false;
                        rd_fast1 <= false;
                        rd_slow <= true;
                        dual    <= false;
                        ddr     <= false;
                    ELSIF ((Instruct = RDAY4_C_0 OR Instruct = RDAY4_4_0) 
                           AND QUADIT = '1') THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSIF Instruct = RDAY3_C_0 OR Instruct = RDAY3_4_0 OR
                    Instruct = RDAY5_C_0 OR Instruct = RDAY5_4_0 THEN
                        rd_fast <= false;
                        rd_fast1 <= false;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSIF Instruct = RDAY7_C_0 OR Instruct = RDAY7_4_0 THEN
                        rd_fast <= false;
                        rd_fast1 <= false;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= true;
                    ELSE
                        IF QPI_IT = '1' THEN
                            rd_fast <= true;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                        ELSE
                            rd_fast <= true;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                        END IF;
                    END IF;
                END IF;

                IF falling_edge(write) THEN
                    IF Instruct = RSEPA_0_0 OR Instruct = RSEPD_0_0 THEN
                        STR2V(0) <= '0'; -- PS
                        STR1V(0) <= '1'; -- RDYBSY
                        PGRES  <= '1', '0' AFTER 1 ns;
                        RES_TO_SUSP_TIME <= '1', '0' AFTER res_time; -- 100us

                    ELSIF Instruct = CLECC_0_0 THEN
                        ECSV(4) := '0';
                        ECSV(3) := '0';
                        ECTV := "0000000000000000";

                    ELSIF Instruct = CLPEF_0_0 THEN
                        STR1V(6) <= '0';-- PRGERR
                        STR1V(5) <= '0';-- ERSERR
                        STR1V(0) <= '0';-- RDYBSY
                    END IF;

                    IF Instruct = SRSTE_0_0 THEN
                        RESET_EN <= '1';
                    ELSE
                        RESET_EN <= '0';
                    END IF;
                END IF;

            WHEN OTP_PG       =>
                IF (Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR 
                    Instruct = RDARG_C_0) THEN
                    IF QPI_IT = '1' THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                ELSE
                rd_fast <= true;
                rd_fast1 <= false;
                rd_slow <= false;
                dual    <= false;
                ddr     <= false;
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;      
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            --Read Status Register 1
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            READ_ALL_REG(RDAR_reg, read_addr);
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            READ_ALL_REG(RDAR_reg, read_addr);
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;

                IF falling_edge(PDONE) THEN
                    IF ((Addr_pgm_ECC mod 16) + Byte_number + 1) mod 16 = 0 THEN
                        ECC_check := ((Addr_pgm_ECC mod 16) + Byte_number+1) / 16;
                    ELSE
                        ECC_check := (((Addr_pgm_ECC mod 16) + Byte_number+1) / 16) + 1;
                    END IF;
                    
                    ECC_data := Addr_pgm_ECC - (Addr_pgm_ECC mod 16);
                    cnt :=0;
                    
                    FOR i IN 0 TO (ECC_check*16-1) LOOP
                        IF (Mem(ECC_data + i - cnt) /= MaxData) THEN
                            ECC_ERR := ECC_ERR + 1;

                        END IF;
                    END LOOP;
                    
                    cnt := 0;
                    FOR i IN 0 TO wr_cnt LOOP
                        new_int := WData(i);
                        old_int := OTPMem(Addr_pgm + i);
                        IF new_int > -1 THEN
                            new_bit := to_slv(new_int,8);
                            IF old_int > -1 THEN
                                old_bit := to_slv(old_int,8);
                                FOR j IN 0 TO 7 LOOP
                                    IF old_bit(j) = '0' THEN
                                        new_bit(j) := '0';
                                    END IF;
                                END LOOP;
                                new_int := to_nat(new_bit);
                            END IF;
                            WData(i) := new_int;
                        ELSE
                            WData(i) := -1;
                        END IF;

                        OTPMem(Addr_pgm + i) :=  -1;
                    END LOOP;
                END IF;

                IF rising_edge(PDONE) THEN
                    IF (((CFR4V(3) = '1')  OR (non_industrial_temp = '1')) 
                        AND (ECC_ERR > 0)) THEN
                        STR1V(0) <= '0'; --RDYBSY
                        STR1V(1) <= '0'; --WRPGEN
                        STR1V(6) <= '1'; --PRGERR
                        ECC_ERR  := 0; --
                        REPORT "WARNING: For non-industrial temperatures " &
                               "it is not allowed to have multi-programming " &
                               "without erasing previously the sector! " &
                               "multi-pass programming within the same data unit" &
                               "will result in a Program Error. "
                        SEVERITY WARNING;
                    ELSE
                        STR1V(0) <= '0'; -- RDYBSY
                        STR1V(1) <= '0';  -- WRPGEN
                        ECC_ERR  := 0; --
                    END IF;
                    FOR i IN 0 TO wr_cnt LOOP
                        OTPMem(Addr_pgm + i) := WData(i);
                    END LOOP;
                    LOCK_BYTE1 := to_slv(OTPMem(16#10#),8);
                    LOCK_BYTE2 := to_slv(OTPMem(16#11#),8);
                    LOCK_BYTE3 := to_slv(OTPMem(16#12#),8);
                    LOCK_BYTE4 := to_slv(OTPMem(16#13#),8);
                END IF;

            WHEN DIC_Calc =>
                IF (Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR 
                    Instruct = RDARG_C_0) THEN
                    IF QPI_IT = '1' THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                ELSIF QPI_IT = '1' THEN
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= true;
                    ddr     <= false;
                ELSE
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= false;
                    ddr     <= false;
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;
                
                DIC_ACT := '1';
                DIC_RD_SETUP := '1';

                IF rising_edge(DICDONE) THEN
                    dic_out := (OTHERS => '0');
                    FOR i IN DIC_Start_Addr_reg TO DIC_End_Addr_reg LOOP
                        dic_in := to_slv(Mem(i),16);
                        FOR J IN 15 DOWNTO 0 LOOP
                            dic_tmp := dic_in(J) XOR dic_out(31);
                            dic_out(31) := dic_out(30);
                            dic_out(30) := dic_out(29);
                            dic_out(29) := dic_out(28);
                            dic_out(28) := dic_out(27) XOR dic_tmp;
                            dic_out(27) := dic_out(26) XOR dic_tmp;
                            dic_out(26) := dic_out(25) XOR dic_tmp;
                            dic_out(25) := dic_out(24) XOR dic_tmp;
                            dic_out(24) := dic_out(23);
                            dic_out(23) := dic_out(22) XOR dic_tmp;
                            dic_out(22) := dic_out(21) XOR dic_tmp;
                            dic_out(21) := dic_out(20);
                            dic_out(20) := dic_out(19) XOR dic_tmp;
                            dic_out(19) := dic_out(18) XOR dic_tmp;
                            dic_out(18) := dic_out(17) XOR dic_tmp;
                            dic_out(17) := dic_out(16);
                            dic_out(16) := dic_out(15);
                            dic_out(15) := dic_out(14);
                            dic_out(14) := dic_out(13) XOR dic_tmp;
                            dic_out(13) := dic_out(12) XOR dic_tmp;
                            dic_out(12) := dic_out(11);
                            dic_out(11) := dic_out(10) XOR dic_tmp;
                            dic_out(10) := dic_out(9) XOR dic_tmp;
                            dic_out(9) := dic_out(8) XOR dic_tmp;
                            dic_out(8) := dic_out(7) XOR dic_tmp;
                            dic_out(7) := dic_out(6);
                            dic_out(6) := dic_out(5) XOR dic_tmp;
                            dic_out(5) := dic_out(4);
                            dic_out(4) := dic_out(3);
                            dic_out(3) := dic_out(2);
                            dic_out(2) := dic_out(1);
                            dic_out(1) := dic_out(0);
                            dic_out(0) :=  DIC_tmp;
                        END LOOP;
                    END LOOP;
                    DCRV := dic_out;
                    STR1V(0) <= '0';  -- RDYBSY
                END IF;

            WHEN DIC_SUSP       =>
                IF (Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR 
                    Instruct = RDARG_C_0) THEN
                    IF QPI_IT = '1' THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                ELSIF QPI_IT = '1' THEN
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= true;
                    ddr     <= false;
                ELSE
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= false;
                    ddr     <= false;
                END IF;
                IF sSTART_T1 = '1' AND START_T1_in = '1' THEN
                    START_T1_in <= '0';
                    -- The RDYBSY bit in the Status Register will indicate that
                    -- the device is ready for another operation.
                    STR1V(0) <= '0';
                    -- The DIC Suspend (DICRCS) bit in the Status Register will
                    -- be set to the logical “1” state to indicate that the
                    -- DIC operation has been suspended.
                    DICRCS <= '1';
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDAY1_C_0 OR Instruct = RDAY1_4_0 THEN
                       -- Read Memory array
                        rd_fast <= false;
                        rd_fast1 <= false;
                        rd_slow <= true;
                        dual    <= false;
                        ddr     <= false;
                        IF pgm_page /= (read_addr / (PageSize+1)) THEN
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
                        ELSE
                            SOut_zd <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                IF read_addr = AddrRANGE THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        END IF;
                    ELSIF (QPI_IT = '1' AND Instruct = RDAY2_4_0) THEN 
                        rd_fast <= true;
                        rd_fast1 <= false;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                        IF pgm_page /= (read_addr / (PageSize+1)) THEN
                            IF bus_cycle_state = DUMMY_BYTES THEN
                               dlp_act := Return_DLP_SDR(Latency_code,dummy_cnt);
                               -- Data Learning Pattern (DLP) is enabled
                               -- Optional DLP
                               IF DLPV /= "00000000" AND dlp_act = true THEN
                                   IO3RESETNegOut_zd <= DLPV(7-read_cnt);
                                   WPNegOut_zd   <= DLPV(7-read_cnt);
                                   SOut_zd       <= DLPV(7-read_cnt);
                                   SIOut_zd      <= DLPV(7-read_cnt);
                                   dlp_act := FALSE;
                                   read_cnt := read_cnt + 1;
                                   IF read_cnt = 8 THEN
                                       read_cnt := 0;
                                   END IF;
                               END IF;
                            ELSE
                               data_out := to_slv(Mem(read_addr),8);
                               IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                               WPNegOut_zd   <= data_out(6-4*read_cnt);
                               SOut_zd       <= data_out(5-4*read_cnt);
                               SIOut_zd      <= data_out(4-4*read_cnt);
                               read_cnt := read_cnt + 1;
                               IF read_cnt = 2 THEN
                                   read_cnt := 0;
                                   IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                       IF read_addr = AddrRANGE THEN
                                           read_addr := 0;
                                       ELSE
                                           read_addr := read_addr + 1;
                                       END IF;
                                   ELSE
                                       read_addr := read_addr + 1;
                                       IF read_addr MOD WrapLength = 0 THEN
                                           read_addr := read_addr - WrapLength;
                                       END IF;
                                   END IF;
                               END IF;
                            END IF;
                        ELSE
                            data_out := to_slv(Mem(read_addr),8);
                            IO3RESETNegOut_zd <= 'X';
                            WPNegOut_zd   <= 'X';
                            SOut_zd       <= 'X';
                            SIOut_zd      <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                                IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                ELSE
                                    read_addr := read_addr + 1;
                                    IF read_addr MOD WrapLength = 0 THEN
                                        read_addr := read_addr - WrapLength;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                    ELSIF Instruct=RDAY2_C_0 OR Instruct=RDAY2_4_0 THEN 
                        IF pgm_page /= (read_addr / (PageSize+1)) THEN
                            IF bus_cycle_state = DUMMY_BYTES AND 
                               QPI_IT = '0' AND Instruct = RDAY2_4_0 THEN 
                               dlp_act := Return_DLP_SDR(Latency_code,dummy_cnt);
                               -- Data Learning Pattern (DLP) is enabled
                               -- Optional DLP
                               IF DLPV /= "00000000" AND dlp_act = true THEN
                                   SOut_zd       <= DLPV(7-read_cnt);
                                   dlp_act := FALSE;
                                   read_cnt := read_cnt + 1;
                                   IF read_cnt = 8 THEN
                                       read_cnt := 0;
                                   END IF;
                               END IF;
                            ELSE
                                data_out := to_slv(Mem(read_addr),8);
                                SOut_zd <= data_out(7-read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                    IF CFR4V(4) = '0' THEN
                                        IF read_addr = AddrRANGE THEN
                                            read_addr := 0;
                                        ELSE
                                            read_addr := read_addr + 1;
                                        END IF;
                                    ELSE
                                        read_addr := read_addr + 1;
                                        IF read_addr mod WrapLength = 0 THEN
                                            read_addr := read_addr - WrapLength;
                                        END IF;
                                    END IF;
                                END IF;
                            END IF;
                        ELSE
                            SOut_zd <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                IF CFR4V(4) = '0' THEN
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                ELSE
                                    read_addr := read_addr + 1;
                                    IF read_addr mod WrapLength = 0 THEN
                                        read_addr := read_addr - WrapLength;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;

                    ELSIF Instruct=RDAY3_C_0 OR Instruct=RDAY3_4_0 THEN
                        -- Read Memory array
                        rd_fast <= true;
                        rd_fast1 <= false;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                        IF pgm_page /= (read_addr / (PageSize+1)) THEN
                            data_out := to_slv(Mem(read_addr),8);
                            SOut_zd <= data_out(7-2*read_cnt);
                            SIOut_zd <= data_out(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                                IF CFR4V(4) = '0' THEN -- Burst read wrap disabled
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                ELSE
                                    read_addr := read_addr + 1;
                                    IF read_addr mod WrapLength = 0 THEN
                                        read_addr := read_addr - WrapLength;
                                    END IF;
                                END IF;
                            END IF;
                        ELSE
                            SOut_zd <= 'X';
                            SIOut_zd <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                                IF CFR4V(4) = '0' THEN -- Burst read wrap disabled
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                ELSE
                                    read_addr := read_addr + 1;
                                    IF read_addr mod WrapLength = 0 THEN
                                        read_addr := read_addr - WrapLength;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;

                    ELSIF (Instruct = RDAY4_C_0 OR Instruct = RDAY4_4_0) AND QUADIT='1' THEN 
                        -- Read Memory array
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                        IF pgm_page /= (read_addr / (PageSize+1)) THEN
                            IF bus_cycle_state = DUMMY_BYTES THEN
                                 dlp_act := Return_DLP_SDR(Latency_code,dummy_cnt);
                                 -- Data Learning Pattern (DLP) is enabled
                                 -- Optional DLP
                                 IF DLPV /= "00000000" AND dlp_act = true THEN
                                     IO3RESETNegOut_zd <= DLPV(7-read_cnt);
                                     WPNegOut_zd   <= DLPV(7-read_cnt);
                                     SOut_zd       <= DLPV(7-read_cnt);
                                     SIOut_zd      <= DLPV(7-read_cnt);
                                     dlp_act := FALSE;
                                     read_cnt := read_cnt + 1;
                                     IF read_cnt = 8 THEN
                                         read_cnt := 0;
                                     END IF;
                                 END IF;
                            ELSE
                                data_out := to_slv(Mem(read_addr),8);
                                IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                                WPNegOut_zd   <= data_out(6-4*read_cnt);
                                SOut_zd       <= data_out(5-4*read_cnt);
                                SIOut_zd      <= data_out(4-4*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                read_cnt := 0;
                                   IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                      IF read_addr = AddrRANGE THEN
                                          read_addr := 0;
                                      ELSE
                                          read_addr := read_addr + 1;
                                      END IF;
                                   ELSE
                                      read_addr := read_addr + 1;
                                      IF read_addr MOD WrapLength = 0 THEN
                                          read_addr := read_addr - WrapLength;
                                      END IF;
                                   END IF;
                                END IF;
                            END IF;
                        ELSE
                            IO3RESETNegOut_zd <= 'X';
                            WPNegOut_zd   <= 'X';
                            SOut_zd       <= 'X';
                            SIOut_zd      <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                            read_cnt := 0;
                               IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                  IF read_addr = AddrRANGE THEN
                                      read_addr := 0;
                                  ELSE
                                      read_addr := read_addr + 1;
                                  END IF;
                               ELSE
                                  read_addr := read_addr + 1;
                                  IF read_addr MOD WrapLength = 0 THEN
                                      read_addr := read_addr - WrapLength;
                                  END IF;
                               END IF;
                            END IF;
                        END IF;
                    ELSIF (Instruct = RDAY5_C_0 OR Instruct = RDAY5_4_0 OR
                    Instruct=RDAY7_C_0 OR Instruct=RDAY7_4_0) AND QUADIT='1' THEN
                        IF Instruct = RDAY7_C_0 OR Instruct = RDAY7_4_0  THEN
                            rd_fast <= false;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= true;
                        ELSE
                            rd_fast <= true;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                        END IF;
                        IF pgm_page /= (read_addr / (PageSize+1)) THEN
                            IF bus_cycle_state = DUMMY_BYTES THEN
                                IF (Instruct = RDAY7_C_0 OR Instruct = RDAY7_4_0) THEN
                                    dlp_act := Return_DLP(Latency_code,dummy_cnt);
                                    -- Data Learning Pattern (DLP) is enabled
                                    -- Optional DLP
                                    IF DLPV /= "00000000" AND dlp_act = true THEN
                                        IO3RESETNegOut_zd <= DLPV(7-read_cnt);
                                        WPNegOut_zd   <= DLPV(7-read_cnt);
                                        SOut_zd       <= DLPV(7-read_cnt);
                                        SIOut_zd      <= DLPV(7-read_cnt);
                                        dlp_act := FALSE;
                                        read_cnt := read_cnt + 1;
                                        IF read_cnt = 8 THEN
                                            read_cnt := 0;
                                        END IF;
                                    END IF;
                                ELSIF (Instruct = RDAY5_4_0 OR Instruct = RDAY5_C_0) THEN
                                    dlp_act := Return_DLP_SDR(Latency_code,dummy_cnt);
                                    -- Data Learning Pattern (DLP) is enabled
                                    -- Optional DLP
                                    IF DLPV /= "00000000" AND dlp_act = true THEN
                                        IO3RESETNegOut_zd <= DLPV(7-read_cnt);
                                        WPNegOut_zd   <= DLPV(7-read_cnt);
                                        SOut_zd       <= DLPV(7-read_cnt);
                                        SIOut_zd      <= DLPV(7-read_cnt);
                                        dlp_act := FALSE;
                                        read_cnt := read_cnt + 1;
                                        IF read_cnt = 8 THEN
                                            read_cnt := 0;
                                        END IF;
                                    END IF;
                                END IF;
                            ELSE
                                data_out := to_slv(Mem(read_addr),8);
                                IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                                WPNegOut_zd   <= data_out(6-4*read_cnt);
                                SOut_zd       <= data_out(5-4*read_cnt);
                                SIOut_zd      <= data_out(4-4*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                    read_cnt := 0;
                                    IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                        IF read_addr = AddrRANGE THEN
                                            read_addr := 0;
                                        ELSE
                                            read_addr := read_addr + 1;
                                        END IF;
                                    ELSE
                                        read_addr := read_addr + 1;
                                        IF read_addr MOD WrapLength = 0 THEN
                                            read_addr := read_addr - WrapLength;
                                        END IF;
                                    END IF;
                                END IF;
                            END IF;
                        ELSE
                            IO3RESETNegOut_zd <= 'X';
                            WPNegOut_zd   <= 'X';
                            SOut_zd <= 'X';
                            SIOut_zd <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                                IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                ELSE
                                    read_addr := read_addr + 1;
                                    IF read_addr MOD WrapLength = 0 THEN
                                        read_addr := read_addr - WrapLength;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    IF Instruct = RDAY1_C_0 OR Instruct = RDAY1_4_0 THEN
                        rd_fast <= false;
                        rd_fast1 <= false;
                        rd_slow <= true;
                        dual    <= false;
                        ddr     <= false;
                    ELSIF ((Instruct = RDAY4_C_0 OR Instruct = RDAY4_4_0) 
                           AND QUADIT = '1') THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSIF Instruct = RDAY3_C_0 OR Instruct = RDAY3_4_0 OR
                    Instruct = RDAY5_C_0 OR Instruct = RDAY5_4_0 THEN
                        rd_fast <= false;
                        rd_fast1 <= false;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSIF Instruct = RDAY7_C_0 OR Instruct = RDAY7_4_0 THEN
                        rd_fast <= false;
                        rd_fast1 <= false;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= true;
                    ELSE
                        IF QPI_IT = '1' THEN
                            rd_fast <= true;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                        ELSE
                            rd_fast <= true;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                        END IF;
                    END IF;
                END IF;

                IF falling_edge(write) THEN
                    IF Instruct = RSEPD_0_0 THEN
                        DICRCS <= '0';
                        STR1V(0) <= '1'; -- RDYBSY
                        DICRES  <= '1', '0' AFTER 1 ns;
                        RES_TO_SUSP_TIME <= '1', '0' AFTER tdevice_DICRL; -- 5 us
                    END IF;

                    ELSIF Instruct = CLECC_0_0 THEN
                        ECSV(4) := '0';
                        ECSV(3) := '0';
                        ECTV := "0000000000000000";
                        EATV := "00000000000000000000000000000000";

                    IF Instruct = SRSTE_0_0 THEN
                        RESET_EN <= '1';
                    ELSE
                        RESET_EN <= '0';
                    END IF;
                END IF;

            WHEN SECTOR_ERS       =>
                IF (Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR 
                    Instruct = RDARG_C_0) THEN
                    IF QPI_IT = '1' THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                ELSIF QPI_IT = '1' THEN
                    rd_fast <= true;
                    rd_slow <= false;
                    rd_fast1 <= false;
                    dual    <= true;
                    ddr     <= false;
                ELSE
                    rd_fast <= true;
                    rd_slow <= false;
                    rd_fast1 <= false;
                    dual    <= false;
                    ddr     <= false;
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;

                IF falling_edge(EDONE) THEN
                    ADDRHILO_SEC(AddrLo, AddrHi, Addr_ers);
                    FOR i IN AddrLo TO AddrHi LOOP
                        Mem(i) := -1;
                    END LOOP;
                END IF;

                IF rising_edge(EDONE) THEN
                    STR1V(0) <= '0'; -- RDYBSY
                    STR1V(1) <= '0';  -- WRPGEN
                    STR1V_DPD <= '0';
                     WVREG    <= '0'; --Write volatile regs
                    ADDRHILO_SEC(AddrLo, AddrHi, Addr_ers);
                    FOR i IN AddrLo TO AddrHi LOOP
                        Mem(i) :=  MaxData;
                    END LOOP;
                    ERS_nosucc(SecAddr_ers) <= '0';
                    
                    -- Increment Sector Erase Count register for a given Sector
                    SECVIN := to_slv(to_nat(SECV_in(SecAddr_ers)) + 1);
                    SECV_in(SecAddr_ers)(23 downto 0) <= SECVIN(23 downto 0);

                    -- Erase multi-pass sector flags register
                    MPASSREG(SecAddr_ers) := '0';
                END IF;

                IF falling_edge(write) THEN
                    IF (Instruct = SPEPA_0_0 OR Instruct = SPEPD_0_0) AND ERSSUSP_in = '0' THEN
                        IF RES_TO_SUSP_TIME = '0' THEN
                            ESUSP <= '1', '0' AFTER 1 ns;
                            ERSSUSP_in <= '1';
                        ELSE
                            ASSERT false
                            REPORT "Minimum for tRS is not satisfied!" &
                                    "PGSP command is ignored"
                            SEVERITY warning;
                        END IF;
                    END IF;
                END IF;

            WHEN BULK_ERS       =>
                IF (Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR 
                    Instruct = RDARG_C_0) THEN
                    IF QPI_IT = '1' THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                ELSIF QPI_IT = '1' THEN
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= true;
                    ddr     <= false;
                ELSE
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= false;
                    ddr     <= false;
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;

                IF falling_edge(EDONE) THEN
                    FOR i IN 0 TO AddrRANGE LOOP
                        sect := ReturnSectorID(i,BottomBoot,TopBoot);
                        IF (PPB_bits(sect) = '1' AND DYB_bits(sect) = '1') THEN
                                Mem(i) := -1;
                        END IF;
                    END LOOP;
                END IF;

                IF EDONE = '1' THEN
                    STR1V(0) <= '0'; -- RDYBSY
                    STR1V(1) <= '0';  -- WRPGEN
                    STR1V_DPD <= '0';
                     WVREG    <= '0'; --Write volatile regs
                    
                    FOR i IN 0 TO AddrRANGE LOOP
                        sect := ReturnSectorID(i,BottomBoot,TopBoot);
                        IF (PPB_bits(sect) = '1' AND DYB_bits(sect) = '1') THEN
                                Mem(i) := MaxData;
                        END IF;
                    END LOOP;
                    FOR i IN 0 TO SecNumHyb LOOP
                        IF (PPB_bits(i) = '1' AND DYB_bits(i) = '1') THEN
                                -- Increment Sector Erase Count register for a given Sector
                                SECVIN := to_slv(to_nat(SECV_in(i)) + 1);
                                SECV_in(i)(23 downto 0) <= SECVIN(23 downto 0);
                                -- Erase multi-pass sector flags register
                                MPASSREG(i) := '0';
                        END IF;
                    END LOOP;
                END IF;

            WHEN ERS_SUSP       =>
                IF (Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR 
                    Instruct = RDARG_C_0) THEN
                    IF QPI_IT = '1' THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                ELSIF QPI_IT = '1' THEN
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= true;
                    ddr     <= false;
                ELSE
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= false;
                    ddr     <= false;
                END IF;
                IF ERSSUSP_out = '1' THEN
                    ERSSUSP_in <= '0';
                    -- The RDYBSY bit in the Status Register will indicate that
                    -- the device is ready for another operation.
                    STR1V(0) <= '0';
                    -- The Erase Suspend (ES) bit in the Status Register will
                    -- be set to the logical “1” state to indicate that the
                    -- erase operation has been suspended.
                    STR2V(1) <= '1';
                END IF;

                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                           rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDAY1_C_0 OR Instruct = RDAY1_4_0 THEN
                       -- Read Memory array
                        rd_fast <= false;
                        rd_fast1 <= false;
                        rd_slow <= true;
                        dual    <= false;
                        ddr     <= false;
                        IF SectorSuspend /= (read_addr/(SecSize256+1)) THEN
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
                        ELSE
                            SOut_zd <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                IF read_addr = AddrRANGE THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        END IF;
                   ELSIF (QPI_IT = '1' AND Instruct = RDAY2_4_0) THEN 
                        rd_fast <= true;
                        rd_fast1 <= false;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                        IF SectorSuspend /= (read_addr/(SecSize256+1)) THEN
                            IF bus_cycle_state = DUMMY_BYTES THEN
                                dlp_act := Return_DLP_SDR(Latency_code,dummy_cnt);
                                -- Data Learning Pattern (DLP) is enabled
                                -- Optional DLP
                                IF DLPV /= "00000000" AND dlp_act = true THEN
                                    IO3RESETNegOut_zd <= DLPV(7-read_cnt);
                                    WPNegOut_zd   <= DLPV(7-read_cnt);
                                    SOut_zd       <= DLPV(7-read_cnt);
                                    SIOut_zd      <= DLPV(7-read_cnt);
                                    dlp_act := FALSE;
                                    read_cnt := read_cnt + 1;
                                    IF read_cnt = 8 THEN
                                        read_cnt := 0;
                                    END IF;
                                END IF;
                           ELSE
                               data_out := to_slv(Mem(read_addr),8);
                               IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                               WPNegOut_zd   <= data_out(6-4*read_cnt);
                               SOut_zd       <= data_out(5-4*read_cnt);
                               SIOut_zd      <= data_out(4-4*read_cnt);
                               read_cnt := read_cnt + 1;
                               IF read_cnt = 2 THEN
                                   read_cnt := 0;
                                   IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                       IF read_addr = AddrRANGE THEN
                                           read_addr := 0;
                                       ELSE
                                           read_addr := read_addr + 1;
                                       END IF;
                                   ELSE
                                       read_addr := read_addr + 1;
                                       IF read_addr MOD WrapLength = 0 THEN
                                           read_addr := read_addr - WrapLength;
                                       END IF;
                                   END IF;
                               END IF;
                           END IF;
                        ELSE
                           data_out := to_slv(Mem(read_addr),8);
                           IO3RESETNegOut_zd <= 'X';
                           WPNegOut_zd   <= 'X';
                           SOut_zd       <= 'X';
                           SIOut_zd      <= 'X';
                           read_cnt := read_cnt + 1;
                           IF read_cnt = 2 THEN
                               read_cnt := 0;
                               IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                   IF read_addr = AddrRANGE THEN
                                       read_addr := 0;
                                   ELSE
                                       read_addr := read_addr + 1;
                                   END IF;
                               ELSE
                                   read_addr := read_addr + 1;
                                   IF read_addr MOD WrapLength = 0 THEN
                                       read_addr := read_addr - WrapLength;
                                   END IF;
                               END IF;
                           END IF;
                        END IF;
                    ELSIF Instruct=RDAY2_C_0 OR Instruct=RDAY2_4_0 THEN 
                        rd_fast <= true;
                        rd_fast1 <= false;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                        IF SectorSuspend /= (read_addr/(SecSize256+1)) THEN
                            IF bus_cycle_state = DUMMY_BYTES AND 
                               QPI_IT = '0' AND Instruct = RDAY2_4_0 THEN 
                                dlp_act := Return_DLP_SDR(Latency_code,dummy_cnt);
                                -- Data Learning Pattern (DLP) is enabled
                                -- Optional DLP
                                IF DLPV /= "00000000" AND dlp_act = true THEN
                                    SOut_zd       <= DLPV(7-read_cnt);
                                    dlp_act := FALSE;
                                    read_cnt := read_cnt + 1;
                                    IF read_cnt = 8 THEN
                                        read_cnt := 0;
                                    END IF;
                                END IF;
                            ELSE
                                data_out := to_slv(Mem(read_addr),8);
                                SOut_zd <= data_out(7-read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                    IF CFR4V(4) = '0' THEN
                                        IF read_addr = AddrRANGE THEN
                                            read_addr := 0;
                                        ELSE
                                            read_addr := read_addr + 1;
                                        END IF;
                                    ELSE
                                        read_addr := read_addr + 1;
                                        IF read_addr mod WrapLength = 0 THEN
                                            read_addr := read_addr - WrapLength;
                                        END IF;
                                    END IF;
                                END IF;
                            END IF;
                        ELSE
                            SOut_zd <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                IF CFR4V(4) = '0' THEN
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                ELSE
                                    read_addr := read_addr + 1;
                                    IF read_addr mod WrapLength = 0 THEN
                                        read_addr := read_addr - WrapLength;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;

                    ELSIF Instruct=RDAY3_C_0 OR Instruct=RDAY3_4_0 THEN
                        -- Read Memory array
                        rd_fast <= true;
                        rd_fast1 <= false;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                        IF SectorSuspend /= (read_addr/(SecSize256+1)) THEN
                            data_out := to_slv(Mem(read_addr),8);
                            SOut_zd <= data_out(7-2*read_cnt);
                            SIOut_zd <= data_out(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                                IF CFR4V(4) = '0' THEN -- Burst read wrap disabled
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                ELSE
                                    read_addr := read_addr + 1;
                                    IF read_addr mod WrapLength = 0 THEN
                                        read_addr := read_addr - WrapLength;
                                    END IF;
                                END IF;
                            END IF;
                        ELSE
                            SOut_zd <= 'X';
                            SIOut_zd <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                                IF CFR4V(4) = '0' THEN -- Burst read wrap disabled
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                ELSE
                                    read_addr := read_addr + 1;
                                    IF read_addr mod WrapLength = 0 THEN
                                        read_addr := read_addr - WrapLength;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;

                        ELSIF (Instruct = RDAY4_C_0 OR Instruct = RDAY4_4_0) AND QUADIT='1' THEN 
                        -- Read Memory array
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                        IF SectorSuspend /= (read_addr/(SecSize256+1)) THEN
                            IF bus_cycle_state = DUMMY_BYTES THEN
                                dlp_act := Return_DLP_SDR(Latency_code,dummy_cnt);
                                -- Data Learning Pattern (DLP) is enabled
                                -- Optional DLP
                                IF DLPV /= "00000000" AND dlp_act = true THEN
                                    IO3RESETNegOut_zd <= DLPV(7-read_cnt);
                                    WPNegOut_zd   <= DLPV(7-read_cnt);
                                    SOut_zd       <= DLPV(7-read_cnt);
                                    SIOut_zd      <= DLPV(7-read_cnt);
                                    dlp_act := FALSE;
                                    read_cnt := read_cnt + 1;
                                    IF read_cnt = 8 THEN
                                        read_cnt := 0;
                                    END IF;
                                END IF;
                            ELSE
                                data_out := to_slv(Mem(read_addr),8);
                                IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                                WPNegOut_zd   <= data_out(6-4*read_cnt);
                                SOut_zd       <= data_out(5-4*read_cnt);
                                SIOut_zd      <= data_out(4-4*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                read_cnt := 0;
                                   IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                      IF read_addr = AddrRANGE THEN
                                          read_addr := 0;
                                      ELSE
                                          read_addr := read_addr + 1;
                                      END IF;
                                   ELSE
                                      read_addr := read_addr + 1;
                                      IF read_addr MOD WrapLength = 0 THEN
                                          read_addr := read_addr - WrapLength;
                                      END IF;
                                   END IF;
                                END IF;
                            END IF;
                        ELSE
                            IO3RESETNegOut_zd <= 'X';
                            WPNegOut_zd   <= 'X';
                            SOut_zd       <= 'X';
                            SIOut_zd      <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                            read_cnt := 0;
                               IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                  IF read_addr = AddrRANGE THEN
                                      read_addr := 0;
                                  ELSE
                                      read_addr := read_addr + 1;
                                  END IF;
                               ELSE
                                  read_addr := read_addr + 1;
                                  IF read_addr MOD WrapLength = 0 THEN
                                      read_addr := read_addr - WrapLength;
                                  END IF;
                               END IF;
                            END IF;
                        END IF;

                    ELSIF (Instruct = RDAY5_C_0 OR Instruct = RDAY5_4_0 OR
                    Instruct=RDAY7_C_0 OR Instruct=RDAY7_4_0) AND QUADIT='1' THEN
                        IF Instruct = RDAY7_C_0 OR Instruct = RDAY7_4_0  THEN
                            rd_fast <= false;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= true;
                        ELSE
                            rd_fast <= true;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                        END IF;
                        IF SectorSuspend /= (read_addr/(SecSize256+1)) THEN
                            IF bus_cycle_state = DUMMY_BYTES THEN
                                IF (Instruct = RDAY7_C_0 OR Instruct = RDAY7_4_0) THEN
                                    dlp_act := Return_DLP(Latency_code,dummy_cnt);
                                    -- Data Learning Pattern (DLP) is enabled
                                    -- Optional DLP
                                    IF DLPV /= "00000000" AND dlp_act = true THEN
                                        IO3RESETNegOut_zd <= DLPV(7-read_cnt);
                                        WPNegOut_zd   <= DLPV(7-read_cnt);
                                        SOut_zd       <= DLPV(7-read_cnt);
                                        SIOut_zd      <= DLPV(7-read_cnt);
                                        dlp_act := FALSE;
                                        read_cnt := read_cnt + 1;
                                        IF read_cnt = 8 THEN
                                            read_cnt := 0;
                                        END IF;
                                    END IF;
                                    ELSIF (Instruct = RDAY5_4_0 OR Instruct = RDAY5_C_0) THEN
                                    dlp_act := Return_DLP_SDR(Latency_code,dummy_cnt);
                                    -- Data Learning Pattern (DLP) is enabled
                                    -- Optional DLP
                                    IF DLPV /= "00000000" AND dlp_act = true THEN
                                        IO3RESETNegOut_zd <= DLPV(7-read_cnt);
                                        WPNegOut_zd   <= DLPV(7-read_cnt);
                                        SOut_zd       <= DLPV(7-read_cnt);
                                        SIOut_zd      <= DLPV(7-read_cnt);
                                        dlp_act := FALSE;
                                        read_cnt := read_cnt + 1;
                                        IF read_cnt = 8 THEN
                                            read_cnt := 0;
                                        END IF;
                                    END IF;
                                END IF;
                            ELSE
                                data_out := to_slv(Mem(read_addr),8);
                                IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                                WPNegOut_zd   <= data_out(6-4*read_cnt);
                                SOut_zd       <= data_out(5-4*read_cnt);
                                SIOut_zd      <= data_out(4-4*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                    read_cnt := 0;
                                    IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                        IF read_addr = AddrRANGE THEN
                                            read_addr := 0;
                                        ELSE
                                            read_addr := read_addr + 1;
                                        END IF;
                                    ELSE
                                        read_addr := read_addr + 1;
                                        IF read_addr MOD WrapLength = 0 THEN
                                            read_addr := read_addr - WrapLength;
                                        END IF;
                                    END IF;
                                END IF;
                            END IF;
                        ELSE
                            IO3RESETNegOut_zd <= 'X';
                            WPNegOut_zd   <= 'X';
                            SOut_zd <= 'X';
                            SIOut_zd <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                                IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                ELSE
                                    read_addr := read_addr + 1;
                                    IF read_addr MOD WrapLength = 0 THEN
                                        read_addr := read_addr - WrapLength;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDDYB_C_0 OR Instruct = RDDYB_4_0 THEN
                        --Read DYB Access Register  
                        rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                        IF DYB_bits(ReturnSectorID
                        (Address,BottomBoot,TopBoot)) = '1' THEN
                            DYAV(7 downto 0) := "11111111";
                        ELSE
                            DYAV(7 downto 0) := "00000000";
                        END IF;

                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            IO3RESETNegOut_zd <= DYAV(7-4*read_cnt);
                            WPNegOut_zd   <= DYAV(6-4*read_cnt);
                            SOut_zd       <= DYAV(5-4*read_cnt);
                            SIOut_zd      <= DYAV(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= DYAV(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDPPB_C_0 OR Instruct = RDPPB_4_0 THEN
                        --Read PPB Access Register
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                        sect := ReturnSectorID(Address,BottomBoot,TopBoot);
                        IF PPB_bits(sect) = '1' THEN
                            PPAV(7 downto 0) := "11111111";
                        ELSE
                            PPAV(7 downto 0) := "00000000";
                        END IF;
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            IO3RESETNegOut_zd <= PPAV(7-4*read_cnt);
                            WPNegOut_zd   <= PPAV(6-4*read_cnt);
                            SOut_zd       <= PPAV(5-4*read_cnt);
                            SIOut_zd      <= PPAV(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            SOut_zd <= PPAV(7-read_cnt); 
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                               read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    IF Instruct = RDAY1_C_0 OR Instruct = RDAY1_4_0 THEN
                        rd_fast <= false;
                        rd_fast1 <= false;
                        rd_slow <= true;
                        dual    <= false;
                        ddr     <= false;
                    ELSIF ((Instruct = RDAY4_C_0 OR Instruct = RDAY4_4_0) 
                           AND QUADIT = '1') THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSIF Instruct = RDAY3_C_0 OR Instruct = RDAY3_4_0 OR
                    Instruct = RDAY5_C_0 OR Instruct = RDAY5_4_0 THEN
                        rd_fast <= false;
                        rd_fast1 <= false;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSIF Instruct = RDAY7_C_0 OR Instruct = RDAY7_4_0 THEN
                        rd_fast <= false;
                        rd_fast1 <= false;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= true;
                    ELSE
                        IF QPI_IT = '1' THEN
                            rd_fast <= true;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                        ELSE
                            rd_fast <= true;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                        END IF;
                    END IF;
                END IF;

                IF falling_edge(write) THEN
                    IF Instruct = RSEPA_0_0 OR Instruct = RSEPD_0_0 THEN
                        STR2V(1) <= '0'; -- ES
                        STR1V(0) <= '1'; -- RDYBSY
                        ERES <= '1', '0' AFTER 1 ns;
                        RES_TO_SUSP_TIME <= '1', '0' AFTER res_time;
                    ELSIF (Instruct = PRPGE_C_1 OR Instruct = PRPGE_4_1) AND WRPGEN='1' AND PRGERR='0' THEN
                        pgm_page := Address/(PageSize+1);
                        SecAddr_pgm := ReturnSectorID(Address,BottomBoot,TopBoot);
                        IF SectorSuspend /= (Address/(SecSize256+1)) THEN
                            IF Sec_Prot(SecAddr_pgm)='0' AND
                            PPB_bits(SecAddr_pgm)='1' AND
                            DYB_bits(SecAddr_pgm)='1' THEN
                                PSTART <= '1', '0' AFTER 1 ns;
                                PGSUSP  <= '0';
                                PGRES   <= '0';
                                STR1V(0) <= '1';  -- RDYBSY
                                Addr_pgm := Address;
                                Addr_pgm_tmp := Address;
                                Addr_pgm_ECC := Address;
                                wr_cnt := Byte_number;
                                FOR I IN wr_cnt DOWNTO 0 LOOP
                                    IF Viol /= '0' THEN
                                        WData(i) := -1;
                                    ELSE
                                        WData(i) := WByte(i);
                                    END IF;
                                END LOOP;
                            ELSE
                            -- PRGERR bit will be set when the user attempts to
                            -- to program within a protected main memory sector
                                STR1V(6) <= '1'; -- PRGERR
                                STR1V(0) <= '1'; -- RDYBSY
                            END IF;
                        ELSE
                            STR1V(6) <= '1'; -- PRGERR
                            STR1V(0) <= '1'; -- RDYBSY
                        END IF;

--                     ELSIF (Instruct=WRDYB_C_1 OR Instruct=WRDYB_4_1) AND WRPGEN = '1' THEN
--                         IF DYAV_in = "11111111" OR
--                         DYAV_in = "00000000" THEN
--                             sect := ReturnSectorID
--                             (Address,BottomBoot,TopBoot);
--                             PSTART <= '1', '0' AFTER 1 ns;
--                             STR1V(0) <= '1'; -- RDYBSY
--                         ELSE
--                             STR1V(6) <= '1'; -- PRGERR
--                             STR1V(0) <= '1'; -- RDYBSY
--                         END IF;
                    ELSIF Instruct = WRENV_0_0 THEN
                         WVREG <= '1'; -- Write volatile Regs
                    ELSIF Instruct = WRENB_0_0 THEN
                        STR1V(1) <= '1'; -- WRPGEN
                        STR1V_DPD <= '1';
                    ELSIF Instruct = CLECC_0_0 THEN
                        ECSV(4) := '0';
                        ECSV(3) := '0';
                        ECTV := "0000000000000000";
                        EATV := "00000000000000000000000000000000";
                    ELSIF Instruct = CLPEF_0_0 THEN
                        STR1V(6) <= '0';-- PRGERR
                        STR1V(5) <= '0';-- ERSERR
                        STR1V(0) <= '0';-- RDYBSY
                    END IF;

--                     IF Instruct = SFRSL_0_0 THEN
--                         RST_in_soft <= '1', '0' AFTER 1 ns;
--                     END IF;
                END IF;

            WHEN ERS_SUSP_PG       =>
                IF (Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR 
                    Instruct = RDARG_C_0) THEN
                    IF QPI_IT = '1' THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                ELSIF QPI_IT = '1' THEN
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= true;
                    ddr     <= false;
                ELSE
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= false;
                    ddr     <= false;
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;

                IF falling_edge(PDONE) THEN
                    ADDRHILO_PG(AddrLo, AddrHi, Addr_pgm);
                    IF ((Addr_pgm_ECC mod 16) + Byte_number + 1) mod 16 = 0 THEN
                        ECC_check := ((Addr_pgm_ECC mod 16) + Byte_number+1) / 16;
                    ELSE
                        ECC_check := (((Addr_pgm_ECC mod 16) + Byte_number+1) / 16) + 1;
                    END IF;
                    
                    ECC_data := Addr_pgm_ECC - (Addr_pgm_ECC mod 16);
                    cnt :=0;
                    FOR i IN 0 TO wr_cnt LOOP
                        new_int := WData(i);
                        old_int := Mem(Addr_pgm + i - cnt);

                        IF new_int > -1 THEN
                            new_bit := to_slv(new_int,8);
                            IF old_int > -1 THEN
                                old_bit := to_slv(old_int,8);
                                FOR j IN 0 TO 7 LOOP
                                    IF old_bit(j) = '0' THEN
                                        new_bit(j) := '0';
                                    END IF;
                                END LOOP;
                                new_int := to_nat(new_bit);
                            END IF;
                            WData(i) := new_int;
                        ELSE
                            WData(i) := -1;
                        END IF;

                        Mem(Addr_pgm + i - cnt) :=  -1;
                        IF (Addr_pgm + i) = AddrHi THEN
                            Addr_pgm := AddrLo;
                            cnt := i + 1;
                        END IF;
                    END LOOP;
                    cnt :=0;
                END IF;

                IF rising_edge(PDONE) THEN
                    IF (((CFR4V(3) = '1')  OR (non_industrial_temp = '1')) 
                        AND (ECC_ERR > 0)) THEN
                        STR1V(0) <= '0'; --RDYBSY
                        STR1V(1) <= '0'; --WRPGEN
                        STR1V(6) <= '1'; --PRGERR
                        ECC_ERR  := 0; --
                        REPORT "WARNING: For non-industrial temperatures " &
                               "it is not allowed to have multi-programming " &
                               "without erasing previously the sector! " &
                               "multi-pass programming within the same data unit" &
                               "will result in a Program Error. "
                        SEVERITY WARNING;
                    ELSE
                        STR1V(0) <= '0'; -- RDYBSY
                        STR1V(1) <= '0';  -- WRPGEN
                        ECC_ERR  := 0; --
                    END IF;
--                        IF MPASSREG(sect) = '0' THEN
--                        MPASSREG(sect) := '1';
--                        END IF;
                        FOR i IN 0 TO wr_cnt LOOP
                        Mem(Addr_pgm_tmp + i - cnt) :=  WData(i);
                        IF (Addr_pgm_tmp + i) = AddrHi THEN
                            Addr_pgm_tmp := AddrLo;
                            cnt := i + 1;
                        END IF;
                        END LOOP;

                ELSIF falling_edge(write) THEN
                    IF (Instruct = SPEPA_0_0 OR Instruct = SPEPD_0_0) AND PRGSUSP_in = '0' THEN
                        IF RES_TO_SUSP_TIME = '0' THEN
                            PGSUSP <= '1', '0' AFTER 1 ns;
                            PRGSUSP_in <= '1';
                        ELSE
                            ASSERT FALSE
                            REPORT "Minimum for tPRS is not satisfied! " &
                                "PGSP command is ignored"
                            SEVERITY warning;
                        END IF;
                    END IF;
                END IF;

            WHEN ERS_SUSP_PG_SUSP       =>
                IF (Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR 
                    Instruct = RDARG_C_0) THEN
                    IF QPI_IT = '1' THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                ELSIF QPI_IT = '1' THEN
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= true;
                    ddr     <= false;
                ELSE
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= false;
                    ddr     <= false;
                END IF;
                IF PRGSUSP_out = '1' AND PRGSUSP_in = '1' THEN
                    PRGSUSP_in <= '0';
                    -- The RDYBSY bit in the Status Register will indicate that
                    -- the device is ready for another operation.
                    STR1V(0) <= '0';
                    -- The Program Suspend (PS) bit in the Status Register will
                    -- be set to the logical “1” state to indicate that the
                    -- program operation has been suspended.
                    STR2V(0) <= '1';
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDAY1_C_0 OR Instruct = RDAY1_4_0 THEN
                       -- Read Memory array
                        rd_fast <= false;
                        rd_fast1 <= false;
                        rd_slow <= true;
                        dual    <= false;
                        ddr     <= false;
                        IF (SectorSuspend /= (read_addr/(SecSize256+1))) AND 
                        (pgm_page /= (read_addr/(PageSize+1))) THEN
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
                        ELSE
                            SOut_zd <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                IF read_addr = AddrRANGE THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        END IF;
                    ELSIF (QPI_IT = '1' AND Instruct = RDAY2_4_0) THEN 
                        IF (SectorSuspend /= (read_addr/(SecSize256+1))) AND
                            (pgm_page /= (read_addr / (PageSize+1))) THEN
                            rd_fast <= true;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                         
                             IF bus_cycle_state = DUMMY_BYTES THEN
                                dlp_act := Return_DLP_SDR(Latency_code,dummy_cnt);
                                -- Data Learning Pattern (DLP) is enabled
                                -- Optional DLP
                                IF DLPV /= "00000000" AND dlp_act = true THEN
                                    IO3RESETNegOut_zd <= DLPV(7-read_cnt);
                                    WPNegOut_zd   <= DLPV(7-read_cnt);
                                    SOut_zd       <= DLPV(7-read_cnt);
                                    SIOut_zd      <= DLPV(7-read_cnt);
                                    dlp_act := FALSE;
                                    read_cnt := read_cnt + 1;
                                    IF read_cnt = 8 THEN
                                        read_cnt := 0;
                                    END IF;
                                END IF;
                             ELSE
                                data_out := to_slv(Mem(read_addr),8);
                                IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                                WPNegOut_zd   <= data_out(6-4*read_cnt);
                                SOut_zd       <= data_out(5-4*read_cnt);
                                SIOut_zd      <= data_out(4-4*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                    read_cnt := 0;
                                    IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                        IF read_addr = AddrRANGE THEN
                                            read_addr := 0;
                                        ELSE
                                            read_addr := read_addr + 1;
                                        END IF;
                                    ELSE
                                        read_addr := read_addr + 1;
                                        IF read_addr MOD WrapLength = 0 THEN
                                            read_addr := read_addr - WrapLength;
                                        END IF;
                                    END IF;
                                END IF;
                             END IF;
                      ELSE
                         data_out := to_slv(Mem(read_addr),8);
                         IO3RESETNegOut_zd <= 'X';
                         WPNegOut_zd   <= 'X';
                         SOut_zd       <= 'X';
                         SIOut_zd      <= 'X';
                         read_cnt := read_cnt + 1;
                         IF read_cnt = 2 THEN
                             read_cnt := 0;
                             IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                 IF read_addr = AddrRANGE THEN
                                     read_addr := 0;
                                 ELSE
                                     read_addr := read_addr + 1;
                                 END IF;
                             ELSE
                                 read_addr := read_addr + 1;
                                 IF read_addr MOD WrapLength = 0 THEN
                                     read_addr := read_addr - WrapLength;
                                 END IF;
                             END IF;
                         END IF;
                      END IF;
                    ELSIF Instruct=RDAY2_C_0 OR Instruct=RDAY2_4_0 THEN 
                        IF (SectorSuspend /= (read_addr/(SecSize256+1))) AND
                            (pgm_page /= (read_addr / (PageSize+1))) THEN
                            IF bus_cycle_state = DUMMY_BYTES AND 
                                QPI_IT = '0' AND Instruct = RDAY2_4_0 THEN 
                                dlp_act := Return_DLP_SDR(Latency_code,dummy_cnt);
                                -- Data Learning Pattern (DLP) is enabled
                                -- Optional DLP
                                IF DLPV /= "00000000" AND dlp_act = true THEN
                                    SOut_zd       <= DLPV(7-read_cnt);
                                    dlp_act := FALSE;
                                    read_cnt := read_cnt + 1;
                                    IF read_cnt = 8 THEN
                                        read_cnt := 0;
                                    END IF;
                                END IF;
                            ELSE
                                data_out := to_slv(Mem(read_addr),8);
                                SOut_zd <= data_out(7-read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                    IF CFR4V(4) = '0' THEN
                                        IF read_addr = AddrRANGE THEN
                                            read_addr := 0;
                                        ELSE
                                            read_addr := read_addr + 1;
                                        END IF;
                                    ELSE
                                        read_addr := read_addr + 1;
                                        IF read_addr mod WrapLength = 0 THEN
                                            read_addr := read_addr - WrapLength;
                                        END IF;
                                    END IF;
                                END IF;
                            END IF;
                        ELSE
                            SOut_zd <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                IF CFR4V(4) = '0' THEN
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                ELSE
                                    read_addr := read_addr + 1;
                                    IF read_addr mod WrapLength = 0 THEN
                                        read_addr := read_addr - WrapLength;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;

                    ELSIF Instruct=RDAY3_C_0 OR Instruct=RDAY3_4_0 THEN
                        -- Read Memory array
                        rd_fast <= true;
                        rd_fast1 <= false;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                        IF (SectorSuspend /= (read_addr/(SecSize256+1))) AND
                            (pgm_page /= (read_addr / (PageSize+1))) THEN
                            data_out := to_slv(Mem(read_addr),8);
                            SOut_zd <= data_out(7-2*read_cnt);
                            SIOut_zd <= data_out(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                                IF CFR4V(4) = '0' THEN -- Burst read wrap disabled
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                ELSE
                                    read_addr := read_addr + 1;
                                    IF read_addr mod WrapLength = 0 THEN
                                        read_addr := read_addr - WrapLength;
                                    END IF;
                                END IF;
                            END IF;
                        ELSE
                            SOut_zd <= 'X';
                            SIOut_zd <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                                IF CFR4V(4) = '0' THEN -- Burst read wrap disabled
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                ELSE
                                    read_addr := read_addr + 1;
                                    IF read_addr mod WrapLength = 0 THEN
                                        read_addr := read_addr - WrapLength;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;

                    ELSIF (Instruct = RDAY4_C_0 OR Instruct = RDAY4_4_0) AND QUADIT='1' THEN 
                        -- Read Memory array
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                        IF SectorSuspend /= (read_addr/(SecSize256+1)) THEN
                            IF bus_cycle_state = DUMMY_BYTES THEN
                                dlp_act := Return_DLP_SDR(Latency_code,dummy_cnt);
                                -- Data Learning Pattern (DLP) is enabled
                                -- Optional DLP
                                IF DLPV /= "00000000" AND dlp_act = true THEN
                                    IO3RESETNegOut_zd <= DLPV(7-read_cnt);
                                    WPNegOut_zd   <= DLPV(7-read_cnt);
                                    SOut_zd       <= DLPV(7-read_cnt);
                                    SIOut_zd      <= DLPV(7-read_cnt);
                                    dlp_act := FALSE;
                                    read_cnt := read_cnt + 1;
                                    IF read_cnt = 8 THEN
                                        read_cnt := 0;
                                    END IF;
                                END IF;
                            ELSE
                                data_out := to_slv(Mem(read_addr),8);
                                IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                                WPNegOut_zd   <= data_out(6-4*read_cnt);
                                SOut_zd       <= data_out(5-4*read_cnt);
                                SIOut_zd      <= data_out(4-4*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                read_cnt := 0;
                                   IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                      IF read_addr = AddrRANGE THEN
                                          read_addr := 0;
                                      ELSE
                                          read_addr := read_addr + 1;
                                      END IF;
                                   ELSE
                                      read_addr := read_addr + 1;
                                      IF read_addr MOD WrapLength = 0 THEN
                                          read_addr := read_addr - WrapLength;
                                      END IF;
                                   END IF;
                                END IF;
                            END IF;
                        ELSE
                            IO3RESETNegOut_zd <= 'X';
                            WPNegOut_zd   <= 'X';
                            SOut_zd       <= 'X';
                            SIOut_zd      <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                            read_cnt := 0;
                               IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                  IF read_addr = AddrRANGE THEN
                                      read_addr := 0;
                                  ELSE
                                      read_addr := read_addr + 1;
                                  END IF;
                               ELSE
                                  read_addr := read_addr + 1;
                                  IF read_addr MOD WrapLength = 0 THEN
                                      read_addr := read_addr - WrapLength;
                                  END IF;
                               END IF;
                            END IF;
                        END IF;

                    ELSIF (Instruct = RDAY5_C_0 OR Instruct = RDAY5_4_0 OR
                    Instruct=RDAY7_C_0 OR Instruct=RDAY7_4_0) AND QUADIT='1' THEN
                        IF Instruct = RDAY7_C_0 OR Instruct = RDAY7_4_0 THEN
                            rd_fast <= false;
                            rd_slow <= false;
                            rd_fast1 <= false;
                            dual    <= true;
                            ddr     <= true;
                        ELSE
                            rd_fast <= true;
                            rd_slow <= false;
                            rd_fast1 <= false;
                            dual    <= true;
                            ddr     <= false;
                        END IF;
                        IF (SectorSuspend /= (read_addr/(SecSize256+1))) AND 
                        (pgm_page /= (read_addr / (PageSize+1))) THEN
                            IF bus_cycle_state = DUMMY_BYTES THEN
                                IF (Instruct = RDAY7_C_0 OR Instruct = RDAY7_4_0) THEN
                                    dlp_act := Return_DLP(Latency_code,dummy_cnt);
                                    -- Data Learning Pattern (DLP) is enabled
                                    -- Optional DLP
                                    IF DLPV /= "00000000" AND dlp_act = true THEN
                                        IO3RESETNegOut_zd <= DLPV(7-read_cnt);
                                        WPNegOut_zd   <= DLPV(7-read_cnt);
                                        SOut_zd       <= DLPV(7-read_cnt);
                                        SIOut_zd      <= DLPV(7-read_cnt);
                                        dlp_act := FALSE;
                                        read_cnt := read_cnt + 1;
                                        IF read_cnt = 8 THEN
                                            read_cnt := 0;
                                        END IF;
                                    END IF;
                                ELSIF (Instruct = RDAY5_4_0 OR Instruct = RDAY5_C_0) THEN
                                    dlp_act := Return_DLP_SDR(Latency_code,dummy_cnt);
                                    -- Data Learning Pattern (DLP) is enabled
                                    -- Optional DLP
                                    IF DLPV /= "00000000" AND dlp_act = true THEN
                                        IO3RESETNegOut_zd <= DLPV(7-read_cnt);
                                        WPNegOut_zd   <= DLPV(7-read_cnt);
                                        SOut_zd       <= DLPV(7-read_cnt);
                                        SIOut_zd      <= DLPV(7-read_cnt);
                                        dlp_act := FALSE;
                                        read_cnt := read_cnt + 1;
                                        IF read_cnt = 8 THEN
                                            read_cnt := 0;
                                        END IF;
                                    END IF;
                                END IF;
                            ELSE
                                data_out := to_slv(Mem(read_addr),8);
                                IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                                WPNegOut_zd   <= data_out(6-4*read_cnt);
                                SOut_zd       <= data_out(5-4*read_cnt);
                                SIOut_zd      <= data_out(4-4*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                    read_cnt := 0;

                                    IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                        IF read_addr = AddrRANGE THEN
                                            read_addr := 0;
                                        ELSE
                                            read_addr := read_addr + 1;
                                        END IF;
                                    ELSE
                                        read_addr := read_addr + 1;
                                        IF read_addr MOD WrapLength = 0 THEN
                                            read_addr := read_addr - WrapLength;
                                        END IF;
                                    END IF;
                                END IF;
                            END IF;
                        ELSE
                            IO3RESETNegOut_zd <= 'X';
                            WPNegOut_zd   <= 'X';
                            SOut_zd <= 'X';
                            SIOut_zd <= 'X';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                                IF CFR4V(4) ='0' THEN  -- Wrap Disabled
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                ELSE
                                    read_addr := read_addr + 1;
                                    IF read_addr MOD WrapLength = 0 THEN
                                        read_addr := read_addr - WrapLength;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    IF Instruct = RDAY1_C_0 OR Instruct = RDAY1_4_0 THEN
                        rd_fast <= false;
                        rd_fast1 <= false;
                        rd_slow <= true;
                        dual    <= false;
                        ddr     <= false;
                    ELSIF ((Instruct = RDAY4_C_0 OR Instruct = RDAY4_4_0) 
                           AND QUADIT = '1') THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSIF Instruct = RDAY3_C_0 OR Instruct = RDAY3_4_0 OR
                    Instruct = RDAY5_C_0 OR Instruct = RDAY5_4_0 THEN
                        rd_fast <= false;
                        rd_fast1 <= false;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSIF Instruct = RDAY7_C_0 OR Instruct = RDAY7_4_0 THEN
                        rd_fast <= false;
                        rd_fast1 <= false;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= true;
                    ELSE
                        IF QPI_IT = '1' THEN
                            rd_fast <= true;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                        ELSE
                            rd_fast <= true;
                            rd_fast1 <= false;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                        END IF;
                    END IF;
                END IF;

                IF falling_edge(write) THEN
                    IF Instruct = RSEPA_0_0 OR Instruct = RSEPD_0_0 THEN
                        STR2V(0) <= '0'; -- PS
                        STR1V(0) <= '1'; -- RDYBSY
                        PGRES  <= '1', '0' AFTER 1 ns;
                        RES_TO_SUSP_TIME <= '1', '0' AFTER res_time; -- 100us
                    ELSIF Instruct = CLECC_0_0 THEN
                        ECSV(4) := '0';
                        ECSV(3) := '0';
                        ECTV := "0000000000000000";
                        EATV := "00000000000000000000000000000000";
                    ELSIF Instruct = CLPEF_0_0 THEN
                        STR1V(6) <= '0';-- PRGERR
                        STR1V(5) <= '0';-- ERSERR
                        STR1V(0) <= '0';-- RDYBSY
                    END IF;

                    IF Instruct = SRSTE_0_0 THEN
                        RESET_EN <= '1';
                    ELSE
                        RESET_EN <= '0';
                    END IF;
                END IF;

            WHEN PASS_PG =>
                IF (Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR 
                    Instruct = RDARG_C_0) THEN
                    IF QPI_IT = '1' THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                ELSE
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= false;
                    ddr     <= false;
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;
                new_pass := PWDO_in;
                old_pass := PWDO;
                FOR j IN 0 TO 63 LOOP
                    IF old_pass(j) = '0' THEN
                        new_pass(j) := '0';
                    END IF;
                END LOOP;

                IF PDONE = '1' THEN
                    PWDO := new_pass;
                    STR1V(0)  <= '0'; -- RDYBSY
                    STR1V(1)  <= '0'; -- WRPGEN
                END IF;

            WHEN PASS_UNLOCK =>
                IF (Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR 
                    Instruct = RDARG_C_0) THEN
                    IF QPI_IT = '1' THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                ELSE
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= false;
                    ddr     <= false;
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;

                IF PASS_TEMP = PWDO THEN
                    PASS_UNLOCKED <= TRUE;
                ELSE
                    PASS_UNLOCKED <= FALSE;
                END IF;

                IF PASSULCK_out = '1' THEN
                    IF PASS_UNLOCKED AND ASPPWD = '0' THEN
                        PPLV(0)  := '1';
                        STR1V(0) <= '0'; -- RDYBSY
                        WRONG_PASS := 0;
                    ELSE
                        WRONG_PASS := 1;
                        REPORT "Incorrect Password!"
                        SEVERITY warning;
                    END IF;
                    PASSULCK_in <= '0';
                END IF;

            WHEN PPB_PG =>
                IF (Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR 
                    Instruct = RDARG_C_0) THEN
                    IF QPI_IT = '1' THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                ELSE
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= false;
                    ddr     <= false;
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;

                IF PDONE = '1' THEN
                    PPB_bits(sect):= '0';
                    STR1V(0) <= '0'; -- RDYBSY
                    STR1V(1) <= '0'; -- WRPGEN
                    STR1V_DPD <= '0'; 
                     WVREG     <= '0';  --Write volatile regs
                END IF;

            WHEN PPB_ERS =>
                IF (Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR 
                    Instruct = RDARG_C_0) THEN
                    IF QPI_IT = '1' THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                ELSE
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= false;
                    ddr     <= false;
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;

                IF PPBERASE_out = '1' THEN
                    PPB_bits:= (OTHERS => '1');
                    STR1V(0) <= '0'; -- RDYBSY
                    STR1V(1) <= '0'; -- WRPGEN
                    STR1V_DPD <= '0'; -- 
                    WVREG <= '0'; -- 
                    PPBERASE_in <= '0';
                END IF;

            WHEN AUTOBOOT_PG    =>
                IF (Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR 
                    Instruct = RDARG_C_0) THEN
                    IF QPI_IT = '1' THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;
--                 ELSIF oe_z THEN
-- --                     IO3RESETNegOut_zd <= 'Z';
-- --                     WPNegOut_zd   <= 'Z';
-- --                     SOut_zd       <= 'Z';
-- --                     SIOut_zd      <= 'Z';
-- --                 END IF;

                IF PDONE = '1' THEN
                    FOR I IN 0 TO 3 LOOP
                        FOR J IN 0 TO 7 LOOP
                            ATBN(I*8+J) :=
                            ATBN_in((3-I)*8+J);
                        END LOOP;
                    END LOOP;
                    RDYBSY  <= '0';
                    WRPGEN  <= '0';
                    STR1V_DPD <= '0';
                    WVREG     <= '0'; --Write volatile regs
                END IF;

            WHEN PLB_PG =>
                IF (Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR 
                    Instruct = RDARG_C_0) THEN
                    IF QPI_IT = '1' THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                ELSE
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= false;
                    ddr     <= false;
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;

                IF PDONE = '1' THEN
                    PPLV(0) := '0';
                    STR1V(0) <= '0'; -- RDYBSY
                    STR1V(1) <= '0'; -- WRPGEN
                    STR1V_DPD <= '0';
                    WVREG     <= '0'; --Write volatile regs
                END IF;

            WHEN DYB_PG  =>
            IF (Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR 
                    Instruct = RDARG_C_0) THEN
                    IF QPI_IT = '1' THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                ELSIF QPI_IT = '1' THEN
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= true;
                    ddr     <= false;
                ELSE
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= false;
                    ddr     <= false;
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;

                IF PDONE = '1' THEN
                    DYAV := DYAV_in;
                    IF DYAV = "11111111" THEN
                        DYB_bits(sect):= '1';
                    ELSIF DYAV = "00000000" THEN
                        DYB_bits(sect):= '0';
                    END IF;
                    STR1V(0) <= '0'; -- RDYBSY
                    STR1V(1) <= '0'; -- WRPGEN
                    STR1V_DPD <= '0';
                    WVREG     <= '0'; --Write volatile regs
                END IF;

            WHEN ASP_PG   =>
                IF (Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR 
                    Instruct = RDARG_C_0) THEN
                    IF QPI_IT = '1' THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                ELSE
                rd_fast <= true;
                rd_fast1 <= false;
                rd_slow <= false;
                dual    <= false;
                ddr     <= false;
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;

                IF PDONE = '1' THEN

                        IF ASPDYB = '0' AND ASPO_in(4) = '1' THEN
                            ASSERT false
                            REPORT "ASPDYB bit is already programmed"
                            SEVERITY warning;
                        ELSE
                            ASPO(4) := ASPO_in(4); -- ASPDYB
                        END IF;

                        IF ASPPPB = '0' AND ASPO_in(3) = '1' THEN
                            ASSERT false
                            REPORT "ASPPPB bit is already programmed"
                            SEVERITY warning;
                        ELSE
                            ASPO(3) := ASPO_in(3); -- ASPPPB
                        END IF;

                        IF ASPPRM = '0' AND ASPO_in(0) = '1' THEN
                            ASSERT false
                            REPORT "ASPPRM bit is already programmed"
                            SEVERITY warning;
                        ELSE
                            ASPO(0) := ASPO_in(0); -- ASPPRM
                        END IF;
 
                    ASPO(2) := ASPO_in(2); -- ASPPWD
                    ASPO(1) := ASPO_in(1); -- ASPPER

                    STR1V(0) <= '0'; -- RDYBSY
                    STR1V(1) <= '0'; -- WRPGEN
                    STR1V_DPD <= '0';
                    WVREG     <= '0'; --Write volatile regs
                END IF;

            WHEN NVDLR_PG   =>
                IF (Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR 
                    Instruct = RDARG_C_0) THEN
                    IF QPI_IT = '1' THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                ELSE
                rd_fast <= true;
                rd_fast1 <= false;
                rd_slow <= false;
                dual    <= false;
                ddr     <= false;
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;
                IF PDONE = '1' THEN
                    STR1V(0) <= '0'; -- RDYBSY
                    STR1V(1) <= '0'; -- WRPGEN
                    STR1V_DPD <= '0';
                    WVREG     <= '0'; --Write volatile regs
                    IF DLPN = "00000000" THEN
                        DLPN := DLPN_in;
                        DLPV := DLPN_in;
                    ELSE
                        REPORT "NVDLR is already programmed"
                        SEVERITY warning;
                    END IF;
                END IF;

            WHEN DP_DOWN =>
                rd_fast <= true;
                rd_fast1 <= false;
                rd_slow <= false;
                dual    <= false;
                ddr     <= false;
                IF oe THEN
                    IF (CSNeg = '0') THEN --???
                        any_read <= false;
                    END IF;
                END IF;

                IF falling_edge(DPDExt_out) AND falling_edge(CSNeg_ipd) THEN
                    REPORT "Device is in DPD Mode; No instructions allowed"
                    SEVERITY NOTE;
                END IF;
                IF rising_edge(DPDExt_out) THEN
                    DPD_in <=  '1', '0' AFTER 1 ns;
                    WVREG      <= '0'; --Write volatile regs
                        IF STR1V_DPD = '1' THEN
                            STR1V(1) <= '1';
                        ELSE
                            STR1V(1) <= '0';
                        END IF;
                END IF;
                IF falling_edge(RST) THEN
                    RST_in_DPD <=  '1', '0' AFTER 1 ns;
                END IF;
                
                
            WHEN RESET_STATE   =>
            -- During Reset,the non-volatile version of the registers is
            -- copied to volatile version to provide the default state of
            -- the volatile register
                STR1V(7 DOWNTO 5) <= STR1N(7 DOWNTO 5);
                STR1V(1 DOWNTO 0) <= STR1N(1 DOWNTO 0);
                DCRV := (OTHERS => '0');
                IF Instruct = SFRSL_0_0 OR Instruct = SFRST_0_0 THEN
                -- The volatile TLPROT bit (CFR1V[0]) and the volatile PPB Lock
                -- bit are not changed by the SW RESET
                    CFR1V(7 DOWNTO 1) <= CFR1N(7 DOWNTO 1);
                ELSE
                    CFR1V <= CFR1N;
                    IF ASPPWD = '0' THEN
                        PPLV(0) := '0';
                    ELSE
                        PPLV(0) := '1';
                    END IF;
                END IF;
                IF (SRNC = '0' AND ((Instruct = RDSR1_0_0) OR (Instruct = RDARG_C_0 AND Address=16#00800000#))) THEN
                      RSTRDAct <= '1';
                ELSE
                      RSTRDAct <= '0';
                END IF;

                IF (oe AND SRNC = '1' AND SWRST_out = '0' ) THEN
                     any_read <= true;
                     IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                     ELSIF (Instruct = RDARG_C_0 AND (Address=16#00000000# OR Address=16#00800000#)) THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;
                CFR2V <= CFR2N;
                CFR3V <= CFR3N;
                CFR4V <= CFR4N;

                DLPV := DLPN;
                dlp_act := false;
                --Loads the Program Buffer with all ones
                WData := (OTHERS => MaxData);

                IF TLPROT = '0' THEN
                -- When BPNV is set to '1'. the LBPROT2-0 bits in Status
                -- Register are volatile and will be reseted after
                -- reset command
                    STR1V(4 downto 2) <= STR1N(4 downto 2);
                    BP_bits := STR1V(4) & STR1V(3) & STR1V(2);
                    change_BP <= '1', '0' AFTER 1 ns;
                END IF;

                RESET_EN <= '0';

            WHEN PGERS_ERROR =>
                IF (Instruct = RDSR1_0_0 OR Instruct = RDSR2_0_0 OR Instruct = RDCR1_0_0 OR 
                    Instruct = RDARG_C_0) THEN
                    IF QPI_IT = '1' THEN
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSE
                        rd_fast <= false;
                        rd_fast1 <= true;
                        rd_slow <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                ELSIF QPI_IT = '1' THEN
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= true;
                    ddr     <= false;
                ELSE
                    rd_fast <= true;
                    rd_fast1 <= false;
                    rd_slow <= false;
                    dual    <= false;
                    ddr     <= false;
                END IF;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;

                IF falling_edge(write) THEN
                    IF (Instruct = WRDIS_0_0 AND PRGERR='0' AND ERSERR='0') OR
                       (Instruct = EXCTM_0_0 AND PRGERR='0' AND ERSERR='0') THEN
                    -- A Clear Status Register (CLPEF_0_0) followed by a Write
                    -- Disable (WRDIS_0_0) command must be sent to return the
                    -- device to standby state
                        STR1V(1) <= '0'; --WRPGEN
                        STR1V_DPD <= '0'; --
                         WVREG     <= '0'; -- Write volatile regs

                    ELSIF Instruct = CLECC_0_0 THEN
                        ECSV(4) := '0';
                        ECSV(3) := '0';
                        ECTV := "0000000000000000";
                        EATV := "00000000000000000000000000000000";

                    ELSIF Instruct = CLPEF_0_0 THEN
                        STR1V(6) <= '0'; -- PRGERR
                        STR1V(5) <= '0'; -- ERSERR
                        STR1V(0) <= '0'; -- RDYBSY
                    END IF;

                    IF Instruct = SRSTE_0_0 THEN
                        RESET_EN <= '1';
                    ELSE
                        RESET_EN <= '0';
                    END IF;
                END IF;

            WHEN BLANK_CHECK   =>
                IF rising_edge(BCDONE) THEN
                    IF NOT_BLANK = '1' THEN
                        -- Start Sector Erase
                        ESTART <= '1', '0' AFTER 1 ns;
                        ESUSP     <= '0';
                        ERES      <= '0';
                        INITIAL_CONFIG <= '1';
                        STR1V(0) <= '1'; -- RDYBSY
                        Addr := Address;
                    ELSE
                        STR1V(1) <= '1'; -- WRPGEN
                    END IF;
                ELSE
                    ADDRHILO_SEC(AddrLo, AddrHi, Addr);
                    FOR i IN AddrLo TO AddrHi LOOP
                        IF Mem(i) /= MaxData THEN
                            NOT_BLANK <= '1';
                        END IF;
                    END LOOP;
                    bc_done <= '1';
                END IF;

            WHEN EVAL_ERS_STAT =>
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;
                IF rising_edge(EESDONE) THEN
                    STR1V(0) <= '0';
                    STR1V(1) <= '0';
                    STR1V_DPD <= '0';
                     WVREG    <= '0';
                    IF ERS_nosucc(sect) = '1' THEN
                        STR2V(2) <= '0';
                    ELSE
                        STR2V(2) <= '1';
                    END IF;
                END IF;
                WHEN SEERC   =>
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR1_0_0 THEN
                        --Read Status Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDSR2_0_0 THEN
                        --Read Status Register 2
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := STR2V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= STR2V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    ELSIF Instruct = RDCR1_0_0 THEN
                        --Read Configuration Register 1
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := CFR1V;
                            IO3RESETNegOut_zd <= data_out(7-4*read_cnt);
                            WPNegOut_zd   <= data_out(6-4*read_cnt);
                            SOut_zd       <= data_out(5-4*read_cnt);
                            SIOut_zd      <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= CFR1V(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDARG_C_0 THEN
                        READ_ALL_REG(RDAR_reg, read_addr);
                        IF QPI_IT = '1' THEN
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= true;
                            ddr     <= false;
                            data_out(7 DOWNTO 0) := RDAR_reg;
                            IO3RESETNegOut_zd <= RDAR_reg(7-4*read_cnt);
                            WPNegOut_zd   <= RDAR_reg(6-4*read_cnt);
                            SOut_zd       <= RDAR_reg(5-4*read_cnt);
                            SIOut_zd      <= RDAR_reg(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            rd_fast <= false;
                            rd_fast1 <= true;
                            rd_slow <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOut_zd <= RDAR_reg(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    END IF;
                END IF;
                IF SEERC_DONE = '1' THEN
                    STR1V(0)<= '0'; --RDYBSY
                    --Mirror particular sector erase register to sector erase counter
                    SECV <= SECV_in(SectorErased); 
                END IF;
         WHEN OTHERS =>
         null;
         
        END CASE;
        IF falling_edge(write) THEN
            IF Instruct = SRSTE_0_0 AND current_state /= DP_DOWN THEN
                RESET_EN <= '1';
            ELSE
                RESET_EN <= '0';
            END IF;
        END IF;
        --Output Disable Control
        IF (CSNeg_ipd = '1') THEN
            SIOut_zd        <= 'Z';
            IO3RESETNegOut_zd   <= 'Z';
            WPNegOut_zd     <= 'Z';
            SOut_zd         <= 'Z';
        END IF;

        IF QPI_IT = '1' THEN
            dual <= true;
        END IF;

        IF rising_edge(reseted) THEN
            ERS_nosucc(SecAddr_ers) <= '1';
        END IF;

    END PROCESS Functional;

    ---------------------------------------------------------------------------
    -- SFDP_CFI Process
    ---------------------------------------------------------------------------
    SFDPPreload:    PROCESS

    BEGIN
        ------------------------------------------------------------------------
        --SFDP Header
        ------------------------------------------------------------------------
        -- Manufacturer and Device ID
        SFDP_array(16#0000#) := 16#53#;
        SFDP_array(16#0001#) := 16#46#;
        SFDP_array(16#0002#) := 16#44#;
        SFDP_array(16#0003#) := 16#50#;
        SFDP_array(16#0004#) := 16#08#;
        SFDP_array(16#0005#) := 16#01#;
        SFDP_array(16#0006#) := 16#03#;
        SFDP_array(16#0007#) := 16#FF#;
        -- 1st Parameter Header
        SFDP_array(16#0008#) := 16#00#;
        SFDP_array(16#0009#) := 16#00#;
        SFDP_array(16#000A#) := 16#01#;
        SFDP_array(16#000B#) := 16#14#;
        SFDP_array(16#000C#) := 16#00#;
        SFDP_array(16#000D#) := 16#01#;
        SFDP_array(16#000E#) := 16#00#;
        SFDP_array(16#000F#) := 16#FF#;
        -- 2nd Parameter Header
        SFDP_array(16#0010#) := 16#84#;
        SFDP_array(16#0011#) := 16#00#;
        SFDP_array(16#0012#) := 16#01#;
        SFDP_array(16#0013#) := 16#02#;
        SFDP_array(16#0014#) := 16#50#;
        SFDP_array(16#0015#) := 16#01#;
        SFDP_array(16#0016#) := 16#00#;
        SFDP_array(16#0017#) := 16#FF#;
        -- 3rd Parameter Header
        SFDP_array(16#0018#) := 16#81#;
        SFDP_array(16#0019#) := 16#00#;
        SFDP_array(16#001A#) := 16#01#;
        SFDP_array(16#001B#) := 16#16#;
        SFDP_array(16#001C#) := 16#C8#;
        SFDP_array(16#001D#) := 16#01#;
        SFDP_array(16#001E#) := 16#00#;
        SFDP_array(16#001F#) := 16#FF#;
        -- 4th Parameter Header
        SFDP_array(16#0020#) := 16#87#;
        SFDP_array(16#0021#) := 16#00#;
        SFDP_array(16#0022#) := 16#01#;
        SFDP_array(16#0023#) := 16#1C#;
        SFDP_array(16#0024#) := 16#58#;
        SFDP_array(16#0025#) := 16#01#;
        SFDP_array(16#0026#) := 16#00#;
        SFDP_array(16#0027#) := 16#FF#;
        -- Unused
        FOR I IN  16#0028# TO 16#00FF# LOOP
           SFDP_array(i) := 16#FF#;
        END LOOP;
        -----------------------------------------------------------------------
        -- JEDEC Basic Flash Parameters ----ID-CFI array data
        -----------------------------------------------------------------------
        -- DWORD-1
        SFDP_array(16#0100#) := 16#E7#;
        SFDP_array(16#0101#) := 16#20#;
        SFDP_array(16#0102#) := 16#FA#;
        SFDP_array(16#0103#) := 16#FF#;
        -- DWORD-2
        SFDP_array(16#0104#) := 16#FF#;
        SFDP_array(16#0105#) := 16#FF#;
        SFDP_array(16#0106#) := 16#FF#;
        SFDP_array(16#0107#) := 16#0F#;
        -- DWORD-3
        SFDP_array(16#0108#) := 16#48#;
        SFDP_array(16#0109#) := 16#EB#;
        SFDP_array(16#010A#) := 16#08#;
        SFDP_array(16#010B#) := 16#6B#;
        -- DWORD-4
        SFDP_array(16#010C#) := 16#00#;
        SFDP_array(16#010D#) := 16#FF#;
        SFDP_array(16#010E#) := 16#88#;
        SFDP_array(16#010F#) := 16#BB#;
        -- DWORD-5
        SFDP_array(16#0110#) := 16#FE#;
        SFDP_array(16#0111#) := 16#FF#;
        SFDP_array(16#0112#) := 16#FF#;
        SFDP_array(16#0113#) := 16#FF#;
        -- DWORD-6
        SFDP_array(16#0114#) := 16#FF#;
        SFDP_array(16#0115#) := 16#FF#;
        SFDP_array(16#0116#) := 16#00#;
        SFDP_array(16#0117#) := 16#FF#;
        -- DWORD-7
        SFDP_array(16#0118#) := 16#FF#;
        SFDP_array(16#0119#) := 16#FF#;
        SFDP_array(16#011A#) := 16#48#;
        SFDP_array(16#011B#) := 16#EB#;
        -- DWORD-8
        SFDP_array(16#011C#) := 16#0C#;
        SFDP_array(16#011D#) := 16#20#;
        SFDP_array(16#011E#) := 16#00#;
        SFDP_array(16#011F#) := 16#FF#;
        -- DWORD-9
        SFDP_array(16#0120#) := 16#00#;
        SFDP_array(16#0121#) := 16#FF#;
        SFDP_array(16#0122#) := 16#12#;
        SFDP_array(16#0123#) := 16#D8#;
        -- DWORD-10
        SFDP_array(16#0124#) := 16#23#;
        SFDP_array(16#0125#) := 16#FA#;
        SFDP_array(16#0126#) := 16#FF#;
        SFDP_array(16#0127#) := 16#8B#;
        -- DWORD-11
        SFDP_array(16#0128#) := 16#91#;
        SFDP_array(16#0129#) := 16#E8#;
        SFDP_array(16#012A#) := 16#FF#;
        SFDP_array(16#012B#) := 16#E1#;
        -- DWORD-12
        SFDP_array(16#012C#) := 16#EC#;
        SFDP_array(16#012D#) := 16#03#;
        SFDP_array(16#012E#) := 16#1C#;
        SFDP_array(16#012F#) := 16#60#;
        -- DWORD-13
        SFDP_array(16#0130#) := 16#8A#;
        SFDP_array(16#0131#) := 16#85#;
        SFDP_array(16#0132#) := 16#7A#;
        SFDP_array(16#0133#) := 16#75#;
        -- DWORD-14
        SFDP_array(16#0134#) := 16#F7#;
        SFDP_array(16#0135#) := 16#66#;
        SFDP_array(16#0136#) := 16#80#;
        SFDP_array(16#0137#) := 16#5C#;
        -- DWORD-15
        SFDP_array(16#0138#) := 16#84#;
        SFDP_array(16#0139#) := 16#D6#;
        SFDP_array(16#013A#) := 16#DD#;
        SFDP_array(16#013B#) := 16#FF#;
        -- DWORD-16
        SFDP_array(16#013C#) := 16#F9#;
        SFDP_array(16#013D#) := 16#38#;
        SFDP_array(16#013E#) := 16#F8#;
        SFDP_array(16#013F#) := 16#A1#;
        -- DWORD-17
        SFDP_array(16#0140#) := 16#00#;
        SFDP_array(16#0141#) := 16#00#;
        SFDP_array(16#0142#) := 16#00#;
        SFDP_array(16#0143#) := 16#00#;
        -- DWORD-18
        SFDP_array(16#0144#) := 16#00#;
        SFDP_array(16#0145#) := 16#00#;
        SFDP_array(16#0146#) := 16#80#;
        SFDP_array(16#0147#) := 16#00#;
        -- DWORD-19
        SFDP_array(16#0148#) := 16#00#;
        SFDP_array(16#0149#) := 16#00#;
        SFDP_array(16#014A#) := 16#00#;
        SFDP_array(16#014B#) := 16#00#;
        -- DWORD-20
        SFDP_array(16#014C#) := 16#F7#;
        SFDP_array(16#014D#) := 16#F5#;
        SFDP_array(16#014E#) := 16#FF#;
        SFDP_array(16#014F#) := 16#FF#;

        
        -- JEDEC 4-Byte Address Instructions Parameter DWORD-1
        SFDP_array(16#0150#) := 16#7B#;
        SFDP_array(16#0151#) := 16#92#;
        SFDP_array(16#0152#) := 16#0F#;
        SFDP_array(16#0153#) := 16#FE#;
        -- JEDEC 4-Byte Address Instructions Parameter DWORD-2
        SFDP_array(16#0154#) := 16#20#;
        SFDP_array(16#0155#) := 16#FF#;
        SFDP_array(16#0156#) := 16#FF#;
        SFDP_array(16#0157#) := 16#D8#;
        
        -----------------------------------------------------------------------
        -- Status, Control and Configuration Register Map Offsets for
        -- Multi-Chip SPI Memory Devices
        -----------------------------------------------------------------------
        
        -- Status, Control and Configuration Register Map DWORD-1
        SFDP_array(16#0158#) := 16#00#;
        SFDP_array(16#0159#) := 16#00#;
        SFDP_array(16#015A#) := 16#80#;
        SFDP_array(16#015B#) := 16#00#;
        -- Status, Control and Configuration Register Map DWORD-2
        SFDP_array(16#015C#) := 16#00#;
        SFDP_array(16#015D#) := 16#00#;
        SFDP_array(16#015E#) := 16#00#;
        SFDP_array(16#015F#) := 16#00#;
        -- Status, Control and Configuration Register Map DWORD-3
        SFDP_array(16#0160#) := 16#C0#;
        SFDP_array(16#0161#) := 16#FF#;
        SFDP_array(16#0162#) := 16#C3#;
        SFDP_array(16#0163#) := 16#EB#;
        -- Status, Control and Configuration Register Map DWORD-4
        SFDP_array(16#0164#) := 16#C8#;
        SFDP_array(16#0165#) := 16#FF#;
        SFDP_array(16#0166#) := 16#E3#;
        SFDP_array(16#0167#) := 16#EB#;
        -- Status, Control and Configuration Register Map DWORD-5
        SFDP_array(16#0168#) := 16#00#;
        SFDP_array(16#0169#) := 16#65#;
        SFDP_array(16#016A#) := 16#00#;
        SFDP_array(16#016B#) := 16#90#;

        -- Status, Control and Configuration Register Map DWORD-6
        SFDP_array(16#016C#) := 16#06#;
        SFDP_array(16#016D#) := 16#05#;
        SFDP_array(16#016E#) := 16#00#;
        SFDP_array(16#016F#) := 16#A1#;
        -- Status, Control and Configuration Register Map DWORD-7
        SFDP_array(16#0170#) := 16#00#;
        SFDP_array(16#0171#) := 16#65#;
        SFDP_array(16#0172#) := 16#00#;
        SFDP_array(16#0173#) := 16#96#;
        -- Status, Control and Configuration Register Map DWORD-8
        SFDP_array(16#0174#) := 16#00#;
        SFDP_array(16#0175#) := 16#65#;
        SFDP_array(16#0176#) := 16#00#;
        SFDP_array(16#0177#) := 16#95#;
        -- Status, Control and Configuration Register Map DWORD-9
        SFDP_array(16#0178#) := 16#71#;
        SFDP_array(16#0179#) := 16#65#;
        SFDP_array(16#017A#) := 16#03#;
        SFDP_array(16#017B#) := 16#D0#;
        -- Status, Control and Configuration Register Map DWORD-10
        SFDP_array(16#017C#) := 16#71#;
        SFDP_array(16#017D#) := 16#65#;
        SFDP_array(16#017E#) := 16#03#;
        SFDP_array(16#017F#) := 16#D0#;
        -- Status, Control and Configuration Register Map DWORD-11
        SFDP_array(16#0180#) := 16#00#;
        SFDP_array(16#0181#) := 16#00#;
        SFDP_array(16#0182#) := 16#00#;
        SFDP_array(16#0183#) := 16#00#;
        -- Status, Control and Configuration Register Map DWORD-12
        SFDP_array(16#0184#) := 16#B0#;
        SFDP_array(16#0185#) := 16#2E#;
        SFDP_array(16#0186#) := 16#00#;
        SFDP_array(16#0187#) := 16#00#;
        -- Status, Control and Configuration Register Map DWORD-13
        SFDP_array(16#0188#) := 16#88#;
        SFDP_array(16#0189#) := 16#A4#;
        SFDP_array(16#018A#) := 16#89#;
        SFDP_array(16#018B#) := 16#AA#;
        -- Status, Control and Configuration Register Map DWORD-14
        SFDP_array(16#018C#) := 16#71#;
        SFDP_array(16#018D#) := 16#65#;
        SFDP_array(16#018E#) := 16#03#;
        SFDP_array(16#018F#) := 16#96#;
        -- Status, Control and Configuration Register Map DWORD-15
        SFDP_array(16#0190#) := 16#71#;
        SFDP_array(16#0191#) := 16#65#;
        SFDP_array(16#0192#) := 16#03#;
        SFDP_array(16#0193#) := 16#96#;
        -- Status, Control and Configuration Register Map DWORD-16
        SFDP_array(16#0194#) := 16#00#;
        SFDP_array(16#0195#) := 16#00#;
        SFDP_array(16#0196#) := 16#00#;
        SFDP_array(16#0197#) := 16#00#;
        -- Status, Control and Configuration Register Map DWORD-17
        SFDP_array(16#0198#) := 16#00#;
        SFDP_array(16#0199#) := 16#00#;
        SFDP_array(16#019A#) := 16#00#;
        SFDP_array(16#019B#) := 16#00#;
        -- Status, Control and Configuration Register Map DWORD-18
        SFDP_array(16#019C#) := 16#00#;
        SFDP_array(16#019D#) := 16#00#;
        SFDP_array(16#019E#) := 16#00#;
        SFDP_array(16#019F#) := 16#00#;
        -- Unused
        FOR I IN  16#01A0# TO 16#0200# LOOP
           SFDP_array(i) := 16#FF#;
        END LOOP;
        -- Status, Control and Configuration Register Map DWORD-19
        SFDP_array(16#01A0#) := 16#00#;
        SFDP_array(16#01A1#) := 16#00#;
        SFDP_array(16#01A2#) := 16#00#;
        SFDP_array(16#01A3#) := 16#00#;
        -- Status, Control and Configuration Register Map DWORD-20
        SFDP_array(16#01A4#) := 16#00#;
        SFDP_array(16#01A5#) := 16#00#;
        SFDP_array(16#01A6#) := 16#00#;
        SFDP_array(16#01A7#) := 16#00#;
        -- Status, Control and Configuration Register Map DWORD-21
        SFDP_array(16#01A8#) := 16#00#;
        SFDP_array(16#01A9#) := 16#00#;
        SFDP_array(16#01AA#) := 16#00#;
        SFDP_array(16#01AB#) := 16#00#;
        -- Status, Control and Configuration Register Map DWORD-22
        SFDP_array(16#01AC#) := 16#00#;
        SFDP_array(16#01AD#) := 16#00#;
        SFDP_array(16#01AE#) := 16#00#;
        SFDP_array(16#01AF#) := 16#00#;
        -- Status, Control and Configuration Register Map DWORD-23
        SFDP_array(16#01B0#) := 16#00#;
        SFDP_array(16#01B1#) := 16#00#;
        SFDP_array(16#01B2#) := 16#00#;
        SFDP_array(16#01B3#) := 16#00#;
        -- Status, Control and Configuration Register Map DWORD-24
        SFDP_array(16#01B4#) := 16#00#;
        SFDP_array(16#01B5#) := 16#00#;
        SFDP_array(16#01B6#) := 16#00#;
        SFDP_array(16#01B7#) := 16#00#;
        -- Status, Control and Configuration Register Map DWORD-25
        SFDP_array(16#01B8#) := 16#00#;
        SFDP_array(16#01B9#) := 16#00#;
        SFDP_array(16#01BA#) := 16#00#;
        SFDP_array(16#01BB#) := 16#00#;
        -- Status, Control and Configuration Register Map DWORD-26
        SFDP_array(16#01BC#) := 16#71#;
        SFDP_array(16#01BD#) := 16#65#;
        SFDP_array(16#01BE#) := 16#05#;
        SFDP_array(16#01BF#) := 16#D5#;
        -- Status, Control and Configuration Register Map DWORD-27
        SFDP_array(16#01C0#) := 16#71#;
        SFDP_array(16#01C1#) := 16#65#;
        SFDP_array(16#01C2#) := 16#05#;
        SFDP_array(16#01C3#) := 16#D5#;
        -- Status, Control and Configuration Register Map DWORD-28
        SFDP_array(16#01C4#) := 16#00#;
        SFDP_array(16#01C5#) := 16#00#;
        SFDP_array(16#01C6#) := 16#EE#;
        SFDP_array(16#01C7#) := 16#72#;
        
        
        -- Sector Map DWORD-1
        SFDP_array(16#01C8#) := 16#FC#;
        SFDP_array(16#01C9#) := 16#65#;
        SFDP_array(16#01CA#) := 16#FF#;
        SFDP_array(16#01CB#) := 16#08#;
        -- Sector Map DWORD-2
        SFDP_array(16#01CC#) := 16#04#;
        SFDP_array(16#01CD#) := 16#00#;
        SFDP_array(16#01CE#) := 16#80#;
        SFDP_array(16#01CF#) := 16#00#;
        -- Sector Map DWORD-3
        SFDP_array(16#01D0#) := 16#FC#;
        SFDP_array(16#01D1#) := 16#65#;
        SFDP_array(16#01D2#) := 16#FF#;
        SFDP_array(16#01D3#) := 16#40#;
        -- Sector Map DWORD-4
        SFDP_array(16#01D4#) := 16#02#;
        SFDP_array(16#01D5#) := 16#00#;
        SFDP_array(16#01D6#) := 16#80#;
        SFDP_array(16#01D7#) := 16#00#;
        -- Sector Map DWORD-5
        SFDP_array(16#01D8#) := 16#FD#;
        SFDP_array(16#01D9#) := 16#65#;
        SFDP_array(16#01DA#) := 16#FF#;
        SFDP_array(16#01DB#) := 16#04#;
        -- Sector Map DWORD-6
        SFDP_array(16#01DC#) := 16#02#;
        SFDP_array(16#01DD#) := 16#00#;
        SFDP_array(16#01DE#) := 16#80#;
        SFDP_array(16#01DF#) := 16#00#;
        -- Sector Map DWORD-7
        SFDP_array(16#01E0#) := 16#FE#;
        SFDP_array(16#01E1#) := 16#00#;
        SFDP_array(16#01E2#) := 16#02#;
        SFDP_array(16#01E3#) := 16#FF#;
        -- Sector Map DWORD-8
        SFDP_array(16#01E4#) := 16#F1#;
        SFDP_array(16#01E5#) := 16#FF#;
        SFDP_array(16#01E6#) := 16#01#;
        SFDP_array(16#01E7#) := 16#00#;
        -- Sector Map DWORD-9
        SFDP_array(16#01E8#) := 16#F8#;
        SFDP_array(16#01E9#) := 16#FF#;
        SFDP_array(16#01EA#) := 16#01#;
        SFDP_array(16#01EB#) := 16#00#;
        -- Sector Map DWORD-10
        SFDP_array(16#01EC#) := 16#F8#;
        SFDP_array(16#01ED#) := 16#FF#;
        SFDP_array(16#01EE#) := 16#FB#;
        SFDP_array(16#01EF#) := 16#01#;
        -- Sector Map DWORD-11
        SFDP_array(16#01F0#) := 16#FE#;
        SFDP_array(16#01F1#) := 16#03#;
        SFDP_array(16#01F2#) := 16#02#;
        SFDP_array(16#01F3#) := 16#FF#;
        -- Sector Map DWORD-12
        SFDP_array(16#01F4#) := 16#F8#;
        SFDP_array(16#01F5#) := 16#FF#;
        SFDP_array(16#01F6#) := 16#FB#;
        SFDP_array(16#01F7#) := 16#01#;
        -- Sector Map DWORD-13
        SFDP_array(16#01F8#) := 16#F8#;
        SFDP_array(16#01F9#) := 16#FF#;
        SFDP_array(16#01FA#) := 16#01#;
        SFDP_array(16#01FB#) := 16#00#;
        -- Sector Map DWORD-14
        SFDP_array(16#01FC#) := 16#F1#;
        SFDP_array(16#01FD#) := 16#FF#;
        SFDP_array(16#01FE#) := 16#01#;
        SFDP_array(16#01FF#) := 16#00#;
        -- Sector Map DWORD-15
        SFDP_array(16#0200#) := 16#FE#;
        SFDP_array(16#0201#) := 16#01#;
        SFDP_array(16#0202#) := 16#04#;
        SFDP_array(16#0203#) := 16#FF#;
        -- Sector Map DWORD-16
        SFDP_array(16#0204#) := 16#F1#;
        SFDP_array(16#0205#) := 16#FF#;
        SFDP_array(16#0206#) := 16#01#;
        SFDP_array(16#0207#) := 16#00#;
        -- Sector Map DWORD-17
        SFDP_array(16#0208#) := 16#F8#;
        SFDP_array(16#0209#) := 16#FF#;
        SFDP_array(16#020A#) := 16#02#;
        SFDP_array(16#020B#) := 16#00#;
        -- Sector Map DWORD-18
        SFDP_array(16#020C#) := 16#F8#;
        SFDP_array(16#020D#) := 16#FF#;
        SFDP_array(16#020E#) := 16#F7#;
        SFDP_array(16#020F#) := 16#01#;
        -- Sector Map DWORD-19
        SFDP_array(16#0210#) := 16#F8#;
        SFDP_array(16#0211#) := 16#FF#;
        SFDP_array(16#0212#) := 16#02#;
        SFDP_array(16#0213#) := 16#00#;
        -- Sector Map DWORD-20
        SFDP_array(16#0214#) := 16#F1#;
        SFDP_array(16#0215#) := 16#FF#;
        SFDP_array(16#0216#) := 16#01#;
        SFDP_array(16#0217#) := 16#00#;
        -- Sector Map DWORD-21
        SFDP_array(16#0218#) := 16#FF#;
        SFDP_array(16#0219#) := 16#04#;
        SFDP_array(16#021A#) := 16#00#;
        SFDP_array(16#021B#) := 16#FF#;
        -- Sector Map DWORD-22
        SFDP_array(16#021C#) := 16#F8#;
        SFDP_array(16#021D#) := 16#FF#;
        SFDP_array(16#021E#) := 16#FF#;
        SFDP_array(16#021F#) := 16#01#;


        FOR I IN SFDPLength DOWNTO 0 LOOP
            SFDP_tmp := to_slv(SFDP_array(SFDPLength-I),8);
            FOR J IN 7 DOWNTO 0 LOOP
                SFDP_array_tmp(8*I +J) := SFDP_tmp(J);
            END LOOP;
        END LOOP;

        WAIT;

    END PROCESS SFDPPreload;
    
    MDIDPreload:    PROCESS
    BEGIN
        -------------------------------
        --MDID 
        -------------------------------
        MDID_array(16#0000#) := 16#34#;
        MDID_array(16#0001#) := 16#2A#;
        MDID_array(16#0002#) := 16#19#;
        MDID_array(16#0003#) := 16#0F#;
        MDID_array(16#0004#) := 16#03#;
        MDID_array(16#0005#) := 16#90#;
        MDID_array(16#0006#) := 16#FF#;
        MDID_array(16#0007#) := 16#FF#;

        MDID_array(16#0008#) := 16#FF#;
        MDID_array(16#0009#) := 16#FF#;
        MDID_array(16#000A#) := 16#FF#;
        MDID_array(16#000B#) := 16#FF#;
        MDID_array(16#000C#) := 16#FF#;
        MDID_array(16#000D#) := 16#FF#;
        MDID_array(16#000E#) := 16#FF#;
        MDID_array(16#000F#) := 16#FF#;

        FOR I IN MDIDLength DOWNTO 0 LOOP
            MDID_tmp := to_slv(MDID_array(MDIDLength-I),8);
            FOR J IN 7 DOWNTO 0 LOOP
                MDID_array_tmp(8*I +J) := MDID_tmp(J);
            END LOOP;
        END LOOP;

        WAIT;
    END PROCESS MDIDPreload;

    Protect : PROCESS(change_BP)
    BEGIN
        IF rising_edge(change_BP) THEN

            CASE STR1V(4 DOWNTO 2) IS
                WHEN "000" =>
                    Sec_Prot := (OTHERS => '0');
                WHEN "001" =>
                    IF CFR3V(3) = '1' THEN -- Uniform Sector Architecture
                        IF TBPROT_NV = '0' THEN
                            Sec_Prot := (OTHERS => '0');
                            Sec_Prot(SecNumUni downto (SecNumUni+1)*63/64)
                                                   := (OTHERS => '1');
                        ELSE
                            Sec_Prot := (OTHERS => '0');
                            Sec_Prot((SecNumUni+1)/64-1 downto 0)
                                                    := (OTHERS => '1');
                        END IF;
                    ELSIF (CFR3V(3) = '0' AND SP4KBS_NV = '1') THEN
                        IF TBPROT_NV = '0' THEN
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(SecNumHyb downto (SecNumHyb-19))
                                                    := (OTHERS => '1');
                        ELSE
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(19 downto 0) := (OTHERS => '1');
                        END IF;
                    ELSE -- / Hybrid Sector Architecture
                        IF TB4KBS_NV = '1' THEN --4 KB Physical Sectors at Top
                            IF TBPROT_NV = '0' THEN -- BP starts at Top
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(SecNumHyb downto (SecNumHyb-31)*63/64)
                                                    := (OTHERS => '1');
                            ELSE
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot((SecNumHyb-31)/64-1 downto 0)
                                                    := (OTHERS => '1');
                            END IF;
                        ELSE --4 KB Physical Sectors at Bottom
                            IF TBPROT_NV = '0' THEN -- BP starts at Top
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(SecNumHyb downto (SecNumHyb-31)*63/64+8)
                                                    := (OTHERS => '1');
                            ELSE  -- BP starts at Bottom
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot((SecNumHyb-31)/64+7 downto 0)
                                                    := (OTHERS => '1');
                            END IF;
                        END IF;
                    END IF;


                WHEN "010" =>
                    IF CFR3V(3) = '1' THEN -- Uniform Sector Architecture
                        IF TBPROT_NV = '0' THEN
                            Sec_Prot := (OTHERS => '0');
                            Sec_Prot(SecNumUni downto (SecNumUni+1)*31/32)
                                                    := (OTHERS => '1');
                        ELSE
                            Sec_Prot := (OTHERS => '0');
                            Sec_Prot((SecNumUni+1)/32-1 downto 0)
                                                    := (OTHERS => '1');
                        END IF;
                    ELSIF (CFR3V(3) = '0' AND SP4KBS_NV = '1') THEN
                        IF TBPROT_NV = '0' THEN
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(SecNumHyb downto (SecNumHyb-23))
                                                    := (OTHERS => '1');
                        ELSE
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(23 downto 0) := (OTHERS => '1');
                        END IF;
                    ELSE -- / Hybrid Sector Architecture
                        IF TB4KBS_NV = '1' THEN --4 KB Physical Sectors at Top
                            IF TBPROT_NV = '0' THEN -- BP starts at Top
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(SecNumHyb downto (SecNumHyb-31)*31/32)
                                                    := (OTHERS => '1');
                            ELSE
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot((SecNumHyb-31)/32-1 downto 0)
                                                    := (OTHERS => '1');
                            END IF;
                        ELSE --4 KB Physical Sectors at Bottom
                            IF TBPROT_NV = '0' THEN -- BP starts at Top
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(SecNumHyb downto (SecNumHyb-31)*31/32+8)
                                                    := (OTHERS => '1');
                            ELSE  -- BP starts at Bottom
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot((SecNumHyb-31)/32+7 downto 0)
                                                    := (OTHERS => '1');
                            END IF;
                        END IF;
                    END IF;

                WHEN "011" =>
                    IF CFR3V(3) = '1' THEN -- Uniform Sector Architecture
                        IF TBPROT_NV = '0' THEN
                            Sec_Prot := (OTHERS => '0');
                            Sec_Prot(SecNumUni downto (SecNumUni+1)*15/16)
                                                    := (OTHERS => '1');
                        ELSE
                            Sec_Prot := (OTHERS => '0');
                            Sec_Prot((SecNumUni+1)/16-1 downto 0)
                                                    := (OTHERS => '1');
                        END IF;
                    ELSIF (CFR3V(3) = '0' AND SP4KBS_NV = '1') THEN
                        IF TBPROT_NV = '0' THEN
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(SecNumHyb downto (SecNumHyb-31))
                                                    := (OTHERS => '1');
                        ELSE
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(31 downto 0) := (OTHERS => '1');
                        END IF;
                    ELSE -- / Hybrid Sector Architecture
                        IF TB4KBS_NV = '1' THEN --4 KB Physical Sectors at Top
                            IF TBPROT_NV = '0' THEN -- BP starts at Top
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(SecNumHyb downto (SecNumHyb-31)*15/16)
                                                    := (OTHERS => '1');
                            ELSE
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot((SecNumHyb-31)/16-1 downto 0)
                                                    := (OTHERS => '1');
                            END IF;
                        ELSE --4 KB Physical Sectors at Bottom
                            IF TBPROT_NV = '0' THEN -- BP starts at Top
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(SecNumHyb downto (SecNumHyb-31)*15/16+8)
                                                    := (OTHERS => '1');
                            ELSE  -- BP starts at Bottom
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot((SecNumHyb-31)/16+7 downto 0)
                                                    := (OTHERS => '1');
                            END IF;
                        END IF;
                    END IF;

                WHEN "100" =>
                    IF CFR3V(3) = '1' THEN -- Uniform Sector Architecture
                        IF TBPROT_NV = '0' THEN
                            Sec_Prot := (OTHERS => '0');
                            Sec_Prot(SecNumUni downto (SecNumUni+1)*7/8)
                                                    := (OTHERS => '1');
                        ELSE
                            Sec_Prot := (OTHERS => '0');
                            Sec_Prot((SecNumUni+1)/8-1 downto 0)
                                                    := (OTHERS => '1');
                        END IF;
                    ELSIF (CFR3V(3) = '0' AND SP4KBS_NV = '1') THEN
                        IF TBPROT_NV = '0' THEN
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(SecNumHyb downto (SecNumHyb-47))
                                                    := (OTHERS => '1');
                        ELSE
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(47 downto 0) := (OTHERS => '1');
                        END IF;
                    ELSE -- / Hybrid Sector Architecture
                        IF TB4KBS_NV = '1' THEN --4 KB Physical Sectors at Top
                            IF TBPROT_NV = '0' THEN -- BP starts at Top
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(SecNumHyb downto (SecNumHyb-31)*7/8)
                                                    := (OTHERS => '1');
                            ELSE
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot((SecNumHyb-31)/8-1 downto 0)
                                                    := (OTHERS => '1');
                            END IF;
                        ELSE --4 KB Physical Sectors at Bottom
                            IF TBPROT_NV = '0' THEN -- BP starts at Top
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(SecNumHyb downto (SecNumHyb-31)*7/8+8)
                                                    := (OTHERS => '1');
                            ELSE  -- BP starts at Bottom
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot((SecNumHyb-31)/8+7 downto 0)
                                                    := (OTHERS => '1');
                            END IF;
                        END IF;
                    END IF;

                WHEN "101" =>
                    IF CFR3V(3) = '1' THEN -- Uniform Sector Architecture
                        IF TBPROT_NV = '0' THEN
                            Sec_Prot := (OTHERS => '0');
                            Sec_Prot(SecNumUni downto (SecNumUni+1)*3/4)
                                                    := (OTHERS => '1');
                        ELSE
                            Sec_Prot := (OTHERS => '0');
                            Sec_Prot((SecNumUni+1)/4-1 downto 0)
                                                    := (OTHERS => '1');
                        END IF;
                    ELSIF (CFR3V(3) = '0' AND SP4KBS_NV = '1') THEN
                        IF TBPROT_NV = '0' THEN
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(SecNumHyb downto (SecNumHyb-79))
                                                    := (OTHERS => '1');
                        ELSE
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(79 downto 0) := (OTHERS => '1');
                        END IF;
                    ELSE -- / Hybrid Sector Architecture
                        IF TB4KBS_NV = '1' THEN --4 KB Physical Sectors at Top
                            IF TBPROT_NV = '0' THEN -- BP starts at Top
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(SecNumHyb downto (SecNumHyb-31)*3/4)
                                                    := (OTHERS => '1');
                            ELSE
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot((SecNumHyb-31)/4-1 downto 0)
                                                    := (OTHERS => '1');
                            END IF;
                        ELSE --4 KB Physical Sectors at Bottom
                            IF TBPROT_NV = '0' THEN -- BP starts at Top
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(SecNumHyb downto (SecNumHyb-31)*3/4+8)
                                                    := (OTHERS => '1');
                            ELSE  -- BP starts at Bottom
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot((SecNumHyb-31)/4+7 downto 0)
                                                    := (OTHERS => '1');
                            END IF;
                        END IF;
                    END IF;

                WHEN "110" =>
                    IF CFR3V(3) = '1' THEN -- Uniform Sector Architecture
                        IF TBPROT_NV = '0' THEN
                            Sec_Prot := (OTHERS => '0');
                            Sec_Prot(SecNumUni downto (SecNumUni+1)/2)
                                                    := (OTHERS => '1');
                        ELSE
                            Sec_Prot := (OTHERS => '0');
                            Sec_Prot((SecNumUni+1)/2-1 downto 0)
                                                    := (OTHERS => '1');
                        END IF;
                    ELSIF (CFR3V(3) = '0' AND SP4KBS_NV = '1') THEN
                        IF TBPROT_NV = '0' THEN
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(SecNumHyb downto (SecNumHyb-143))
                                                    := (OTHERS => '1');
                        ELSE
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(143 downto 0) := (OTHERS => '1');
                        END IF;
                    ELSE -- / Hybrid Sector Architecture
                        IF TB4KBS_NV = '1' THEN --4 KB Physical Sectors at Top
                            IF TBPROT_NV = '0' THEN -- BP starts at Top
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(SecNumHyb downto (SecNumHyb-31)/2)
                                                    := (OTHERS => '1');
                            ELSE
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot((SecNumHyb-31)/2-1 downto 0)
                                                    := (OTHERS => '1');
                            END IF;
                        ELSE --4 KB Physical Sectors at Bottom
                            IF TBPROT_NV = '0' THEN -- BP starts at Top
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot(SecNumHyb downto (SecNumHyb-31)/2+8)
                                                    := (OTHERS => '1');
                            ELSE  -- BP starts at Bottom
                                Sec_Prot := (OTHERS => '0');
                                Sec_Prot((SecNumHyb-31)/2+7 downto 0)
                                                    := (OTHERS => '1');
                            END IF;
                        END IF;
                    END IF;

                WHEN OTHERS =>
                    Sec_Prot := (OTHERS => '1');
            END CASE;
        END IF;
    END PROCESS Protect;

    WP_PULL_UP : PROCESS(WPNegIn)
    BEGIN
        IF (QUADIT = '0') THEN
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
        VARIABLE S_ind        : NATURAL RANGE 0 TO SecNumHyb:= 0;
        VARIABLE index        : NATURAL RANGE 0 TO SecSize256:=0;
        VARIABLE otp_ind      : NATURAL RANGE 16#000# TO 16#3FF# := 16#000#;
        VARIABLE buf          : line;
        VARIABLE reported     : NATURAL;

    BEGIN
    ---------------------------------------------------------------------------
    --s35hl256t memory preload file format
-----------------------------------
    ---------------------------------------------------------------------------
    --   /       - comment
    --   @aaaaaa - <aaaaaa> stands for address
    --   dd      - <dd> is byte to be written at Mem(aaaaaa++)
    --             (aaaaaa is incremented at every load)
    --   only first 1-7 columns are loaded. NO empty lines !!!!!!!!!!!!!!!!
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
                    ind := h(buf(2 to 8));
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
    --s35hl256t_otp memory preload file format
    ---------------------------------------------------------------------------
    --   /       - comment
    --   @aaa - <aaa> stands for address
    --   dd      - <dd> is byte to be written at OTPMem(aaa++)
    --             (aaa is incremented at every load)
    --   only first 1-4 columns are loaded. NO empty lines !!!!!!!!!!!!!!!!
    ---------------------------------------------------------------------------

         -- memory preload
        IF (otp_file_name /= "none" AND UserPreload) THEN
            otp_ind := 16#000#;
            OTPMem := (OTHERS => MaxData);
            WHILE (not ENDFILE (otp_file)) LOOP
                READLINE (otp_file, buf);
                IF buf(1) = '/' THEN
                    NEXT;
                ELSIF buf(1) = '@' THEN
                    IF otp_ind > 16#3FF# OR otp_ind < 16#000# THEN
                        ASSERT false
                            REPORT "Given preload address is out of" &
                                   "OTP address range"
                            SEVERITY warning;
                    ELSE
                        otp_ind := h(buf(2 to 4)); --address
                    END IF;
                ELSE
                    OTPMem(otp_ind) := h(buf(1 to 2));
                    otp_ind := otp_ind + 1;
                END IF;
            END LOOP;
        END IF;

        LOCK_BYTE1 := to_slv(OTPMem(16#10#),8);
        LOCK_BYTE2 := to_slv(OTPMem(16#11#),8);
        LOCK_BYTE3 := to_slv(OTPMem(16#12#),8);
        LOCK_BYTE4 := to_slv(OTPMem(16#13#),8);

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
                        PathDelay => VitalExtendtofillDelay(tpd_sck_so_timing_30),
                        PathCondition   => (ddr = FALSE AND timing_30 = '1')),
                    1 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_sck_so_ntb_timing_30),
                        PathCondition   => (ddr = FALSE AND timing_30 = '0')),
                    2 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay   => VitalExtendtofillDelay(tpd_sck_so_op_ddr_an_timing_30_cp),
                        PathCondition   => (ddr AND timing_30 = '1')),
                    3 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay   => VitalExtendtofillDelay(tpd_sck_so_op_ddr_an_ntb_timing_30_cp),
                        PathCondition   => (ddr AND  timing_30 = '0')),
                    4 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_SO_rst_quad_EQ_0,
                        PathCondition   => CSNeg_ipd = '1' AND NOT rst_quad),
                    5 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_SO_rst_quad_EQ_1,
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
                        PathDelay => VitalExtendtofillDelay(tpd_sck_so_timing_30),
                        PathCondition   => (ddr = FALSE AND timing_30 = '1' AND dual)),
                    1 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_sck_so_ntb_timing_30),
                        PathCondition   => (ddr = FALSE AND timing_30 = '0' AND dual)),
                    2 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay   => VitalExtendtofillDelay(tpd_sck_so_op_ddr_an_timing_30_cp),
                        PathCondition   => (ddr AND timing_30 = '1' AND dual)),
                    3 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay   => VitalExtendtofillDelay(tpd_sck_so_op_ddr_an_ntb_timing_30_cp),
                        PathCondition   => (ddr AND  timing_30 = '0' AND dual)),
                    4 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_SO_rst_quad_EQ_0,
                        PathCondition   => CSNeg_ipd = '1' AND NOT rst_quad
                                            AND dual),
                    5 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_SO_rst_quad_EQ_1,
                        PathCondition   => CSNeg_ipd = '1' AND rst_quad
                                            AND dual)
                )
            );
        END PROCESS;

    RESET_Out_PathDelay : PROCESS(RESETNegOut_zd)

            VARIABLE WP_GlitchData : VitalGlitchDataType;
        BEGIN
            VitalPathDelay01Z (
                OutSignal       => RESETNegOut,
                OutSignalName   => "RESETNeg",
                OutTemp         => RESETNegOut_zd,
                Mode            => VitalTransport,
                GlitchData      => WP_GlitchData,
                Paths           => (
                    0 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_SCK_SO),
                        PathCondition   =>  not(ddr) AND (QPI_IT = '1' OR QUADIT = '1')),
                    1 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_SCK_SO),
                        PathCondition   => ddr AND (QPI_IT = '1' OR QUADIT = '1')),
                    2 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_SO_rst_quad_EQ_0,
                        PathCondition   => CSNeg_ipd = '1' AND
                                            NOT rst_quad AND(QPI_IT = '1' OR QUADIT = '1')),
                    3 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_SO_rst_quad_EQ_1,
                        PathCondition   => CSNeg_ipd = '1' AND
                                            rst_quad AND (QPI_IT = '1' OR QUADIT = '1'))
                )
            );
        END PROCESS;

    IO3_RESET_Out_PathDelay : PROCESS(IO3RESETNegOut_zd)

            VARIABLE WP_GlitchData : VitalGlitchDataType;
        BEGIN
            VitalPathDelay01Z (
                OutSignal       => IO3RESETNegOut,
                OutSignalName   => "IO3RESETNeg",
                OutTemp         => IO3RESETNegOut_zd,
                Mode            => VitalTransport,
                GlitchData      => WP_GlitchData,
                Paths           => (
                    0 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_sck_so_timing_30),
                        PathCondition   =>  not(ddr) AND (QPI_IT = '1' OR QUADIT = '1') AND timing_30 = '1'),
                    1 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_sck_so_ntb_timing_30),
                        PathCondition   =>  not(ddr) AND (QPI_IT = '1' OR QUADIT = '1') AND timing_30 = '0'),
                    2 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_sck_so_op_ddr_an_timing_30_cp),
                        PathCondition   => ddr AND (QPI_IT = '1' OR QUADIT = '1') AND timing_30 = '1'),
                    3 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_sck_so_op_ddr_an_ntb_timing_30_cp),
                        PathCondition   => ddr AND (QPI_IT = '1' OR QUADIT = '1') AND timing_30 = '0'),
                    4 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_SO_rst_quad_EQ_0,
                        PathCondition   => CSNeg_ipd = '1' AND
                                            NOT rst_quad AND (QPI_IT = '1' OR QUADIT = '1')),
                    5 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_SO_rst_quad_EQ_1,
                        PathCondition   => CSNeg_ipd = '1' AND
                                            rst_quad AND (QPI_IT = '1' OR QUADIT = '1'))
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
                        PathDelay => VitalExtendtofillDelay(tpd_sck_so_timing_30),
                        PathCondition   =>  not(ddr) AND (QPI_IT = '1' OR QUADIT = '1') AND timing_30 = '1'),
                    1 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_sck_so_ntb_timing_30),
                        PathCondition   =>  not(ddr) AND (QPI_IT = '1' OR QUADIT = '1') AND timing_30 = '0'),
                    2 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_sck_so_op_ddr_an_timing_30_cp),
                        PathCondition   => ddr AND (QPI_IT = '1' OR QUADIT = '1') AND timing_30 = '1'),
                    3 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_sck_so_op_ddr_an_ntb_timing_30_cp),
                        PathCondition   => ddr AND (QPI_IT = '1' OR QUADIT = '1') AND timing_30 = '0'),
                    4 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_SO_rst_quad_EQ_0,
                        PathCondition   => CSNeg_ipd = '1' AND
                                            NOT rst_quad AND (QPI_IT = '1' OR QUADIT = '1')),
                    5 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_SO_rst_quad_EQ_1,
                        PathCondition   => CSNeg_ipd = '1' AND
                                            rst_quad AND (QPI_IT = '1' OR QUADIT = '1'))
                )
            );
        END PROCESS;

    END BLOCK behavior;
END vhdl_behavioral;

