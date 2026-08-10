-------------------------------------------------------------------------------
--  File Name: at25df161.vhd
-------------------------------------------------------------------------------
--  Copyright (C) 2009 Free Model Foundry; http://www.FreeModelFoundry.com
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License version 2 as
--  published by the Free Software Foundation.
--
--  MODIFICATION HISTORY:
--
--  version:   |      author:       |   mod date:    |  changes made:
--    V1.0         H.Dimitrijevic     09 August 18     Initial Release
--                   V.Mancev
-------------------------------------------------------------------------------
--  PART DESCRIPTION:
--
--  Library:    FLASH
--  Technology: FLASH MEMORY
--  Part:       AT25DF161
--
--   Description: 16 Megabit Serial Flash Memory with 100 MHz SPI Bus Interface
--
-------------------------------------------------------------------------------
--  Known Bugs:
--
-------------------------------------------------------------------------------
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
ENTITY at25df161 IS
    GENERIC (
    ---------------------------------------------------------------------------
    -- TIMING GENERICS:
    ---------------------------------------------------------------------------
        -- tipd delays: interconnect path delays (delay between components)
        --    There should be one for each IN or INOUT pin in the port list
        --    They are given default values of zero delay.
        tipd_SCK            : VitalDelayType01 := VitalZeroDelay01;
        tipd_SI             : VitalDelayType01 := VitalZeroDelay01;
        tipd_SO             : VitalDelayType01 := VitalZeroDelay01;

        tipd_CSNeg          : VitalDelayType01 := VitalZeroDelay01;
        tipd_HOLDNeg        : VitalDelayType01 := VitalZeroDelay01;
        tipd_WPNeg          : VitalDelayType01 := VitalZeroDelay01;

        -- tpd delays: propagation delays (pin-to-pin delay within a component)
        tpd_SCK_SO          : VitalDelayType01Z := UnitDelay01Z; -- tV
        tpd_SCK_SI          : VitalDelayType01Z := UnitDelay01Z; -- tV
        tpd_CSNeg_SO        : VitalDelayType01Z := UnitDelay01Z; -- tDIS
        tpd_HOLDNeg_SO      : VitalDelayType01Z := UnitDelay01Z; -- tHLQZ,tHHQX

        -- tsetup values: setup times
        --   setup time is minimum time before the referent signal edge the
        --   input should be stable
        tsetup_CSNeg_SCK    : VitalDelayType := UnitDelay;  -- tCSLS,tCSHS /
        tsetup_HOLDNeg_SCK  : VitalDelayType := UnitDelay;  -- tHLS,tHHS /
        tsetup_SI_SCK       : VitalDelayType := UnitDelay;  -- tDS /
        tsetup_WPNeg_CSNeg  : VitalDelayType := UnitDelay;  -- tWPS \

        -- thold values: hold times
        --   hold time is minimum time the input should be present stable
        --   after the referent signal edge
        thold_CSNeg_SCK     : VitalDelayType := UnitDelay;  -- tCSLH,tCSHH /
        thold_HOLDNeg_SCK   : VitalDelayType := UnitDelay;  -- tCHLH,tCHHH /
        thold_SI_SCK        : VitalDelayType := UnitDelay;  -- tDH /
        thold_WPNeg_CSNeg   : VitalDelayType := UnitDelay;  -- tWPH /
        thold_SO_SCK        : VitalDelayType := UnitDelay;  -- tOH \

        --tpw values: pulse width
        tpw_SCK_posedge     : VitalDelayType := UnitDelay;--tCLKH
        tpw_SCK_negedge     : VitalDelayType := UnitDelay;--tCLKL
        tpw_CSNeg_posedge   : VitalDelayType := UnitDelay;--tCSH

        -- tperiod min (calculated as 1/max freq)
        tperiod_SCK         : VitalDelayType := UnitDelay;--fCLK=85MHz
        tperiod_SCK_rd      : VitalDelayType := UnitDelay;--fRDLF=50MHz
        tperiod_SCK_rapidS  : VitalDelayType := UnitDelay;--fMAX=100MHz
        tperiod_SCK_dual_rd : VitalDelayType := UnitDelay;--fRDDO=85MHz

        -- tdevice values: values for internal delays
            --timing values that are internal to the model and not associated
            --with any port.

        --Sector Protect Time
        tdevice_SectorProtect        : VitalDelayType := 20 ns;--tSECP
        --Sector Unprotect Time
        tdevice_SectorUnprotect      : VitalDelayType := 20 ns;--tSECUP
        --Sector Lockdown and Freeze Sector Lockdown State Time
        tdevice_Lockdown             : VitalDelayType := 200 us;--tLOCK
        --Chip Select High to Deep Power-Down
        tdevice_EDPD                 : VitalDelayType := 1 us;--tEDPD
        --Chip Select High to Standby Mode
        tdevice_RDPD                 : VitalDelayType := 30 us;--tRDPD
        --Reset Time
        tdevice_Reset                : VitalDelayType := 30 us;--tRST
        --Page Program Time
        tdevice_PageProgram          : VitalDelayType := 1 ms;--tPP
        --Byte Program Time
        tdevice_ByteProgram          : VitalDelayType := 7 us;--tBP
        --Block Erase Time
        tdevice_BlockErase4          : VitalDelayType := 50 ms;--tBLKE
        tdevice_BlockErase32         : VitalDelayType := 250 ms;--tBLKE
        tdevice_SectorErase          : VitalDelayType := 400 ms;--tBLKE
        --Chip Erase Time
        tdevice_ChipErase            : VitalDelayType := 16 sec;--tCHPE
        --Suspend Time
        tdevice_ProgramSuspend       : VitalDelayType := 10 us;--tSUSP
        tdevice_EraseSuspend         : VitalDelayType := 25 us;--tSUSP
        --Resume Time
        tdevice_ProgramResume        : VitalDelayType := 10 us;--tRES
        tdevice_EraseResume          : VitalDelayType := 12 us;--tRES
        --OTP Security Register Program Time
        tdevice_OTPProgram           : VitalDelayType := 200 us;--tOTPP
        --Write Status Register Time
        tdevice_WriteRegister        : VitalDelayType := 200 ns;--tWRSR
        --Power-up Device Delay Before Program or Erase Allowed
        tdevice_PowerUpDelay         : VitalDelayType := 70 us;--tVCSL

    ---------------------------------------------------------------------------
    -- CONTROL GENERICS:
    ---------------------------------------------------------------------------
        -- generic control parameters
        InstancePath        : STRING    := DefaultInstancePath;
        TimingChecksOn      : BOOLEAN   := DefaultTimingChecks;
        MsgOn               : BOOLEAN   := DefaultMsgOn;
        XOn                 : BOOLEAN   := DefaultXon;
        -- memory file to be loaded
        mem_file_name       : STRING    := "at25df161.mem";
        otp_file_name       : STRING    := "at25df161OTP.mem";

        UserPreload         : BOOLEAN   := FALSE; --TRUE;
        LongTimming         : BOOLEAN   := TRUE;

        -- For FMF SDF technology file usage
        TimingModel         : STRING    := DefaultTimingModel
    );
    PORT (
        -- Data Inputs/Outputs
        SI              : INOUT std_ulogic := 'U'; -- serial data input
        SO              : INOUT std_ulogic := 'U'; -- serial data output
        -- Controls
        SCK             : IN    std_ulogic := 'U'; -- serial clock input
        CSNeg           : IN    std_ulogic := 'U'; -- chip select input
        WPNeg           : IN    std_ulogic := 'U'; -- write protect input
        HOLDNeg         : IN    std_ulogic := 'U'  -- hold input
    );

    ATTRIBUTE VITAL_LEVEL0 of at25df161 : ENTITY IS TRUE;
END at25df161;

-------------------------------------------------------------------------------
-- ARCHITECTURE DECLARATION
-------------------------------------------------------------------------------
ARCHITECTURE vhdl_behavioral of at25df161 IS
    ATTRIBUTE VITAL_LEVEL0 OF vhdl_behavioral : ARCHITECTURE IS TRUE;

    ---------------------------------------------------------------------------
    -- CONSTANT AND SIGNAL DECLARATION
    ---------------------------------------------------------------------------
    --Declaration of constants - memory characteristics
        -- The constant declared here are used to enable the creation of models
        -- of memories within a family with a minimum amount of editing 

    CONSTANT PartID        : STRING  := "at25df161";
    CONSTANT MaxData       : NATURAL := 16#FF#;     --255;
    CONSTANT MemSize       : NATURAL := 16#1FFFFF#;
    CONSTANT SecSize       : NATURAL := 16#FFFF#;   --65535
    CONSTANT SecNum        : NATURAL := 31;         -- number of sectors
    CONSTANT BlockSize_4   : NATURAL := 16#FFF#;
    CONSTANT BlockSize_32  : NATURAL := 16#7FFF#;
    CONSTANT BlockSize_64  : NATURAL := 16#FFFF#;
    CONSTANT OTPSize       : NATURAL := 128;
    CONSTANT OTPLoAddr     : NATURAL := 16#0#;
    CONSTANT OTPHiAddr     : NATURAL := 16#7F#;
    CONSTANT PageNum       : NATURAL := 16#1FFF#; --256 Bytes 
    CONSTANT HiAddrBit     : NATURAL := 23;
    CONSTANT AddrRANGE     : NATURAL := 16#1FFFFF#;
    CONSTANT BYTE          : NATURAL := 8;

    --Manufacturer Identification
    CONSTANT Manuf_ID      : NATURAL := 16#1F#;
    --Device ID
    CONSTANT Jedec_ID      : NATURAL := 16#1F#; -- 1st byte of Device ID
    CONSTANT DeviceID_P1   : NATURAL := 16#46#; -- 2nd byte of Device ID
    CONSTANT DeviceID_P2   : NATURAL := 16#02#; -- 3rd byte of Device ID
    --Extended Device Information String Length
    CONSTANT ExtendedBytes : NATURAL := 16#00#; -- 4th byte of Device ID

    -- Declaration of signals that will hold the delayed values of ports
    SIGNAL SI_ipd          : std_ulogic := 'U';
    SIGNAL SO_ipd          : std_ulogic := 'U';
    SIGNAL SCK_ipd         : std_ulogic := 'U';
    SIGNAL CSNeg_ipd       : std_ulogic := 'U';
    SIGNAL HOLDNeg_ipd     : std_ulogic := 'U';
    SIGNAL WPNeg_ipd       : std_ulogic := 'U';

    -- internal delays
    SIGNAL SectorProtect_in           : std_ulogic := '0';
    SIGNAL SectorProtect_out          : std_ulogic := '0';
    SIGNAL SectorUnprotect_in         : std_ulogic := '0';
    SIGNAL SectorUnprotect_out        : std_ulogic := '0';
    SIGNAL Lockdown_in                : std_ulogic := '0';
    SIGNAL Lockdown_out               : std_ulogic := '0';
    SIGNAL EDPD_in                    : std_ulogic := '0';
    SIGNAL EDPD_out                   : std_ulogic := '0';
    SIGNAL RDPD_in                    : std_ulogic := '0';
    SIGNAL RDPD_out                   : std_ulogic := '0';
    SIGNAL Reset_in                   : std_ulogic := '0';
    SIGNAL Reset_out                  : std_ulogic := '0';
    SIGNAL PageProgram_in             : std_ulogic := '0';
    SIGNAL PageProgram_out            : std_ulogic := '0';
    SIGNAL ByteProgram_in             : std_ulogic := '0';
    SIGNAL ByteProgram_out            : std_ulogic := '0';
    SIGNAL BlockErase4_in             : std_ulogic := '0';
    SIGNAL BlockErase4_out            : std_ulogic := '0';
    SIGNAL BlockErase32_in            : std_ulogic := '0';
    SIGNAL BlockErase32_out           : std_ulogic := '0';
    SIGNAL SectorErase_in             : std_ulogic := '0';
    SIGNAL SectorErase_out            : std_ulogic := '0';
    SIGNAL ChipErase_in               : std_ulogic := '0';
    SIGNAL ChipErase_out              : std_ulogic := '0';
    SIGNAL ProgramSuspend_in          : std_ulogic := '0';
    SIGNAL ProgramSuspend_out         : std_ulogic := '0';
    SIGNAL EraseSuspend_in            : std_ulogic := '0';
    SIGNAL EraseSuspend_out           : std_ulogic := '0';
    SIGNAL ProgramResume_in           : std_ulogic := '0';
    SIGNAL ProgramResume_out          : std_ulogic := '0';
    SIGNAL EraseResume_in             : std_ulogic := '0';
    SIGNAL EraseResume_out            : std_ulogic := '0';
    SIGNAL OTPProgram_in              : std_ulogic := '0';
    SIGNAL OTPProgram_out             : std_ulogic := '0';
    SIGNAL WriteRegister_in           : std_ulogic := '0';
    SIGNAL WriteRegister_out          : std_ulogic := '0';
    SIGNAL PowerUpDelay_in            : std_ulogic := '0';
    SIGNAL PowerUpDelay_out           : std_ulogic := '0';

BEGIN

    ---------------------------------------------------------------------------
    -- Internal Delays
    ---------------------------------------------------------------------------
    -- Artificial VITAL primitives to incorporate internal delays  
    -- Because a tdevice generics is used, there must be a VITAL_primitives
    -- assotiated with them 
    SectorProtect    : VitalBuf(SectorProtect_out,    SectorProtect_in,
                               (tdevice_SectorProtect,   UnitDelay));
    SectorUnprotect  : VitalBuf(SectorUnprotect_out,  SectorUnprotect_in,
                               (tdevice_SectorUnprotect, UnitDelay));
    Lockdown         : VitalBuf(Lockdown_out,         Lockdown_in,
                               (tdevice_Lockdown,        UnitDelay));
    EDPD             : VitalBuf(EDPD_out,             EDPD_in,
                               (tdevice_EDPD,            UnitDelay));
    RDPD             : VitalBuf(RDPD_out,             RDPD_in,
                               (tdevice_RDPD,            UnitDelay));
    Reset            : VitalBuf(Reset_out,            Reset_in,
                               (tdevice_Reset,           UnitDelay));
    PageProgram      : VitalBuf(PageProgram_out,      PageProgram_in,
                               (tdevice_PageProgram,     UnitDelay));
    ByteProgram      : VitalBuf(ByteProgram_out,      ByteProgram_in,
                               (tdevice_ByteProgram,     UnitDelay));
    BlockErase4      : VitalBuf(BlockErase4_out,      BlockErase4_in,
                               (tdevice_BlockErase4,     UnitDelay));
    BlockErase32     : VitalBuf(BlockErase32_out,     BlockErase32_in,
                               (tdevice_BlockErase32,    UnitDelay));
    SectorErase      : VitalBuf(SectorErase_out,      SectorErase_in,
                               (tdevice_SectorErase,     UnitDelay));
    ChipErase        : VitalBuf(ChipErase_out,        ChipErase_in,
                               (tdevice_ChipErase,       UnitDelay));
    ProgramSuspend   : VitalBuf(ProgramSuspend_out,   ProgramSuspend_in,
                               (tdevice_ProgramSuspend,  UnitDelay));
    EraseSuspend     : VitalBuf(EraseSuspend_out,     EraseSuspend_in,
                               (tdevice_EraseSuspend,    UnitDelay));
    ProgramResume    : VitalBuf(ProgramResume_out,    ProgramResume_in,
                               (tdevice_ProgramResume,   UnitDelay));
    EraseResume      : VitalBuf(EraseResume_out,      EraseResume_in,
                               (tdevice_EraseResume,     UnitDelay));
    OTPProgram       : VitalBuf(OTPProgram_out,       OTPProgram_in,
                               (tdevice_OTPProgram,      UnitDelay));
    WriteRegister    : VitalBuf(WriteRegister_out,    WriteRegister_in,
                               (tdevice_WriteRegister,   UnitDelay));
    PowerUpDelay     : VitalBuf(PowerUpDelay_out,     PowerUpDelay_in,
                               (tdevice_PowerUpDelay,    UnitDelay));

    ---------------------------------------------------------------------------
    -- Wire Delays
    ---------------------------------------------------------------------------
    WireDelay : BLOCK
    BEGIN

        w_1 : VitalWireDelay (SI_ipd,      SI,      tipd_SI);
        w_2 : VitalWireDelay (SO_ipd,      SO,      tipd_SO);
        w_3 : VitalWireDelay (SCK_ipd,     SCK,     tipd_SCK);
        w_4 : VitalWireDelay (CSNeg_ipd,   CSNeg,   tipd_CSNeg);
        w_5 : VitalWireDelay (HOLDNeg_ipd, HOLDNeg, tipd_HOLDNeg);
        w_6 : VitalWireDelay (WPNeg_ipd,   WPNeg,   tipd_WPNeg);

    END BLOCK;

    ---------------------------------------------------------------------------
    -- Main Behavior Block
    ---------------------------------------------------------------------------
    Behavior: BLOCK

        PORT (
            SIIn             : IN          std_ulogic := 'U';
            SIOut            : OUT         std_ulogic := 'U';
            SOIn             : IN          std_logic  := 'U';
            SOOut            : OUT         std_logic  := 'U';
            SCK              : IN          std_ulogic := 'U';
            CSNeg            : IN          std_ulogic := 'U';
            HOLDNeg          : IN          std_ulogic := 'U';
            WPNeg            : IN          std_ulogic := 'U'
        );

        PORT MAP (
            SIIn       => SI_ipd,
            SIOut      => SI,
            SOIn       => SO_ipd,
            SOOut      => SO,
            SCK        => SCK_ipd,
            CSNeg      => CSNeg_ipd,
            HOLDNeg    => HOLDNeg_ipd,
            WPNeg      => WPNeg_ipd
        );

        -- State Machine : State_Type
        TYPE state_type IS (IDLE,
                            WRITE_SR1,        --Write Status Register Byte1
                            WRITE_SR2,        --Write Status Register Byte2
                            PAGE_PG,          --Byte/Page Programming
                            OTP_PG,           --Program OTP Security Register
                            PG_SUSP,          --Program Suspend
                            BLK_ERS_4,        --Block Erase (4KByte Block)
                            BLK_ERS_32,       --Block Erase (32KByte Block)
                            SEC_ERS,          --Block Erase (64KByte Block)
                            CHIP_ERS,         --Chip Erase
                            ERS_SUSP,         --Erase Suspend
                            ERS_SUSP_PG,      --Programming During Ers Susp
                            ERS_SUSP_PG_SUSP, --Prog Susp During Ers Susp
                            PROT_SEC,         --Protect Sector
                            UNPROT_SEC,       --Unprotect Sector
                            SEC_LOCKDOWN,     --Sector Lockdown
                            FREEZE_SEC_LD,    --Freeze Sector Lockdown State
                            DP_DOWN,          --Deep Power-Down
                            RST               --Reset
                            );

        -- Instruction Type
        TYPE instruction_type IS (NONE,
                               WREN,          --write enable
                               WRDI,          --write disable
                               WRR1,          --write status reg. byte1
                               WRR2,          --write status reg. byte2
                               RDSR,          --read status register
                               READ,          --up to 50 MHz
                               READ_ID,       --Manufacturer ID
                               FAST_READ,     --up to 100 MHz
                               DUAL_READ,
                               MED_READ,      --up to 85 MHz
                               SE,            --block erase_64
                               B32E,          --block erase_32
                               B4E,           --block erase_4
                               CH_ERS,        --chip erase
                               PESP,          --Program/Erase suspend
                               PP,            --Byte/Page Programming
                               DUAL_PP,       --Dual Byte/Page Programming
                               DP,            --Deep Power-Down
                               SELD,          --Sector Lockdown
                               FR_SELD,       --Freeze Sector Lockdown
                               RD_SELD_REG,   --Read Sector Lockdown Register
                               PROT_SE,       --Protect Sector
                               UNPROT_SE,     --Unprotect Sector
                               RD_SE_PROT_REG,--Read Sec Prot Register
                               RESET,         --Reset
                               PERES,         --Program/Erase Resume
                               RES,           --resume from Deep Power-Down
                               OTPR,          --read OTP
                               OTPP           --program OTP
                                  );

        TYPE WByteType IS ARRAY (0 TO 255) OF INTEGER RANGE -1 TO MaxData;

        --Flash Memory Array
        TYPE MemArray IS ARRAY (0 TO AddrRANGE) OF INTEGER
                                                      RANGE -1 TO MaxData;
        --OTP Memory Array
        TYPE OTPArray IS ARRAY (OTPLoAddr TO OTPHiAddr) OF INTEGER
                                                      RANGE -1 TO MaxData;
        -----------------------------------------------------------------------
        --  memory declaration
        -----------------------------------------------------------------------
        SHARED VARIABLE Mem          : MemArray  := (OTHERS => MaxData);
        -- OTP Sector
        SHARED VARIABLE OTPMem       : OTPArray  := (OTHERS => MaxData);
        --Internal buffer
        SIGNAL WByte                 : WByteType := (OTHERS => 0);
        SIGNAL WOTPByte              : OTPArray  := (OTHERS => 0);

        -- states
        SIGNAL current_state         : state_type;
        SIGNAL next_state            : state_type;

        SIGNAL Instruct              : instruction_type;
        --zero delay signal
        SIGNAL SOut_zd               : std_logic := 'Z';
        SIGNAL SIOut_zd              : std_logic := 'Z';
        --HOLD delay on output data
        SIGNAL SOut_z                : std_logic := 'Z';
        SIGNAL SIOut_z               : std_logic := 'Z';
        -- powerup
        SIGNAL PoweredUp             : std_logic := '0';

        SHARED VARIABLE Status_reg   : std_logic_vector(15 downto 0)
                                                := (others => '0');

        SIGNAL Status_reg_in         : std_logic_vector(15 downto 0)
                                                := (others => '0');

        --Status Register Byte 1
        -- Ready/Busy Status
        ALIAS RDYBSY1 :std_logic IS Status_reg(8);
        -- Write Enable Latch Bit
        ALIAS WEL     :std_logic IS Status_reg(9);
        -- Software Protection Status
        ALIAS SWP2    :std_logic IS Status_reg(10);
        ALIAS SWP3    :std_logic IS Status_reg(11);
        --Write Protect (WPNeg) Pin Status
        ALIAS WPP     :std_logic IS Status_reg(12);
        -- Erase/Program Error bit
        ALIAS EPE     :std_logic IS Status_reg(13);
        -- Sector Protection Register Locked
        ALIAS SPRL    :std_logic IS Status_reg(15);

        --Status Register Byte 2
        -- Ready/Busy Status
        ALIAS RDYBSY2 :std_logic IS Status_reg(0);
        -- Erase Suspend Status
        ALIAS ES      :std_logic IS Status_reg(1);
        -- Program Suspend Status
        ALIAS PS      :std_logic IS Status_reg(2);
        -- Sector Lockdown Enabled
        ALIAS SLE     :std_logic IS Status_reg(3);
        -- Reset Enable Pin
        ALIAS RSTE     :std_logic IS Status_reg(4);

        -- The Sector Protection Registers
        --   Upon device power-up, each Sector Protection Register will default
        --   to the logical '1' state indicating that all sectors are protected 
        --   and cannot be programmed or erased.
        SHARED VARIABLE SEC_PROT     :std_logic_vector(SecNum downto 0):=
                                                      (OTHERS => '1');

        --The Sector Lockdown Registers
        --Each 64-Kbyte physical sector has a corresponding single-bit Sector 
        --Lockdown Register that is used to control the lockdown status of that
        --sector.Once a sector is locked down,it can never be erased or 
        --programmed again, and it can never be unlocked from the locked-down 
        --state.
        SHARED VARIABLE SEC_LOCK_REG :std_logic_vector(SecNum downto 0):=
                                                      (OTHERS => '0');

        --Command Register
        SIGNAL write              : std_logic := '0';
        SIGNAL read_out           : std_logic := '0';

        SIGNAL rd                 : boolean   := false;--up to 50 MHz
        SIGNAL medium_rd          : boolean   := false;--up to 85 MHz
        SIGNAL fast_rd            : boolean   := true; --up to 100 MHz
        SIGNAL dual               : boolean   := false;

        SHARED VARIABLE read_cnt  : NATURAL := 0;
        
        SIGNAL change_addr        : std_logic := '0';
        SHARED VARIABLE hold_mode : boolean := false;

        --FSM control signals

        SIGNAL WDONE              : std_logic := '1'; --Write Done
        SIGNAL WSTART             : std_logic := '0'; --Start Write

        SIGNAL PSTART             : std_logic := '0'; --Start Program
        SIGNAL PDONE              : std_logic := '1'; --Program Done
        SIGNAL PGSUSP             : std_logic := '0'; --Suspend Program
        SIGNAL PGRES              : std_logic := '0'; --Resume Program

        SIGNAL OTPSTART           : std_logic := '0'; --Start OTP Programming
        SIGNAL OTPDONE            : std_logic := '1'; --OTP Programming Done
        SIGNAL OTP_BLOCKED        : std_logic := '0'; --OTP Programming Blocked

        SIGNAL ESTART             : std_logic := '0'; --Start Erase
        SIGNAL EDONE              : std_logic := '1'; --Erase Done
        SIGNAL ESUSP              : std_logic := '0'; --Suspend Erase
        SIGNAL ERES               : std_logic := '0'; --Resume Erase
       
        SIGNAL FROZEN             : std_logic := '0'; --Freeze Sec Lockdown 

        --Sector Address
        SIGNAL SA                 : NATURAL RANGE 0 TO SecNum := 0;
        SHARED VARIABLE sect      : NATURAL RANGE 0 TO SecNum := 0;
        --Address
        SIGNAL Address            : NATURAL RANGE 0 TO AddrRANGE := 0;
        SIGNAL SectorSuspend      : NATURAL RANGE 0 TO SecNum := 0;

        SIGNAL Byte_number        : INTEGER RANGE 0 TO 300    := 0;

        -- timing check violation
        SIGNAL Viol               : X01 := '0';

        PROCEDURE ADDRHILO_SEC(
            VARIABLE   AddrLOW  : INOUT NATURAL RANGE 0 to ADDRRange;
            VARIABLE   AddrHIGH : INOUT NATURAL RANGE 0 to ADDRRange;
            VARIABLE   Addr     : NATURAL) IS
            VARIABLE   sector   : NATURAL RANGE 0 TO SecNum;
        BEGIN
            sector   := Addr/16#10000#;
            AddrLOW  := sector*16#10000#;
            AddrHIGH := sector*16#10000# + 16#00FFFF#;
        END ADDRHILO_SEC;

        PROCEDURE ADDRHILO_PP(
            VARIABLE   AddrLOW  : INOUT NATURAL RANGE 0 to ADDRRange;
            VARIABLE   AddrHIGH : INOUT NATURAL RANGE 0 to ADDRRange;
            VARIABLE   Addr     : NATURAL) IS
            VARIABLE   page     : NATURAL RANGE 0 TO PageNum;
        BEGIN
            page     := Addr/16#100#;
            AddrLOW  := Page*16#100#;
            AddrHIGH := Page*16#100# + 16#FF#;
        END AddrHILO_PP;

        PROCEDURE ADDRHILO_BS4(
            VARIABLE   AddrLOW  : INOUT NATURAL RANGE 0 to ADDRRange;
            VARIABLE   AddrHIGH : INOUT NATURAL RANGE 0 to ADDRRange;
            VARIABLE   Addr     : NATURAL) IS
            VARIABLE   sector   : NATURAL RANGE 0 TO 512;
        BEGIN
            sector   := Addr/16#1000#;
            AddrLOW  := sector*16#1000#;
            AddrHIGH := sector*16#1000# + 16#000FFF#;
        END ADDRHILO_BS4;

        PROCEDURE ADDRHILO_BS32(
            VARIABLE   AddrLOW  : INOUT NATURAL RANGE 0 to ADDRRange;
            VARIABLE   AddrHIGH : INOUT NATURAL RANGE 0 to ADDRRange;
            VARIABLE   Addr     : NATURAL) IS
            VARIABLE   sector   : NATURAL RANGE 0 TO 64;
        BEGIN
            sector   := Addr/16#8000#;
            AddrLOW  := sector*16#8000#;
            AddrHIGH := sector*16#8000# + 16#007FFF#;
        END ADDRHILO_BS32;

    BEGIN

    ---------------------------------------------------------------------------
    --Power Up time
    ---------------------------------------------------------------------------
    PoweredUp <= '1' AFTER tdevice_PowerUpDelay;

    ---------------------------------------------------------------------------
    -- VITAL Timing Checks Procedures
    ---------------------------------------------------------------------------
    VITALTimingCheck: PROCESS(SIIn, SOIn, SCK_ipd, CSNeg_ipd, HOLDNeg_ipd,
                              WPNeg_ipd)
        -- Timing Check Variables
        --Setup/Hold Check Variables 
        VARIABLE Tviol_SI_SCK          : X01 := '0';
        VARIABLE TD_SI_SCK             : VitalTimingDataType;

        VARIABLE Tviol_HOLDNeg_SCK_L   : X01 := '0';
        VARIABLE TD_HOLDNeg_SCK_L      : VitalTimingDataType;    --ok

        VARIABLE Tviol_HOLDNeg_SCK_H   : X01 := '0';
        VARIABLE TD_HOLDNeg_SCK_H      : VitalTimingDataType;    --ok

        VARIABLE Tviol_CSNeg_SCK_L     : X01 := '0';
        VARIABLE TD_CSNeg_SCK_L        : VitalTimingDataType;    --ok

        VARIABLE Tviol_CSNeg_SCK_H     : X01 := '0';
        VARIABLE TD_CSNeg_SCK_H        : VitalTimingDataType;    --ok

        VARIABLE Tviol_WPNeg_CSNeg     : X01 := '0';
        VARIABLE TD_WPNeg_CSNeg        : VitalTimingDataType;    --ok

        VARIABLE Tviol_SO_SCK          : X01 := '0';
        VARIABLE TD_SO_SCK             : VitalTimingDataType;    --ok

        --Pulse Width and Period Check Variables 
        VARIABLE Pviol_SCK             : X01 := '0';
        VARIABLE PD_SCK                : VitalPeriodDataType := 
                                                          VitalPeriodDataInit;

        VARIABLE Pviol_CSNeg           : X01 := '0';
        VARIABLE PD_CSNeg              : VitalPeriodDataType := 
                                                          VitalPeriodDataInit;

        --Functionality Results Variables
        --(used to OR all individual violations) 
        VARIABLE Violation        : X01 := '0';

    BEGIN
    ---------------------------------------------------------------------------
    -- Timing Check Section
    ---------------------------------------------------------------------------
    IF (TimingChecksOn) THEN

        -- Setup/Hold Check between SI and SCK
        VitalSetupHoldCheck (
            TestSignal      => SIIn,
            TestSignalName  => "SI",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK,
            SetupLow        => tsetup_SI_SCK,
            HoldHigh        => thold_SI_SCK,
            HoldLow         => thold_SI_SCK,
            CheckEnabled    => SIOut_zd /= SIIn,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_SI_SCK,
            Violation       => Tviol_SI_SCK
        );

        -- Setup/Hold Check between HOLDNeg and SCK /
        VitalSetupHoldCheck (
            TestSignal      => HOLDNeg_ipd,
            TestSignalName  => "HOLDNeg",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupLow        => tsetup_HOLDNeg_SCK,
            HoldLow         => thold_HOLDNeg_SCK,
            CheckEnabled    => true,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_HOLDNeg_SCK_L,
            Violation       => Tviol_HOLDNeg_SCK_L
        );

        VitalSetupHoldCheck (
            TestSignal      => HOLDNeg_ipd,
            TestSignalName  => "HOLDNeg",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_HOLDNeg_SCK,
            HoldHigh        => thold_HOLDNeg_SCK,
            CheckEnabled    => true,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_HOLDNeg_SCK_H,
            Violation       => Tviol_HOLDNeg_SCK_H
        );

        -- Setup/Hold Check between CSNeg and SCK
        VitalSetupHoldCheck (
            TestSignal      => CSNeg_ipd,
            TestSignalName  => "CSNeg",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupLow        => tsetup_CSNeg_SCK,
            HoldLow         => thold_CSNeg_SCK,
            CheckEnabled    => true,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_CSNeg_SCK_L,
            Violation       => Tviol_CSNeg_SCK_L
        );

        VitalSetupHoldCheck (
            TestSignal      => CSNeg_ipd,
            TestSignalName  => "CSNeg",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_CSNeg_SCK,
            HoldHigh        => thold_CSNeg_SCK,
            CheckEnabled    => true,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_CSNeg_SCK_H,
            Violation       => Tviol_CSNeg_SCK_H
        );

        -- Setup Check between WPNeg and CSNeg 
        VitalSetupHoldCheck (
            TestSignal      => WPNeg_ipd,
            TestSignalName  => "WNeg",
            RefSignal       => CSNeg_ipd,
            RefSignalName   => "CSNeg",
            SetupHigh       => tsetup_WPNeg_CSNeg,
            CheckEnabled    => true,
            RefTransition   => '\',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_WPNeg_CSNeg,
            Violation       => Tviol_WPNeg_CSNeg
        );

        -- Hold Check between WPNeg and CSNeg 
        VitalSetupHoldCheck (
            TestSignal      => WPNeg_ipd,
            TestSignalName  => "WNeg",
            RefSignal       => CSNeg_ipd,
            RefSignalName   => "CSNeg",
            HoldHigh        => thold_WPNeg_CSNeg,
            CheckEnabled    => true,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_WPNeg_CSNeg,
            Violation       => Tviol_WPNeg_CSNeg
        );

        -- Hold Check between SO and SCK 
        VitalSetupHoldCheck (
            TestSignal      => SOIn,
            TestSignalName  => "SO",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            HoldHigh        => thold_SO_SCK,
            CheckEnabled    => SOut_z /= SOIn,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_SO_SCK,
            Violation       => Tviol_SO_SCK
        );

        -- PulseWidth Check for SCK
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            PulseWidthLow   =>  tpw_SCK_negedge,
            PulseWidthHigh  =>  tpw_SCK_posedge,
            PeriodData      =>  PD_SCK,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  TRUE
        );

        -- PulseWidth Check for CSNeg
        VitalPeriodPulseCheck (
            TestSignal      =>  CSNeg_ipd,
            TestSignalName  =>  "CSNeg",
            PulseWidthHigh  =>  tpw_CSNeg_posedge,
            PeriodData      =>  PD_CSNeg,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_CSNeg,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  TRUE);

        Violation := Tviol_SI_SCK            OR
                     Tviol_HOLDNeg_SCK_L     OR
                     Tviol_HOLDNeg_SCK_H     OR
                     Tviol_CSNeg_SCK_L       OR
                     Tviol_CSNeg_SCK_H       OR
                     Tviol_WPNeg_CSNeg       OR
                     Tviol_SO_SCK            OR
                     Pviol_SCK               OR
                     Pviol_CSNeg;

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
    StateTransition : PROCESS(next_state, PoweredUp)

    BEGIN
        IF PoweredUp = '1' THEN
            current_state <= next_state;
        END IF;
    END PROCESS StateTransition;
    ---------------------------------------------------------------------------
    --  Write cycle decode
    ---------------------------------------------------------------------------

    BusCycleDecode : PROCESS(SCK_ipd, CSNeg_ipd, HOLDNeg_ipd, SIIn )

        TYPE bus_cycle_type IS (STAND_BY,
                                OPCODE_BYTE,
                                ADDRESS_BYTES,
                                DUMMY_BYTES,
                                DATA_BYTES
                                );

        TYPE dual_data_type IS ARRAY (0 TO 1023) OF INTEGER RANGE 0 TO 3;

        VARIABLE bus_cycle_state    : bus_cycle_type;

        VARIABLE data_cnt        : INTEGER RANGE 0 TO 4096 := 0;
        VARIABLE addr_cnt        : NATURAL := 0;
        VARIABLE opcode_cnt      : NATURAL := 0;
        VARIABLE dummy_cnt       : NATURAL := 0;
        VARIABLE bit_cnt         : NATURAL := 0;
        VARIABLE Data_in         : std_logic_vector(2047 downto 0)
                                                    := (others => '0');

        VARIABLE dual_data_in    : dual_data_type;
        VARIABLE dual_nybble     : std_logic_vector(1 downto 0);
        VARIABLE dual_slv        : std_logic_vector(1 downto 0);
        VARIABLE opcode          : std_logic_vector(7 downto 0);
        VARIABLE opcode_in       : std_logic_vector(7 downto 0);
        VARIABLE Byte_slv        : std_logic_vector(7 downto 0);
        VARIABLE addr_bytes      : std_logic_vector(HiAddrBit downto 0);
        VARIABLE Address_in      : std_logic_vector(23 downto 0);  

    BEGIN

    IF rising_edge(CSNeg_ipd) THEN
        bus_cycle_state := STAND_BY;
        IF Instruct = PP OR Instruct = DUAL_PP OR Instruct = OTPP THEN
            write <= '0';
        END IF;
    ELSE
        CASE bus_cycle_state IS
            WHEN STAND_BY =>
                IF falling_edge(CSNeg_ipd) THEN
                    Instruct <= NONE;
                    write    <= '1';
                    opcode_cnt  := 0;
                    addr_cnt    := 0;
                    data_cnt    := 0;
                    dummy_cnt   := 0;
                    bus_cycle_state := OPCODE_BYTE;
                END IF;

            WHEN OPCODE_BYTE =>
            --If rising edge SCK then command, address or data present on SI 
            --pin are latched.
            --If HOLDNeg is LOW, transition on the SCK pin and data on SI pin
            -- will  be ignored.
                IF rising_edge(SCK_ipd) AND HOLDNeg_ipd = '1' THEN
                    opcode_in(opcode_cnt) := SIIn;
                    opcode_cnt := opcode_cnt + 1;
                    IF opcode_cnt = BYTE THEN
                        --MSB first
                        FOR I IN 7 DOWNTO 0 LOOP
                            opcode(i) := opcode_in(7-i);
                        END LOOP;

                        CASE opcode IS
                            WHEN "00000110" => --06h
                                Instruct <= WREN;
                                bus_cycle_state := DATA_BYTES;
                            WHEN "00000100" => --04h
                                Instruct <= WRDI;
                                bus_cycle_state := DATA_BYTES;
                            WHEN "00000001" => --01h
                                Instruct <= WRR1;
                                bus_cycle_state := DATA_BYTES;
                            WHEN "00110001" => --31h
                                Instruct <= WRR2;
                                bus_cycle_state := DATA_BYTES;
                            WHEN "00000101" => --05h
                                Instruct <= RDSR;
                                bus_cycle_state := DATA_BYTES;
                            WHEN "00000011" => --03h
                                Instruct <= READ;
                                bus_cycle_state := ADDRESS_BYTES;
                            WHEN "10011111" => --9Fh
                                Instruct <= READ_ID;
                                bus_cycle_state := DATA_BYTES;
                            WHEN "00011011" => --1Bh
                                Instruct <= FAST_READ;
                                    bus_cycle_state := ADDRESS_BYTES;
                             WHEN "00111011" => --3Bh
                                Instruct <= DUAL_READ;
                                    bus_cycle_state := ADDRESS_BYTES;
                            WHEN "00001011" => --0Bh
                                Instruct <= MED_READ;
                                    bus_cycle_state := ADDRESS_BYTES;
                            WHEN "11011000"  => --D8h
                                Instruct <= SE;
                                bus_cycle_state := ADDRESS_BYTES;
                            WHEN "01010010"  => --52h
                                Instruct <= B32E;
                                bus_cycle_state := ADDRESS_BYTES;
                            WHEN "00100000"  => --20h
                                Instruct <= B4E;
                                bus_cycle_state := ADDRESS_BYTES;
                            WHEN "11000111"  => --C7h or 60h
                                Instruct <= CH_ERS;
                                bus_cycle_state := DATA_BYTES;
                            WHEN "10110000"  => --B0h
                                Instruct <= PESP;
                                bus_cycle_state := DATA_BYTES;
                            WHEN "00000010" => --02h
                                Instruct <= PP;
                                bus_cycle_state := ADDRESS_BYTES;
                            WHEN "10100010" => --A2h
                                Instruct <= DUAL_PP;
                                bus_cycle_state := ADDRESS_BYTES;
                            WHEN "10111001" => --B9h
                                Instruct <= DP;
                                bus_cycle_state := DATA_BYTES;
                            WHEN "00110011" => --33h
                                Instruct <= SELD;
                                bus_cycle_state := ADDRESS_BYTES;
                            WHEN "00110100" => --34h
                                Instruct <= FR_SELD;
                                bus_cycle_state := ADDRESS_BYTES;
                            WHEN "00110101" => --35h
                                Instruct <= RD_SELD_REG;
                                bus_cycle_state := ADDRESS_BYTES;
                            WHEN "00110110" => --36h
                                Instruct <= PROT_SE;
                                bus_cycle_state := ADDRESS_BYTES;
                            WHEN "00111001" => --39h
                                Instruct <= UNPROT_SE;
                                bus_cycle_state := ADDRESS_BYTES;
                            WHEN "00111100" => --3Ch
                                Instruct <= RD_SE_PROT_REG;
                                bus_cycle_state := ADDRESS_BYTES;
                            WHEN "11110000" => --F0h
                                Instruct <= RESET;
                                bus_cycle_state := DATA_BYTES;
                            WHEN "11010000" => --DOh
                                Instruct <= PERES;
                                bus_cycle_state := DATA_BYTES;
                            WHEN "10101011" => --ABh
                                Instruct <= RES;
                                bus_cycle_state := DATA_BYTES;
                            WHEN "01110111" => --77h
                                Instruct <= OTPR;
                                bus_cycle_state := ADDRESS_BYTES;
                            WHEN "10011011" => --9Bh
                                Instruct <= OTPP;
                                bus_cycle_state := ADDRESS_BYTES;
                            WHEN others =>
                                null;
                        END CASE;
                    END IF;
                END IF;

                WHEN ADDRESS_BYTES =>
                    IF rising_edge(SCK_ipd) THEN
                        IF Instruct=FR_SELD THEN
                            Address_in(23 downto 0) := 
                                                    "010101011010101001000000";
                            bus_cycle_state := DATA_BYTES;
                        ELSE
                            Address_in(addr_cnt) := SIIn;
                            addr_cnt := addr_cnt + 1;
                            IF addr_cnt = 3*BYTE THEN
                                FOR I IN 23 DOWNTO 23-HiAddrBit LOOP
                                    addr_bytes(23-i) := Address_in(i);
                                END LOOP;
                                IF Instruct=OTPP THEN
                                    Address <= to_nat(addr_bytes(5 DOWNTO 0));
                                ELSE
                                    Address <= to_nat(addr_bytes);
                                END IF;
                                change_addr <= '1','0' AFTER 1 ns;
                                IF ((Instruct=FAST_READ OR Instruct=MED_READ 
                                     OR Instruct=OTPR OR Instruct=DUAL_READ) 
                                     AND HOLDNeg_ipd = '1') THEN
                                    bus_cycle_state := DUMMY_BYTES;
                                ELSE 
                                    bus_cycle_state := DATA_BYTES;
                                END IF;
                            END IF;
                        END IF;
                    END IF;

                WHEN DUMMY_BYTES =>
                    IF rising_edge(SCK_ipd) THEN
                        IF HOLDNeg_ipd = '1' THEN
                            dummy_cnt := dummy_cnt + 1;
                            IF dummy_cnt = BYTE THEN
                                IF Instruct=MED_READ OR Instruct=DUAL_READ THEN
                                    bus_cycle_state := DATA_BYTES;
                                END IF;
                            ELSIF dummy_cnt = 2*BYTE THEN
                                IF Instruct=FAST_READ OR Instruct=OTPR THEN
                                    bus_cycle_state := DATA_BYTES;
                                END IF;
                            END IF;
                        END IF;
                    END IF;

                WHEN DATA_BYTES =>
                    IF falling_edge(SCK_ipd) AND CSNeg_ipd = '0' THEN
                        IF ((Instruct = READ     OR Instruct = FAST_READ
                          OR Instruct = MED_READ OR Instruct = DUAL_READ
                          OR Instruct = READ_ID  OR Instruct = OTPR
                          OR Instruct = RDSR     OR Instruct = RD_SELD_REG
                          OR Instruct = RD_SE_PROT_REG) 
                          AND HOLDNeg_ipd = '1') THEN
                            read_out <= '1', '0' AFTER 1 ns;
                        END IF;
                    END IF;
                    IF rising_edge(SCK_ipd) THEN
                        IF Instruct = DUAL_PP THEN
                            dual_nybble :=  SOIn & SIIn;
                            IF data_cnt > 1023 THEN
                            --In case of dual mode and DPP,
                            --if more than 256 bytes are sent to the device
                                IF bit_cnt = 0 THEN
                                    FOR I IN 0 TO 1019 LOOP
                                        dual_data_in(i) := dual_data_in(i+4);
                                    END LOOP;
                                END IF;
                                dual_data_in(1020 + bit_cnt) :=
                                                           to_nat(dual_nybble);
                                bit_cnt := bit_cnt + 1;
                                IF bit_cnt = 4 THEN
                                    bit_cnt := 0;
                                END IF;
                                data_cnt := data_cnt + 1;
                            ELSE
                                IF dual_nybble /= "ZZ" THEN
                                    dual_data_in(data_cnt) :=
                                    to_nat(dual_nybble);
                                END IF;
                                data_cnt := data_cnt + 1;
                                IF (data_cnt mod 4) = 0 THEN
                                    Byte_number <= data_cnt/4 -1;
                                END IF;
                            END IF;
                        ELSIF HOLDNeg_ipd = '1' THEN
                            IF data_cnt > 2047 THEN
                            --In case of serial mode and PP,
                            --if more than 256 bytes are sent to the device
                                IF bit_cnt = 0 THEN
                                    FOR I IN 0 TO (255*BYTE - 1) LOOP
                                        Data_in(i) := Data_in(i+8);
                                    END LOOP;
                                END IF;
                                Data_in(2040 + bit_cnt) := SIIn;
                                bit_cnt := bit_cnt + 1;
                                IF bit_cnt = 8 THEN
                                    bit_cnt := 0;
                                END IF;
                                data_cnt := data_cnt + 1;
                            ELSE
                                Data_in(data_cnt) := SIIn;
                                data_cnt := data_cnt + 1;
                                IF Instruct = PP AND (data_cnt mod 8) = 0 THEN
                                    Byte_number <= data_cnt/8 ;
                                END IF;
                                bit_cnt := 0;
                            END IF;
                        END IF;
                    END IF;

                        CASE Instruct IS
                            WHEN WREN | WRDI | DP | B4E | B32E | SE | CH_ERS |
                                 PROT_SE | UNPROT_SE | PERES | RES  =>
                                IF HOLDNeg_ipd = '1' THEN
                                    IF (data_cnt mod 8) = 0 THEN
                                        write <= '0';
                                    END IF;
                                END IF;

                            WHEN RESET | SELD | FR_SELD =>
                                IF HOLDNeg_ipd = '1' THEN
                                    IF Instruct = SELD THEN
                                        IF data_cnt = 7 THEN
                                            write <= '0';
                                        END IF;
                                    ELSIF Instruct = FR_SELD THEN
                                        IF data_cnt = 30 THEN
                                            write <= '0';
                                        END IF;
                                    ELSIF ((data_cnt mod 8) = 0
                                       AND data_cnt > 0) THEN
                                        IF data_cnt = 8 AND
                                          Data_in(7 DOWNTO 0) = "00001011" THEN
                                            write <= '0';
                                        END IF;
                                    END IF;
                                END IF;

                            WHEN WRR1 =>
                                IF HOLDNeg_ipd = '1' THEN
                                    IF (NOT (WPP='0')) THEN
                                        IF ((data_cnt mod 8) = 0
                                             AND data_cnt > 0) THEN
                                            IF data_cnt = 8 THEN
                                                write <= '0';
                                                FOR i IN 0 TO 7 LOOP
                                                    Status_reg_in(8+i) <=
                                                       Data_in(7-i);
                                                END LOOP;
                                            END IF;
                                        END IF;
                                    END IF;
                                END IF;

                            WHEN WRR2 =>
                                IF HOLDNeg_ipd = '1' THEN
                                    IF ((data_cnt mod 8) = 0
                                           AND data_cnt > 0) THEN
                                        IF data_cnt = 8 THEN
                                            write <= '0';
                                            FOR i IN 0 TO 7 LOOP
                                                Status_reg_in(i) <=
                                                   Data_in(7-i);
                                            END LOOP;
                                        END IF;
                                    END IF;
                                END IF;

                            WHEN PP =>
                                IF HOLDNeg_ipd = '1' THEN
                                    IF ((data_cnt mod 8) = 0
                                           AND data_cnt > 8*Byte_number) THEN
                                            FOR I IN 0 TO 255 LOOP
                                                FOR J IN 7 DOWNTO 0 LOOP
                                                    Byte_slv(j) :=
                                                    Data_in((i*8) + (7-j));
                                                END LOOP;
                                                WByte(i) <=
                                                    to_nat(Byte_slv);
                                            END LOOP;
                                            IF data_cnt > 256*BYTE THEN
                                                Byte_number <= 255;
                                            ELSE
                                                Byte_number <= data_cnt/8-1;
                                            END IF;
                                    END IF;
                                END IF;

                            WHEN DUAL_PP =>
                                IF data_cnt > 4*Byte_number THEN
                                    IF data_cnt mod 4 = 0 THEN
                                        FOR i IN 0 TO 255 LOOP
                                            FOR J IN 3 DOWNTO 0 LOOP
                                                dual_slv :=
                                                  to_slv(dual_data_in((i*4)
                                                          + (3-j)),2);
                                                Byte_slv(2*j+1 DOWNTO 2*j) 
                                                        := dual_slv;
                                            END LOOP;
                                            WByte(i) <=
                                                    to_nat(Byte_slv);
                                        END LOOP;
                                        IF data_cnt > 1024 THEN
                                            Byte_number <= 255;
                                        ELSE
                                            Byte_number <= data_cnt/4-1;
                                        END IF;
                                    END IF;
                                END IF;

                            WHEN OTPP =>
                                IF HOLDNeg_ipd = '1' THEN
                                    IF (data_cnt > 0) AND 
                                            (data_cnt mod 8 = 0) THEN
                                        FOR I IN 0 TO 63 LOOP
                                            FOR J IN 7 DOWNTO 0 LOOP
                                                Byte_slv(j) :=
                                                   Data_in((i*8) + (7-j));
                                            END LOOP;
                                            WOTPByte(i) <= to_nat(Byte_slv);
                                        END LOOP;
                                        Byte_number <= data_cnt/8-1;
                                    END IF;
                                END IF;

                            WHEN others =>
                                null;
                        END CASE;
            END CASE;
        END IF;

    END PROCESS BusCycleDecode;

    ---------------------------------------------------------------------------
    -- Timing control for the Program Operations
    ---------------------------------------------------------------------------
    ProgTime : PROCESS(PSTART, PGSUSP, PGRES, Reset_out)
        VARIABLE pob      : time;
        VARIABLE pop      : time;
        VARIABLE elapsed  : time;
        VARIABLE start    : time;
        VARIABLE duration : time;

    BEGIN
        pob  := tdevice_ByteProgram;
        pop  := tdevice_PageProgram;
        IF rising_edge(PSTART) AND PDONE = '1' THEN 
            IF Sec_Prot(SA) = '0' THEN
                IF Byte_number = 0 THEN
                   duration := pob;
                ELSE
                   duration := pop;
                END IF;
                elapsed := 0 ns;
                PDONE <= '0', '1' AFTER duration;
                start := NOW;
            END IF;
        ELSIF PGSUSP = '1' AND PDONE /= '1' THEN
                elapsed  := NOW - start;
                duration := duration - elapsed;
                PDONE <= '0';
        ELSIF PGRES = '1' AND PDONE /= '1'THEN
                start := NOW;
                PDONE <= '0', '1' AFTER duration;
        ELSIF Reset_out = '1' THEN
            PDONE <= '1';
        END IF;
    END PROCESS ProgTime;

    ---------------------------------------------------------------------------
    -- Timing control for the OTP Program Operation
    ---------------------------------------------------------------------------
    OTPProgTime : PROCESS(OTPSTART)
        VARIABLE pootp      : time;
        VARIABLE elapsed  : time;
        VARIABLE start    : time;
        VARIABLE duration : time;

    BEGIN
        pootp := tdevice_OTPProgram;
        IF rising_edge(OTPSTART) AND OTPDONE = '1' THEN 
            duration := pootp;
            elapsed := 0 ns;
            OTPDONE <= '0', '1' AFTER duration;
            start := NOW;
        END IF;
    END PROCESS OTPProgTime;
    ---------------------------------------------------------------------------
    -- Timing control for the Write Status Register
    ---------------------------------------------------------------------------
    WriteTime : PROCESS(WSTART)
        VARIABLE wob      : time;
    BEGIN
        wob  := tdevice_WriteRegister;
        IF rising_edge(WSTART) AND WDONE = '1' THEN
            WDONE <= '0', '1' AFTER wob;
        END IF;
    END PROCESS WriteTime;

    ---------------------------------------------------------------------------
    -- Timing control for the Erase Operations
    ---------------------------------------------------------------------------
    ErsTime : PROCESS(ESTART, ESUSP, ERES, Reset_out)
        VARIABLE be4o     : time;
        VARIABLE be32o    : time;
        VARIABLE seo      : time;
        VARIABLE ceo      : time;
        VARIABLE elapsed  : time;
        VARIABLE start    : time;
        VARIABLE duration : time;
    BEGIN
        be4o  := tdevice_BlockErase4;
        be32o := tdevice_BlockErase32;
        seo   := tdevice_SectorErase;
        ceo   := tdevice_ChipErase;
        IF rising_edge(ESTART) AND EDONE = '1' THEN
                IF Instruct = B4E THEN
                    duration := be4o;
                ELSIF Instruct = B32E THEN
                    duration := be32o;
                ELSIF Instruct = SE THEN
                    duration := seo;
                ELSE
                    duration := ceo;
                END IF;
                elapsed := 0 ns;
                EDONE <= '0', '1' AFTER duration;
                start := NOW;
        ELSIF ESUSP = '1' AND EDONE /= '1' THEN
            elapsed  := NOW - start;
            duration := duration - elapsed;
            EDONE <= '0';
        ELSIF ERES = '1' AND EDONE /= '1' THEN
            start := NOW;
            EDONE <= '0', '1' AFTER duration;
        ELSIF Reset_out = '1' THEN
                EDONE <= '1';
        END IF;
    END PROCESS ErsTime;

    CheckCEOnPowerUP :PROCESS
    BEGIN
        IF CSNeg /= '1' THEN
            REPORT InstancePath & partID &
            ": Device is selected during Power Up"
            SEVERITY WARNING;
        END IF;
        WAIT;
    END PROCESS;

    ---------------------------------------------------------------------------
    -- Main Behavior Process
    -- combinational process for next state generation
    ---------------------------------------------------------------------------
    StateGen :PROCESS(write, CSNeg, WDONE, PDONE, OTPDONE, EDONE, 
                      ProgramSuspend_out, ProgramResume_out, EraseSuspend_out,
                      EraseResume_out, Reset_out, Lockdown_out)

    BEGIN
        -----------------------------------------------------------------------
        -- Functionality Section
        -----------------------------------------------------------------------
        CASE current_state IS
            WHEN IDLE          =>
                IF falling_edge(write) THEN
                    IF Instruct = WRR1 AND WEL = '1'
                       AND not(SPRL = '1' AND WPP = '0') THEN
                        next_state <= WRITE_SR1;
                    ELSIF Instruct = WRR2 AND WEL = '1' THEN
                        next_state <= WRITE_SR2;
                    ELSIF (Instruct = PP OR Instruct = DUAL_PP) 
                           AND WEL = '1' THEN
                        sect := Address / 16#10000#;
                        IF Sec_PROT(sect) = '0' AND 
                           SEC_LOCK_REG(sect) = '0' THEN 
                            next_state <= PAGE_PG;
                        ELSE
                            IF rising_edge(CSNeg_ipd) THEN
                                next_state <= IDLE;
                            END IF;
                        END IF;
                    ELSIF Instruct = OTPP AND WEL = '1' THEN
                        IF OTP_BLOCKED /= '1' THEN
                        --If OTP_BLOCKED = '1', then user-programmable space 
                        --has been programmed with some number of bytes and 
                        --cannot be programmed again;
                            next_state <=  OTP_PG;
                        END IF;
                    ELSIF Instruct = B4E AND WEL = '1' THEN
                        sect := Address / 16#10000#;
                        IF Sec_PROT(sect) = '0' AND 
                           SEC_LOCK_REG(sect) = '0' THEN
                            next_state <=  BLK_ERS_4;
                         ELSE
                            IF rising_edge(CSNeg_ipd) THEN
                                next_state <= IDLE;
                            END IF;
                        END IF;
                    ELSIF Instruct = B32E AND WEL = '1' THEN
                        sect := Address / 16#10000#;
                        IF Sec_PROT(sect) = '0' AND 
                           SEC_LOCK_REG(sect) = '0' THEN
                            next_state <=  BLK_ERS_32;
                         ELSE
                            IF rising_edge(CSNeg_ipd) THEN
                                next_state <= IDLE;
                            END IF;
                        END IF;
                    ELSIF Instruct = SE AND WEL = '1' THEN
                        sect := Address / 16#10000#;
                        SectorSuspend <= sect;
                        IF Sec_PROT(sect) = '0' AND 
                           SEC_LOCK_REG(sect) = '0' THEN
                            next_state <=  SEC_ERS;
                        ELSE
                            IF rising_edge(CSNeg_ipd) THEN
                                next_state <= IDLE;
                            END IF;
                        END IF;
                    ELSIF Instruct = CH_ERS AND WEL = '1' THEN
                        IF (SWP3 = '0' AND SWP2 = '0') THEN
                            next_state <=  CHIP_ERS;
                        ELSE
                            IF rising_edge(CSNeg_ipd) THEN
                                next_state <= IDLE;
                            END IF;
                        END IF;
                    ELSIF Instruct = PROT_SE AND WEL = '1' THEN
                        IF SPRL /= '1'  THEN
                            next_state <= PROT_SEC;
                        ELSE
                            IF rising_edge(CSNeg_ipd) THEN
                                next_state <= IDLE;
                            END IF;
                        END IF;
                    ELSIF Instruct = UNPROT_SE AND WEL = '1' THEN
                        IF SPRL /= '1' THEN
                            next_state <= UNPROT_SEC;
                        ELSE
                            IF rising_edge(CSNeg_ipd) THEN
                                next_state <= IDLE;
                            END IF;
                        END IF;
                    ELSIF Instruct = SELD AND WEL = '1' AND SLE = '1' THEN
                        sect := Address / 16#10000#;
                        IF SEC_LOCK_REG(sect) /= '1' THEN
                            next_state <= SEC_LOCKDOWN;
                        ELSE
                            IF rising_edge(CSNeg_ipd) THEN
                                next_state <= IDLE;
                            END IF;
                        END IF;
                    ELSIF Instruct = FR_SELD AND WEL = '1' AND SLE = '1' THEN
                        next_state <= FREEZE_SEC_LD;
                    ELSIF Instruct = DP THEN
                        next_state <= DP_DOWN;
                    ELSE
                        next_state <= IDLE;
                    END IF;
                END IF;

            WHEN WRITE_SR1     =>
                IF rising_edge(WDONE) THEN
                    next_state <= IDLE;
                END IF;

            WHEN WRITE_SR2     =>
                IF rising_edge(WDONE) THEN
                    next_state <= IDLE;
                END IF;

            WHEN PAGE_PG         =>
                IF falling_edge(write) THEN
                    IF Instruct = RESET AND RSTE = '1' THEN
                        next_state <= RST;
                    END IF;
                ELSIF ProgramSuspend_out = '1' THEN
                    next_state <= PG_SUSP;
                ELSIF PDONE = '1' THEN
                    next_state <= IDLE;
                END IF;

            WHEN OTP_PG       =>
                IF rising_edge(OTPDONE) THEN
                    next_state <= IDLE;
                END IF;

            WHEN PG_SUSP      =>
                IF falling_edge(write) THEN
                    IF Instruct = RESET AND RSTE = '1' THEN
                        next_state <= RST;
                    END IF;
                ELSIF ProgramResume_out = '1' THEN
                    next_state <=  PAGE_PG;
                ELSE
                    next_state <= PG_SUSP;
                END IF;

            WHEN SEC_ERS  =>
                IF falling_edge(write) THEN
                    IF Instruct = RESET AND RSTE = '1' THEN
                        next_state <= RST;
                    END IF;
                ELSIF EraseSuspend_out = '1' THEN
                    next_state <= ERS_SUSP;
                ELSIF EDONE = '1' THEN
                    next_state <= IDLE;
                END IF;

            WHEN BLK_ERS_4 | BLK_ERS_32 | CHIP_ERS  =>
                IF falling_edge(write) THEN
                    IF Instruct = RESET AND RSTE = '1' THEN
                        next_state <= RST;
                    END IF;
                ELSIF EDONE = '1' THEN
                    next_state <= IDLE;
                END IF;

            WHEN ERS_SUSP      =>
                IF falling_edge(write) THEN
                    IF Instruct = RESET AND RSTE = '1' THEN
                        next_state <= RST;
                    ELSIF Instruct = PP AND
                          SectorSuspend /= Address / 16#10000# THEN
                        next_state <= ERS_SUSP_PG;
                    END IF;
                ELSIF EraseResume_out = '1' THEN
                    next_state <=  SEC_ERS;
                ELSE
                    next_state <= ERS_SUSP;
                END IF;

            WHEN ERS_SUSP_PG         =>
                IF falling_edge(write) THEN
                    IF Instruct = RESET AND RSTE = '1' THEN
                        next_state <= RST;
                    END IF;
                ELSIF ProgramSuspend_out = '1' THEN
                        next_state <= ERS_SUSP_PG_SUSP;
                ELSIF PageProgram_out = '1' THEN
                    next_state <= ERS_SUSP;
                END IF;

            WHEN ERS_SUSP_PG_SUSP      =>
                IF falling_edge(write) THEN
                    IF Instruct = RESET AND RSTE = '1' THEN
                        next_state <= RST;
                    END IF;
                ELSIF ProgramResume_out = '1' THEN
                        next_state <=  ERS_SUSP_PG;
                ELSE
                    next_state <= ERS_SUSP_PG_SUSP;
                END IF;

            WHEN PROT_SEC | UNPROT_SEC       =>
                IF rising_edge(CSNeg_ipd) THEN
                    next_state <= IDLE;
                END IF;

           WHEN FREEZE_SEC_LD     =>
                IF Lockdown_out = '1' THEN
                    next_state <= IDLE;
                END IF;

           WHEN SEC_LOCKDOWN     =>
                IF Lockdown_out = '1' THEN
                    next_state <= IDLE;
                END IF;

            WHEN DP_DOWN      =>
                IF falling_edge(write) AND Instruct = RES THEN
                    next_state <= IDLE;
                END IF;

            WHEN RST      =>
                IF Reset_out = '1' THEN
                    next_state <= IDLE;
                END IF;
        END CASE;

    END PROCESS StateGen;

    ---------------------------------------------------------------------------
    --FSM Output generation and general funcionality
    ---------------------------------------------------------------------------
    Functional : PROCESS(write,read_out, WDONE, PDONE, OTPDONE, EDONE, 
                         current_state, CSNeg_ipd, HOLDNeg_ipd, Instruct,
                         Address, WByte,change_addr, PoweredUp, 
                         Reset_out, RDPD_out, EDPD_out,Lockdown_out,
                         SectorProtect_out,SectorUnprotect_out,
                         EraseSuspend_out, EraseResume_out,
                         PageProgram_out,
                         ProgramSuspend_out, ProgramResume_out,WPNeg_ipd)

        TYPE WDataType IS ARRAY (0 TO 255) OF INTEGER RANGE -1 TO MaxData;
        TYPE WOTPDataType IS ARRAY (0 TO 100) OF INTEGER RANGE -1 TO MaxData;

        VARIABLE WData          : WDataType := (OTHERS => 0);
        VARIABLE WOTPData       : WOTPDataType  := (OTHERS => 0);
        VARIABLE oe             : boolean := FALSE;

        VARIABLE AddrLo         : NATURAL;
        VARIABLE AddrHi         : NATURAL;
        VARIABLE Addr           : NATURAL;
        VARIABLE Addr_tmp       : NATURAL;

        VARIABLE read_addr      : NATURAL RANGE 0 TO AddrRANGE;
        VARIABLE data_out       : std_logic_vector(7 downto 0);
        VARIABLE ident_out      : std_logic_vector(31 downto 0);

        VARIABLE old_bit        : std_logic_vector(7 downto 0);
        VARIABLE new_bit        : std_logic_vector(7 downto 0);
        VARIABLE old_int        : INTEGER RANGE -1 to MaxData;
        VARIABLE new_int        : INTEGER RANGE -1 to MaxData;
        VARIABLE wr_cnt         : NATURAL RANGE 0 TO 255;
        VARIABLE byte_cnt       : NATURAL RANGE 0 TO 255;

        VARIABLE sect           : NATURAL RANGE 0 TO SecNum;
        VARIABLE sectERS        : NATURAL RANGE 0 TO SecNum;
        VARIABLE cnt            : NATURAL RANGE 0 TO 256 := 0;

    BEGIN
        -----------------------------------------------------------------------
        -- Functionality Section
        -----------------------------------------------------------------------
        oe := rising_edge(read_out) AND PoweredUp = '1';

        IF Instruct'EVENT THEN
            read_cnt := 0;
            rd        <= false;
            medium_rd <= true;
            fast_rd   <= false;
            dual      <= false;
        END IF;

        IF rising_edge(PoweredUp) THEN
            --the default states after power-up
            --Status Register Byte 1
            SPRL    := '0';
            WPP     := WPNeg_ipd;
            SWP3    := '1';
            SWP2    := '1';
            WEL     := '0';
            RDYBSY1 := '0';
            --Status Register Byte 2
            RSTE    := '0';
            SLE     := '0';
            PS      := '0';
            ES      := '0';
            RDYBSY2 := '0';
        END IF;

        IF rising_edge(change_addr) THEN
            read_addr := Address;
        END IF;

        CASE current_state IS
            WHEN IDLE          =>
                IF falling_edge(write) THEN
                    read_cnt := 0;
                    IF Instruct = WREN THEN
                        WEL := '1';
                    ELSIF Instruct = WRDI THEN
                        WEL := '0';
                    ELSIF Instruct = WRR1 AND WEL = '1'
                       AND not(SPRL = '1' AND WPP = '0') THEN
                        WSTART <= '1', '0' AFTER 1 ns;
                    ELSIF Instruct = WRR2 AND WEL = '1' THEN
                        WSTART <= '1', '0' AFTER 1 ns;
                    ELSIF (Instruct = PP OR Instruct = DUAL_PP) 
                           AND WEL = '1' THEN
                        sect := Address / 16#10000#;
                        IF Sec_Prot(sect) = '0' THEN
                            PSTART <= '1', '0' AFTER 5 ns;
                            PGSUSP  <= '0';
                            PGRES   <= '0';
                            RDYBSY1 := '1' ;
                            RDYBSY2 := '1' ;
                            SA <= sect;
                            Addr := Address;
                            Addr_tmp := Address;
                            wr_cnt := Byte_number;
                            FOR I IN wr_cnt DOWNTO 0 LOOP
                                IF Viol /= '0' THEN
                                    WData(i) := -1;
                                ELSE
                                    WData(i) := WByte(i);
                                END IF;
                            END LOOP;
                        ELSE
                            WEL   := '0';
                        END IF;
                   ELSIF Instruct = OTPP AND WEL = '1' THEN
                        IF OTP_BLOCKED /= '1' THEN
                            OTPSTART <= '1', '0' AFTER 1 ns;
                            RDYBSY1 := '1';
                            RDYBSY2 := '1';
                            Addr := Address;
                            Addr_tmp := Address;
                            wr_cnt := Byte_number;
                            FOR i IN 0 TO 63 LOOP
                                IF wr_cnt > 64 THEN
                                    WOTPData(i) := WOTPByte(i + wr_cnt-63);
                                ELSE
                                    WOTPData(i) := WOTPByte(i);
                                END IF;
                            END LOOP;
                        ELSE
                            WEL   := '0';
                        END IF;
                    ELSIF Instruct = B4E AND WEL = '1' THEN
                        sect := Address / 16#10000#;
                        IF Sec_PROT(sect) = '0' AND 
                           SEC_LOCK_REG(sect) = '0' THEN
                            ESTART  <= '1', '0' AFTER 1 ns;
                            ESUSP   <= '0';
                            ERES    <= '0';
                            RDYBSY1 := '1';
                            RDYBSY2 := '1';
                            Addr := Address;
                            sectERS := sect;
                        ELSE
                            WEL   := '0';
                        END IF;
                    ELSIF Instruct = B32E AND WEL = '1' THEN
                        sect := Address / 16#10000#;
                        IF Sec_PROT(sect) = '0' AND 
                           SEC_LOCK_REG(sect) = '0' THEN
                            ESTART  <= '1', '0' AFTER 1 ns;
                            ESUSP   <= '0';
                            ERES    <= '0';
                            RDYBSY1 := '1';
                            RDYBSY2 := '1';
                            Addr := Address;
                            sectERS := sect;
                        ELSE
                            WEL   := '0';
                        END IF;
                    ELSIF Instruct = SE AND WEL = '1' THEN
                        sect := Address / 16#10000#;
                        IF Sec_PROT(sect) = '0' AND 
                           SEC_LOCK_REG(sect) = '0' THEN
                            ESTART  <= '1', '0' AFTER 1 ns;
                            ESUSP   <= '0';
                            ERES    <= '0';
                            RDYBSY1 := '1';
                            RDYBSY2 := '1';
                            Addr := Address;
                            sectERS := sect;
                        ELSE
                            WEL   := '0';
                        END IF;
                    ELSIF Instruct = CH_ERS AND WEL = '1' THEN
                        IF (SWP3 = '0' AND SWP2 = '0') THEN
                            ESTART <= '1', '0' AFTER 1 ns;
                            RDYBSY1 := '1';
                            RDYBSY2 := '1';
                        ELSE
                            WEL   := '0';
                        END IF;
                    ELSIF Instruct = PROT_SE AND WEL = '1' THEN
                        IF SPRL /= '1'  THEN
                            sect := Address / 16#10000#;
                            SectorProtect_in <= '1';
                        ELSE
                            WEL   := '0';
                        END IF;
                    ELSIF Instruct = UNPROT_SE AND WEL = '1' THEN
                        IF SPRL /= '1' THEN
                            sect := Address / 16#10000#;
                            SectorUnprotect_in <= '1';
                        ELSE
                            WEL   := '0';
                        END IF;
                    ELSIF Instruct = SELD AND WEL = '1' AND SLE = '1' THEN
                            sect := Address / 16#10000#;
                            IF SEC_LOCK_REG(sect) /= '1' THEN
                               Lockdown_in <= '1'; 
                            ELSE
                               WEL   := '0';
                            END IF;
                    ELSIF Instruct = FR_SELD AND WEL = '1' THEN
                        IF SLE = '1' THEN
                            Lockdown_in <= '1';
                        ELSE
                            WEL   := '0';
                        END IF;
                    ELSIF Instruct = DP THEN
                    --When the CS pin is deasserted, the device will enter the
                    --Deep Power-Down mode within the maximum time of tEDPD.
                        EDPD_in  <= '1', '0' AFTER tdevice_EDPD ;
                    END IF;
                ELSIF oe THEN
                    IF Instruct = RDSR THEN
                        --Read Status Register
                        SOut_zd <= Status_reg(15-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 16 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = READ OR Instruct = FAST_READ 
                          OR Instruct = MED_READ THEN
                        --Read Memory array
                        IF Instruct = READ THEN
                            rd        <= true;
                            medium_rd <= false;
                            fast_rd   <= false;
                            dual      <= false;
                        ELSIF Instruct = FAST_READ THEN
                            rd        <= false;
                            medium_rd <= false;
                            fast_rd   <= true;
                            dual      <= false;
                        ELSE
                            rd        <= false;
                            medium_rd <= true;
                            fast_rd   <= false;
                            dual   <= false;
                        END IF;
                        IF Mem(read_addr) /= -1 THEN
                            data_out := to_slv(Mem(read_addr),8);
                            SOut_zd <= data_out(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                IF read_addr = AddrRANGE THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        ELSE
                            SOut_zd <= 'U';
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
                    ELSIF Instruct = DUAL_READ THEN
                        --Read Memory array
                        fast_rd   <= false;
                        medium_rd <= false;
                        rd        <= false;
                        dual      <= true;
                        data_out  := to_slv(Mem(read_addr),8);
                        SOut_zd   <= data_out(7-2*read_cnt);
                        SIOut_zd  <= data_out(6-2*read_cnt);
                        read_cnt  := read_cnt + 1;
                        IF read_cnt = 4 THEN
                            read_cnt := 0;
                            IF read_addr = AddrRANGE THEN
                                read_addr := 0;
                            ELSE
                                read_addr := read_addr + 1;
                            END IF;
                        END IF;
                    ELSIF Instruct = READ_ID THEN
                    --Read Manufacturer and Device ID
                    --can be terminated by driving CSNeg high
                    --at any time
                        IF read_cnt < 32 THEN
                            ident_out := to_slv(Manuf_ID,8) 
                                       & to_slv(DeviceID_P1,8)
                                       & to_slv(DeviceID_P2,8) 
                                       & to_slv(ExtendedBytes,8);
                            SOut_zd <= ident_out(31-read_cnt);
                            read_cnt  := read_cnt + 1;
                        ELSE
                            SOut_zd <= 'Z';
                        END IF;
                    ELSIF Instruct = RD_SELD_REG THEN
                        sect := Address / 16#10000#;
                        SOut_zd <= SEC_LOCK_REG(sect);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RD_SE_PROT_REG THEN
                        sect := Address / 16#10000#;
                        SOut_zd <= SEC_PROT(sect);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = OTPR  THEN
                        data_out := to_slv(OTPMem(read_addr),8);
                        SOut_zd <= data_out(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                            IF read_addr = OTPHiAddr THEN
                                read_addr := 0;
                            ELSE
                                read_addr := read_addr + 1;
                            END IF;
                        END IF;
                    END IF;
                END IF;

            WHEN WRITE_SR1     =>
                IF WDONE = '1' THEN
                    IF SPRL = '0' THEN
                        IF Status_reg_in(13 DOWNTO 10) = "0000" THEN
                        --Global Unprotect function
                            SEC_PROT:= (others => '0');
                            SPRL := Status_reg_in(15);
                            SWP2 := '0';
                            SWP3 := '0';
                        ELSIF Status_reg_in(13 DOWNTO 10) = "1111" THEN
                        --Global Protect function
                            IF PS = '0' OR ES = '0' THEN
                            --If a Global Protect operation is attempted while
                            --a sector is erase or program suspended, the 
                            --protection operation will abort,the protection 
                            --states of all sectors in the Flash memory array 
                            --will not change, and WEL bit in the Status 
                            --Register will be reset back to a logical “0”.
                                SEC_PROT:= (others => '1'); 
                                SPRL := Status_reg_in(15);
                                SWP2 := '1';
                                SWP3 := '1';
                            ELSE
                                WEL  := '0';
                            END IF;
                        ELSE
                            SPRL := Status_reg_in(15);
                        END IF;
                    ELSE
                        IF WPP = '1' THEN 
                        --The Sector Protection Registers are soft-locked and
                        --cannot be changed. Therefore, a Global Protect/
                        --Unprotect will not occur.The SPRL bit can be changed
                        --back to a 0 from a 1 since the WP pin is HIGH.
                            SPRL := Status_reg_in(7);
                        ELSE
                        --If an attempt is made to reset the SPRL bit to a 
                        --logical “0” while the WP pin is asserted, then the
                        --Write Status Register Byte 1 command will be ignored
                        --and the WEL bit in the Status Register will be reset
                        --back to the logical “0” state.
                            WEL  := '0';
                        END IF;
                    END IF;
                    WEL := '0';
                END IF;

            WHEN WRITE_SR2     =>
                IF WDONE = '1' THEN
                    RSTE := Status_Reg_in(4);
                    IF FROZEN = '0' THEN 
                    --the Freeze Sector Lockdown State command has not been
                    --previously issued.
                        SLE := Status_Reg_in(3); 
                    END IF;
                    WEL := '0';
                END IF;

            WHEN PAGE_PG         =>
                IF oe AND Instruct = RDSR THEN
                    --Read Status Register
                    SOut_zd <= Status_reg(15-read_cnt);
                    read_cnt := read_cnt + 1;
                    IF read_cnt = 16 THEN
                        read_cnt := 0;
                    END IF;
                END IF;

                ADDRHILO_PP(AddrLo, AddrHi, Addr);
                cnt := 0;

                FOR i IN 0 TO wr_cnt LOOP
                    new_int := WData(i);
                    old_int := Mem(Addr + i - cnt);
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

                    Mem(Addr + i - cnt) :=  -1;

                    IF (Addr + i) = AddrHi THEN
                        Addr := AddrLo;
                        cnt := i + 1;
                    END IF;
                END LOOP;
                cnt :=0;

                IF PDONE = '1' AND SEC_PROT(sect) = '0' AND PS /= '1' AND
                           SEC_LOCK_REG(sect) = '0' THEN
                    FOR i IN 0 TO wr_cnt LOOP
                        Mem(Addr_tmp + i - cnt) := WData(i);
                        IF (Addr_tmp + i) = AddrHi THEN
                            Addr_tmp := AddrLo;
                            cnt := i + 1;
                        END IF;
                    END LOOP;
                    WEL := '0';
                    EPE := '0';
                    RDYBSY1 := '0';
                    RDYBSY2 := '0';
                    Addr := secters * 16#10000#;
                ELSIF Instruct = PESP THEN
                    PGSUSP <= '1', '0' AFTER 1 ns;
                    ProgramSuspend_in <= '1';
                ELSIF ProgramResume_out = '1' THEN
                    ProgramResume_in <= '0';
                    PS := '0';
                    RDYBSY1 := '1';
                    RDYBSY2 := '1';
                ELSIF Instruct=RESET AND RSTE = '1' THEN
                    Reset_in <= '1';
                END IF;

            WHEN OTP_PG       =>
                IF oe AND Instruct = RDSR THEN
                    --Read Status Register
                    SOut_zd <= Status_reg(15-read_cnt);
                    read_cnt := read_cnt + 1;
                    IF read_cnt = 16 THEN
                        read_cnt := 0;
                    END IF;
                END IF;

                cnt := 0;

                FOR i IN 0 TO 63 LOOP
                    new_int := WOTPData(i);
                    old_int := OTPMem(Addr + i - cnt);
                    IF new_int > 0 THEN
                        new_bit := to_slv(new_int,8);
                        old_bit := to_slv(old_int,8);
                        new_int := to_nat(new_bit);
                        WOTPData(i) := new_int;
                    ELSE
                        WOTPData(i) := 16#FF#;
                    END IF;
                    IF (Addr + i) = 63 THEN
                        Addr := OTPLoAddr;
                        cnt := i + 1;
                    END IF;
                END LOOP;

                cnt :=0;

                IF OTPDONE = '1' THEN

                    FOR i IN 0 TO 63 LOOP
                        OTPMem(Addr_tmp + i - cnt) := WOTPData(i);
                        IF (Addr_tmp + i) = 63 THEN
                            Addr_tmp := OTPLoAddr;
                            cnt := i + 1;
                        END IF;
                    END LOOP;

                    WEL := '0';
                    RDYBSY1 := '0';
                    RDYBSY2 := '0';
                    OTP_BLOCKED <= '1';
                END IF;

            WHEN PG_SUSP | ERS_SUSP_PG_SUSP     =>
                IF ProgramSuspend_out = '1' THEN
                    ProgramSuspend_in <= '0';
                    --The RDY/BSY bit in the Status Register will indicate that
                    --the device is ready for another operation.
                    RDYBSY1 := '0';
                    RDYBSY2 := '0';
                    --The Program Suspend (PS) bit in the Status Register will
                    --be set to the logical “1” state to indicate that the 
                    --program operation has been suspended.
                    PS := '1';
                END IF;
                IF PS = '1' THEN
                    IF oe THEN
                        IF Instruct = RDSR THEN
                            --Read Status Register
                            SOut_zd <= Status_reg(15-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 16 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF Instruct = READ OR Instruct = FAST_READ 
                            OR Instruct = MED_READ THEN
                            --Read Memory array
                            IF Instruct = READ THEN
                                rd        <= true;
                                medium_rd <= false;
                                fast_rd   <= false;
                                dual      <= false;
                            ELSIF Instruct = FAST_READ THEN
                                rd        <= false;
                                medium_rd <= false;
                                fast_rd   <= true;
                                dual      <= false;
                            ELSE
                                rd        <= false;
                                medium_rd <= true;
                                fast_rd   <= false;
                                dual      <= false;
                            END IF;
                            IF  SA /= read_addr/16#10000#  THEN
                                data_out := to_slv(Mem(read_addr),8);
                                SOut_zd <= data_out(7-read_cnt);
                            ELSE 
                                SOut_zd <= 'U';
                            END IF;
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                IF read_addr = AddrRANGE THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        ELSIF Instruct = DUAL_READ THEN
                            --Read Memory array
                            fast_rd   <= false;
                            medium_rd <= false;
                            rd        <= false;
                            dual      <= true;
                            IF SA /= read_addr/16#10000# THEN
                                data_out  := to_slv(Mem(read_addr),8);
                                SOut_zd   <= data_out(7-2*read_cnt);
                                SIOut_zd  <= data_out(6-2*read_cnt);
                            ELSE 
                                SOut_zd  <= 'U';
                                SIOUT_zd <= 'U';
                            END IF;
                            read_cnt  := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                                IF read_addr = AddrRANGE THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        ELSIF Instruct = READ_ID THEN
                        --Read Manufacturer and Device ID
                        --can be terminated by driving CSNeg high
                        --at any time
                            IF read_cnt < 32 THEN
                                ident_out := to_slv(Manuf_ID,8) 
                                           & to_slv(DeviceID_P1,8)
                                           & to_slv(DeviceID_P2,8) 
                                           & to_slv(ExtendedBytes,8);
                                SOut_zd <= ident_out(31-read_cnt);
                                read_cnt  := read_cnt + 1;
                            ELSE
                                SOut_zd <= 'Z';
                            END IF;
                        ELSIF Instruct = RD_SELD_REG THEN
                            sect := Address / 16#10000#;
                            SOut_zd <= SEC_LOCK_REG(sect);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF Instruct = RD_SE_PROT_REG THEN
                            sect := Address / 16#10000#;
                            SOut_zd <= SEC_PROT(sect);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF Instruct = OTPR  THEN
                            data_out := to_slv(OTPMem(read_addr),8);
                            SOut_zd <= data_out(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                IF read_addr = OTPHiAddr THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        END IF;
                    ELSIF Instruct = RESET AND RSTE = '1' THEN
                        Reset_in <= '1';
                        PageProgram_in <= '0';
                    ELSIF Instruct = PERES THEN
                        ProgramResume_in <= '1';
                        PGRES <= '1', '0' AFTER 1 ns;
                    END IF;
                END IF;

            WHEN SEC_ERS  =>
                IF oe AND Instruct = RDSR THEN
                    --Read Status Register
                    SOut_zd <= Status_reg(15-read_cnt);
                    read_cnt := read_cnt + 1;
                    IF read_cnt = 16 THEN
                        read_cnt := 0;
                    END IF;
                END IF;

                ADDRHILO_SEC(AddrLo, AddrHi, Addr);
                FOR i IN AddrLo TO AddrHi LOOP
                    Mem(i) := -1;
                END LOOP;
                IF EDONE = '1' AND SEC_PROT(sect) = '0' AND 
                           SEC_LOCK_REG(sect) = '0' THEN
                    WEL   := '0';
                    RDYBSY1 := '0';
                    RDYBSY2 := '0';
                    EPE := '0';
                    FOR i IN AddrLo TO AddrHi LOOP
                        Mem(i) :=  MaxData;
                    END LOOP;
                ELSIF Instruct = PESP THEN
                    ESUSP <= '1', '0' AFTER 1 ns;
                    EraseSuspend_in <= '1';
                ELSIF EraseResume_out = '1' THEN
                    EraseResume_in <= '0';
                    ES := '0';
                    RDYBSY1 := '1';
                    RDYBSY2 := '1';
                ELSIF Instruct=RESET AND RSTE = '1' THEN
                    Reset_in <= '1';
                    EPE := '1';
                END IF;

            WHEN BLK_ERS_4  =>
                IF oe AND Instruct = RDSR THEN
                    --Read Status Register
                    SOut_zd <= Status_reg(15-read_cnt);
                    read_cnt := read_cnt + 1;
                    IF read_cnt = 16 THEN
                        read_cnt := 0;
                    END IF;
                END IF;
                ADDRHILO_BS4(AddrLo, AddrHi, Addr);
                FOR i IN AddrLo TO AddrHi LOOP
                    Mem(i) := -1;
                END LOOP;
                IF EDONE = '1' AND SEC_PROT(sect) = '0' AND 
                           SEC_LOCK_REG(sect) = '0' THEN
                    WEL   := '0';
                    RDYBSY1 := '0';
                    RDYBSY2 := '0';
                    EPE := '0';
                    FOR i IN AddrLo TO AddrHi LOOP
                        Mem(i) :=  MaxData;
                    END LOOP;
                ELSIF Instruct=RESET AND RSTE = '1' THEN
                    Reset_in <= '1';
                    EPE := '1';
                END IF;

            WHEN BLK_ERS_32  =>
                IF oe AND Instruct = RDSR THEN
                    --Read Status Register
                    SOut_zd <= Status_reg(15-read_cnt);
                    read_cnt := read_cnt + 1;
                    IF read_cnt = 16 THEN
                        read_cnt := 0;
                    END IF;
                END IF;

                ADDRHILO_BS32(AddrLo, AddrHi, Addr);
                FOR i IN AddrLo TO AddrHi LOOP
                    Mem(i) := -1;
                END LOOP;
                IF EDONE = '1' AND SEC_PROT(sect) = '0' AND 
                           SEC_LOCK_REG(sect) = '0' THEN
                    WEL   := '0';
                    RDYBSY1 := '0';
                    RDYBSY2 := '0';
                    EPE := '0';
                    FOR i IN AddrLo TO AddrHi LOOP
                        Mem(i) :=  MaxData;
                    END LOOP;
                ELSIF Instruct=RESET AND RSTE = '1' THEN
                    Reset_in <= '1';
                    EPE := '1';
                END IF;

            WHEN CHIP_ERS    =>
                IF oe AND Instruct = RDSR THEN
                    --Read Status Register
                    SOut_zd <= Status_reg(15-read_cnt);
                    read_cnt := read_cnt + 1;
                    IF read_cnt = 16 THEN
                        read_cnt := 0;
                    END IF;
                END IF;

                FOR i IN 0 TO AddrRANGE LOOP
                    Mem(i) := -1;
                END LOOP;
                IF EDONE = '1' AND (SWP3 = '0' AND SWP2 = '0') THEN
                    WEL   := '0';
                    RDYBSY1 := '0';
                    RDYBSY2 := '0';
                    EPE := '0';
                    FOR i IN 0 TO AddrRANGE LOOP
                        Mem(i) :=  MaxData;
                    END LOOP;
                ELSIF Instruct=RESET AND RSTE = '1' THEN
                    Reset_in <= '1';
                    EPE := '1';
                END IF;

            WHEN ERS_SUSP    =>
                IF PageProgram_out = '1' THEN
                   PageProgram_in <= '0';
                END IF;
                IF EraseSuspend_out = '1' THEN
                    EraseSuspend_in <= '0';
                    --The Erase Suspend (ES) bit in the Status Register will
                    --be set to the logical “1” state to indicate that the 
                    --erase operation has been suspended.
                    ES := '1';
                    --The RDY/BSY bit in the Status Register will indicate that
                    --the device is ready for another operation.
                    RDYBSY1 := '0';
                    RDYBSY2 := '0';
                END IF;
                IF ES = '1' THEN
                    IF oe THEN
                        IF Instruct = RDSR THEN
                            --Read Status Register
                            SOut_zd <= Status_reg(15-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 16 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF Instruct = READ OR Instruct = FAST_READ 
                            OR Instruct = MED_READ THEN
                            --Read Memory array
                            IF Instruct = READ THEN
                                rd        <= true;
                                medium_rd <= false;
                                fast_rd   <= false;
                                dual      <= false;
                            ELSIF Instruct = FAST_READ THEN
                                rd        <= false;
                                medium_rd <= false;
                                fast_rd   <= true;
                                dual      <= false;
                            ELSE
                                rd        <= false;
                                medium_rd <= true;
                                fast_rd   <= false;
                                dual      <= false;
                            END IF;
                            IF  sectErs /= read_addr/16#10000#  THEN
                                data_out := to_slv(Mem(read_addr),8);
                                SOut_zd <= data_out(7-read_cnt);
                            ELSE 
                                SOut_zd <= 'U';
                            END IF;
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                IF read_addr = AddrRANGE THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        ELSIF Instruct = DUAL_READ THEN
                            --Read Memory array
                            fast_rd   <= false;
                            medium_rd <= false;
                            rd        <= false;
                            dual      <= true;
                            IF sectErs /= read_addr/16#10000# THEN
                                data_out  := to_slv(Mem(read_addr),8);
                                SOut_zd   <= data_out(7-2*read_cnt);
                                SIOut_zd  <= data_out(6-2*read_cnt);
                            ELSE 
                                SOut_zd  <= 'U';
                                SIOUT_zd <= 'U';
                            END IF;
                            read_cnt  := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                                IF read_addr = AddrRANGE THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        ELSIF Instruct = READ_ID THEN
                        --Read Manufacturer and Device ID
                        --can be terminated by driving CSNeg high
                        --at any time
                            IF read_cnt < 32 THEN
                                ident_out := to_slv(Manuf_ID,8) 
                                           & to_slv(DeviceID_P1,8)
                                           & to_slv(DeviceID_P2,8) 
                                           & to_slv(ExtendedBytes,8);
                                SOut_zd <= ident_out(31-read_cnt);
                                read_cnt  := read_cnt + 1;
                            ELSE
                                SOut_zd <= 'Z';
                            END IF;
                        ELSIF Instruct = RD_SELD_REG THEN
                            sect := Address / 16#10000#;
                            SOut_zd <= SEC_LOCK_REG(sect);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF Instruct = RD_SE_PROT_REG THEN
                            sect := Address / 16#10000#;
                            SOut_zd <= SEC_PROT(sect);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        ELSIF Instruct = OTPR  THEN
                            data_out := to_slv(OTPMem(read_addr),8);
                            SOut_zd <= data_out(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                IF read_addr = OTPHiAddr THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        END IF;
                    ELSIF Instruct = WREN THEN
                        WEL := '1';
                    ELSIF Instruct = WRDI THEN
                        WEL := '0';
                    ELSIF Instruct = PP AND WEL = '1' THEN
                        sect := Address / 16#10000#;
                        IF sectERS /= sect THEN
                            IF Sec_Prot(sect) = '0' THEN
                                PageProgram_in <= '1';
                                PGSUSP  <= '0';
                                PGRES   <= '0';
                                RDYBSY1 := '1' ;
                                RDYBSY2 := '1' ;
                                SA <= sect;
                                Addr := Address;
                                Addr_tmp := Address;
                                wr_cnt := Byte_number;
                                FOR I IN wr_cnt DOWNTO 0 LOOP
                                    IF Viol /= '0' THEN
                                        WData(i) := -1;
                                    ELSE
                                        WData(i) := WByte(i);
                                    END IF;
                                END LOOP;
                            ELSE
                                WEL   := '0';
                            END IF;
                        ELSE
                            WEL := '0';
                        END IF;
                    ELSIF Instruct = RESET AND RSTE = '1' THEN
                        Reset_in <= '1';
                    ELSIF Instruct = PERES THEN
                        EraseResume_in <= '1';
                        ERES <= '1', '0' AFTER 5 ns;
                    END IF;
                END IF;

            WHEN ERS_SUSP_PG        =>
                IF oe AND Instruct = RDSR THEN
                    --Read Status Register
                    SOut_zd <= Status_reg(15-read_cnt);
                    read_cnt := read_cnt + 1;
                    IF read_cnt = 16 THEN
                        read_cnt := 0;
                    END IF;
                END IF;

                IF sectERS /= sect THEN
                    ADDRHILO_PP(AddrLo, AddrHi, Addr);
                    cnt := 0;
                ELSE
                   WEL := '0'; 
                END IF;

                IF PageProgram_out = '1' AND SEC_PROT(sect) = '0' AND 
                       SEC_LOCK_REG(sect) = '0' THEN
                        FOR i IN 0 TO wr_cnt LOOP
                            Mem(Addr + i - cnt) := WData(i);
                            IF (Addr + i) = AddrHi THEN
                                Addr := AddrLo;
                                cnt := i + 1;
                            END IF;
                        END LOOP;
                    WEL := '0';
                    EPE := '0';
                    RDYBSY1 := '0';
                    RDYBSY2 := '0';
                    Addr := secters * 16#10000#;
                ELSIF Instruct = PESP THEN
                    PGSUSP <= '1', '0' AFTER 1 ns;
                    ProgramSuspend_in <= '1';
                ELSIF ProgramResume_out = '1' THEN
                    ProgramResume_in <= '0';
                    PS := '0';
                    RDYBSY1 := '1';
                    RDYBSY2 := '1';
                ELSIF Instruct=RESET AND RSTE = '1' THEN
                    Reset_in <= '1';
                    PageProgram_in <= '0';
                END IF;

            WHEN PROT_SEC      =>
                IF SectorProtect_out = '1' THEN
                    SectorProtect_in <= '0';
                    SEC_PROT(sect) := '1';
                    SWP2 := '1';
                    WEL := '0';
                ELSIF SectorProtect_in = '1' AND Instruct'EVENT THEN
                    ASSERT false
                        REPORT InstancePath & partID & "Command results" &
                              " can be corrupted, a delay of tSECP" &
                              " currently in progress."
                        SEVERITY WARNING;
                END IF; 

            WHEN UNPROT_SEC    => 
                IF rising_edge(SectorUnprotect_out) THEN
                    SectorUnprotect_in <= '0';
                    SEC_PROT(sect) := '0';
                    WEL := '0';
                ELSIF SectorUnprotect_in = '1' AND Instruct'EVENT THEN
                    ASSERT false
                        REPORT InstancePath & partID & "Command results" &
                              " can be corrupted, a delay of tSECUP" &
                              " currently in progress."
                        SEVERITY WARNING;
                END IF;

            WHEN SEC_LOCKDOWN  =>
                IF rising_edge(Lockdown_out) THEN
                    Lockdown_in <= '0';
                    SEC_LOCK_REG(sect) := '1';
                    WEL := '0';
                ELSIF Lockdown_in = '1' AND Instruct'EVENT THEN
                    ASSERT false
                        REPORT InstancePath & partID & "Command results" &
                              " can be corrupted, a delay of tLOCK" &
                              " currently in progress."
                        SEVERITY WARNING;
                END IF;

            WHEN FREEZE_SEC_LD    =>
                IF rising_edge(Lockdown_out) THEN
                    Lockdown_in <= '0';
                    FROZEN <= '1';
                    --If the Freeze Sector Lockdown State command has been 
                    --issued, then the SLE bit will be permanently reset in
                    --the logical “0” state to indicate that the Sector 
                    --Lockdown command has been disabled.
                    SLE := '0';
                    WEL := '0';
                ELSIF Lockdown_in = '1' AND Instruct'EVENT THEN
                    ASSERT false
                        REPORT InstancePath & partID & "Command results" &
                              " can be corrupted, a delay of tLOCK" &
                              " currently in progress."
                        SEVERITY WARNING;
                END IF;

            WHEN DP_DOWN      =>
                IF rising_edge(EDPD_out) THEN
                    EDPD_in <= '0';
                ELSIF EDPD_in = '1' AND Instruct'EVENT THEN
                    ASSERT false
                        REPORT InstancePath & partID & "Command results" &
                              " can be corrupted, a delay of tEDPD" &
                              " currently in progress."
                        SEVERITY WARNING;
               ELSIF Instruct = RES THEN
                   RDPD_in <= '1';
                   IF RDPD_out = '1' THEN
                       RDPD_in <= '0'; 
                   ELSIF RDPD_in = '1' AND Instruct'EVENT THEN
                        ASSERT false
                            REPORT InstancePath & partID & "Command results" &
                                  " can be corrupted, a delay of tRDPD" &
                                  " currently in progress."
                            SEVERITY WARNING;
                   END IF;
               END IF;

            WHEN RST          =>
                   IF Reset_out = '1' THEN 
                      Reset_in <= '0';
                    IF (PDONE /= '1' AND PS = '0') OR 
                              (EDONE /= '1' AND ES = '0') THEN 
                        ASSERT false
                            REPORT InstancePath & partID & "The contents" &
                                  " cannot be guaranteed to be valid." &
                                   "  Program/Erase operation is terminated."
                            SEVERITY warning;
                        WEL := '0';
                        RDYBSY1 := '0';
                        RDYBSY2 := '0';
                    ELSIF (PDONE /= '1' AND PS = '1') THEN
                    --Prog suspend in progress
                        ADDRHILO_PP(AddrLo, AddrHi, Addr);
                        FOR i IN 0 TO byte_cnt LOOP
                            Mem(Addr + i) :=  -1;
                            IF (Addr + i) = AddrHi THEN
                                Addr := AddrLo;
                            END IF;
                        END LOOP;
                        WEL := '0';
                        PS  := '0';
                        PDONE <= '1';
                        RDYBSY1 := '0';
                        RDYBSY2 := '0';
                    ELSIF (EDONE /= '1' AND ES = '1') THEN
                    --Ers suspend in progress
                        ADDRHILO_SEC(AddrLo, AddrHi, Addr);
                        FOR i IN AddrLo TO AddrHi LOOP
                            Mem(i) := -1;
                        END LOOP;
                        WEL := '0';
                        ES  := '0';
                        PS  := '0';
                        EDONE <= '1';
                        RDYBSY1 := '0';
                        RDYBSY2 := '0'; 
                    END IF;
                END IF;

        END CASE;

        --Output Disable Control
        IF (CSNeg_ipd = '1') THEN
            SIOut_zd        <= 'Z';
            SOut_zd         <= 'Z';
        END IF;
    END PROCESS Functional;

    HOLD_FRAME_ON_PO_ZD : PROCESS(SOut_zd, SIOut_zd, HOLDNeg_ipd)
    BEGIN
        IF HOLDNeg_ipd = '0' THEN
            hold_mode := TRUE;
            SIOut_z <= 'Z';
            SOut_z  <= 'Z';
        ELSE
            IF hold_mode THEN
                SIOut_z <= SIOut_zd AFTER tpd_HOLDNeg_SO(trz0);
                SOut_z  <= SOut_zd AFTER tpd_HOLDNeg_SO(trz0);
                hold_mode := FALSE;
            ELSE
                SIOut_z <= SIOut_zd;
                SOut_z  <= SOut_zd;
                hold_mode := FALSE;
            END IF;
        END IF;
    END PROCESS HOLD_FRAME_ON_PO_ZD;
    ---------------------------------------------------------------------------
    ---- File Read Section - Preload Control
    ---------------------------------------------------------------------------
    MemPreload : PROCESS

        -- text file input variables
        FILE mem_file        : text  is  mem_file_name;
        FILE otp_file        : text  is  otp_file_name;
        VARIABLE ind         : NATURAL RANGE 0 TO AddrRANGE := 0;
        VARIABLE otp_ind   : NATURAL RANGE 16#00# TO 16#3F# := 16#00#;
        VARIABLE buf         : line;

    BEGIN
    ---------------------------------------------------------------------------
    --at25df161 memory preload file format
    ---------------------------------------------------------------------------
    --   /       - comment
    --   @aaaaaa - <aaaaaa> stands for address
    --   dd      - <dd> is byte to be written at Mem(aaaaaa++)
    --             (aaaaaa is incremented at every load)
    --   only first 1-7 columns are loaded. NO empty lines !!!!!!!!!!!!!!!!
    ---------------------------------------------------------------------------

         -- memory preload
        IF (mem_file_name /= "none" AND UserPreload) THEN
            ind := 0;
            Mem := (OTHERS => MaxData);
            WHILE (not ENDFILE (mem_file)) LOOP

                report "Preload Memory File: " & buf.all;
                IF buf(1) = '/' THEN
                    NEXT;
                ELSIF buf(1) = '@' THEN
                    IF ind > AddrRANGE THEN
                        ASSERT false
                            REPORT "Given preload address is out of" &
                                   "memory address range"
                            SEVERITY warning;
                    ELSE
                        ind := h(buf(2 to 7)); --address
                    END IF;
                ELSE
                    Mem(ind) := h(buf(1 to 2));
                    ind := ind + 1;
                END IF;
            END LOOP;
        END IF;

    ---------------------------------------------------------------------------
    --at25df161_otp memory preload file format
    ---------------------------------------------------------------------------
    --   /       - comment
    --   @aa     - <aa> stands for address
    --   dd      - <dd> is byte to be written at OTPMem(aaa++)
    --             (aa is incremented at every load)
    --   only first 1-3 columns are loaded. NO empty lines !!!!!!!!!!!!!!!!
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
                    IF otp_ind > 16#3F# THEN
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
                OutSignal       => SOOut,
                OutSignalName   => "SO",
                OutTemp         => SOut_zd,
                GlitchData      => SO_GlitchData,
                Paths           => (
                    0 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay       => VitalExtendtofillDelay(tpd_SCK_SO),
                        PathCondition   => SOut_zd = 'Z'),
                    1 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay       => VitalExtendtofillDelay(tpd_SCK_SI),
                        PathCondition   => SOut_zd = 'Z'
                                           AND dual),
                    2 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_SO,
                        PathCondition   => CSNeg_ipd = '1'),
                    3 => (InputChangeTime => HOLDNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_HOLDNeg_SO,
                        PathCondition   => TRUE)
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
                GlitchData      => SI_GlitchData,
                Paths           => (
                    0 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                         PathDelay       => VitalExtendtofillDelay(tpd_SCK_SI),
                         PathCondition   => SIOut_zd = 'Z' AND dual),
                    1 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                         PathDelay       => tpd_CSNeg_SO,
                         PathCondition   => CSNeg_ipd = '1' AND dual),
                    2 => (InputChangeTime => HOLDNeg_ipd'LAST_EVENT,
                         PathDelay       => tpd_HOLDNeg_SO,
                         PathCondition   => dual)
                )
            );
        END PROCESS;

    END BLOCK behavior;
END vhdl_behavioral;
