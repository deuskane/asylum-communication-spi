# SPI Communication Module

## Table of Contents

1. [Introduction](#introduction)
2. [HDL Modules](#hdl-modules)
   - [spi_master](#spi_master)
   - [sbi_SPI](#sbi_spi)
   - [spi_pkg](#spi_pkg)
3. [Register Map](#register-map)
4. [Verification](#verification)

---

## Introduction

This repository contains a complete SPI (Serial Peripheral Interface) communication module for the Asylum project. The module provides a full-featured SPI master interface with configurable clock polarity and phase, programmable prescaler, and integrated FIFOs for transmit and receive data paths.

The design is implemented in VHDL and follows a modular architecture:
- **spi_master**: Low-level SPI protocol engine
- **sbi_SPI**: Integration wrapper with register-based control and FIFO management
- **spi_pkg**: Package containing component declarations

### Key Features

- Configurable SPI Mode (CPOL, CPHA)
- Programmable clock prescaler for flexible clock frequency control
- Separate FIFOs for transmit (TX) and receive (RX) data streams
- Command FIFO for controlling transfer parameters
- AXI-Stream compatible data interfaces
- Full-duplex SPI communication
- Loopback mode support for testing
- GHDL simulation support via FuseSoC

### Project Structure

```
hdl/                      # HDL source files
├── spi_master.vhd       # SPI protocol engine
├── sbi_SPI.vhd          # Register-based wrapper and integration layer
├── spi_pkg.vhd          # Package with component declarations
└── csr/                 # Control Status Register definitions
    ├── SPI.hjson        # Register map in JSON format
    ├── SPI_csr.md       # Register documentation (auto-generated)
    ├── SPI_csr.vhd      # CSR implementation (auto-generated)
    └── SPI_csr.h        # C header for register definitions (auto-generated)

sim/                      # Simulation files
├── basic/               # Basic testbenches
│   ├── tb_SPI.vhd      # Main SPI testbench
│   └── tb_SPI_pkg.vhd  # Testbench package
├── models/              # Flash memory models and utilities
│   ├── m25p40.vhd      # M25P40 Flash model
│   ├── m45pe80.vhd     # M45PE80 Flash model
│   ├── s25fl064p.vhd   # S25FL064P Flash model
│   ├── s25fl512s.vhd   # S25FL512S Flash model
│   ├── s35hl256t.vhd   # S35HL256T Flash model
│   ├── memory.mem      # Memory initialization file
│   └── memoryOTP.mem   # OTP memory initialization file
└── utilities/           # Simulation utilities
    ├── conversions.vhd  # Data conversion utilities
    └── gen_utils.vhd    # General utility functions

mk/                       # Build configuration
├── defs.mk              # Build definitions
└── targets.txt          # Build targets

SPI.core                  # FuseSoC core file (main project)
SPI_models.core           # FuseSoC core file (memory models)
Makefile                  # Build and simulation automation
```

---

## HDL Modules

### spi_master

**File**: `hdl/spi_master.vhd`

The `spi_master` entity implements the low-level SPI protocol engine. It handles all timing and bit-level operations for SPI communication, including clock generation, data shifting, and chip select control.

#### Generics

| Generic | Type | Default | Description |
|---------|------|---------|-------------|
| `PRESCALER_WIDTH` | integer | 8 | Width of the prescaler ratio input |

#### Ports - Clock & Reset

| Port | Direction | Type | Description |
|------|-----------|------|-------------|
| `clk_i` | in | std_logic | System clock |
| `arst_b_i` | in | std_logic | Asynchronous active-low reset |

#### Ports - TX Data (AXI-Stream)

| Port | Direction | Type | Description |
|------|-----------|------|-------------|
| `tx_tvalid_i` | in | std_logic | TX data valid flag |
| `tx_tready_o` | out | std_logic | TX data ready flag (receiver can accept data) |
| `tx_tdata_i` | in | std_logic_vector(7 downto 0) | TX data byte |

#### Ports - RX Data (AXI-Stream)

| Port | Direction | Type | Description |
|------|-----------|------|-------------|
| `rx_tvalid_o` | out | std_logic | RX data valid flag |
| `rx_tready_i` | in | std_logic | RX data ready flag (receiver can accept data) |
| `rx_tdata_o` | out | std_logic_vector(7 downto 0) | RX data byte |

#### Ports - Command

| Port | Direction | Type | Description |
|------|-----------|------|-------------|
| `cmd_tvalid_i` | in | std_logic | Command valid flag |
| `cmd_tready_o` | out | std_logic | Command ready flag |
| `cmd_tlast_i` | in | std_logic | Last command flag (0: keep CS active, 1: deassert CS after transfer) |
| `cmd_enable_rx_i` | in | std_logic | Enable RX FIFO push (1: push received bytes to RX FIFO) |
| `cmd_enable_tx_i` | in | std_logic | Enable TX FIFO pop (1: transmit from TX FIFO, MOSI driven) |
| `cmd_nb_bytes_i` | in | std_logic_vector | Number of bytes to transfer |

#### Ports - Configuration

| Port | Direction | Type | Description |
|------|-----------|------|-------------|
| `cfg_cpol_i` | in | std_logic | SPI Clock Polarity (0: clock idles low, 1: clock idles high) |
| `cfg_cpha_i` | in | std_logic | SPI Clock Phase (0: sample on leading edge, 1: sample on trailing edge) |
| `cfg_prescaler_ratio_i` | in | std_logic_vector(PRESCALER_WIDTH-1 downto 0) | Prescaler ratio for clock division |

#### Ports - SPI Interface

| Port | Direction | Type | Description |
|------|-----------|------|-------------|
| `sclk_o` | out | std_logic | SPI Clock output |
| `sclk_oe_o` | out | std_logic | SPI Clock output enable |
| `cs_b_o` | out | std_logic | Chip Select output (active low) |
| `cs_b_oe_o` | out | std_logic | Chip Select output enable |
| `mosi_o` | out | std_logic | Master Output Slave Input |
| `mosi_oe_o` | out | std_logic | MOSI output enable |
| `miso_i` | in | std_logic | Master Input Slave Output |

#### Operation

The `spi_master` operates as a finite state machine with the following states:
- **IDLE**: Waiting for a new command
- **START**: Preparing the transfer (asserting CS)
- **TRANSFER**: Shifting bits in/out according to SPI timing
- **POSTAMBLE**: Deasserting CS if command indicates last transfer
- **DONE**: Signaling transfer completion

The module uses an internal prescaler counter to generate the SPI clock from the system clock. The prescaler ratio is controlled by the `cfg_prescaler_ratio_i` input. The actual SPI clock frequency is: `f_sclk = f_clk / (2 * (prescaler_ratio + 1))`

#### SPI Modes

| Mode | CPOL | CPHA | Description |
|------|------|------|-------------|
| 0 | 0 | 0 | Clock idles low, sample on leading edge |
| 1 | 0 | 1 | Clock idles low, sample on trailing edge |
| 2 | 1 | 0 | Clock idles high, sample on leading edge |
| 3 | 1 | 1 | Clock idles high, sample on trailing edge |

---

### sbi_SPI

**File**: `hdl/sbi_SPI.vhd`

The `sbi_SPI` entity provides the system-level integration of the SPI master. It includes:
- Register-based control interface using the SBI (System Bus Interface)
- FIFO management for TX, RX, and command channels
- Optional activity logging to files (simulation-only feature)
- Loopback mode for testing
- Integration with the auto-generated register control module

#### Generics

| Generic | Type | Default | Description |
|---------|------|---------|-------------|
| `USER_DEFINE_PRESCALER` | boolean | - | Enable software prescaler control register |
| `PRESCALER_RATIO` | std_logic_vector(7 downto 0) | - | Default prescaler ratio value |
| `DEPTH_CMD` | natural | 0 | Command FIFO depth (0 = no FIFO) |
| `DEPTH_TX` | natural | 0 | TX FIFO depth (0 = no FIFO) |
| `DEPTH_RX` | natural | 0 | RX FIFO depth (0 = no FIFO) |
| `FILENAME_CMD` | string | "dump_spi_cmd.txt" | File for logging command transactions (sim only) |
| `FILENAME_TX` | string | "dump_spi_tx.txt" | File for logging TX transactions (sim only) |
| `FILENAME_RX` | string | "dump_spi_rx.txt" | File for logging RX transactions (sim only) |

#### Ports

| Port | Direction | Type | Description |
|------|-----------|------|-------------|
| `clk_i` | in | std_logic | System clock |
| `arst_b_i` | in | std_logic | Asynchronous active-low reset |
| `sbi_ini_i` | in | sbi_ini_t | SBI initiator interface (bus input) |
| `sbi_tgt_o` | out | sbi_tgt_t | SBI target interface (bus output) |
| `sclk_o` | out | std_logic | SPI Clock output |
| `sclk_oe_o` | out | std_logic | SPI Clock output enable |
| `cs_b_o` | out | std_logic | Chip Select output (active low) |
| `cs_b_oe_o` | out | std_logic | Chip Select output enable |
| `mosi_o` | out | std_logic | Master Output Slave Input |
| `mosi_oe_o` | out | std_logic | MOSI output enable |
| `miso_i` | in | std_logic | Master Input Slave Output |

#### Operation

The `sbi_SPI` acts as a bridge between the system bus and the `spi_master` core:

1. **Register Access**: Software accesses SPI through memory-mapped registers at specified addresses
2. **FIFO Management**: Configurable FIFOs provide buffering for commands, transmitted data, and received data
3. **Data Path Mapping**: 
   - TX data written to address 0x0 goes to the TX FIFO, which feeds the SPI master
   - RX data received by the SPI master is written to the RX FIFO and read from address 0x0
   - Commands written to address 0x1 control transfer parameters
4. **Configuration**: Configuration register at address 0x2 controls SPI mode (CPOL, CPHA) and loopback mode
5. **Prescaler Control**: Optional prescaler register at address 0x3 allows software to modify the clock prescaler

---

### spi_pkg

**File**: `hdl/spi_pkg.vhd`

This VHDL package contains component declarations for the SPI module entities, allowing other modules to instantiate them.

#### Contents

- **spi_master component**: Declaration of the low-level SPI protocol engine
- **sbi_SPI component**: Declaration of the system-level SPI integration wrapper

This package should be used by any module needing to instantiate these components.

---

## Register Map

The SPI module provides a memory-mapped register interface through the SBI (System Bus Interface). All registers are 8-bit wide.

**Register Map Address Space**:
- Register Address Range: 0x0 to 0x3
- Data Width: 8 bits

See [Register Details](hdl/csr/SPI_csr.md) for complete register documentation.

### Quick Register Reference

| Address | Register | Type | Access | Description |
|---------|----------|------|--------|-------------|
| 0x0 | `data` | FIFO | R/W | Data register: TX FIFO write / RX FIFO read |
| 0x1 | `cmd` | FIFO | W/O | Command register: Control transfer parameters |
| 0x2 | `cfg` | R/W | R/W | Configuration register: SPI mode, loopback control |
| 0x3 | `prescaler` | R/W | R/W | Prescaler register: Clock frequency control (optional) |

### Register 0x0: data

**Type**: Dual-port FIFO (RW)

**Description**: 
- **Write**: Enqueues a byte in the TX FIFO to be transmitted
- **Read**: Dequeues a byte from the RX FIFO after reception

**Fields**:
- `[7:0] value`: Data byte to transmit or receive

**Blocking Behavior**: Configured with blocking read/write semantics (waits for FIFO availability)

### Register 0x1: cmd

**Type**: Write-only FIFO

**Description**: Command FIFO that controls SPI transfer parameters. Each command specifies:
- The number of bytes to transfer
- Whether to push received data to RX FIFO
- Whether to pop transmit data from TX FIFO
- Whether this is the last transfer (CS handling)

**Fields**:
- `[4:0] nb_bytes`: Number of bytes to transfer (1-32)
- `[5] last`: Last transfer flag
  - 0 = Keep CS active after transfer (CS remains low)
  - 1 = Deassert CS after transfer (CS goes high)
  - **Special Case**: If `last = 0`, `enable_rx = 0`, and `enable_tx = 0`, the transfer is stopped
- `[6] enable_rx`: Enable RX path
  - 0 = Do not push received data to RX FIFO
  - 1 = Push received bytes to RX FIFO
- `[7] enable_tx`: Enable TX path
  - 0 = Do not pop TX FIFO, MOSI output disabled (high-impedance)
  - 1 = Pop bytes from TX FIFO and drive MOSI during transfer

**Typical Usage Pattern**:
1. Write data to `data` register (fills TX FIFO)
2. Write command to `cmd` register to initiate transfer
3. Read received data from `data` register (drains RX FIFO)

### Register 0x2: cfg

**Type**: Configuration Register (RW)

**Description**: SPI configuration register controlling protocol parameters and operational modes.

**Fields**:
- `[0] spi_enable`: Not currently used (legacy field for parity configuration)
- `[1] cpol`: Clock Polarity
  - 0 = SCK idles low
  - 1 = SCK idles high
- `[2] cpha`: Clock Phase
  - 0 = Sample MISO on leading edge of SCK
  - 1 = Sample MISO on trailing edge of SCK
- `[3] loopback`: Loopback mode
  - 0 = MISO connected to external SPI slave
  - 1 = MISO internally connected to MOSI (for testing)

### Register 0x3: prescaler (Optional)

**Type**: Read/Write Register

**Conditional**: Only present if `USER_DEFINE_PRESCALER` generic is set to `true`

**Description**: Prescaler ratio register that controls SPI clock frequency. The SCK frequency is calculated as:

$$f_{SCK} = \frac{f_{CLK}}{2 \times (\text{prescaler\_ratio} + 1)}$$

**Fields**:
- `[7:0] ratio`: Prescaler ratio value (default: `PRESCALER_RATIO` generic)

**Example**: If system clock is 100 MHz and prescaler is set to 4:
$$f_{SCK} = \frac{100 \text{ MHz}}{2 \times (4 + 1)} = 10 \text{ MHz}$$

### Detailed Register Documentation

For complete field descriptions, bit patterns, and access notes, see: [hdl/csr/SPI_csr.md](hdl/csr/SPI_csr.md)

---

## Verification

The SPI module includes comprehensive verification infrastructure using GHDL and FuseSoC for simulation.

### Simulation Structure

#### Basic Testbenches (`sim/basic/`)

- **tb_SPI.vhd**: Main testbench for the `spi_master` entity
  - Tests SPI protocol timing and bit-level operations
  - Validates CPOL/CPHA mode selection
  - Tests prescaler clock generation
  - Tests command and data path handshaking
  
- **tb_SPI_pkg.vhd**: Testbench package containing utility procedures and functions for test stimulus generation

#### Flash Memory Models (`sim/models/`)

The module includes simulation models for various commercial SPI Flash devices for realistic testing:

| Device | File | Description |
|--------|------|-------------|
| M25P40 | m25p40.vhd | 512 KB Flash memory model |
| M45PE80 | m45pe80.vhd | 1 MB Flash memory model with OTP |
| S25FL064P | s25fl064p.vhd | 8 MB Serial Flash model |
| S25FL512S | s25fl512s.vhd | 64 MB Serial Flash model |
| S35HL256T | s35hl256t.vhd | 32 MB Serial Flash model |

These models implement realistic device behavior including:
- SPI protocol compliance checking
- Command decoding (Read, Write, Erase, etc.)
- State machine for device operations
- Timing constraint enforcement

**Memory Initialization Files**:
- `memory.mem`: Data memory initialization file
- `memoryOTP.mem`: One-Time Programmable (OTP) memory initialization file

#### Simulation Utilities (`sim/utilities/`)

- **conversions.vhd**: Data type conversion utilities for stimulus generation
- **gen_utils.vhd**: General utility functions for simulation helpers

### Building and Running Simulations

#### Prerequisites

- GHDL 2.0 or later (VHDL simulator)
- FuseSoC (build automation for HDL projects)
- Make

#### Build System Files

- **SPI.core**: Main FuseSoC core file defining:
  - Source filesets for HDL, simulation, and models
  - Default build target configuration
  - Register generation from HJSON (regtool)
  - Dependencies on Asylum utility libraries

- **SPI_models.core**: FuseSoC core file for Flash memory models:
  - Memory model VHDL sources
  - Memory initialization files (memory.mem, memoryOTP.mem)

- **Makefile**: Automation targets for:
  - Building the design
  - Running simulations
  - Generating documentation
  - Cleaning build artifacts

#### Simulation Targets

The SPI.core file defines the following simulation targets:

| Target | Description | Filesets |
|--------|-------------|----------|
| `default` | Default verification target | hdl + gen_csr |
| `sim_basic` | Basic SPI testbench | hdl + sim_basic |
| `sim_models` | Flash models testbench | hdl + sim_basic + models |

#### Running Simulations

To run the default simulation:

```bash
fusesoc run --target default asylum:communication:SPI
```

To run with the basic testbench:

```bash
fusesoc run --target sim_basic asylum:communication:SPI
```

To run with memory models (realistic device testing):

```bash
fusesoc run --target sim_models asylum:communication:SPI
```

Using the Makefile:

```bash
make sim              # Run default simulation
make clean            # Clean build artifacts
make help             # Display available targets
```

### Simulation Features

#### Activity Logging

The `sbi_SPI` module supports optional activity logging to files during simulation (synthesis translate_off blocks):

- **dump_spi_cmd.txt**: Logs all command FIFO transactions
- **dump_spi_tx.txt**: Logs all TX FIFO transactions
- **dump_spi_rx.txt**: Logs all RX FIFO transactions

These log files are useful for:
- Verifying transaction sequences
- Debugging protocol issues
- Post-simulation analysis
- Regression testing

The log filenames can be customized via the `FILENAME_CMD`, `FILENAME_TX`, and `FILENAME_RX` generics.

#### Loopback Mode Testing

The configuration register includes a loopback mode for testing without external hardware:
- Setting `cfg.loopback = 1` internally connects MOSI to MISO
- Allows verification of data path integrity
- Useful for basic functionality testing

### Dependencies

The SPI module depends on external libraries from the Asylum project:

| Dependency | Description |
|------------|-------------|
| `asylum:utils:generators` | Generator utility library |
| `asylum:utils:pkg` | Utility package with common functions |
| `asylum:sbi_pkg` | System Bus Interface definitions |
| `fmf:memory:flash_nor` | Flash memory models (for sim_models target) |

These dependencies are specified in the SPI.core file and automatically resolved by FuseSoC.

