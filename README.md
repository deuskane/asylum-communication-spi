# asylum-component-gpio

This documentation covers the modules found in the `hdl` directory of the [asylum-communication-spi](https://github.com/deuskane/asylum-communication-spi) repository.  
**Note:** Only the most relevant HDL modules are documented here. For a complete and up-to-date view, please refer to the [GitHub repository](https://github.com/deuskane/asylum-communication-spi/tree/master/hdl).

---

## 1. pbi_SPI

### Description

`pbi_SPI` is a SPI controller module with integrated bus interface and register map. It supports configurable prescaler, FIFO depth for commands, TX and RX, and file logging for debugging purposes.

### Parameters

| Name                 | Type                | Default Value         | Description                                              |
|----------------------|---------------------|-----------------------|----------------------------------------------------------|
| USER_DEFINE_PRESCALER| boolean             | -                     | Enable user-defined prescaler                            |
| PRESCALER_RATIO      | std_logic_vector(7:0)| -                    | Default value for prescaler ratio                        |
| DEPTH_CMD            | natural             | 0                     | Depth of command FIFO                                    |
| DEPTH_TX             | natural             | 0                     | Depth of TX FIFO                                         |
| DEPTH_RX             | natural             | 0                     | Depth of RX FIFO                                         |
| FILENAME_CMD         | string              | "dump_spi_cmd.txt"    | Command log file name                                    |
| FILENAME_TX          | string              | "dump_spi_tx.txt"     | TX log file name                                         |
| FILENAME_RX          | string              | "dump_spi_rx.txt"     | RX log file name                                         |

### Interface Signals

| Name          | Direction | Type            | Description                  |
|---------------|-----------|-----------------|------------------------------|
| clk_i         | in        | std_logic       | Clock                        |
| arst_b_i      | in        | std_logic       | Asynchronous reset           |
| pbi_ini_i     | in        | pbi_ini_t       | Bus interface                |
| pbi_tgt_o     | out       | pbi_tgt_t       | Bus interface                |
| sw2hw_o       | out       | SPI_sw2hw_t     | SW to HW register map        |
| hw2sw_i       | in        | SPI_hw2sw_t     | HW to SW register map        |

### Example Instantiation

```vhdl
inst_pbi_spi : pbi_SPI
  generic map (
    USER_DEFINE_PRESCALER => true,
    PRESCALER_RATIO       => "00010000",
    DEPTH_CMD             => 8,
    DEPTH_TX              => 16,
    DEPTH_RX              => 16
  )
  port map (
    clk_i     => clk,
    arst_b_i  => reset_n,
    pbi_ini_i => bus_in,
    pbi_tgt_o => bus_out,
    sw2hw_o   => sw2hw,
    hw2sw_i   => hw2sw
  );
```

---

## 2. SPI_registers

### Description

`SPI_registers` provides the control and status register (CSR) interface for the SPI module, including prescaler configuration and FIFO management.

### Parameters

| Name                 | Type                | Description                         |
|----------------------|---------------------|-------------------------------------|
| USER_DEFINE_PRESCALER| boolean             | Enable user-defined prescaler       |
| PRESCALER_RATIO      | std_logic_vector(7:0)| Default value for prescaler ratio   |
| DEPTH_TX             | natural             | TX FIFO depth                       |
| DEPTH_RX             | natural             | RX FIFO depth                       |
| DEPTH_CMD            | natural             | Command FIFO depth                  |

### Interface Signals

| Name          | Direction | Type            | Description                  |
|---------------|-----------|-----------------|------------------------------|
| clk_i         | in        | std_logic       | Clock                        |
| arst_b_i      | in        | std_logic       | Asynchronous reset           |
| pbi_ini_i     | in        | pbi_ini_t       | Bus interface                |
| pbi_tgt_o     | out       | pbi_tgt_t       | Bus interface                |
| sw2hw_o       | out       | SPI_sw2hw_t     | SW to HW register map        |
| hw2sw_i       | in        | SPI_hw2sw_t     | HW to SW register map        |

### Example Instantiation

```vhdl
ins_csr : SPI_registers
  generic map(
    USER_DEFINE_PRESCALER => USER_DEFINE_PRESCALER,
    PRESCALER_RATIO       => PRESCALER_RATIO,
    DEPTH_CMD             => DEPTH_CMD,
    DEPTH_TX              => DEPTH_TX,
    DEPTH_RX              => DEPTH_RX
  )
  port map(
    clk_i      => clk,
    arst_b_i   => reset_n,
    pbi_ini_i  => bus_in,
    pbi_tgt_o  => bus_out,
    sw2hw_o    => sw2hw,
    hw2sw_i    => hw2sw
  );
```

---

## 3. spi_master

### Description

`spi_master` is a low-level SPI master controller handling clock generation, data transfer, and chip select logic.

### Interface Signals

| Name         | Direction | Type              | Description                       |
|--------------|-----------|-------------------|-----------------------------------|
| sclk_o       | out       | std_logic         | SPI clock output                  |
| mosi_o       | out       | std_logic         | Master Out Slave In               |
| cs_b_o       | out       | std_logic         | Chip Select (active low)          |
| sclk_oe_o    | out       | std_logic         | SPI clock output enable           |
| mosi_oe_o    | out       | std_logic         | MOSI output enable                |
| cs_b_oe_o    | out       | std_logic         | CS output enable                  |
| tx_tready_o  | out       | std_logic         | TX ready                          |
| rx_tdata_o   | out       | std_logic_vector  | RX data                           |
| rx_tvalid_o  | out       | std_logic         | RX valid                          |
| cmd_tready_o | out       | std_logic         | Command ready                     |
| cfg_cpol_i   | in        | std_logic         | Clock polarity configuration      |

### Example Instantiation

```vhdl
inst_spi_master : spi_master
  port map (
    sclk_o       => sclk,
    mosi_o       => mosi,
    cs_b_o       => cs_n,
    sclk_oe_o    => sclk_oe,
    mosi_oe_o    => mosi_oe,
    cs_b_oe_o    => cs_b_oe,
    tx_tready_o  => tx_tready,
    rx_tdata_o   => rx_tdata,
    rx_tvalid_o  => rx_tvalid,
    cmd_tready_o => cmd_tready,
    cfg_cpol_i   => cfg_cpol
  );
```

---

## References

- [hdl/pbi_SPI.vhd](https://github.com/deuskane/asylum-communication-spi/blob/master/hdl/pbi_SPI.vhd)
- [hdl/spi_pkg.vhd](https://github.com/deuskane/asylum-communication-spi/blob/master/hdl/spi_pkg.vhd)
- [hdl/csr/SPI_csr.vhd](https://github.com/deuskane/asylum-communication-spi/blob/master/hdl/csr/SPI_csr.vhd)
- [hdl/spi_master.vhd](https://github.com/deuskane/asylum-communication-spi/blob/master/hdl/spi_master.vhd)

---

**Note:**  
Some results may be incomplete. For more details, check the [hdl directory on GitHub](https://github.com/deuskane/asylum-communication-spi/tree/master/hdl).  
If you need documentation for additional modules or a deeper description, let me know!