# SPI
CSR for SPI

| Address | Registers |
|---------|-----------|
|0x0|data|
|0x1|cmd, status|
|0x2|cfg|
|0x3|prescaler|

## 0x0 data
Write : data to tansmit, Read : data to receive

### [7:0] value
Data TX or Data RX

## 0x1 cmd
Command FIFO

### [1:0] nb_bytes
Transfert Length in bytes (N+1)

### [3:2] size
Transfert Size : 1/2/4/8

### [4:4] enable_rx
Push in RX FIFO - 0 : don't push in RX FIFO, 1 : push in RX FIFO when receive byte

### [5:5] enable_tx
POP from TX FIFO - 0 : don't pop TX FIFO and keep mosi_oe to 0, 1 pop TX FIFO and mosi_oe_o is 1 during the transfert

### [6:6] last
Last Transfert - 0 : not last cs keep active after transfer, 1 : last packet to transfer cs go inactive after transfer. SPECIAL CASE if last = enable_rx = enable_tx = 0 then stop the transfert

### [7:7] cfg
Configuration - 0 : configure enable_tx/enable_rx and size, 1 : replace enable_tx/enable_rx and size by nb_bytes[5:2]

## 0x2 cfg
SPI Configuration Register

### [0:0] spi_enable
0 : SPI master is disabled (held in reset), 1 : SPI master is enabled

### [1:1] cpol
Clock Polarity

### [2:2] cpha
Clock Phase

### [3:3] loopback
0 : MISO is connected to SPI MISO, 1 MISO is connected to MOSI

## 0x3 prescaler
SPI Clock Prescaler. SCLK Frequency is CLK / 2*(prescaler+1)

### [7:0] ratio
Baud Tick Counter Max

## 0x1 status
SPI Status Register (read only, writes are ignored). data and cmd are blocking FIFO registers : check tx_full / cmd_full before a write and rx_empty before a read to never stall the bus

### [0:0] tx_empty
1 : TX FIFO (data write) is empty

### [1:1] tx_full
1 : TX FIFO (data write) is full, a write to data waits until the SPI master pops a byte (always 1 with DEPTH_TX=0 outside the master handshake)

### [2:2] rx_empty
1 : RX FIFO (data read) is empty, a read of data waits until the SPI master pushes a byte

### [3:3] rx_full
1 : RX FIFO (data read) is full, the SPI master waits for a read of data before receiving the next byte

### [4:4] cmd_empty
1 : Command FIFO is empty

### [5:5] cmd_full
1 : Command FIFO is full, a write to cmd waits until the SPI master pops a command (always 1 with DEPTH_CMD=0 outside the master handshake)

### [6:6] busy
1 : SPI transaction in progress (CS# active or SPI master not idle) or command pending, 0 : all commands are done and CS# is released

