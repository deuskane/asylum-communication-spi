# SPI
CSR for SPI

| Address | Registers |
|---------|-----------|
|0x0|data|
|0x1|cmd|
|0x2|cfg|
|0x3|prescaler|

## 0x0 data
Write : data to tansmit, Read : data to receive

### [7:0] value
Data TX or Data RX

## 0x1 cmd
Command FIFO

### [4:0] nb_byte
NB Byte to TX/RX

### [5:5] last
0 : not last cs keep active after transfer, 1 : last packet to transfer cs go inactive after transfer

### [6:6] enable_rx
if 1 then push receive byte into rx fifo else not

### [7:7] enable_tx
if 1 then mosi_oe_o is 1 else mosi_oe_o = 0

## 0x2 cfg
SPI Configuration Register

### [0:0] spi_enable
0 : Parity is even, 1 : Parity is odd

### [1:1] cpol
Clock Polarity

### [2:2] cpha
Clock Phase

### [3:3] loopback
0 : MISO is connected to SPI MISO, 1 MISO is connected to MOSI

### [7:4] cs
Chip Select

## 0x3 prescaler
SPI Clock Prescaler. SCLK Frequency is CLK / 2*(prescaler+1)

### [7:0] ratio
Baud Tick Counter Max

