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

### [4:0] nb_bytes
Transfert Length in bytes

### [5:5] last
Last Transfert - 0 : not last cs keep active after transfer, 1 : last packet to transfer cs go inactive after transfer. SPECIAL CASE if last = enable_rx = enable_tx = 0 then stop the transfert

### [6:6] enable_rx
Push in RX FIFO - 0 : don't push in RX FIFO, 1 : push in RX FIFO when receive byte

### [7:7] enable_tx
POP from TX FIFO - 0 : don't pop TX FIFO and keep mosi_oe to 0, 1 pop TX FIFO and mosi_oe_o is 1 during the transfert

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

## 0x3 prescaler
SPI Clock Prescaler. SCLK Frequency is CLK / 2*(prescaler+1)

### [7:0] ratio
Baud Tick Counter Max

