#ifndef SPI_REGISTERS_H
#define SPI_REGISTERS_H

#include <stdint.h>

// Module      : SPI
// Description : CSR for SPI
// Width       : 8

//==================================
// Register    : data
// Description : Write : data to tansmit, Read : data to receive
// Address     : 0x0
//==================================
#define SPI_DATA 0x0

// Field       : data.value
// Description : Data TX or Data RX
// Range       : [7:0]
#define SPI_DATA_VALUE      0
#define SPI_DATA_VALUE_MASK 255

//==================================
// Register    : cmd
// Description : Command FIFO
// Address     : 0x1
//==================================
#define SPI_CMD 0x1

// Field       : cmd.nb_byte
// Description : NB Byte to TX/RX
// Range       : [4:0]
#define SPI_CMD_NB_BYTE      0
#define SPI_CMD_NB_BYTE_MASK 31

// Field       : cmd.last
// Description : 0 : not last cs keep active after transfer, 1 : last packet to transfer cs go inactive after transfer
// Range       : [5]
#define SPI_CMD_LAST      5
#define SPI_CMD_LAST_MASK 1

// Field       : cmd.enable_rx
// Description : if 1 then push receive byte into rx fifo else not
// Range       : [6]
#define SPI_CMD_ENABLE_RX      6
#define SPI_CMD_ENABLE_RX_MASK 1

// Field       : cmd.enable_tx
// Description : if 1 then mosi_oe_o is 1 else mosi_oe_o = 0
// Range       : [7]
#define SPI_CMD_ENABLE_TX      7
#define SPI_CMD_ENABLE_TX_MASK 1

//==================================
// Register    : cfg
// Description : SPI Configuration Register
// Address     : 0x2
//==================================
#define SPI_CFG 0x2

// Field       : cfg.spi_enable
// Description : 0 : Parity is even, 1 : Parity is odd
// Range       : [0]
#define SPI_CFG_SPI_ENABLE      0
#define SPI_CFG_SPI_ENABLE_MASK 1

// Field       : cfg.cpol
// Description : Clock Polarity
// Range       : [1]
#define SPI_CFG_CPOL      1
#define SPI_CFG_CPOL_MASK 1

// Field       : cfg.cpha
// Description : Clock Phase
// Range       : [2]
#define SPI_CFG_CPHA      2
#define SPI_CFG_CPHA_MASK 1

// Field       : cfg.loopback
// Description : 0 : MISO is connected to SPI MISO, 1 MISO is connected to MOSI
// Range       : [3]
#define SPI_CFG_LOOPBACK      3
#define SPI_CFG_LOOPBACK_MASK 1

//==================================
// Register    : prescaler
// Description : SPI Clock Prescaler. SCLK Frequency is CLK / 2*(prescaler+1)
// Address     : 0x3
//==================================
#define SPI_PRESCALER 0x3

// Field       : prescaler.ratio
// Description : Baud Tick Counter Max
// Range       : [7:0]
#define SPI_PRESCALER_RATIO      0
#define SPI_PRESCALER_RATIO_MASK 255

//----------------------------------
// Structure {module}_t
//----------------------------------
typedef struct {
  uint8_t data; // 0x0
  uint8_t cmd; // 0x1
  uint8_t cfg; // 0x2
  uint8_t prescaler; // 0x3
} SPI_t;

#endif // SPI_REGISTERS_H
