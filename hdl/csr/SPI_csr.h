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

// Field       : cmd.nb_bytes
// Description : Transfert Length in bytes (N+1)
// Range       : [1:0]
#define SPI_CMD_NB_BYTES      0
#define SPI_CMD_NB_BYTES_MASK 3

// Field       : cmd.size
// Description : Transfert Size : 1/2/4/8
// Range       : [3:2]
#define SPI_CMD_SIZE      2
#define SPI_CMD_SIZE_MASK 3

// Enum        : cmd.size.SINGLE
// Description : SPI SINGLE Legacy (MISO/MOSI) mode
#define SPI_CMD_SIZE_SINGLE     0
#define SPI_CMD_SIZE_SINGLE_RAW (0<<2)

// Enum        : cmd.size.DUAL
// Description : SPI DUAL (2b) mode
#define SPI_CMD_SIZE_DUAL     1
#define SPI_CMD_SIZE_DUAL_RAW (1<<2)

// Enum        : cmd.size.QUAD
// Description : SPI QUAD (4b) mode
#define SPI_CMD_SIZE_QUAD     2
#define SPI_CMD_SIZE_QUAD_RAW (2<<2)

// Enum        : cmd.size.OCTAL
// Description : SPI OCTAL (8b) mode
#define SPI_CMD_SIZE_OCTAL     3
#define SPI_CMD_SIZE_OCTAL_RAW (3<<2)

// Field       : cmd.enable_rx
// Description : Push in RX FIFO - 0 : don't push in RX FIFO, 1 : push in RX FIFO when receive byte
// Range       : [4]
#define SPI_CMD_ENABLE_RX      4
#define SPI_CMD_ENABLE_RX_MASK 1

// Enum        : cmd.enable_rx.DISABLE
// Description : don't push in RX FIFO
#define SPI_CMD_ENABLE_RX_DISABLE     0
#define SPI_CMD_ENABLE_RX_DISABLE_RAW (0<<4)

// Enum        : cmd.enable_rx.ENABLE
// Description : push in RX FIFO when receive byte
#define SPI_CMD_ENABLE_RX_ENABLE     1
#define SPI_CMD_ENABLE_RX_ENABLE_RAW (1<<4)

// Field       : cmd.enable_tx
// Description : POP from TX FIFO - 0 : don't pop TX FIFO and keep mosi_oe to 0, 1 pop TX FIFO and mosi_oe_o is 1 during the transfert
// Range       : [5]
#define SPI_CMD_ENABLE_TX      5
#define SPI_CMD_ENABLE_TX_MASK 1

// Enum        : cmd.enable_tx.DISABLE
// Description : don't pop TX FIFO and keep mosi_oe to 0
#define SPI_CMD_ENABLE_TX_DISABLE     0
#define SPI_CMD_ENABLE_TX_DISABLE_RAW (0<<5)

// Enum        : cmd.enable_tx.ENABLE
// Description : pop TX FIFO and mosi_oe_o is 1 during the transfert
#define SPI_CMD_ENABLE_TX_ENABLE     1
#define SPI_CMD_ENABLE_TX_ENABLE_RAW (1<<5)

// Field       : cmd.last
// Description : Last Transfert - 0 : not last cs keep active after transfer, 1 : last packet to transfer cs go inactive after transfer. SPECIAL CASE if last = enable_rx = enable_tx = 0 then stop the transfert
// Range       : [6]
#define SPI_CMD_LAST      6
#define SPI_CMD_LAST_MASK 1

// Enum        : cmd.last.DISABLE
// Description : not last cs keep active after transfer
#define SPI_CMD_LAST_DISABLE     0
#define SPI_CMD_LAST_DISABLE_RAW (0<<6)

// Enum        : cmd.last.ENABLE
// Description : last packet to transfer cs go inactive after transfer. SPECIAL CASE if last = enable_rx = enable_tx = 0 then stop the transfert
#define SPI_CMD_LAST_ENABLE     1
#define SPI_CMD_LAST_ENABLE_RAW (1<<6)

// Field       : cmd.cfg
// Description : Configuration - 0 : configure enable_tx/enable_rx and size, 1 : replace enable_tx/enable_rx and size by nb_bytes[5:2]
// Range       : [7]
#define SPI_CMD_CFG      7
#define SPI_CMD_CFG_MASK 1

// Enum        : cmd.cfg.CONFIG
// Description : configure enable_tx/enable_rx and size
#define SPI_CMD_CFG_CONFIG     0
#define SPI_CMD_CFG_CONFIG_RAW (0<<7)

// Enum        : cmd.cfg.KEEP
// Description : replace enable_tx/enable_rx and size by nb_bytes[5:2]
#define SPI_CMD_CFG_KEEP     1
#define SPI_CMD_CFG_KEEP_RAW (1<<7)

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
// Structure SPI_t
//----------------------------------
typedef struct {
  uint8_t data; // 0x0
  uint8_t cmd; // 0x1
  uint8_t cfg; // 0x2
  uint8_t prescaler; // 0x3
} SPI_t;

#endif // SPI_REGISTERS_H
