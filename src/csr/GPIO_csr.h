#ifndef GPIO_REGISTERS_H
#define GPIO_REGISTERS_H

#include <stdint.h>

// Module      : GPIO
// Description : CSR for General Purpose I/O
// Width       : 8

//==================================
// Register    : data
// Description : data - with data_oe mask apply
// Address     : 0x0
//==================================
#define GPIO_DATA 0x0

// Field       : data.value
// Description : Data with data_oe with mask apply
// Range       : [7:0]
#define GPIO_DATA_VALUE      0
#define GPIO_DATA_VALUE_MASK 255

//==================================
// Register    : data_oe
// Description : GPIO Direction
// Address     : 0x1
//==================================
#define GPIO_DATA_OE 0x1

// Field       : data_oe.value
// Description : GPIO Direction : 0 input, 1 output
// Range       : [7:0]
#define GPIO_DATA_OE_VALUE      0
#define GPIO_DATA_OE_VALUE_MASK 255

//==================================
// Register    : data_in
// Description : GPIO Input
// Address     : 0x2
//==================================
#define GPIO_DATA_IN 0x2

// Field       : data_in.value
// Description : Input Data of GPIO
// Range       : [7:0]
#define GPIO_DATA_IN_VALUE      0
#define GPIO_DATA_IN_VALUE_MASK 255

//==================================
// Register    : data_out
// Description : GPIO Output
// Address     : 0x3
//==================================
#define GPIO_DATA_OUT 0x3

// Field       : data_out.value
// Description : Output Data of GPIO
// Range       : [7:0]
#define GPIO_DATA_OUT_VALUE      0
#define GPIO_DATA_OUT_VALUE_MASK 255

//----------------------------------
// Structure {module}_t
//----------------------------------
typedef struct {
  uint8_t data; // 0x0
  uint8_t data_oe; // 0x1
  uint8_t data_in; // 0x2
  uint8_t data_out; // 0x3
} GPIO_t;

#endif // GPIO_REGISTERS_H
