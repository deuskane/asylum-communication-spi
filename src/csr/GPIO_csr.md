# GPIO
CSR for General Purpose I/O

| Address | Registers |
|---------|-----------|
|0x0|data|
|0x1|data_oe|
|0x2|data_in|
|0x3|data_out|

## 0x0 data
data - with data_oe mask apply

### [7:0] value
Data with data_oe with mask apply

## 0x1 data_oe
GPIO Direction

### [7:0] value
GPIO Direction : 0 input, 1 output

## 0x2 data_in
GPIO Input

### [7:0] value
Input Data of GPIO

## 0x3 data_out
GPIO Output

### [7:0] value
Output Data of GPIO

