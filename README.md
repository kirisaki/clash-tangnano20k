# Clash Tang Nano 20K Example

This repository contains a **Clash (Haskell HDL)** sample project targeting the **Sipeed Tang Nano 20K** FPGA board.

- Generates Verilog from Clash and builds with the open-source Gowin toolchain  
- Demonstrates an interactive **LED controller** with button and UART input
- Includes comprehensive button debouncing, UART communication, and mode switching
- Well-organized, documented codebase following Haskell best practices

## Features

### Hardware Interface
- **6 LEDs**: Position display with automatic or manual advancement
- **2 Buttons**: Mode toggle (Auto/Manual) and manual advance
- **UART**: 115200 baud communication for remote control and status messages

### Functionality
- **Auto Mode**: LEDs advance automatically every second
- **Manual Mode**: LEDs advance only on button press or UART command
- **Button Control**: Physical buttons with hardware debouncing
- **UART Control**: Keyboard simulation via serial interface
  - Keys `1`, `A`, `a`: Toggle mode (same as Button 1)
  - Keys `2`, `Space`, `Enter`: Advance LED (same as Button 2)
- **Status Messages**: Automatic transmission of position changes ("CNT: N\r\n")

## Code Organization

The project is organized into clean, well-documented modules:

- **`app/Types.hs`**: All type definitions, constants, and utility functions
- **`app/Button.hs`**: Button debouncing and event detection logic  
- **`app/LED.hs`**: LED control state machine and mode management
- **`app/UART.hs`**: UART transmitter/receiver and message handling
- **`app/Main.hs`**: System integration and synthesis top entity

Each module follows consistent patterns:
- Clear separation of concerns
- Comprehensive documentation
- Type-safe hardware description
- Testable component design

## Getting Started

### Prerequisites

1. Install the [OSS CAD Suite](https://github.com/YosysHQ/oss-cad-suite-build)
   Make sure `yosys`, `nextpnr-himbaechel`, `gowin_pack`, and `openFPGALoader` are in your `PATH`

2. Install GHC and Cabal (recommended: via [GHCup](https://www.haskell.org/ghcup/))

### Building and Programming

1. **Test the design** (optional):
   ```console
   make test
   ```

2. **Build and load to SRAM** (temporary programming):
   ```console
   make sram
   ```

3. **Program the onboard flash** (persistent):
   ```console
   make flash
   ```

### Verification

If successful, you should observe:
- **LED**: Automatically advancing through positions 0-5 at 1Hz (Auto mode)
- **Button 1**: Toggles between Auto and Manual modes
- **Button 2**: Advances LED position in Manual mode
- **UART**: Sends "CNT: N\r\n" messages on position changes

### UART Communication

Connect a serial terminal (115200 baud, 8N1) to the UART pins:
- **RX**: Pin 70 (input to FPGA)
- **TX**: Pin 69 (output from FPGA)

Use keyboard commands to control the system remotely:
- `1`, `A`, `a`: Toggle operation mode
- `2`, `Space`, `Enter`: Advance LED position

## Development

### Build System

The Makefile provides several useful targets:
- `make check`: Verify configuration and show design hierarchy
- `make clean`: Remove build artifacts
- `make realclean`: Clean everything including generated Verilog
- `make test`: Run Haskell test suite

### Module Structure

Each hardware module follows a consistent pattern:
1. **Types**: Clear data type definitions with derived instances
2. **Constants**: Hardware-specific timing and configuration values
3. **State Machines**: Well-defined FSMs with clear state transitions
4. **Utility Functions**: Reusable helper functions and type conversions

### Testing

The project includes simulation-based testing:
- UART receiver verification with ideal waveforms
- Button event sequence testing  
- LED state machine validation

Run tests with: `cabal run clash-tangnano20k`

## Hardware Details

### Tang Nano 20K Connections

- **Clock**: 27MHz onboard oscillator
- **LEDs**: Pins 15-20 (active low)
- **Buttons**: Pins 87-88 (active low, pull-up enabled)
- **UART**: Pins 69-70 (TX/RX respectively)

### FPGA Resource Usage

The design uses minimal resources on the GW2AR-18C FPGA:
- Logic elements for state machines and counters
- Block RAM for message buffers
- I/O pins for LEDs, buttons, and UART

## License

This project is released under **dual licensing**:

* [0BSD](https://opensource.org/license/0bsd/) - For software components
* **ZDL** (Zero Design License) - For hardware designs

You may choose either license when using this code.