# Clash Tang Nano 20K Example

This repository contains a **Clash (Haskell HDL)** sample project targeting the **Sipeed Tang Nano 20K** FPGA board.

- Generates Verilog from Clash and builds with the open-source Gowin toolchain.  
- Demonstrates the classic **LED blinky** ("L-Chika") design.  
- Intended as a simple starting point for experimenting with Clash on real hardware.  

## Getting Started

1. Install the [OSS CAD Suite](https://github.com/YosysHQ/oss-cad-suite-build).  
  Make sure `yosys`, `nextpnr-himbaechel`, `gowin_pack`, and `openFPGALoader` are in your `PATH`.

2. Build and load to SRAM (temporary programming):

  ```bash
   make sram
  ````

3. To program the onboard flash (persistent):

   ```bash
   make flash
  ```

If successful, the onboard LED will **blink** at about 1 Hz.

## License

This project is released under **dual licensing**:

* [0BSD](https://opensource.org/license/0bsd/)
* **ZDL** (Zero Design License)

You may choose either license when using this code.
