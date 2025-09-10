# ==== Settings ===============================================================
# Clash Haskell sources (updated module list)
HS_SRC      ?= app/Main.hs app/Types.hs app/Button.hs app/LED.hs app/UART.hs

# Clash-emitted top module name (matches t_name in Haskell)
CLASH_TOP   ?= blinky

# Wrapper top name (ties RST=0, EN=1)
TOP         ?= top
BUILD_DIR   ?= build
VERILOG_DIR ?= verilog

# Gowin / Tang Nano 20K settings
DEVICE      ?= GW2AR-LV18QN88C8/I7
BOARD       ?= tangnano20k
CST         ?= tangnano20k.cst

# Toolchain binaries (assume oss-cad-suite in PATH)
CLASH         ?= clash
OSSCAD        ?= $(HOME)/tools/oss-cad-suite
YOSYS         ?= $(OSSCAD)/bin/yosys
NEXTPNR       ?= $(OSSCAD)/bin/nextpnr-himbaechel
GOWIN_PACK    ?= $(OSSCAD)/bin/gowin_pack
LOADER        ?= $(OSSCAD)/bin/openFPGALoader

# ==== Phony targets ==========================================================
.PHONY: all verilog wrapper synth pnr pack sram flash clean realclean check test

all: sram

# ==== 1) Generate Verilog from Clash ========================================
$(VERILOG_DIR)/.stamp: $(HS_SRC)
	@mkdir -p $(VERILOG_DIR)
	cabal exec -- $(CLASH) --verilog -outputdir $(VERILOG_DIR) -iapp app/Main.hs
	@touch $(VERILOG_DIR)/.stamp

verilog: $(VERILOG_DIR)/.stamp

# ==== 2) Auto-generate wrapper (ties RST=0, EN=1, connects buttons and UART) ===
$(VERILOG_DIR)/$(TOP)_wrapper.v: verilog
	@mkdir -p $(VERILOG_DIR)
	@echo "// Auto-generated wrapper for $(CLASH_TOP)" >  $@
	@echo "// Ties RST=0, EN=1, connects buttons and UART" >> $@
	@echo "module $(TOP) ("                                 >> $@
	@echo "  input wire clk,"                               >> $@
	@echo "  input wire btn1,"                              >> $@
	@echo "  input wire btn2,"                              >> $@
	@echo "  input wire UART_RX,"                           >> $@
	@echo "  output wire [5:0] LED,"                        >> $@
	@echo "  output wire UART_TX"                           >> $@
	@echo ");"                                              >> $@
	@echo "  // System control signals"                     >> $@
	@echo "  wire RST = 1'b0;"                              >> $@
	@echo "  wire EN  = 1'b1;"                              >> $@
	@echo "  // Button inversion (active low hardware -> active high logic)" >> $@
	@echo "  wire BTN1 = ~btn1;"                            >> $@
	@echo "  wire BTN2 = ~btn2;"                            >> $@
	@echo "  // Instantiate Clash-generated module"         >> $@
	@echo "  $(CLASH_TOP) u_$(CLASH_TOP) ("                 >> $@
	@echo "    .CLK(clk),"                                  >> $@
	@echo "    .RST(RST),"                                  >> $@
	@echo "    .EN(EN),"                                    >> $@
	@echo "    .BTN1(BTN1),"                                >> $@
	@echo "    .BTN2(BTN2),"                                >> $@
	@echo "    .UART_RX(UART_RX),"                          >> $@
	@echo "    .LED(LED),"                                  >> $@
	@echo "    .UART_TX(UART_TX)"                           >> $@
	@echo "  );"                                            >> $@
	@echo "endmodule"                                       >> $@

wrapper: $(VERILOG_DIR)/$(TOP)_wrapper.v

# ==== 3) Synthesis with Yosys ===============================================
$(BUILD_DIR)/top.json: verilog wrapper $(CST)
	@mkdir -p $(BUILD_DIR)
	@vfiles="$$(find $(VERILOG_DIR) -type f -name '*.v' | tr '\n' ' ')"; \
	echo "[INFO] Synthesizing with Verilog files: $$vfiles"; \
	printf '%s\n' \
	  "# Clash Tang Nano 20K synthesis script" \
	  "read_verilog $$vfiles" \
	  "hierarchy -check -top $(TOP)" \
	  "proc; opt; fsm; opt; memory; opt" \
	  "synth_gowin -top $(TOP)" \
	  "write_json $(BUILD_DIR)/top.json" \
	  > $(BUILD_DIR)/synth.ys
	$(YOSYS) -q -s $(BUILD_DIR)/synth.ys

synth: $(BUILD_DIR)/top.json

# ==== 4) Place & Route with nextpnr =========================================
$(BUILD_DIR)/pnr.json: $(BUILD_DIR)/top.json $(CST)
	$(NEXTPNR) --json $(BUILD_DIR)/top.json \
	  --device $(DEVICE) \
	  --vopt family=GW2A-18C \
	  --vopt cst=$(CST) \
	  --write $(BUILD_DIR)/pnr.json

pnr: $(BUILD_DIR)/pnr.json

# ==== 5) Pack bitstream (.fs) ===============================================
$(BUILD_DIR)/top.fs: $(BUILD_DIR)/pnr.json
	$(GOWIN_PACK) -d GW2A-18C -o $(BUILD_DIR)/top.fs $(BUILD_DIR)/pnr.json

pack: $(BUILD_DIR)/top.fs

# ==== 6a) SRAM load (temporary programming) =================================
sram: $(BUILD_DIR)/top.fs
	$(LOADER) -b $(BOARD) $(BUILD_DIR)/top.fs

# ==== 6b) Flash programming (persistent) ====================================
flash: $(BUILD_DIR)/top.fs
	$(LOADER) -b $(BOARD) -f $(BUILD_DIR)/top.fs

# ==== Testing =================================================================
test:
	@echo "Running Clash test suite..."
	cabal run clash-tangnano20k

# ==== Utilities ==============================================================
check:
	@echo "== Build Configuration Check =="
	@echo "Clash top module      : $(CLASH_TOP)"
	@echo "Wrapper top module    : $(TOP)"
	@echo "Target device         : $(DEVICE)"
	@echo "Constraint file       : $(CST)"
	@echo "Haskell sources       : $(HS_SRC)"
	@echo ""
	@echo "== Generated Verilog Files =="
	@/bin/sh -c 'ls -la $(VERILOG_DIR)/*.v 2>/dev/null || echo "No Verilog files generated yet"'
	@echo ""
	@echo "== Design Hierarchy Preview =="
	@/bin/sh -c 'if [ -d "$(VERILOG_DIR)" ]; then \
	  vfiles="$$(find $(VERILOG_DIR) -type f -name "*.v" | tr "\n" " ")"; \
	  test -n "$$vfiles" && $(YOSYS) -p "read_verilog $$vfiles; hierarchy -check -top $(TOP); stat" 2>/dev/null | \
	  sed -n "/=== design hierarchy ===/,\$$p" | head -n 20 || echo "Cannot analyze hierarchy yet"; \
	else echo "No Verilog directory found"; fi'

clean:
	@rm -rf $(BUILD_DIR) $(VERILOG_DIR)/$(TOP)_wrapper.v $(VERILOG_DIR)/.stamp
	@echo "Build artifacts cleaned"

realclean: clean
	@rm -rf $(VERILOG_DIR)
	@cabal clean
	@echo "All generated files cleaned"