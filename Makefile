# ==== Settings ===============================================================
# Clash Haskell source (where topEntity is defined)
HS_SRC      ?= app/Main.hs
# Clash-emitted top module name (Haskell側の t_name と一致させる)
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
.PHONY: all verilog wrapper synth pnr pack sram flash clean realclean check

all: sram

# ==== 1) Generate Verilog from Clash ========================================
$(VERILOG_DIR)/.stamp: $(HS_SRC)
	@mkdir -p $(VERILOG_DIR)
	cabal exec -- $(CLASH) --verilog -outputdir $(VERILOG_DIR) $(HS_SRC)
	@touch $(VERILOG_DIR)/.stamp

verilog: $(VERILOG_DIR)/.stamp

# ==== 2) Auto-generate wrapper (ties RST=0, EN=1, connects buttons) =========
$(VERILOG_DIR)/$(TOP)_wrapper.v: verilog
	@mkdir -p $(VERILOG_DIR)
	@echo "// Auto-generated wrapper: ties RST=0, EN=1, connects buttons"     >  $@
	@echo "module $(TOP) ("                                                   >> $@
	@echo "  input wire clk,"                                                 >> $@
	@echo "  input wire btn1,"                                                >> $@
	@echo "  input wire btn2,"                                                >> $@
	@echo "  output wire [5:0] LED"                                           >> $@
	@echo ");"                                                                >> $@
	@echo "  wire RST = 1'b0;"                                                >> $@
	@echo "  wire EN  = 1'b1;"                                                >> $@
	@echo "  // Buttons are active low (0 when pressed) so invert"                >> $@
	@echo "  wire BTN1 = ~btn1;"                                              >> $@
	@echo "  wire BTN2 = ~btn2;"                                              >> $@
	@echo "  $(CLASH_TOP) u (.CLK(clk), .RST(RST), .EN(EN), .BTN1(BTN1), .BTN2(BTN2), .LED(LED));" >> $@
	@echo "endmodule"                                                         >> $@

wrapper: $(VERILOG_DIR)/$(TOP)_wrapper.v

# ==== 3) Synthesis with Yosys ===============================================
$(BUILD_DIR)/top.json: verilog wrapper $(CST)
	@mkdir -p $(BUILD_DIR)
	@vfiles="$$(find $(VERILOG_DIR) -type f -name '*.v' | tr '\n' ' ')"; \
	echo "[INFO] Verilog files:" $$vfiles; \
	printf '%s\n' \
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

# ==== Utilities ==============================================================
check:
	@echo "== Clash top (expected) : $(CLASH_TOP)"
	@echo "== Wrapper top          : $(TOP)"
	@echo "== Verilog head         :"
	@/bin/sh -c 'head -n 3 $(VERILOG_DIR)/*.v 2>/dev/null || true'
	@echo "== Yosys design hierarchy (short) :"
	@/bin/sh -c 'vfiles="$$(find $(VERILOG_DIR) -type f -name "*.v" | tr "\n" " ")"; \
	  test -n "$$vfiles" && $(YOSYS) -p "read_verilog $$vfiles; stat" | sed -n "/=== design hierarchy ===/,\$$p" | head -n 40 || echo "(no verilog yet)"'

clean:
	@rm -rf $(BUILD_DIR) $(VERILOG_DIR)/$(TOP)_wrapper.v $(VERILOG_DIR)/.stamp

realclean: clean
	@rm -rf $(VERILOG_DIR)