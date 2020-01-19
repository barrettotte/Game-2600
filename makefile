# DASM docs https://github.com/munsie/dasm/blob/master/doc/dasm.txt

CART = cart
OUTFMT = f3
VERBOSE = v0

all:
	dasm *.asm -$(OUTFMT) -$(VERBOSE) -o$(CART).bin
test:
	dasm test.asm -$(OUTFMT) -$(VERBOSE) -o$(CART).bin
run:
	stella $(CART).bin
