# Defender-2600

A super basic game made for the Atari 2600 (VCS) to strengthen my assembly knowledge in a fun way.

I didn't want to make anything too crazy that I would spend weeks on.
I was just genuinely curious what all goes into making a game with this architecture.


## References
**Main reference**: https://atariage.com/forums/profile/3056-spiceware/ . This guy is absolutely incredible with 6502 asm.


* 6502 instruction set with cycle counts - http://nesdev.com/6502_cpu.txt
* Atari 2600 killer hacks - https://atariage.com/forums/topic/71120-6502-killer-hacks/page/3/?tab=comments#comment-1071314
  * ```.byte $2C``` - "skip 2 bytes" pseudo-instruction (ABS BIT)
* Counting cycles on Atari 2600 - https://www.randomterrain.com/atari-2600-memories-guide-to-cycle-counting.html
* DASM Macro Assembler - https://github.com/munsie/dasm
* Introduction to Atari2600 - https://www.randomterrain.com/
* Sprite ASM Generator - https://alienbill.com/2600/playerpalnext.html
* Stella Emulator - https://stella-emu.github.io/
* Unofficial 6502 opcodes - https://wiki.nesdev.com/w/index.php/Programming_with_unofficial_opcodes
  * ```dcp $0F``` - decrement at memory address and compare with accumulator
