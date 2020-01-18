# Game-2600

Some fun messing around with Atari 2600 (VCS) to strengthen my 6502 assembly knowledge in a fun way.


## Game
The player collects the ball and dodges the enemy that randomly spawns and moves up or down.

That's it.

So, this isn't quite a game; its more of a screw around project I kicked around.

I really wanted to do more with this. 
But, truthfully I just don't feel like working on this anymore. 
Its time for this to go on the shelf.


## Stella Emulator Controls
* Arrow keys - joystick
* Spacebar   - fire
* F2 - reset
* F3 - Color TV
* F4 - Black and white TV
* PAUSE - pause emulator


## Unimplemented Ideas
* Player and enemy projectiles
* Basic pathfinding for Enemy
* Difficulty switches increase enemy speed
* Random generated playfield obstacles


## Notes


#### Memory Map
```
+---------------------------+
|       TIA Registers       | $00-$7F
+---------------------------+
|         PIA RAM           | $80-$FF
+---------------------------+
|           ???             |
+---------------------------+
|    PIA Ports and Timer    | $280-$297
+---------------------------+
|           ???             |
+---------------------------+
|       Cartridge ROM       | $F000-$FFFF
+---------------------------+
```

<br>

#### NTSC Video Frame
```

+-------------------------------------------------+        <-----+
|                 Vertical Sync                   |   3 lines    |
|                                                 |              |
+-------------------------------------------------+              |
|                 Vertical Blank                  |  37 lines    |
|                                                 |              |
+--------------------+----------------------------+              +-- 262 lines
|    Horiz.          |           Visible          |              |
|    Blank           |            Frame           | 192 lines    |
|                    |  <-- 160 color clocks -->  |              |
+--------------------+----------------------------+              |
|                    Overscan                     |  30 lines    |
|                                                 |              |
+-------------------------------------------------+        <-----+

|--------------  228 color clocks ----------------|
|---------------  76  CPU cycles -----------------|

```


## References
**Main reference**: https://atariage.com/forums/profile/3056-spiceware/ . This guy is absolutely incredible with 6502 asm.


* 6502 instruction set with cycle counts - https://www.masswerk.at/6502/6502_instruction_set.html
* Atari 2600 killer hacks - https://atariage.com/forums/topic/71120-6502-killer-hacks/page/3/?tab=comments#comment-1071314
  * ```.byte $2C``` - "skip 2 bytes" pseudo-instruction (ABS BIT)
* Counting cycles on Atari 2600 - https://www.randomterrain.com/atari-2600-memories-guide-to-cycle-counting.html
* DASM Macro Assembler - https://github.com/munsie/dasm
* Introduction to Atari2600 - https://www.randomterrain.com/
* Sprite ASM Generator - https://alienbill.com/2600/playerpalnext.html
* Stella Emulator - https://stella-emu.github.io/
* Unofficial 6502 opcodes - https://wiki.nesdev.com/w/index.php/Programming_with_unofficial_opcodes
  * ```dcp $0F``` - decrement at memory address and compare with accumulator
