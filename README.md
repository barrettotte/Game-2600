# TBD

A game made for the Atari 2600 (VCS) to continue practicing with 6502 assembly.



## NTSC Video Frame
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
* Stella Emulator https://stella-emu.github.io/
* DASM Macro Assembler https://github.com/munsie/dasm
