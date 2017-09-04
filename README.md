bvsmGol
=========
NES "Simpsons, The - Bart Vs. the Space Mutants (E) [!].nes" text tool. Game uses Golomb coding for text. This tool can work with it.


Synopsis:
```
                       
bvsmGol -d <file>  Decompress script from given ROM file to script.txt.    
bvsmGol -c <file>  Compress script.txt and inssert in given ROM file.           
            
Options:
-h   show help  
-v   show version  
```
Script is best viewed with Notepad++ with Language > C# (or similar) highlighting enabled. Encoding and decoding tables should be in files encode.tbl and decode.tbl 

Actual compression scheme is relatively simple. Compressed stream is a stream of nybbles. Each nybble is either index of frequency sorted alphabet or expansion command (0xE or 0xF). The next nybble after expansion command is an index in further part of alphabet.
```
0-0xD - idx in alph0
0xE XXXX - idx = 0xE+YYYY, YYYY- idx in alph1
0xF XXXX - idx = 0x1E+YYYY, YYYY - idx in alph2


(alph0) (alph1) (alph2)
  0-D    size F  size F
```
So 0xE most frequent chars are encoded by 4 bits, less frequent 0x20 chars are encoded by 8 bits. Char map can be then 0x2E characters, but due to game's feature of screen clearing only 0x2B characters available. 