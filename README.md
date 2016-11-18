#Chip8

A [Chip8](https://en.wikipedia.org/wiki/CHIP-8) emulator implemented in Haskell


## Installation

Requirements:
  * SDL 1.2 development files
  * cabal-install

Clone or download the repository.

    git clone https://github.com/mgbarotto/Chip8.git
Move into the repository's directory
   
    cd Chip8
Install using cabal
    
    cabal install 
    sudo ln -s ~/.cabal/bin/Chip8 /usr/bin/
## Usage
    Chip8 romfile [debug]

The Chip8 keyboard is composed of 16 keys, mapped to:

    |1 2 3 4|  
    |q w e r|  
    |a s d f|  
    |z x c v|  

The keys that are actually used depend on the ROM and should be discovered through experimentation.

## Some ROMs
 [Zophar's ROMs](http://www.zophar.net/pdroms/chip8/chip-8-games-pack.html)

 More ROMs can be found for free around the internet


## License

This project is licensed under the GPL License - see the [LICENSE](LICENSE) file for details

## Sources

[SDL Haskel bindings](https://hackage.haskell.org/package/SDL)

[aartamonau's Haskell reimplementation of Lazyfoo's SDL tutorials](https://github.com/aartamonau/lazyfoo)

[Cowgod's Chip-8 Technical Reference v1.0](http://devernay.free.fr/hacks/chip8/C8TECH10.HTM)

