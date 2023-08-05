nasm -Z game.err game.asm -o game.obj -f obj
nasm -Z sound.err sound.asm -o sound.obj -f obj
nasm -Z timer.err timer.asm -o timer.obj -f obj

tlink game.obj sound.obj timer.obj
game.exe