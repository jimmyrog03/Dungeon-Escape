# Dungeon Escape - An Assembly Game
Dungeon Escape was a semester long school project written entirely in assembly language for my Microprocessor Systems Lab at West Virginia University. 
The provided files should contain everything needed to run the game yourself, as well as additional prototype files that shows the development of the game throughout the semester. Some files are also included that were not used in the final version of the game, but provide suggested improvements that could be continued in the future.

> The following instructions should be suitable to run the game on any Windows 10/11 Systems.

Setup:
  - Clone repository to location of choice.
  - In folder **_Dungeon-Escape-An-Assembly-Game_**, right click on folder **_GAME_** and choose option **copy as path**. 
  - Go to **_DOSBox_** folder and edit "**_DOSBox 0.74-3 Options_**" in any notepad editor.
  - Scroll to the bottom of file **_dosbox-0.74-3.conf_** that opens.
  - Under the line that says "**# You can put your MOUNT lines here.**", type `mount c ` and then paste the path that you copied for the **_GAME_** folder on the same line.
  - Under that line, type `c://`, followed on the next line by `GAME.EXE`

Should look similar to

````
# You can put your MOUNT lines here.
mount c C:\Users\username\Documents\GitHub\Dungeon-Escape-An-Assembly-Game\GAME
c://
GAME.EXE
````

- In the same file, scroll to the top where there are options for setting fullscreen, resolutions, and other settings.
- I recommend using the following settings for this block to avoid stretching the game window, but you adjust these values as you see fit.
  
````
fullscreen=false
fulldouble=false
fullresolution=original
windowresolution=1600x900
output=openglnb
autolock=true
sensitivity=100
waitonerror=true
priority=higher,normal
mapperfile=mapper-0.74-3.map
usescancodes=true
````
- Save this file and now run the **DOSBox** application from the **_DOSBox_** folder.
- The game should run automatically from here and you can begin playing!

