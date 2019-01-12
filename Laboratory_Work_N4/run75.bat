@ECHO OFF
SET /A LSIZE=512 - %~z1
SET /A ISIZE=37376
SET /A KSIZE=1474560 - 512 - 37376 - %~z2
ECHO F | XCOPY %1 image.img
FSUTIL FILE CREATENEW file.txt %LSIZE%
TYPE file.txt >> image.img
DEL file.txt
FSUTIL FILE CREATENEW file.txt %ISIZE%
TYPE file.txt >> image.img
DEL file.txt
FSUTIL FILE CREATENEW file.txt %KSIZE%
TYPE %2 >> image.img
TYPE file.txt >> image.img
DEL file.txt