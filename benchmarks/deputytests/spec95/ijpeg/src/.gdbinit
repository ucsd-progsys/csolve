# .gdbinit
file ijpeg.exe
#set args -image_file ../data/ref/input/penguin.ppm -GO
set args -image_file ../data/ref/input/vigo.ppm -GO
break main
run
