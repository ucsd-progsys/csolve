# .gdbinit

file anagram.exe
set args words
break main
run words <input.orig.in
