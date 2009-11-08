SRCS=\
  Time \
  NewPairs \
  NewAddPol \
  NewRed \
  NewUtil \
  og \
  cmu/src/cmump/Madd \
  cmu/src/cmump/Mdiv \
  cmu/src/cmump/Mgcd \
  cmu/src/cmump/Mout \
  cmu/src/cmump/Mmult \
  cmu/src/cmump/MExpo \
  cmu/src/cmump/Mutil \
  cmu/src/cmump/Mfloat \
  cmu/src/cmump/Mfatal \
  cmu/src/cmump/Mstrtoul \
  cmu/src/multpol/MPolAdd \
  cmu/src/multpol/MPolIn \
  cmu/src/multpol/MPolOut \
  cmu/src/multpol/MPolFree \
  cmu/src/multpol/MPolCopy

HDRS=include/multpol include/cmump include/gbas
LEXS=cmu/src/multpol/lexpol
YACCS=cmu/src/multpol/translate

TESTS="eg/eg01 eg/eg80 eg/eg81 eg/eg97 eg/eg98 eg/eg99"
#TESTS="eg/eg01 eg/eg02 eg/eg03 eg/eg04 eg/eg05 eg/eg80 eg/eg81 eg/eg97 eg/eg98 eg/eg99"
TEST=$(TESTS)
