#ML_OPTION=-c -Zdfi
#LINK_OPTION=/codeview

ML_OPTION=-c
LINK_OPTION=

all: turing.exe

turing.obj: turing.asm utils.ash screenio.ash tapeio.ash stateio.ash
        ml $(ML_OPTION) turing.asm

screenio.obj: screenio.asm screenio.ash
        ml $(ML_OPTION) screenio.asm

tapeio.obj: tapeio.asm tapeio.ash screenio.ash errorio.ash utils.ash
        ml $(ML_OPTION) tapeio.asm

stateio.obj: stateio.asm stateio.ash screenio.ash utils.ash errorio.ash
        ml $(ML_OPTION) stateio.asm

utils.obj: utils.asm utils.ash
        ml $(ML_OPTION) utils.asm

errorio.obj: errorio.asm errorio.ash screenio.ash turing.ash
        ml $(ML_OPTION) errorio.asm

fileio.obj: fileio.asm fileio.ash
        ml $(ML_OPTION) fileio.asm

turing.exe: turing.obj screenio.obj tapeio.obj stateio.obj utils.obj errorio.obj\
                fileio.obj
        link $(LINK_OPTION) turing screenio tapeio stateio utils errorio\
                fileio;


