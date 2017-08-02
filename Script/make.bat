:: Program Constants
set BINARY_NAME="Main"
set BINARY_DIR="Binary"
set BINARY_EXT=".exe"
set OBJECT_DIR="Object"
set OBJECT_EXT=".o"
set INTERFACE_DIR="Interface"
set INTERFACE_EXT=".hi"

:: Compile Binaries
ghc %BINARY_NAME%.hs

:: Move Interface Files
mkdir %INTERFACE_DIR%
move *%INTERFACE_EXT% %INTERFACE_DIR%

:: Move Object Files
mkdir %OBJECT_DIR%
move *%OBJECT_EXT% %OBJECT_DIR%

:: Move Binary Files
mkdir %BINARY_DIR%
move *%BINARY_EXT% %BINARY_DIR%
