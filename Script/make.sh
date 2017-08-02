# Program Constants
BINARY_NAME="Main"
BINARY_DIR="Binary"
BINARY_EXT=".exe"
OBJECT_DIR="Object"
OBJECT_EXT=".o"
INTERFACE_DIR="Interface"
INTERFACE_EXT=".hi"

# Compile Binaries
ghc "$BINARY_NAME.hs"

# Move Interface Files
mkdir $INTERFACE_DIR
move "*$INTERFACE_EXT" $INTERFACE_DIR

# Move Object Files
mkdir $OBJECT_DIR
move "*$OBJECT_EXT" $OBJECT_DIR

# Move Binary Files
mkdir $BINARY_DIR
move "*$BINARY_EXT" $BINARY_DIR
