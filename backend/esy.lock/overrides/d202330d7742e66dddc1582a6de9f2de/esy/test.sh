pwd
mkdir -p _test
cd _test

if which x86_64-w64-mingw32-gcc; then
    CC=x86_64-w64-mingw32-gcc
else
    CC=gcc
fi

echo "Using compiler: $CC"

$CC ./../../esy/test.c -o ./test.exe $LIBEV_CFLAGS $LIBEV_LIBS

echo "Test executable path:"
ls -a .

./test.exe
