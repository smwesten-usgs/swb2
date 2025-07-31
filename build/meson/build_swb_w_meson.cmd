:: point Meson to new LLVM versions of intel compilers on Windows
set FC=ifx
set CC=icx
set CXX=icx

rmdir /S /Q builddir
mkdir builddir
meson setup builddir ..\.. && cd builddir && meson compile
cd src
copy /Y swb2.exe ..\..\..\..\bin\win_x64\swb2.exe
copy /Y swbstats2.exe ..\..\..\..\bin\win_x64\swbstats2.exe

copy /Y swb2.exe d:\bin\swb2.exe
copy /Y swbstats2.exe d:\bin\swbstats2.exe
cd ..\test\unit_tests
copy /Y swbtest.exe ..\..\..\..\..\test\unit_tests\swbtest.exe