:: point Meson to new LLVM versions of intel compilers on Windows

del /S /Q builddir
rmdir builddir
mkdir builddir

set LATEST_NETCDF="c:\Program Files\netCDF 4.9.3\bin"

set FC=gfortran && set CC=gcc && set CXX=g++ && meson setup -Dnetcdf_root=%LATEST_NETCDF% builddir ..\.. && cd builddir && meson compile
cd src
copy swb2.exe d:\bin\swb2.exe
copy swbstats2.exe d:\bin\swbstats2.exe

cd ..\test\unit_tests
copy swbtest.exe ..\..\..\..\..\test\unit_tests\swbtest.exe