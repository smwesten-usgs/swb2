@echo off
:: remove existing Cmake cache and directories
del /F /Q CMakeCache.*
rmdir /S /Q CMakeFiles
rmdir /S /Q src
rmdir /S /Q Testing
rmdir /S /Q tests
del /S /Q *.txt

:: set CMAKE-related and build-related variables
set CMAKEROOT=C:\Program Files\CMake\
set COMPILER_DIR=C:\mingw64
set Fortran_COMPILER_NAME=gfortran
set CMAKE_C_COMPILER=gcc

set MAKE_EXECUTABLE_NAME=mingw32-make.exe
set R_HOME=C:\Program Files\R\R-3.3.1\bin

:: explicitly locate each key library
for /f %%x in ('dir /b /s c:\mingw64\*libhdf5_hl.a') do call set LIB_HDF5_HL=%%x
for /f %%x in ('dir /b /s c:\mingw64\*libhdf5.a') do call set LIB_HDF5=%%x
for /f %%x in ('dir /b /s c:\mingw64\*libsz.a') do call set LIB_SZ=%%x
for /f %%x in ('dir /b /s c:\mingw64\*libdl.a') do call set LIB_DL=%%x
for /f %%x in ('dir /b /s c:\mingw64\*libz.a') do call set LIB_Z=%%x
for /f %%x in ('dir /b /s c:\mingw64\*libnetcdf.a') do call set LIB_NETCDF=%%x
for /f %%x in ('dir /b /s c:\mingw64\*libgcc.a') do call set LIB_GCC=%%x
for /f %%x in ('dir /b /s c:\mingw64\*libgfortran.a') do call set LIB_GFORTRAN=%%x

:: substitute forward slash for backward slash
set LIB_HDF5_HL=%LIB_HDF5_HL:\=/%
set LIB_HDF5=%LIB_HDF5:\=/%
set LIB_NETCDF=%LIB_NETCDF:\=/%
set LIB_GCC=%LIB_GCC:\=/%
set LIB_GFORTRAN=%LIB_GFORTRAN:\=/%
set LIB_Z=%LIB_Z:\=/%
set LIB_DL=%LIB_DL:\=/%
set LIB_SZ=%LIB_SZ:\=/%

set SWB_EXTERNAL_LIBS="%LIB_HDF5_HL%;%LIB_HDF5%;%LIB_NETCDF%;%LIB_Z%;%LIB_DL%;%LIB_SZ%;%LIB_GCC%;%LIB_GFORTRAN%"


:: define where 'make copy' will place executables
set INSTALL_PREFIX=d:/DOS

:: define other variables for use in the CMakeList.txt file
:: options are "Release", "Profile" or "Debug"
set BUILD_TYPE="Release"

:: options are "x86" (32-bit) or "x64" (64-bit)
set SYSTEM_TYPE="win_x64"

:: define platform and compiler specific compilation flags
set CMAKE_Fortran_FLAGS_DEBUG="-Og -g -ggdb -fbacktrace -cpp -fcheck=all -fstack-usage -fexceptions -ffree-line-length-none -static -static-libgcc -static-libgfortran -ffpe-trap=zero,overflow"
set CMAKE_Fortran_FLAGS_RELEASE="-O2 -cpp -ffree-line-length-none -static -static-libgcc -static-libgfortran"
set CMAKE_Fortran_FLAGS_PROFILE="-O2 -pg -g -cpp -fno-omit-frame-pointer -DNDEBUG -fno-inline-functions -fno-inline-functions-called-once -fno-optimize-sibling-calls -ffree-line-length-none -static -static-libgcc -static-libgfortran"
::set CMAKE_Fortran_FLAGS_RELEASE="-O3 -mtune=native -fopenmp -flto -ffree-line-length-none -static-libgcc -static-libgfortran -DCURL_STATICLIB"

:: recreate clean Windows environment
set PATH=c:\windows;c:\windows\system32;c:\windows\system32\Wbem
set PATH=%PATH%;C:\Program Files (x86)\7-Zip
set PATH=%PATH%;C:\Program Files\Git\bin;c:\Program Files\CMake\bin
set PATH=%PATH%;%CMAKEROOT%\bin;%CMAKEROOT%\share
set PATH=%PATH%;c:\mingw64\bin
set PATH=%PATH%;c:\mingw64\include;c:\mingw64\lib

:: set a useful alias for make
echo %COMPILER_DIR%\bin\%MAKE_EXECUTABLE_NAME% %%1 > make.bat

:: not every installation will have these; I (SMW) find them useful
set PATH=%PATH%;D:\DOS\gnuwin32\bin

:: invoke CMake; add --trace to see copious details re: CMAKE
for %%f in ( "CodeBlocks - MinGW Makefiles" "MinGW Makefiles" ) do ^
cmake ..\..\.. -G %%f ^
-DCMAKE_Fortran_COMPILER=%Fortran_COMPILER_NAME% ^
-DCMAKE_C_COMPILER=%CMAKE_C_COMPILER%   ^
-DSWB_EXECUTABLE=%SWB_EXECUTABLE%       ^
-DSWB_EXTERNAL_LIBS=%SWB_EXTERNAL_LIBS% ^
-DR_SCRIPT="%R_SCRIPT%"           ^
-DCMAKE_EXE_LINKER_FLAGS=%LINKER_FLAGS%  ^
-DSYSTEM_TYPE=%SYSTEM_TYPE%  ^
-DCMAKE_BUILD_TYPE=%BUILD_TYPE%  ^
-DCMAKE_INSTALL_PREFIX:PATH=%INSTALL_PREFIX% ^
-DCMAKE_Fortran_FLAGS_DEBUG=%CMAKE_Fortran_FLAGS_DEBUG%  ^
-DCMAKE_Fortran_FLAGS_RELEASE=%CMAKE_Fortran_FLAGS_RELEASE%
