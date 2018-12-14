@echo off
:: remove existing Cmake cache and directories
del /F /Q CMakeCache.*
rmdir /S /Q CMakeFiles
rmdir /S /Q src
rmdir /S /Q Testing
rmdir /S /Q tests
del /S /Q *.txt
del /S /Q *.sln
del /S /Q *.vcxproj*
del /S /Q *.cmake

:: set CMAKE-related and build-related variables
set CMAKEROOT=C:\Program Files\CMake\
set Fortran_COMPILER_NAME=ifort
set CMAKE_C_COMPILER=icl
set NETCDF_VERSION='netCDF 4.6.1'

:: set path to Intel build tools
set INTEL_LIBDIR="C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\compiler\lib\intel64_win"
set INTEL_BINDIR="c:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\bin\intel64"
set MAKE_EXECUTABLE_NAME=nmake

:: explicitly locate each key library
for /f "delims=" %%x in ('where /R "c:\Program Files" libhdf5_hl.lib') do call set LIB_HDF5_HL=%%x
for /f "delims=" %%x in ('where /R "c:\Program Files" libhdf5.lib') do call set LIB_HDF5=%%x
for /f "delims=" %%x in ('where /R "c:\Program Files" zlibstatic.lib') do call set LIB_Z=%%x
for /f "delims=" %%x in ('where /R "c:\Program Files" netcdf.lib') do call set LIB_NETCDF=%%x
for /f "delims=" %%x in ('where /R %INTEL_LIBDIR% libirc.lib') do call set LIB_IRC=%%x
for /f "delims=" %%x in ('where /R %INTEL_LIBDIR% libifcore.lib') do call set LIB_IFCORE=%%x
for /f "delims=" %%x in ('where /R %INTEL_LIBDIR% ifconsol.lib') do call set LIB_IFCONSOL=%%x
::for /f "delims=" %%q in ('where /R %INTEL_BINDIR% ifort.exe') do call set IFORT_EXE=%%q

::echo 1) %IFORT_EXE%
:: substitute forward slash for backward slash
set LIB_HDF5_HL=%LIB_HDF5_HL:\=/%
set LIB_HDF5=%LIB_HDF5:\=/%
set LIB_NETCDF=%LIB_NETCDF:\=/%
set LIB_Z=%LIB_Z:\=/%
set LIB_IRC=%LIB_IRC:\=/%
set LIB_IFCORE=%LIB_IFCORE:\=/%
set LIB_IFCONSOL=%LIB_IFCONSOL:\=/%
::set IFORT_EXE=%IFORT_EXE:\=/%

::set LIB_HDF5_HL=%LIB_HDF5_HL: =\ %
::set LIB_HDF5=%LIB_HDF5: =\ %
::set LIB_NETCDF=%LIB_NETCDF: =\ %
::set LIB_Z=%LIB_Z: =\ %

:: define where 'make copy' will place executables
set INSTALL_PREFIX=d:\DOS

:: define other variables for use in the CMakeList.txt file
:: options are "Release", "Profile" or "Debug"
set BUILD_TYPE="Debug"

:: options are "x86" (32-bit) or "x64" (64-bit)
set SYSTEM_TYPE="win_x64"

:: define platform and compiler specific compilation flags
set CMAKE_Fortran_FLAGS_DEBUG="/Od /fpp /MTd /debug:full /check:all /traceback"
set CMAKE_Fortran_FLAGS_RELEASE="/O2 /QxHost /MT /fpp /traceback"
set CMAKE_C_FLAGS_DEBUG="/Od /MTd /debug:full"
set CMAKE_C_FLAGS_RELEASE="/O2 /QxHost /MT"
::set CMAKE_EXE_LINKER_FLAGS='/NODEFAULTLIB:"LIBCMT"'
::set LINKER_FLAGS="/nodefaultlib:vcomp"

::set SWB_EXTERNAL_LIBS="%LIB_HDF5_HL%;%LIB_HDF5%;%LIB_NETCDF%;%LIB_Z%;%LIB_IFCONSOL%;%LIB_IFCORE%"
::set SWB_EXTERNAL_LIBS="%LIB_HDF5_HL%;%LIB_HDF5%;%LIB_NETCDF%;%LIB_Z%;%LIB_IFCORE%;%LIB_IRC%;%LIB_IFCONSOL%"
set SWB_EXTERNAL_LIBS="%LIB_HDF5_HL%;%LIB_HDF5%;%LIB_NETCDF%;%LIB_Z%"
set CMAKE_OUTPUT="Visual Studio 15 2017"

echo "Running CMake..."
cmake ..\..\.. -G "NMake Makefiles" ^
-DCMAKE_Fortran_COMPILER=%Fortran_COMPILER_NAME% ^
-DCMAKE_C_COMPILER=%CMAKE_C_COMPILER%   ^
-DSWB_EXTERNAL_LIBS=%SWB_EXTERNAL_LIBS% ^
-DSYSTEM_TYPE=%SYSTEM_TYPE%             ^
-DCMAKE_BUILD_TYPE=%BUILD_TYPE%         ^
-DCMAKE_INSTALL_PREFIX:PATH=%INSTALL_PREFIX%                ^
-DCMAKE_Fortran_FLAGS_DEBUG=%CMAKE_Fortran_FLAGS_DEBUG%     ^
-DCMAKE_Fortran_FLAGS_RELEASE=%CMAKE_Fortran_FLAGS_RELEASE% ^
-DCMAKE_EXE_LINKER_FLAGS=%CMAKE_EXE_LINKER_FLAGS% ^
-DCMAKE_C_FLAGS_DEBUG=%CMAKE_C_FLAGS_DEBUG%       ^
-DCMAKE_C_FLAGS_RELEASE=%CMAKE_C_FLAGS_RELEASE%
