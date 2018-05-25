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

set MAKE_EXECUTABLE_NAME=nmake
set R_HOME=C:\Program Files\R\R-3.3.1\bin

:: explicitly locate each key library
for /f "delims=" %%x in ('where /R "c:\Program Files" libhdf5_hl.lib') do call set LIB_HDF5_HL=%%x
for /f "delims=" %%x in ('where /R "c:\Program Files" libhdf5.lib') do call set LIB_HDF5=%%x
for /f "delims=" %%x in ('where /R "c:\Program Files" zlibstatic.lib') do call set LIB_Z=%%x
for /f "delims=" %%x in ('where /R "c:\Program Files" netcdf.lib') do call set LIB_NETCDF=%%x

::for /f "delims=" %%x in ('where /R "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\compiler\lib\intel64_win" libvcruntime.lib') do call set LIB_VCRUNTIME=%%x
::for /f "delims=" %%x in ('where /R "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\compiler\lib\intel64_win" libucrt.lib') do call set LIB_UCRT=%%x
::for /f "delims=" %%x in ('where /R "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\compiler\lib\intel64_win" libifcore.lib') do call set LIB_GCC=%%x
::for /f "delims=" %%x in ('where /R "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\compiler\lib\intel64_win" libifcorert.lib') do call set LIB_GFORTRAN=%%x

:: substitute forward slash for backward slash
::set LIB_HDF5_HL=%LIB_HDF5_HL:\=/%
::set LIB_HDF5=%LIB_HDF5:\=/%
::set LIB_NETCDF=%LIB_NETCDF:\=/%
::set LIB_GCC=
::set LIB_GFORTRAN=
::set LIB_Z=%LIB_Z:\=/%
::set LIB_DL=
::set LIB_SZ=

echo NetCDF library: %LIB_NETCDF%
echo HDF5 library: %LIB_HDF5%
echo HDF5_HL library: %LIB_HDF5_HL%
echo GCC library: %LIB_GCC%
echo GFORTRAN library: %LIB_GFORTRAN%


:: define where 'make copy' will place executables
set INSTALL_PREFIX=d:\DOS

:: define other variables for use in the CMakeList.txt file
:: options are "Release", "Profile" or "Debug"
set BUILD_TYPE="Debug"

:: options are "x86" (32-bit) or "x64" (64-bit)
set SYSTEM_TYPE="win_x64"

:: define platform and compiler specific compilation flags
set CMAKE_Fortran_FLAGS_DEBUG="/Od /debug:all /libs:static /MTd /fpp"
set CMAKE_Fortran_FLAGS_RELEASE="/O2 /QxHost /static /fpp /MT"
set CMAKE_C_FLAGS_DEBUG="/Od /MTd"
set CMAKE_C_FLAGS_RELEASE="/O2 /QxHost /static /fpp /MT"

::set LINKER_FLAGS="/nodefaultlib:vcomp"

echo "Running CMake..."
cmake ..\..\.. -G "NMake Makefiles" ^
-DCMAKE_Fortran_COMPILER=%Fortran_COMPILER_NAME% ^
-DCMAKE_C_COMPILER=%CMAKE_C_COMPILER% ^
-DSWB_EXECUTABLE=%SWB_EXECUTABLE%      ^
-DLIB_HDF5_HL="%LIB_HDF5_HL%"   ^
-DLIB_HDF5="%LIB_HDF5%"         ^
-DLIB_SZ=%LIB_SZ%               ^
-DLIB_Z="%LIB_Z%"               ^
-DLIB_GCC="%LIB_GCC%"           ^
-DLIB_GFORTRAN="%LIB_GFORTRAN%" ^
-DLIB_NETCDF="%LIB_NETCDF%"     ^
-DR_SCRIPT=%R_SCRIPT%           ^
-DSYSTEM_TYPE=%SYSTEM_TYPE%  ^
-DCMAKE_BUILD_TYPE=%BUILD_TYPE%  ^
-DCMAKE_INSTALL_PREFIX:PATH=%INSTALL_PREFIX% ^
-DCMAKE_Fortran_FLAGS_DEBUG=%CMAKE_Fortran_FLAGS_DEBUG%  ^
-DCMAKE_Fortran_FLAGS_RELEASE=%CMAKE_Fortran_FLAGS_RELEASE% ^
-DCMAKE_C_FLAGS_DEBUG=%CMAKE_C_FLAGS_DEBUG%  ^
-DCMAKE_C_FLAGS_RELEASE=%CMAKE_C_FLAGS_RELEASE%
