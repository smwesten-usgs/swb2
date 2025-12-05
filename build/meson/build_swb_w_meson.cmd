@echo off
setlocal enabledelayedexpansion

:: Set base directory where NetCDF is usually installed
set "BASE_DIR=C:\Program Files"
set "PATTERN=netCDF 4.9."

:: Initialize variable
set "LATEST_NETCDF="

:: Loop through directories in BASE_DIR
for /d %%D in ("%BASE_DIR%\%PATTERN%*") do (
    set "LATEST_NETCDF=%%D"
)

:: Check if we found a match
if "%LATEST_NETCDF%"=="" (
    echo No matching NetCDF installation found in %BASE_DIR%\%PATTERN%*
    exit /b 1
)

echo Found NetCDF installation: %LATEST_NETCDF%

:: point Meson to new LLVM versions of intel compilers on Windows
set FC=ifx
set CC=icx
set CXX=icx

rmdir /S /Q builddir
mkdir builddir
:: meson setup builddir -Dnetcdf_root="%LATEST_NETCDF%" ..\.. && cd builddir && meson compile --verbose
meson setup builddir -Dnetcdf_root="%LATEST_NETCDF%" ..\.. && cd builddir && meson compile
cd src
copy /Y swb2.exe ..\..\..\..\bin\win_x64\swb2.exe
copy /Y swbstats2.exe ..\..\..\..\bin\win_x64\swbstats2.exe

copy /Y swb2.exe d:\bin\swb2.exe
copy /Y swbstats2.exe d:\bin\swbstats2.exe
cd ..\test\unit_tests
copy /Y swbtest.exe ..\..\..\..\..\test\unit_tests\swbtest.exe