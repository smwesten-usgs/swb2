# Building SWB version 2.0 on MacOS under Mac Ports

Make sure you have the necessary libraries and executables installed by means of Mac Ports:

```
sudo port

MacPorts 2.4.1
Entering shell mode...

> install gcc5
> install cmake
> install netcdf
> quit

Goodbye
```

Next, navigate to your local copy of this directory. Run the configuration script:

```
./run_cmake_gfortran.sh
```

CMake should then run some tests on the compilers and create the makefile needed to build SWB 2.0

Run:

```
make

sudo make copy
```

SWB 2.0 will be compiled and copied to /usr/local/bin/swb2.

The shell script assumes that you have installed Mac Ports in the standard directory, e.g. /opt/local. If this is not the case, the configuration script will fail. You'll need to edit this configuration file so that the locate command is able to produce a single correct filename for each of the libraries.
