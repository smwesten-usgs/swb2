copy ..\..\..\..\build\win_x64\ifort_vs\src\Debug\swb2.exe .
mkdir output
swb2 --output_prefix=central_sands_ --output_dir=output central_sands_swb2.ctl
