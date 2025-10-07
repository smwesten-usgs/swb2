set OUTPUT_DIR=output
set LOGFILE_DIR=logfile
set GRID_DIR=..\..\test_data\cs
set TABLE_DIR=..\..\test_data\tables
swb2 --output_prefix=central_sands_            ^
  --data_dir=%GRID_DIR%                        ^
  --weather_data_dir=%GRID_DIR%                ^
  --output_dir=%OUTPUT_DIR%                    ^
  --logfile_dir=%LOGFILE_DIR%                  ^
  --lookup_dir=%TABLE_DIR%                     ^
  central_sands_swb2.ctl
