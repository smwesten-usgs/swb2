#!/bin/bash

# obtain pathname specifying root of repository
PATHNAME=$(pwd | sed -e 's;/test/regression_tests/cs/swb2;;g') 

CONTROL_FILE_DIR=$(pwd)
OUTPUT_DIR=${CONTROL_FILE_DIR}/output
LOGFILE_DIR=${CONTROL_FILE_DIR}/logfile
GRID_DIR=${PATHNAME}/test/test_data/cs
TABLE_DIR=${PATHNAME}/test/test_data/tables
swb2 --output_prefix=central_sands_             \
  --data_dir=${GRID_DIR}                        \
  --weather_data_dir=${GRID_DIR}                \
  --output_dir=${OUTPUT_DIR}                    \
  --logfile_dir=${LOGFILE_DIR}                  \
  --lookup_dir=${TABLE_DIR}                     \
  central_sands_swb2.ctl
