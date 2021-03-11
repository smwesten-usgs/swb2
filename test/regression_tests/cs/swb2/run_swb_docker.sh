#!/bin/bash

# obtain pathname specifying root of repository
PATHNAME=$(pwd | sed -e 's;/test/regression_tests/cs/swb2;;g') 

CONTROL_FILE_DIR=$(pwd)
OUTPUT_DIR=${PATHNAME}/output
GRID_DIR=${PATHNAME}/test/test_data/cs
TABLE_DIR=${PATHNAME}/test/test_data/tables

docker run -w /work_dir                           \
  -v ${CONTROL_FILE_DIR}:/work_dir/control_file   \
  -v ${OUTPUT_DIR}:/work_dir/output               \
  -v ${GRID_DIR}:/work_dir/grid_data              \
  -v ${TABLE_DIR}:/work_dir/table_data            \
  swb2 --output_prefix=central_sands_             \
    --data_dir=/work_dir/grid_data                \
    --weather_data_dir=/work_dir/grid_data        \
    --output_dir=/work_dir/output                 \
    --lookup_dir=/work_dir/table_data             \
    /work_dir/control_file/central_sands_swb2.ctl
