call set_run_variables.bat
%SWB2% --output_prefix=%OUTPUT_FILE_PREFIX%   ^
        --data_dir=%DATA_DIR%                 ^
        --weather_data_dir=%WEATHER_DIR%      ^
		--lookup_dir=%LOOKUP_DIR%             ^
        %SWB_CONTROL_FILE%

