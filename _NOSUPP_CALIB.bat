rem GET DATE TIME AND CREATE NEW FOLDER
set date_time=%date:~-4%%date:~4,-8%%date:~7,-5%_%time:~0,-9%%time:~3,-6%
set date_time=%date_time: =0%

rem SET DIRECTORY
md LANDIS_Sim_Tripod_CalibRun_%date_time%
cd ./LANDIS_Sim_Tripod_CalibRun_%date_time%

rem COPY INPUT FILES INTO NEW DIR FOR ARCHIVING
md Input_file_archive
xcopy  ".\Scenario.txt"		".\Input_file_archive" 
xcopy  "..\Tripod\ext_NECN_Tripod10.txt"			".\Input_file_archive" 
xcopy  "..\Tripod\ext_SCRAPPLE_NoSupp_Tripod.txt"		".\Input_file_archive" 
xcopy  "..\Tripod\ext_SCRAPPLE_Suppression_Effort_NoSupp.csv"		".\Input_file_archive" 

rem RUN LANDIS-II SCENARIO
call landis-ii-8 ../Testing/scenario_nosupp_calib.txt

pause


