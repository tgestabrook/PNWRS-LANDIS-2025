rem GET DATE TIME AND CREATE NEW FOLDER
set date_time=%date:~-4%%date:~4,-8%%date:~7,-5%_%time:~0,-9%%time:~3,-6%
set date_time=%date_time: =0%

rem SET DIRECTORY
md LANDIS_Sim_OkaMet_CalibRun_%date_time%
cd ./LANDIS_Sim_OkaMet_CalibRun_%date_time%

rem COPY INPUT FILES INTO NEW DIR FOR ARCHIVING
md Input_file_archive
xcopy  ".\Scenario.txt"		".\Input_file_archive" 
xcopy  "..\OkaMet\ext_NECN_OkaMet10.txt"			".\Input_file_archive" 
xcopy  "..\SPECIES.txt"				".\Input_file_archive" 
xcopy  "..\OkaMet\ext_NECN8_Species_Table.csv"		".\Input_file_archive" 
xcopy  "..\OkaMet\ext_SCRAPPLE_OkaMet.txt"		".\Input_file_archive" 
xcopy  "..\ext_Output_Biomass10.txt"		".\Input_file_archive" 
xcopy  "..\ext_Output_CohortStats10.txt"		".\Input_file_archive" 
xcopy  "..\OkaMet\INITIAL_COMMUNITIES_OkaMet.txt"	".\Input_file_archive" 
xcopy  "..\OkaMet\INITIAL_COMMUNITIES_OkaMet.tif"		".\Input_file_archive" 
xcopy  "..\OkaMet\ECOREGIONS_OkaMet.tif"		".\Input_file_archive" 
xcopy  "..\OkaMet\ECOREGIONS_OkaMet.txt"		".\Input_file_archive" 

rem RUN LANDIS-II SCENARIO
call landis-ii-8 ../Testing/scenario.txt

"C:\Program Files\R\R-4.4.1\bin\R.exe" CMD BATCH --no-echo --no-save --no-restore ../assign_crs_to_landis_output_and_compress_imgs.R ./post-processing-log.txt

pause


