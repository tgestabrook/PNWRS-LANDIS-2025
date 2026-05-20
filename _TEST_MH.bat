rem GET DATE TIME AND CREATE NEW FOLDER
set date_time=%date:~-4%%date:~4,-8%%date:~7,-5%_%time:~0,-9%%time:~3,-6%
set date_time=%date_time: =0%
set extent=WenEnt

rem SET DIRECTORY
md LANDIS_Sim_WenEnt_MHtest_%date_time%
cd ./LANDIS_Sim_WenEnt_MHtest_%date_time%

rem COPY INPUT FILES INTO NEW DIR FOR ARCHIVING
md Input_file_archive

REM COPY INPUT FILES INTO NEW DIR FOR ARCHIVING
mkdir Input_file_archive
copy /Y *.txt Input_file_archive
copy /Y ..\Shared_inputs\*.txt Input_file_archive
copy /Y ..\%extent%\*.txt Input_file_archive
copy /Y ..\*.csv Input_file_archive
copy /Y ..\Shared_inputs\*.csv Input_file_archive
copy /Y ..\%extent%\*.csv Input_file_archive
copy /Y ..\%extent%\INITIAL_COMMUNITIES_*.tif Input_file_archive
copy /Y ..\%extent%\ECOREGIONS_*.tif Input_file_archive
copy /Y ..\%extent%\ext_BiomassHarvestMgmt_*.tif Input_file_archive
copy /Y ..\%extent%\ext_BiomassHarvestStands_*.tif Input_file_archive
copy /Y ..\%extent%\zClimate_Library\ClimateGenerator_baseline.txt Input_file_archive

REM SET UP MAGIC HARVEST
mkdir MagicHarvest
copy /Y ..\%extent%\ext_BiomassHarvestMgmt_WenEnt.tif MagicHarvest\MH_mgmt_areas.tif
copy /Y ..\%extent%\ext_BiomassHarvestStands_WenEnt.tif MagicHarvest\MH_stands.tif
copy /Y ..\%extent%\MH_Stand_age*.tif Input_file_archive

rem RUN LANDIS-II SCENARIO
call landis-ii-8 ../Testing/scenarioWenMH.txt

pause


