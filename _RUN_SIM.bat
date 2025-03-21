REM SET SCENARIO NAME
set scenarioFile=%1
set extent=%2
set climate=%3

REM Set the directory name
set dirName=LANDIS_Sim_%2_%3_Sc%1

set date_time=%date:~-4%%date:~4,-8%%date:~7,-5%_%time:~0,-9%%time:~3,-6%
set date_time=%date_time: =0%

REM CREATE DIRECTORY
echo Creating directory: %dirName%_%date_time%
mkdir %dirName%_%date_time%

REM Run Build_Scenario.R
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" Build_Scenario7.R %scenarioFile% %dirName%_%date_time%

cd ./%dirName%_%date_time%

REM CREATE SCENARIO FILE WITH CORRECT EXTENT
powershell -Command "(Get-Content './scenario.txt') -replace 'EXTENT', '%extent%' | Set-Content './scenario.txt'"
powershell -Command "(Get-Content './scenario.txt') -replace 'CLIMATE', '%climate%' | Set-Content './scenario.txt'"

REM COPY INPUT FILES INTO NEW DIR FOR ARCHIVING
mkdir Input_file_archive
copy /Y *.txt Input_file_archive
copy /Y ..\*.txt Input_file_archive
copy /Y ..\%extent%\*.txt Input_file_archive
copy /Y ..\*.csv Input_file_archive
copy /Y ..\%extent%\*.csv Input_file_archive
copy /Y ..\%extent%\INITIAL_COMMUNITIES_*.tif Input_file_archive
copy /Y ..\%extent%\ECOREGIONS_*.tif Input_file_archive
copy /Y ..\%extent%\ext_BiomassHarvestMgmt_*.tif Input_file_archive
copy /Y ..\%extent%\ext_BiomassHarvestStands_*.tif Input_file_archive
copy /Y ..\%extent%\zClimate_Library\ClimateGenerator_baseline.txt Input_file_archive

REM SET UP MAGIC HARVEST
mkdir MagicHarvest
copy /Y ..\%extent%\ext_BiomassHarvestMgmt_*.tif MagicHarvest\MH_mgmt_areas.tif
copy /Y ..\%extent%\ext_BiomassHarvestStands_*.tif MagicHarvest\MH_stands.tif
copy /Y ..\%extent%\MH_Stand_age*.tif Input_file_archive

REM RUN LANDIS-II SCENARIO
echo Running LANDIS-II scenario...
call landis-ii-7 scenario.txt

REM ZIP THE RUN AND EXPORT TO THE CLOUD
echo ----------------------------------------
echo Simulation number %%i complete!
echo ----------------------------------------
pause











