## SET SCENARIO NAME
scenarioFile=$1
extent=$2
scenarioname=$3
dirName="LANDIS_Sim_${2}_${3}"

## GET DATE TIME AND CREATE NEW FOLDER
date_time="$(date +'%Y%m%d_%H%M')"
    
## IF DIRECTORY EXISTS, SLEEP FOR 1 MIN:
if [ -d "./${dirName}_$date_time" ]; then
    echo ${dirName}_$date_time already exists. Waiting for 1 minute…
    sleep 1m
    date_time="$(date +'%Y%m%d_%H%M')"
fi

## SET DIRECTORY
echo Creating directory: ${dirName}_$date_time
mkdir ${dirName}_$date_time
cd ./${dirName}_$date_time

## COPY INPUT FILES INTO NEW DIR FOR ARCHIVING
mkdir Input_file_archive 
cp -p ../*.txt					                                ./Input_file_archive 
cp -p ../$2/*.txt					                            ./Input_file_archive
cp -p ../*.csv 					                                ./Input_file_archive 
cp -p ../$2/*.csv					                            ./Input_file_archive
cp -p ../$2/INITIAL_COMMUNITIES_*.tif 			                    ./Input_file_archive 
cp -p ../$2/ECOREGIONS_*.tif 				                        ./Input_file_archive 
cp -p ../$2/ext_BiomassHarvestMgmt_*.tif 		                    ./Input_file_archive 
cp -p ../$2/ext_BiomassHarvestStands_*.tif 		                ./Input_file_archive 
cp -p ../$2/zClimate_Library/ClimateGenerator_baseline.txt 		./Input_file_archive 

## CREATE SCENARIO FILE WITH CORRECT EXTENT
sed -i "s|EXTENT|$2|g" ./Input_file_archive/$scenarioFile

## RUN LANDIS-II SCENARIO
echo Running LANDIS-II scenario...
dotnet $HOME/Core-Model-v7-LINUX/build/Release/Landis.Console.dll ./Input_file_archive/$scenarioFile

## RUN POST-PROCESSING SCRIPT TO PROJECT OUTPUT MAPS AND CONVERT TO GeoTIFF
echo Post-processing LANDIS-II Outputs...
Rscript ../assign_crs_to_landis_output_and_compress_imgs.R > ./post-processing-log.txt

read -p "Run complete. Press any key to close."
