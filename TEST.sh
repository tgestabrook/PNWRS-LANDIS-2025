scenarioFile=$1
extent=$2
scenarioname=$3
dirName="LANDIS_Sim_${2}_${3}"

echo $dirName

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

cat ./Input_file_archive/$scenarioFile