## Set starting directory
back=$PWD

## SET SCENARIO NAME
scenarioFile=$1
extent=$2
climate=$3
maxsims=$4
GIT_ID=$(git rev-parse --short=6 HEAD)

# Fix Rscript command in magic_harvest
sed -i 's|"C:\\Program Files\\R\\R-4.4.1\\bin\\Rscript.exe"|Rscript|g' ./ext_magic_harvest*

#re="^scenario_(.+).txt$"
#if [[ $scenarioFile =~ $re ]]; then scenarioname=${BASH_REMATCH[1]}; fi

dirName="Sim${GIT_ID}_${2}_${3}_Sc${1}"

## Set starting directory
set back=$PWD
## add a new comment

for repeat in `seq 1 $maxsims`
do 
    echo Simulation number: $repeat
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
    Rscript ./Build_Scenario.R $scenarioFile ${dirName}_$date_time
    cd ./${dirName}_$date_time

    ## CREATE SCENARIO FILE WITH CORRECT EXTENT
    sed -i "s|EXTENT|$extent|g" ./scenario.txt
    sed -i "s|CLIMATE|$climate|g" ./scenario.txt    

    ## COPY INPUT FILES INTO NEW DIR FOR ARCHIVING
    mkdir Input_file_archive 
    cp -p ./*.txt                                                   ./Input_file_archive 
    cp -p ../*.txt					                                ./Input_file_archive 
    cp -p ../$extent/*.txt					                            ./Input_file_archive
    cp -p ../*.csv 					                                ./Input_file_archive 
    cp -p ../$extent/*.csv					                            ./Input_file_archive
    cp -p ../$extent/INITIAL_COMMUNITIES_*.tif 			                    ./Input_file_archive 
    cp -p ../$extent/ECOREGIONS_*.tif 				                        ./Input_file_archive 
    cp -p ../$extent/ext_BiomassHarvestMgmt_*.tif 		                    ./Input_file_archive 
    cp -p ../$extent/ext_BiomassHarvestStands_*.tif 		                ./Input_file_archive 
    cp -p ../$extent/zClimate_Library/ClimateGenerator_baseline.txt 		./Input_file_archive 

    ## SET UP MAGIC HARVEST
    mkdir MagicHarvest
	cp -p ../$extent/ext_BiomassHarvestMgmt_*.tif 		./MagicHarvest/MH_mgmt_areas.tif
	cp -p ../$extent/ext_BiomassHarvestStands_*.tif 		./MagicHarvest/MH_stands.tif
    cp -p ../$extent/MH_Stand_age*.tif		   	./Input_file_archive



    ## RUN LANDIS-II SCENARIO
    echo Running LANDIS-II scenario...
    dotnet $LANDIS_CONSOLE ./scenario.txt
    # dotnet $HOME/Core-Model-v7-LINUX/build/Release/Landis.Console ./Input_file_archive/$scenarioFile

    ## RUN POST-PROCESSING SCRIPT TO PROJECT OUTPUT MAPS AND CONVERT TO GeoTIFF
    ## echo Post-processing LANDIS-II Outputs...
    ## Rscript ../assign_crs_to_landis_output_and_compress_imgs.R > ./post-processing-log.txt

    echo ----------------------------------------
    echo Simulation number $repeat complete!
    echo ----------------------------------------

    


    cd $back
    
    echo Exporting to cloud...
    zip -q -r ${dirName}_$date_time.zip ${dirName}_$date_time
    gsutil -m cp -r ${dirName}_$date_time.zip gs://usfs_landis_outputs
    rm -r -f ${dirName}_$date_time
    echo Done!
done

# ## ZIP RUNS AND EXPORT TO THE CLOUD
# for dir in ./LANDIS_*/; do 
# 	dir=${dir%*/} # Remove the trailing "/"
# 	echo "${dir##*/}" # Print everything after the final "/"
	
# done


read -p "Run complete. Press any key to close."
