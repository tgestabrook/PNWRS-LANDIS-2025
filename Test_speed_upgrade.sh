## Set starting directory
back=$PWD

## Set starting directory
set back=$PWD

echo Creating directory: Baseline_sim
mkdir Baseline_sim
cd ./Baseline_sim

mkdir Input_file_archive 
cp -p ./*.txt                                                   ./Input_file_archive 
cp -p ../Shared_inputs/*.txt					                                ./Input_file_archive 
cp -p ../Tripod/*.txt					                            ./Input_file_archive
cp -p ../*.csv 					                                ./Input_file_archive 
cp -p ../Shared_inputs/*.csv 					                                ./Input_file_archive 
cp -p ../Tripod/*.csv					                            ./Input_file_archive
cp -p ../Tripod/INITIAL_COMMUNITIES_*.tif 			                    ./Input_file_archive 
cp -p ../Tripod/ECOREGIONS_*.tif 				                        ./Input_file_archive 
cp -p ../Tripod/ext_BiomassHarvestMgmt_*.tif 		                    ./Input_file_archive 
cp -p ../Tripod/ext_BiomassHarvestStands_*.tif 		                ./Input_file_archive 
cp -p ../Tripod/zClimate_Library/ClimateGenerator_baseline.txt 		./Input_file_archive 

cd $back
echo Running LANDIS-II scenario...

docker run --cpus=4 --memory=64g --mount type=bind,src=".",dst="/scenarioFolder" --name ContainerBaseline \
landis-ii-8-uclv2-release:release /bin/sh -c "cd /scenarioFolder/Baseline_sim && dotnet \$LANDIS_CONSOLE ../Testing/scenario_test_speed_upgrade.txt"

echo Done!

set back=$PWD

echo Creating directory: Updated_sim
mkdir Updated_sim
cd ./Updated_sim

mkdir Input_file_archive 
cp -p ./*.txt                                                   ./Input_file_archive 
cp -p ../Shared_inputs/*.txt					                                ./Input_file_archive 
cp -p ../Tripod/*.txt					                            ./Input_file_archive
cp -p ../*.csv 					                                ./Input_file_archive 
cp -p ../Shared_inputs/*.csv 					                                ./Input_file_archive 
cp -p ../Tripod/*.csv					                            ./Input_file_archive
cp -p ../Tripod/INITIAL_COMMUNITIES_*.tif 			                    ./Input_file_archive 
cp -p ../Tripod/ECOREGIONS_*.tif 				                        ./Input_file_archive 
cp -p ../Tripod/ext_BiomassHarvestMgmt_*.tif 		                    ./Input_file_archive 
cp -p ../Tripod/ext_BiomassHarvestStands_*.tif 		                ./Input_file_archive 
cp -p ../Tripod/zClimate_Library/ClimateGenerator_baseline.txt 		./Input_file_archive 

cd $back
echo Running LANDIS-II scenario...

docker run --cpus=4 --memory=64g --mount type=bind,src=".",dst="/scenarioFolder" --name ContainerUpdated \
landis-ii-8-uclv2-release:speedup /bin/sh -c "cd /scenarioFolder/Updated_sim && dotnet \$LANDIS_CONSOLE ../Testing/scenario_test_speed_upgrade.txt"

echo Done!

    
echo Exporting to cloud...
zip -q -r Baseline_sim.zip Baseline_sim
gsutil -m cp -r Baseline_sim.zip gs://usfs_landis_outputs
rm -r -f Baseline_sim
zip -q -r Updated_sim.zip Updated_sim
gsutil -m cp -r Updated_sim.zip gs://usfs_landis_outputs
rm -r -f Updated_sim
echo Done!

read -p "Run complete. Press any key to close."
