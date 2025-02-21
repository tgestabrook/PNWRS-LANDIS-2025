"C:\Program Files\7-Zip\7z.exe" a "./CLIMATE_FILES.zip" "./*/zClimate_Library/*"

gcloud storage cp "./CLIMATE_FILES.zip" gs://usfs_landis_inputs
pause