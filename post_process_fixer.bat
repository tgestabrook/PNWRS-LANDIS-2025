rem SET DIRECTORY
cd LANDIS_Sim_OkaMet_CalibRun_20250226_1659

echo Post-processing LANDIS-II Outputs...
rem RUN POST-PROCESSING SCRIPT TO PROJECT OUTPUT MAPS AND CONVERT TO GeoTIFF
"C:\Program Files\R\R-4.4.1\bin\R.exe" CMD BATCH --no-echo --no-save --no-restore ../assign_crs_to_landis_output_and_compress_imgs.R ./post-processing-log.txt
rem call ../_run_post_processing.bat

pause