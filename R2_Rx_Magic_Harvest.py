import sys
import os
import re
import pandas as pd
import geopandas as geo
import rasterio as rast
import numpy as np

'''Directories'''
fire_dir = './social-climate-fire'
harvest_dir = "./Harvest"
magic_harvest_dir = "./MagicHarvest"
input_file_archive = "./Input_file_archive"

'''Functions'''
def make_zero_raster(template_rast, dtype = np.float64):
    zero_data = np.zeros((template_rast.count, template_rast.height, template_rast.width), dtype=dtype)
    return zero_data

'''Load command arguments.'''
timestep = sys.argv[1]  # timestep of LANDIS simulation
last_year = timestep-1
scenario_rx_name = sys.argv[2]  # which areas are valid for Rx burning?
thin_cooldown = int(sys.argv[3])  # baseline time between thinnings
if thin_cooldown <= 30:
    cold_cooldown = thin_cooldown + 5
else:
    cold_cooldown = thin_cooldown

mh_file_name = sys.argv[4]  # which magic harvest file are we using?

'''Load basic parameters'''
severity_thresholds = [5, 41, 176, 376, 2001]  # breaks to divide simulated fire severity into classes

rx_fire_cooldown = 10  # years till a stand is eligible for Rx fire

files = os.listdir(input_file_archive)
ecos_file = [f for f in files if "ECOREGIONS" in f][0]  # get name of extent from the ecoregion file in the input file archive

with rasterio.open(input_file_archive + "/ECOREGIONS_" + LANDIS_EXTENT + ".tif") as src:
    ecos_rast = src.read(1)

LANDIS_EXTENT = ecos_file.replace("ECOREGIONS_", "").replace(".tif", "")

# base_mgmt_rast = rasterio.open("./Input_file_archive/ext_BiomassHarvestMgmt_" + LANDIS_EXTENT + ".tif")
with rasterio.open("./Input_file_archive/ext_BiomassHarvestStands" + LANDIS_EXTENT + ".tif") as src:
    base_stands_rast = src.read(1)

with rasterio.open("./Input_file_archive/ext_BiomassHarvestMgmt_" + LANDIS_EXTENT + ".tif") as src:
    base_mgmt_rast = src.read(1)

max_annual_thin = (1 / thin_cooldown) * np.isin(base_mgmt_rast, [1, 2, 3]).sum()
max_annual_ind = 0.05 * (base_mgmt_rast == 4).sum()
max_annual_DNR = 0.05 * (base_mgmt_rast == 7).sum()
max_annual_salvage = 0.1 * (base_mgmt_rast < 9).sum()

'''Initialize raster for time 0'''
if timestep == 1:
    if not os.path.isdir("./MagicHarvest"): os.makedirs("./MagicHarvest")

    # copy stand age from input file archive
    with rasterio.open(input_file_archive + "/MH_Stand_age" + LANDIS_EXTENT + ".tif") as src:
        stand_age_rast = src.read()
        kwargs = src.meta.copy()
    with rasterio.open(magic_harvest_dir + "/stand_age.tif", 'w', **kwargs) as dst:
        dst.write(stand_age_rast)

    # copy rx dynamic 
    with rasterio.open("../" + LANDIS_EXTENT + "/SCRAPPLE_input_maps/" + scenario_rx_name) as src:
        rx_dynamic_rast = src.read()
        kwargs = src.meta.copy()
    with rasterio.open(magic_harvest_dir + "/Rx_base.tif", 'w', **kwargs) as dst:
        dst.write(rx_dynamic_rast)

    # initialize fire cooldown
    fire_cooldown_rast = make_zero_raster(ecos_rast, dtype=int)
    with rasterio.open("./MagicHarvest/fire_cooldown.tif", "w") as dst:
        dst.write(fire_cooldown_rast)

    # initialize thinning cooldown
    thinning_cooldown_rast = make_zero_raster(ecos_rast, dtype=int)
    with rasterio.open("./MagicHarvest/thinning_cooldown.tif", "w") as dst:
        dst.write(thinning_cooldown_rast)

'''Load this year's rasters'''

# Load last year's disturbances
if timestep == 1:
    last_year_fire_severity_rast = make_zero_raster(ecos_rast, dtype=int)
    last_year_harvest_prescrip.r = make_zero_raster(ecos_rast, dtype=int)
else:
    # TDOD
    print("TODO")

# Load cooldown rasters
fire_cooldown_rast = []
thinning_cooldown_rast = []

# stand age
stand_age_rast = []

'''Update tracking rasters'''
# Calculate this year's stand age
print("Calculating stand age...")

# Update fire cooldown
print("Updating fire cooldown...")

# Update thinning cooldown
print("Updating thinning cooldown...")

'''Determine harvest eligibility'''
# Calculate zones elegible for salvage


# Calculate zones elegitible for thinning


# Create a new stand map by grouping patches


'''Write new management zones'''


'''Find patches eligible for Rx fire'''
print("Generating new Rx allocation...")

rx_zone_rast = []  # area that can be burned, either mgmt or mgmt + wildland
rx_stands_rast = []  # original map of forest stands
topo_asp_rast = []  # topo-asp map used to assign rx fire zones














