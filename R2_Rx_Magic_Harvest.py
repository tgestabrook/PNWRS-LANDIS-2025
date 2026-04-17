import sys
import os
import re
import pandas as pd
import geopandas as geo
import rasterio as rast
import numpy as np
from scipy.ndimage import generic_filter

'''Directories'''
fire_dir = './social-climate-fire'
harvest_dir = "./Harvest"
magic_harvest_dir = "./MagicHarvest"
input_file_archive = "./Input_file_archive"

'''Functions'''
def make_zero_raster(template_rast, dtype = np.float64):
    zero_data = np.zeros((template_rast.count, template_rast.height, template_rast.width), dtype=dtype)
    return zero_data

def smooth_fires(fire_severity_rast):
    fire_severity_rast = np.where(fire_severity_rast > 1, fire_severity_rast, np.nan)
    is_nan = np.isnan(fire_severity_rast)
    focal_median = generic_filter(fire_severity_rast, np.nanmedian, size=(3, 3), mode='constant', cval=np.nan)
    fire_severity_rast[is_nan] = focal_median[is_nan]
    fire_severity_rast = np.digitize(fire_severity_rast, severity_thresholds, right = True)
    return fire_severity_rast

def make_contiguous_patches(patch_id_rast):
    results = (
        {'properties': {'val': v}, 'geometry': s}
        for i, (s, v) in enumerate(rasterio.features.shapes(patch_id_rast, transform=kwargs.transform))
    )
    gdf = geo.GeoDataFrame.from_features(list(results), crs=ecos_crs)

    # 4. Mutate ID and Shuffle
    # mutate(ID = row_number()) %>% mutate(ID = sample(ID))
    gdf['ID'] = np.arange(len(gdf)) + 1
    gdf['ID'] = np.random.permutation(gdf['ID'].values)

    # 5. Rasterize back to grid
    # Burn the 'ID' column back into a raster array
    patch_id_rast = rasterio.features.rasterize(
        ((geom, id_val) for geom, id_val in zip(gdf.geometry, gdf.ID)),
        out_shape=ecos_rast.shape,
        transform=ecos_transform,
        fill=-999,
        dtype='int32'
    )
    return patch_id_rast

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
    ecos_transform = src.transform
    ecos_crs = src.crs
    kwargs = src.profile

print(kwargs)

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
    with rasterio.open(magic_harvest_dir + "/stand_age.tif", 'w', **kwargs) as dst:
        dst.write(stand_age_rast)

    # copy rx dynamic 
    with rasterio.open("../" + LANDIS_EXTENT + "/SCRAPPLE_input_maps/" + scenario_rx_name) as src:
        rx_dynamic_rast = src.read()
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
    print("Initializing rasters for timestep 1.")
    last_year_fire_severity_rast = make_zero_raster(ecos_rast, dtype=int)
    last_year_harvest_prescrip_rast = make_zero_raster(ecos_rast, dtype=int)
else:
    print("Loading last year's fires and prescriptions.")
    with rasterio.open(magic_harvest_dir + "/fire-dnbr-" + last_year + ".tif") as src:
        last_year_fire_severity_rast = src.read()
    last_year_fire_severity_rast = smooth_fires(last_year_fire_severity_rast)

    with rasterio.open(harvest_dir, "/biomass-harvest-prescripts-" + last_year + ".tif") as src:
        last_year_harvest_prescrip_rast = src.read()

# Load cooldown rasters
with rasterio.open(magic_harvest_dir, + "/fire_cooldown.tif") as src:
    fire_cooldown_rast = src.read()
with rasterio.open(magic_harvest_dir, + "/thinning_cooldown.tif") as src:
    thinning_cooldown_rast = src.read()

# stand age
with rasterio.open(magic_harvest_dir, + "/stand_age.tif") as src:
    stand_age_rast = src.read()

'''Update tracking rasters'''
# Calculate this year's stand age
print("Calculating stand age...")
stand_age_rast = stand_age_rast + 1  # age up stands by 1 year
stand_age_rast[last_year_fire_severity_rast == 0] = 0  # stands burned at high severity are age 0
stand_age_rast[last_year_harvest_prescrip_rast in [5, 8]] = 1 # stands clearcut are age 1, to bypass salvage
# NOTE: salvage happens within 2 years. If we set age to 0, we can't easily load data from two years ago to exclude from salvage zone, so we set to one so we only need to exclude last year

with rasterio.open(magic_harvest_dir + "/stand_age.tif", 'w', **kwargs) as dst:
    dst.write(stand_age_rast)

# Update fire cooldown
print("Updating fire cooldown...")
fire_cooldown_rast = fire_cooldown_rast - 1  # increment cooldown by one
fire_cooldown_rast[last_year_fire_severity_rast > 0] = rx_fire_cooldown  # areas burned by any fire are reset
fire_cooldown_rast[last_year_fire_severity_rast > 3] = 5  # shorter timer for high severity fires 
fire_cooldown_rast[last_year_harvest_prescrip_rast > 1] = rx_fire_cooldown  # areas thinned get slash burned, which resets fire timer

with rasterio.open(magic_harvest_dir + "/fire_cooldown.tif", "w", **kwargs) as dst:
    dst.write(fire_cooldown_rast)

# Update thinning cooldown
print("Updating thinning cooldown...")
thinning_cooldown_rast = thinning_cooldown_rast - 1 # increment cooldown by one
thinning_cooldown_rast[thinning_cooldown_rast < 0] = 0  # can't go below zero
thinning_cooldown_rast[last_year_harvest_prescrip_rast in [9, 10, 12, 13]] = thin_cooldown  # thinned stands get set to cooldown
thinning_cooldown_rast[last_year_harvest_prescrip_rast in [11, 14, 15]] = cold_cooldown  # separate cooldown set for cold forest prescripts

with rasterio.open(magic_harvest_dir + "/thinning_cooldown.tif", "w", **kwargs) as dst:
    dst.write(thinning_cooldown_rast)

'''Determine harvest eligibility'''
# Calculate zones elegible for salvage
eligible_salvage_rast = make_zero_raster(ecos_rast, dtype = int)
eligible_salvage_rast[(stand_age_rast < 2) & (base_mgmt_rast < 9)] = 1  # salvage areas with stand age < 2 in managed forest
eligible_salvage_rast[last_year_harvest_prescrip_rast == 7] = 0  # don't salvage twice in a row
eligible_salvage_rast[last_year_harvest_prescrip_rast in [5, 8]] = 0  # don't salvage areas that were clearcut last year.

# Calculate zones elegitible for thinning
eligible_thin_rast = make_zero_raster(ecos_rast, dtype = int)
eligible_thin_rast[(stand_age_rast >= 20) & (base_mgmt_rast in [1, 2, 3]) & (thinning_cooldown_rast == 0)] = 1  # thin if age 20+ and cooldown is 0 and in managed forest

# Create a new stand map by grouping patches
print("Generating new patches...")
# Generate new patch IDs from unique combinations of stand age and management zone
new_patches_rast = np.char.add(stand_age_rast.astype(str), base_mgmt_rast.astype(str)).astype(int)
# new_patches_vect = geo.features.shapes(source = new_patches_rast, transform = kwargs.transform)

new_patches_rast = make_contiguous_patches(new_patches_rast)

# Save new patches
kwargs.update(dtype='int32', count=1, nodata=-999)
with rasterio.open(magic_harvest_dir + "/MH_stands" + timestep + ".tif", 'w', **kwargs) as dst:
    dst.write(new_patches_rast, 1)


# Read and alter harvest input file to point it toward the new management areas
with open(os.path.join('.', 'Input_file_archive', mh_file_name), 'r') as f:
    harvest_input = f.readlines()

new_input = []
for line in harvest_input:
    if 'ManagementAreas' in line:
        new_input.append(f'ManagementAreas    ./MagicHarvest/MH_mgmt_areas_{timestep}.tif\n')
    elif 'Stands' in line:
        new_input.append(f'Stands    ./MagicHarvest/MH_stands_{timestep}.tif\n')
    else:
        new_input.append(line)
with open(os.path.join('.', 'Input_file_archive', mh_file_name), 'w') as f:
    f.writelines(new_input)

'''Write new management zones'''
print('Generating management zones...')

elig_df = {
    "Patch": new_patches_rast.flatten(),
    "Salvage_elig": eligible_salvage_rast.flatten(),
    "Thin_elig": eligible_thin_rast.flatten(),
    "Mgmt_zone": base_mgmt_rast.flatten()
}

elig_df = (pd.DataFrame(elig_df)
           .query('Patch != 0')
           .groupby('Patch')
           .agg(
               Salvage_elig=('Salvage_elig', 'mean'),
               Thin_elig=('Thin_elig', 'mean'),
               Mgmt_zone=('Mgmt_zone', 'mean'),
               Pixels=('Patch', 'count')
           )
           .reset_index())

# Randomly shuffle the order of stands that are at least 75% eligible
# then calculate cumulative sum, and cut off the dataframe at or below
# the predefined max treatable area 
salvageable_stands_df = (elig_df
                         .query('Salvage_elig > 0.75')
                         .sample(frac=1)  # Shuffle the dataframe (n=nrow in R)
                         .assign(Area_cumsum = lambda x: x['Pixels'].cumsum())
                         .query('Area_cumsum <= @max_annual_salvage'))

thinnable_stands_df = (elig_df
                         .query('Thin_elig > 0.75')
                         .sample(frac=1)  # Shuffle the dataframe (n=nrow in R)
                         .assign(Area_cumsum = lambda x: x['Pixels'].cumsum())
                         .query('Area_cumsum <= @max_annual_thin'))

new_zones_r = np.full(ecos_rast.shape, 9, dtype=np.int32)

# use base harvest to allocate industrial and WA dnr area
new_zones_r[base_mgmt_rast == 4] = 4 
new_zones_r[base_mgmt_rast == 7] = 7

if not thinnable_stands_df.empty:
    thin_mask = np.isin(new_patches_rast, thinnable_stands_df['Patch'])
    # Assign values from base_mgmt_r only where the mask is True
    new_zones_r[thin_mask] = base_mgmt_rast[thin_mask]

if not salvageable_stands_df.empty:
    salv_mask = np.isin(new_patches_rast, salvageable_stands_df['Patch'])
    new_zones_r[salv_mask] = 6

with rasterio.open(magic_harvest_dir + "/MH_mgmt_areas_" + timestep + ".tif", 'w', **kwargs) as dst:
    dst.write(new_zones_r, 1)


'''Find patches eligible for Rx fire'''
print("Generating new Rx allocation...")

with rasterio.open(magic_harvest_dir + "/Rx_base.tif") as src:
    rx_zone_rast = src.read(1)  # area that can be burned, either mgmt or mgmt + wildland

with rasterio.open(os.path.join('..', LANDIS.EXTENT, 'ext_BiomassHarvestStands_' + LANDIS.EXTENT + '.tif')) as src:
    rx_stands_rast = src.read(1) # original map of forest stands

with rasterio.open(os.path.join('..', LANDIS.EXTENT, 'TopoAsp_' + LANDIS.EXTENT + '.tif')) as src:
    topo_asp_rast = src.read(1) # original map of forest stands

# get a maximum burnable area value that keeps a steady cycle of burning
max_annual_Rx <- np.sum(rx_zone_r > 0) / fire_cooldown


elig_df = {
    "Patch": rx_stands_rast.flatten(),
    "Rx_zone": rx_zone_rast.flatten(),
    "Rx_cooldown": fire_cooldown_rast.flatten(),
    "TopoAsp": topo_asp_rast.flatten()
}

elig_df = (pd.DataFrame(elig_df)
           .query('Patch != 0')
           .query('Rx_zone != 0')
           .groupby('Patch')
           .agg(
               Mean_cooldown=('Rx_cooldown', 'mean'),
               TopoAsp =('TopoAsp', 'mean'),
               Pixels=('Patch', 'count')
           )
           .query('Mean_cooldown <= 0.33')
           .reset_index())

# Mask inelegigible stands out of 
burn_mask = np.isin(rx_stands_rast, elig_df['Patch'])
topo_asp_rast[burn_mask] = np.nan
topo_asp_rast[rx_zone_rast == 0] = np.nan

# Turn this edited topoasp layer into discrete contiguous patch polygons
topo_asp_rast = make_contiguous_patches(topo_asp_rast)

topo_asp_selected_df = {
    "TopoAspID": topo_asp_rast.flatten(),
    "FireCooldown": fire_cooldown_rast.flatten()
}

topo_asp_selected_df = (pd.DataFrame(topo_asp_selected_df)
                        .groupby('TopoAspID')
                        .agg(
                            Pixels=('Patch', 'count'),
                            FireCooldown=('FireCooldown', 'mean')
                        )
                        .assign(FireCooldown = lambda x: x['FireCooldown'] * -1)
                        .sample(frac = 1, weights="FireCooldown")
                        .assign(Area_cumsum = lambda x: x['Pixels'].cumsum())
                        .query('Area_cumsum <= @max_annual_Rx')
                        )

new_rx_rast = make_zero_raster()
selected_patch_mask = np.isin(topo_asp_rast, topo_asp_selected_df['Patch'])
new_rx_rast[selected_patch_mask] = 1  # burn the topo-asp patches selected via the process above
new_rx_rast[rx_zone_rast == 0] = 0
new_rx_rast[fire_cooldown_rast > 0] = 0

with rasterio.open(magic_harvest_dir + "RxIgnProb_dynamic.tif", 'w', **kwargs) as dst:
    dst.write(new_zones_r, 1)
