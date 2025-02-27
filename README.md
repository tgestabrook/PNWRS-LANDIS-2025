# PNWRS-LANDIS-2025

## Notes
- Files shared across AOI are kept in the main directory. Diverging files are kept in their respective AOI
    - Scrapple: ignitions, spread, and severity all calibrated to AOI
    - NECN: input maps differ, species `MaxBiomass` differs within `species.csv`.
- Scenario files are generic w/r/t AOI and climate scenario.
    - `EXTENT` and `CLIMATE` placeholder, respectively.
- Model runs output to main directory
- Model run scripts take inputs:
    1. Scenario file (must start with `scenario_` and end with `.txt`; whatever is in the middle will be used as the scenario name)
    2. Extent ('OkaMet' or 'WenEnt')
    3. Climate ('baseclim' or 'rcp85')
    4. Number of repeated simulations (can be 1 for a single simulation)

## Examples
Run five calibration tests for Wenatchee-Entiat:
```shell
./_RUN_SIM.sh 1 "WenEnt" "baseclim" 5
```