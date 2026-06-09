import os
import sys
import pandas as pd

# read in the scenario list
args = sys.argv[1:]
scenario_list_df = pd.read_csv("Scenario_list.csv")

# stupid but we gotta fix UTF encoding issue
scenario_list_df.rename(columns={scenario_list_df.columns[0]: "SCNUM"}, inplace=True)

# Select the scenario
selected_scnum = int(args[0]) if args[0].isdigit() else args[0]
# Handle potential data type mismatch (string vs integer) from CSV read
scenario_list_df["SCNUM"] = scenario_list_df["SCNUM"].astype(str)
selected_scenario = scenario_list_df[scenario_list_df["SCNUM"] == str(selected_scnum)]

# Ensure we found a match before proceeding
if not selected_scenario.empty:
    row = selected_scenario.iloc[0]
    
    # write a scenario.txt to model run directory
    modeldir = args[1]
    outfile = os.path.join(modeldir, "scenario.txt")
    
    with open(outfile, "w") as f:
        f.write(
            f"LandisData \"Scenario\"\n"
            f">> Scenario:{row['Scenario']}\n\n"
            f"Duration {row['Duration']}\n\n"
            f"Species {row['Species']}\n\n"
            f"Ecoregions ../EXTENT/ECOREGIONS_EXTENT.txt\n\n"
            f"EcoregionsMap ../EXTENT/ECOREGIONS_EXTENT.tif\n\n"
            f"CellLength 90 << meters\n\n"
            f">> Succession Extension Initialization File\n"
            f">> ---------------------- -------------------\n"
        )
        
        f.write(f"\"NECN Succession\" {row['NECN']}\n\n")
        
        # Check for non-null/non-NaN values
        has_scrpple = pd.notna(row.get("SCRPPLE"))
        has_harvest = pd.notna(row.get("Harvest"))
        has_mh = pd.notna(row.get("MH"))
        
        if has_scrpple or has_harvest:
            f.write(
                ">> Disturbance Extensions Initialization File\n"
                ">> ---------------------- -------------------"
            )
            
        if has_mh:
            f.write(f"\n\"Magic Harvest\" {row['MH']}")
            
        if has_scrpple:
            f.write(f"\n\"Social Climate Fire\" {row['SCRPPLE']}")
            
        if has_harvest:
            f.write(f"\n\"Biomass Harvest\" {row['Harvest']}")
            
        f.write(
            "\n\nDisturbancesRandomOrder no\n\n"
            ">> Other Extensions Initialization File\n"
            ">> ---------------------- -------------------\n"
            "\"Output Biomass-by-Age\" ../Shared_inputs/ext_Output_Biomass_by_Age.txt"
        )
else:
    print(f"Error: Scenario number {selected_scnum} not found.")
