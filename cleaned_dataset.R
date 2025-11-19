library(nhanesA)
library(tidyverse)

# Define the Cycle Suffix (e.g., '_J' is 2017-2018, '_I' is 2015-2016)
cycle_suffix <- "_J"

# ---------------------------------------------------------
# 2. Fetch Data
# ---------------------------------------------------------

# A. Demographics (Immigration Status)
demo_raw <- nhanes(paste0("DEMO", cycle_suffix)) %>%
  select(SEQN, 
         RIAGENDR, # Gender
         RIDAGEYR, # Age
         DMDBORN4, # Country of Birth (Immigration proxy)
         DMDCITZN, # Citizenship
         DMDYRSUS) # Years in the US

# B. Laboratory (Heavy Metals: Lead, Cadmium, Mercury)
# Note: Table name is usually 'PBCD' for Lead/Cadmium/Mercury
metals_raw <- nhanes(paste0("PBCD", cycle_suffix)) %>%
  select(SEQN, 
         LBXBPB,   # Blood Lead (ug/dL)
         LBXBCD,   # Blood Cadmium (ug/L)
         LBXTHG)   # Blood Mercury Total (ug/L)

# C. Questionnaire (Cardiovascular Outcomes)
# MCQ = Medical Conditions Questionnaire
cvd_raw <- nhanes(paste0("MCQ", cycle_suffix)) %>%
  select(SEQN, 
         MCQ160E,  # Heart Attack
         MCQ160F,  # Stroke
         MCQ160B,  # Heart Failure
         MCQ160C)  # Coronary Heart Disease

# ---------------------------------------------------------
# 3. Merge and Clean
# ---------------------------------------------------------

# Merge all tables by the unique identifier 'SEQN'
df_merged <- list(demo_raw, metals_raw, cvd_raw) %>%
  reduce(inner_join, by = "SEQN")

# Clean Variables
df_final <- df_merged %>%
  mutate(
    Is_Immigrant = if_else(
      (DMDBORN4 == "Born in 50 US states or Washington, DC"),
      0, 1, missing = 0
    ),
    
    # Create a Composite CVD Indicator (Any of the 4 conditions)
    # NHANES codes: 1=Yes, 2=No. We convert to 1=Yes, 0=No.
    Has_CVD = if_else(
      (MCQ160E == "Yes" | MCQ160F == "Yes" | MCQ160B == "Yes" | MCQ160C == "Yes"), 
      1, 0, missing = 0
    )
  ) %>%
  # Filter out missing values for heavy metals and age
  filter(!is.na(LBXBPB) & !is.na(LBXBCD) &!is.na(LBXTHG) & RIDAGEYR>=18)

# Check the final data structure
View(df_final)
