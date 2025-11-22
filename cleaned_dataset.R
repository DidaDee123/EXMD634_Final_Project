library(nhanesA)
library(tidyverse)

cycle_suffix <- "_J"

demo_raw <- nhanes(paste0("DEMO", cycle_suffix)) %>%
  select(SEQN, 
         RIAGENDR, 
         RIDAGEYR, 
         DMDBORN4, 
         DMDCITZN, 
         DMDYRSUS) 

metals_raw <- nhanes(paste0("PBCD", cycle_suffix)) %>%
  select(SEQN, 
         LBXBPB, 
         LBXBCD, 
         LBXTHG) 

cvd_raw <- nhanes(paste0("MCQ", cycle_suffix)) %>%
  select(SEQN, 
         MCQ160E, 
         MCQ160F, 
         MCQ160B, 
         MCQ160C) 

df_merged <- list(demo_raw, metals_raw, cvd_raw) %>%
  reduce(inner_join, by = "SEQN")

df_final <- df_merged %>%
  mutate(
    Is_Immigrant = if_else(
      (DMDBORN4 == "Born in 50 US states or Washington, DC"),
      0, 1, missing = 0
    ),
    Has_CVD = if_else(
      (MCQ160E == "Yes" | MCQ160F == "Yes" | MCQ160B == "Yes" | MCQ160C == "Yes"), 
      1, 0, missing = 0
    )
  ) %>%
  filter(!is.na(LBXBPB) & !is.na(LBXTHG)
         & !is.na(MCQ160E) & !is.na(MCQ160F)
         & !is.na(MCQ160B) & !is.na(MCQ160C)
         & MCQ160E != "Don't know" & MCQ160F != "Don't know" & 
           MCQ160B != "Don't know" & MCQ160C != "Don't know"
         & RIDAGEYR>=18)

View(df_final)

write.csv(df_final, "data/dataset.csv", row.names = TRUE)