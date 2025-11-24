# --- STEP 1: PREPARATION & CATEGORIZATION ---
# Create the High vs Low Pb category (Cutoff = 3.5)
df_final$Pb_Category <- ifelse(df_final$LBXBPB >= 3.5, "High Pb", "Low Pb")

# Create your two main subsets
df_immigrants <- df_final[df_final$Is_Immigrant == 1, ]
df_nonimmigrants <- df_final[df_final$Is_Immigrant == 0, ]

# --- STEP 2: TEST A (Within Immigrants) ---
# Question: Does High Pb increase CVD risk specifically within immigrants?
# Null: CVD incidence is the same for High Pb and Low Pb immigrants.

print("--- TEST A: Immigrants (High Pb vs Low Pb) ---")
# Create table automatically
table_immigrants <- table(df_immigrants$Pb_Category, df_immigrants$Has_CVD)
print(table_immigrants)

# Check expected values to see if Chi-Sq is valid
chisq_check <- chisq.test(table_immigrants, correct=FALSE)
print("Expected Values:")
print(chisq_check$expected)

# Run Fisher's Exact Test (Better choice here since one expected value is likely < 5)
fisher_test_a <- fisher.test(table_immigrants)
print("Fisher's Exact Test Result:")
print(fisher_test_a)


# --- STEP 3: TEST B (Immigrants vs Non-Immigrants) ---
# Question: Among people with High Pb, do Immigrants have different CVD rates than Non-Immigrants?

print("--- TEST B: High Pb Population (Immigrants vs Non-Immigrants) ---")
# Subset only those with High Pb
df_high_pb <- df_final[df_final$LBXBPB >= 3.5, ]

# Create table: Rows = Immigrant Status, Cols = CVD Status
table_high_pb <- table(df_high_pb$Is_Immigrant, df_high_pb$Has_CVD)
print(table_high_pb)

# Run Chi-Square
chisq_test_b <- chisq.test(table_high_pb, correct=FALSE)
print(chisq_test_b)


# --- STEP 4: THE CONFOUNDER CHECK (Age) ---
# Question: Is the "Non-Immigrant" group significantly older? 
# (If they are older, that explains why they have more CVD, regardless of Lead).

print("--- CONFOUNDER CHECK: Age Difference ---")
# Compare age of High Pb Immigrants vs High Pb Non-Immigrants
immigrant_ages <- df_high_pb$RIDAGEYR[df_high_pb$Is_Immigrant == 1]
non_immigrant_ages <- df_high_pb$RIDAGEYR[df_high_pb$Is_Immigrant == 0]

t_test_age <- t.test(immigrant_ages, non_immigrant_ages)
print(t_test_age)

# Quick visualization of the means
print(paste("Mean Age Immigrants (High Pb):", round(mean(immigrant_ages, na.rm=T), 2)))
print(paste("Mean Age Non-Immigrants (High Pb):", round(mean(non_immigrant_ages, na.rm=T), 2)))