data <- read.csv("data/dataset.csv")

df_only_immigrants <- data[data$Is_Immigrant == 1, ]
summary(df_only_immigrants)
sd(df_only_immigrants$RIDAGEYR)

summary(df_only_immigrants$Has_CVD==1)
immigrants_high <- df_only_immigrants[df_only_immigrants$LBXBPB>=3.5 & df_only_immigrants$Has_CVD==1, ]
summary(immigrants_high)
immigrants_high2 <- df_only_immigrants[df_only_immigrants$LBXBPB>=3.5 & df_only_immigrants$Has_CVD==0, ]
summary(immigrants_high2)

immigrants_nothigh <- df_only_immigrants[df_only_immigrants$LBXBPB<3.5 & df_only_immigrants$Has_CVD==1, ]
summary(immigrants_nothigh)
immigrants_nothigh2 <- df_only_immigrants[df_only_immigrants$LBXBPB<3.5 & df_only_immigrants$Has_CVD==0, ]
summary(immigrants_nothigh2)

df_only_nonimmigrants <- data[data$Is_Immigrant == 0, ]
summary(df_only_nonimmigrants)
sd(df_only_nonimmigrants$RIDAGEYR)

summary(df_only_nonimmigrants$Has_CVD==1)

nonimmigrants_high <- df_only_nonimmigrants[df_only_nonimmigrants$LBXBPB>=3.5 & df_only_nonimmigrants$Has_CVD==1, ]
summary(nonimmigrants_high)
nonimmigrants_high2 <- df_only_nonimmigrants[df_only_nonimmigrants$LBXBPB>=3.5 & df_only_nonimmigrants$Has_CVD==0, ]
summary(nonimmigrants_high2)

nonimmigrants_nothigh <- df_only_nonimmigrants[df_only_nonimmigrants$LBXBPB<3.5 & df_only_nonimmigrants$Has_CVD==1, ]
summary(nonimmigrants_nothigh)
nonimmigrants_nothigh2 <- df_only_nonimmigrants[df_only_nonimmigrants$LBXBPB<3.5 & df_only_nonimmigrants$Has_CVD==0, ]
summary(nonimmigrants_nothigh2)

# chi square for  immigrants:
# Dynamically create matrix 'a' using row counts
a <- matrix(c(nrow(immigrants_high),    nrow(immigrants_high2),
              nrow(immigrants_nothigh), nrow(immigrants_nothigh2)),
            byrow=TRUE, nrow=2)


print(a)
# fisher.test(a) -- used since error message that chi sq approximation may be incorrect -- possible reason that some exp value is
# less than 5 (seen); however, even larger p value here possibly cuz more conservative
# gonna stick w chi sq for this test
chisq.test(a,correct = F)
# don't need to correct since large enough 'n', hence correct is 'F'
chisq.test(a,correct=F)$expected # can use this table
# p = 0.168; can't reject null
# null: incidence of CVD risk in immigrants w Pb > cut off is equal to the incidence
# of cvd risk in immigrants w Pb < cut off

# chi square null: incidence of CVD in immigrants with high Pb is equal to incidence of CVD in non-immigrants w high Pb
# Dynamically create the matrix using the row counts of your subset dataframes
b <- matrix(c(nrow(immigrants_high),    nrow(immigrants_high2), 
              nrow(nonimmigrants_high), nrow(nonimmigrants_high2)), 
            byrow=TRUE, nrow=2)

# Print b to verify it matches
print(b)
chisq.test(b,correct=F)
# p = 0.039; can reject null and accept alternative


