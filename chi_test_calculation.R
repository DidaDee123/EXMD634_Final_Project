library(tidyverse)
library(ggplot2)

data <- read.csv("data/dataset.csv")

data_cvd <- data %>%
  filter(!is.na(Is_Immigrant) & !is.na(Has_CVD)) %>%
  mutate(
    Immigrant_Label = factor(Is_Immigrant, 
                             levels = c(0, 1), 
                             labels = c("US Born", "Immigrant")),
    CVD_Label = factor(Has_CVD, 
                       levels = c(0, 1), 
                       labels = c("No Event", "Cardiac Event"))
  )

cvd_table <- table(data_cvd$Immigrant_Label, data_cvd$CVD_Label)

chisq_result <- chisq.test(cvd_table)

print("--- CARDIAC EVENT CONTINGENCY TABLE ---")
print(cvd_table)

print("--- CHI-SQUARE TEST RESULTS ---")
print(chisq_result)


prevalence <- prop.table(cvd_table, 1) * 100
print("--- PREVALENCE OF CVD (%) ---")
print(round(prevalence, 2))


plot_data <- data_cvd %>%
  group_by(Immigrant_Label, CVD_Label) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

plot_cvd <- ggplot(plot_data, aes(x = Immigrant_Label, y = Percentage, fill = CVD_Label)) +
  geom_bar(stat = "identity") +
  labs(title = "Prevalence of Cardiac Events",
       subtitle = "US Born vs. Immigrants",
       y = "Percentage (%)",
       x = "",
       fill = "Status") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), 
            size = 4)

print(plot_cvd)
ggsave("cvd_prevalence.png", plot_cvd, width = 6, height = 5)
