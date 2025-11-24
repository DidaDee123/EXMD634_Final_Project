library(tidyverse)

data <- read.csv("data/dataset.csv")

data <- data %>%
  filter(!is.na(LBXBPB) & !is.na(Is_Immigrant)) %>%
  mutate(
    log_lead = log(LBXBPB),
    Immigrant_Label = factor(Is_Immigrant,
                             levels = c(0, 1),
                             labels = c("US Born", "Immigrant"))
  )

ttest_lead <- t.test(log_lead ~ Immigrant_Label, data = data, var.equal = FALSE)
print(ttest_lead)

geo_means_pb <- data %>%
  group_by(Immigrant_Label) %>%
  summarise(
    GeoMean_Lead = exp(mean(log_lead)), # since we log
    N = n()
  )
print(geo_means_pb)

ratio <- geo_means_pb$GeoMean_Lead[2] / geo_means_pb$GeoMean_Lead[1]
cat("\nRatio:", ratio)

plot_pb <- ggplot(data, aes(x = Immigrant_Label, y = log_lead, fill = Immigrant_Label)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Log-Blood Lead (Pb) Concentrations",
       y = "Log Lead (ug/dL)",
       x = "") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "none")

print(plot_pb)
ggsave("lead_comparison.png", plot_pb, width = 6, height = 5)