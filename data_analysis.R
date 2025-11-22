data <- read.csv("data/dataset.csv")

## Plots for Lead Pb ##

pb_val = data$LBXBPB

min_val <- floor(min(pb_val, na.rm = TRUE))
max_val <- ceiling(max(pb_val, na.rm = TRUE))

breaks_vec <- seq(from = min_val, to = max_val, by = 0.1)

hist(pb_val, 
     breaks = breaks_vec,
     main = "Histogram of LBXBPB (Step 0.1)",
     xlab = "Blood Lead Level",
     las = 1)

log_pb_val <- log10(pb_val)
range(log_pb_val, na.rm = TRUE)
min_log <- floor(min(log_pb_val, na.rm = TRUE))
max_log <- ceiling(max(log_pb_val, na.rm = TRUE))

breaks_log <- seq(from = min_log, to = max_log, by = 0.1)

hist(log_pb_val, 
     breaks = breaks_log,
     main = "Histogram of Log10(LBXBPB)",
     xlab = "Log10 Blood Lead Level",
     col = "lightblue",
     las = 1)

## Plots for Mercury Hg ##

hg_val = data$LBXTHG

min_val <- floor(min(hg_val, na.rm = TRUE))
max_val <- ceiling(max(hg_val, na.rm = TRUE))

breaks_vec <- seq(from = min_val, to = max_val, by = 0.1)

hist(hg_val, 
     breaks = breaks_vec,
     main = "Histogram of LBXTHG (Step 0.1)",
     xlab = "Blood Mercury Level",
     las = 1)

log_hg_val <- log10(hg_val)
range(log_hg_val, na.rm = TRUE)
min_log <- floor(min(log_hg_val, na.rm = TRUE))
max_log <- ceiling(max(log_hg_val, na.rm = TRUE))

breaks_log <- seq(from = min_log, to = max_log, by = 0.1)

hist(log_hg_val, 
     breaks = breaks_log,
     main = "Histogram of Log10(LBXTHG)",
     xlab = "Log10 Blood Mercury Level",
     col = "lightgreen",
     las = 1)