library(data.table)
library(magrittr)
library(ggplot2)
library(purrr)

# Data cleaning and organizing
skater_stats <- fread("nhl_data/skater_stats.csv")
skater_stats <- skater_stats[Season >= 2008]
skater_stats <- skater_stats[!(Pos == "RW/LW")]
skater_stats <- skater_stats[!(Pos == "LW/RW")]
skater_stats <- skater_stats[!(Pos == "RW/C")]
skater_stats <- skater_stats[!(Pos == "C/LW")]
skater_stats <- skater_stats[!(Pos == "LW/C")]
skater_stats[, PTS := as.numeric(PTS)]
skater_stats$PTS[is.na(skater_stats$PTS)] <- 0

# work_data <- skater_stats[, .(points_scored = mean(PTS)), by = Pos]

# Creating sample
set.seed(1234)
bootstrapped_stats <- map_df(1:1000, ~{
  skater_stats[sample(.N, .N, replace = TRUE)] %>%
    .[,
      .(bootstrap_id = .x,
        points_scored = mean(PTS)),
      by = Pos
      ]
})

# Calculating uncertainty
temp = bootstrapped_stats[Pos == "C"]
CI_l_C = quantile(temp$points_scored, 0.025)
CI_h_C = quantile(temp$points_scored, 0.975)
temp = bootstrapped_stats[Pos == "LW"]
CI_l_LW = quantile(temp$points_scored, 0.025)
CI_h_LW = quantile(temp$points_scored, 0.975)
temp = bootstrapped_stats[Pos == "RW"]
CI_l_RW = quantile(temp$points_scored, 0.025)
CI_h_RW = quantile(temp$points_scored, 0.975)
temp = bootstrapped_stats[Pos == "D"]
CI_l_D = quantile(temp$points_scored, 0.025)
CI_h_D = quantile(temp$points_scored, 0.975)


# Creating plot, with confidence intervals
ggplot(bootstrapped_stats, aes(points_scored, fill = as.factor(Pos))) + 
  geom_histogram(alpha = 0.7, position = "dodge", bins = 50) +
  theme_minimal() +
  labs(x = "Average points scored", y = "Number of players") +
  geom_vline(xintercept = CI_l_C, color = "#fa9f99", linetype = "dashed") +
  geom_vline(xintercept = CI_h_C, color = "#fa9f99", linetype = "dashed") +
  geom_vline(xintercept = CI_l_D, color = "#a3c64c", linetype = "dashed") +
  geom_vline(xintercept = CI_h_D, color = "#a3c64c", linetype = "dashed") +
  geom_vline(xintercept = CI_l_LW, color = "#4cd2d6", linetype = "dashed") +
  geom_vline(xintercept = CI_h_LW, color = "#4cd2d6", linetype = "dashed") +
  geom_vline(xintercept = CI_l_RW, color = "#d8a3ff", linetype = "dashed") +
  geom_vline(xintercept = CI_h_RW, color = "#d8a3ff", linetype = "dashed") +
  scale_fill_discrete(name = "Position", labels = c("Center", "Defenseman", "Leftwing", "Rightwing"))

# ggsave("homework3_img.png")
