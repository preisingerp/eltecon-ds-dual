# library(readxl)
library(data.table)
#library(chron)
library(ggplot2)

# skater_stats <- read_excel("nhl_data/nhl_skaterstats.xlsx", sheet = 1)
# player_list <- read_excel("nhl_data/nhl_skaterstats.xlsx", sheet = 2)
# team_list <- read_excel("nhl_data/nhl_skaterstats.xlsx", sheet = 3)
# pos_list <- read_excel("nhl_data/nhl_skaterstats.xlsx", sheet = 4)
skater_stats <- fread("../nhl_data/skater_stats.csv")

# summary(skater_stats)

# Delete col "V1"
skater_stats$V1 <- NULL

### need to remove two person with AGE NA!!!
### possibly need to rework conversion and 0/NA change into function

# Rename columnes: "+/-", "S%", "FO%"
setnames(skater_stats, c("+/-", "S%", "FO%"), c("PlusMinus", "S_perc", "FO_perc"))

# Delete data before 2008, because some data was not collected before
skater_stats <- skater_stats[Season >= 2008]

# Converting to numeric where neccesary
cols_to_convert <- c("G", "PTS", "PlusMinus", "A", "PIM", "EVG", "PPG", "SHG", "GWG", "EVA", "PPA", "SHA", "S", "S_perc")
skater_stats[, (cols_to_convert) := lapply(.SD, as.numeric), .SDcols = cols_to_convert]

# Converting to times
# skater_stats[, ATOI := paste0("00:", ATOI)]                # needs to be converted to hours first
# skater_stats[, ATOI := times(ATOI)]                        # special numeric type for time
# skater_stats$TOI = skater_stats$ATOI*skater_stats$GP     # setting TOI from ATOI and GP to have time format

skater_stats$TOI = gsub(",", "", skater_stats$TOI)         # delete ","-s 
skater_stats[, TOI := as.numeric(TOI)]                     #in case time format is not needed
skater_stats[, ATOI := TOI/GP]

# Changing between NA and 0 to make sense (percentages to NA if there is division with 0, and FOwin/loss to NA if both are 0)
skater_stats$G[is.na(skater_stats$G)] <- 0
skater_stats$A[is.na(skater_stats$A)] <- 0
skater_stats$PTS[is.na(skater_stats$PTS)] <- 0
skater_stats$PlusMinus[is.na(skater_stats$PlusMinus)] <- 0
skater_stats$PIM[is.na(skater_stats$PIM)] <- 0
skater_stats$EVG[is.na(skater_stats$EVG)] <- 0
skater_stats$PPG[is.na(skater_stats$PPG)] <- 0
skater_stats$SHG[is.na(skater_stats$SHG)] <- 0
skater_stats$GWG[is.na(skater_stats$GWG)] <- 0
skater_stats$EVA[is.na(skater_stats$EVA)] <- 0
skater_stats$PPA[is.na(skater_stats$PPA)] <- 0
skater_stats$SHA[is.na(skater_stats$SHA)] <- 0
skater_stats$S[is.na(skater_stats$S)] <- 0
skater_stats$S_perc[skater_stats$S == 0] <- NA
skater_stats$BLK[is.na(skater_stats$BLK)] <- 0
skater_stats$HIT[is.na(skater_stats$HIT)] <- 0
skater_stats$FOwin[is.na(skater_stats$FO_perc)] <- NA
skater_stats$FOloss[is.na(skater_stats$FO_perc)] <- NA

write.csv(skater_stats, file = "../nhl_data/clean_nhl.csv")

summary(skater_stats)

# Plots
#ggplot(skater_stats, aes(TOI, PTS)) + geom_point() + scale_x_chron()
#ggplot(skater_stats, aes(FOwin + FOloss, FO_perc)) + geom_point()
