# Ethan Chang
# CSCI 420


main_folder <- "HW_03_Data" #Initial main folder containing subfolders with csv files

#R function list.files() lists all files in a directory.
#Finds files that include subtext of Data_##.csv and use recursive to look all of the subfolders
csvs <- list.files(main_folder, pattern = "Data_.*\\.csv", recursive = TRUE, full.names = TRUE)

dfs <- lapply(csvs, read.csv) #lapply() applies function for each element of vectors and returns whole list of subfolders

data_all <- do.call(rbind, dfs) #Combine all data frames into one data frame

all_speed_nontruncated <- data_all$SPEED
all_lane_change <- data_all$LANE_CHANGES
all_brightness <- data_all$BRIGHTNESS
all_intent <- data_all$INTENT

all_speeds <- trunc(all_speed_nontruncated) # Truncates all of the speed data

#print(all_speeds)

data_all$is_aggressive <- ifelse(data_all$INTENT %in% c(2), 1L, 0L) #If intent = 2, it's marked as aggressive. With intent = 0 or 1, it's non-aggressive
truth <- data_all$is_aggressive

min_threshold <- min(all_speeds) # Lowest speed of all speed data
max_threshold <- max(all_speeds) # Highest speed of all speed data

threshold <- min_threshold:max_threshold

FPR <- numeric(length(threshold))
FNR <- numeric(length(threshold))
flag <- logical(length(threshold)) 
badness <- numeric(length(threshold))

for (individual in seq_along(threshold)) {

    ind_t <- threshold[individual]

    left_group <- truth[all_speeds <= ind_t]

    left_group_agg <- sum(left_group == 1L)
    
    left_group_nagg <- sum(left_group == 0L)

    if (left_group_agg > left_group_nagg) {

        flag[individual] <- TRUE

    } else {

        flag[individual] <- FALSE

    }

    predict_aggresive <- if (flag[individual]) {

        all_speeds <= ind_t

    } else {

        all_speeds > ind_t

    }

    TP <- sum(predict_aggresive & truth == 1L)
    TN <- sum(!predict_aggresive & truth == 1L)
    FP <- sum(predict_aggresive & truth == 0L)
    FN <- sum(!predict_aggresive & truth == 0L)

    if ((FP + TN) > 0) {

        FPR[individual] <- FP / (FP + TN)

    } else {

        FPR[individual] <- 0

    }

    if ((FN + TP) > 0) {

        FNR[individual] <- FN / (FN + TP)

    } else {

        FNR[individual] <- 0

    }

    badness[individual] <- FPR[individual] + FNR[individual]

}

print(FP)
print(TP)
print(FN)
print(TN)