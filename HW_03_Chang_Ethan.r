# HW_03 One-Rule Threshold Classifier
# Author: Ethan Chang
# Course: CSCI 420

main_folder <- "HW_03_Data" #Initial main folder containing subfolders with csv files

#R function list.files() lists all files in a directory.
#Finds files that include subtext of Data_##.csv and use recursive to look all of the subfolders
csvs <- list.files(main_folder, pattern = "Data_.*\\.csv", recursive = TRUE, full.names = TRUE)

dfs <- lapply(csvs, read.csv) #lapply() applies function for each element of vectors and returns whole list of subfolders

data_all <- do.call(rbind, dfs) #Combine all data frames into one data frame

all_speed_nontruncated <- data_all$SPEED
all_intent <- data_all$INTENT

##############################################################################
# Attribute: Speed
##############################################################################

# Note that Intent 2 represents aggresiveness while Intent 0 and 1 represents non aggresiveness
data_all$is_aggressive <- ifelse(data_all$INTENT %in% c(2), 1L, 0L) #If intent = 2, it's marked as aggressive. With intent = 0 or 1, it's non-aggressive
truth <- data_all$is_aggressive # Convert it into truth variable name for simplicity

all_speeds <- trunc(all_speed_nontruncated) # Truncates all of the speed data since it's natural data representation is a float
s_min_threshold <- min(all_speeds) # Lowest speed of all speed data
s_max_threshold <- max(all_speeds) # Highest speed of all speed data
s_threshold <- s_min_threshold:s_max_threshold # Minimum to Maximum speed threshold
s_FPR <- numeric(length(s_threshold)) # Speed False Positive Rate
s_FNR <- numeric(length(s_threshold)) # Speed False Negative Rate
s_flag <- logical(length(s_threshold)) # Speed Flag
s_badness <- numeric(length(s_threshold)) # Speed Badness

#Loop representing attribute SPEED to collect all of speed FPR, FNR, flag, and badness values
for (individual in seq_along(s_threshold)) {

    ind_t <- s_threshold[individual]

    # Find majority class of data that is <= the threshold value
    left_group <- truth[all_speeds <= ind_t]

    # Mainly aggressive drivers are <= the threshold and aggresive means 1
    left_group_agg <- sum(left_group == 1L)
    
    # Group of non-aggressive drivers with truth value of 0
    left_group_nagg <- sum(left_group == 0L)

    # Set flag if there are more aggresive drivers than non-aggresive drivers
    if (left_group_agg > left_group_nagg) {

        s_flag[individual] <- TRUE

    } else {

        s_flag[individual] <- FALSE

    }

    prediction <- if (s_flag[individual]) {

        all_speeds <= ind_t

    } else {

        all_speeds > ind_t

    }

    TP <- sum(prediction & truth == 1L)
    TN <- sum(!prediction & truth == 0L)
    FP <- sum(prediction & truth == 0L)
    FN <- sum(!prediction & truth == 1L)

    if ((FP + TN) > 0) {

        s_FPR[individual] <- FP / (FP + TN)

    } else {

        s_FPR[individual] <- NA_real_

    }

    if ((FN + TP) > 0) {

        s_FNR[individual] <- FN / (FN + TP)

    } else {

        s_FNR[individual] <- NA_real_

    }

    s_badness[individual] <- s_FPR[individual] + s_FNR[individual]

}

s_index <- which.min(s_badness)
s_threshold_min <- s_threshold[s_index]
s_flag_min <- s_flag[s_index]
s_badness_min <- s_badness[s_index]

##############################################################################
# Attribute: LANE_CHANGES
##############################################################################
all_lane_change <- data_all$LANE_CHANGES

lc_min_threshold <- min(all_lane_change)
lc_max_threshold <- max(all_lane_change)
lc_threshold <- lc_min_threshold:lc_max_threshold

lc_FPR <- numeric(length(lc_threshold))
lc_FNR <- numeric(length(lc_threshold))
lc_flag <- logical(length(lc_threshold))
lc_badness <- numeric(length(lc_threshold))

for (individual in seq_along(lc_threshold)) {
    
    ind_t <- lc_threshold[individual]

    left_group <- truth[all_lane_change <= ind_t]

    left_group_agg <- sum(left_group == 1L)
    
    left_group_nagg <- sum(left_group == 0L)

    if (left_group_agg > left_group_nagg) {

        lc_flag[individual] <- TRUE

    } else {

        lc_flag[individual] <- FALSE

    }

    prediction <- if (lc_flag[individual]) {

        all_lane_change <= ind_t

    } else {

        all_lane_change > ind_t

    }

    TP <- sum(prediction & truth == 1L)
    TN <- sum(!prediction & truth == 0L)
    FP <- sum(prediction & truth == 0L)
    FN <- sum(!prediction & truth == 1L)

    if ((FP + TN) > 0) {

        lc_FPR[individual] <- FP / (FP + TN)

    } else {

        lc_FPR[individual] <- NA_real_

    }

    if ((FN + TP) > 0) {

        lc_FNR[individual] <- FN / (FN + TP)

    } else {

        lc_FNR[individual] <- NA_real_

    }

    lc_badness[individual] <- lc_FPR[individual] + lc_FNR[individual]

}

lc_index <- which.min(lc_badness)
lc_threshold_min <- lc_threshold[lc_index]
lc_flag_min <- lc_flag[lc_index]
lc_badness_min <- lc_badness[lc_index]

############################################################################## 
# Attribute: BRIGHTNESS
##############################################################################

all_brightness <- data_all$BRIGHTNESS
b_min_threshold <- min(all_brightness)
b_max_threshold <- max(all_brightness)
b_threshold <- b_min_threshold:b_max_threshold

b_FPR <- numeric(length(b_threshold))
b_FNR <- numeric(length(b_threshold))
b_flag <- logical(length(b_threshold))
b_badness <- numeric(length(b_threshold))

for (individual in seq_along(b_threshold)) {

    ind_t <- b_threshold[individual]

    left_group <- truth[all_brightness <= ind_t]

    left_group_agg <- sum(left_group == 1L)
    
    left_group_nagg <- sum(left_group == 0L)

    if (left_group_agg > left_group_nagg) {

        b_flag[individual] <- TRUE

    } else {

        b_flag[individual] <- FALSE

    }

    prediction <- if (b_flag[individual]) {

        all_brightness <= ind_t

    } else {

        all_brightness > ind_t

    }

    TP <- sum(prediction & truth == 1L)
    TN <- sum(!prediction & truth == 0L)
    FP <- sum(prediction & truth == 0L)
    FN <- sum(!prediction & truth == 1L)

    if ((FP + TN) > 0) {

        b_FPR[individual] <- FP / (FP + TN)

    } else {

        b_FPR[individual] <- NA_real_

    }

    if ((FN + TP) > 0) {

        b_FNR[individual] <- FN / (FN + TP)

    } else {

        b_FNR[individual] <- NA_real_

    }

    b_badness[individual] <- b_FPR[individual] + b_FNR[individual]

}

b_index <- which.min(b_badness)
b_badness_threshold_min <- b_threshold[b_index]
b_flag_min <- b_flag[b_index]
b_badness_min <- b_badness[b_index]

##############################################################################
# Data Result Showcasing
##############################################################################

cands_bad <- c(speed = s_badness_min,
               lane_changes = lc_badness_min,
               brightness   = b_badness_min)

best_attr <- names(which.min(cands_bad))

if (best_attr == "speed") {

    best_feature <- "SPEED"      
    best_tau <- as.integer(s_threshold_min) 
    best_flag <- s_flag_min

} else if (best_attr == "lane_changes") {

    best_feature <- "LANE_CHANGES"
    best_tau <- as.integer(lc_threshold_min)
    best_flag <- lc_flag_min

} else {

    best_feature <- "BRIGHTNESS"
    best_tau <- as.integer(b_threshold_min)
    best_flag <- b_flag_min

}              

classifier_path <- file.path(getwd(), "HW_03_Chang_Ethan_Classifier.r")

cls <- c(
  "# One-Rule Threshold Classifier (auto-generated)",
  sprintf('BEST_FEATURE <- "%s"', toupper(best_feature)),
  sprintf("BEST_TAU <- %dL", as.integer(best_tau)),
  sprintf("BEST_FLAG_LEFT <- %s", if (best_flag) "TRUE" else "FALSE"),
  "",
  "args <- commandArgs(trailingOnly = TRUE)",
  'if (length(args) < 1) stop("Usage: Rscript HW_03_Chang_Ethan_Classifier.r <test_suite.csv>")',
  "",
  "fname <- args[1]",
  'fname <- file.path("TEST_SUITE_33", fname)',
  "",
  "df <- read.csv(fname, stringsAsFactors = FALSE)",
  "",
  "all_speeds <- trunc(df$SPEED)",
  "all_lane_change <- df$LANE_CHANGES",
  "all_brightness <- df$BRIGHTNESS_TRUNC",
  "",
  'x <- if (BEST_FEATURE == "SPEED") all_speeds else if (BEST_FEATURE == "LANE_CHANGES") all_lane_change else all_brightness',
  "",
  "if (BEST_FLAG_LEFT) {
   pred_aggr <- (x <= BEST_TAU) 
  } else {
   pred_aggr <- (x > BEST_TAU)
  }",
  "",
  "cat(sum(pred_aggr), '\\n')",
  "cat(sum(!pred_aggr), '\\n')"
)

writeLines(cls, con = classifier_path)



