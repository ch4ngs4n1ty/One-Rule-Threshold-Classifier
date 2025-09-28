# HW_03 One-Rule Threshold Classifier
# Author: Ethan Chang
# Course: CSCI 420
# Compile: Rscript HW_03_Chang_Ethan.r

main_folder <- "HW_03_Data" #Initial main folder containing subfolders with csv files

#R function list.files() lists all files in a directory.
#Finds files that include subtext of Data_##.csv and use recursive to look all of the subfolders
csvs <- list.files(main_folder, pattern = "Data_.*\\.csv", recursive = TRUE, full.names = TRUE)

dfs <- lapply(csvs, read.csv) #lapply() applies function for each element of vectors and returns whole list of subfolders

data_all <- do.call(rbind, dfs) #Combine all data frames into one data frame

all_speed_nontruncated <- data_all$SPEED # Pull all float values of speed
all_intent <- data_all$INTENT # Pull all intent values of each individual

##############################################################################
# Attribute: Speed
# Data Type: Float
# Must be truncated into whole numbers
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

# Loop representing attribute SPEED to collect all of speed FPR, FNR, flag, and badness values
# Note: From reading the write up, it said "For each possible attribute", so my approach is set up
# for loops for each individual attribute to get the data we need.
# It could be complicated if we use one loop for all of the attributes.
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

    if (s_flag[individual]) { 

        prediction <- all_speeds <= ind_t # If flagged, that means we pull left side of data

    } else {

        prediction <- all_speeds > ind_t # If non-flagged, that means we pull right side of data

    }
                                                        # Prediction , Truth
    TP <- sum(prediction & truth == 1L) # True Positive (Aggressive, Aggressive)
    TN <- sum(!prediction & truth == 0L) # True Negative (Not Aggresive, Not Aggresive)
    FP <- sum(prediction & truth == 0L) # False Positive (Aggresive, Not Aggresive) (False Alarm) (Number of Non-aggressive drivers that are incorrectly identified)
    FN <- sum(!prediction & truth == 1L) # False Negative (Not Aggresive, Aggresive) (False Identification) (Number of aggressive drivers who are incorrectly identified)

    if ((FP + TN) > 0) {

        s_FPR[individual] <- FP / (FP + TN) # Speed False Positive Rate Value

    } else {

        # NA-real_ means not defined answer
        s_FPR[individual] <- NA_real_ # Setting it to 0 wouldn't make sense since 0 can represent a value, so we set it to NA_real

    }

    if ((FN + TP) > 0) {

        s_FNR[individual] <- FN / (FN + TP) # Speed Negative Positive Rate Value

    } else {

        # NA-real_ means not defined answer
        s_FNR[individual] <- NA_real_ # Setting it to 0 wouldn't make sense since 0 can represent a value, so we set it to NA_real

    }

    s_badness[individual] <- s_FPR[individual] + s_FNR[individual] # Speed Badness sum value of two error rates of a threshold

}

s_index <- which.min(s_badness)             # Index of minimum badness for brightness
s_threshold_min <- s_threshold[s_index]     # Best brightness threshold
s_flag_min <- s_flag[s_index]               # Best brightness flag
s_badness_min <- s_badness[s_index]         # Best brightness badness

##############################################################################
# Attribute: LANE_CHANGES
# Data Type: Whole Number
# Doesn't need to be truncated since they are integers 
##############################################################################
 
all_lane_change <- data_all$LANE_CHANGES             # Lane change count
lc_min_threshold <- min(all_lane_change)             # Lowest lane change count of all data
lc_max_threshold <- max(all_lane_change)             # Highest lane change count of all data
lc_threshold <- lc_min_threshold:lc_max_threshold    # Minimum to maximum lane change threshold
lc_FPR <- numeric(length(lc_threshold))              # Lane Changes False Positive Rate
lc_FNR <- numeric(length(lc_threshold))              # Lane Changes False Negative Rate
lc_flag <- logical(length(lc_threshold))             # Lane Changes Flag
lc_badness <- numeric(length(lc_threshold))          # Lane Changes Badness

# Loop representing attribute LANE_CHANGES to collect all of speed FPR, FNR, flag, and badness values
# Note: From reading the write up, it said "For each possible attribute", so my approach is set up
# for loops for each individual attribute to get the data we need.
# It could be complicated if we use one loop for all of the attributes.
for (individual in seq_along(lc_threshold)) {
    
    ind_t <- lc_threshold[individual]

    # Find majority class of data that is <= the threshold value
    left_group <- truth[all_lane_change <= ind_t]

    # Mainly aggressive drivers are <= the threshold and aggresive means 1
    left_group_agg <- sum(left_group == 1L)
    
    # Group of non-aggressive drivers with truth value of 0
    left_group_nagg <- sum(left_group == 0L)

    # Set flag if there are more aggresive drivers than non-aggresive drivers
    if (left_group_agg > left_group_nagg) { 

        lc_flag[individual] <- TRUE 

    } else {

        lc_flag[individual] <- FALSE

    }

    if (lc_flag[individual]) { # If flagged, that means we pull left side of data

        prediction <- all_lane_change <= ind_t

    } else {

        prediction <- all_lane_change > ind_t # If non-flagged, that means we pull right side of data

    }

                                                        # Prediction , Truth
    TP <- sum(prediction & truth == 1L) # True Positive (Aggressive, Aggressive)
    TN <- sum(!prediction & truth == 0L) # True Negative (Not Aggresive, Not Aggresive)
    FP <- sum(prediction & truth == 0L) # False Positive (Aggresive, Not Aggresive) (False Alarm) (Number of Non-aggressive drivers that are incorrectly identified)
    FN <- sum(!prediction & truth == 1L) # False Negative (Not Aggresive, Aggresive) (False Identification) (Number of aggressive drivers who are incorrectly identified)

    if ((FP + TN) > 0) {

        lc_FPR[individual] <- FP / (FP + TN) # Lane Change FPR individual value

    } else {

        lc_FPR[individual] <- NA_real_ # Setting it to 0 wouldn't make sense since 0 can represent a value, so we set it to NA_real

    }

    if ((FN + TP) > 0) {

        lc_FNR[individual] <- FN / (FN + TP) # Lane Change FNR individual value

    } else {

        lc_FNR[individual] <- NA_real_ # Setting it to 0 wouldn't make sense since 0 can represent a value, so we set it to NA_real

    }

    lc_badness[individual] <- lc_FPR[individual] + lc_FNR[individual] # Lane Change Badness sum value of two error rates of a threshold

}

lc_index <- which.min(lc_badness)              # Index of minimum badness for lane changes
lc_threshold_min <- lc_threshold[lc_index]     # Best lane change threshold
lc_flag_min <- lc_flag[lc_index]               # Best lane change flag
lc_badness_min <- lc_badness[lc_index]         # Best lane change badness

############################################################################## 
# Attribute: BRIGHTNESS
# Data Type: Whole Number
##############################################################################

all_brightness <- data_all$BRIGHTNESS               # Brightness values
b_min_threshold <- min(all_brightness)              # Lowest brightness in all data
b_max_threshold <- max(all_brightness)              # Highest brightness in all data
b_threshold <- b_min_threshold:b_max_threshold      # Minimum to maximum brightness threshold (integers)

b_FPR <- numeric(length(b_threshold))               # Brightness False Positive Rate
b_FNR <- numeric(length(b_threshold))               # Brightness False Negative Rate
b_flag <- logical(length(b_threshold))              # Brightness flag
b_badness <- numeric(length(b_threshold))           # Brightness badness


# Loop representing attribute BRIGHTNESS to collect all of speed FPR, FNR, flag, and badness values
# Note: From reading the write up, it said "For each possible attribute", so my approach is set up
# for loops for each individual attribute to get the data we need.
# It could be complicated if we use one loop for all of the attributes.
for (individual in seq_along(b_threshold)) {

    ind_t <- b_threshold[individual]

    # Find majority class of data that is <= the threshold value
    left_group <- truth[all_brightness <= ind_t]

    # Mainly aggressive drivers are <= the threshold and aggresive means 1
    left_group_agg <- sum(left_group == 1L)
    
    # Group of non-aggressive drivers with truth value of 0
    left_group_nagg <- sum(left_group == 0L)

    # Set flag if there are more aggresive drivers than non-aggresive drivers
    if (left_group_agg > left_group_nagg) {

        b_flag[individual] <- TRUE

    } else {

        b_flag[individual] <- FALSE

    }

    if (b_flag[individual]) { # If flagged, that means we pull left side of data

        prediction <- all_brightness <= ind_t

    } else {

        prediction <- all_brightness > ind_t # If non-flagged, that means we pull right side of data

    }

                                                        # Prediction , Truth
    TP <- sum(prediction & truth == 1L) # True Positive (Aggressive, Aggressive)
    TN <- sum(!prediction & truth == 0L) # True Negative (Not Aggresive, Not Aggresive)
    FP <- sum(prediction & truth == 0L) # False Positive (Aggresive, Not Aggresive) (False Alarm) (Number of Non-aggressive drivers that are incorrectly identified)
    FN <- sum(!prediction & truth == 1L) # False Negative (Not Aggresive, Aggresive) (False Identification) (Number of aggressive drivers who are incorrectly identified)


    if ((FP + TN) > 0) {

        b_FPR[individual] <- FP / (FP + TN)

    } else {

        b_FPR[individual] <- NA_real_ # Setting it to 0 wouldn't make sense since 0 can represent a value, so we set it to NA_real

    }

    if ((FN + TP) > 0) {

        b_FNR[individual] <- FN / (FN + TP)

    } else {
 
        b_FNR[individual] <- NA_real_# Setting it to 0 wouldn't make sense since 0 can represent a value, so we set it to NA_real

    }

    b_badness[individual] <- b_FPR[individual] + b_FNR[individual] # Brightness Badness sum value of two error rates of a threshold

}

b_index <- which.min(b_badness)           # Index of minimum badness for brightness
b_threshold_min <- b_threshold[b_index]   # Best brightness threshold
b_flag_min <- b_flag[b_index]             # Best brightness flag
b_badness_min <- b_badness[b_index]       # Best brightness badness


##############################################################################
# Data Result Showcasing
# Creates a new classifier script to test with Data_33.csv
##############################################################################

# Representation of all attributes minimum badness values
attr_badness <- c(speed = s_badness_min, lane_changes = lc_badness_min, brightness = b_badness_min)

best_attr <- names(which.min(attr_badness)) # Gets the best attribute option

if (best_attr == "speed") { # Gets minimum badness speed values

    best_feature <- "SPEED"      
    best_threshold <- as.integer(s_threshold_min) 
    best_flag <- s_flag_min

} else if (best_attr == "lane_changes") { # Gets minimum badness number of lane change values

    best_feature <- "LANE_CHANGES"
    best_threshold <- as.integer(lc_threshold_min)
    best_flag <- lc_flag_min

} else {
 
    best_feature <- "BRIGHTNESS" # Gets minimum badness brightness values
    best_threshold <- as.integer(b_threshold_min)
    best_flag <- b_flag_min

}              

classifier_path <- file.path(getwd(), "HW_03_Chang_Ethan_Classifier.r") # Creates new classifier file 

# Write programmming script to the Classifier file
cls <- c(
  "# One-Rule Threshold Classifier",
  "# Ethan Chang",
  "# CSCI 420",
  "# Compile: Rscript HW_03_Chang_Ethan_Classifier.r Data_33.csv",
  "",
  sprintf('BEST_FEATURE <- "%s"', toupper(best_feature)),
  sprintf("BEST_THRESHOLD <- %dL", as.integer(best_threshold)),
  sprintf("BEST_FLAG_LEFT <- %s", if (best_flag) "TRUE" else "FALSE"),
  "",
  "args <- commandArgs(trailingOnly = TRUE)",
  "",
  "# Makes sure program has 1 argument. ",
  'if (length(args) < 1) stop("Usage: Rscript HW_03_Chang_Ethan_Classifier.r <test_suite.csv>")',
  "",
  "# args[1] = TEST_SUITE_33",
  "fname <- args[1]",
  'fname <- file.path("TEST_SUITE_33", fname)',
  "",
  "df <- read.csv(fname, stringsAsFactors = FALSE)",
  "",
  "all_speeds <- trunc(df$SPEED)",
  "all_lane_change <- df$LANE_CHANGES",
  "all_brightness <- df$BRIGHTNESS",
  "",
  'if (BEST_FEATURE == "SPEED") {
    x <- all_speeds 
   } else if (BEST_FEATURE == "LANE_CHANGES") {
    x <- all_lane_change
   } else {
    x <- all_brightness
   }',
  "",
  "if (BEST_FLAG_LEFT) {
   pred_aggr <- (x <= BEST_THRESHOLD) 
  } else {
   pred_aggr <- (x > BEST_THRESHOLD)
  }",
  "",
  "# Outputs the results ",
  "cat('Aggresive: ', sum(pred_aggr), '\\n')",
  "cat('Non-Aggresive: ', sum(!pred_aggr), '\\n')"
)

writeLines(cls, con = classifier_path) # Writes the following script into the classifier file



