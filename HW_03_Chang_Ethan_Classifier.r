# One-Rule Threshold Classifier

BEST_FEATURE <- "LANE_CHANGES"
BEST_THRESHOLD <- 2L
BEST_FLAG_LEFT <- FALSE

args <- commandArgs(trailingOnly = TRUE)

# Makes sure program has 1 argument. 
if (length(args) < 1) stop("Usage: Rscript HW_03_Chang_Ethan_Classifier.r <test_suite.csv>")

# args[1] = TEST_SUITE_33
fname <- args[1]
fname <- file.path("TEST_SUITE_33", fname)

df <- read.csv(fname, stringsAsFactors = FALSE)

all_speeds <- trunc(df$SPEED)
all_lane_change <- df$LANE_CHANGES
all_brightness <- df$BRIGHTNESS_TRUNC

if (BEST_FEATURE == "SPEED") {
    x <- all_speeds 
   } else if (BEST_FEATURE == "LANE_CHANGES") {
    x <- all_lane_change
   } else {
    x <- all_brightness
   }

if (BEST_FLAG_LEFT) {
   pred_aggr <- (x <= BEST_THRESHOLD) 
  } else {
   pred_aggr <- (x > BEST_THRESHOLD)
  }

# Outputs the results 
cat('Aggresive: ', sum(pred_aggr), '\n')
cat('Non-Aggresive: ', sum(!pred_aggr), '\n')
