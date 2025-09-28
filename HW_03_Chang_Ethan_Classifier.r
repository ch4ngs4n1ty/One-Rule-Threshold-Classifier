# One-Rule Threshold Classifier (auto-generated)
BEST_FEATURE <- "LANE_CHANGES"
BEST_TAU <- 2L
BEST_FLAG_LEFT <- FALSE

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript HW_03_Chang_Ethan_Classifier.r <test_suite.csv>")

fname <- args[1]
fname <- file.path("TEST_SUITE_33", fname)

df <- read.csv(fname, stringsAsFactors = FALSE)

all_speeds <- trunc(df$SPEED)
all_lane_change <- df$LANE_CHANGES
all_brightness <- df$BRIGHTNESS_TRUNC

x <- if (BEST_FEATURE == "SPEED") all_speeds else if (BEST_FEATURE == "LANE_CHANGES") all_lane_change else all_brightness

if (BEST_FLAG_LEFT) {
   pred_aggr <- (x <= BEST_TAU) 
  } else {
   pred_aggr <- (x > BEST_TAU)
  }

cat(sum(pred_aggr), '\n')
cat(sum(!pred_aggr), '\n')
