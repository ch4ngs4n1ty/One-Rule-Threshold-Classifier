# --- learned constants from training ---
BEST_FEATURE   <- "SPEED"      # replace with your winning attribute name
BEST_TAU       <- 60L          # replace with your winning integer threshold
BEST_FLAG_LEFT <- FALSE         # TRUE: predict aggressive on <=; FALSE: on >

# --- read test csv from command line ---
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript HW_03_Chang_Ethan_Classifier.r <test_suite.csv>")
test_path <- args[1]
df <- read.csv(test_path, stringsAsFactors = FALSE)

# --- preprocessing: EXACTLY like training ---
df$SPEED_TRUNC        <- trunc(df$SPEED)
df$LANE_CHANGES_TRUNC <- trunc(df$LANE_CHANGES)
df$BRIGHTNESS_TRUNC   <- trunc(df$BRIGHTNESS)

# --- choose the attribute column based on BEST_FEATURE ---
feat <- toupper(BEST_FEATURE)
x <- switch(feat,
  "SPEED"        = df$SPEED_TRUNC,
  "LANE_CHANGES" = df$LANE_CHANGES_TRUNC,
  "BRIGHTNESS"   = df$BRIGHTNESS_TRUNC,
  stop("Unknown BEST_FEATURE: ", BEST_FEATURE)
)

# --- classify using the learned threshold + direction ---
pred_aggr <- if (BEST_FLAG_LEFT) (x <= BEST_TAU) else (x > BEST_TAU)

# --- required outputs: two counts (each on its own line) ---
num_aggressive     <- sum(pred_aggr,  na.rm = TRUE)
num_non_aggressive <- sum(!pred_aggr, na.rm = TRUE)

cat(num_aggressive, "\n")
cat(num_non_aggressive, "\n")
