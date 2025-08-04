# Set directories
GazeDir <- "C:/Users/mitra/Desktop/SP/CoRA/Analysis/Data/Gaze/"
BehDir <- "C:/Users/mitra/Desktop/SP/CoRA/Analysis/Data/Behavioural/"

# Specify participant ID (e.g., "P001")
participant_id <- "P001"

# List all gaze files for the participant
participant_gaze_files <- list.files(path = file.path(GazeDir, participant_id),
                                     pattern = "\\.csv$", full.names = TRUE)

# Check that there are exactly two files
if (length(participant_gaze_files) != 2) {
  stop(paste("Expected 2 gaze files for", participant_id, "but found", length(participant_gaze_files)))
}

# Read the two gaze files
gaze1 <- read.csv(participant_gaze_files[1])
gaze2 <- read.csv(participant_gaze_files[2])

# Report row counts
cat("Rows in first file:", nrow(gaze1), "\n")
cat("Rows in second file:", nrow(gaze2), "\n")

# Combine the two data frames
gaze_combined <- rbind(gaze1, gaze2)

# Sort if needed (e.g., by Trial or Time column)
# gaze_combined <- gaze_combined[order(gaze_combined$Trial), ]  # Adjust column name as needed

# Report combined size
cat("Total rows after combining:", nrow(gaze_combined), "\n")

# Preview structure
cat("\nStructure of combined data:\n")
str(gaze_combined)

# Check for missing trials (assuming a Trial column exists)
if ("Trial" %in% names(gaze_combined)) {
  expected_trials <- min(gaze_combined$Trial):max(gaze_combined$Trial)
  missing_trials <- setdiff(expected_trials, unique(gaze_combined$Trial))
  
  if (length(missing_trials) > 0) {
    cat("Missing trial numbers:", paste(missing_trials, collapse = ", "), "\n")
  } else {
    cat("✅ No missing trials.\n")
  }
} else {
  warning("No 'Trial' column found to check for missing trials.")
}
