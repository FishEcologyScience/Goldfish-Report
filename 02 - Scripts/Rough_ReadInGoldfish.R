
############################################################
# 10_morphology_plots_field_only.R
# Purpose: Read a single raw FIELD dataset, do minimal cleaning,
#          and produce three basic plots:
#          (1) Fork Length (FL_mm) histogram
#          (2) Width (width_mm) histogram
#          (3) Mass (weight_g) histogram
#
# Style: Beginner-friendly, tidyverse-only, many inline comments.
# Run:   source("02_scripts/10_morphology_plots_field_only.R")
############################################################

# ------------ 0) Packages and species selection -----------------

# Load tidyverse for import, wrangling, and plotting
library(tidyverse)
# lubridate is optional—only used if a datetime column exists
library(lubridate)

# Choose which species to process (folder names depend on this)
species <- "Species"  # <-- change to "Carp", etc. when needed

# ------------ 1) Folder paths (match project_template layout) ----

# Input: raw field data folder (no lab/ages used here)
raw_field_path <- file.path("01 - Data", "Data", "HH_Rudd_GF_LengthWidths_2024.csv")

# Intermediate / processed data
processed_path <- file.path("01 - Data", "processed")

# Output folders
figs_path   <- file.path("03 - Plots", "01_figures")
tables_path <- file.path("03 - Plots", "01_tables") 

# Create folders if missing (safe if they already exist)
dir.create(processed_path, recursive = TRUE, showWarnings = FALSE)
dir.create(figs_path,      recursive = TRUE, showWarnings = FALSE)
dir.create(tables_path,    recursive = TRUE, showWarnings = FALSE)

# ------------ 2) File name (EDIT to your actual file) ------------

# Build the expected raw file path; change the pattern if your file is named differently
field_csv <- file.path("01 - Data", "Data", "HH_Rudd_GF_LengthWidths_2024.csv")

# Optional: quick message for confirmation
message("Reading field file: ", field_csv)

# ------------ 3) Import -----------------------------------------

# Read the raw field CSV
data_field <- read_csv(field_csv, show_col_types = FALSE)

# Quick peek (helps beginners see what was read)
message("Field data preview:")
print(head(data_field))

# ------------ 4) Minimal cleaning (SAFE & EXPLICIT) ---------------

# We will:
#  - ensure numeric columns are actually numeric
#  - (optionally) parse datetime if present
#  - keep as-is otherwise (simple, readable steps)

clean_df <- data_field %>%
 # Convert length/width/weight to numeric, if they aren't already
 mutate(
  ForkLength_mm    = suppressWarnings(as.numeric(ForkLength_mm)),
  Width_mm = suppressWarnings(as.numeric(Width_mm)),
  Mass_g = suppressWarnings(as.numeric(Mass_g))
 ) %>%
 # If a datetime column exists, try to parse it; otherwise leave untouched
 {
  if ("datetime" %in% names(.)) {
   mutate(., datetime = suppressWarnings(as.POSIXct(datetime)))
  } else {
   .
  }
 }

# Preview the cleaned data
message("Preview of cleaned data:")
print(head(clean_df))

# Save a processed copy (handy for re-use)
saveRDS(clean_df, file.path(processed_path, paste0(species, "_field_clean.rds")))

# ------------ 5) Basic summaries (QA) ----------------------------

summary_tbl <- clean_df %>%
 summarise(
  n_rows       = n(),
  n_FL         = sum(!is.na(ForkLength_mm)),
  n_width      = sum(!is.na(Width_mm)),
  n_weight     = sum(!is.na(Mass_g)),
  FL_mean_mm   = mean(ForkLength_mm,    na.rm = TRUE),
  width_mean_mm= mean(Width_mm, na.rm = TRUE),
  weight_mean_g= mean(Mass_g, na.rm = TRUE)
 )

message("Summary:")
print(summary_tbl)

# Save summary table
write_csv(summary_tbl, file.path(tables_path, paste0(species, "_morphology_summary_field_only.csv")))


############################################################
# 6) Scatterplots (Width vs Fork Length, and Width vs Mass)
############################################################

# We will make two simple scatterplots and save them as PNGs:
#   (a) Width (mm) vs Fork Length (mm)
#   (b) Width (mm) vs Mass (g)

# Before plotting, it's OK to remove rows where
# the required variables are missing (optional but tidy):

df_for_length <- clean_df %>%
 filter(!is.na(Width_mm), !is.na(ForkLength_mm))

df_for_mass <- clean_df %>%
 filter(!is.na(Width_mm), !is.na(Mass_g))

# -------------------------------
# 6a) WIDTH (Y) BY FORK LENGTH (X)
# -------------------------------

p_width_by_fork <- ggplot(df_for_length, aes(x = ForkLength_mm, y = Width_mm)) +
 geom_point(color = "#2c7fb8", alpha = 0.6, size = 2) +     # points with slight transparency
 labs(
  title = paste0(species, " - Width by Fork Length"),
  x = "Fork Length (mm)",
  y = "Width (mm)"
 ) +
 theme_minimal(base_size = 12)

# Save the scatterplot
ggsave(
 filename = file.path(figs_path, paste0(species, "_width_by_forklength.png")),
 plot = p_width_by_fork,
 width = 7, height = 5, dpi = 300
)

# --------------------------
# 6b) WIDTH (Y) BY MASS (X)
# --------------------------

p_width_by_mass <- ggplot(df_for_mass, aes(x = Mass_g, y = Width_mm)) +
 geom_point(color = "#f16913", alpha = 0.6, size = 2) +
 labs(
  title = paste0(species, " - Width by Mass"),
  x = "Mass (g)",
  y = "Width (mm)"
 ) +
 theme_minimal(base_size = 12)

# Save the scatterplot
ggsave(
 filename = file.path(figs_path, paste0(species, "_width_by_mass.png")),
 plot = p_width_by_mass,
 width = 7, height = 5, dpi = 300
)

message("Scatterplots saved to: ", figs_path)
``

