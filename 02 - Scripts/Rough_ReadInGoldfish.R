############################################################
# 10_morphology_plots_field_only.R  (Scatterplots Only)
#
# Purpose:
#   - Read species‑specific raw morphology dataset
#   - Clean numeric columns
#   - Produce a summary table
#   - Create TWO scatterplots:
#         (1) Width vs Fork Length  
#         (2) Width vs Mass
#
# Species-driven folder structure:
#   01 - Data/Species/<species>/
#   03 - Plots/
############################################################

# ------------ 0) Load packages ----------------------------

library(tidyverse)
library(readxl)
library(lubridate)

# ------------ 1) Select species ---------------------------

species <- "Rudd"   # Change this to "Carp", "Goldfish", etc.
species_folder <- species

# ------------ 2) Folder paths -----------------------------

raw_dir       <- file.path("01 - Data", "Species", species_folder)
processed_dir <- file.path("01 - Data", "processed", species_folder)
figs_dir      <- file.path("03 - Plots", "01_figures")
tables_dir    <- file.path("03 - Plots", "01_tables")

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figs_dir,      recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir,    recursive = TRUE, showWarnings = FALSE)

# ------ 3) Combine All Excel files for the selected species -------

# Detect all .xlsx files
raw_files <- list.files(raw_dir,
                        pattern = "\\.xlsx$",
                        ignore.case = TRUE,
                        full.names = TRUE)

if (length(raw_files) == 0) {
 stop("No Excel (.xlsx) files found in: ", raw_dir)
}

message("Found ", length(raw_files), " Excel files for ", species, ".")
message("Combining all files into one dataset...")

# Read each Excel file into a list
df_list <- lapply(raw_files, readxl::read_excel)

# Combine all into one dataframe
data_field <- bind_rows(df_list)

message("Combined dataset preview:")
print(head(data_field))


# ------------ 4) Minimal cleaning --------------------------

clean_df <- data_field %>%
 mutate(
  ForkLength_mm = suppressWarnings(as.numeric(ForkLength_mm)),
  Width_mm      = suppressWarnings(as.numeric(Width_mm)),
  Mass_g        = suppressWarnings(as.numeric(Mass_g))
 )

message("Preview of cleaned data:")
print(head(clean_df))

saveRDS(clean_df,
        file.path(processed_dir, paste0(species, "_clean.rds")))

# ------------ 5) Summary table -----------------------------

summary_tbl <- clean_df %>%
 summarise(
  n_rows        = n(),
  n_FL          = sum(!is.na(ForkLength_mm)),
  n_width       = sum(!is.na(Width_mm)),
  n_weight      = sum(!is.na(Mass_g)),
  FL_mean_mm    = mean(ForkLength_mm, na.rm = TRUE),
  width_mean_mm = mean(Width_mm,      na.rm = TRUE),
  weight_mean_g = mean(Mass_g,        na.rm = TRUE)
 )

write_csv(summary_tbl,
          file.path(tables_dir, paste0(species, "_summary.csv")))

message("Summary table created.")

# ------------ 6) Caption helper ----------------------------

caption_n <- function(df, text) {
 paste0(species, " (n = ", nrow(df), "): ", text)
}

# ------------ 7) Scatterplot: Width vs Fork Length ---------

df_scatter_fl <- clean_df %>%
 filter(!is.na(Width_mm), !is.na(ForkLength_mm))

p_scatter_fl <- ggplot(df_scatter_fl, aes(x = ForkLength_mm, y = Width_mm)) +
 geom_point(color = "#2c7fb8", alpha = 0.6, size = 2) +
 labs(
  title   = paste0(species, " - Width by Fork Length"),
  x       = "Fork Length (mm)",
  y       = "Width (mm)",
  caption = caption_n(df_scatter_fl, "Width plotted against fork length.")
 ) +
 theme_minimal()

ggsave(
 filename = file.path(figs_dir, paste0(species, "_scatter_width_by_forklength.png")),
 plot = p_scatter_fl, width = 7, height = 5, dpi = 300
)

# ------------ 8) Scatterplot: Width vs Mass ----------------

df_scatter_mass <- clean_df %>%
 filter(!is.na(Width_mm), !is.na(Mass_g))

p_scatter_mass <- ggplot(df_scatter_mass, aes(x = Mass_g, y = Width_mm)) +
 geom_point(color = "#f16913", alpha = 0.6, size = 2) +
 labs(
  title   = paste0(species, " - Width by Mass"),
  x       = "Mass (g)",
  y       = "Width (mm)",
  caption = caption_n(df_scatter_mass, "Width plotted against mass.")
 ) +
 theme_minimal()

ggsave(
 filename = file.path(figs_dir, paste0(species, "_scatter_width_by_mass.png")),
 plot = p_scatter_mass, width = 7, height = 5, dpi = 300
)

message("Scatterplots generated for: ", species)

