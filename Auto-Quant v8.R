packages <- c("readxl", "openxlsx", "writexl", "dplyr", "ggplot2")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# --- Global Paths & Sheets ---
in_path_src  <- "Y:/Agilent QQQ-4 6495C/Serge/Data/20260204_VLTR559_Full PK Study/YDAVSLEGR/YDAVSLEGR.xlsx"  # source file
in_sheet_src <- "Sheet1"
in_path      <- "Y:/Agilent QQQ-4 6495C/Serge/Data/20260204_VLTR559_Full PK Study/YDAVSLEGR/MHdata.xlsx"      # input workbook path
in_sheet     <- "Formatted"                     # sheet to read
out_path     <- "Y:/Agilent QQQ-4 6495C/Serge/Data/20260204_VLTR559_Full PK Study/YDAVSLEGR/MHdata.xlsx"      # output workbook path
sheet_form   <- "Formatted"
sheet_high   <- "Highlighted"      # sheet to read

# Ensure destination directory exists
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

# Target names for the first 12 columns ONLY
new_names_20 <- c(
  "Name", "DataFile", "Type", "Level", "AcqDateTime", "ExpConc", "RTa", "Resp", "MI",
  "CalcConc", "Accuracy", "SN", "Ratio", "MI", "Ratio", "MI", "RT", "Resp.", "Ratio", "MI"
)

# --- Read input without interpreting headers (original header becomes first data row) ---
df <- readxl::read_excel(in_path_src, sheet = in_sheet_src, col_names = FALSE)

# --- Remove the original header row (row 1) and the first data row (row 2) ---
if (nrow(df) < 2) stop("The sheet must have at least 2 rows to remove header and first data row.")
df <- df[-c(1, 2), , drop = FALSE]

# --- Remove the first two columns ---
if (ncol(df) < 2) stop("The sheet must have at least 2 columns to remove the first two columns.")
df <- df[, -(1:2), drop = FALSE]

# --- Rename only the first up-to-12 columns; leave others unchanged ---
n_to_rename <- min(ncol(df), length(new_names_20))
colnames(df)[seq_len(n_to_rename)] <- new_names_20[seq_len(n_to_rename)]

# --- Helper to coerce selected columns to numeric (strip non-numeric characters) ---
coerce_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(x))))
}

# Choose indices to coerce (bounded by existing columns)
# Adjust these indices as needed based on your actual data layout
numeric_idx <- c(5, 6, 7, 8, 10, 11, 12, 13, 17, 18)  # examples: AcqDateTime, ExpConc, RTa, Resp, CalcConc, Accuracy, SN, etc.
numeric_idx <- numeric_idx[numeric_idx <= ncol(df)]
df[numeric_idx] <- lapply(df[numeric_idx], coerce_numeric)

# --- Write "Formatted" to MHdata.xlsx ---
wb <- if (file.exists(out_path)) openxlsx::loadWorkbook(out_path) else openxlsx::createWorkbook()
if (sheet_form %in% openxlsx::sheets(wb)) {
  openxlsx::removeWorksheet(wb, sheet_form)
}
openxlsx::addWorksheet(wb, sheet_form)
openxlsx::writeData(wb, sheet = sheet_form, x = df, startRow = 1, startCol = 1, colNames = TRUE)
openxlsx::saveWorkbook(wb, file = out_path, overwrite = TRUE)
cat("Saved 'Formatted' sheet to:", out_path, "\n")

# ---------------------------------------
# Create "Highlighted" in the same workbook and apply conditional formatting
# ---------------------------------------
# Read the formatted data back (ensures we format exactly what's written)
df_fmt <- readxl::read_excel(out_path, sheet = sheet_form)

# Resolve column indices safely
resolve_col_idx <- function(col, names_vec) {
  if (is.numeric(col)) {
    idx <- as.integer(col)
    if (idx < 1 || idx > length(names_vec)) stop("Column index is out of range.")
    idx
  } else {
    idx <- match(col, names_vec)
    if (is.na(idx)) stop(paste0("Column '", col, "' not found in sheet."))
    idx
  }
}

target_rt_col  <- "RTa"
target_acc_col <- "Accuracy"
target_sn_col  <- "SN"

rt_idx  <- resolve_col_idx(target_rt_col,  names(df_fmt))
acc_idx <- resolve_col_idx(target_acc_col, names(df_fmt))
sn_idx  <- resolve_col_idx(target_sn_col,  names(df_fmt))

# Coerce target columns to numeric (again, to be safe)
df_fmt[[rt_idx]]  <- coerce_numeric(df_fmt[[rt_idx]])
df_fmt[[acc_idx]] <- coerce_numeric(df_fmt[[acc_idx]])
df_fmt[[sn_idx]]  <- coerce_numeric(df_fmt[[sn_idx]])

# Compute RT mean/sd excluding NA
rt_vals <- df_fmt[[rt_idx]]
rt_mean <- mean(rt_vals, na.rm = TRUE)
rt_sd   <- stats::sd(rt_vals, na.rm = TRUE)
rt_lower <- rt_mean - 2 * rt_sd
rt_upper <- rt_mean + 2 * rt_sd

# Load existing workbook and add "Highlighted"
wb2 <- openxlsx::loadWorkbook(out_path)
if (sheet_high %in% openxlsx::sheets(wb2)) {
  openxlsx::removeWorksheet(wb2, sheet_high)
}
openxlsx::addWorksheet(wb2, sheet_high)
openxlsx::writeData(wb2, sheet = sheet_high, x = df_fmt, startRow = 1, startCol = 1, colNames = TRUE)

# Style for highlighting (red fill, dark red text)
highlight_style <- openxlsx::createStyle(bgFill = "#FFC7CE", fontColour = "#9C0006")

# Rows to format (skip header row)
rows_to_format <- 2:(nrow(df_fmt) + 1)

# Column letters for Excel formulas
rt_letter  <- openxlsx::int2col(rt_idx)
acc_letter <- openxlsx::int2col(acc_idx)
sn_letter  <- openxlsx::int2col(sn_idx)

# --- Conditional formatting rules WITHOUT '\$' characters ---

# RTa: highlight outside mean ± 2*sd
rule_rt_low  <- sprintf("=AND(ISNUMBER(%s2), %s2<%s)", rt_letter,  rt_letter,  format(rt_lower, scientific = FALSE, trim = TRUE))
rule_rt_high <- sprintf("=AND(ISNUMBER(%s2), %s2>%s)", rt_letter,  rt_letter,  format(rt_upper, scientific = FALSE, trim = TRUE))

openxlsx::conditionalFormatting(
  wb = wb2, sheet = sheet_high,
  rows = rows_to_format, cols = rt_idx,
  type = "expression", rule = rule_rt_low, style = highlight_style
)
openxlsx::conditionalFormatting(
  wb = wb2, sheet = sheet_high,
  rows = rows_to_format, cols = rt_idx,
  type = "expression", rule = rule_rt_high, style = highlight_style
)

# Accuracy: highlight <= 85 or >= 115
acc_lower <- 85
acc_upper <- 115
rule_acc_low  <- sprintf("=AND(ISNUMBER(%s2), %s2<%s)", acc_letter, acc_letter, format(acc_lower, scientific = FALSE, trim = TRUE))
rule_acc_high <- sprintf("=AND(ISNUMBER(%s2), %s2>%s)", acc_letter, acc_letter, format(acc_upper, scientific = FALSE, trim = TRUE))

openxlsx::conditionalFormatting(
  wb = wb2, sheet = sheet_high,
  rows = rows_to_format, cols = acc_idx,
  type = "expression", rule = rule_acc_low, style = highlight_style
)
openxlsx::conditionalFormatting(
  wb = wb2, sheet = sheet_high,
  rows = rows_to_format, cols = acc_idx,
  type = "expression", rule = rule_acc_high, style = highlight_style
)

# SN: highlight values < 5 (numeric cells only)
rule_sn_low <- sprintf("=AND(ISNUMBER(%s2), %s2<5)", sn_letter, sn_letter)

openxlsx::conditionalFormatting(
  wb = wb2, sheet = sheet_high,
  rows = rows_to_format, cols = sn_idx,
  type = "expression", rule = rule_sn_low, style = highlight_style
)


# Resolve indices for Resp (handle both "Resp" and "Resp.") and Level (and optionally Type)
resp_name <- if ("Resp" %in% names(df_fmt)) "Resp" else if ("Resp." %in% names(df_fmt)) "Resp." else stop("Neither 'Resp' nor 'Resp.' column found.")
resp_idx   <- resolve_col_idx(resp_name, names(df_fmt))
level_idx  <- resolve_col_idx("Level",    names(df_fmt))
type_idx   <- if ("Type" %in% names(df_fmt)) resolve_col_idx("Type", names(df_fmt)) else NA_integer_

# Coerce Resp to numeric (safe guard)
df_fmt[[resp_idx]] <- coerce_numeric(df_fmt[[resp_idx]])

# Build mask for L1 rows (robust to case and spaces)
level_vec <- tolower(gsub("\\s+", "", trimws(as.character(df_fmt[[level_idx]]))))
mask_l1   <- level_vec == "l1"

# Optionally restrict L1 rows to calibrators if 'Type' looks like calibrator ('Cal')
if (!is.na(type_idx)) {
  type_vec <- tolower(trimws(as.character(df_fmt[[type_idx]])))
  if (any(grepl("cal", type_vec, ignore.case = TRUE))) {
    mask_l1 <- mask_l1 & grepl("cal", type_vec, ignore.case = TRUE)
  }
}

# Extract numeric Resp values for L1 rows
l1_resp_vals <- coerce_numeric(df_fmt[[resp_idx]][mask_l1])
l1_resp_vals <- l1_resp_vals[is.finite(l1_resp_vals)]

if (length(l1_resp_vals) > 0) {
  # Choose how to summarize multiple L1 responses: 'max' is conservative (flags any value below the highest L1 response)
  l1_resp_threshold_func <- max  # alternatives: mean, median, min
  l1_resp_threshold <- l1_resp_threshold_func(l1_resp_vals, na.rm = TRUE)
  
  # Build Excel conditional formula for Resp column
  resp_letter     <- openxlsx::int2col(resp_idx)
  rule_below_l1_r <- sprintf("=AND(ISNUMBER(%s2), %s2<%s)",
                             resp_letter, resp_letter,
                             format(l1_resp_threshold, scientific = FALSE, trim = TRUE))
  
  openxlsx::conditionalFormatting(
    wb = wb2, sheet = sheet_high,
    rows = rows_to_format, cols = resp_idx,
    type = "expression", rule = rule_below_l1_r, style = highlight_style
  )
} else {
  message("No L1 rows with numeric Resp found; skipping Resp < L1 highlighting.")
}



# Save workbook with both sheets
openxlsx::saveWorkbook(wb2, file = out_path, overwrite = TRUE)

# Log summary
cat("Saved 'Highlighted' sheet to:", out_path, "\n")
cat(sprintf("RT mean: %s, RT sd: %s -> bounds [%.6f, %.6f]\n",
            format(rt_mean, scientific = FALSE),
            format(rt_sd,   scientific = FALSE),
            rt_lower, rt_upper))


# Load data and workbook
df <- readxl::read_excel(out_path, sheet = sheet_high)
wb <- openxlsx::loadWorkbook(out_path)

# Locate the Type column (case-insensitive, trims header whitespace)
type_idx <- which(tolower(trimws(names(df))) == "type")
if (length(type_idx) == 0) stop("Column 'Type' not found.")
type_idx    <- type_idx[1]
type_letter <- openxlsx::int2col(type_idx)

# Data rows to format (assumes headers in row 1, data start at row 2)
if (nrow(df) == 0) stop("No data rows to format.")
rows_to_format <- 2:(nrow(df) + 1)

# Styles
style_qc     <- openxlsx::createStyle(bgFill = "#C6EFCE", fontColour = "#006100") # green
style_cal    <- openxlsx::createStyle(bgFill = "#D9E1F2", fontColour = "#1F4E79") # blue
style_sample <- openxlsx::createStyle(bgFill = "#FCE4D6", fontColour = "#9C6500") # orange

# Conditional formatting formulas (no dollar signs; robust to case/whitespace)
rule_qc     <- sprintf("=UPPER(TRIM(%s2))=\"QC\"",     type_letter)
rule_cal    <- sprintf("=UPPER(TRIM(%s2))=\"CAL\"",    type_letter)
rule_sample <- sprintf("=UPPER(TRIM(%s2))=\"SAMPLE\"", type_letter)

# Apply rules to the Type column only
openxlsx::conditionalFormatting(
  wb = wb, sheet = sheet_high,
  rows = rows_to_format, cols = type_idx,
  type = "expression", rule = rule_qc, style = style_qc
)
openxlsx::conditionalFormatting(
  wb = wb, sheet = sheet_high,
  rows = rows_to_format, cols = type_idx,
  type = "expression", rule = rule_cal, style = style_cal
)
openxlsx::conditionalFormatting(
  wb = wb, sheet = sheet_high,
  rows = rows_to_format, cols = type_idx,
  type = "expression", rule = rule_sample, style = style_sample
)

# Save the workbook
openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)


# ------------------------ Write the sample names and their associated values to a new sheet ---------------------
# ---- Parameters to set ----
out_sheet_name <- "Sample Values"      # name of the new sheet to create

# ---- Packages ----
# Requires: readxl, openxlsx
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
library(readxl)
library(openxlsx)

# ---- Read data ----
# Assumes 'in_path' (file path) and 'in_sheet' (e.g., "Formatted") are defined earlier
df <- readxl::read_excel(in_path, sheet = in_sheet)

# ---- Validate required columns ----
required_cols <- c("Name", "Type", "Level", "Resp", "CalcConc")
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

# ---- Helpers ----
norm_str <- function(x) tolower(gsub("\\s+", "", trimws(as.character(x))))  # trim, collapse spaces, lowercase
coerce_num <- function(x) suppressWarnings(as.numeric(x))

# ---- Build L1 response threshold ----
level_norm <- norm_str(df[["Level"]])
mask_l1 <- !is.na(level_norm) & level_norm == "l1"

type_norm <- norm_str(df[["Type"]])
has_cal <- any(!is.na(type_norm) & type_norm == "cal")
if (has_cal) {
  mask_l1 <- mask_l1 & (type_norm == "cal")
}

resp_vec <- coerce_num(df[["Resp"]])
l1_resp_vals <- resp_vec[mask_l1]
l1_resp_vals <- l1_resp_vals[is.finite(l1_resp_vals)]

if (length(l1_resp_vals) == 0) {
  warning("No L1 response values found; 'Sample Values' will be written without red highlighting.")
}

# Conservative choice: use the maximum L1 response as the threshold
l1_threshold <- if (length(l1_resp_vals) > 0) max(l1_resp_vals, na.rm = TRUE) else NA_real_
message(sprintf("L1 threshold used (Resp): %s",
                if (is.finite(l1_threshold)) format(l1_threshold) else "NA"))

# ---- Filter rows where Type contains 'Sample' (case-insensitive) ----
sel <- !is.na(df[["Type"]]) & grepl("Sample", as.character(df[["Type"]]), ignore.case = TRUE)

# ---- Build output data frame with Name and CalcConc ----
out_df <- data.frame(
  Name     = df[["Name"]][sel],
  CalcConc = df[["CalcConc"]][sel],
  stringsAsFactors = FALSE
)

# If no matches, create an empty table with headers
if (nrow(out_df) == 0) {
  out_df <- data.frame(Name = character(0), CalcConc = character(0), stringsAsFactors = FALSE)
  message("No rows found where Type contains 'Sample'. Writing empty table with headers.")
}

# ---- Determine which selected rows should be highlighted (Resp < L1 threshold) ----
flag_rel <- integer(0)
if (is.finite(l1_threshold) && nrow(out_df) > 0) {
  # Indices in original df of rows selected as samples
  idx_sel <- which(sel)
  # Per-sample flags based on Resp
  resp_sel <- resp_vec[idx_sel]
  flag_mask <- is.finite(resp_sel) & (resp_sel < l1_threshold)
  flag_rel <- which(flag_mask)  # positions within out_df to highlight
}

# ---- Write to a new sheet inside the same workbook ----
wb <- openxlsx::loadWorkbook(in_path)
existing_sheets <- openxlsx::sheets(wb)
if (out_sheet_name %in% existing_sheets) {
  openxlsx::removeWorksheet(wb, out_sheet_name)  # replace if exists
}
openxlsx::addWorksheet(wb, out_sheet_name)
openxlsx::writeData(wb, out_sheet_name, out_df)

# ---- Highlight flagged samples in red on 'Sample Values' ----
if (length(flag_rel) > 0) {
  # Excel row numbers for data rows (header is row 1)
  rows_excel <- flag_rel + 1L
  # Highlight both Name and CalcConc columns (1 and 2 in the output)
  cols_to_highlight <- 1:2  # set to 2 if you only want to color CalcConc
  
  # Style for highlighting (soft yellow fill with dark gold text)
  highlight_style <- openxlsx::createStyle(
    fgFill     = "#FFEB9C",
    fontColour = "#9C6500"
  )  # Excel yellow scheme

  openxlsx::addStyle(
    wb = wb, sheet = out_sheet_name, style = highlight_style,
    rows = rows_excel, cols = cols_to_highlight, gridExpand = TRUE, stack = TRUE
  )
} else if (nrow(out_df) > 0 && is.finite(l1_threshold)) {
  message("No 'Sample' rows had Resp below the L1 threshold; no red highlights applied.")
}

openxlsx::saveWorkbook(wb, in_path, overwrite = TRUE)

# ---- Print a quick summary ----
message(sprintf("Wrote %d rows to sheet '%s' in workbook: %s", nrow(out_df), out_sheet_name, in_path))

# Required packages
# install.packages(c("readxl", "ggplot2", "openxlsx"))
library(readxl)
library(ggplot2)
library(openxlsx)

# ------------------------ Make a plot of the Accuracies ---------------------
tiff_file <- "accuracy_plot.tiff"
png_file  <- "accuracy_plot.png"
sheet_name <- "Accuracy Plot"

# Read and prepare data
df <- read_excel(in_path, sheet = in_sheet)

df_plot <- data.frame(
  Level    = df[["Level"]],
  Accuracy = to_num(df[["Accuracy"]])
)

# Keep only complete rows for Level and Accuracy
df_plot <- df_plot[complete.cases(df_plot[["Level"]], df_plot[["Accuracy"]]), ]

# Flag out-of-range (Accuracy <= 85 or >= 115)
df_plot[["col"]] <- ifelse(df_plot[["Accuracy"]] <= 85 | df_plot[["Accuracy"]] >= 115, "red", "steelblue")

# Build the ggplot (element_text belongs in theme(), not ggplot())
p <- ggplot(df_plot, aes(x = Level, y = Accuracy, color = col)) +
  geom_point(size = 6) +
  scale_color_identity(guide = "none") +
  geom_hline(yintercept = c(85, 115), linetype = "dashed", color = "red") +
  labs(title = "Accuracies of the Levels", x = "Level", y = "Bias") +
  theme_minimal(base_size = 20) +
  theme(
    plot.title  = element_text(family = "Arial", face = "bold"),
    axis.title  = element_text(family = "Arial", face = "bold"),
    axis.text   = element_text(family = "Arial")
  )

# Save TIFF (archival-quality)
tiff(filename = tiff_file, width = 2000, height = 1400, res = 300, compression = "lzw")
print(p)
dev.off()

# Save PNG (embed-friendly for Excel)
png(filename = png_file, width = 2000, height = 1400, res = 300)
print(p)
dev.off()

# Create or open Excel workbook and add the plot on the "Accuracy Plot" sheet
wb <- if (file.exists(out_path)) loadWorkbook(out_path) else createWorkbook()
if (sheet_name %in% names(wb)) removeWorksheet(wb, sheet_name)
addWorksheet(wb, sheet_name)

insertImage(
  wb, sheet = sheet_name, file = png_file,
  startRow = 1, startCol = 1, width = 12, height = 8, units = "in"
)

saveWorkbook(wb, out_path, overwrite = TRUE)

message(sprintf("Saved TIFF to '%s' and embedded PNG into sheet '%s' in '%s'",
                tiff_file, sheet_name, out_path))


# ------------------------ Combined: CV Summary + %CV Plot ---------------------
library(readxl)
library(ggplot2)
library(openxlsx)

# ---- Parameters to set ----
level_column   <- "Level"        # column containing levels
accuracy_col   <- "Accuracy"     # column containing accuracies
calcconc_col   <- "CalcConc"     # column containing calculated concentrations
expconc_col    <- "ExpConc"      # column containing experimental concentrations

out_sheet_name <- "CV_Summary"
plot_sheet     <- " CV Plot"     # leading space as requested
cv_threshold   <- 15

tiff_file      <- "cv_plot.tiff"
png_file       <- "cv_plot.png"

# ---- Helper to safely coerce to numeric ----
to_num <- function(x) {
  if (is.numeric(x)) return(x)
  as.numeric(gsub("[^0-9.-]", "", as.character(x)))
}

# ---- Read data ----
df <- read_excel(in_path, sheet = in_sheet)

# ---- Validate required columns ----
required_cols <- c(level_column, accuracy_col, calcconc_col, expconc_col)
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

# ---- Extract vectors without using \$ ----
lev_vec   <- df[[level_column]]
acc_vec   <- to_num(df[[accuracy_col]])
calc_vec  <- to_num(df[[calcconc_col]])
exp_vec   <- to_num(df[[expconc_col]])

# ---- Identify levels with multiple rows (>= 2), preserve first appearance order ----
lev_chr <- as.character(lev_vec)
lev_chr_non_na <- lev_chr[!is.na(lev_chr)]
tab <- table(lev_chr_non_na)
dup_levels <- names(tab)[tab >= 2]
target_levels <- unique(lev_chr_non_na[lev_chr_non_na %in% dup_levels])

# ---- Function to compute stats for one level, including ExpConc ----
summarise_level <- function(level_value, lev_vec, acc_vec, calc_vec, exp_vec) {
  idx <- !is.na(lev_vec) & (as.character(lev_vec) == as.character(level_value))
  
  # Accuracy values for CV calculation
  acc_vals <- acc_vec[idx]
  acc_vals <- acc_vals[is.finite(acc_vals)]
  
  # Calculated concentrations for mean calculation
  calc_vals <- calc_vec[idx]
  calc_vals <- calc_vals[is.finite(calc_vals)]
  
  # Experimental concentrations (expect usually one unique value per level)
  exp_vals <- exp_vec[idx]
  exp_vals <- exp_vals[is.finite(exp_vals)]
  exp_unique <- unique(exp_vals)
  exp_conc <- if (length(exp_unique) == 0) {
    NA_real_
  } else if (length(exp_unique) == 1) {
    exp_unique
  } else {
    warning(sprintf("Multiple ExpConc values found for Level '%s'; using the first: %s",
                    as.character(level_value),
                    paste(exp_unique, collapse = ", ")))
    exp_unique[1]
  }
  
  n_vals   <- length(acc_vals)
  mean_val <- if (n_vals > 0) mean(acc_vals) else NA_real_
  sd_val   <- if (n_vals > 1) stats::sd(acc_vals) else NA_real_
  cv_pct   <- if (!is.na(mean_val) && mean_val != 0 && !is.na(sd_val)) 100 * sd_val / mean_val else NA_real_
  
  mean_calcconc <- if (length(calc_vals) > 0) mean(calc_vals) else NA_real_
  
  data.frame(
    Level        = level_value,
    N            = n_vals,
    ExpConc      = exp_conc,        # Experimental concentration
    Avg_CalcConc = mean_calcconc,   # Mean of CalcConc
    Avg_Bias     = mean_val,        # Mean of Accuracy
    SD           = sd_val,          # SD of Accuracy
    CV_percent   = cv_pct,          # CV% of Accuracy
    stringsAsFactors = FALSE
  )
}

# ---- Build the summary table ----
if (length(target_levels) == 0) {
  message("No levels with multiple rows found.")
  summary_df <- data.frame(
    Level = character(0),
    N = integer(0),
    ExpConc = numeric(0),
    Avg_CalcConc = numeric(0),
    Avg_Bias = numeric(0),
    SD = numeric(0),
    CV_percent = numeric(0),
    stringsAsFactors = FALSE
  )
} else {
  summary_list <- lapply(
    target_levels,
    summarise_level,
    lev_vec = lev_vec,
    acc_vec = acc_vec,
    calc_vec = calc_vec,
    exp_vec = exp_vec
  )
  summary_df <- do.call(rbind, summary_list)
}

# ---- Print to console for quick inspection ----
print(summary_df)

# ---- Create plot data from summary_df ----
df_plot <- data.frame(
  Level = summary_df[["Level"]],
  CV    = to_num(summary_df[["CV_percent"]])
)

# Flag out-of-range (%CV >= threshold)
df_plot[["col"]] <- ifelse(df_plot[["CV"]] >= cv_threshold, "red", "steelblue")

# ---- Plot and save images if there's data ----
if (nrow(df_plot) > 0) {
  p_cv <- ggplot(df_plot, aes(x = Level, y = CV, color = col)) +
    geom_point(size = 10) +
    scale_color_identity(guide = "none") +
    geom_hline(yintercept = c(0, cv_threshold), linetype = "dashed", color = "red") +
    labs(title = "%CV of Replicates", x = "Level", y = "%CV") +
    theme_minimal(base_size = 30)
  
  # Save high-quality TIFF (archival)
  tiff(filename = tiff_file, width = 2000, height = 1400, res = 300, compression = "lzw")
  print(p_cv)
  dev.off()
  
  # Save a PNG for embedding into Excel
  png(filename = png_file, width = 2000, height = 1400, res = 300)
  print(p_cv)
  dev.off()
} else {
  warning("No data available to generate the %CV plot; skipping image save.")
}

# ---- Create or open the Excel workbook and write outputs ----
wb <- if (file.exists(out_path)) {
  loadWorkbook(out_path)
} else {
  createWorkbook()
}

# Replace the summary sheet if it exists
if (out_sheet_name %in% names(wb)) removeWorksheet(wb, out_sheet_name)
addWorksheet(wb, out_sheet_name)
writeData(wb, out_sheet_name, summary_df)

# Replace the plot sheet if it exists
if (plot_sheet %in% names(wb)) removeWorksheet(wb, plot_sheet)
addWorksheet(wb, plot_sheet)

# Embed the PNG into the plot sheet (if the file exists)
if (file.exists(png_file)) {
  insertImage(
    wb, sheet = plot_sheet, file = png_file,
    startRow = 1, startCol = 1, width = 12, height = 8, units = "in"
  )
} else {
  writeData(wb, plot_sheet, "Plot image not found; no image embedded.")
}

# Save the workbook once with both outputs
saveWorkbook(wb, out_path, overwrite = TRUE)

message(sprintf(
  "Wrote summary to '%s' and embedded plot on sheet '%s' in workbook: %s",
  out_sheet_name, plot_sheet, normalizePath(out_path, mustWork = FALSE)
))


# ---- Calculate the Level Accuracies as Type Cal, QC, Sample, etc. ----
# ---- Parameters to set ----
type_col      <- "Type"                    # column containing levels
accuracy_col  <- "Accuracy"                 # column containing accuracies
lower_bound   <- 85                         # inclusive lower bound
upper_bound   <- 115                        # inclusive upper bound
out_sheet_name <- "%Passing Overall"

# ---- Helper to safely coerce to numeric ----
to_num <- function(x) {
  if (is.numeric(x)) return(x)
  as.numeric(gsub("[^0-9.-]", "", as.character(x)))
}

# ---- Read data ----
df_overall <- read_excel(in_path, sheet = in_sheet)

# ---- Validate required columns ----
required_cols <- c(type_col, accuracy_col)
missing_cols <- setdiff(required_cols, names(df_overall))
if (length(missing_cols) > 0) {
  stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

# ---- Extract vectors without using \$ ----
type_vec <- df_overall[[type_col]]
acc_vec_all <- to_num(df_overall[[accuracy_col]])

# ---- Identify levels with duplicates (appear more than once) ----
type_chr <- as.character(type_vec)
type_chr_non_na <- type_chr[!is.na(type_chr)]
tab <- table(type_chr_non_na)
dup_levels <- names(tab)[tab >= 2]

# Preserve order of first appearance for duplicate levels
target_types <- unique(type_chr_non_na[type_chr_non_na %in% dup_levels])

# ---- Function to compute percentage-in-range for one level ----
summarise_level_overall <- function(type_value, type_vec, acc_vec_all, lo, hi) {
  idx  <- !is.na(type_vec) & (as.character(type_vec) == as.character(type_value))
  vals <- acc_vec_all[idx]
  # Use only finite numeric values for the denominator
  valid <- vals[is.finite(vals)]
  
  n_total_all     <- length(valid)
  n_in_range_all  <- sum(valid >= lo & valid <= hi)
  pct_in_range_all <- if (n_total_all > 0) 100 * n_in_range_all / n_total_all else NA_real_
  
  data.frame(
    Type_all               = type_value,
    N_valid_all            = n_total_all,
    N_in_range_85_115_all  = n_in_range_all,
    Percent_in_range_all   = pct_in_range_all,
    stringsAsFactors_all   = FALSE
  )
}

# ---- Build and print the summary table ----
if (length(target_types) == 0) {
  message("No levels with multiple rows found.")
  summary_list_overall <- data.frame(
    Type_all = character(0),
    N_valid_all = integer(0),
    N_in_range_85_115_all = integer(0),
    Percent_in_range_all = numeric(0),
    stringsAsFactors_all = FALSE
  )
} else {
  summary_list_overall <- lapply(
    target_types,
    summarise_level_overall,
    type_vec = type_vec,
    acc_vec_all = acc_vec_all,
    lo = lower_bound,
    hi = upper_bound
  )
  summary_df_overall <- do.call(rbind, summary_list_overall)
}
print(summary_df_overall)

# ---- Write to a new sheet inside the same workbook ----
wb <- loadWorkbook(in_path)
existing_sheets <- sheets(wb)
if (out_sheet_name %in% existing_sheets) {
  removeWorksheet(wb, out_sheet_name)  # replace if exists
}
addWorksheet(wb, out_sheet_name)
writeData(wb, out_sheet_name, summary_df_overall)
saveWorkbook(wb, in_path, overwrite = TRUE)



# ---- Calculate the Level Accuracys by Unique Entities ----
# ---- Parameters to set ----
level_column  <- "Level"                    # column containing levels
accuracy_col  <- "Accuracy"                 # column containing accuracys
lower_bound   <- 85                         # inclusive lower bound
upper_bound   <- 115                        # inclusive upper bound
out_sheet_name <- "%Passing Unique"

# ---- Helper to safely coerce to numeric ----
to_num <- function(x) {
  if (is.numeric(x)) return(x)
  as.numeric(gsub("[^0-9.-]", "", as.character(x)))
}

# ---- Read data ----
df <- read_excel(in_path, sheet = in_sheet)

# ---- Validate required columns ----
required_cols <- c(level_column, accuracy_col)
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

# ---- Extract vectors without using \$ ----
lev_vec <- df[[level_column]]
acc_vec <- to_num(df[[accuracy_col]])

# ---- Identify levels with duplicates (appear more than once) ----
lev_chr <- as.character(lev_vec)
lev_chr_non_na <- lev_chr[!is.na(lev_chr)]
tab <- table(lev_chr_non_na)
dup_levels <- names(tab)[tab >= 2]

# Preserve order of first appearance for duplicate levels
target_levels <- unique(lev_chr_non_na[lev_chr_non_na %in% dup_levels])

# ---- Function to compute percentage-in-range for one level ----
summarise_level <- function(level_value, lev_vec, acc_vec, lo, hi) {
  idx  <- !is.na(lev_vec) & (as.character(lev_vec) == as.character(level_value))
  vals <- acc_vec[idx]
  # Use only finite numeric values for the denominator
  valid <- vals[is.finite(vals)]
  
  n_total     <- length(valid)
  n_in_range  <- sum(valid >= lo & valid <= hi)
  pct_in_range <- if (n_total > 0) 100 * n_in_range / n_total else NA_real_
  
  data.frame(
    Level              = level_value,
    N_valid            = n_total,
    N_in_range_85_115  = n_in_range,
    Percent_in_range   = pct_in_range,
    stringsAsFactors   = FALSE
  )
}

# ---- Build and print the summary table ----
if (length(target_levels) == 0) {
  message("No levels with multiple rows found.")
  summary_list <- data.frame(
    Level = character(0),
    N_valid = integer(0),
    N_in_range_85_115 = integer(0),
    Percent_in_range = numeric(0),
    stringsAsFactors = FALSE
  )
} else {
  summary_list <- lapply(
    target_levels,
    summarise_level,
    lev_vec = lev_vec,
    acc_vec = acc_vec,
    lo = lower_bound,
    hi = upper_bound
  )
  summary_df_unique <- do.call(rbind, summary_list)
}


# ---- Write to a new sheet inside the same workbook ----
wb <- loadWorkbook(in_path)
existing_sheets <- sheets(wb)
if (out_sheet_name %in% existing_sheets) {
  removeWorksheet(wb, out_sheet_name)  # replace if exists
}
addWorksheet(wb, out_sheet_name)
writeData(wb, out_sheet_name, summary_df_unique)
saveWorkbook(wb, in_path, overwrite = TRUE)


# Print to console
print(summary_df_unique)
print(summary_df_overall)