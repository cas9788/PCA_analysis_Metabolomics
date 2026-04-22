
long_raw_ms <- read.csv('C:/Users/clementina.strozek/Documents/test/05032024_gelfer_exp02.csv', stringsAsFactors = FALSE)
head(long_raw_ms)
dim(long_raw_ms)
library(dplyr)
library(tidyr)

col_mapping <- c(
  "Replicate" = "sample",
  "File.Name" = "Filename", 
  "Molecule" = "Compound",
  "Best.Retention.Time" = "RT",
  "Molecule.Formula" = "Formula"
)

# Only rename columns that exist
existing_cols <- intersect(names(col_mapping), names(long_raw_ms))
if (length(existing_cols) > 0) {
  long_raw_ms <- plyr::rename(long_raw_ms, col_mapping[existing_cols])
}

# Handle intensity column
if ("Total.Area" %in% names(long_raw_ms)) {
  long_raw_ms <- long_raw_ms %>%
    mutate(intensity = as.numeric(Total.Area)) %>%
    select(-Total.Area)
}

# Clean dataframe
cols_to_remove <- intersect(c("Max.Height", "Isotope.Label.Type"), names(long_raw_ms))
if (length(cols_to_remove) > 0) {
  long_MS_Int <- long_raw_ms %>% select(-all_of(cols_to_remove))
} else {
  long_MS_Int <- long_raw_ms
}

long_MS_Int$intensity <- as.numeric(long_MS_Int$intensity)

head(long_MS_Int)
library(tidyr)
# Samples
sample_pattern <- "WT|IN|INR"
long_MS_Int_samples <- long_MS_Int %>% 
  filter(grepl(sample_pattern, sample, ignore.case = TRUE)) %>%
  mutate(Type = "sample") %>%
  separate(sample, c("Condition", "replicate"), "_", extra = "merge", fill = "right")

# Blanks
blank_pattern <- "blank|up|sb|mb"
long_MS_Int_blanks <- long_MS_Int %>% 
  filter(grepl(blank_pattern, sample, ignore.case = TRUE)) %>%
  mutate(Type = "blank") %>%
  separate(sample, c("Condition", "replicate"), "_", extra = "merge", fill = "right") %>%
  mutate(Type = case_when(
    grepl("cleanup", Condition, ignore.case = TRUE) ~ "CU",
    grepl("wakeup", Condition, ignore.case = TRUE) ~ "WU",
    grepl("sb", Condition, ignore.case = TRUE) ~ "S",
    grepl("mb", Condition, ignore.case = TRUE) ~ "M",
    TRUE ~ "blank"
  ))

# QCs
qc_pattern <- 'QC'
long_MS_Int_QCs <- long_MS_Int %>% 
  filter(grepl(qc_pattern, sample, ignore.case = TRUE)) %>%
  mutate(Type = "QC") %>%
  separate(sample, c("Condition", "replicate"), "_", extra = "merge", fill = "right")

# Combine
long_MS_Int <- bind_rows(long_MS_Int_samples, long_MS_Int_blanks, long_MS_Int_QCs)

tail(long_MS_Int)
cols_to_keep <- intersect(c("Molecule.List", "Compound", "Formula", "Precursor.Mz", 
                            "Precursor.Adduct", "Molecule.Note", "Filename", "intensity"),
                          names(long_MS_Int))

MS_Int <- long_MS_Int %>%
  select(all_of(cols_to_keep)) %>%
  spread(Filename, intensity)

head(MS_Int)
tail(MS_Int)

cols_to_keep <- intersect(c("Molecule.List", "Compound", "Formula", "Precursor.Mz",
                            "Precursor.Adduct", "Molecule.Note", "Filename", "intensity"),
                          names(long_MS_Int))

MS_Int <- long_MS_Int %>%
  select(all_of(cols_to_keep)) %>%
  spread(Filename, intensity)

# Calculate CV for QC samples
qc_cols <- names(MS_Int)[grepl("QC", names(MS_Int), ignore.case = TRUE)]

# Keep only real columns
qc_cols <- intersect(qc_cols, names(MS_Int))

print("QC columns detected:")
print(qc_cols)
if (length(qc_cols) > 0) {
  
  qc_data <- MS_Int[, qc_cols, drop = FALSE]
  Pooled_QC_CV <- MS_Int %>%
    mutate(
      CV = apply(qc_data, 1, function(x) {
        if (all(is.na(x))) return(NA_real_)
        m <- mean(x, na.rm = TRUE)
        if (m == 0) return(NA_real_)
        sd(x, na.rm = TRUE) / m * 100
      })
    ) %>%
    select(Compound, CV) %>%
    distinct() %>%
    arrange(desc(CV))
  
  #rv$cv_data <- Pooled_QC_CV
  
  # High CV features
  high_cv <- Pooled_QC_CV %>% filter(CV > 30)
  #rv$high_cv <- high_cv
  
  # Merge CV back to long data
  long_MS_Int <- long_MS_Int %>%
    left_join(Pooled_QC_CV, by = "Compound")
  # Remove high CV features
  long_MS_Int <- long_MS_Int %>%
    filter(is.na(CV) | CV <= 30)
}
  print(high_cv)


  if ("Condition" %in% names(long_MS_Int)) {
    
    # Compute mean QC and blank intensities per compound
    blank_check <- long_MS_Int %>%
      group_by(Compound) %>%
      summarise(
        mean_QC = mean(intensity[Type == "QC"], na.rm = TRUE),
        mean_MethodBlank = mean(intensity[Type == "M"], na.rm = TRUE),
        mean_SolventBlank = mean(intensity[Type == "S"], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Percent_MethodBlank = ifelse(mean_QC > 0,
                                     mean_MethodBlank / mean_QC * 100, NA_real_),
        Percent_SolventBlank = ifelse(mean_QC > 0,
                                      mean_SolventBlank / mean_QC * 100, NA_real_)
      ) %>%
      mutate(across(starts_with("Percent"), ~replace_na(., 0))) %>%
      mutate(sum_blank = Percent_MethodBlank + Percent_SolventBlank) %>%
      arrange(desc(sum_blank))
    
    #rv$blank_check <- blank_check
    
    # Filter based on user threshold
    remove_compounds <- blank_check %>%
      filter(sum_blank >= 30) %>%
      pull(Compound)
    print(remove_compounds)
    # Remove contaminated features
    long_MS_Int <- long_MS_Int %>%
      filter(!Compound %in% remove_compounds)
  }


print(remove_compounds)































if ("Condition" %in% names(long_MS_Int)) {
  
  blank_summary <- long_MS_Int %>%
    group_by(Compound, Condition) %>%
    summarise(meanIntensity = mean(intensity, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Condition, values_from = meanIntensity) %>%
    mutate(across(where(is.numeric), ~replace_na(., 0)))
  
  # Calculate percentages vs QC
  blank_check <- blank_summary %>%
    mutate(
      Percent_MethodBlank = if ("Method" %in% names(.))
        Method / PooledQC * 100 else NA_real_,
      Percent_SolventBlank = if ("Solvent" %in% names(.))
        Solvent / PooledQC * 100 else NA_real_
    ) %>%
    mutate(across(starts_with("Percent"), ~replace_na(., 0))) %>%
    mutate(sum_blank = Percent_MethodBlank + Percent_SolventBlank) %>%
    arrange(desc(sum_blank))
  
  #rv$blank_check <- blank_check
  
  # Filter based on user threshold
  remove_compounds <- blank_check %>%
    filter(sum_blank >= 20) %>%
    pull(Compound)
  
  # Remove contaminated features
  long_MS_Int <- long_MS_Int %>%
    filter(!Compound %in% remove_compounds)
}


















# Calculate CV for QC samples
qc_cols <- names(MS_Int)[grepl("QC", names(MS_Int), ignore.case = TRUE)]

# Keep only real columns
qc_cols <- intersect(qc_cols, names(MS_Int))

print("QC columns detected:")
print(qc_cols)
if (length(qc_cols) > 0) {
  
  qc_data <- MS_Int[, qc_cols, drop = FALSE]
  Pooled_QC_CV <- MS_Int %>%
    mutate(
      CV = apply(qc_data, 1, function(x) {
        if (all(is.na(x))) return(NA_real_)
        m <- mean(x, na.rm = TRUE)
        if (m == 0) return(NA_real_)
        sd(x, na.rm = TRUE) / m * 100
      })
    ) %>%
    select(Compound, CV) %>%
    distinct() %>%
    arrange(desc(CV))
  
  rv$cv_data <- Pooled_QC_CV
  
  # High CV features
  high_cv <- Pooled_QC_CV %>% filter(CV > 30)
  rv$high_cv <- high_cv
  
  # Merge CV back to long data
  long_MS_Int <- long_MS_Int %>%
    left_join(Pooled_QC_CV, by = "Compound")
  
  # Remove high CV features
  long_MS_Int <- long_MS_Int %>%
    filter(is.na(CV) | CV <= 30)
}
rv<- ""
if ("Condition" %in% names(long_MS_Int)) {
  blank_check <- tryCatch({
    long_MS_Int %>%
      group_by(Compound, Condition) %>%
      summarise(meanIntensity = mean(intensity, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = Condition, values_from = meanIntensity) %>%
      mutate(across(where(is.numeric), ~replace_na(., 0)))
  }, error = function(e) NULL) }
long_MS_Int <- long_MS_Int %>% 
  mutate (intensity_zeros = intensity)%>% 
  mutate_at(vars(contains("zeros")), ~replace(., is.na(.), 0))
wide_overview <- dcast(long_MS_Int, Condition + replicate ~  Compound,  value.var="intensity_zeros")
print(wide_overview)

long_MS_Int <- long_MS_Int %>%
  mutate(intensity_zeros = replace_na(intensity, 0))

wide_overview <- dcast(long_MS_Int, Condition + replicate ~ Compound,
                       value.var = "intensity_zeros") %>%
  unite("PCA_ID", Condition, replicate, sep = "-", remove = FALSE) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

Data_X_overview <- wide_overview
rownames(Data_X_overview) <- Data_X_overview$PCA_ID
Data_X_overview <- Data_X_overview %>%
  select(-Condition, -replicate, -PCA_ID)

# Remove zero-variance columns
vars <- apply(Data_X_overview, 2, function(x) {
  if (is.numeric(x)) var(x, na.rm = TRUE) else NA
})

Data_X_overview <- Data_X_overview[, !is.na(vars) & vars > 0]
res.pca <- prcomp(Data_X_overview, scale = as.logical(TRUE))
print(res.pca)