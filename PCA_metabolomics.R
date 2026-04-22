# Install libraries
library(shiny)
library(bslib)

install.packages(c("shiny", "tidyverse", "plyr", "reshape2",
                   "factoextra", "patchwork", "ggrepel", "DT"))
# app.R - Shiny Application for MS Data PCA Analysis
library(dplyr)
library(shiny)
library(tidyverse)
library(plyr)
library(reshape2)
library(factoextra)
library(patchwork)
library(ggrepel)
library(DT)
library(ggplot2)
library(tidyr)

# Define color palette
MediaPalette <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
                  "#FFFF33", "#A65628", "#F781BF", "#999999")

# UI
ui <- fluidPage(
  titlePanel("MS Data QC and PCA Analysis"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput("file", "Upload CSV File", accept = ".csv"),
      hr(),
      h4("Sample Pattern Matching"),
      textInput("sample_pattern", "Sample patterns (regex)", "WT|IN|INR"),
      textInput("blank_pattern", "Blank patterns (regex)", "blank|up|sb|mb"),
      textInput("qc_pattern", "QC patterns (regex)", "QC"),
      hr(),
      h4("QC Filtering"),
      numericInput("cv_threshold", "CV threshold (%)", value = 30, min = 0, max = 100),
      numericInput("blank_threshold", "Blank threshold (%)", value = 20, min = 0, max = 100),
      hr(),
      h4("PCA Settings"),
      selectInput("scaling", "Scaling method",
                  choices = c("UV (Unit Variance)" = "TRUE", "Mean Centered" = "FALSE")),
      numericInput("top_loadings", "Top loadings to display", value = 30, min = 5, max = 100),
      hr(),
      actionButton("run_analysis", "Run Analysis", class = "btn-primary btn-lg")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Data Preview",
                 h4("Raw Data Preview"),
                 DTOutput("raw_data_preview"),
                 hr(),
                 h4("Processed Data Summary"),
                 verbatimTextOutput("data_summary")),

        tabPanel("CV Analysis",
                 h4("Pooled QC Coefficient of Variation"),
                 DTOutput("cv_table"),
                 hr(),
                 h4("Features Flagged (CV > threshold)"),
                 DTOutput("high_cv_table"),
                 hr(),
                 downloadButton("download_high_cv", "Download High CV Features")),

        tabPanel("Blank Check",
                 h4("Blank Background Check"),
                 DTOutput("blank_check_table"),
                 hr(),
                 verbatimTextOutput("blank_summary")),

        tabPanel("PCA Plots",
                 plotOutput("pca_combined", height = "800px"),
                 hr(),
                 fluidRow(
                   column(6, downloadButton("download_pca", "Download PCA Plot (PNG)")),
                   column(6, downloadButton("download_pca_pdf", "Download PCA Plot (PDF)"))
                 )),

        tabPanel("Individual Plots",
                 h4("Scree Plot"),
                 plotOutput("scree_plot", height = "300px"),
                 hr(),
                 h4("Scores Plot"),
                 plotOutput("scores_plot", height = "500px"),
                 hr(),
                 h4("Loadings Plot"),
                 plotOutput("loadings_plot", height = "500px"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive values to store processed data
  rv <- reactiveValues(
    long_MS_Int = NULL,
    cv_data = NULL,
    high_cv = NULL,
    blank_check = NULL,
    pca_result = NULL,
    wide_overview = NULL
  )

  # Read and process data
  processed_data <- eventReactive(input$run_analysis, {
    req(input$file)

    withProgress(message = 'Processing data...', value = 0, {

      # Read raw data
      incProgress(0.1, detail = "Reading CSV")
      long_raw_ms <- read.csv(input$file$datapath, stringsAsFactors = FALSE)

      # Rename columns if they exist
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
      incProgress(0.2, detail = "Cleaning data")
      cols_to_remove <- intersect(c("Max.Height", "Isotope.Label.Type"), names(long_raw_ms))
      if (length(cols_to_remove) > 0) {
        long_MS_Int <- long_raw_ms %>% select(-all_of(cols_to_remove))
      } else {
        long_MS_Int <- long_raw_ms
      }

      long_MS_Int$intensity <- as.numeric(long_MS_Int$intensity)

      # Split by sample type
      incProgress(0.3, detail = "Categorizing samples")

      # Samples
      long_MS_Int_samples <- long_MS_Int %>%
        filter(grepl(input$sample_pattern, sample, ignore.case = TRUE)) %>%
        mutate(Type = "sample") %>%
        separate(sample, c("Condition", "replicate"), "_", extra = "merge", fill = "right")

      # Blanks
      long_MS_Int_blanks <- long_MS_Int %>%
        filter(grepl(input$blank_pattern, sample, ignore.case = TRUE)) %>%
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
      long_MS_Int_QCs <- long_MS_Int %>%
        filter(grepl(input$qc_pattern, sample, ignore.case = TRUE)) %>%
        mutate(Type = "QC") %>%
        separate(sample, c("Condition", "replicate"), "_", extra = "merge", fill = "right")

      # Combine
      long_MS_Int <- bind_rows(long_MS_Int_samples, long_MS_Int_blanks, long_MS_Int_QCs)

      # Convert to wide format for CV calculation
      incProgress(0.4, detail = "Calculating CVs")

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

         rv$cv_data <- Pooled_QC_CV

         # High CV features
         high_cv <- Pooled_QC_CV %>% filter(CV > input$cv_threshold)
         rv$high_cv <- high_cv

         # Merge CV back to long data
         long_MS_Int <- long_MS_Int %>%
           left_join(Pooled_QC_CV, by = "Compound")

     # Remove high CV features
      long_MS_Int <- long_MS_Int %>%
          filter(is.na(CV) | CV <= input$cv_threshold)
       }

      # Blank check
      incProgress(0.5, detail = "Checking blanks")
      
      if ("Condition" %in% names(long_MS_Int)) {
        
        # Compute mean QC and blank intensities per compound
        blank_check <- long_MS_Int %>%
          group_by(Compound) %>%
          dplyr::summarise(
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
        
        rv$blank_check <- blank_check
        
        # Filter based on user threshold
        remove_compounds <- blank_check %>%
          filter(sum_blank >= input$blank_threshold) %>%
          pull(Compound)
        
        # Remove contaminated features
        #long_MS_Int <- long_MS_Int %>%
         # filter(!Compound %in% remove_compounds)
      }
      
      
      # Prepare for PCA
      incProgress(0.6, detail = "Preparing PCA data")

      long_MS_Int <- long_MS_Int %>%
        mutate(intensity_zeros = replace_na(intensity, 0))

      # Create wide format for PCA
      wide_overview <- dcast(long_MS_Int, Condition + replicate ~ Compound,
                             value.var = "intensity_zeros") %>%
        unite("PCA_ID", Condition, replicate, sep = "-", remove = FALSE) %>%
        mutate(across(where(is.numeric), ~replace_na(., 0)))

      rv$wide_overview <- wide_overview
      rv$long_MS_Int <- long_MS_Int

      # Run PCA
      incProgress(0.8, detail = "Computing PCA")

      Data_X_overview <- wide_overview
      rownames(Data_X_overview) <- Data_X_overview$PCA_ID
      Data_X_overview <- Data_X_overview %>%
        select(-Condition, -replicate, -PCA_ID)

      # Remove zero-variance columns
      vars <- apply(Data_X_overview, 2, function(x) {
        if (is.numeric(x)) var(x, na.rm = TRUE) else NA
      })
      
      Data_X_overview <- Data_X_overview[, !is.na(vars) & vars > 0]
      
      res.pca <- prcomp(Data_X_overview, scale = as.logical(input$scaling))
      rv$pca_result <- res.pca

      incProgress(1, detail = "Complete!")

      return(list(
        long_MS_Int = long_MS_Int,
        wide_overview = wide_overview,
        pca_result = res.pca,
        n_features = ncol(Data_X_overview)
      ))
    })
  })

  # Data preview
  output$raw_data_preview <- renderDT({
    req(input$file)
    data <- read.csv(input$file$datapath, stringsAsFactors = FALSE, nrows = 100)
    datatable(data, options = list(scrollX = TRUE, pageLength = 10))
  })

  # Data summary
  output$data_summary <- renderPrint({
    req(processed_data())
    data <- processed_data()
    cat("Processed Data Summary\n")
    cat("======================\n")
    cat("Total features after filtering:", data$n_features, "\n")
    cat("Number of samples:", nrow(data$wide_overview), "\n")
    cat("Conditions:", paste(unique(data$wide_overview$Condition), collapse = ", "), "\n")
  })

  # CV table
  output$cv_table <- renderDT({
    req(rv$cv_data)
    datatable(rv$cv_data, options = list(pageLength = 15, scrollX = TRUE)) %>%
      formatRound("CV", 2)
  })

  # High CV table
  output$high_cv_table <- renderDT({
    req(rv$high_cv)
    datatable(rv$high_cv, options = list(pageLength = 15, scrollX = TRUE)) %>%
      formatRound("CV", 2)
  })

  # Download high CV
  output$download_high_cv <- downloadHandler(
    filename = function() {
      paste0("Features_flagged_high_CV_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(rv$high_cv, file, row.names = FALSE)
    }
  )

  # Blank check table
  output$blank_check_table <- renderDT({
    req(rv$blank_check)
    datatable(rv$blank_check, options = list(pageLength = 15, scrollX = TRUE))
  })

  output$blank_summary <- renderPrint({
    req(rv$blank_check)
    cat("Blank check completed.\n")
    cat("Features checked:", nrow(rv$blank_check), "\n")
  })

  # Generate PCA plots
  pca_plots <- reactive({
    req(processed_data())

    data <- processed_data()
    res.pca <- data$pca_result
    wide_overview <- data$wide_overview
    n_features <- data$n_features

    scaling_label <- if (as.logical(input$scaling)) "UV scaled" else "Mean centered"

    # Scree plot
    scree <- fviz_eig(res.pca, geom = "bar", width = 0.8, addlabels = FALSE) +
      labs(title = expression(paste("R"^2, ": variance explained by each component"))) +
      theme(text = element_text(size = 8))

    # Scores plot
    scores <- fviz_pca_ind(res.pca,
                           fill.ind = wide_overview$Condition,
                           col.ind = "black",
                           pointshape = 21,
                           pointsize = 5,
                           addEllipses = TRUE,
                           ellipse.type = "confidence",
                           ellipse.level = 0.95,
                           legend.title = "Groups",
                           mean.point = FALSE,
                           ellipse.border.remove = TRUE,
                           repel = TRUE,
                           labelsize = 2) +
      labs(title = paste("PCA scores -", scaling_label),
           subtitle = paste("Total features:", n_features),
           caption = "Missing values replaced with zero") +
      scale_fill_manual(values = MediaPalette) +
      theme(legend.position = "bottom", text = element_text(size = 8))

    # Loadings plot
    loadings <- fviz_pca_var(res.pca,
                             select.var = list(contrib = input$top_loadings),
                             col.var = "contrib",
                             labelsize = 2,
                             repel = TRUE,
                             gradient.cols = c("blue", "orange", "red")) +
      labs(title = paste("PCA loadings -", scaling_label),
           subtitle = paste("Showing top", input$top_loadings, "contributing features"),
           caption = paste("Total features:", n_features)) +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 5, angle = 45),
            legend.title = element_text(size = 7),
            legend.key.size = unit(0.3, 'cm'),
            text = element_text(size = 8))

    list(scree = scree, scores = scores, loadings = loadings)
  })

  # Combined PCA plot
  output$pca_combined <- renderPlot({
    req(pca_plots())
    plots <- pca_plots()

    design <- "
      1#
      23
    "
    plots$scree + plots$scores + plots$loadings +
      plot_layout(design = design, heights = c(0.3, 1), widths = c(1, 1))
  }, res = 150)

  # Individual plots
  output$scree_plot <- renderPlot({
    req(pca_plots())
    pca_plots()$scree
  }, res = 150)

  output$scores_plot <- renderPlot({
    req(pca_plots())
    pca_plots()$scores
  }, res = 150)

  output$loadings_plot <- renderPlot({
    req(pca_plots())
    pca_plots()$loadings
  }, res = 150)

  # Download handlers
  output$download_pca <- downloadHandler(
    filename = function() {
      paste0("PCA_analysis_", Sys.Date(), ".png")
    },
    content = function(file) {
      plots <- pca_plots()
      design <- "
        1#
        23
      "
      combined <- plots$scree + plots$scores + plots$loadings +
        plot_layout(design = design, heights = c(0.3, 1), widths = c(1, 1))
      ggsave(file, combined, width = 10, height = 10, dpi = 300)
    }
  )

  output$download_pca_pdf <- downloadHandler(
    filename = function() {
      paste0("PCA_analysis_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      plots <- pca_plots()
      design <- "
        1#
        23
      "
      combined <- plots$scree + plots$scores + plots$loadings +
        plot_layout(design = design, heights = c(0.3, 1), widths = c(1, 1))
      ggsave(file, combined, width = 10, height = 10)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)

