

library(shiny)
library(tidyverse)
library(reshape2)

perf_by_nestimators <- read.csv('perf_by_nestimators.csv')
perf_by_lr <- read.csv('perf_by_lr.csv')
tuning_methods <- read.csv('tuning_methods.csv')
conf_matrices <- read.csv('conf_matrices.csv')
metrics <- read.csv('metrics(1).csv')

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #0f1419;
        color: #e6edf3;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      .container-fluid {
        background-color: #0f1419;
      }
      h2 {
        color: #58a6ff;
        font-weight: 600;
        text-shadow: 0 0 10px rgba(88, 166, 255, 0.3);
      }
      h4 {
        color: #79c0ff;
        font-weight: 500;
      }
      .well {
        background-color: #161b22;
        border: 1px solid #30363d;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.3);
      }
      .form-control {
        background-color: #0d1117;
        border: 1px solid #30363d;
        color: #e6edf3;
      }
      .form-control:focus {
        background-color: #161b22;
        border-color: #58a6ff;
        color: #e6edf3;
        box-shadow: 0 0 0 0.2rem rgba(88, 166, 255, 0.25);
      }
      .selectize-input {
        background-color: #0d1117 !important;
        border: 1px solid #30363d !important;
        color: #e6edf3 !important;
      }
      .selectize-dropdown {
        background-color: #161b22 !important;
        border: 1px solid #30363d !important;
        color: #e6edf3 !important;
      }
      .selectize-dropdown-content .option {
        color: #e6edf3 !important;
      }
      .selectize-dropdown-content .option:hover {
        background-color: #21262d !important;
        color: #58a6ff !important;
      }
      .checkbox label {
        color: #e6edf3;
      }
      .checkbox input[type='checkbox']:checked + span {
        color: #58a6ff;
      }
      .irs--shiny .irs-bar {
        background: linear-gradient(to bottom, #58a6ff 0%, #1f6feb 100%);
        border-top: 1px solid #58a6ff;
        border-bottom: 1px solid #1f6feb;
      }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
        background: #58a6ff;
        color: #0d1117;
        font-weight: 600;
      }
      .irs--shiny .irs-handle {
        background: #58a6ff;
        border: 2px solid #0d1117;
      }
      .irs--shiny .irs-line {
        background: #30363d;
        border: 1px solid #21262d;
      }
      .irs--shiny .irs-grid-text {
        color: #8b949e;
      }
      #metrics_summary {
        background-color: #0d1117;
        border: 1px solid #30363d;
        color: #7ee787;
        padding: 15px;
        border-radius: 6px;
        font-family: 'Courier New', monospace;
        font-size: 13px;
        line-height: 1.8;
        box-shadow: inset 0 2px 4px rgba(0, 0, 0, 0.3);
      }
      .btn-danger {
        background: linear-gradient(135deg, #da3633 0%, #f85149 100%);
        border: none;
        font-weight: bold;
        transition: all 0.3s ease;
        box-shadow: 0 2px 8px rgba(248, 81, 73, 0.4);
      }
      .btn-danger:hover {
        background: linear-gradient(135deg, #f85149 0%, #ff7b72 100%);
        box-shadow: 0 4px 12px rgba(248, 81, 73, 0.6);
        transform: translateY(-2px);
      }
      .plot-container {
        background-color: #161b22;
        border: 2px solid #30363d;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 20px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.4);
        transition: all 0.3s ease;
      }
      .plot-container:hover {
        border-color: #58a6ff;
        box-shadow: 0 6px 16px rgba(88, 166, 255, 0.2);
      }
      label {
        color: #c9d1d9;
        font-weight: 500;
      }
    "))
  ),
  
  titlePanel("ML Models Metrics Dashboard"),
  
  sidebarPanel(
    width = 3,
    selectInput("selected_models",
                "Select Models:",
                choices = metrics$Model,
                selected = metrics$Model[1:2],
                multiple = TRUE),
    
    selectInput("selected_tuning",
                "Select Tuning Methods:",
                choices = NULL,
                multiple = TRUE),
    
    checkboxGroupInput("selected_metrics",
                       "Select Metrics:",
                       choices = c("Accuracy", "Precision", "Recall", "F1_Score", "ROC_AUC", "Gini", "Time_sec"),
                       selected = c("Accuracy", "F1_Score")),
    
    sliderInput("lr_range",
                "Select Learning Rate Range:",
                min = 0,
                max = 1,
                value = c(0, 1),
                step = 0.001),
    
    sliderInput("n_estimators_range",
                "Select Number of Estimators:",
                min = 50,
                max = 1000,
                value = c(50, 1000),
                step = 1), 
    
    selectInput("selected_metric_lr",
                "Select Metric to Plot:",
                choices = colnames(perf_by_lr)[!colnames(perf_by_lr) %in% c("X", "LearningRate")],
                selected = "Accuracy"), 
    
    h4("Best Model per Metric"),
    div(verbatimTextOutput("metrics_summary")),
    
    actionButton("reset_all", "RESET",
                 style = "background-color: #da3633; color: white; font-weight: bold; border: none; width: 100%; margin-top: 15px;")
  ),
  
  mainPanel(
    div(
      style = "padding-bottom: 30px;",
      
      div(class = "plot-container",
          plotOutput("metrics_bar_chart", height = '400px')
      ),
      
      div(class = "plot-container",
          uiOutput("confusion_matrices")
      ),
      
      div(class = "plot-container",
          plotOutput("lollipop_chart", height = '300px')
      ),
      
      div(class = "plot-container",
          plotOutput("metric_area_chart", height = "200px")
      ),
      
      div(class = "plot-container",
          plotOutput("metric_estimators_area_chart", height = "200px")
      )
    )
  )
)

server <- function(input, output, session) {
  
  updateSelectInput(session, "selected_tuning", choices = tuning_methods$X, selected = tuning_methods$X[1:2])
  
  filtered_data <- reactive({
    req(input$selected_models)
    metrics[metrics$Model %in% input$selected_models, c("Model", input$selected_metrics), drop = FALSE]
  })
  
  output$metrics_bar_chart <- renderPlot({
    data <- filtered_data()
    req(nrow(data) > 0, length(input$selected_metrics) > 0)
    
    data_melt <- melt(data, id.vars = "Model")
    
    ggplot(data_melt, aes(x = Model, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      geom_text(aes(label = round(value, 2)), 
                position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5, color = "#e6edf3") +
      labs(x = "Model", y = "Metric Value", fill = "Metric") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#161b22", color = NA),
        panel.background = element_rect(fill = "#0d1117", color = NA),
        panel.grid.major = element_line(color = "#30363d", size = 0.3),
        panel.grid.minor = element_line(color = "#21262d", size = 0.2),
        axis.text.x = element_text(angle = 0, hjust = 1, size = 12, color = "#e6edf3"),
        axis.text.y = element_text(size = 12, color = "#e6edf3"),
        axis.title = element_text(color = "#c9d1d9", size = 13),
        legend.position = "top",
        legend.background = element_rect(fill = "#161b22", color = NA),
        legend.title = element_text(size = 12, color = "#c9d1d9"),
        legend.text = element_text(size = 10, color = "#e6edf3")
      ) +
      scale_fill_manual(values = c("#58a6ff", "#56d364", "#f778ba", "#ffa657", "#8957e5", "#d29922", "#ff7b72")) + 
      scale_x_discrete(labels = c(
        "LogisticRegression" = "LogReg", "XGBoost" = "XGB", "RandomForest" = "RandFor",
        "CatBoost" = "CatBoost", "NaiveBayes" = "Naive", "KNN" = "KNN",
        "DecisionTree" = "DecTree", "SVM_Linear" = "SVM(ln)", "SVM_RBF" = "SVM(rbf)",
        "Bagging_SVM" = "Bagging(SVM)", "EXtraTree" = "ExtraTree", "Adaboost" = "Adaboost",
        "LightGBM" = "LightGBM"
      )) + 
      scale_y_continuous(limits = c(0,1))
  }, bg = "#161b22")
  
  output$confusion_matrices <- renderUI({
    req(input$selected_models)
    
    table_list <- lapply(input$selected_models, function(model_name) {
      cm <- conf_matrices[conf_matrices$Model == model_name, ]
      
      tags$div(
        style = "display:inline-block; margin-right:20px; text-align:center; margin-bottom: 15px;",
        tags$strong(model_name, style = "color: #58a6ff; font-size: 14px;"),
        tags$table(
          style = "border-collapse: collapse; margin-top:8px; font-size:12px; background-color: #0d1117; border: 1px solid #30363d;",
          tags$tr(
            tags$td(" ", style="width:50px; color: #8b949e;"),
            tags$td("1", style="padding:5px; font-weight:bold; color: #79c0ff;"),
            tags$td("0", style="padding:5px; font-weight:bold; color: #79c0ff;")
          ),
          tags$tr(
            tags$td("1", style="padding:5px; font-weight:bold; color: #79c0ff;"),
            tags$td(cm$True_Positive, style="padding:12px; background-color:#238636; color:white; font-weight: 600;"),
            tags$td(cm$False_Negative, style="padding:12px; background-color:#da3633; color:white; font-weight: 600;")
          ),
          tags$tr(
            tags$td("0", style="padding:5px; font-weight:bold; color: #79c0ff;"),
            tags$td(cm$False_Positive, style="padding:12px; background-color:#da3633; color:white; font-weight: 600;"),
            tags$td(cm$True_Negative, style="padding:12px; background-color:#238636; color:white; font-weight: 600;")
          )
        )
      )
    })
    
    do.call(tags$div, table_list)
  })
  
  output$lollipop_chart <- renderPlot({
    req(input$selected_tuning)
    req(length(input$selected_metrics) > 0)
    
    data <- tuning_methods[tuning_methods$X %in% input$selected_tuning, ]
    selected_cols <- tolower(input$selected_metrics)
    
    data_melt <- melt(data, id.vars = c("X", "time_sec"), measure.vars = selected_cols)
    
    all_methods <- c("bagging_svm", "grid_search_tuned", "random_search_tuned",
                     "bayes_opt_tuned", "hypergrad_tuned")
    
    data_melt$X_pos <- match(data_melt$X, all_methods)
    
    ggplot(data_melt, aes(x = X_pos, y = value, color = variable)) +
      geom_segment(aes(x = X_pos + as.numeric(factor(variable))*0.15 - 0.3,
                       xend = X_pos + as.numeric(factor(variable))*0.15 - 0.3,
                       y = 0, yend = value),
                   linewidth = 1.2) +
      geom_point(aes(x = X_pos + as.numeric(factor(variable))*0.15 - 0.3,
                     size = time_sec),
                 show.legend = TRUE) +
      scale_x_continuous(
        breaks = 1:length(all_methods),
        labels = c(
          "bagging_svm" = "Bagging(SVM)", "grid_search_tuned" = "GridSearch",
          "random_search_tuned" = "RandomSearch", "bayes_opt_tuned" = "BayesOptim",
          "hypergrad_tuned" = "HyperGradient"
        )[all_methods]
      ) +
      scale_y_continuous(limits = c(0,1)) +
      labs(x = "Tuning Method", y = "Metric Value", color = "Metric", size = "Time (sec)") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#161b22", color = NA),
        panel.background = element_rect(fill = "#0d1117", color = NA),
        panel.grid.major = element_line(color = "#30363d", size = 0.3),
        panel.grid.minor = element_line(color = "#21262d", size = 0.2),
        axis.text.x = element_text(angle = 0, hjust = 0.7, size = 14, color = "#e6edf3"),
        axis.text.y = element_text(color = "#e6edf3"),
        axis.title = element_text(color = "#c9d1d9"),
        legend.position = "top",
        legend.background = element_rect(fill = "#161b22", color = NA),
        legend.text = element_text(size = 12, color = "#e6edf3"),
        legend.title = element_text(color = "#c9d1d9")
      ) +
      scale_color_manual(values = c("#58a6ff", "#56d364", "#f778ba", "#ffa657", "#8957e5", "#d29922", "#ff7b72"))
  }, bg = "#161b22")
  
  output$metric_area_chart <- renderPlot({
    req(input$selected_metric_lr)
    data <- perf_by_lr %>%
      filter(LearningRate >= input$lr_range[1], LearningRate <= input$lr_range[2])
    
    ymin <- min(data[[input$selected_metric_lr]], na.rm = TRUE) - 0.01
    ymax <- max(data[[input$selected_metric_lr]], na.rm = TRUE) + 0.01
    
    ggplot(data, aes(x = LearningRate, y = .data[[input$selected_metric_lr]])) +
      geom_area(fill = "#58a6ff", alpha = 0.3) +
      geom_line(color = "#58a6ff", size = 1.2) +
      geom_point(color = "#79c0ff", size = 2.5) +
      scale_x_continuous(breaks = c(0.001, 0.010, 0.030, 0.050, 0.100, 0.200, 0.300, 0.500, 0.700, 1.000)) +
      scale_y_continuous(limits = c(ymin, ymax)) +
      labs(x = "Learning Rate", y = input$selected_metric_lr,
           title = paste(input$selected_metric_lr, "vs Learning Rate")) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#161b22", color = NA),
        panel.background = element_rect(fill = "#0d1117", color = NA),
        panel.grid.major = element_line(color = "#30363d", size = 0.3),
        panel.grid.minor = element_line(color = "#21262d", size = 0.2),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 10, color = "#e6edf3"),
        axis.text.y = element_text(angle = 0, hjust = 1, size = 12, color = "#e6edf3"),
        axis.title = element_text(size = 14, color = "#c9d1d9"),
        plot.title = element_text(size = 16, face = "bold", color = "#58a6ff")
      )
  }, bg = "#161b22")
  
  output$metric_estimators_area_chart <- renderPlot({
    req(input$selected_metric_lr)
    
    data <- perf_by_nestimators %>%
      filter(n_estimators >= input$n_estimators_range[1],
             n_estimators <= input$n_estimators_range[2])
    
    ymin <- min(data[[input$selected_metric_lr]], na.rm = TRUE) - 0.01
    ymax <- max(data[[input$selected_metric_lr]], na.rm = TRUE) + 0.01
    
    ggplot(data, aes(x = n_estimators, y = .data[[input$selected_metric_lr]])) +
      geom_area(fill = "#56d364", alpha = 0.3) +
      geom_line(color = "#56d364", size = 1.2) +
      geom_point(color = "#7ee787", size = 2.5) +
      scale_y_continuous(limits = c(ymin, ymax)) +
      labs(x = "Number of Estimators", y = input$selected_metric_lr,
           title = paste(input$selected_metric_lr, "vs Number of Estimators")) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#161b22", color = NA),
        panel.background = element_rect(fill = "#0d1117", color = NA),
        panel.grid.major = element_line(color = "#30363d", size = 0.3),
        panel.grid.minor = element_line(color = "#21262d", size = 0.2),
        axis.text.x = element_text(angle = 0, hjust = 1, size = 12, color = "#e6edf3"),
        axis.text.y = element_text(angle = 0, hjust = 1, size = 12, color = "#e6edf3"),
        axis.title = element_text(size = 14, color = "#c9d1d9"),
        plot.title = element_text(size = 16, face = "bold", color = "#56d364")
      )
  }, bg = "#161b22")
  
  observeEvent(input$reset_all, {
    updateSelectInput(session, "selected_models", selected = metrics$Model[1:2])
    updateSelectInput(session, "selected_tuning", selected = tuning_methods$X[1:2])
    updateCheckboxGroupInput(session, "selected_metrics", selected = c("Accuracy", "F1_Score"))
    updateSliderInput(session, "lr_range", value = c(0, 1))
    updateSliderInput(session, "n_estimators_range", value = c(50, 1000))
    updateSelectInput(session, "selected_metric_lr", selected = "Accuracy")
  })
  
  output$metrics_summary <- renderPrint({
    req(input$selected_metrics)
    data <- metrics[, c("Model", input$selected_metrics), drop = FALSE]
    
    summary_text <- sapply(input$selected_metrics, function(metric) {
      best_model <- data$Model[which.max(data[[metric]])]
      paste0(metric, ": ", round(max(data[[metric]]), 3), 
             " → Best Model: ", best_model)
    })
    
    cat(paste(summary_text, collapse = "\n"))
  })
}

shinyApp(ui, server)
















