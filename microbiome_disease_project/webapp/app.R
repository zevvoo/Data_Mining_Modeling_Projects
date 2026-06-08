######################################
## Microbiome Disease Prediction   ##
## Implemented by Jewoo Yoo         ##
## 2026-06-08                       ##
######################################

library(shiny)
library(data.table)
library(class)
library(nnet)

model_name <- "microbiome_disease_model_R.rds"

find_model_path <- function() {
  wd <- normalizePath(getwd())
  candidates <- unique(c(
    file.path(wd, model_name),
    file.path(wd, "model", model_name),
    file.path(wd, "webapp", model_name),
    file.path(dirname(wd), "model", model_name),
    file.path(dirname(wd), "webapp", model_name)
  ))

  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) {
    stop("Model file not found: microbiome_disease_model_R.rds")
  }
  existing[1]
}

transform_features <- function(x, prep) {
  x_selected <- x[, prep$features, drop = FALSE]
  sweep(sweep(x_selected, 2, prep$center, "-"), 2, prep$scale, "/")
}

predict_models <- function(models, x_scaled) {
  p_multinom <- predict(models$multinom, newdata = data.frame(x_scaled, check.names = FALSE))
  p_knn <- knn(train = models$knn_x, test = x_scaled, cl = models$knn_y, k = 5)

  votes <- data.frame(
    multinom = as.character(p_multinom),
    knn = as.character(p_knn),
    stringsAsFactors = FALSE
  )

  apply(votes, 1, function(row) {
    counts <- sort(table(row), decreasing = TRUE)
    names(counts)[1]
  })
}

load_bundle <- function() {
  readRDS(find_model_path())
}

ui <- fluidPage(
  titlePanel("Microbiome Disease Predictor"),

  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file",
        "Choose TSV File",
        multiple = FALSE,
        accept = c("text/tab-separated-values", "text/plain", ".tsv", ".txt")
      ),

      tags$hr(),

      radioButtons(
        "disp",
        "Display",
        choices = c(Head = "head", All = "all"),
        selected = "head"
      ),

      tags$hr(),

      helpText("The file must contain ID and microbiome_1 through microbiome_1953 columns."),

      tags$hr(),

      downloadButton("download", "Download predictions")
    ),

    mainPanel(
      h4("Prediction Preview"),
      tableOutput("preview"),

      tags$hr(),

      h4("Prediction Status"),
      verbatimTextOutput("status")
    )
  )
)

server <- function(input, output, session) {
  bundle <- reactiveVal(NULL)
  predictions <- reactiveVal(NULL)
  status_message <- reactiveVal("Upload a TSV file to run prediction.")

  observe({
    bundle(load_bundle())
  })

  observeEvent(input$file, {
    req(input$file)
    model_bundle <- bundle()
    df <- fread(input$file$datapath, data.table = FALSE)

    if (!("ID" %in% names(df))) {
      predictions(NULL)
      status_message("Uploaded file must include an ID column.")
      showNotification(status_message(), type = "error", duration = 6)
      return(NULL)
    }

    missing_cols <- setdiff(model_bundle$feature_cols, names(df))
    if (length(missing_cols) > 0) {
      predictions(NULL)
      status_message(paste("Missing feature columns:", paste(head(missing_cols, 5), collapse = ", ")))
      showNotification(status_message(), type = "error", duration = 6)
      return(NULL)
    }

    x <- as.matrix(df[, model_bundle$feature_cols])
    x_scaled <- transform_features(x, model_bundle$preprocessor)
    pred <- predict_models(model_bundle$models, x_scaled)

    predictions(data.frame(
      ID = df$ID,
      Prediction = pred,
      stringsAsFactors = FALSE
    ))
    status_message(paste(nrow(predictions()), "rows predicted."))
  })

  output$preview <- renderTable({
    req(predictions())
    if (input$disp == "head") {
      head(predictions(), 20)
    } else {
      predictions()
    }
  })

  output$status <- renderText({
    if (is.null(predictions())) {
      status_message()
    } else {
      paste(nrow(predictions()), "rows predicted.")
    }
  })

  output$download <- downloadHandler(
    filename = function() "microbiome_predictions.csv",
    content = function(file) {
      req(predictions())
      fwrite(predictions(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
