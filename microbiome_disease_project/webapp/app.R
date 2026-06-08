######################################
## Microbiome Disease Prediction   ##
## Implemented by Jewoo Yoo         ##
## 2026-06-08                       ##
######################################

library(shiny)
library(data.table)
library(class)

app_dir <- normalizePath(getwd())
model_path <- file.path(app_dir, "microbiome_disease_model_R.rds")
if (!file.exists(model_path)) {
  model_path <- file.path(dirname(app_dir), "model", "microbiome_disease_model_R.rds")
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
  if (!file.exists(model_path)) {
    stop("Model file not found: microbiome_disease_model_R.rds")
  }
  readRDS(model_path)
}

ui <- fluidPage(
  titlePanel("Microbiome Disease Predictor"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload TSV file", accept = c(".tsv", ".txt")),
      helpText("The file must contain ID and microbiome_1 through microbiome_1953 columns."),
      downloadButton("download", "Download predictions")
    ),
    mainPanel(
      h4("Prediction Preview"),
      tableOutput("preview"),
      verbatimTextOutput("status")
    )
  )
)

server <- function(input, output, session) {
  bundle <- reactiveVal(NULL)
  predictions <- reactiveVal(NULL)

  observe({
    bundle(load_bundle())
  })

  observeEvent(input$file, {
    req(input$file)
    model_bundle <- bundle()
    df <- fread(input$file$datapath, data.table = FALSE)

    if (!("ID" %in% names(df))) {
      stop("Uploaded file must include an ID column.")
    }

    missing_cols <- setdiff(model_bundle$feature_cols, names(df))
    if (length(missing_cols) > 0) {
      stop(paste("Missing feature columns:", paste(head(missing_cols, 5), collapse = ", ")))
    }

    x <- as.matrix(df[, model_bundle$feature_cols])
    x_scaled <- transform_features(x, model_bundle$preprocessor)
    pred <- predict_models(model_bundle$models, x_scaled)

    predictions(data.frame(
      ID = df$ID,
      Prediction = pred,
      stringsAsFactors = FALSE
    ))
  })

  output$preview <- renderTable({
    req(predictions())
    head(predictions(), 20)
  })

  output$status <- renderText({
    if (is.null(predictions())) {
      "Upload a TSV file to run prediction."
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
