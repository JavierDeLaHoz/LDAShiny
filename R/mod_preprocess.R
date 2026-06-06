# ======================================================================
# MODULE: Text Preprocessing for LDA
# ======================================================================

#' Text Preprocessing Module UI
#'
#' @description
#' Provides UI controls to clean, tokenize, stem, remove stopwords, build a
#' Document-Term Matrix (DTM), and filter sparse terms.
#'
#' @param id Shiny module ID.
#' @return A Shiny UI element (\code{tagList}).
#'
#' @importFrom shiny NS tagList h2 h4 tabsetPanel tabPanel fluidRow column
#' @importFrom shiny br uiOutput verbatimTextOutput fileInput numericInput
#' @importFrom shiny checkboxInput sliderInput actionButton downloadButton tags
#' @importFrom shinybusy add_busy_spinner
#' @importFrom DT DTOutput
#' @importFrom parallel detectCores
#' @export
mod_preprocess_ui <- function(id) {
  ns <- NS(id)

  n_cores_max <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
  n_cores_def <- max(1L, n_cores_max - 1L)

  tagList(

    shinybusy::add_busy_spinner(
      spin     = "fading-circle",
      position = "top-right",
      margins  = c(15, 15)
    ),

    shiny::h2("Text Preprocessing"),

    tabsetPanel(

      # ----------------------------------------------------------------
      # TAB 1: SETTINGS
      # ----------------------------------------------------------------
      tabPanel(
        title = "Settings",
        shiny::h4("Preprocessing Options"),

        fluidRow(
          column(4, offset = 2,
                 shiny::uiOutput(ns("column_selector_ui")),
                 shiny::fileInput(
                   ns("stopword_file"),
                   "Upload Additional Stopwords (.xlsx, one word per row)",
                   accept = ".xlsx"
                 ),
                 numericInput(ns("ngram_min"), "Min n-gram:", 1, min = 1, max = 3),
                 numericInput(ns("ngram_max"), "Max n-gram:", 2, min = 1, max = 3)
          ),
          column(4,
                 tags$label(tags$strong("Cleaning and Stemming:")),
                 shiny::checkboxInput(ns("do_stem"),           "Apply stemming (Porter)",  TRUE),
                 shiny::checkboxInput(ns("remove_numbers"),    "Remove numbers",            TRUE),
                 shiny::checkboxInput(ns("remove_punctuation"),"Remove punctuation",        TRUE),
                 br(),
                 sliderInput(
                   ns("sparse_level"),
                   "Sparse filter (max allowed sparsity):",
                   min = 0.90, max = 0.9999, step = 0.0005, value = 0.995
                 ),
                 sliderInput(
                   ns("cpus"), "Number of CPUs:",
                   min = 1L, max = n_cores_max, value = n_cores_def
                 )
          )
        ),
        br(),
        shiny::actionButton(ns("run_preprocess"), "Run Preprocessing",
                            class = "btn-danger")
      ),

      # ----------------------------------------------------------------
      # TAB 2: DTM SUMMARY
      # ----------------------------------------------------------------
      tabPanel(
        title = "DTM Summary",
        shiny::h4("Dimensions of Document-Term Matrix (raw)"),
        shiny::verbatimTextOutput(ns("dtm_dims")),
        br(),
        shiny::h4("Dimensions After Sparse Filtering"),
        shiny::verbatimTextOutput(ns("dtm_slim_dims"))
      ),

      # ----------------------------------------------------------------
      # TAB 3: TERM FREQUENCY
      # ----------------------------------------------------------------
      tabPanel(
        title = "Term Frequency (tf_mat)",
        br(),
        DT::DTOutput(ns("tf_table_dt"))
      ),

      # ----------------------------------------------------------------
      # TAB 4: DOWNLOADS
      # ----------------------------------------------------------------
      tabPanel(
        title = "Downloads",
        br(),
        shiny::h4("Download Processed Objects"),
        br(),
        downloadButton(ns("download_tfmat"), "Download tf_mat (Excel)"),
        br(), br(),
        downloadButton(ns("download_dtm"),   "Download Final DTM (.rds)")
      )
    )
  )
}


# ======================================================================
# SERVER
# ======================================================================

#' Text Preprocessing Module Server
#'
#' @param id Shiny module ID.
#' @param data_reactive A reactive expression returning the input \code{data.frame}.
#' @return A list with:
#' \describe{
#'   \item{dtm}{Reactive returning the filtered sparse DTM (\code{dgCMatrix}).}
#'   \item{tf_mat}{Reactive returning the term-frequency data.frame.}
#' }
#'
#' @importFrom shiny moduleServer NS renderUI req validate need reactive
#' @importFrom shiny reactiveVal observeEvent renderPrint downloadHandler
#' @importFrom shiny selectInput showNotification
#' @importFrom shinybusy show_modal_spinner remove_modal_spinner
#' @importFrom shinyjs disable enable
#' @importFrom DT renderDT datatable
#' @importFrom openxlsx write.xlsx
#' @importFrom textmineR CreateDtm TermDocFreq
#' @importFrom stopwords stopwords
#' @importFrom SnowballC wordStem
#' @importFrom quanteda as.dfm convert
#' @importFrom tm removeSparseTerms
#' @importFrom slam as.simple_triplet_matrix
#' @importFrom Matrix sparseMatrix
#' @importFrom readxl read_excel
#' @importFrom parallel detectCores
#' @export
mod_preprocess_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {

    shinyjs::disable("download_tfmat")
    shinyjs::disable("download_dtm")

    # ------------------------------------------------------------------
    # Dynamic column selector
    # ------------------------------------------------------------------
    output$column_selector_ui <- shiny::renderUI({
      shiny::req(data_reactive())
      df   <- data_reactive()
      cols <- names(df)

      def_text <- if ("abstract" %in% cols) {
        "abstract"
      } else {
        char_cols <- cols[vapply(df, is.character, logical(1))]
        if (length(char_cols) > 0) char_cols[1] else cols[1]
      }

      def_id <- if ("title" %in% cols) "title" else if ("doi" %in% cols) "doi" else cols[1]

      tagList(
        shiny::selectInput(session$ns("text_col"), "Select text column:",
                           choices = cols, selected = def_text),
        shiny::selectInput(session$ns("id_col"),   "Select document ID column:",
                           choices = cols, selected = def_id)
      )
    })

    # ------------------------------------------------------------------
    # Reactive storage
    # ------------------------------------------------------------------
    dtm_raw       <- shiny::reactiveVal(NULL)
    dtm_slim      <- shiny::reactiveVal(NULL)
    final_dtm     <- shiny::reactiveVal(NULL)
    tf_mat_output <- shiny::reactiveVal(NULL)

    # ------------------------------------------------------------------
    # PREPROCESS
    # ------------------------------------------------------------------
    shiny::observeEvent(input$run_preprocess, {

      shinybusy::show_modal_spinner(
        spin  = "orbit",
        text  = tags$div(
          style = "font-size:20px; font-weight:bold; color:#2c3e50;",
          "Processing text data. This may take several minutes..."
        ),
        color = "#2c3e50"
      )

      shiny::req(data_reactive())
      df <- data_reactive()

      shiny::validate(
        shiny::need(input$text_col %in% names(df),
                    "Selected text column not found in data."),
        shiny::need(input$id_col   %in% names(df),
                    "Selected ID column not found in data."),
        shiny::need(input$ngram_min <= input$ngram_max,
                    "Min n-gram must be <= Max n-gram.")
      )

      text_vec <- as.character(df[[input$text_col]])
      text_vec[is.na(text_vec)] <- ""

      id_vec <- as.character(df[[input$id_col]])
      id_vec[is.na(id_vec)] <- paste0("doc_", which(is.na(id_vec)))
      id_vec <- make.unique(id_vec)

      # Stopwords
      stop_en   <- stopwords::stopwords("en")
      custom_sw <- character(0)

      if (!is.null(input$stopword_file)) {
        sw_df <- tryCatch(
          readxl::read_excel(input$stopword_file$datapath,
                             col_names = FALSE),
          error = function(e) {
            shiny::showNotification(
              paste("Could not read stopword file:", e$message),
              type = "warning", duration = 5)
            NULL
          }
        )
        if (!is.null(sw_df) && ncol(sw_df) > 0) {
          custom_sw <- trimws(tolower(as.character(sw_df[[1]])))
          custom_sw <- custom_sw[!is.na(custom_sw) & nzchar(custom_sw)]
        }
      }

      stop_all <- unique(c(stop_en, custom_sw))
      cpus     <- max(1L, as.integer(input$cpus))

      stem_fn <- if (isTRUE(input$do_stem)) {
        function(x) SnowballC::wordStem(x, language = "porter")
      } else {
        NULL
      }

      # Build raw DTM
      dtm <- tryCatch(
        textmineR::CreateDtm(
          doc_vec            = text_vec,
          doc_names          = id_vec,
          ngram_window       = c(as.integer(input$ngram_min),
                                 as.integer(input$ngram_max)),
          stopword_vec       = stop_all,
          lower              = TRUE,
          remove_punctuation = isTRUE(input$remove_punctuation),
          remove_numbers     = isTRUE(input$remove_numbers),
          stem_lemma_function = stem_fn,
          cpus               = cpus
        ),
        error = function(e) {
          shinybusy::remove_modal_spinner()
          shiny::showNotification(paste("DTM creation failed:", e$message),
                                  type = "error", duration = 8)
          NULL
        }
      )

      if (is.null(dtm)) return()
      dtm_raw(dtm)

      shiny::validate(
        shiny::need(nrow(dtm) > 0 && ncol(dtm) > 0,
                    "DTM is empty. Check options or input data.")
      )

      # Sparse filter via tm
      dfm      <- quanteda::as.dfm(dtm)
      tm_dtm   <- quanteda::convert(dfm, to = "topicmodels")
      zdtmt    <- tm::removeSparseTerms(tm_dtm, sparse = input$sparse_level)
      dtm_slim(zdtmt)

      shiny::validate(
        shiny::need(nrow(zdtmt) > 0 && ncol(zdtmt) > 0,
                    "All terms removed by sparsity filter. Try a lower value.")
      )

      # Back to Matrix::dgCMatrix
      triplet <- slam::as.simple_triplet_matrix(zdtmt)
      zdtmF   <- Matrix::sparseMatrix(
        i        = triplet$i,
        j        = triplet$j,
        x        = triplet$v,
        dims     = dim(triplet),
        dimnames = dimnames(triplet)
      )
      final_dtm(zdtmF)

      tf_mat <- textmineR::TermDocFreq(zdtmF)
      tf_mat_output(tf_mat)

      shinybusy::remove_modal_spinner()
      shinyjs::enable("download_tfmat")
      shinyjs::enable("download_dtm")

      shiny::showNotification(
        paste0("Preprocessing complete. DTM: ", nrow(zdtmF),
               " docs x ", ncol(zdtmF), " terms."),
        type = "message", duration = 6
      )
    })

    # ------------------------------------------------------------------
    # OUTPUTS
    # ------------------------------------------------------------------
    output$dtm_dims <- shiny::renderPrint({
      shiny::req(dtm_raw()); dim(dtm_raw())
    })

    output$dtm_slim_dims <- shiny::renderPrint({
      shiny::req(dtm_slim()); dim(dtm_slim())
    })

    output$tf_table_dt <- DT::renderDT({
      shiny::req(tf_mat_output())
      DT::datatable(tf_mat_output(),
                    options = list(pageLength = 15, scrollX = TRUE))
    })

    output$download_dtm <- downloadHandler(
      filename = function() "dtm_final_sparse.rds",
      content  = function(file) saveRDS(final_dtm(), file)
    )

    output$download_tfmat <- downloadHandler(
      filename = function() "tf_mat.xlsx",
      content  = function(file) openxlsx::write.xlsx(tf_mat_output(), file)
    )

    # ------------------------------------------------------------------
    # RETURN
    # ------------------------------------------------------------------
    list(
      dtm    = shiny::reactive(final_dtm()),
      tf_mat = shiny::reactive(tf_mat_output())
    )
  })
}
