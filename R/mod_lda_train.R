# ======================================================================
# MODULE: Final LDA Model Training
# ======================================================================

#' Final LDA Model Training Module
#'
#' @description
#' Trains the final LDA model using the processed DTM. The number of topics (K)
#' is automatically initialised from the inference module but can be overridden.
#'
#' @name mod_lda_train
#' @param id Shiny module ID.
#' @param dtm_reactive Reactive returning the DTM (\code{dgCMatrix}).
#' @param k_optimal_reactive Reactive returning the optimal K (\code{integer}).
#' @param bd_reactive Reactive returning the original metadata \code{data.frame}.
#'
#' @importFrom shiny NS tagList fluidRow column h2 br icon tags p strong
#' @importFrom shiny numericInput selectInput actionButton textOutput plotOutput
#' @importFrom shiny downloadButton moduleServer reactive observe observeEvent
#' @importFrom shiny req updateNumericInput checkboxInput checkboxGroupInput
#' @importFrom shiny validate need renderText renderUI showNotification
#' @importFrom shinydashboard box
#' @importFrom DT DTOutput renderDT datatable formatRound
#' @importFrom shinybusy show_modal_spinner remove_modal_spinner add_busy_spinner
#' @importFrom shinyjs disable enable
#' @importFrom textmineR FitLdaModel GetTopTerms LabelTopics
#' @importFrom dplyr group_by summarise mutate filter select desc arrange
#' @importFrom dplyr slice_max ungroup inner_join rename distinct
#' @importFrom tidyr pivot_longer
#' @importFrom tibble as_tibble rowid_to_column
#' @importFrom openxlsx write.xlsx
#' @importFrom parallel detectCores
#' @importFrom wordcloud wordcloud
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @importFrom colourpicker colourInput
#' @importFrom grDevices png jpeg pdf tiff dev.off
#'
#' @return A list of reactive values with the LDA model outputs.
#' @export
NULL

#' @rdname mod_lda_train
#' @export
mod_lda_train <- function() invisible(NULL)


# ======================================================================
# UI
# ======================================================================

#' @rdname mod_lda_train
#' @export
mod_lda_train_ui <- function(id) {
  ns <- NS(id)

  n_cores_max <- max(1L, parallel::detectCores(logical = TRUE))
  n_cores_def <- max(1L, n_cores_max - 1L)

  tagList(
    shinybusy::add_busy_spinner(
      spin = "fading-circle", position = "top-right", margins = c(15, 15)),

    shiny::h2("Final LDA Model Training"),

    fluidRow(

      # ----------------------------------------------------------------
      # PARAMS BOX
      # ----------------------------------------------------------------
      shinydashboard::box(
        width = 4, title = "Model Parameters",
        status = "primary", solidHeader = TRUE,

        numericInput(ns("k"), tags$strong("Number of topics (K):"),
                     value = 10, min = 2, max = 500),
        tags$p(tags$i(
          "Initialised with the optimal K from the inference module.")),
        hr(),
        tags$h4("Advanced Parameters"),
        fluidRow(
          column(6, numericInput(ns("iterations"), "Iterations:",
                                 500, min = 100, max = 5000, step = 50)),
          column(6, numericInput(ns("burnin"),     "Burn-in:",
                                 50,  min = 0,   max = 1000, step = 10))
        ),
        fluidRow(
          column(6, numericInput(ns("alpha"), "Alpha:", 0.1,  min = 0.001, max = 10,  step = 0.01)),
          column(6, numericInput(ns("beta"),  "Beta:",  0.05, min = 0.001, max = 1,   step = 0.01))
        ),
        hr(),
        sliderInput(ns("cpus"), "CPUs:", min = 1L, max = n_cores_max, value = n_cores_def),

        shiny::checkboxInput(ns("optimize_alpha"), tags$strong("Optimize Alpha"), TRUE),
        shiny::checkboxGroupInput(
          ns("calc_options"), tags$strong("Evaluation Metrics:"),
          choices = c(
            "Calculate Likelihood" = "calc_likelihood",
            "Calculate Coherence"  = "calc_coherence",
            "Calculate R-squared"  = "calc_r2"
          ),
          selected = c("calc_likelihood", "calc_coherence")
        ),
        br(),
        shiny::actionButton(ns("run_lda_train"), "Train Final LDA Model",
                            class = "btn-danger", icon = icon("play")),
        br(), br(),
        shiny::textOutput(ns("model_status")),
        br(),
        downloadButton(ns("download_model_rds"), "Download LDA Model (.rds)")
      ),

      # ----------------------------------------------------------------
      # OUTPUTS BOX
      # ----------------------------------------------------------------
      shinydashboard::box(
        width = 8, title = "Model Results", status = "info",

        tabsetPanel(

          tabPanel("Model Evaluation Metrics", br(),
                   tags$p("Global model evaluation metrics."),
                   DT::DTOutput(ns("metrics_table")), br(),
                   downloadButton(ns("download_metrics"), "Download Metrics (Excel)")),

          tabPanel("Top Terms Matrix", br(),
                   numericInput(ns("top_terms_m"), tags$strong("Top terms (M) per topic:"),
                                20, min = 3, max = 100),
                   tags$p("Columns = topics; rows = term rank (1 = most important)."),
                   DT::DTOutput(ns("top_terms_matrix_dt")),
                   downloadButton(ns("download_top_terms_matrix"), "Download (Excel)")),

          tabPanel("Document Topic Assignment", br(),
                   tags$p("Dominant topic for each document."),
                   DT::DTOutput(ns("top_topic_assignment_dt")),
                   downloadButton(ns("download_top_topics_assignment"),
                                  "Download (toptopic.xlsx)")),

          tabPanel("Top Documents per Topic", br(),
                   numericInput(ns("top_docs_m"), tags$strong("Top documents per topic:"),
                                5, min = 1, max = 50),
                   DT::DTOutput(ns("top_m_docs_dt")),
                   downloadButton(ns("download_top_m_docs"), "Download (clasificacion.xlsx)")),

          tabPanel("Topic-Term Weights (Beta)", br(),
                   DT::DTOutput(ns("tidy_beta_dt")),
                   downloadButton(ns("download_tidy_beta"), "Download (tidy_beta.xlsx)")),

          tabPanel("Document-Topic Weights (Gamma)", br(),
                   DT::DTOutput(ns("tidy_gamma_dt")),
                   downloadButton(ns("download_tidy_gamma"), "Download (tidy_gamma.xlsx)")),

          tabPanel(
            "Topic Word Cloud", br(),
            fluidRow(
              column(4,
                     tags$h4(icon("cloud"), "Word Cloud Configuration"),
                     shiny::uiOutput(ns("topic_select_ui")),
                     sliderInput(ns("wc_max_words"), "Max Words:", 10, 200, 50, step = 10),
                     colourpicker::colourInput(ns("wc_base_color"), "Base Color:", "#4682B4"),
                     shiny::selectInput(
                       ns("wc_color_palette"), "Color Palette:",
                       choices  = c("None (Use Base Color)" = "None",
                                    rownames(RColorBrewer::brewer.pal.info)),
                       selected = "Set1"
                     ),
                     shiny::actionButton(ns("generate_wordcloud"), "Generate Word Cloud",
                                         icon = icon("sync"), class = "btn-info"),
                     hr(),
                     tags$h4("Export Options"),
                     shiny::selectInput(ns("wc_export_format"), "Format:",
                                        choices = c("png", "jpeg", "pdf", "tiff")),
                     numericInput(ns("wc_export_dpi"),    "DPI:",           300, 72, 1200, 50),
                     numericInput(ns("wc_export_width"),  "Width (in):",    8, 1, 20),
                     numericInput(ns("wc_export_height"), "Height (in):",   8, 1, 20),
                     downloadButton(ns("download_wordcloud_plot"), "Download Word Cloud")
              ),
              column(8,
                     tags$h4(shiny::textOutput(ns("selected_topic_title"))),
                     shiny::plotOutput(ns("topic_wordcloud_plot"), height = "500px")
              )
            )
          )
        )
      )
    )
  )
}


# ======================================================================
# SERVER
# ======================================================================

#' @rdname mod_lda_train
#' @export
mod_lda_train_server <- function(id, dtm_reactive, k_optimal_reactive, bd_reactive) {
  moduleServer(id, function(input, output, session) {

    shinyjs::disable("download_model_rds")

    rv <- shiny::reactiveValues(
      lda_model                = NULL,
      tidy_beta                = NULL,
      tidy_gamma               = NULL,
      df_year_topic            = NULL,
      df_source_topic          = NULL,
      global_metrics_df        = NULL,
      top_topics_df            = NULL,
      top_topics_with_label_df = NULL
    )

    # ------------------------------------------------------------------
    # 1. Sync K from inference module
    # ------------------------------------------------------------------
    shiny::observe({
      k_opt <- k_optimal_reactive()
      if (!is.null(k_opt) && is.numeric(k_opt) && k_opt >= 2L) {
        shiny::updateNumericInput(session, "k",     value = as.integer(k_opt))
        shiny::updateNumericInput(session, "alpha", value = round(50 / k_opt, 2))
      }
    })

    # ------------------------------------------------------------------
    # 2. Model Training
    # ------------------------------------------------------------------
    shiny::observeEvent(input$run_lda_train, {

      shiny::req(dtm_reactive())
      dtm_local <- dtm_reactive()

      shiny::validate(
        shiny::need(input$k >= 2L, "K must be >= 2."),
        shiny::need(NROW(dtm_local) > 1L && NCOL(dtm_local) > 1L,
                    "DTM must have >= 2 documents and >= 2 terms.")
      )

      k_local               <- as.integer(input$k)
      calc_likelihood_local <- "calc_likelihood" %in% input$calc_options
      calc_coherence_local  <- "calc_coherence"  %in% input$calc_options
      calc_r2_local         <- "calc_r2"         %in% input$calc_options

      shinybusy::show_modal_spinner(
        spin  = "fading-circle",
        text  = tags$div(
          style = "font-size:20px; font-weight:bold; color:#2c3e50;",
          sprintf("Training final LDA model with K = %d...", k_local)
        ),
        color = "#2c3e50"
      )

      time_result <- system.time(tryCatch({

        m <- textmineR::FitLdaModel(
          dtm             = dtm_local,
          k               = k_local,
          iterations      = as.integer(input$iterations),
          burnin          = as.integer(input$burnin),
          alpha           = as.numeric(input$alpha),
          beta            = as.numeric(input$beta),
          optimize_alpha  = isTRUE(input$optimize_alpha),
          calc_likelihood = calc_likelihood_local,
          calc_coherence  = calc_coherence_local,
          calc_r2         = calc_r2_local,
          cpus            = as.integer(input$cpus)
        )

        # Store parameters (FitLdaModel does not return them)
        m$k              <- k_local
        m$iterations     <- as.integer(input$iterations)
        m$burnin         <- as.integer(input$burnin)
        m$alpha          <- as.numeric(input$alpha)
        m$beta           <- as.numeric(input$beta)
        m$optimize_alpha <- isTRUE(input$optimize_alpha)

        # Prevalence
        m$prevalence <- colSums(m$theta) / sum(m$theta) * 100

        # Labels
        m$labels <- tryCatch(
          textmineR::LabelTopics(
            assignments = m$theta > 0.15,
            dtm         = dtm_local,
            M           = 1L
          ),
          error = function(e) {
            df_ph <- data.frame(label = rep("No Label", NROW(m$phi)),
                                stringsAsFactors = FALSE)
            rownames(df_ph) <- rownames(m$phi)
            df_ph
          }
        )

        labels_vec <- if ("label" %in% colnames(m$labels)) {
          as.character(m$labels[, "label"])
        } else {
          rep("No Label", NROW(m$phi))
        }

        label_df <- data.frame(Topic = rownames(m$phi),
                               Label = labels_vec,
                               stringsAsFactors = FALSE)

        # Dominant topic per document
        top_topics_vec <- apply(m$theta, 1, function(x) names(which.max(x))[1])

        rv$top_topics_with_label_df <- data.frame(
          Document  = names(top_topics_vec),
          Top_Topic = top_topics_vec,
          stringsAsFactors = FALSE
        ) |>
          dplyr::inner_join(label_df, by = c("Top_Topic" = "Topic"))

        rv$top_topics_df <- rv$top_topics_with_label_df |>
          dplyr::select(-Label) |>
          dplyr::rename(Dominant_Topic = Top_Topic)

        rv$lda_model <- m

        # Global metrics
        fmt <- function(val, d = 4) {
          if (is.null(val) || length(val) == 0 || is.na(val[1])) return("N/A")
          if (is.numeric(val[1]) && val[1] == round(val[1]))
            return(as.character(as.integer(val[1])))
          sprintf(paste0("%.", d, "f"), val[1])
        }

        log_lik_mean <- if (calc_likelihood_local && !is.null(m$log_likelihood))
          mean(m$log_likelihood, na.rm = TRUE) else NA_real_

        rv$global_metrics_df <- data.frame(
          Metric = c("K", "Iterations", "Burnin", "Alpha", "Beta",
                     "Optimized Alpha", "Log-Likelihood (mean)",
                     "Coherence (mean)", "R-squared"),
          Value  = c(
            fmt(m$k, 0), fmt(m$iterations, 0), fmt(m$burnin, 0),
            fmt(m$alpha), fmt(m$beta),
            ifelse(isTRUE(m$optimize_alpha), "TRUE", "FALSE"),
            fmt(log_lik_mean),
            fmt(mean(m$coherence, na.rm = TRUE)),
            fmt(if ("r2" %in% names(m)) m$r2 else NA)
          ),
          stringsAsFactors = FALSE
        )

        # Tidy beta / gamma
        rv$tidy_beta <- m$phi |>
          tibble::as_tibble(rownames = "Topic") |>
          tidyr::pivot_longer(-Topic, names_to = "Term", values_to = "Beta")

        rv$tidy_gamma <- m$theta |>
          tibble::as_tibble(rownames = "Document") |>
          tidyr::pivot_longer(-Document, names_to = "Topic", values_to = "Gamma")

        # Trends data
        bd_local <- bd_reactive()
        shiny::req(bd_local)
        if ("database" %in% names(bd_local))
          bd_local <- dplyr::rename(bd_local, source = database)

        df_gamma_year <- rv$tidy_gamma |>
          dplyr::mutate(Document = as.integer(gsub("[^0-9]", "", Document))) |>
          dplyr::inner_join(
            bd_local |>
              tibble::rowid_to_column(var = "Document") |>
              dplyr::select(Document, year, source),
            by = "Document"
          )

        rv$df_year_topic <- df_gamma_year |>
          dplyr::group_by(year, Topic) |>
          dplyr::summarise(Intensity = mean(Gamma), .groups = "drop") |>
          dplyr::filter(!is.na(year))

        rv$df_source_topic <- df_gamma_year |>
          dplyr::group_by(source, Topic) |>
          dplyr::summarise(Intensity = mean(Gamma), .groups = "drop") |>
          dplyr::filter(!is.na(source))

      }, error = function(e) {
        shinybusy::remove_modal_spinner()
        shiny::showNotification(paste("Training failed:", e$message),
                                type = "error", duration = 8)
      }))

      shinybusy::remove_modal_spinner()
      shinyjs::enable("download_model_rds")

      output$model_status <- shiny::renderText({
        if (!is.null(rv$lda_model)) {
          sprintf("Model trained. K=%d (%.2f s). Download available below.",
                  k_local, time_result[["elapsed"]])
        } else {
          "Model training failed."
        }
      })
    })

    # ------------------------------------------------------------------
    # 3. Top Terms (reactive, respects top_terms_m input)
    # ------------------------------------------------------------------
    top_terms_matrix_reactive <- shiny::reactive({
      shiny::req(rv$lda_model)
      phi <- rv$lda_model$phi
      M   <- max(3L, as.integer(input$top_terms_m %||% 20L))

      topic_names <- if (!is.null(rownames(phi))) rownames(phi) else
        paste0("Topic_", seq_len(nrow(phi)))

      cols <- lapply(seq_len(nrow(phi)), function(i) {
        ord  <- order(phi[i, ], decreasing = TRUE)
        trms <- colnames(phi)[ord]
        if (length(trms) >= M) trms[seq_len(M)] else
          c(trms, rep("", M - length(trms)))
      })
      df <- as.data.frame(cols, stringsAsFactors = FALSE)
      colnames(df)  <- topic_names
      rownames(df)  <- paste0("Rank_", seq_len(M))
      df
    })

    top_docs_reactive <- shiny::reactive({
      shiny::req(rv$tidy_gamma)
      m_local <- max(1L, as.integer(input$top_docs_m %||% 5L))
      rv$tidy_gamma |>
        dplyr::group_by(Topic) |>
        dplyr::slice_max(Gamma, n = m_local, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::select(Topic, Document, Gamma) |>
        dplyr::rename(Classification_Gamma = Gamma)
    })

    # ------------------------------------------------------------------
    # 4. Word Cloud
    # ------------------------------------------------------------------
    output$topic_select_ui <- shiny::renderUI({
      shiny::req(rv$lda_model)
      tnms <- rownames(rv$lda_model$phi) %||% paste0("Topic_", seq_len(nrow(rv$lda_model$phi)))
      shiny::selectInput(session$ns("wc_topic"), "Select Topic:",
                         choices = tnms, selected = tnms[1])
    })

    output$selected_topic_title <- shiny::renderText({
      shiny::req(input$wc_topic)
      paste("Word Cloud for", input$wc_topic)
    })

    wordcloud_data_computed <- shiny::reactive({
      shiny::req(rv$lda_model, input$wc_topic)
      phi <- rv$lda_model$phi
      if (!input$wc_topic %in% rownames(phi)) return(NULL)
      row <- phi[input$wc_topic, ]
      data.frame(term = names(row), weight = as.numeric(row),
                 stringsAsFactors = FALSE) |>
        dplyr::arrange(desc(weight))
    })

    draw_wordcloud <- function() {
      pd <- wordcloud_data_computed()
      shiny::req(pd)
      pd <- utils::head(pd, as.integer(input$wc_max_words))

      colors_wc <- if (input$wc_color_palette == "None") {
        input$wc_base_color
      } else {
        n_col <- max(3L, min(8L, as.integer(input$wc_max_words)))
        RColorBrewer::brewer.pal(n_col, input$wc_color_palette)
      }

      min_freq <- min(pd$weight[pd$weight > 1e-6], na.rm = TRUE)
      wordcloud::wordcloud(
        words        = pd$term,
        freq         = pd$weight,
        min.freq     = min_freq,
        max.words    = as.integer(input$wc_max_words),
        random.order = FALSE,
        colors       = colors_wc
      )
      invisible(NULL)
    }

    output$topic_wordcloud_plot <- shiny::renderPlot({
      shiny::req(input$generate_wordcloud)
      shiny::isolate(draw_wordcloud())
    })

    output$download_wordcloud_plot <- downloadHandler(
      filename = function() paste0("wordcloud_", input$wc_topic, ".", input$wc_export_format),
      content  = function(file) {
        shiny::req(wordcloud_data_computed())
        dev_fn <- switch(input$wc_export_format,
                         png  = grDevices::png,
                         jpeg = grDevices::jpeg,
                         tiff = grDevices::tiff,
                         pdf  = grDevices::pdf,
                         stop("Invalid format"))
        if (input$wc_export_format == "pdf") {
          dev_fn(file,
                 width  = as.numeric(input$wc_export_width),
                 height = as.numeric(input$wc_export_height))
        } else {
          dev_fn(file,
                 width  = as.numeric(input$wc_export_width),
                 height = as.numeric(input$wc_export_height),
                 units  = "in",
                 res    = as.integer(input$wc_export_dpi))
        }
        draw_wordcloud()
        grDevices::dev.off()
      }
    )

    # ------------------------------------------------------------------
    # 5. Table Outputs
    # ------------------------------------------------------------------
    output$metrics_table <- DT::renderDT({
      shiny::req(rv$global_metrics_df)
      DT::datatable(rv$global_metrics_df,
                    options = list(scrollX = TRUE, pageLength = 10))
    })

    output$top_terms_matrix_dt <- DT::renderDT({
      DT::datatable(top_terms_matrix_reactive(),
                    options = list(scrollX = TRUE, pageLength = 20))
    })

    output$top_topic_assignment_dt <- DT::renderDT({
      shiny::req(rv$top_topics_df)
      DT::datatable(rv$top_topics_df,
                    options = list(scrollX = TRUE, pageLength = 10))
    })

    output$top_m_docs_dt <- DT::renderDT({
      shiny::req(top_docs_reactive())
      DT::datatable(top_docs_reactive(),
                    options = list(scrollX = TRUE, pageLength = 10)) |>
        DT::formatRound("Classification_Gamma", 6)
    })

    output$tidy_beta_dt <- DT::renderDT({
      shiny::req(rv$tidy_beta)
      smpl <- rv$tidy_beta |>
        dplyr::group_by(Topic) |>
        dplyr::slice_max(Beta, n = 5L, with_ties = FALSE) |>
        dplyr::ungroup()
      DT::datatable(smpl, options = list(scrollX = TRUE, pageLength = 10)) |>
        DT::formatRound("Beta", 6)
    })

    output$tidy_gamma_dt <- DT::renderDT({
      shiny::req(rv$tidy_gamma)
      smpl <- rv$tidy_gamma |>
        dplyr::group_by(Topic) |>
        dplyr::slice_max(Gamma, n = 5L, with_ties = FALSE) |>
        dplyr::ungroup()
      DT::datatable(smpl, options = list(scrollX = TRUE, pageLength = 10)) |>
        DT::formatRound("Gamma", 6)
    })

    # ------------------------------------------------------------------
    # 6. Downloads
    # ------------------------------------------------------------------
    output$download_model_rds <- downloadHandler(
      filename = function() "lda_final_model.rds",
      content  = function(file) saveRDS(rv$lda_model, file)
    )
    output$download_metrics <- downloadHandler(
      filename = function() "lda_model_metrics.xlsx",
      content  = function(file) {
        shiny::req(rv$global_metrics_df)
        openxlsx::write.xlsx(rv$global_metrics_df, file)
      }
    )
    output$download_top_terms_matrix <- downloadHandler(
      filename = function() "lda_top_terms_matrix.xlsx",
      content  = function(file) openxlsx::write.xlsx(top_terms_matrix_reactive(), file)
    )
    output$download_top_topics_assignment <- downloadHandler(
      filename = function() "toptopic.xlsx",
      content  = function(file) openxlsx::write.xlsx(rv$top_topics_df, file)
    )
    output$download_top_m_docs <- downloadHandler(
      filename = function() "clasificacion.xlsx",
      content  = function(file) openxlsx::write.xlsx(top_docs_reactive(), file)
    )
    output$download_tidy_beta <- downloadHandler(
      filename = function() "tidy_beta.xlsx",
      content  = function(file) openxlsx::write.xlsx(rv$tidy_beta, file)
    )
    output$download_tidy_gamma <- downloadHandler(
      filename = function() "tidy_gamma.xlsx",
      content  = function(file) openxlsx::write.xlsx(rv$tidy_gamma, file)
    )

    # ------------------------------------------------------------------
    # 7. Return
    # ------------------------------------------------------------------
    list(
      tidy_beta       = shiny::reactive(rv$tidy_beta),
      tidy_gamma      = shiny::reactive(rv$tidy_gamma),
      lda_model       = shiny::reactive(rv$lda_model),
      df_year_topic   = shiny::reactive(rv$df_year_topic),
      df_source_topic = shiny::reactive(rv$df_source_topic)
    )
  })
}

# Null-coalescing helper (base R)
`%||%` <- function(a, b) if (!is.null(a)) a else b
