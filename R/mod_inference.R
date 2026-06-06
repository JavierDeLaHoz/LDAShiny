# ======================================================================
# MODULE: LDA Inference (K Selection via Coherence)
# ======================================================================

#' LDA Inference Module UI
#'
#' @description
#' UI for running multiple LDA models over a range of topic counts (k) and
#' selecting the best k via mean coherence.
#'
#' @param id Shiny module ID.
#' @return A Shiny UI element (\code{tagList}).
#'
#' @importFrom shiny NS tagList h2 h4 tabsetPanel tabPanel fluidRow column
#' @importFrom shiny numericInput sliderInput actionButton verbatimTextOutput
#' @importFrom shiny plotOutput downloadButton textInput selectInput br tags
#' @importFrom shinybusy add_busy_spinner
#' @importFrom shinyWidgets dropdown
#' @importFrom colourpicker colourInput
#' @importFrom DT DTOutput
#' @importFrom parallel detectCores
#' @export
mod_inference_ui <- function(id) {
  ns <- NS(id)

  n_cores_max <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
  n_cores_def <- max(1L, n_cores_max - 1L)

  tagList(

    shinybusy::add_busy_spinner(
      spin     = "fading-circle",
      position = "top-right",
      margins  = c(15, 15)
    ),

    shiny::h2("LDA Model Selection (Inference)"),

    tabsetPanel(

      # ----------------------------------------------------------------
      # TAB 1: SETTINGS
      # ----------------------------------------------------------------
      tabPanel(
        title = "Settings",
        shiny::h4("LDA Inference Options"),

        fluidRow(
          column(4, numericInput(ns("k_start"), "Min topics (k start):",
                                 value = 5,  min = 2,   max = 500)),
          column(4, numericInput(ns("k_end"),   "Max topics (k end):",
                                 value = 40, min = 3,   max = 500)),
          column(4, numericInput(ns("k_step"),  "Step for k:",
                                 value = 1,  min = 1,   max = 50))
        ),

        fluidRow(
          column(4, numericInput(ns("iterations"), "Iterations:",
                                 value = 500, min = 100, max = 5000, step = 50)),
          column(4, numericInput(ns("burnin"),     "Burn-in:",
                                 value = 50,  min = 0,   max = 1000, step = 10)),
          column(4, numericInput(ns("alpha"),      "Alpha:",
                                 value = 0.1, min = 0.001, max = 10, step = 0.01))
        ),

        sliderInput(ns("cpus"), "Number of CPUs:",
                    min = 1L, max = n_cores_max, value = n_cores_def),

        br(),
        shiny::actionButton(ns("run_inference"), "Run LDA Inference",
                            class = "btn-danger"),
        br(), br(),
        shiny::verbatimTextOutput(ns("inference_status"))
      ),

      # ----------------------------------------------------------------
      # TAB 2: COHERENCE TABLE
      # ----------------------------------------------------------------
      tabPanel(
        title = "Coherence Table",
        br(),
        DT::DTOutput(ns("coherence_dt")),
        br(),
        downloadButton(ns("download_coherence"),
                       "Download coherence table (Excel)")
      ),

      # ----------------------------------------------------------------
      # TAB 3: COHERENCE PLOT
      # ----------------------------------------------------------------
      tabPanel(
        title = "Coherence Plot",
        br(),
        shiny::h3("Customize and download the graph"),

        shinyWidgets::dropdown(
          tags$h3("Customize graph"),
          width  = "850px",
          status = "danger",
          icon   = shiny::icon("sliders-h"),
          style  = "unite",

          fluidRow(
            column(4,
                   tags$h4("Labels and Theme"),
                   shiny::textInput(ns("xlab"),  "X-axis Label", "Number of topics (k)"),
                   shiny::textInput(ns("ylab"),  "Y-axis Label", "Coherence"),
                   shiny::selectInput(ns("font_family"), "Font Family",
                                      choices = c("sans", "serif", "mono")),
                   shiny::selectInput(ns("theme_choice"), "Theme",
                                      choices = names(lda_themes())),
                   sliderInput(ns("font_size"), "Font Size", 6, 30, 12)
            ),
            column(4,
                   tags$h4("Line and Point Style"),
                   colourpicker::colourInput(ns("line_color"),  "Line Color",  "blue"),
                   sliderInput(ns("line_width"),  "Line Width",  0.1, 5, 1.5),
                   colourpicker::colourInput(ns("point_color"), "Point Color", "black"),
                   sliderInput(ns("point_size"),  "Point Size",  1, 10, 3),
                   sliderInput(ns("point_alpha"), "Alpha",       0.01, 1, 0.7),
                   sliderInput(ns("point_shape"), "Point Shape", 0, 25, 16)
            ),
            column(4,
                   tags$h4("Export Options"),
                   shiny::selectInput(ns("export_format"), "Format:",
                                      choices = c("png", "tiff", "jpeg", "pdf")),
                   numericInput(ns("export_width"),  "Width (in)",  7),
                   numericInput(ns("export_height"), "Height (in)", 5),
                   numericInput(ns("export_dpi"),    "DPI",         300),
                   br(),
                   downloadButton(ns("download_plot"), "Download Plot",
                                  class = "btn-success")
            )
          )
        ),

        br(),
        shiny::plotOutput(ns("coherence_plot"), height = "500px")
      )
    )
  )
}


# ======================================================================
# SERVER
# ======================================================================

#' LDA Inference Module Server
#'
#' @param id Shiny module ID.
#' @param dtm_reactive A reactive expression returning a \code{dgCMatrix} DTM.
#' @return A list with:
#' \describe{
#'   \item{model_list}{Reactive list of fitted LDA models, one per k.}
#'   \item{coherence_mat}{Reactive \code{data.frame} with columns \code{k} and
#'     \code{coherence}.}
#'   \item{k_optimal}{Reactive integer: k with highest mean coherence.}
#' }
#'
#' @importFrom shiny moduleServer reactiveValues reactive observe observeEvent
#' @importFrom shiny req validate need renderText renderPlot downloadHandler
#' @importFrom shinybusy show_modal_spinner remove_modal_spinner
#' @importFrom shinyjs disable enable
#' @importFrom DT renderDT datatable
#' @importFrom textmineR TmParallelApply FitLdaModel
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme element_text ggsave
#' @importFrom openxlsx write.xlsx
#' @importFrom parallel detectCores
#' @export
mod_inference_server <- function(id, dtm_reactive) {
  moduleServer(id, function(input, output, session) {

    shinyjs::disable("download_coherence")
    shinyjs::disable("download_plot")

    rv <- shiny::reactiveValues(
      model_list             = NULL,
      coherence_mat          = NULL,
      inference_time_elapsed = NULL
    )

    # Safe DTM check
    safe_dtm <- shiny::reactive({
      dtm <- dtm_reactive()
      if (is.null(dtm)) return(NULL)
      if (!inherits(dtm, "dgCMatrix")) return(NULL)
      if (NROW(dtm) == 0L || NCOL(dtm) == 0L) return(NULL)
      dtm
    })

    # ------------------------------------------------------------------
    # MAIN INFERENCE
    # ------------------------------------------------------------------
    shiny::observeEvent(input$run_inference, {

      shiny::req(safe_dtm())
      dtm_local <- safe_dtm()

      shiny::validate(
        shiny::need(input$k_end > input$k_start, "k end must be > k start."),
        shiny::need(input$k_step > 0,            "k step must be positive."),
        shiny::need(NROW(dtm_local) > 1L && NCOL(dtm_local) > 1L,
                    "DTM must have >= 2 documents and >= 2 terms.")
      )

      k_list           <- seq(as.integer(input$k_start),
                              as.integer(input$k_end),
                              by = as.integer(input$k_step))
      iterations_local <- as.integer(input$iterations)
      burnin_local     <- as.integer(input$burnin)
      alpha_local      <- as.numeric(input$alpha)
      cpus_local       <- min(as.integer(input$cpus),
                              max(1L, parallel::detectCores() - 1L))

      shinybusy::show_modal_spinner(
        spin  = "orbit",
        text  = tags$div(
          style = "font-size:20px; font-weight:bold; color:#2c3e50;",
          sprintf("Running LDA inference for %d models...", length(k_list))
        ),
        color = "#2c3e50"
      )

      output$inference_status <- shiny::renderText(
        sprintf("Running inference: k = %d to %d (step %d), iterations = %d",
                input$k_start, input$k_end, input$k_step, iterations_local)
      )

      time_result <- system.time({
        model_list <- tryCatch(
          textmineR::TmParallelApply(
            X   = k_list,
            FUN = function(k) {
              m    <- textmineR::FitLdaModel(
                dtm             = dtm_local,
                k               = k,
                iterations      = iterations_local,
                burnin          = burnin_local,
                alpha           = alpha_local,
                optimize_alpha  = TRUE,
                calc_likelihood = TRUE,
                calc_coherence  = TRUE,
                calc_r2         = FALSE
              )
              m$k <- k
              m
            },
            export = c("dtm_local", "iterations_local",
                       "burnin_local", "alpha_local"),
            cpus   = cpus_local
          ),
          error = function(e) {
            shiny::showNotification(
              paste("Inference error:", e$message),
              type = "error", duration = 8)
            NULL
          }
        )

        if (!is.null(model_list)) {
          rv$model_list <- model_list
          rv$coherence_mat <- data.frame(
            k         = vapply(model_list, function(x) x$k,         integer(1)),
            coherence = vapply(model_list, function(x) mean(x$coherence), double(1))
          )
        }
      })

      rv$inference_time_elapsed <- time_result[["elapsed"]]
      shinybusy::remove_modal_spinner()

      if (!is.null(rv$coherence_mat) && NROW(rv$coherence_mat) > 0) {
        shinyjs::enable("download_coherence")
        shinyjs::enable("download_plot")

        max_k <- rv$coherence_mat$k[which.max(rv$coherence_mat$coherence)]
        output$inference_status <- shiny::renderText(
          sprintf("Inference complete. Best k = %d (%.2f s)",
                  max_k, rv$inference_time_elapsed)
        )

        shiny::showNotification(
          sprintf("Inference complete. Best k = %d", max_k),
          type = "message", duration = 5
        )
      } else {
        output$inference_status <- shiny::renderText(
          "Inference failed. No models were trained successfully.")
      }
    })

    # Best k
    best_k_reactive <- shiny::reactive({
      shiny::req(rv$coherence_mat)
      as.integer(rv$coherence_mat$k[which.max(rv$coherence_mat$coherence)])
    })

    # ------------------------------------------------------------------
    # OUTPUTS
    # ------------------------------------------------------------------
    output$coherence_dt <- DT::renderDT({
      shiny::req(rv$coherence_mat)
      DT::datatable(rv$coherence_mat, options = list(pageLength = 10))
    })

    coherence_plot <- shiny::reactive({
      shiny::req(rv$coherence_mat)

      themes <- lda_themes()

      ggplot2::ggplot(rv$coherence_mat, ggplot2::aes(k, coherence)) +
        ggplot2::geom_line(color    = input$line_color,
                           linewidth = as.numeric(input$line_width)) +
        ggplot2::geom_point(
          color = input$point_color, size  = as.numeric(input$point_size),
          alpha = as.numeric(input$point_alpha),
          shape = as.integer(input$point_shape)
        ) +
        ggplot2::labs(x = input$xlab, y = input$ylab) +
        themes[[input$theme_choice]] +
        ggplot2::theme(
          text = ggplot2::element_text(
            family = input$font_family,
            size   = as.numeric(input$font_size)
          )
        )
    })

    output$coherence_plot <- shiny::renderPlot(coherence_plot())

    output$download_plot <- downloadHandler(
      filename = function() paste0("coherence_plot.", input$export_format),
      content  = function(file) {
        ggplot2::ggsave(
          filename = file, plot   = coherence_plot(),
          device   = input$export_format,
          dpi      = as.numeric(input$export_dpi),
          width    = as.numeric(input$export_width),
          height   = as.numeric(input$export_height)
        )
      }
    )

    output$download_coherence <- downloadHandler(
      filename = function() "coherence_mat.xlsx",
      content  = function(file) openxlsx::write.xlsx(rv$coherence_mat, file)
    )

    # ------------------------------------------------------------------
    # RETURN
    # ------------------------------------------------------------------
    list(
      model_list    = shiny::reactive(rv$model_list),
      coherence_mat = shiny::reactive(rv$coherence_mat),
      k_optimal     = best_k_reactive
    )
  })
}
