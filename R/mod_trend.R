# ======================================================================
# MODULE: Topic Trend Analysis (HOT / COLD / EQUAL)
# ======================================================================

#' Topic Trend Analysis Module UI
#'
#' @description
#' Performs linear regression on topic intensity (mean Gamma) over years and
#' classifies topics as increasing (HOT), decreasing (COLD), or stable (EQUAL).
#'
#' @param id Shiny module ID.
#' @return A Shiny UI element (\code{tagList}).
#'
#' @importFrom shiny NS tagList fluidRow column h2 br icon tags p strong
#' @importFrom shiny numericInput actionButton textOutput plotOutput
#' @importFrom shiny downloadButton textInput selectInput sliderInput
#' @importFrom shiny renderUI
#' @importFrom shinydashboard box
#' @importFrom shinyWidgets dropdown
#' @importFrom colourpicker colourInput
#' @importFrom DT DTOutput
#' @export
mod_trend_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shiny::h2("Topic Trend Analysis"),
    tags$p(
      tags$i(paste0(
        "Classifies topics by linear regression (Year vs. mean Gamma). ",
        "HOT = positive significant slope, COLD = negative significant slope, ",
        "EQUAL = non-significant slope."
      ))
    ),

    fluidRow(

      # ----------------------------------------------------------------
      # CONTROL BOX
      # ----------------------------------------------------------------
      shinydashboard::box(
        width = 4, title = "Configuration", status = "primary", solidHeader = TRUE,

        numericInput(ns("p_value_threshold"), tags$b("P-Value Threshold:"),
                     value = 0.05, min = 0.001, max = 0.2, step = 0.01),

        shiny::uiOutput(ns("topic_selector_ui")),

        br(),
        shiny::actionButton(ns("run_trend_analysis"), "Run Trend Analysis",
                            class = "btn-danger", icon = icon("chart-line")),
        br(), br(),
        shiny::textOutput(ns("analysis_status")),
        hr(),
        tags$h4("Download"),
        downloadButton(ns("download_trend_table"),
                       "Regression Results (Excel)")
      ),

      # ----------------------------------------------------------------
      # RESULTS BOX
      # ----------------------------------------------------------------
      shinydashboard::box(
        width = 8, title = "Results", status = "info",

        tabsetPanel(

          tabPanel(
            "Topic-Year Data", br(),
            tags$p("Source data: yearly mean Gamma per topic."),
            DT::DTOutput(ns("year_topic_table")), br(),
            downloadButton(ns("download_year_topic"), "Download (Excel)")
          ),

          tabPanel(
            "Regression Results", br(),
            DT::DTOutput(ns("regression_table"))
          ),

          tabPanel(
            "Topic Trend Plot", br(),
            shiny::h3("Customize and download graph"),

            shinyWidgets::dropdown(
              tags$h3("Customize Graph"),
              width  = "850px", status = "danger",
              icon   = icon("sliders-h"), style = "unite",

              fluidRow(
                column(4,
                       tags$h4("Labels and Theme"),
                       shiny::textInput(ns("xlab"),       "X-axis Label",  "Year"),
                       shiny::textInput(ns("ylab"),       "Y-axis Label",  "Mean Gamma"),
                       shiny::textInput(ns("plot_title"), "Plot Title",    "Topic Trend"),
                       shiny::selectInput(ns("font_family"), "Font Family",
                                          choices = c("sans", "serif", "mono")),
                       shiny::selectInput(ns("theme_choice"), "Theme",
                                          choices = names(lda_themes())),
                       sliderInput(ns("font_size"), "Font Size", 6, 30, 14)
                ),
                column(4,
                       tags$h4("Line and Point Style"),
                       colourpicker::colourInput(ns("point_color"), "Point Color", "black"),
                       sliderInput(ns("line_width"),  "Line Width",  0.1, 5, 1.5),
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
            tags$p("Line colour: HOT = red, COLD = light-blue, EQUAL = grey."),
            shiny::plotOutput(ns("single_trend_plot"), height = "550px")
          )
        )
      )
    )
  )
}


# ======================================================================
# SERVER
# ======================================================================

#' Topic Trend Analysis Module Server
#'
#' @param id Shiny module ID.
#' @param df_year_topic_reactive Reactive returning a \code{data.frame} with
#'   columns \code{Topic}, \code{year}, and \code{Intensity}.
#' @param lda_model_reactive Reactive returning the final LDA model object
#'   (used for completeness; labels are derived from data columns only).
#' @return \code{NULL} invisibly.
#'
#' @importFrom shiny moduleServer reactiveValues reactive observe observeEvent
#' @importFrom shiny req validate need renderText renderPlot downloadHandler
#' @importFrom shiny renderUI selectInput NS isolate showNotification
#' @importFrom shinyjs disable enable
#' @importFrom DT renderDT datatable formatRound
#' @importFrom dplyr group_by filter mutate select rename pull case_when do
#' @importFrom tidyr unnest
#' @importFrom broom tidy
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth labs theme element_text
#' @importFrom ggplot2 scale_x_continuous ggsave
#' @importFrom openxlsx write.xlsx
#' @importFrom stats lm
#' @export
mod_trend_server <- function(id, df_year_topic_reactive, lda_model_reactive) {
  moduleServer(id, function(input, output, session) {

    shinyjs::disable("download_trend_table")

    rv <- shiny::reactiveValues(
      df_reg  = NULL,
      df_plot = NULL
    )

    # ------------------------------------------------------------------
    # Dynamic topic selector (populated after analysis)
    # ------------------------------------------------------------------
    output$topic_selector_ui <- shiny::renderUI({
      shiny::req(rv$df_reg)
      topic_ids <- unique(rv$df_reg$Topic)
      shiny::selectInput(
        session$ns("selected_topic"),
        label    = tags$b("Select Topic for Plot"),
        choices  = topic_ids,
        selected = topic_ids[1]
      )
    })

    # ------------------------------------------------------------------
    # 1. CORE ANALYSIS
    # ------------------------------------------------------------------
    shiny::observeEvent(input$run_trend_analysis, {

      df_yt <- df_year_topic_reactive()
      shiny::req(df_yt, lda_model_reactive())

      shiny::validate(
        shiny::need(NROW(df_yt) > 0,
                    "No topic-year data. Train the LDA model first."),
        shiny::need(all(c("Topic", "year", "Intensity") %in% colnames(df_yt)),
                    "Missing columns in topic-year data: need 'Topic', 'year', 'Intensity'.")
      )

      alpha_crit <- as.numeric(input$p_value_threshold)

      df_plot_data <- df_yt |>
        dplyr::rename(s = Topic, x = year, y = Intensity) |>
        dplyr::mutate(s = as.factor(s))

      rv$df_plot <- df_plot_data

      # Regression per topic (via do / broom)
      dfreg <- df_plot_data |>
        dplyr::group_by(s) |>
        dplyr::do(fit = broom::tidy(stats::lm(y ~ x, data = .))) |>
        tidyr::unnest(fit)

      slope_rows <- dplyr::filter(dfreg, term == "x")

      df_reg_final <- slope_rows |>
        dplyr::mutate(
          Trend = dplyr::case_when(
            p.value <  alpha_crit & estimate > 0 ~ "HOT (Increasing)",
            p.value <  alpha_crit & estimate < 0 ~ "COLD (Decreasing)",
            p.value >= alpha_crit                 ~ "EQUAL (Stable)",
            TRUE                                  ~ "NA"
          )
        ) |>
        dplyr::select(Topic = s, Trend, Estimate = estimate, P_Value = p.value)

      rv$df_reg <- df_reg_final

      n_topics <- length(unique(df_plot_data$s))
      output$analysis_status <- shiny::renderText(
        sprintf("Analysis complete. %d topics processed (threshold = %.3f).",
                n_topics, alpha_crit)
      )

      shinyjs::enable("download_trend_table")

      shiny::showNotification(
        sprintf("Trend analysis complete: %d topics.", n_topics),
        type = "message", duration = 5
      )
    })

    # ------------------------------------------------------------------
    # 2. OUTPUTS: Tables
    # ------------------------------------------------------------------
    output$year_topic_table <- DT::renderDT({
      shiny::req(df_year_topic_reactive())
      DT::datatable(df_year_topic_reactive(),
                    caption = "Topic-Year Intensity Data",
                    options = list(scrollX = TRUE, pageLength = 10)) |>
        DT::formatRound("Intensity", 6)
    })

    output$regression_table <- DT::renderDT({
      shiny::req(rv$df_reg)
      DT::datatable(rv$df_reg,
                    caption = "Topic Trend Regression Results",
                    options = list(scrollX = TRUE, pageLength = 10)) |>
        DT::formatRound(c("Estimate", "P_Value"), 6)
    })

    # ------------------------------------------------------------------
    # 3. PLOT
    # ------------------------------------------------------------------
    trend_color_map <- c(
      "HOT (Increasing)"  = "#E41A1C",   # Red
      "COLD (Decreasing)" = "#56B4E9",   # Light blue
      "EQUAL (Stable)"    = "#999999",   # Grey
      "NA"                = "#009E73"    # Green (fallback)
    )

    trend_plot_reactive <- shiny::reactive({
      shiny::req(rv$df_reg, rv$df_plot, input$selected_topic)

      topic_id   <- input$selected_topic
      data_topic <- dplyr::filter(rv$df_plot, s == topic_id)

      shiny::validate(
        shiny::need(nrow(data_topic) >= 2L,
                    sprintf("Not enough data points for %s.", topic_id))
      )

      trend_status <- rv$df_reg |>
        dplyr::filter(Topic == topic_id) |>
        dplyr::pull(Trend)

      line_col <- trend_color_map[trend_status]
      if (is.na(line_col)) line_col <- "#009E73"

      # Safe theme lookup — no get()
      themes    <- lda_themes()
      theme_nm  <- input$theme_choice
      if (!theme_nm %in% names(themes)) theme_nm <- "theme_bw"
      base_theme <- themes[[theme_nm]]

      ggplot2::ggplot(data_topic, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(
          size   = as.numeric(input$point_size),
          colour = input$point_color,
          alpha  = as.numeric(input$point_alpha),
          shape  = as.integer(input$point_shape)
        ) +
        ggplot2::geom_smooth(
          method    = "lm",
          se        = FALSE,
          colour    = line_col,
          linewidth = as.numeric(input$line_width),
          formula   = y ~ x
        ) +
        ggplot2::scale_x_continuous(
          breaks = sort(unique(data_topic$x))
        ) +
        ggplot2::labs(
          title = input$plot_title,
          x     = input$xlab,
          y     = input$ylab
        ) +
        base_theme +
        ggplot2::theme(
          text        = ggplot2::element_text(
            family = input$font_family,
            size   = as.numeric(input$font_size)
          ),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )
    })

    output$single_trend_plot <- shiny::renderPlot(
      trend_plot_reactive(), height = 550)

    # ------------------------------------------------------------------
    # 4. DOWNLOADS
    # ------------------------------------------------------------------
    output$download_year_topic <- downloadHandler(
      filename = function() "topic_year_intensity.xlsx",
      content  = function(file) {
        shiny::req(df_year_topic_reactive())
        openxlsx::write.xlsx(df_year_topic_reactive(), file)
      }
    )

    output$download_trend_table <- downloadHandler(
      filename = function() "topic_trend_regression.xlsx",
      content  = function(file) {
        shiny::req(rv$df_reg)
        openxlsx::write.xlsx(rv$df_reg, file)
      }
    )

    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("topic_trend_", input$selected_topic, ".", input$export_format)
      },
      content  = function(file) {
        shiny::req(trend_plot_reactive())
        ggplot2::ggsave(
          filename = file,
          plot     = trend_plot_reactive(),
          device   = input$export_format,
          dpi      = as.numeric(input$export_dpi),
          width    = as.numeric(input$export_width),
          height   = as.numeric(input$export_height)
        )
      }
    )

    invisible(NULL)
  })
}
