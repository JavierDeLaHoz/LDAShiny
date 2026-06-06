# ======================================================================
# MODULE: Import and Merge Bibliometric Data (Scopus + Web of Science)
# ======================================================================

#' Import Bibliometrics UI
#'
#' @description
#' Shiny module UI for importing and validating bibliometric datasets
#' from Scopus and Web of Science in LDAShiny.
#'
#' @param id Shiny module ID.
#' @return A Shiny UI element (\code{tagList}).
#'
#' @importFrom shiny NS tagList h2 br tabsetPanel tabPanel fluidRow
#' @importFrom shiny conditionalPanel fileInput actionButton hr uiOutput
#' @importFrom shiny column p em strong icon selectInput sliderInput
#' @importFrom shiny numericInput downloadButton textInput
#' @importFrom shiny plotOutput tableOutput
#' @importFrom shinydashboard box
#' @importFrom shinybusy add_busy_spinner
#' @importFrom shinyWidgets pickerInput dropdown
#' @importFrom DT dataTableOutput
#' @importFrom colourpicker colourInput
#' @export
mod_import_bibliometrics_ui <- function(id) {
  ns <- NS(id)

  tagList(

    shinybusy::add_busy_spinner(
      spin     = "fading-circle",
      position = "top-right",
      margins  = c(15, 15)
    ),

    shiny::h2("Data Import"),

    shinyWidgets::pickerInput(
      inputId = ns("action"),
      label   = "Select an action:",
      choices = c(
        "-- Select an action --"     = "",
        "Merge Scopus + Web of Science" = "merge",
        "Load Integrated Excel File"    = "load_excel"
      )
    ),

    br(),

    tabsetPanel(

      # ================================================================
      # TAB 1: IMPORT DATA
      # ================================================================
      tabPanel(
        "Import Data",
        br(),
        fluidRow(

          shinydashboard::box(
            width       = 6,
            title       = "Upload and Process Files",
            status      = "primary",
            solidHeader = TRUE,

            shiny::conditionalPanel(
              condition = sprintf("input['%s']=='merge'", ns("action")),
              fileInput(ns("scopus_file"), "Upload Scopus CSV file(s)",
                        accept = ".csv", multiple = TRUE),
              fileInput(ns("wos_file"),   "Upload Web of Science TXT file(s)",
                        accept = ".txt", multiple = TRUE),
              p(em("Note: You can select multiple files for each source.")),
              actionButton(ns("process_merge"), "Process and Merge",
                           icon = icon("link"), class = "btn-danger"),
              hr()
            ),

            shiny::conditionalPanel(
              condition = sprintf("input['%s']=='load_excel'", ns("action")),
              fileInput(ns("excel_file"), "Upload Excel File (.xlsx)",
                        accept = ".xlsx"),
              hr()
            )
          ),

          shinydashboard::box(
            width       = 6,
            title       = "Instructions and Status",
            status      = "info",
            solidHeader = TRUE,

            p(strong(icon("info-circle"), "Instructions:")),
            tags$ul(
              tags$li("Choose Merge or Load Excel."),
              tags$li("You can upload multiple CSVs for Scopus and multiple TXTs for WoS."),
              tags$li("Duplicates are removed based on DOI (case-insensitive)."),
              tags$li("Use the tabs for summary, preview, missing values, and plots.")
            ),
            hr(),
            p(strong(icon("check-square"), "Current Status:")),
            shiny::uiOutput(ns("status_output"))
          )
        )
      ),

      # ================================================================
      # TAB 2: SUMMARY STATISTICS
      # ================================================================
      tabPanel(
        "Summary Statistics",
        br(),
        shiny::h4("Bibliometric Summary"),
        shiny::tableOutput(ns("summary_table")),
        hr(),
        shiny::h3("Counts and Visualizations"),

        tabsetPanel(

          tabPanel(
            "Count Tables",
            fluidRow(
              column(6,
                     shiny::h4(icon("newspaper"), "Documents per Journal"),
                     DT::dataTableOutput(ns("journal_table_dt"))
              ),
              column(6,
                     shiny::h4(icon("calendar-alt"), "Documents by Year"),
                     DT::dataTableOutput(ns("year_table_dt"))
              )
            )
          ),

          tabPanel(
            "Customizable Plots",
            br(),
            fluidRow(
              column(4,
                     selectInput(ns("bibliometric_plot_type"), "Plot Type:",
                                 choices = c("Journal Count", "Year Count")),
                     shiny::conditionalPanel(
                       condition = sprintf("input['%s']=='Journal Count'",
                                           ns("bibliometric_plot_type")),
                       sliderInput(ns("num_journals_to_show"), "N Journals",
                                   5, 50, 15),
                       selectInput(ns("journal_geom_type"), "Plot Style:",
                                   choices = c("Bar", "Lollipop"))
                     ),
                     actionButton(ns("update_plot"), "Update Plot",
                                  icon = icon("sync"), class = "btn-success")
              ),
              column(8,
                     shinyWidgets::dropdown(
                       tags$h3("Customize Plot"),
                       width  = "850px",
                       status = "danger",
                       icon   = icon("sliders-h"),
                       style  = "unite",
                       fluidRow(
                         column(4,
                                shiny::h4("Labels & Theme"),
                                textInput(ns("xlab"), "X-axis Label", "Publication Year"),
                                textInput(ns("ylab"), "Y-axis Label", "Number of Documents"),
                                selectInput(ns("font_family"), "Font Family",
                                            choices = c("sans", "serif", "mono")),
                                selectInput(ns("theme_choice"), "Theme",
                                            choices = names(lda_themes())),
                                sliderInput(ns("font_size"), "Font Size", 6, 30, 12)
                         ),
                         column(4,
                                shiny::h4("Visual Style"),
                                colourpicker::colourInput(ns("main_color"),  "Main Color",  "blue"),
                                sliderInput(ns("line_width"),  "Line Width",  0.1, 5, 1.5),
                                colourpicker::colourInput(ns("point_color"), "Point Color", "black"),
                                sliderInput(ns("point_size"),  "Point Size",  1, 10, 3),
                                sliderInput(ns("point_alpha"), "Alpha",       0.05, 1, 0.7),
                                sliderInput(ns("point_shape"), "Point Shape", 0, 25, 16)
                         ),
                         column(4,
                                shiny::h4("Export"),
                                selectInput(ns("export_format"), "Format",
                                            choices = c("png", "tiff", "jpeg", "pdf")),
                                numericInput(ns("export_width"),  "Width (inches)",  7),
                                numericInput(ns("export_height"), "Height (inches)", 5),
                                numericInput(ns("export_dpi"),    "DPI",             300),
                                br(),
                                downloadButton(ns("download_bibliometric_plot"),
                                               "Download Plot", class = "btn-success")
                         )
                       )
                     )
              )
            ),
            shiny::plotOutput(ns("bibliometric_custom_plot"), height = "500px")
          )
        )
      ),

      # ================================================================
      # TAB 3: MISSING VALUES
      # ================================================================
      tabPanel(
        "Missing Values",
        br(),
        shiny::h4("Missing Values by Column"),
        shiny::tableOutput(ns("missing_table"))
      ),

      # ================================================================
      # TAB 4: PREVIEW
      # ================================================================
      tabPanel(
        "Preview",
        br(),
        shiny::h4("Top 10 Records"),
        shiny::tableOutput(ns("preview"))
      ),

      # ================================================================
      # TAB 5: DOWNLOAD
      # ================================================================
      tabPanel(
        "Download",
        br(),
        shiny::conditionalPanel(
          condition = sprintf("input['%s']!=''", ns("action")),
          p(strong(icon("download"), "Download integrated dataset")),
          hr(),
          downloadButton(ns("download"), "Download Excel", class = "btn-primary")
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s']==''", ns("action")),
          p(em("Please select an action first."))
        )
      )
    )
  )
}


# ======================================================================
# HELPER: Parse Web of Science .txt
# ======================================================================

#' Parse a Web of Science plain-text export file
#'
#' @param path Character. Path to the .txt file.
#' @return A data.frame with columns: doi, title, year, Journal, abstract, database.
#' @keywords internal
parse_wos <- function(path) {

  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)

  valid_tags <- c("TI", "PY", "SO", "AB", "DI")
  map <- c(TI = "title", PY = "year", SO = "Journal", AB = "abstract", DI = "doi")

  records    <- list()
  current    <- list()
  current_tag <- NULL

  add_field <- function(tag, value) {
    key <- map[[tag]]
    if (is.null(key)) return()
    if (is.null(current[[key]])) {
      current[[key]] <<- value
    } else {
      current[[key]] <<- paste(current[[key]], value, sep = " ")
    }
  }

  for (ln in lines) {
    if (grepl("^ER", ln)) {
      if (length(current) > 0) records[[length(records) + 1]] <- current
      current     <- list()
      current_tag <- NULL
      next
    }
    if (grepl("^[A-Z]{2} ", ln)) {
      tag   <- substr(ln, 1, 2)
      value <- trimws(substr(ln, 4, nchar(ln)))
      if (!(tag %in% valid_tags)) {
        current_tag <- NULL
        next
      }
      current_tag <- tag
      add_field(tag, value)
    } else {
      if (!is.null(current_tag) && nzchar(trimws(ln))) {
        add_field(current_tag, trimws(ln))
      }
    }
  }

  required_cols <- c("doi", "title", "year", "Journal", "abstract")

  if (length(records) == 0) {
    empty <- as.data.frame(
      matrix(NA_character_, nrow = 0, ncol = length(required_cols)),
      stringsAsFactors = FALSE
    )
    names(empty) <- required_cols
    empty$year     <- integer(0)
    empty$database <- character(0)
    return(empty)
  }

  all_cols <- unique(c(required_cols, unlist(lapply(records, names))))
  df <- as.data.frame(
    matrix(NA_character_, nrow = length(records), ncol = length(all_cols)),
    stringsAsFactors = FALSE
  )
  names(df) <- all_cols

  for (i in seq_along(records)) {
    for (nm in names(records[[i]])) {
      df[i, nm] <- records[[i]][[nm]]
    }
  }

  suppressWarnings(df$year <- as.integer(df$year))
  df$database <- "Web of Science"
  df
}


# ======================================================================
# HELPER: Standardise Scopus CSV
# ======================================================================

#' Standardise a raw Scopus CSV data.frame
#'
#' @param df A data.frame read from a Scopus CSV export.
#' @return A standardised data.frame with columns: doi, title, year, Journal,
#'   abstract, database.
#' @keywords internal
standardize_scopus <- function(df) {

  names(df) <- tolower(gsub(" ", "_", names(df)))
  n <- nrow(df)

  getcol <- function(opts) {
    for (o in opts) {
      if (o %in% names(df)) return(df[[o]])
    }
    rep(NA_character_, n)
  }

  data.frame(
    doi      = as.character(getcol(c("doi"))),
    title    = as.character(getcol(c("title", "document_title"))),
    year     = suppressWarnings(as.integer(getcol(c("year", "pubyear")))),
    Journal  = as.character(getcol(c("source_title", "journal", "source"))),
    abstract = as.character(getcol(c("abstract", "description"))),
    database = "Scopus",
    stringsAsFactors = FALSE
  )
}


# ======================================================================
# SERVER
# ======================================================================

#' Import Bibliometrics Server
#'
#' @description
#' Server logic for importing, validating and combining Scopus/WoS datasets.
#'
#' @param id Shiny module ID.
#' @return A list with one element:
#' \describe{
#'   \item{data}{A reactive expression returning the cleaned combined
#'     \code{data.frame}.}
#' }
#'
#' @importFrom shiny moduleServer reactiveValues renderUI req observe
#' @importFrom shiny observeEvent showNotification tags icon em strong
#' @importFrom shiny renderTable downloadHandler renderPlot eventReactive
#' @importFrom shiny updateTextInput reactive
#' @importFrom shinybusy add_busy_spinner
#' @importFrom DT renderDT datatable
#' @importFrom dplyr filter mutate arrange count distinct bind_rows
#' @importFrom dplyr inner_join select rename pull desc group_by row_number
#' @importFrom ggplot2 ggplot aes geom_col geom_segment geom_point geom_line
#' @importFrom ggplot2 coord_flip labs scale_x_continuous theme element_text
#' @importFrom ggplot2 ggsave
#' @importFrom openxlsx write.xlsx
#' @export
mod_import_bibliometrics_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # ------------------------------------------------------------------
    # Reactive state
    # ------------------------------------------------------------------
    rv <- shiny::reactiveValues(
      data            = NULL,
      summary_stats   = NULL,
      display_summary = NULL,
      missing_stats   = NULL,
      status_message  = tags$p(em("Data is waiting for upload."))
    )

    output$status_output <- shiny::renderUI(rv$status_message)

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------
    is_missing_abstract <- function(x) {
      x <- trimws(tolower(as.character(x)))
      is.na(x) | x == "" |
        x %in% c(
          "no abstract available",
          "[no abstract available]",
          "[no abstract available.]"
        )
    }

    create_bibliometric_summary_df <- function(df, num_top = 5) {
      shiny::req(df)

      total_row <- data.frame(Metric = "Total Records Loaded", Count = nrow(df))

      year_df <- tryCatch({
        df |> dplyr::filter(!is.na(year)) |>
          dplyr::mutate(year = as.integer(year))
      }, error = function(e) data.frame(year = integer(0)))

      if (nrow(year_df) > 0) {
        year_rows <- data.frame(
          Metric = c("Earliest Publication Year", "Latest Publication Year"),
          Count  = c(min(year_df$year), max(year_df$year))
        )
      } else {
        year_rows <- data.frame(
          Metric = c("Earliest Publication Year", "Latest Publication Year"),
          Count  = c(NA_integer_, NA_integer_)
        )
      }

      journal_counts <- tryCatch(
        df |> dplyr::count(Journal, name = "Count") |>
          dplyr::arrange(desc(Count)) |> utils::head(num_top),
        error = function(e)
          data.frame(Journal = character(0), Count = integer(0))
      )

      if (nrow(journal_counts) > 0) {
        journal_rows <- data.frame(
          Metric = paste0("Top Journal ", seq_len(nrow(journal_counts)),
                          " (", journal_counts$Journal, ")"),
          Count  = journal_counts$Count
        )
      } else {
        journal_rows <- data.frame(Metric = character(0), Count = integer(0))
      }

      dplyr::bind_rows(
        total_row, year_rows,
        data.frame(Metric = "--- Top Journals (Count) ---", Count = NA_integer_),
        journal_rows
      )
    }

    # ------------------------------------------------------------------
    # MERGE: Scopus + WoS
    # ------------------------------------------------------------------
    shiny::observeEvent(input$process_merge, {

      shiny::req(input$scopus_file, input$wos_file)

      rv$status_message <- tags$p(
        icon("spinner", class = "fa-spin"),
        strong("Processing files...")
      )

      tryCatch({

        scopus_list <- lapply(input$scopus_file$datapath, function(fp) {
          df_raw <- utils::read.csv(fp, stringsAsFactors = FALSE,
                                    sep = ",", encoding = "UTF-8")
          standardize_scopus(df_raw)
        })
        scopus_std <- dplyr::bind_rows(scopus_list)

        wos_list <- lapply(input$wos_file$datapath, parse_wos)
        wos_raw  <- dplyr::bind_rows(wos_list)

        n_scopus <- nrow(scopus_std)
        n_wos    <- nrow(wos_raw)

        merged <- dplyr::bind_rows(scopus_std, wos_raw) |>
          dplyr::filter(!is.na(title) & nchar(trimws(title)) > 0)

        merged$doi <- tolower(trimws(merged$doi))

        with_doi    <- merged[!is.na(merged$doi) & nzchar(merged$doi), ]
        without_doi <- merged[ is.na(merged$doi) | !nzchar(merged$doi), ]

        dedup_doi     <- dplyr::distinct(with_doi,    doi, .keep_all = TRUE)
        dedup_no_doi  <- dplyr::distinct(without_doi, .keep_all = TRUE)

        final <- dplyr::bind_rows(dedup_doi, dedup_no_doi)

        n_no_abstract <- tryCatch(
          sum(is_missing_abstract(final$abstract)), error = function(e) NA_integer_)

        rv$missing_stats <- data.frame(
          column  = names(final),
          missing = colSums(is.na(final) | final == ""),
          row.names = NULL
        )

        rv$data <- final
        rv$summary_stats <- data.frame(
          Metric = c("Records in Scopus", "Records in Web of Science",
                     "Duplicates Removed", "Records without DOI",
                     "Records without Abstract", "Final Merged Records"),
          Count  = c(n_scopus, n_wos,
                     (n_scopus + n_wos) - nrow(final),
                     nrow(dedup_no_doi),
                     n_no_abstract, nrow(final))
        )
        rv$display_summary <- rv$summary_stats

        rv$status_message <- tags$p(
          icon("check-circle", class = "text-success"),
          strong("Merge Complete!"),
          paste0(" (Final: ", nrow(final), " records)")
        )

        shiny::showNotification(
          paste("Merge finished. Final records:", nrow(final)),
          type = "message", duration = 5
        )

      }, error = function(e) {
        rv$status_message <- tags$p(
          icon("times-circle", class = "text-danger"),
          strong("Merge Failed! "),
          e$message
        )
        shiny::showNotification(paste("Merge failed:", e$message),
                                type = "error", duration = 8)
      })
    })

    # ------------------------------------------------------------------
    # EXCEL LOAD
    # ------------------------------------------------------------------
    shiny::observeEvent(input$excel_file, {
      shiny::req(input$excel_file)

      rv$status_message <- tags$p(
        icon("spinner", class = "fa-spin"), strong("Loading Excel..."))

      tryCatch({
        df        <- openxlsx::read.xlsx(input$excel_file$datapath)
        names(df) <- tolower(names(df))
        rv$data   <- df

        rv$missing_stats <- data.frame(
          column  = names(df),
          missing = colSums(is.na(df) | df == ""),
          row.names = NULL
        )
        rv$display_summary <- create_bibliometric_summary_df(df)

        rv$status_message <- tags$p(
          icon("check-circle", class = "text-success"),
          strong("File Loaded!"),
          paste0(" (Records: ", nrow(df), ")")
        )
        shiny::showNotification(
          paste("Excel loaded. Records:", nrow(df)),
          type = "message", duration = 5
        )
      }, error = function(e) {
        rv$status_message <- tags$p(
          icon("times-circle", class = "text-danger"),
          strong("Load Failed! "), e$message
        )
        shiny::showNotification(paste("Excel load failed:", e$message),
                                type = "error", duration = 8)
      })
    })

    # ------------------------------------------------------------------
    # OUTPUTS: Summary / Missing / Preview
    # ------------------------------------------------------------------
    output$summary_table <- shiny::renderTable(rv$display_summary)
    output$missing_table <- shiny::renderTable(rv$missing_stats)
    output$preview       <- shiny::renderTable(utils::head(rv$data, 10))

    output$download <- downloadHandler(
      filename = function() "merged_bibliometrics.xlsx",
      content  = function(file) openxlsx::write.xlsx(rv$data, file)
    )

    # ------------------------------------------------------------------
    # COUNT TABLES
    # ------------------------------------------------------------------
    data_filtered <- shiny::reactive({
      df <- rv$data
      shiny::req(df)
      required <- c("Journal", "year")
      if (!all(required %in% names(df))) return(data.frame())
      df |>
        dplyr::filter(!is.na(Journal), !is.na(year),
                      nzchar(Journal), nzchar(as.character(year))) |>
        dplyr::mutate(year = as.integer(year))
    })

    journal_count_table <- shiny::reactive({
      shiny::req(data_filtered())
      data_filtered() |>
        dplyr::count(Journal, name = "Documents") |>
        dplyr::arrange(desc(Documents))
    })

    output$journal_table_dt <- DT::renderDT({
      shiny::req(journal_count_table())
      DT::datatable(utils::head(journal_count_table(), 15),
                    options = list(pageLength = 10, dom = "tip", searching = FALSE),
                    rownames = FALSE)
    })

    year_count_table <- shiny::reactive({
      shiny::req(data_filtered())
      data_filtered() |>
        dplyr::count(year, name = "Documents") |>
        dplyr::arrange(year) |>
        dplyr::rename(Year = year)
    })

    output$year_table_dt <- DT::renderDT({
      shiny::req(year_count_table())
      DT::datatable(year_count_table(),
                    options = list(pageLength = 10, dom = "tip", searching = FALSE),
                    rownames = FALSE)
    })

    # ------------------------------------------------------------------
    # CUSTOM PLOT
    # ------------------------------------------------------------------
    bibliometric_plot_reactive <- shiny::eventReactive(input$update_plot, {

      shiny::req(data_filtered())
      themes <- lda_themes()

      base_theme <- themes[[input$theme_choice]] +
        ggplot2::theme(
          text       = ggplot2::element_text(family = input$font_family,
                                             size   = as.numeric(input$font_size)),
          plot.title = ggplot2::element_text(hjust = 0.5)
        )

      if (input$bibliometric_plot_type == "Journal Count") {

        shiny::updateTextInput(session, "xlab", value = "Number of Documents")
        shiny::updateTextInput(session, "ylab", value = "")

        plot_data <- utils::head(journal_count_table(), input$num_journals_to_show)
        if (nrow(plot_data) == 0)
          return(ggplot2::ggplot() + base_theme +
                   ggplot2::labs(title = "No data available"))

        plot_data$Journal <- factor(plot_data$Journal,
                                    levels = rev(plot_data$Journal))

        if (input$journal_geom_type == "Bar") {
          p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Journal, y = Documents)) +
            ggplot2::geom_col(fill = input$main_color) +
            ggplot2::coord_flip()
        } else {
          p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Journal, y = Documents)) +
            ggplot2::geom_segment(
              ggplot2::aes(x = Journal, xend = Journal, y = 0, yend = Documents),
              linewidth = as.numeric(input$line_width), color = input$main_color
            ) +
            ggplot2::geom_point(
              color = input$point_color, size  = as.numeric(input$point_size),
              alpha = as.numeric(input$point_alpha),
              shape = as.integer(input$point_shape)
            ) +
            ggplot2::coord_flip()
        }

        p <- p + ggplot2::labs(
          title = paste("Top", input$num_journals_to_show, "Journals"),
          x = input$ylab, y = input$xlab
        )

      } else {

        shiny::updateTextInput(session, "xlab", value = "Publication Year")
        shiny::updateTextInput(session, "ylab", value = "Number of Documents")

        plot_data <- year_count_table()
        if (nrow(plot_data) == 0)
          return(ggplot2::ggplot() + base_theme +
                   ggplot2::labs(title = "No data available"))

        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Year, y = Documents)) +
          ggplot2::geom_line(color = input$main_color,
                             linewidth = as.numeric(input$line_width)) +
          ggplot2::geom_point(
            color = input$point_color, size  = as.numeric(input$point_size),
            alpha = as.numeric(input$point_alpha),
            shape = as.integer(input$point_shape)
          ) +
          ggplot2::scale_x_continuous(breaks = unique(plot_data$Year)) +
          ggplot2::labs(title = "Document Count Over Time",
                        x = input$xlab, y = input$ylab) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      }

      p + base_theme
    })

    output$bibliometric_custom_plot <- shiny::renderPlot({
      bibliometric_plot_reactive()
    })

    output$download_bibliometric_plot <- downloadHandler(
      filename = function() paste0("bibliometric_plot.", input$export_format),
      content  = function(file) {
        ggplot2::ggsave(
          filename = file,
          plot     = bibliometric_plot_reactive(),
          dpi      = as.numeric(input$export_dpi),
          width    = as.numeric(input$export_width),
          height   = as.numeric(input$export_height),
          device   = input$export_format
        )
      }
    )

    # ------------------------------------------------------------------
    # RETURN
    # ------------------------------------------------------------------
    list(data = shiny::reactive(rv$data))
  })
}
