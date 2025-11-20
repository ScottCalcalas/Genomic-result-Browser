# app.R — Shiny UI for Xiaopei's dataset index/search/print workflow (with DT highlights)
# Author: Xiaopei Zhang | Rewritten by ChatGPT | Date: 2025-10-21 | V9.1
# - Reliable DT population (no replaceData into zero-column tables)
# - Define rv before observers; safer rendering path
# - Highlighting: yellow rows when stats missing; gray wash for description cols; emphasis for key IDs
# - Optional in-cell bar for log2FC
# - Smart preview for CSV/TSV/TXT/XLSX; Output listing with “Open” links

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(readxl)
  library(dplyr)
  library(purrr)
  library(rlang)
  library(tools)
  library(stringr)
  library(tibble)
})

# External functions used by this app (search/print/index builders)
# Make sure this file exists and defines:
#   xiaopei.input.all()
#   xiaopei.prepare.GeneID()
#   xiaopei.prepare.geneSymbol()
#   xiaopei.print.geneSymbol()
#   xiaopei.print.geneSymbol.family()
#   xiaopei.print.geneSymbol.fuzzy()
#   xiaopei.print.GeneID()
source("XZ_DB_functions.r", local = TRUE)

# Ensure folders exist
if (!dir.exists("IndexedData")) dir.create("IndexedData", showWarnings = FALSE)
if (!dir.exists("Output"))      dir.create("Output",      showWarnings = FALSE)
if (!dir.exists("datasets"))    dir.create("datasets",    showWarnings = FALSE)

timestamp_str <- function() format(Sys.time(), "%Y%m%d-%H%M%S")

# ---------- UI ----------
ui <- page_fluid(
  theme = bs_theme(
    version = 5, bootswatch = "minty",
    base_font = font_google("Inter"),
    heading_font = font_google("Plus Jakarta Sans")
  ),
  
  # ===== CSS for highlighting =====
  tags$head(
    tags$style(HTML("
    /* yellow row highlight when no stats present */
    table.dataTable tbody tr.row-missing > td {
      background-color:#fff3cd !important;
    }

    /* darker gray wash for description columns */
    table.dataTable tbody tr td.col-desc,
    table.dataTable tbody tr.odd td.col-desc,
    table.dataTable tbody tr.even td.col-desc {
      background-color:#e0e0e0 !important;
    }

    /* emphasized ID / filename columns */
    table.dataTable tbody tr td.col-emph {
      background-color:#E8F4FF !important;
      font-weight:600;
    }
  "))
  ),
  
  layout_sidebar(
    sidebar = sidebar(
      h4("Tutorial"),
      #fileInput("index_xlsx", "Datasets infomation.xlsx (optional upload)", accept = c(".xlsx")),
      helpText("-Updated 10/21/25-"),
      helpText("1. Search Tab: will display and automatically save results (all & by gene)."),
      helpText("Empty means that dataset doesn't have that column."),
      helpText("2. Check Tab: see where an item exists in the database."),
      helpText("3. Data info Tab: view dataset metadata."),
      helpText("4. Output files Tab: browse previous results."),
      hr(),
      helpText("Helpful notes:"),
      helpText("Yellow rows: no statistical columns present for that record."),
      helpText("KD = knockdown; OE = overexpression; GOI = gene of interest."),
      helpText("DEG = Differentially expressed genes; DSG = Differentially spliced genes; RNA IP = RNA immunoprecipitation; MS = Mass spec."),
      hr(),
      helpText("⚠️ Clean up the Output folder if searching becomes slow."),
      actionButton("btn_clear_outputs", "Delete ALL Output files", class = "btn-danger"),
      helpText("⚠️ Permanently deletes everything inside /Output."),
      hr(),
      h4("Administrator Operation"),
      helpText("Use to rebuild indices and search tables."),
      actionButton("btn_run_all", "Build Index (all)", class = "btn-success"),
      br(), br(),
      actionButton("btn_prepare_geneid", "Build for GeneID"),
      actionButton("btn_prepare_symbol", "Build for GeneSymbol"),
      helpText("After rebuilding the full index, rebuild the GeneID/GeneSymbol search files."),
      hr(),
      h4("Logs"),
      verbatimTextOutput("log_box")
    ),
    
    card(
      card_header(
        h3("Genomic DB Searching Browser"),
        tooltip = "Interactive UI for building indexes, searching, and exporting rows."
      ),
      navset_card_tab(
        
        # 1) Search / Print
        nav_panel(
          "Search",
          layout_columns(
            col_widths = c(3, 9),
            
            # LEFT controls
            div(class = "pe-stack",
                selectInput(
                  "print_mode", "Mode (Search both Gene/Protein)",
                  choices = c(
                    "Symbol (exact)"  = "geneSymbol",
                    "Symbol family"   = "geneSymbol.family",
                    "Symbol fuzzy"    = "geneSymbol.fuzzy",
                    "ID (Gene/Protein)" = "GeneID"
                  )
                ),
                textAreaInput(
                  "print_terms",
                  "Terms (comma / space / newline separated)",
                  rows = 6,
                  placeholder = "e.g.\nCDK13\nPLEKHA7\n\nor: CDK13, PLEKHA7, CD44"
                ),
                textAreaInput(
                  "print_outname",
                  "Customize your result files' name",
                  rows = 1,
                  placeholder = "Type Here"
                ),
                #textInput("print_outname", "Customize your result files' name", "Type Here"),
                actionButton("btn_print_run",  "Run & Save", class = "btn-primary")
            ),
            
            # RIGHT live preview
            div(
              h6("FULL searching results will be shown below. Columns before 'Date_Modified' are dataset descriptions."),
              h6("The right side search box: You can add dataset or cell type filters in your downstream workflows."),
              DTOutput("tbl_to_print"),
              br(), br(),
              h6("If nothing appears right away after searching, it may be working —-some queries take time.")
            )
          )
        ),
        
        # 2) Check
        nav_panel(
          "Check",
          fluidRow(
            column(
              4,
              selectInput("search_mode", "Mode",
                            choices = c(
                              "Symbol (exact)" = "geneSymbol",
                              "ID (exact)"  = "GeneID"
                            )),
              textInput("search_term", "Search term"),
              div(style="margin-top:28px",
                  actionButton("btn_search","Check", class="btn btn-primary"))
            )
          ),
          fluidRow(
            column(
              12,
              h6(class="text-muted",
                 "Note: Results show where the term appears, in the format: fileName.sheet~row"),
              DTOutput("tbl_search")
            )
          )
        ),
        
        # 3) Data Info (index table)
        nav_panel(
          "Data Info",
          tagList(
            div(
              actionButton("btn_load_index", "Load Data Info", class = "btn-primary"),
              style = "margin-bottom: 8px;"
            ),
            DTOutput("tbl_index")
          )
        ),
        
        # 4) Output files
        nav_panel(
          "Output files",
          tagList(
            div(
              actionButton("btn_preview_selected", "Preview selected", class = "btn-secondary"),
              style = "margin-bottom: 8px;"
            ),
            DTOutput("tbl_outputs")
          )
        )
      )
    )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # ---- state (define FIRST) ----
  rv <- reactiveValues(
    index_path = NULL,
    index_df   = NULL,
    search_df  = NULL,
    to_print   = NULL,
    outputs    = NULL,
    log_msgs   = character()
  )
  
  # ---- placeholders (1-column DF avoids DT zero-column quirks) ----
  empty_df <- data.frame(`(no data yet)` = character(0), check.names = FALSE)
  output$tbl_search    <- DT::renderDT(DT::datatable(empty_df, options = list(dom = 't')))
  output$tbl_to_print  <- DT::renderDT(DT::datatable(empty_df, options = list(dom = 't')))
  output$tbl_index     <- DT::renderDT(DT::datatable(empty_df, options = list(dom = 't')))
  output$tbl_outputs   <- DT::renderDT(DT::datatable(empty_df, options = list(dom = 't')))
  output$modal_preview <- DT::renderDT(DT::datatable(
    data.frame(`(preview waiting...)` = "", check.names = FALSE), options = list(dom = 't'))
  )
  
  # Ensure outputs render even when hidden
  outputOptions(output, "tbl_search", suspendWhenHidden = FALSE)
  outputOptions(output, "tbl_to_print", suspendWhenHidden = FALSE)
  outputOptions(output, "tbl_index",  suspendWhenHidden = FALSE)
  outputOptions(output, "tbl_outputs", suspendWhenHidden = FALSE)
  outputOptions(output, "modal_preview", suspendWhenHidden = FALSE)
  
  # Serve /Output/ as /out/<file> so links open in a new tab
  shiny::addResourcePath("out", normalizePath("Output", mustWork = FALSE))
  
  # ---------- helpers ----------
  log_append <- function(rv, ...) {
    msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = " "))
    rv$log_msgs <- c(rv$log_msgs, msg)
  }
  
  safe_read_xlsx <- function(path) {
    validate(need(file.exists(path), paste0("Missing: ", path)))
    readxl::read_xlsx(path)
  }
  
  # safe render wrapper: tries fancy table, falls back to plain if anything errors
  render_dt_safe <- function(expr, fallback_df = NULL) {
    tryCatch({ expr }, error = function(e) {
      showNotification(paste("DT styling failed, falling back:", conditionMessage(e)), type = "warning")
      if (is.null(fallback_df)) fallback_df <- data.frame()
      DT::datatable(fallback_df, options = list(pageLength = 15, scrollX = TRUE, dom = 't'))
    })
  }
  
  make_hilite_table <- function(
    df,
    stat_like = c("pvalue","p.value","p_val","pval","p-val","PVal","P.Value",
                  "padj","p_adj","adj.p.val","adj.P.Val","p.adjust","qvalue","q.value","FDR",
                  "log2FC","logFC","log_2_fc","LFC"),
    emph_cols  = c("geneSymbol","GeneID","fileName","sheet","row","file","Open"),
    desc_until = c("Date_Modified","Date modified","DateModified","Last_Modified","LastModified"),
    bar_col = "log2FC",
    enable_row_missing = TRUE,
    enable_desc  = TRUE,
    enable_emph  = TRUE,
    enable_bar   = TRUE
  ){
    df <- as.data.frame(df, check.names = FALSE)
    cn <- names(df); n <- length(cn)
    
    ## detect stat columns (case-insensitive)
    stat_mask1 <- tolower(cn) %in% tolower(stat_like)
    stat_mask2 <- grepl("(^|\\b)(p\\.?val(ue)?|adj\\.?p\\.?val|p\\.?adj|q\\.?val(ue)?|fdr|log2?fc|logfc)\\b",
                        cn, ignore.case = TRUE, perl = TRUE)
    stat_mask  <- (stat_mask1 | stat_mask2)
    stat_cols0 <- which(stat_mask) - 1L  # JS 0-based
    
    ## description block strictly before Date_Modified (any variant)
    dm_idx <- which(cn %in% desc_until)
    desc_cols0 <- if (length(dm_idx) && dm_idx[1] > 1L) seq_len(dm_idx[1] - 1L) - 1L else integer(0)
    
    ## emphasis cols
    emph_cols0 <- which(tolower(cn) %in% tolower(emph_cols)) - 1L
    
    ## rowCallback: yellow if ALL stat cells blank (only if enabled)
    row_cb <- if (enable_row_missing && length(stat_cols0)) {
      DT::JS(sprintf("
      function(row, data){
        var idx=[%s];
        var miss=true;
        for (var i=0;i<idx.length;i++){
          var v=data[idx[i]];
          if (!(v === null || v === '' || v === 'NA' || v === 'NaN' || v === 'na' || v === 'N/A')) {
            miss=false; break;
          }
        }
        if (miss){ $(row).addClass('row-missing'); }
      }", paste(stat_cols0, collapse=",")))
    } else NULL
    
    ## attach classes via columnDefs (only if enabled)
    colDefs <- list()
    if (enable_desc  && length(desc_cols0)) colDefs <- append(colDefs, list(list(targets = desc_cols0, className = "col-desc")))
    if (enable_emph  && length(emph_cols0)) colDefs <- append(colDefs, list(list(targets = emph_cols0, className = "col-emph")))
    
    dt <- DT::datatable(
      df,
      options = list(
        pageLength = 15, scrollX = TRUE, autoWidth = TRUE,
        dom = '<"top"fip>t',
        rowCallback = row_cb,
        columnDefs = colDefs
      ),
      class = 'nowrap stripe row-border order-column compact',
      escape = FALSE
    )
    
    ## optional mini bar for log2FC (only if enabled)
    if (enable_bar && bar_col %in% cn && is.numeric(df[[bar_col]])) {
      rng <- range(df[[bar_col]], na.rm = TRUE)
      if (all(is.finite(rng))) {
        dt <- DT::formatStyle(
          dt, which(cn == bar_col)[1],
          background = DT::styleColorBar(rng, "#9ecae1"),
          backgroundSize = "98% 80%", backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
      }
    }
    
    dt
  }
  
  
  
  
  
  # Robust term parser: split on any non-alphanumeric
  parse_terms <- function(x) {
    s <- enc2utf8(x %||% "")
    if (!nzchar(s)) return(character(0))
    s <- gsub("[^A-Za-z0-9]+", "\n", s, perl = TRUE)
    s <- gsub("^(\\n+)|(\\n+)$", "", s, perl = TRUE)
    parts <- unlist(strsplit(s, "\n", fixed = TRUE), use.names = FALSE)
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    if (!length(parts)) return(character(0))
    keep <- !duplicated(toupper(parts))
    parts[keep]
  }
  
  # Output listing with clickable Open links (skip directories)
  list_output_files <- function() {
    paths <- list.files("Output", pattern = ".", full.names = TRUE)
    if (!length(paths)) {
      return(tibble(file=character(), path=character(), size_kb=numeric(),
                    modified=as.POSIXct(character()), Open=character()))
    }
    info <- file.info(paths)
    keep <- !info$isdir %in% TRUE
    paths <- paths[keep]
    info <- info[keep, , drop = FALSE]
    if (!length(paths)) {
      return(tibble(file=character(), path=character(), size_kb=numeric(),
                    modified=as.POSIXct(character()), Open=character()))
    }
    tibble(
      file = basename(paths),
      path = paths,
      size_kb = round(info$size / 1024, 1),
      modified = info$mtime,
      Open = sprintf('<a href="%s" target="_blank">Open</a>',
                     file.path("out", utils::URLencode(basename(paths), reserved = TRUE)))
    )
  }
  
  sanitize_filename <- function(x) {
    x <- enc2utf8(trimws(x %||% ""))
    if (!nzchar(x)) x <- "PrintResults"
    gsub("[<>:\"/\\\\|?*]", "_", x)
  }
  
  # Smart preview for delimited files (handles comment headers & repeated headers)
  smart_preview_delim <- function(path, sep = ",", max_rows = 2000L, quote_char = "\"") {
    lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
                      error = function(e) character(0))
    if (!length(lines)) return(data.frame(check.names = FALSE))
    keep <- nzchar(trimws(lines)) & !grepl("^\\s*#", lines)
    lines <- lines[keep]
    if (!length(lines)) return(data.frame(check.names = FALSE))
    header <- lines[1]
    rest   <- lines[-1]
    rest   <- rest[rest != header]
    if (length(rest) > max_rows) rest <- rest[seq_len(max_rows)]
    lines  <- c(header, rest)
    con <- textConnection(paste(lines, collapse = "\n"))
    on.exit(close(con), add = TRUE)
    utils::read.table(con,
                      header = TRUE, sep = sep, quote = quote_char,
                      comment.char = "", check.names = FALSE,
                      stringsAsFactors = FALSE, fill = TRUE)
  }
  
  # ---------- INDEX LOAD ----------
  observeEvent(input$btn_load_index, {
    path <- if (!is.null(input$index_xlsx) && !is.null(input$index_xlsx$datapath)) {
      input$index_xlsx$datapath
    } else {
      "Datasets infomation.xlsx"
    }
    rv$index_path <- path
    tryCatch({
      df <- safe_read_xlsx(path)
      rv$index_df <- df
      output$tbl_index <- renderDT({
        render_dt_safe(
          make_hilite_table(
            df,
            stat_like = character(0),      # no stat detection
            emph_cols = character(0),      # no blue emphasis
            enable_row_missing = FALSE,    # no yellow rows
            enable_desc = FALSE,           # no gray wash
            enable_emph = FALSE,           # no emphasis styles
            enable_bar = FALSE             # no mini bar
          ),
          fallback_df = df
        )
      })
      
      log_append(rv, "Loaded index file", basename(path))
    }, error = function(e) {
      log_append(rv, "Failed to load index:", conditionMessage(e))
      showNotification(conditionMessage(e), type = "error")
    })
  })
  
  # ---------- ADMIN BUTTONS ----------
  observeEvent(input$btn_run_all, {
    req(rv$index_path %||% file.exists("Datasets infomation.xlsx"))
    tryCatch({
      xiaopei.input.all(rv$index_path %||% "Datasets infomation.xlsx")
      log_append(rv, "Ran input.all")
      showNotification("Index build completed.", type = "message")
    }, error = function(e) {
      log_append(rv, "Error (build all):", conditionMessage(e))
      showNotification(conditionMessage(e), type = "error")
    })
  })
  
  observeEvent(input$btn_prepare_geneid, {
    tryCatch({
      xiaopei.prepare.GeneID()
      log_append(rv, "Prepared searching_GeneID.csv")
      showNotification("Prepared searching_GeneID.csv", type = "message")
    }, error = function(e) {
      log_append(rv, "Error (prepare GeneID):", conditionMessage(e))
      showNotification(conditionMessage(e), type = "error")
    })
  })
  
  observeEvent(input$btn_prepare_symbol, {
    tryCatch({
      xiaopei.prepare.geneSymbol()
      log_append(rv, "Prepared searching_geneSymbol.csv")
      showNotification("Prepared searching_geneSymbol.csv", type = "message")
    }, error = function(e) {
      log_append(rv, "Error (prepare geneSymbol):", conditionMessage(e))
      showNotification(conditionMessage(e), type = "error")
    })
  })
  
  # ---------- CHECK TAB SEARCH ----------
  observeEvent(input$btn_search, {
    term <- trimws(input$search_term %||% "")
    mode <- input$search_mode %||% "geneSymbol"
    if (!nzchar(term)) return(NULL)
    
    tryCatch({
      if (identical(mode, "geneSymbol")) {
        validate(need(file.exists("searching_geneSymbol.csv"),
                      "Please build searching_geneSymbol.csv first."))
        idx <- read.csv("searching_geneSymbol.csv", check.names = FALSE)
        res <- idx[toupper(idx$geneSymbol) == toupper(term), , drop = FALSE]
      } else {
        validate(need(file.exists("searching_GeneID.csv"),
                      "Please build searching_GeneID.csv first."))
        idx <- read.csv("searching_GeneID.csv", check.names = FALSE)
        res <- idx[as.character(idx$GeneID) == as.character(term), , drop = FALSE]
      }
      rv$search_df <- res
      output$tbl_search <- renderDT({
        render_dt_safe(make_hilite_table(res), fallback_df = res)
      })
      log_append(rv, "Search done:", mode, shQuote(term), sprintf("(%d rows)", nrow(res)))
    }, error = function(e) {
      log_append(rv, "Search error:", conditionMessage(e))
      showNotification(conditionMessage(e), type = "error")
    })
  })
  
  # ---------- PRINT / EXPORT ----------
  observeEvent(input$btn_print_run, {
    terms <- parse_terms(input$print_terms)
    log_append(rv, sprintf("Parsed %d term(s): %s", length(terms), paste(terms, collapse = " | ")))
    validate(need(length(terms) > 0, "Enter at least one valid term (commas/newlines OK)."))
    
    mode <- input$print_mode %||% "geneSymbol"
    stem <- input$print_outname %||% "PrintResults"
    idx_path <- rv$index_path %||% "Datasets infomation.xlsx"
    
    pre <- list_output_files()
    
    # Call the appropriate worker; ensure we end up with a data.frame to preview
    res_df <- NULL
    tryCatch({
      res_df <- switch(
        mode,
        "geneSymbol"        = xiaopei.print.geneSymbol(terms, output_name = stem, add_timestamp = TRUE,
                                                       xlsx.index.location = idx_path),
        "geneSymbol.family" = xiaopei.print.geneSymbol.family(terms, output_name = stem, add_timestamp = TRUE,
                                                              xlsx.index.location = idx_path),
        "geneSymbol.fuzzy"  = xiaopei.print.geneSymbol.fuzzy(terms, output_name = stem, add_timestamp = TRUE,
                                                             xlsx.index.location = idx_path),
        "GeneID"            = xiaopei.print.GeneID(terms, output_name = stem, add_timestamp = TRUE,
                                                   xlsx.index.location = idx_path),
        stop("Unknown print mode: ", mode)
      )
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      log_append(rv, "Print worker error:", conditionMessage(e))
    })
    
    # If worker doesn't return a df, try to read the newest file we just wrote
    if (is.null(res_df)) {
      post_try <- list_output_files()
      newest <- post_try[order(post_try$modified, decreasing = TRUE), , drop = FALSE]
      pat <- paste0("^", sanitize_filename(stem), ".*FULL searching results_")
      pick <- newest[grep(pat, newest$file), , drop = FALSE]
      if (nrow(pick)) {
        res_df <- tryCatch(
          utils::read.csv(pick$path[1], check.names = FALSE, stringsAsFactors = FALSE),
          error = function(e) NULL
        )
      }
    }
    
    validate(need(!is.null(res_df) && nrow(res_df) > 0, "No rows to show."))
    
    rv$to_print <- res_df
    output$tbl_to_print <- renderDT({
      render_dt_safe(make_hilite_table(res_df), fallback_df = res_df)
    })
    
    # Save preview table with timestamp (even if workers wrote their own files)
    full_name <- paste0(sanitize_filename(stem), "_FULL searching results_", timestamp_str(), ".csv")
    full_path <- file.path("Output", full_name)
    try(write.csv(res_df, full_path, row.names = FALSE), silent = TRUE)
    log_append(rv, "Saved full preview:", full_name)
    
    post <- list_output_files()
    new_files <- subset(post, !(path %in% (pre$path %||% character(0))))
    if (nrow(new_files)) {
      shiny::showNotification(sprintf("Saved %d file(s): %s",
                                      nrow(new_files), paste(head(new_files$file, 5), collapse = ", ")),
                              type = "message")
    } else {
      shiny::showNotification("No files were written.", type = "warning")
    }
    rv$outputs <- post
    output$tbl_outputs <- renderDT({
      make_hilite_table(rv$outputs, stat_like = character(0), emph_cols = c("file","Open"))
    })
    log_append(rv, sprintf("Print/Export %s for %s → %d rows, files written: %d (terms parsed: %d)",
                           mode, paste(terms, collapse="|"), nrow(res_df), nrow(new_files), length(terms)))
  })
  
  # ---------- OUTPUTS LISTING (auto refresh at startup and when needed) ----------
  observe({
    rv$outputs <- list_output_files()
    output$tbl_outputs <- renderDT({
      make_hilite_table(rv$outputs, stat_like = character(0), emph_cols = c("file","Open"))
    })
  })
  
  # ---------- Preview selected file (modal) ----------
  observeEvent(input$btn_preview_selected, {
    sel <- input$tbl_outputs_rows_selected
    req(!is.null(sel), length(sel) == 1)
    req(nrow(rv$outputs) >= sel)
    
    row <- rv$outputs[sel, , drop = FALSE]
    path <- row$path
    ext  <- tolower(tools::file_ext(path))
    
    df <- tryCatch({
      if (ext %in% c("csv")) {
        # Try standard CSV first (respects quoted commas)
        tryCatch(
          utils::read.csv(path, nrows = 2000, check.names = FALSE, stringsAsFactors = FALSE),
          error = function(e) smart_preview_delim(path, sep = ",", max_rows = 2000L, quote_char = "\"")
        )
      } else if (ext %in% c("tsv")) {
        smart_preview_delim(path, sep = "\t", max_rows = 2000L, quote_char = "\"")
      } else if (ext %in% c("txt")) {
        # Most TXT here are tab-delimited
        smart_preview_delim(path, sep = "\t", max_rows = 2000L, quote_char = "\"")
      } else if (ext %in% c("xlsx")) {
        as.data.frame(readxl::read_excel(path, n_max = 2000), check.names = FALSE)
      } else {
        data.frame(`(preview not supported for this file type)` = basename(path), check.names = FALSE)
      }
    }, error = function(e) {
      data.frame(`(error reading file, please open it on your computer)` = conditionMessage(e),
                 check.names = FALSE)
    })
    
    output$modal_preview <- DT::renderDT({
      make_hilite_table(df, emph_cols = c("geneSymbol","GeneID","file","Open","fileName","sheet","row"))
    })
    
    showModal(modalDialog(
      title = paste("Preview:", basename(path)),
      size = "xl",
      easyClose = TRUE,
      DTOutput("modal_preview")
    ))
  })
  
  # ---------- Delete ALL files in /Output ----------
  clear_output_dir <- function() {
    if (!dir.exists("Output")) return(list(n_files = 0L, n_dirs = 0L))
    paths <- list.files("Output", full.names = TRUE, recursive = TRUE,
                        include.dirs = TRUE, all.files = TRUE, no.. = TRUE)
    if (!length(paths)) return(list(n_files = 0L, n_dirs = 0L))
    info <- file.info(paths)
    n_files <- sum(!info$isdir, na.rm = TRUE)
    n_dirs  <- sum(info$isdir,  na.rm = TRUE)
    unlink(paths, recursive = TRUE, force = TRUE)
    list(n_files = n_files, n_dirs = n_dirs)
  }
  
  observeEvent(input$btn_clear_outputs, {
    showModal(modalDialog(
      title = "Delete ALL files in /Output?",
      "This will permanently delete every file and subfolder inside the Output directory.",
      "Are you sure you want to continue?",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear_outputs", "Yes, delete everything", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_clear_outputs, {
    removeModal()
    res <- tryCatch(clear_output_dir(), error = function(e) e)
    if (inherits(res, "error")) {
      showNotification(paste("Delete failed:", conditionMessage(res)), type = "error")
      log_append(rv, "Delete Output failed:", conditionMessage(res))
      return(invisible(NULL))
    }
    rv$outputs <- list_output_files()
    output$tbl_outputs <- renderDT({
      make_hilite_table(rv$outputs, stat_like = character(0), emph_cols = c("file","Open"))
    })
    showNotification(sprintf("Deleted %d file(s) and %d folder(s) from Output.",
                             res$n_files, res$n_dirs), type = "message")
    log_append(rv, sprintf("Cleared Output: files=%d, dirs=%d", res$n_files, res$n_dirs))
  })
  
  # ---------- Logs ----------
  output$log_box <- renderText({
    paste(rv$log_msgs, collapse = "\n")
  })
}

shinyApp(ui, server)
