# app.R — Shiny UI for Xiaopei's dataset index/search/print workflow (with DT highlights + UniProt/MINT compare)
# Author: Xiaopei Zhang


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
  library(httr)  # for UniProt/MINT REST calls
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
                    "Symbol (exact)"    = "geneSymbol",
                    "Symbol family"     = "geneSymbol.family",
                    "Symbol fuzzy"      = "geneSymbol.fuzzy",
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
                actionButton("btn_print_run",  "Run & Save", class = "btn-primary")
            ),
            
            # RIGHT live preview
            div(
              h6("FULL searching results will be shown below. Grey shadow part are dataset descriptions."),
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
                            "ID (exact)"     = "GeneID"
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
        ),
        
        # 5) External DB compare
        nav_panel(
          "Compare (UniProt / MINT)",
          fluidRow(
            column(
              4,
              h6("1. Choose a protein (or use gene to find a protein) "),
              radioButtons(
                "cmp_source", "2. Source:",
                choices = c(
                  "Type UniProt ID / gene symbol"    = "manual",
                  "Pick from current Search results" = "from_results"
                ),
                selected = "manual"
              ),
              uiOutput("cmp_id_input"),
              br(),
              actionButton("btn_cmp_fetch", "Search UniProt & MINT", class = "btn-primary"),
              helpText("Tip: UniProt is for search protein ID entry. Mint use uniprotID to search protein interactions. Requires internet access.")
            ),
            column(
              8,
              h5("UniProtKB protein entry"),
              DTOutput("tbl_cmp_uniprot"),
              br(),
              h5("MINT protein interactions (MITAB)"),
              DTOutput("tbl_cmp_mint")
            )
          ),
          hr(),
          fluidRow(
            column(
              12,
              h5("Quick overlap summary (MINT vs current Search results)"),
              DTOutput("tbl_cmp_summary")
            )
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
    index_path  = NULL,
    index_df    = NULL,
    search_df   = NULL,
    to_print    = NULL,
    outputs     = NULL,
    log_msgs    = character(),
    cmp_uniprot = NULL,
    cmp_mint    = NULL,
    cmp_summary = NULL
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
  output$tbl_cmp_uniprot <- DT::renderDT(DT::datatable(empty_df, options = list(dom = 't')))
  output$tbl_cmp_mint    <- DT::renderDT(DT::datatable(empty_df, options = list(dom = 't')))
  output$tbl_cmp_summary <- DT::renderDT(DT::datatable(empty_df, options = list(dom = 't')))
  
  # Ensure outputs render even when hidden
  outputOptions(output, "tbl_search",       suspendWhenHidden = FALSE)
  outputOptions(output, "tbl_to_print",     suspendWhenHidden = FALSE)
  outputOptions(output, "tbl_index",        suspendWhenHidden = FALSE)
  outputOptions(output, "tbl_outputs",      suspendWhenHidden = FALSE)
  outputOptions(output, "modal_preview",    suspendWhenHidden = FALSE)
  outputOptions(output, "tbl_cmp_uniprot",  suspendWhenHidden = FALSE)
  outputOptions(output, "tbl_cmp_mint",     suspendWhenHidden = FALSE)
  outputOptions(output, "tbl_cmp_summary",  suspendWhenHidden = FALSE)
  
  # Serve /Output/ as /out/<file> so links open in a new tab
  shiny::addResourcePath("out", normalizePath("Output", mustWork = FALSE))
  
  # ---------- helpers ----------
  log_append <- function(rv, ...) {
    msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = " "))
    rv$log_msgs <- c(rv$log_msgs, msg)
  }
  
  # no validate() here, just stop() so tryCatch will showNotification()
  safe_read_xlsx <- function(path) {
    if (!file.exists(path)) {
      stop("Missing: ", path)
    }
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
                  "log2FC","logFC","log_2_fc","LFC","Probability","Prob"),
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
  
  # Output listing with clickable Open links
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
      Download = sprintf('<a href="%s" target="_blank">Download</a>',
                         file.path("out", utils::URLencode(basename(paths), reserved = TRUE))),
      file = basename(paths),
      size_kb = round(info$size / 1024, 1),
      modified = info$mtime,
      path = paths
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
  
  # ---------- UniProt & MINT helpers ----------
  fetch_uniprot_basic <- function(id) {
    id <- trimws(id)
    if (!nzchar(id)) return(NULL)
    
    base <- "https://rest.uniprot.org/uniprotkb/search"
    url <- httr::modify_url(
      base,
      query = list(
        query  = id,  # can be accession (P00533) or gene symbol (CDK13)
        format = "tsv",
        fields = paste(
          c("accession", "id", "protein_name", "gene_names", "organism_name", "length"),
          collapse = ","
        )
      )
    )
    res <- httr::GET(url)
    if (httr::http_error(res)) {
      stop("UniProt request failed: HTTP ", httr::status_code(res))
    }
    txt <- httr::content(res, as = "text", encoding = "UTF-8")
    if (!nzchar(trimws(txt))) return(NULL)
    con <- textConnection(txt)
    on.exit(close(con), add = TRUE)
    df <- utils::read.delim(con, header = TRUE, sep = "\t",
                            stringsAsFactors = FALSE, check.names = FALSE)
    if (!nrow(df)) return(NULL)
    df
  }
  
  fetch_mint_interactions <- function(uniprot_ac) {
    ac <- trimws(uniprot_ac)
    if (!nzchar(ac)) return(NULL)
    
    # MINT PSICQUIC is hosted at EBI, not mint.bio.uniroma2.it
    # Base from PSICQUIC / MINT registry
    base <- "https://www.ebi.ac.uk/Tools/webservices/psicquic/mint/webservices/current/search"
    
    # PSICQUIC spec: .../interactor/<ID>?format=tab25
    url  <- sprintf("%s/interactor/%s?format=tab25",
                    base, utils::URLencode(ac, reserved = TRUE))
    
    res <- httr::GET(url)
    if (httr::http_error(res)) {
      stop("MINT request failed: HTTP ", httr::status_code(res))
    }
    
    txt <- httr::content(res, as = "text", encoding = "UTF-8")
    if (!nzchar(trimws(txt))) return(NULL)
    
    con <- textConnection(txt)
    on.exit(close(con), add = TRUE)
    df <- utils::read.delim(con, header = FALSE, sep = "\t",
                            stringsAsFactors = FALSE, check.names = FALSE)
    if (!nrow(df)) return(NULL)
    
    # MITAB 2.5 has up to 15 cols; name first ones nicely
    base_names <- c(
      "A_id","B_id","A_alt","B_alt","A_aliases","B_aliases",
      "Detection_method","First_author","Pubmed","Taxid_A","Taxid_B",
      "Interaction_type","Source_db","Interaction_id","Confidence"
    )
    colnames(df) <- head(base_names, ncol(df))
    df
  }
  
  
  
  # ---------- small helpers for UniProt / MINT / PubMed links ----------
  make_uniprot_link <- function(id) {
    id <- as.character(id)
    ifelse(
      nzchar(id),
      sprintf(
        '<a href="https://www.uniprot.org/uniprotkb/%s" target="_blank">%s</a>',
        id, id
      ),
      NA_character_
    )
  }
  
  # Partner -> IntAct/MINT search (used on Partner column)
  make_mint_search_link <- function(id) {
    id <- as.character(id)
    ifelse(
      nzchar(id),
      sprintf(
        '<a href="https://www.ebi.ac.uk/intact/search?query=%s" target="_blank">%s</a>',
        id, id
      ),
      NA_character_
    )
  }
  
  # Query_AC -> mint.bio “results-interactions” page
  make_mint_result_link <- function(id) {
    id <- as.character(id)
    ifelse(
      nzchar(id),
      sprintf(
        '<a href="https://mint.bio.uniroma2.it/index.php/results-interactions/?id=%s" target="_blank">%s</a>',
        id, id
      ),
      NA_character_
    )
  }
  
  # PubMed ID -> PubMed page
  make_pubmed_link <- function(pm) {
    pm <- as.character(pm)
    ifelse(
      nzchar(pm),
      sprintf(
        '<a href="https://pubmed.ncbi.nlm.nih.gov/%s" target="_blank">%s</a>',
        pm, pm
      ),
      NA_character_
    )
  }
  
  # Fetch UniProt short display name (protein or gene name)
  fetch_uniprot_display_name <- function(ac) {
    ac <- trimws(ac)
    if (!nzchar(ac)) return(NA_character_)
    
    df <- tryCatch(fetch_uniprot_basic(ac), error = function(e) NULL)
    if (is.null(df) || !nrow(df)) return(NA_character_)
    
    # Prefer gene name, fallback to protein name
    if ("gene_names" %in% names(df) && nzchar(df$gene_names[1])) {
      return(df$gene_names[1])
    }
    if ("Gene Names" %in% names(df) && nzchar(df$`Gene Names`[1])) {
      return(df$`Gene Names`[1])
    }
    if ("protein_name" %in% names(df) && nzchar(df$protein_name[1])) {
      return(df$protein_name[1])
    }
    if ("Protein names" %in% names(df) && nzchar(df$`Protein names`[1])) {
      return(df$`Protein names`[1])
    }
    
    NA_character_
  }
  
  
  
  
  
  
  
  
  # ---- Clean MINT MITAB into a readable partner table ----
  clean_mint_mitab <- function(mint_df, ac, uni_df = NULL) {
    if (is.null(mint_df) || !nrow(mint_df)) 
      return(data.frame())
    
    # Extract best UniProt accession from MITAB ID field
    extract_uniprot <- function(x) {
      x <- as.character(x)
      toks <- unlist(strsplit(x, "\\|"))
      hits <- toks[grepl("^uniprotkb:", toks, ignore.case = TRUE)]
      if (!length(hits)) return(NA_character_)
      sub("^uniprotkb:", "", hits[1], ignore.case = TRUE)
    }
    
    # Extract PubMed id
    extract_pubmed <- function(x) {
      x <- as.character(x)
      toks <- unlist(strsplit(x, "\\|"))
      hits <- toks[grepl("pubmed:", toks, ignore.case = TRUE)]
      if (!length(hits)) return(NA_character_)
      sub(".*pubmed:", "", hits[1], ignore.case = TRUE)
    }
    
    # Extract detection method short string
    extract_method <- function(x) {
      x <- as.character(x)
      toks <- unlist(strsplit(x, "\\|"))
      hits <- toks[grepl("^psi-mi:", toks, ignore.case = TRUE)]
      if (!length(hits)) return(NA_character_)
      s <- sub("^psi-mi:", "", hits[1], ignore.case = TRUE)
      desc <- sub(".*\\(([^()]*)\\).*", "\\1", s)
      if (!nzchar(desc) || identical(desc, s)) s else desc
    }
    
    ac <- trimws(ac)
    
    out <- lapply(seq_len(nrow(mint_df)), function(i) {
      row <- mint_df[i, , drop = FALSE]
      
      tibble::tibble(
        Query_AC     = ac,
        Partner_ID   = c(extract_uniprot(row$B_id), extract_uniprot(row$A_id)),  # raw ID
        Partner_Type = c("B", "A"),
        Method       = rep(extract_method(row$Detection_method), 2),
        PubMed       = rep(extract_pubmed(row$Pubmed), 2),
        Source       = rep(row$Source_db %||% NA_character_, 2)
      )
    })
    
    out <- dplyr::bind_rows(out)
    
    # Remove self-interactions and empty partner IDs
    out <- out[!is.na(out$Partner_ID) & nzchar(out$Partner_ID), , drop = FALSE]
    if (nzchar(ac)) {
      out <- out[out$Partner_ID != ac, , drop = FALSE]
    }
    
    out <- dplyr::distinct(out)
    
    # --- Make columns pretty & clickable ---
    
    # Query_AC: link to mint.bio results page
    out$Query <- make_mint_result_link(out$Query_AC)
    
    # Keep raw partner IDs for overlap checking and for links
    partner_ids <- out$Partner_ID
    
    # Partner column: link to IntAct/MINT search
    out$Partner <- make_mint_search_link(partner_ids)
    
    # ---------- Partner_Uniprot from existing UniProt table ----------
    partner_display <- partner_ids
    
    if (!is.null(uni_df) && nrow(uni_df)) {
      cn <- names(uni_df)
      acc_col <- intersect(c("Entry","accession","Accession","id"), cn)
      if (length(acc_col)) {
        acc_col <- acc_col[1]
        gene_cols <- intersect(c("Gene Names","gene_names"), cn)
        prot_cols <- intersect(c("protein_name","Protein names","Protein Names"), cn)
        
        if (length(gene_cols)) {
          name_vec <- uni_df[[gene_cols[1]]]
        } else if (length(prot_cols)) {
          name_vec <- uni_df[[prot_cols[1]]]
        } else {
          name_vec <- uni_df[[acc_col]]
        }
        
        acc_vec  <- as.character(uni_df[[acc_col]])
        name_vec <- as.character(name_vec)
        
        idx <- match(partner_ids, acc_vec)
        hit <- !is.na(idx)
        partner_display[hit] <- name_vec[idx[hit]]
      }
    }
    
    # Uniprot_Name: display name text, UniProt link
    out$Uniprot_Name <- sprintf(
      '<a href="https://www.uniprot.org/uniprotkb/%s" target="_blank">%s</a>',
      partner_ids, partner_display
    )
    
    # PubMed: clickable
    out$PubMed <- make_pubmed_link(out$PubMed)
    
    # Final column order (keep Partner_ID for overlap)
    out <- out[, c("Query", "Partner_ID", "Partner", "Uniprot_Name", "Method", "PubMed"), drop = FALSE]
    
    out
  }
  
  
  
  mint_extract_partners <- function(mint_df) {
    if (is.null(mint_df) || !nrow(mint_df)) return(character(0))
    cols <- intersect(c("A_alt","B_alt","A_aliases","B_aliases"), names(mint_df))
    if (!length(cols)) return(character(0))
    txt  <- paste(unlist(mint_df[cols], use.names = FALSE), collapse = "|")
    toks <- unique(unlist(strsplit(txt, "[| ]+", perl = TRUE), use.names = FALSE))
    toks <- toks[!grepl(":", toks, fixed = TRUE)]
    toks <- toks[nzchar(toks) & nchar(toks) <= 12]
    toks
  }
  
  
  # Pull ID vector from current Search results (rv$to_print)
  get_current_ids_from_results <- function(my_df) {
    if (is.null(my_df) || !nrow(my_df)) return(character(0))
    
    if ("Interaction_With_ID" %in% names(my_df)) {
      unique(as.character(my_df$Interaction_With_ID))
    } else if ("Protein_ID" %in% names(my_df)) {
      unique(as.character(my_df$Protein_ID))
    } else if ("Uniprot" %in% names(my_df)) {
      unique(as.character(my_df$Uniprot))
    } else if ("ID" %in% names(my_df)) {
      unique(as.character(my_df$ID))
    } else {
      character(0)
    }
  }
  
  
  make_cmp_summary <- function(uniprot_df, mint_df, my_df) {
    # IDs from current Search results
    my_ids <- get_current_ids_from_results(my_df)
    
    # Partner IDs from MINT (already cleaned in clean_mint_mitab)
    mint_ids <- if (!is.null(mint_df) && "Partner_ID" %in% names(mint_df)) {
      unique(as.character(mint_df$Partner_ID))
    } else {
      character(0)
    }
    
    # Overlap definition: MINT Partner_ID ∩ IDs in current Search results
    overlap_ids <- intersect(my_ids, mint_ids)
    
    # Helper: make a short text description like "A,B,C,... (n total)"
    short_vec <- function(x, max_n = 8) {
      x <- unique(x)
      x <- x[nzchar(x)]
      if (!length(x)) return("none")
      x <- sort(x)
      if (length(x) <= max_n) {
        paste(x, collapse = ", ")
      } else {
        paste0(
          paste(x[1:max_n], collapse = ", "),
          ", ... (", length(x), " total)"
        )
      }
    }
    
    data.frame(
      Metric = c(
        "UniProt hits returned",
        "MINT interaction rows (MITAB)",
        sprintf("Unique ID in current Search results (%s)", short_vec(my_ids)),
        sprintf("Overlap: MINT partners ∩ current ID (%s)", short_vec(overlap_ids))
      ),
      Value = c(
        if (is.null(uniprot_df)) 0L else nrow(uniprot_df),
        if (is.null(mint_df))    0L else nrow(mint_df),
        length(unique(my_ids)),
        length(unique(overlap_ids))
      ),
      stringsAsFactors = FALSE
    )
  }
  
  
  # ---------- Compare tab: dynamic ID input (simple version) ----------
  output$cmp_id_input <- renderUI({
    src <- input$cmp_source %||% "manual"
    
    ## --- 1. Manual typing mode ---
    if (identical(src, "manual")) {
      return(
        textInput("cmp_id", "UniProt accession or gene symbol", "")
      )
    }
    
    ## --- 2. Pick from current Search results ---
    # Prefer full Search results (to_print), then Check results (search_df)
    df <- NULL
    if (!is.null(rv$to_print) && nrow(rv$to_print)) {
      df <- rv$to_print
    } else if (!is.null(rv$search_df) && nrow(rv$search_df)) {
      df <- rv$search_df
    }
    
    if (is.null(df) || !nrow(df)) {
      return(tagList(
        helpText("No search results yet. Run 'Search' or 'Check' first."),
        textInput("cmp_id", "UniProt accession or gene symbol", "")
      ))
    }
    
    cn <- names(df)
    
    # Choose a default column (Symbol preferred, otherwise the first column)
    default_col <- if ("Symbol" %in% cn) "Symbol" else cn[1]
    
    # Use current selection if valid, otherwise default
    col_selected <- input$cmp_col
    if (is.null(col_selected) || !(col_selected %in% cn)) {
      col_selected <- default_col
    }
    
    vals <- sort(unique(as.character(df[[col_selected]])))
    vals <- vals[nzchar(vals)]
    
    tagList(
      selectInput(
        "cmp_col",
        "Choose ID column from results",
        choices  = setNames(cn, cn),
        selected = col_selected
      ),
      selectInput(
        "cmp_id",
        "Pick ID from current results",
        choices = vals
      )
    )
  })
  
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
        if (!file.exists("searching_geneSymbol.csv")) {
          showNotification("Please build searching_geneSymbol.csv first.", type = "error")
          log_append(rv, "Search error: searching_geneSymbol.csv not found")
          return()
        }
        idx <- read.csv("searching_geneSymbol.csv", check.names = FALSE)
        res <- idx[toupper(idx$geneSymbol) == toupper(term), , drop = FALSE]
      } else {
        if (!file.exists("searching_GeneID.csv")) {
          showNotification("Please build searching_GeneID.csv first.", type = "error")
          log_append(rv, "Search error: searching_GeneID.csv not found")
          return()
        }
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
    
    if (length(terms) == 0) {
      showNotification("Enter at least one valid term (commas/newlines OK).", type = "error")
      return()
    }
    
    mode <- input$print_mode %||% "geneSymbol"
    stem <- input$print_outname %||% "PrintResults"
    idx_path <- rv$index_path %||% "Datasets infomation.xlsx"
    
    pre <- list_output_files()
    
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
    
    if (is.null(res_df) || nrow(res_df) == 0) {
      showNotification("Didn't find, no rows to show.", type = "warning")
      log_append(rv, "Print/Export produced no rows.")
      return()
    }
    
    rv$to_print <- res_df
    output$tbl_to_print <- renderDT({
      render_dt_safe(make_hilite_table(res_df), fallback_df = res_df)
    })
    
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
    path <- paste0("Output/", row$file)
    ext  <- tolower(tools::file_ext(path))
    
    df <- tryCatch({
      if (ext %in% c("csv")) {
        tryCatch(
          utils::read.csv(path, nrows = 2000, check.names = FALSE, stringsAsFactors = FALSE),
          error = function(e) smart_preview_delim(path, sep = ",", max_rows = 2000L, quote_char = "\"")
        )
      } else if (ext %in% c("tsv")) {
        smart_preview_delim(path, sep = "\t", max_rows = 2000L, quote_char = "\"")
      } else if (ext %in% c("txt")) {
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
  
  # ---------- Compare tab: UniProt + MINT ----------
  observeEvent(input$btn_cmp_fetch, {
    id <- trimws(input$cmp_id %||% "")
    if (!nzchar(id)) {
      showNotification("Please provide a UniProt accession or gene symbol.", type = "error")
      return()
    }
    
    # 1) UniProt
    uni_df <- tryCatch({
      fetch_uniprot_basic(id)
    }, error = function(e) {
      showNotification(paste("UniProt fetch error:", conditionMessage(e)), type = "error")
      NULL
    })
    
    if (is.null(uni_df) || !nrow(uni_df)) {
      showNotification("No UniProtKB entry returned for this query.", type = "warning")
    }
    rv$cmp_uniprot <- uni_df
    
    # Choose accession / entry to send to MINT
    ac <- if (!is.null(uni_df)) {
      if ("Entry" %in% names(uni_df)) {
        uni_df$Entry[1]
      } else if ("accession" %in% names(uni_df)) {
        uni_df$accession[1]
      } else if ("Accession" %in% names(uni_df)) {
        uni_df$Accession[1]
      } else {
        id
      }
    } else {
      id
    }
    
    # 2) MINT
    mint_df <- tryCatch({
      fetch_mint_interactions(ac)
    }, error = function(e) {
      showNotification(paste("MINT fetch error:", conditionMessage(e)), type = "error")
      NULL
    })
    
    if (is.null(mint_df) || !nrow(mint_df)) {
      showNotification("No MINT interactions found (or service not responding).", type = "message")
    }
    rv$cmp_mint    <- clean_mint_mitab(mint_df, ac, uni_df)
    rv$cmp_summary <- make_cmp_summary(uni_df, rv$cmp_mint, rv$to_print)
    
    # ---- MINT vs current Search results: mark overlap & move to front ----
    my_ids <- get_current_ids_from_results(rv$to_print)
    
    if (!is.null(rv$cmp_mint) && nrow(rv$cmp_mint)) {
      if (length(my_ids) && "Partner_ID" %in% names(rv$cmp_mint)) {
        rv$cmp_mint$Overlap <- ifelse(rv$cmp_mint$Partner_ID %in% my_ids, "Yes", "")
        # move overlap ("Yes") rows to the top
        rv$cmp_mint <- rv$cmp_mint[order(rv$cmp_mint$Overlap == "Yes", decreasing = TRUE), , drop = FALSE]
      } else {
        rv$cmp_mint$Overlap <- ""
      }
    }
    
    
    # 3) Render DTs
    output$tbl_cmp_uniprot <- renderDT({
      df <- rv$cmp_uniprot
      if (is.null(df) || !nrow(df)) {
        df <- data.frame(`(no UniProt results yet)` = "", check.names = FALSE)
      } else {
        # turn accession into clickable UniProt link
        if ("Entry" %in% names(df)) {
          df$Entry <- make_uniprot_link(df$Entry)
        } else if ("accession" %in% names(df)) {
          df$accession <- make_uniprot_link(df$accession)
        } else if ("Accession" %in% names(df)) {
          df$Accession <- make_uniprot_link(df$Accession)
        } else if ("id" %in% names(df)) {
          df$id <- make_uniprot_link(df$id)
        }
      }
      
      make_hilite_table(
        df,
        stat_like = character(0),
        emph_cols = intersect(
          c("Entry","accession","Accession","id","Gene Names","Protein Names","gene_names"),
          names(df)
        ),
        enable_row_missing = FALSE,
        enable_desc = FALSE,
        enable_emph = FALSE,
        enable_bar = FALSE
      )
    })
    
    
    
    
    output$tbl_cmp_mint <- renderDT({
      df <- rv$cmp_mint
      if (is.null(df) || !nrow(df)) {
        df <- data.frame(`(no MINT interactions yet)` = "", check.names = FALSE)
        return(DT::datatable(df, options = list(pageLength = 10, dom = "t"),
                             rownames = FALSE))
      }
      
      # Drop Partner_ID from display (keep it inside rv$cmp_mint for logic)
      if ("Partner_ID" %in% names(df)) {
        df_show <- df[, setdiff(names(df), "Partner_ID"), drop = FALSE]
      } else {
        df_show <- df
      }
      
      dt <- DT::datatable(
        df_show,
        options = list(
          pageLength = 10,
          scrollX    = TRUE,
          dom        = "ftip"
        ),
        rownames = FALSE,
        escape = FALSE   # keep <a> links clickable
      )
      
      # ✅ Green highlight ONLY on Overlap column
      if ("Overlap" %in% names(df_show)) {
        dt <- DT::formatStyle(
          dt,
          "Overlap",
          backgroundColor = DT::styleEqual("Yes", "#d4edda"),
          fontWeight      = DT::styleEqual("Yes", "bold")
        )
      }
      
      dt
    })
    
    
    output$tbl_cmp_summary <- renderDT({
      df <- rv$cmp_summary
      if (is.null(df) || !nrow(df)) {
        df <- data.frame(`(no summary yet)` = "", check.names = FALSE)
      }
      DT::datatable(df, options = list(pageLength = 10, dom = "t"), rownames = FALSE)
    })
    
    log_append(rv, "External compare fetched for ID:", shQuote(id), " (UniProt AC used:", ac, ")")
  })
  
  # ---------- Logs ----------
  output$log_box <- renderText({
    paste(rv$log_msgs, collapse = "\n")
  })
}

shinyApp(ui, server)
