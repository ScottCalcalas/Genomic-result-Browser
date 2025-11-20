##* Xiaopei Zhang
##* 9/16/2025  V5 (hardened + back-fill IDs/Symbols)
##* - Trims/normalizes index text; tolerates blanks.
##* - Safe one-row builders (no length-0 scalars).
##* - PREPARE steps tolerate missing Row/Gene columns.
##* - Input: back-fills missing GeneID/Symbol using local annotation if available.
##* - If one side missing and not resolvable: synthesize NA_<other>.
##* - If both missing: drop the row (don’t index).
##* - Strict blank-like collapsing (e.g., "NA", ".", "null").
##* - Smarter ci_match handles readxl’s "ID...n" suffixes.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(purrr)
  library(rlang)
  library(tools)
  library(stringr)
  library(tibble)
})

dir.create("IndexedData", showWarnings = FALSE)
dir.create("Output",      showWarnings = FALSE)

# ---- helpers ----
`%||%` <- function(a, b) if (!is.null(a)) a else b

sanitize_filename <- function(x) {
  x <- enc2utf8(trimws(x %||% ""))
  if (!nzchar(x)) x <- "PrintResults"
  gsub("[<>:\"/\\\\|?*]", "_", x)
}

timestamp_str <- function() format(Sys.time(), "%Y%m%d-%H%M%S")

# unique while preserving first-seen order
unique_preserve <- function(x) {
  seen <- new.env(parent = emptyenv()); out <- vector("list", length(x)); k <- 0L
  for (v in x) { key <- as.character(v); if (!exists(key, envir = seen, inherits = FALSE)) {
    assign(key, TRUE, envir = seen); k <- k + 1L; out[[k]] <- v } }
  unlist(out[seq_len(k)], use.names = FALSE)
}

# normalize all character cols: trim + convert "" to NA
trim_df <- function(df) {
  if (!ncol(df)) return(df)
  for (nm in names(df)) {
    if (is.character(df[[nm]]) || is.factor(df[[nm]])) {
      v <- as.character(df[[nm]])
      v <- trimws(v)
      v[!nzchar(v)] <- NA_character_
      df[[nm]] <- v
    }
  }
  df
}

# scalar guards (no length-0 scalars)
scalarize_chr <- function(x, na = NA_character_) {
  if (is.null(x) || length(x) == 0) return(na)
  as.character(x)[1]
}
scalarize_num <- function(x, na = NA_real_) {
  if (is.null(x) || length(x) == 0) return(na)
  suppressWarnings(as.numeric(x)[1])
}

infer_type <- function(fname) {
  if (grepl("\\.csv$",  fname, TRUE)) return("CSV")
  if (grepl("\\.txt$",  fname, TRUE)) return("TXT")
  if (grepl("\\.xlsx$", fname, TRUE)) return("XLSX")
  return(NA_character_)
}

# smarter match: exact (case-insens.), else base before "...n"
ci_match <- function(want, cols) {
  want <- scalarize_chr(want)
  if (!nzchar(want) || is.na(want)) return(NA_character_)
  m <- which(tolower(cols) == tolower(want))
  if (length(m) == 1) return(cols[m])
  basecols <- gsub("\\.\\.\\d+$", "", cols)
  m2 <- which(tolower(basecols) == tolower(want))
  if (length(m2) >= 1) return(cols[m2[1]])
  NA_character_
}

# ----- blank-like handling -----
.blank_like <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  tolower(x) %in% c("", "na", "n/a", "null", "nil", "none", ".", "nan", "missing")
}
cleanse_tokens <- function(x) {
  x <- as.character(x)
  x[.blank_like(x)] <- NA_character_
  x
}

# ---------- Lightweight gene mappers with caching ----------
.hcache <- new.env(parent = emptyenv())
.pkg_ok <- function(pkg) isTRUE(requireNamespace(pkg, quietly = TRUE))

# Normalizers & detectors
norm_symbol <- function(x) {
  x <- scalarize_chr(x, na = NA_character_)
  if (is.na(x)) return(NA_character_)
  toupper(gsub("\\s+", "", x))
}
norm_ensembl <- function(x) {
  x <- scalarize_chr(x, na = NA_character_)
  if (is.na(x)) return(NA_character_)
  sub("\\.\\d+$", "", toupper(x))  # strip version
}
is_ensembl <- function(x) grepl("^ENSG\\d{6,}", toupper(x %||% ""))
is_entrez  <- function(x) grepl("^\\d+$", x %||% "")

map_symbol_to_id <- function(sym) {
  sym <- norm_symbol(sym)
  if (!nzchar(sym) || is.na(sym)) return(NA_character_)
  key <- paste0("S2I:", sym)
  if (exists(key, envir = .hcache, inherits = FALSE)) return(get(key, .hcache))
  out <- NA_character_
  if (.pkg_ok("AnnotationDbi") && .pkg_ok("org.Hs.eg.db")) {
    try({
      eg <- AnnotationDbi::mapIds(org.Hs.eg.db::org.Hs.eg.db,
                                  keys = sym, keytype = "SYMBOL",
                                  column = "ENSEMBL", multiVals = "first")
      out <- scalarize_chr(unname(eg[[1]]), NA_character_)
      if (nzchar(out)) out <- norm_ensembl(out)
    }, silent = TRUE)
  }
  if (is.na(out) && .pkg_ok("ensembldb") && .pkg_ok("EnsDb.Hsapiens.v86")) {
    try({
      edb <- EnsDb.Hsapiens.v86::EnsDb.Hsapiens.v86
      res <- ensembldb::select(edb, keys = sym, keytype = "SYMBOL", columns = "GENEID")
      if (NROW(res)) out <- norm_ensembl(res$GENEID[1])
    }, silent = TRUE)
  }
  assign(key, out, envir = .hcache); out
}

map_id_to_symbol <- function(id) {
  id <- scalarize_chr(id, na = NA_character_); if (is.na(id)) return(NA_character_)
  id_norm <- if (is_ensembl(id)) norm_ensembl(id) else id
  key <- paste0("I2S:", id_norm)
  if (exists(key, envir = .hcache, inherits = FALSE)) return(get(key, .hcache))
  out <- NA_character_
  if (.pkg_ok("AnnotationDbi") && .pkg_ok("org.Hs.eg.db")) {
    try({
      if (is_ensembl(id_norm)) {
        sy <- AnnotationDbi::mapIds(org.Hs.eg.db::org.Hs.eg.db,
                                    keys = id_norm, keytype = "ENSEMBL",
                                    column = "SYMBOL", multiVals = "first")
      } else if (is_entrez(id_norm)) {
        sy <- AnnotationDbi::mapIds(org.Hs.eg.db::org.Hs.eg.db,
                                    keys = id_norm, keytype = "ENTREZID",
                                    column = "SYMBOL", multiVals = "first")
      } else sy <- NA_character_
      out <- scalarize_chr(unname(sy[[1]]), NA_character_)
      if (nzchar(out)) out <- norm_symbol(out)
    }, silent = TRUE)
  }
  if (is.na(out) && is_ensembl(id_norm) && .pkg_ok("ensembldb") && .pkg_ok("EnsDb.Hsapiens.v86")) {
    try({
      edb <- EnsDb.Hsapiens.v86::EnsDb.Hsapiens.v86
      res <- ensembldb::select(edb, keys = id_norm, keytype = "GENEID", columns = "SYMBOL")
      if (NROW(res)) out <- norm_symbol(res$SYMBOL[1])
    }, silent = TRUE)
  }
  assign(key, out, envir = .hcache); out
}

# Vectorized fill with rules:
# - If both missing -> drop
# - If one missing -> try to infer; if still missing -> synthesize "NA_<other>"
# - Ensure no blanks in outputs
resolve_gene_pairs <- function(id_vec, sym_vec) {
  n <- length(id_vec)
  id  <- cleanse_tokens(id_vec)
  sym <- cleanse_tokens(sym_vec)
  
  # attempt back-fill
  for (i in seq_len(n)) {
    if (is.na(id[i]) && !is.na(sym[i])) {
      guess <- map_symbol_to_id(sym[i])
      if (!is.na(guess) && nzchar(guess)) id[i] <- guess
    }
    if (is.na(sym[i]) && !is.na(id[i])) {
      guess <- map_id_to_symbol(id[i])
      if (!is.na(guess) && nzchar(guess)) sym[i] <- guess
    }
  }
  # synthesize if still one side missing
  for (i in seq_len(n)) {
    if (is.na(id[i]) && !is.na(sym[i]))  id[i]  <- paste0("NA_", norm_symbol(sym[i]))
    if (!is.na(id[i]) && is.na(sym[i])) sym[i] <- paste0("NA_", norm_ensembl(id[i]))
  }
  
  keep <- !(is.na(id) & is.na(sym))
  list(GeneID = id[keep], GeneSymbol = sym[keep], keep = keep)
}

# ---------- Build per-dataset index CSVs ----------
xiaopei.input <- function(file.Name, Sheet = 1, xlsx.index.location = "Datasets infomation.xlsx") {
  indexfile <- readxl::read_xlsx(xlsx.index.location)
  indexfile <- trim_df(indexfile)
  
  # match index row
  i <- which(as.character(indexfile$DatasetName) == as.character(file.Name) &
               as.character(indexfile$Sheet)       == as.character(Sheet))
  if (length(i) == 0) {
    warning(sprintf("[xiaopei.input] No index row for %s (Sheet=%s). Writing empty CSV.", file.Name, Sheet))
    out <- tibble::tibble(Row = character(0), GeneID = character(0), geneSymbol = character(0))
    utils::write.csv(out, file.path("IndexedData", sprintf("%s~%s.csv", file.Name, Sheet)), row.names = FALSE)
    return(invisible(NULL))
  }
  i <- i[1]
  
  # determine file type
  File_Type <- toupper(scalarize_chr(indexfile$File_Type[i]))
  if (!nzchar(File_Type) || is.na(File_Type)) File_Type <- infer_type(file.Name)
  
  # read data
  path <- file.path("datasets", file.Name)
  nowdata <- tryCatch({
    if (identical(File_Type, "CSV"))  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    else if (identical(File_Type, "TXT"))  utils::read.table(path, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
    else if (identical(File_Type, "XLSX")) as.data.frame(readxl::read_xlsx(path, sheet = Sheet), check.names = FALSE, stringsAsFactors = FALSE)
    else stop(sprintf("Unknown File_Type for %s: %s", file.Name, File_Type))
  }, error = function(e) {
    warning(sprintf("[xiaopei.input] Failed reading %s (Sheet=%s): %s", file.Name, Sheet, conditionMessage(e)))
    NULL
  })
  
  if (is.null(nowdata) || !NROW(nowdata)) {
    out <- tibble::tibble(Row = character(0), GeneID = character(0), geneSymbol = character(0))
    utils::write.csv(out, file.path("IndexedData", sprintf("%s~%s.csv", file.Name, Sheet)), row.names = FALSE)
    cat("run done (empty): ", file.Name, "\n")
    return(invisible(NULL))
  }
  
  # map gene columns (case-insensitive)
  id_col  <- scalarize_chr(indexfile$VarN_GeneID[i])
  sym_col <- scalarize_chr(indexfile$VarN_GeneName[i])
  id_in   <- ci_match(id_col,  names(nowdata))
  sym_in  <- ci_match(sym_col, names(nowdata))
  
  n <- nrow(nowdata)
  gene_id_vec  <- if (!is.na(id_in))  as.character(nowdata[[id_in]])  else rep(NA_character_, n)
  gene_sym_vec <- if (!is.na(sym_in)) as.character(nowdata[[sym_in]]) else rep(NA_character_, n)
  
  # normalize -> back-fill -> enforce no blanks (drop both-missing)
  gene_id_vec  <- cleanse_tokens(gene_id_vec)
  gene_sym_vec <- cleanse_tokens(gene_sym_vec)
  filled <- resolve_gene_pairs(gene_id_vec, gene_sym_vec)
  keep   <- filled$keep
  
  if (!any(keep)) {
    out <- tibble::tibble(Row = character(0), GeneID = character(0), geneSymbol = character(0))
    utils::write.csv(out, file.path("IndexedData", sprintf("%s~%s.csv", file.Name, Sheet)), row.names = FALSE)
    cat("run done (all dropped: missing IDs & Symbols): ", file.Name, "\n")
    return(invisible(NULL))
  }
  
  # build Dataindex with fixed length columns
  Row_all <- paste0(indexfile$DatasetName[i], "~", Sheet, ".", seq_len(n))
  Row     <- Row_all[keep]
  Dataindex <- tibble::tibble(
    Row        = Row,
    GeneID     = filled$GeneID,
    geneSymbol = filled$GeneSymbol
  )
  
  utils::write.csv(Dataindex, file.path("IndexedData", sprintf("%s~%s.csv", file.Name, Sheet)), row.names = FALSE)
  cat("run done : ", file.Name, "\n")
}

xiaopei.input.all <- function(xlsx.index.location = "Datasets infomation.xlsx") {
  indexfile <- readxl::read_xlsx(xlsx.index.location)
  indexfile <- trim_df(indexfile)
  if (!NROW(indexfile)) {
    warning("[input.all] Index has 0 rows.")
    return(invisible(NULL))
  }
  for (i in seq_len(nrow(indexfile))) {
    fn <- scalarize_chr(indexfile$DatasetName[i])
    sh <- indexfile$Sheet[i]
    tryCatch(
      xiaopei.input(fn, Sheet = sh, xlsx.index.location = xlsx.index.location),
      error = function(e) warning(sprintf("[input.all] Failed on row %d: %s (Sheet=%s): %s", i, fn, as.character(sh), conditionMessage(e)))
    )
  }
}

##********************************************************##
# PREPARE search tables (robust to missing columns)

# ---- PREPARE: GeneID ----------------------------------------------
xiaopei.prepare.GeneID <- function(index.location = "IndexedData",
                                   file_regex = "~[0-9]+\\.csv$") {
  message("[prepare.GeneID] selecting {GeneID, Row}; coercing Row -> character")
  files <- list.files(index.location, pattern = file_regex, full.names = TRUE)
  if (!length(files)) {
    utils::write.csv(data.frame(GeneID = character()), "searching_GeneID.csv", row.names = FALSE)
    message("No index CSVs found; wrote empty searching_GeneID.csv")
    return(invisible(NULL))
  }
  
  read_one <- function(path) {
    nm <- tools::file_path_sans_ext(basename(path))
    df <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    if (!"Row" %in% names(df))   df$Row    <- paste0(basename(path), "~1.", seq_len(nrow(df)))
    if (!"GeneID" %in% names(df)) df$GeneID <- NA_character_
    df$GeneID <- cleanse_tokens(df$GeneID)
    out <- dplyr::select(df, GeneID, Row)
    out$Row <- as.character(out$Row)
    dplyr::rename(out, !!nm := Row)
  }
  
  pieces <- purrr::map(files, read_one)
  combined <- purrr::reduce(pieces, dplyr::full_join, by = "GeneID")
  utils::write.csv(combined, file = "searching_GeneID.csv", row.names = FALSE)
  message(sprintf("Done: %d files → %d rows, %d columns", length(files), nrow(combined), ncol(combined)))
  invisible(NULL)
}

# ---- PREPARE: geneSymbol ------------------------------------------
xiaopei.prepare.geneSymbol <- function(index.location = "IndexedData",
                                       file_regex = "~[0-9]+\\.csv$") {
  message("[prepare.geneSymbol] selecting {geneSymbol, Row}; coercing Row -> character")
  files <- list.files(index.location, pattern = file_regex, full.names = TRUE)
  if (!length(files)) {
    utils::write.csv(data.frame(geneSymbol = character()), "searching_geneSymbol.csv", row.names = FALSE)
    message("No index CSVs found; wrote empty searching_geneSymbol.csv")
    return(invisible(NULL))
  }
  
  read_one <- function(path) {
    nm <- tools::file_path_sans_ext(basename(path))
    df <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    if (!"Row" %in% names(df)) df$Row <- paste0(basename(path), "~1.", seq_len(nrow(df)))
    if (!"geneSymbol" %in% names(df) && "GeneSymbol" %in% names(df)) {
      names(df)[names(df) == "GeneSymbol"] <- "geneSymbol"
    }
    if (!"geneSymbol" %in% names(df)) df$geneSymbol <- NA_character_
    df$geneSymbol <- cleanse_tokens(df$geneSymbol)
    out <- dplyr::select(df, geneSymbol, Row)
    out$Row <- as.character(out$Row)
    dplyr::rename(out, !!nm := Row)
  }
  
  pieces <- purrr::map(files, read_one)
  combined <- purrr::reduce(pieces, dplyr::full_join, by = "geneSymbol")
  utils::write.csv(combined, file = "searching_geneSymbol.csv", row.names = FALSE)
  message(sprintf("Done: %d files → %d rows, %d columns", length(files), nrow(combined), ncol(combined)))
  invisible(NULL)
}

# Convenience wrapper
xiaopei.prepare.all <- function(index.location = "IndexedData") {
  xiaopei.prepare.GeneID(index.location)
  xiaopei.prepare.geneSymbol(index.location)
}

##********************************************************##
# search (console)
xiaopei.search.geneSymbol <- function(geneSymbol) {
  index <- utils::read.csv("searching_geneSymbol.csv", check.names = FALSE)
  result <- index[which(index$geneSymbol == geneSymbol), , drop = FALSE]
  cat("Find: "); print(result)
}

xiaopei.search.GeneID <- function(GeneID) {
  index <- utils::read.csv("searching_GeneID.csv", check.names = FALSE)
  result <- index[which(index$GeneID == GeneID), , drop = FALSE]
  cat("Find: "); print(result)
}

##********************************************************##
# print core (with metadata block first)

print_and_save_selected_rows <- function(
    result,
    output_file = "Output/TEMP searching results.csv",
    xlsx.index.location = "Datasets infomation.xlsx",
    warn_on_malformed = FALSE,
    return_rows = FALSE
) {
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  if (file.exists(output_file)) file.remove(output_file)
  
  indexfile <- readxl::read_excel(xlsx.index.location)
  indexfile <- trim_df(indexfile)
  
  vals   <- as.vector(unlist(result, use.names = FALSE))
  vals   <- vals[!is.na(vals) & nzchar(vals)]
  tokens <- grep("^[^~]+~.+\\.[0-9]+$", vals, value = TRUE)
  if (!length(tokens)) return(if (return_rows) tibble() else invisible(NULL))
  
  parts    <- strsplit(tokens, "~", fixed = TRUE)
  files    <- trimws(vapply(parts, `[[`, "", 1L))
  sheetrow <- trimws(vapply(parts, `[[`, "", 2L))
  
  lastdot <- regexpr("\\.(?=[^\\.]*$)", sheetrow, perl = TRUE)
  sheets <- ifelse(lastdot > 0, substr(sheetrow, 1L, lastdot - 1L), NA_character_)
  rows_s <- ifelse(lastdot > 0, substr(sheetrow, lastdot + 1L, nchar(sheetrow)), NA_character_)
  rows   <- suppressWarnings(as.integer(trimws(rows_s)))
  
  keep  <- nzchar(files) & nzchar(sheets) & is.finite(rows) & rows >= 1L
  files <- files[keep]; sheets <- sheets[keep]; rows <- rows[keep]
  if (!length(files)) return(if (return_rows) tibble() else invisible(NULL))
  
  SEP <- "\u001F"
  keys <- paste0(files, SEP, sheets)
  keys_order <- unique(keys)
  rows_by_key <- lapply(keys_order, function(k) unique(rows[keys == k]))
  names(rows_by_key) <- keys_order
  
  # Exclude mapping columns and also hide File_Type & Sheet from metadata
  drop_meta <- c("VarN_GeneID", "VarN_GeneName", "File_Type", "Sheet")
  
  collected <- list()
  
  for (key in keys_order) {
    split_key <- strsplit(key, SEP, fixed = TRUE)[[1]]
    file_name  <- split_key[1]
    sheet_name <- split_key[2]
    wanted     <- rows_by_key[[key]]
    path <- file.path("datasets", file_name)
    
    # index row for metadata + mapping
    i <- which(as.character(indexfile$DatasetName) == as.character(file_name) &
                 as.character(indexfile$Sheet)       == as.character(sheet_name))
    File_Type <- if (length(i)) toupper(scalarize_chr(indexfile$File_Type[i[1]])) else NA_character_
    if (!nzchar(File_Type) || is.na(File_Type)) File_Type <- infer_type(file_name)
    if (!nzchar(File_Type) || is.na(File_Type)) next
    
    # read the data table
    df <- tryCatch({
      if (File_Type == "CSV") {
        utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
      } else if (File_Type == "TXT") {
        utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE)
      } else if (File_Type == "XLSX") {
        sheet_arg <- suppressWarnings(as.integer(sheet_name))
        if (!is.finite(sheet_arg)) sheet_arg <- sheet_name
        as.data.frame(readxl::read_excel(path, sheet = sheet_arg), check.names = FALSE, stringsAsFactors = FALSE)
      } else NULL
    }, error = function(e) NULL)
    if (is.null(df)) next
    
    valid <- wanted[wanted <= nrow(df)]
    if (!length(valid)) next
    
    # console + file output
    cat("=== File:", file_name, " | Sheet:", sheet_name, " | Type:", File_Type, "===\n")
    cat("Rows:", paste(valid, collapse = ", "), "\n")
    for (r in valid) { cat(sprintf("[Row %d]\n", r)); print(df[r, , drop = FALSE]) }
    cat("\n")
    cat(sprintf("# File: %s\n", file_name), file = output_file, append = TRUE)
    cat(sprintf("# Sheet: %s\n", sheet_name), file = output_file, append = TRUE)
    suppressWarnings(
      utils::write.table(head(df, 0), file = output_file, sep = ",",
                         col.names = TRUE, row.names = FALSE, append = TRUE, qmethod = "double")
    )
    utils::write.table(df[valid, , drop = FALSE], file = output_file, sep = ",",
                       col.names = FALSE, row.names = FALSE, append = TRUE, qmethod = "double")
    cat("\n", file = output_file, append = TRUE)
    
    if (return_rows) {
      #1) Metadata block (drop mapping vars + File_Type + Sheet) -------
      meta_row <- if (length(i)) indexfile[i[1], , drop = FALSE] else tibble()
      if (nrow(meta_row) == 0) meta_row <- tibble(DatasetName = file_name)
      meta_cols <- setdiff(names(meta_row), drop_meta)
      meta_rep  <- meta_row[rep(1, length(valid)), meta_cols, drop = FALSE]
      meta_rep  <- dplyr::mutate(meta_rep, dplyr::across(everything(), ~as.character(.x %||% NA_character_)))
      
      #2) Data block, with standardized gene columns -------------------
      block_data <- as_tibble(df[valid, , drop = FALSE])
      
      id_col  <- if (length(i)) scalarize_chr(indexfile$VarN_GeneID[i[1]])  else NA_character_
      sym_col <- if (length(i)) scalarize_chr(indexfile$VarN_GeneName[i[1]]) else NA_character_
      
      present_id  <- ci_match(id_col,  names(block_data))
      present_sym <- ci_match(sym_col, names(block_data))
      
      # === Rename for USER-FACING table ===
      if (!is.na(present_id) && !"ID" %in% names(block_data)) {
        names(block_data)[names(block_data) == present_id] <- "ID"
      }
      if ("GeneID" %in% names(block_data) && !"ID" %in% names(block_data)) {
        names(block_data)[names(block_data) == "GeneID" ] <- "ID"
      }
      if (!is.na(present_sym) && !"GeneSymbol" %in% names(block_data)) {
        names(block_data)[names(block_data) == present_sym] <- "Symbol"
      }
      
      block_data <- dplyr::mutate(block_data, dplyr::across(everything(), as.character))
      
      #3) Assemble: [metadata] + [data] -------
      block <- dplyr::bind_cols(meta_rep, block_data)
      
      
      block_data <- dplyr::relocate(
        block_data,
        dplyr::any_of(c("Symbol", "ID", "GeneID")),
        .before = 1
      )
      
      
      collected[[length(collected) + 1L]] <- block
    }
  }
  
  message(sprintf("Done. Saved to '%s'.", output_file))
  if (return_rows) {
    if (length(collected)) return(dplyr::bind_rows(collected))
    return(tibble())
  }
  invisible(NULL)
}












##********************************************************##
# print helpers (pass index path through)

xiaopei.print.geneSymbol <- function(geneSymbol, output_name = NULL,
                                     fulldata.location = "datasets",
                                     add_timestamp = TRUE,
                                     xlsx.index.location = "Datasets infomation.xlsx") {
  index <- utils::read.csv("searching_geneSymbol.csv", check.names = FALSE)
  terms <- unique_preserve(as.character(geneSymbol))
  base  <- sanitize_filename(output_name %||% "")
  combined <- tibble()
  
  for (t in terms) {
    result <- index[which(toupper(index$geneSymbol) == toupper(t)), , drop = FALSE]
    cat("Find: "); print(result); cat("\n\n")
    if (!nrow(result)) next
    
    suffix <- sanitize_filename(t)
    stem   <- if (nzchar(base)) paste0(base, "_", suffix) else suffix
    mode_tag <- "exact"
    fname <- if (add_timestamp) paste0(stem, "__", mode_tag, "_", timestamp_str()) else paste0(stem, "__", mode_tag)
    
    ofile  <- file.path("Output", paste0(fname, ".csv"))
    df <- print_and_save_selected_rows(result, output_file = ofile,
                                       xlsx.index.location = xlsx.index.location,
                                       return_rows = TRUE)
    if (nrow(df)) combined <- dplyr::bind_rows(combined, dplyr::mutate(df, `__Query` = t))
  }
  return(combined)
}

xiaopei.print.GeneID <- function(GeneID, output_name = NULL,
                                 fulldata.location = "datasets",
                                 add_timestamp = TRUE,
                                 xlsx.index.location = "Datasets infomation.xlsx") {
  index <- utils::read.csv("searching_GeneID.csv", check.names = FALSE)
  terms <- unique_preserve(as.character(GeneID))
  base  <- sanitize_filename(output_name %||% "")
  combined <- tibble()
  
  for (t in terms) {
    result <- index[which(as.character(index$GeneID) == as.character(t)), , drop = FALSE]
    cat("Find: "); print(result); cat("\n\n")
    if (!nrow(result)) next
    
    suffix <- sanitize_filename(t)
    stem   <- if (nzchar(base)) paste0(base, "_", suffix) else suffix
    mode_tag <- "exact"
    fname <- if (add_timestamp) paste0(stem, "__", mode_tag, "_", timestamp_str()) else paste0(stem, "__", mode_tag)
    
    ofile  <- file.path("Output", paste0(fname, ".csv"))
    df <- print_and_save_selected_rows(result, output_file = ofile,
                                       xlsx.index.location = xlsx.index.location,
                                       return_rows = TRUE)
    if (nrow(df)) combined <- dplyr::bind_rows(combined, dplyr::mutate(df, `__Query` = t))
  }
  return(combined)
}

xiaopei.print.geneSymbol.family <- function(geneSymbol, output_name = NULL,
                                            fulldata.location = "datasets",
                                            add_timestamp = TRUE,
                                            xlsx.index.location = "Datasets infomation.xlsx") {
  index <- utils::read.csv("searching_geneSymbol.csv", check.names = FALSE)
  terms <- unique_preserve(as.character(geneSymbol))
  base  <- sanitize_filename(output_name %||% "")
  combined <- tibble()
  
  for (t in terms) {
    family_key <- gsub("[^A-Za-z]", "", t)
    if (!nzchar(family_key)) next
    result <- index[grepl(family_key, index$geneSymbol, ignore.case = TRUE), , drop = FALSE]
    
    cat("Find (family:", family_key, ")\n"); print(result); cat("\n\n")
    if (!nrow(result)) next
    
    suffix <- sanitize_filename(family_key)
    stem   <- if (nzchar(base)) paste0(base, "_", suffix) else suffix
    mode_tag <- "family"
    fname <- if (add_timestamp) paste0(stem, "__", mode_tag, "_", timestamp_str()) else paste0(stem, "__", mode_tag)
    
    ofile  <- file.path("Output", paste0(fname, ".csv"))
    df <- print_and_save_selected_rows(result, output_file = ofile,
                                       xlsx.index.location = xlsx.index.location,
                                       return_rows = TRUE)
    if (nrow(df)) {
      df <- dplyr::mutate(df, `__Query` = t, `__Family` = family_key)
      df <- dplyr::relocate(df, dplyr::starts_with("__"), .after = dplyr::last_col())
      combined <- dplyr::bind_rows(combined, df)
    }
  }
  return(combined)
}

xiaopei.print.geneSymbol.fuzzy <- function(geneSymbol, output_name = NULL,
                                           fulldata.location = "datasets",
                                           add_timestamp = TRUE,
                                           xlsx.index.location = "Datasets infomation.xlsx") {
  index <- utils::read.csv("searching_geneSymbol.csv", check.names = FALSE)
  terms <- unique_preserve(as.character(geneSymbol))
  base  <- sanitize_filename(output_name %||% "")
  combined <- tibble()
  
  for (t in terms) {
    hits_logical <- agrepl(t, index$geneSymbol, ignore.case = TRUE, max.distance = 0.1, useBytes = FALSE)
    result <- index[hits_logical, , drop = FALSE]
    
    cat("Find (fuzzy:", t, ")\n"); print(result); cat("\n\n")
    if (!nrow(result)) next
    
    suffix <- sanitize_filename(t)
    stem   <- if (nzchar(base)) paste0(base, "_", suffix) else suffix
    mode_tag <- "fuzzy-0.1"
    fname <- if (add_timestamp) paste0(stem, "__", mode_tag, "_", timestamp_str()) else paste0(stem, "__", mode_tag)
    
    ofile  <- file.path("Output", paste0(fname, ".csv"))
    df <- print_and_save_selected_rows(result, output_file = ofile,
                                       xlsx.index.location = xlsx.index.location,
                                       return_rows = TRUE)
    if (nrow(df)) {
      df <- dplyr::mutate(df, `__Query` = t, `__Fuzzy` = "0.1")
      df <- dplyr::relocate(df, dplyr::starts_with("__"), .after = dplyr::last_col())
      combined <- dplyr::bind_rows(combined, df)
    }
  }
  return(combined)
}
