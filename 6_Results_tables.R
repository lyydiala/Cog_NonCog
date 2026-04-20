library(openxlsx)

# ── Polut ─────────────────────────────────────────────────────────────────────
raw_dir  <- "/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/results"
out_file <- "/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/OUTPUT/tables/Results.xlsx"

# ── Asteikot ──────────────────────────────────────────────────────────────────
scales <- list(
  c("Agr", "Aggressive behavior"),
  c("Att", "Attention problems"),
  c("Del", "Rule-breaking behavior"),
  c("Anx", "Anxious"),
  c("Som", "Somatic complaints"),
  c("Wth", "Withdrawn"),
  c("Soc", "Social problems"),
  c("Tht", "Thought problems"),
  c("Ext", "Externalising problem behavior"),
  c("Int", "Internalising problem behavior")
)

# ── Apufunktiot ───────────────────────────────────────────────────────────────
read_raw <- function(filepath) {
  sheet_names <- getSheetNames(filepath)
  data <- list()
  for (nm in sheet_names) {
    df <- read.xlsx(filepath, sheet = nm, colNames = FALSE)
    sheet_data <- list()
    for (i in seq_len(nrow(df))) {
      key <- trimws(as.character(df[i, 1]))
      if (is.na(key) || key == "") next
      if (key == "Sample Size") {
        sheet_data[["Sample Size"]] <- as.integer(df[i, 2])
      } else if (!key %in% c("term", "AIC", "BIC", "Adjusted R2")) {
        est  <- suppressWarnings(as.numeric(df[i, 2]))
        pval <- suppressWarnings(as.numeric(df[i, 5]))
        if (!is.na(est)) {
          sheet_data[[key]] <- list(estimate = est, p.value = pval)
        }
      }
    }
    data[[nm]] <- sheet_data
  }
  data
}

get_vals <- function(raw, prefix, suffix, term) {
  key   <- paste0(prefix, "Stdm", suffix)
  entry <- raw[[key]][[term]]
  if (is.null(entry)) return(list(estimate = NA, p.value = NA))
  entry
}

get_n <- function(raw, prefix, suffix) {
  raw[[paste0(prefix, "Stdm", suffix)]][["Sample Size"]]
}

top_sty <- createStyle(border = "Top",    borderStyle = "thin")
bot_sty <- createStyle(border = "Bottom", borderStyle = "thin")

num_sty      <- createStyle(numFmt = "0.000")
num_bold_sty <- createStyle(numFmt = "0.000", textDecoration = "bold")

write_val <- function(wb, ws, estimate, p.value, col, row) {
  writeData(wb, ws, estimate, startCol = col, startRow = row, keepNA = FALSE)
  if (!is.na(estimate)) {
    sty <- if (!is.na(p.value) && p.value < 0.01) num_bold_sty else num_sty
    addStyle(wb, ws, sty, rows = row, cols = col, stack = TRUE)
  }
}

write_pval <- function(wb, ws, p.value, col, row) {
  writeData(wb, ws, p.value, startCol = col, startRow = row, keepNA = FALSE)
  if (!is.na(p.value)) addStyle(wb, ws, num_sty, rows = row, cols = col, stack = TRUE)
}

write_top_rows <- function(wb, ws, left_label, right_label, title_a, title_b) {
  writeData(wb, ws, title_a, startCol = 1, startRow = 1)
  writeData(wb, ws, title_b, startCol = 2, startRow = 1)
  writeData(wb, ws, left_label,  startCol = 2, startRow = 2)
  writeData(wb, ws, right_label, startCol = 8, startRow = 2)
  mergeCells(wb, ws, cols = 2:6,  rows = 2)
  mergeCells(wb, ws, cols = 8:12, rows = 2)
  addStyle(wb, ws, createStyle(border = "TopBottom", borderStyle = "thin", halign = "center"),
           rows = 2, cols = 2:12, gridExpand = TRUE)
  writeData(wb, ws, "Behavioral scale", startCol = 1, startRow = 3)
  writeData(wb, ws, "Model 1", startCol = 3,  startRow = 3)
  writeData(wb, ws, "Model 2", startCol = 5,  startRow = 3)
  writeData(wb, ws, "Model 1", startCol = 9,  startRow = 3)
  writeData(wb, ws, "Model 2", startCol = 11, startRow = 3)
  mergeCells(wb, ws, cols = 3:4,   rows = 3)
  mergeCells(wb, ws, cols = 5:6,   rows = 3)
  mergeCells(wb, ws, cols = 9:10,  rows = 3)
  mergeCells(wb, ws, cols = 11:12, rows = 3)
  addStyle(wb, ws, createStyle(border = "TopBottom", borderStyle = "thin", halign = "center"),
           rows = 3, cols = 2:12, gridExpand = TRUE)
  writeData(wb, ws, "Predictor", startCol = 1, startRow = 4)
  for (item in list(c(2,"N"),c(3,"Beta"),c(4,"P-value"),c(5,"Beta"),c(6,"P-value"),
                    c(8,"N"),c(9,"Beta"),c(10,"P-value"),c(11,"Beta"),c(12,"P-value"))) {
    writeData(wb, ws, item[2], startCol = as.integer(item[1]), startRow = 4)
  }
}

write_full_ea <- function(wb, ws, left_raw, right_raw, left_sfx, right_sfx) {
  row <- 5
  for (sc in scales) {
    prefix <- sc[1]; display_name <- sc[2]
    writeData(wb, ws, display_name,                          startCol = 1, startRow = row)
    writeData(wb, ws, get_n(left_raw,  prefix, left_sfx[1]), startCol = 2, startRow = row)
    writeData(wb, ws, get_n(right_raw, prefix, right_sfx[1]),startCol = 8, startRow = row)
    addStyle(wb, ws, top_sty, rows = row, cols = 1:12, gridExpand = TRUE)
    row <- row + 1
    m1l <- get_vals(left_raw,  prefix, left_sfx[1],  "pgi_EA4")
    m2l <- get_vals(left_raw,  prefix, left_sfx[2],  "pgi_EA4")
    m1r <- get_vals(right_raw, prefix, right_sfx[1], "pgi_EA4")
    m2r <- get_vals(right_raw, prefix, right_sfx[2], "pgi_EA4")
    writeData(wb, ws, "PGI Offspring EA", startCol=1,  startRow=row)
    write_val(wb, ws, m1l$estimate, m1l$p.value, 3, row)
    write_pval(wb, ws, m1l$p.value, 4, row)
    write_val(wb, ws, m2l$estimate, m2l$p.value, 5, row)
    write_pval(wb, ws, m2l$p.value, 6, row)
    write_val(wb, ws, m1r$estimate, m1r$p.value, 9, row)
    write_pval(wb, ws, m1r$p.value, 10, row)
    write_val(wb, ws, m2r$estimate, m2r$p.value, 11, row)
    write_pval(wb, ws, m2r$p.value, 12, row)
    row <- row + 1
    p2l <- get_vals(left_raw,  prefix, left_sfx[2],  "pgi_sum_EA4")
    p2r <- get_vals(right_raw, prefix, right_sfx[2], "pgi_sum_EA4")
    writeData(wb, ws, "PGI Parental EA", startCol=1,  startRow=row)
    write_val(wb, ws, p2l$estimate, p2l$p.value, 5, row)
    write_pval(wb, ws, p2l$p.value, 6, row)
    write_val(wb, ws, p2r$estimate, p2r$p.value, 11, row)
    write_pval(wb, ws, p2r$p.value, 12, row)
    addStyle(wb, ws, bot_sty, rows = row, cols = 2:12, gridExpand = TRUE, stack = TRUE)
    row <- row + 1
  }
  setColWidths(wb, ws, cols = 1, widths = 26)
}

write_decomposed_ea <- function(wb, ws, left_raw, right_raw, left_sfx, right_sfx) {
  row <- 5
  for (sc in scales) {
    prefix <- sc[1]; display_name <- sc[2]
    writeData(wb, ws, display_name,                          startCol = 1, startRow = row)
    writeData(wb, ws, get_n(left_raw,  prefix, left_sfx[1]), startCol = 2, startRow = row)
    writeData(wb, ws, get_n(right_raw, prefix, right_sfx[1]),startCol = 8, startRow = row)
    row <- row + 1
    for (cog in c("NonCog", "Cog")) {
      label <- if (cog == "NonCog") "PGI Offspring EA NonCognitive" else "PGI Offspring EA Cognitive"
      m1l <- get_vals(left_raw,  prefix, left_sfx[1],  paste0("pgi_EA3", cog))
      m2l <- get_vals(left_raw,  prefix, left_sfx[2],  paste0("pgi_EA3", cog))
      m1r <- get_vals(right_raw, prefix, right_sfx[1], paste0("pgi_EA3", cog))
      m2r <- get_vals(right_raw, prefix, right_sfx[2], paste0("pgi_EA3", cog))
      writeData(wb, ws, label,        startCol=1,  startRow=row)
      write_val(wb, ws, m1l$estimate, m1l$p.value, 3, row)
      write_pval(wb, ws, m1l$p.value, 4, row)
      write_val(wb, ws, m2l$estimate, m2l$p.value, 5, row)
      write_pval(wb, ws, m2l$p.value, 6, row)
      write_val(wb, ws, m1r$estimate, m1r$p.value, 9, row)
      write_pval(wb, ws, m1r$p.value, 10, row)
      write_val(wb, ws, m2r$estimate, m2r$p.value, 11, row)
      write_pval(wb, ws, m2r$p.value, 12, row)
      row <- row + 1
    }
    for (cog in c("NonCog", "Cog")) {
      label <- if (cog == "NonCog") "PGI Parental EA NonCognitive" else "PGI Parental EA Cognitive"
      p2l <- get_vals(left_raw,  prefix, left_sfx[2],  paste0("pgi_sum_EA3", cog))
      p2r <- get_vals(right_raw, prefix, right_sfx[2], paste0("pgi_sum_EA3", cog))
      writeData(wb, ws, label,        startCol=1,  startRow=row)
      write_val(wb, ws, p2l$estimate, p2l$p.value, 5, row)
      write_pval(wb, ws, p2l$p.value, 6, row)
      write_val(wb, ws, p2r$estimate, p2r$p.value, 11, row)
      write_pval(wb, ws, p2r$p.value, 12, row)
      if (cog == "Cog") addStyle(wb, ws, bot_sty, rows = row, cols = 1:12, gridExpand = TRUE, stack = TRUE)
      row <- row + 1
    }
  }
  setColWidths(wb, ws, cols = 1, widths = 26)
}

write_interaction_full <- function(wb, ws, cbcl_raw, ysr_raw) {
  writeData(wb, ws, "Sex interaction with", startCol = 1, startRow = 4)
  for (item in list(c(2,"N"),c(3,"Beta"),c(4,"P-value"),c(5,"Beta"),c(6,"P-value"),
                    c(8,"N"),c(9,"Beta"),c(10,"P-value"),c(11,"Beta"),c(12,"P-value"))) {
    writeData(wb, ws, item[2], startCol = as.integer(item[1]), startRow = 4)
  }
  row <- 5
  for (sc in scales) {
    prefix <- sc[1]; display_name <- sc[2]
    writeData(wb, ws, display_name,                     startCol = 1, startRow = row)
    writeData(wb, ws, get_n(cbcl_raw, prefix, "1cbcl"), startCol = 2, startRow = row)
    writeData(wb, ws, get_n(ysr_raw,  prefix, "1ysr"),  startCol = 8, startRow = row)
    addStyle(wb, ws, top_sty, rows = row, cols = 1:12, gridExpand = TRUE)
    row <- row + 1
    m1c <- get_vals(cbcl_raw, prefix, "1cbcl", "pgi_EA4:female_ctd")
    m2c <- get_vals(cbcl_raw, prefix, "2cbcl", "pgi_EA4:female_ctd")
    m1y <- get_vals(ysr_raw,  prefix, "1ysr",  "pgi_EA4:female_ctd")
    m2y <- get_vals(ysr_raw,  prefix, "2ysr",  "pgi_EA4:female_ctd")
    writeData(wb, ws, "PGI Offspring EA", startCol=1,  startRow=row)
    write_val(wb, ws, m1c$estimate, m1c$p.value, 3, row)
    write_pval(wb, ws, m1c$p.value, 4, row)
    write_val(wb, ws, m2c$estimate, m2c$p.value, 5, row)
    write_pval(wb, ws, m2c$p.value, 6, row)
    write_val(wb, ws, m1y$estimate, m1y$p.value, 9, row)
    write_pval(wb, ws, m1y$p.value, 10, row)
    write_val(wb, ws, m2y$estimate, m2y$p.value, 11, row)
    write_pval(wb, ws, m2y$p.value, 12, row)
    row <- row + 1
    p2c <- get_vals(cbcl_raw, prefix, "2cbcl", "pgi_sum_EA4:female_ctd")
    p2y <- get_vals(ysr_raw,  prefix, "2ysr",  "pgi_sum_EA4:female_ctd")
    writeData(wb, ws, "PGI Parental EA", startCol=1,  startRow=row)
    write_val(wb, ws, p2c$estimate, p2c$p.value, 5, row)
    write_pval(wb, ws, p2c$p.value, 6, row)
    write_val(wb, ws, p2y$estimate, p2y$p.value, 11, row)
    write_pval(wb, ws, p2y$p.value, 12, row)
    addStyle(wb, ws, bot_sty, rows = row, cols = 2:12, gridExpand = TRUE, stack = TRUE)
    row <- row + 1
  }
  setColWidths(wb, ws, cols = 1, widths = 26)
}

write_interaction_decomposed <- function(wb, ws, cbcl_raw, ysr_raw) {
  writeData(wb, ws, "Sex interaction with", startCol = 1, startRow = 4)
  for (item in list(c(2,"N"),c(3,"Beta"),c(4,"P-value"),c(5,"Beta"),c(6,"P-value"),
                    c(8,"N"),c(9,"Beta"),c(10,"P-value"),c(11,"Beta"),c(12,"P-value"))) {
    writeData(wb, ws, item[2], startCol = as.integer(item[1]), startRow = 4)
  }
  row <- 5
  for (sc in scales) {
    prefix <- sc[1]; display_name <- sc[2]
    writeData(wb, ws, display_name,                     startCol = 1, startRow = row)
    writeData(wb, ws, get_n(cbcl_raw, prefix, "1cbcl"), startCol = 2, startRow = row)
    writeData(wb, ws, get_n(ysr_raw,  prefix, "1ysr"),  startCol = 8, startRow = row)
    row <- row + 1
    for (cog in c("NonCog", "Cog")) {
      label <- if (cog == "NonCog") "PGI Offspring EA NonCognitive" else "PGI Offspring EA Cognitive"
      term  <- paste0("pgi_EA3", cog, ":female_ctd")
      m1c <- get_vals(cbcl_raw, prefix, "1cbcl", term)
      m2c <- get_vals(cbcl_raw, prefix, "2cbcl", term)
      m1y <- get_vals(ysr_raw,  prefix, "1ysr",  term)
      m2y <- get_vals(ysr_raw,  prefix, "2ysr",  term)
      writeData(wb, ws, label,        startCol=1,  startRow=row)
      write_val(wb, ws, m1c$estimate, m1c$p.value, 3, row)
      write_pval(wb, ws, m1c$p.value, 4, row)
      write_val(wb, ws, m2c$estimate, m2c$p.value, 5, row)
      write_pval(wb, ws, m2c$p.value, 6, row)
      write_val(wb, ws, m1y$estimate, m1y$p.value, 9, row)
      write_pval(wb, ws, m1y$p.value, 10, row)
      write_val(wb, ws, m2y$estimate, m2y$p.value, 11, row)
      write_pval(wb, ws, m2y$p.value, 12, row)
      row <- row + 1
    }
    for (cog in c("NonCog", "Cog")) {
      label <- if (cog == "NonCog") "PGI Parental EA NonCognitive" else "PGI Parental EA Cognitive"
      term  <- paste0("pgi_sum_EA3", cog, ":female_ctd")
      p2c <- get_vals(cbcl_raw, prefix, "2cbcl", term)
      p2y <- get_vals(ysr_raw,  prefix, "2ysr",  term)
      writeData(wb, ws, label,        startCol=1,  startRow=row)
      writeData(wb, ws, "",           startCol=3,  startRow=row)
      writeData(wb, ws, "",           startCol=4,  startRow=row)
      write_val(wb, ws, p2c$estimate, p2c$p.value, 5, row)
      write_pval(wb, ws, p2c$p.value, 6, row)
      writeData(wb, ws, "",           startCol=9,  startRow=row)
      writeData(wb, ws, "",           startCol=10, startRow=row)
      write_val(wb, ws, p2y$estimate, p2y$p.value, 11, row)
      write_pval(wb, ws, p2y$p.value, 12, row)
      if (cog == "Cog") addStyle(wb, ws, bot_sty, rows = row, cols = 1:12, gridExpand = TRUE, stack = TRUE)
      row <- row + 1
    }
  }
  setColWidths(wb, ws, cols = 1, widths = 26)
}

# ════════════════════════════════════════════════════════════════════════════════
# SHEETS 1-8
# ════════════════════════════════════════════════════════════════════════════════
build_t1 <- function(wb, cbcl_raw, ysr_raw) {
  addWorksheet(wb, "t1.EAfull")
  write_top_rows(wb, "t1.EAfull", "CBCL", "YSR", "Table 1.",
                 paste("Comparison of the estimated effects of full EA offspring and parental polygenic indices on",
                       "Behavioral scales across CBCL and YSR samples. All regressions include a set of controls.",
                       "P-values are based on family clustered standard errors."))
  write_full_ea(wb, "t1.EAfull", cbcl_raw, ysr_raw, c("1cbcl","2cbcl"), c("1ysr","2ysr"))
}

build_t2 <- function(wb, cbcl_raw, ysr_raw) {
  addWorksheet(wb, "t2.EAdecomposed")
  write_top_rows(wb, "t2.EAdecomposed", "CBCL", "YSR", "Table 2.",
                 paste("Comparison of the estimated effects of decomposed EA offspring and parental polygenic indices on",
                       "Behavioral scales across CBCL and YSR samples. All regressions include a set of controls.",
                       "P-values are based on family clustered standard errors."))
  write_decomposed_ea(wb, "t2.EAdecomposed", cbcl_raw, ysr_raw, c("1cbcl","2cbcl"), c("1ysr","2ysr"))
}

build_t3 <- function(wb, girls_raw, boys_raw) {
  addWorksheet(wb, "t3.cbcl_EAfull_sex")
  write_top_rows(wb, "t3.cbcl_EAfull_sex", "Girls", "Boys", "Table 3.",
                 paste("Comparison of the estimated effects of full EA offspring and parental polygenic indices on",
                       "Behavioral scales by sex in the parental report sample (CBCL). All regressions include a set of controls.",
                       "P-values are based on family clustered standard errors."))
  write_full_ea(wb, "t3.cbcl_EAfull_sex", girls_raw, boys_raw,
                c("1girlscbcl","2girlscbcl"), c("1boyscbcl","2boyscbcl"))
}

build_t4 <- function(wb, girls_raw, boys_raw) {
  addWorksheet(wb, "t4.cbcl_EAdecomposed_sex")
  write_top_rows(wb, "t4.cbcl_EAdecomposed_sex", "Girls", "Boys", "Table 4.",
                 paste("Comparison of the estimated effects of decomposed EA offspring and parental polygenic indices on",
                       "Behavioral scales by sex in the parental report sample (CBCL). All regressions include a set of controls.",
                       "P-values are based on family clustered standard errors."))
  write_decomposed_ea(wb, "t4.cbcl_EAdecomposed_sex", girls_raw, boys_raw,
                      c("1girlscbcl","2girlscbcl"), c("1boyscbcl","2boyscbcl"))
}

build_t5 <- function(wb, girls_raw, boys_raw) {
  addWorksheet(wb, "t5.ysr_EAfull_sex")
  write_top_rows(wb, "t5.ysr_EAfull_sex", "Girls", "Boys", "Table 5.",
                 paste("Comparison of the estimated effects of full EA offspring and parental polygenic indices on",
                       "Behavioral scales by sex in the self-reported sample (YSR). All regressions include a set of controls.",
                       "P-values are based on family clustered standard errors."))
  write_full_ea(wb, "t5.ysr_EAfull_sex", girls_raw, boys_raw,
                c("1girlsysr","2girlsysr"), c("1boysysr","2boysysr"))
}

build_t6 <- function(wb, girls_raw, boys_raw) {
  addWorksheet(wb, "t6.ysr_EAdecomposed_sex")
  write_top_rows(wb, "t6.ysr_EAdecomposed_sex", "Girls", "Boys", "Table 6.",
                 paste("Comparison of the estimated effects of decomposed EA offspring and parental polygenic indices on",
                       "Behavioral scales by sex in the self-reported sample (YSR). All regressions include a set of controls.",
                       "P-values are based on family clustered standard errors."))
  write_decomposed_ea(wb, "t6.ysr_EAdecomposed_sex", girls_raw, boys_raw,
                      c("1girlsysr","2girlsysr"), c("1boysysr","2boysysr"))
}

build_t7 <- function(wb, cbcl_raw, ysr_raw) {
  addWorksheet(wb, "t7.EAfull_sex_interaction")
  ws <- "t7.EAfull_sex_interaction"
  writeData(wb, ws, "Table 7.", startCol = 1, startRow = 1)
  writeData(wb, ws, paste(
    "Comparison of the estimated sex-interaction effects of full EA offspring and parental polygenic indices on",
    "Behavioral scales across CBCL and YSR samples. All regressions include a set of controls.",
    "P-values are based on family clustered standard errors."
  ), startCol = 2, startRow = 1)
  writeData(wb, ws, "CBCL", startCol = 2, startRow = 2)
  writeData(wb, ws, "YSR",  startCol = 8, startRow = 2)
  mergeCells(wb, ws, cols = 2:6,  rows = 2)
  mergeCells(wb, ws, cols = 8:12, rows = 2)
  addStyle(wb, ws, createStyle(border = "TopBottom", borderStyle = "thin", halign = "center"),
           rows = 2, cols = 2:12, gridExpand = TRUE)
  writeData(wb, ws, "Behavioral scale", startCol = 1, startRow = 3)
  writeData(wb, ws, "Model 1", startCol = 3,  startRow = 3)
  writeData(wb, ws, "Model 2", startCol = 5,  startRow = 3)
  writeData(wb, ws, "Model 1", startCol = 9,  startRow = 3)
  writeData(wb, ws, "Model 2", startCol = 11, startRow = 3)
  mergeCells(wb, ws, cols = 3:4,   rows = 3)
  mergeCells(wb, ws, cols = 5:6,   rows = 3)
  mergeCells(wb, ws, cols = 9:10,  rows = 3)
  mergeCells(wb, ws, cols = 11:12, rows = 3)
  addStyle(wb, ws, createStyle(border = "TopBottom", borderStyle = "thin", halign = "center"),
           rows = 3, cols = 2:12, gridExpand = TRUE)
  write_interaction_full(wb, ws, cbcl_raw, ysr_raw)
}

build_t8 <- function(wb, cbcl_raw, ysr_raw) {
  addWorksheet(wb, "t8.EAdecomposed_sex_interaction")
  ws <- "t8.EAdecomposed_sex_interaction"
  writeData(wb, ws, "Table 8.", startCol = 1, startRow = 1)
  writeData(wb, ws, paste(
    "Comparison of the estimated sex-interaction effects of decomposed EA offspring and parental polygenic indices on",
    "Behavioral scales across CBCL and YSR samples. All regressions include a set of controls.",
    "P-values are based on family clustered standard errors."
  ), startCol = 2, startRow = 1)
  writeData(wb, ws, "CBCL", startCol = 2, startRow = 2)
  writeData(wb, ws, "YSR",  startCol = 8, startRow = 2)
  mergeCells(wb, ws, cols = 2:6,  rows = 2)
  mergeCells(wb, ws, cols = 8:12, rows = 2)
  addStyle(wb, ws, createStyle(border = "TopBottom", borderStyle = "thin", halign = "center"),
           rows = 2, cols = 2:12, gridExpand = TRUE)
  writeData(wb, ws, "Behavioral scale", startCol = 1, startRow = 3)
  writeData(wb, ws, "Model 1", startCol = 3,  startRow = 3)
  writeData(wb, ws, "Model 2", startCol = 5,  startRow = 3)
  writeData(wb, ws, "Model 1", startCol = 9,  startRow = 3)
  writeData(wb, ws, "Model 2", startCol = 11, startRow = 3)
  mergeCells(wb, ws, cols = 3:4,   rows = 3)
  mergeCells(wb, ws, cols = 5:6,   rows = 3)
  mergeCells(wb, ws, cols = 9:10,  rows = 3)
  mergeCells(wb, ws, cols = 11:12, rows = 3)
  addStyle(wb, ws, createStyle(border = "TopBottom", borderStyle = "thin", halign = "center"),
           rows = 3, cols = 2:12, gridExpand = TRUE)
  write_interaction_decomposed(wb, ws, cbcl_raw, ysr_raw)
}


# ════════════════════════════════════════════════════════════════════════════════
# SHEET 9: t9.wald  —  Wald test CBCL vs YSR
# ════════════════════════════════════════════════════════════════════════════════
build_t9 <- function(wb, wald_ea4_file, wald_ea3_file) {
  addWorksheet(wb, "t9.wald")
  ws <- "t9.wald"
  
  df4 <- read.xlsx(wald_ea4_file, sheet = "CBCL_vs_YSR", colNames = TRUE)
  df3 <- read.xlsx(wald_ea3_file, sheet = "CBCL_vs_YSR", colNames = TRUE)
  
  scale_order <- data.frame(
    outcome = c("aggressive_std","attention_std","delinquent_std","anxious_std",
                "somatic_std","withdrawn_std","social_std","thought_std",
                "externalising_std","internalising_std"),
    label = c("Aggressive behavior","Attention problems","Rule-breaking behavior",
              "Anxious","Somatic complaints","Withdrawn","Social problems",
              "Thought problems","Externalizing problem behavior",
              "Internalizing problem behavior"),
    stringsAsFactors = FALSE
  )
  
  get_wald <- function(df, outcome, model, predictor) {
    row <- df[df$outcome == outcome & df$model == model & df$predictor == predictor, ]
    if (nrow(row) == 0) return(list(diff = NA, p_value = NA))
    list(diff = as.numeric(row$diff[1]), p_value = as.numeric(row$p_value[1]))
  }
  
  tb_sty <- createStyle(border = "TopBottom", borderStyle = "thin")
  top_s  <- createStyle(border = "Top",    borderStyle = "thin")
  bot_s  <- createStyle(border = "Bottom", borderStyle = "thin")
  
  writeData(wb, ws, "Table 9.", startCol = 1, startRow = 1)
  writeData(wb, ws, paste(
    "Comparison of estimated coefficients across CBCL and YSR samples using Wald tests.",
    "All regressions include a set of controls. P-values are based on family clustered standard errors."
  ), startCol = 2, startRow = 1)
  
  writeData(wb, ws, "Behavioral Scale",             startCol = 1, startRow = 2)
  writeData(wb, ws, "Model",                        startCol = 2, startRow = 2)
  writeData(wb, ws, "Value",                        startCol = 3, startRow = 2)
  writeData(wb, ws, "PGI Offspring EA",             startCol = 4, startRow = 2)
  writeData(wb, ws, "PGI Offspring EA NonCognitive", startCol = 5, startRow = 2)
  writeData(wb, ws, "PGI Offspring EA Cognitive",   startCol = 6, startRow = 2)
  addStyle(wb, ws, tb_sty, rows = 2, cols = 1:6, gridExpand = TRUE)
  
  row <- 3
  for (i in seq_len(nrow(scale_order))) {
    outcome      <- scale_order$outcome[i]
    display_name <- scale_order$label[i]
    scale_start  <- row
    
    for (model in c("M1", "M2")) {
      model_start <- row
      
      ea4_d  <- get_wald(df4, outcome, model, "pgi_EA4")
      ea3_nc <- get_wald(df3, outcome, model, "pgi_EA3NonCog")
      ea3_c  <- get_wald(df3, outcome, model, "pgi_EA3Cog")
      
      writeData(wb, ws, model,                   startCol = 2, startRow = row)
      writeData(wb, ws, "Difference (CBCL-YSR)", startCol = 3, startRow = row)
      write_val(wb, ws, ea4_d$diff,  ea4_d$p_value,  4, row)
      write_val(wb, ws, ea3_nc$diff, ea3_nc$p_value, 5, row)
      write_val(wb, ws, ea3_c$diff,  ea3_c$p_value,  6, row)
      addStyle(wb, ws, top_s, rows = row, cols = 1:6, gridExpand = TRUE, stack = TRUE)
      row <- row + 1
      
      writeData(wb, ws, "P-value",       startCol = 3, startRow = row)
      write_pval(wb, ws, ea4_d$p_value,  4, row)
      write_pval(wb, ws, ea3_nc$p_value, 5, row)
      write_pval(wb, ws, ea3_c$p_value,  6, row)
      addStyle(wb, ws, bot_s, rows = row, cols = 1:6, gridExpand = TRUE, stack = TRUE)
      row <- row + 1
      
      mergeCells(wb, ws, cols = 2, rows = model_start:(row - 1))
    }
    
    writeData(wb, ws, display_name, startCol = 1, startRow = scale_start)
    mergeCells(wb, ws, cols = 1, rows = scale_start:(row - 1))
  }
  
  setColWidths(wb, ws, cols = 1, widths = 14.33)
  setColWidths(wb, ws, cols = 2, widths = 7)
  setColWidths(wb, ws, cols = 3, widths = 19.33)
  setColWidths(wb, ws, cols = 4, widths = 14.44)
}

# MAIN
# ════════════════════════════════════════════════════════════════════════════════
cat("Luetaan raakadata...\n")
ea4_cbcl             <- read_raw(file.path(raw_dir, "full_cbcl_EA4.xlsx"))
ea4_ysr              <- read_raw(file.path(raw_dir, "full_ysr_EA4.xlsx"))
ea3_cbcl             <- read_raw(file.path(raw_dir, "full_cbcl_EA3.xlsx"))
ea3_ysr              <- read_raw(file.path(raw_dir, "full_ysr_EA3.xlsx"))
girls_cbcl_ea4       <- read_raw(file.path(raw_dir, "girls_cbcl_EA4.xlsx"))
boys_cbcl_ea4        <- read_raw(file.path(raw_dir, "boys_cbcl_EA4.xlsx"))
girls_cbcl_ea3       <- read_raw(file.path(raw_dir, "girls_cbcl_EA3.xlsx"))
boys_cbcl_ea3        <- read_raw(file.path(raw_dir, "boys_cbcl_EA3.xlsx"))
girls_ysr_ea4        <- read_raw(file.path(raw_dir, "girls_ysr_EA4.xlsx"))
boys_ysr_ea4         <- read_raw(file.path(raw_dir, "boys_ysr_EA4.xlsx"))
girls_ysr_ea3        <- read_raw(file.path(raw_dir, "girls_ysr_EA3.xlsx"))
boys_ysr_ea3         <- read_raw(file.path(raw_dir, "boys_ysr_EA3.xlsx"))
interaction_cbcl_ea4 <- read_raw(file.path(raw_dir, "interaction_cbcl_EA4.xlsx"))
interaction_ysr_ea4  <- read_raw(file.path(raw_dir, "interaction_ysr_EA4.xlsx"))
interaction_cbcl_ea3 <- read_raw(file.path(raw_dir, "interaction_cbcl_EA3.xlsx"))
interaction_ysr_ea3  <- read_raw(file.path(raw_dir, "interaction_ysr_EA3.xlsx"))

wb <- createWorkbook()

cat("Rakennetaan sheet 1...\n"); build_t1(wb, ea4_cbcl, ea4_ysr)
cat("Rakennetaan sheet 2...\n"); build_t2(wb, ea3_cbcl, ea3_ysr)
cat("Rakennetaan sheet 3...\n"); build_t3(wb, girls_cbcl_ea4, boys_cbcl_ea4)
cat("Rakennetaan sheet 4...\n"); build_t4(wb, girls_cbcl_ea3, boys_cbcl_ea3)
cat("Rakennetaan sheet 5...\n"); build_t5(wb, girls_ysr_ea4, boys_ysr_ea4)
cat("Rakennetaan sheet 6...\n"); build_t6(wb, girls_ysr_ea3, boys_ysr_ea3)
cat("Rakennetaan sheet 7...\n"); build_t7(wb, interaction_cbcl_ea4, interaction_ysr_ea4)
cat("Rakennetaan sheet 8...\n"); build_t8(wb, interaction_cbcl_ea3, interaction_ysr_ea3)
cat("Rakennetaan sheet 9...
"); build_t9(wb,
             file.path(raw_dir, "wald_test_results_EA4.xlsx"),
             file.path(raw_dir, "wald_test_results_EA3.xlsx"))

saveWorkbook(wb, out_file, overwrite = TRUE)
cat("Valmis! Tallennettu:", out_file, "\n")