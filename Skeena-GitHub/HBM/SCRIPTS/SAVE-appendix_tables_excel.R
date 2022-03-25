library(openxlsx)
wb <- createWorkbook()

for (run in names(app.tables)) {
  
  tab <- app.tables[[run]]
  
  addWorksheet(wb, run)
  writeData(wb, run, tab)
  
  # header
  style <- createStyle(
    border= c("Bottom", "Top"), 
    fgFill = rgb(208,208,208, maxColorValue = 255),
    textDecoration = "bold",
    wrapText = TRUE
  )  
  addStyle(wb, run, style, rows= 1, cols = 1:ncol(tab), gridExpand = TRUE)
  
  if (str_detect(run, "Shrinkage")) {
    style <- createStyle(numFmt = "0.000")
    addStyle(wb, run, style, rows= 2:(nrow(tab) + 1), cols = c(2:5), gridExpand = TRUE)
    style <- createStyle(numFmt = "0.0%")
    addStyle(wb, run, style, rows= 2:(nrow(tab) + 1), cols = c(6:7), gridExpand = TRUE)
  }
  
  if (str_detect(run, "Run")) {
    
    # Percent change column
    check1 <- run %in% c( "Run 2 Smsy", "Run 3 Smsy")
    check2 <-  str_detect(run, "Umsy")
    if (check1 | check2) {
      style <- createStyle(numFmt = "0.0%")
    } else {
      style <- createStyle(numFmt = "0%")
    }
    addStyle(wb, run, style, rows= 2:(nrow(tab) + 1), cols = c(4,7), gridExpand = TRUE)
    # Format estimates
    if (run == "Run 1" | str_detect(run, "Umsy")) {
      style <- createStyle(numFmt = "0.000")
    } else {
      style <- createStyle(numFmt = "#,##0")
    }
    addStyle(wb, run, style, rows= 2:(nrow(tab) + 1), cols = c(2:3,5:6), gridExpand = TRUE)
    
    
    setColWidths(wb, run, cols = 1:7, widths = 16.5) 
  }
  
  
}

out.file <- file.path(output.dir, paste0("appendix_results_JAGS--", ver, ".xlsx"))
saveWorkbook(wb, file = out.file, overwrite = TRUE)