library(plyr)
library(dplyr)

## TODO: tables can have different columns in different periods

readSubjectsTables <- function (filename, encoding="latin1") {
  con <- file(filename, encoding=encoding)
  lines <- readLines(con)
  close(con)
  
  cells <- laply (lines, strsplit, "\t", fixed = TRUE)

  getColumn <- function (col, table, func = identity) {
    func (laply (table, function(x) x[col]))
  }

  tryAsNumeric <- function (x) {
    is.na(x) <- which(x == "-")
    tryCatch(as.numeric(x), warning = function(w) x)  
  }

  ## Treatment column
  treatCol <- as.integer(getColumn(2, cells))
  treats <- unique(treatCol)

  ## Table column
  tableCol <- as.factor (getColumn(3, cells))

  extractTable <- function (treatment, type) {
    rows <- cells[treatCol == treatment & tableCol == type]
    headRowsIdx <- which(getColumn(4, rows) == "Period")
    header <- rows[[headRowsIdx[1]]]
    header[1:3] <- c("Time", "Treatment", "Table")
    rows <- rows[-headRowsIdx]
    df <- llply (1:length(header), getColumn, rows, tryAsNumeric)
    as.data.frame (setNames(df, header), stringsAsFactors = FALSE)
  }
  
  setNames(llply(treats, function(x) extractTable(x, "subjects")),
           paste("T", treats, sep = ""))
}

summTable <- function(df, vname, 
                      labels = c("Mean", "SD", 
                                 "Min", "25%", "Median", "75%", "Max")) {
  x <- df[[vname]]
  setNames (c(mean(x), sd(x), quantile(x)), labels)
}



