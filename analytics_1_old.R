library(readxl)
library(dplyr)
library(tidyr)
library(magrittr)
library(purrr)
library(stringr)
library(tibble)

col_types <- readxl:::xlsx_col_types("../catalys/merged_master_families_matrix_ver_0_13.xlsx")
mmfm <- read_excel("../catalys/merged_master_families_matrix_ver_0_13.xlsx", sheet = "merged_master_families_matrix", col_types=col_types)

#as.character(as.POSIXct(as.numeric(mmfm[["Priority date"]]) * (60*60*24), origin="1899-12-30", tz="GMT"), format = "%Y%m%d")

#mmfm[nchar(mmfm[["Priority date"]]) == 5, "Priority date"]

# test -------------------------------------------------------------------

## test1 ---

### sec ----


mmfm[nchar(mmfm[["Priority date"]]) == 5, "Priority date"] <- 
  as.character(as.POSIXct(as.numeric(mmfm[nchar(mmfm[["Priority date"]]) == 5, "Priority date"]) * (60*60*24)
    , origin="1899-12-30"
    , tz="GMT"), format = "%Y%m%d")

