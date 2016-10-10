library(readxl)

load_excel_sheet <- function(excel_file_name, excel_sheet_name)
{
  
  sheets <- readxl::excel_sheets(excel_file_name)
  
  sheet_num <- match(excel_sheet_name, sheets) - 1
  col_types <- readxl:::xlsx_col_types(excel_file_name, 
                                       sheet = sheet_num)
  df <- read_excel(excel_file_name, 
                   sheet = excel_sheet_name, 
                   col_types = rep("text", length(col_types))) %>%
        repair_names(prefix = "repaired_", sep = "") %>%
        select(-starts_with("repaired_"))
}