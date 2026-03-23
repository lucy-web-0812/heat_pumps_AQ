# function for importing GOV.UK stats files 

library(janitor)
library(tidyverse)


# Need to make this work for files that do not have spiel at the top! 

spiel_remover <- function(file, sheet = NULL)  {
  
  file_ext <- tools::file_ext(file) # get the file type by the extension 
  
  
  if (file_ext == "csv"){
    raw_data <- readr::read_csv(file)
    
  } else if(file_ext %in% c("xls", "xlsx")) {
    
    raw_data <- readxl::read_xlsx(file, sheet) 
  } else {
    print("File type not supported. Use .csv, .xls, .xlsx instead.")
  }
  
  cleaned_data <- raw_data |> 
    dplyr::select(dplyr::where(~!all(is.na(.x)))) # removing all of the columns where everything is NA
  
   first_real_row_index <- which(apply(cleaned_data, 1, function(row) all(!is.na(row))))[1]
  
    #dplyr::filter(dplyr::if_all(everything(), ~ !is.na(.x))) # This does not quite work as getting rid of any rows that have NA values. 
  
  
  colnames(cleaned_data) <- cleaned_data[first_real_row_index,]
  
  
  cleaned_data <- cleaned_data[(first_real_row_index + 1):nrow(cleaned_data), ] |> 
    janitor::clean_names()
  
  return(cleaned_data)
  
  
}




