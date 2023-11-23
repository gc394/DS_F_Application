
library(openxlsx)
library(tibble)
library(magrittr)
library(tidyr)
library(purrr)

#### cleaning ####

# sheet 1

# create workbook object
wb = openxlsx::loadWorkbook(file = 'DataScientist_009749_Dataset.xlsx')

# both sheets are formatted the same way so we want to
# build a function to stop code repetition!
wrangle_dataframe = function(df){
  
  # take the date vector, remove the first item which is NA (missing firm) to re-add in
  dates_vec = unname(df[1, 2:ncol(df)]) %>% 
    base::as.character() %>%
    gsub(pattern = '[A-z]', replacement = '', x = .)
  
  output_df = df %>%
    # pivot the metrics into a column
    tidyr::pivot_longer(cols = -X1) %>%
    # removes the first row
    dplyr::filter(!is.na(X1)) %>%
    # add in year from the general dates vector created earlier
    # we need to repeat this for each firm 
    dplyr::mutate(year = rep(dates_vec, times = length(unique(.$X1))),
                  value = as.numeric(value)) %>%
    # rename and make easier for me to read
    dplyr::select(firm = X1, metric = name, year, value)
  
  return(output_df)
  
}

# read, wrangle and combine datasets
df = purrr::map(
  .x = openxlsx::sheets(wb),
  .f = function(x){
    
    # read data in
    df = openxlsx::readWorkbook(
      xlsxFile = wb, 
      sheet = x) %>%
      # wrangle using function defined above
      wrangle_dataframe()
    
  }) %>%
  # purrr recommends this approach over map_dfr due to edge cases from dplyr::bind_rows()
  purrr::list_rbind() %>%
  # just to make it easier to read for me - again.
  dplyr::arrange(firm, metric, year) %>%
  tidyr::pivot_wider(
    names_from = metric, 
    values_from = value)
