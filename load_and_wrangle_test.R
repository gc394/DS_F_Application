
library(openxlsx)
library(tibble)
library(magrittr)
library(tidyr)
library(purrr)

#### cleaning ####

# sheet 1

# create workbook object
wb = openxlsx::loadWorkbook(file = 'DataScientist_009749_Dataset.xlsx')

tst = openxlsx::readWorkbook(
  xlsxFile = wb, 
  sheet = openxlsx::sheets(wb)[1])

wrangle_tst = wrangle_dataframe(tst)
  
# find the highest n firms for a particular metric (median therefore 'consistent')
tst_ %>%
  dplyr::filter(metric == 'Total.assets.(£m)', name == 'median_value') %>%
  dplyr::group_by(firm) %>%
  dplyr::summarise(m = max(value)) %>%
  dplyr::slice_max(m, n = 3) %>%
  dplyr::pull('firm')


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
  
  # due to volatility we want to create descriptive metrics to find biggest, most volatile etc. firms
  added_metrics_output_df = output_df %>%
    # we are going to describe the whole time series so just need firm and metric
    dplyr::group_by(firm, metric) %>%
    dplyr::summarise(
      # this are the variables I will use (for now)
      mean_value = mean(value, na.rm = T),
      median_value = median(value, na.rm = T),
      sd_value = sd(value, na.rm = T),
      mad_value = mad(value, na.rm = T)
      )
  
  # add them back to the original output and then pivot longer
  # creating 'value type' variable - only value element varies temporarily
  output_df = output_df %>%
    dplyr::left_join(
      added_metrics_output_df,
      by = c('firm', 'metric')) %>%
    tidyr::pivot_longer(
      cols = contains('value'), 
      names_to = 'value_type'
    ) %>%
    dplyr::mutate(
      # renaming value to time_series_value for ease
      value_type = case_match(value_type, 
                              'value' ~ 'time_series_value', 
                              .default = value_type))
    
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
  dplyr::arrange(firm, metric, year, value_type) %>%
  tidyr::pivot_wider(
    names_from = metric, 
    values_from = value)
