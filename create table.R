
# decide the variables by most variance in median value?


# we want to find the top sd firms for all size metrics and make a table for the top 15/10%
tst = df %>%
  # take the mean value
  dplyr::filter(value_type == 'median_value') %>%
  dplyr::select(-year, -value_type) %>%
  # remove duplications from value_types
  dplyr::distinct() %>%
  # find standard deviation for each firm 
  dplyr::mutate(
    across(
      .cols = where(is.numeric),
      # taking the log of the absolute standard deviation to try and normalise large values and
      # protect weightings
      .fns = ~ log(abs((.x - mean(.x, na.rm = T)) / mean(.x, na.rm = T))), 
      .names = '{.col}_sd')
    ) %>% 
  rowwise() %>%
  dplyr::mutate(size_median_sd = median(nwp_m_sd, equity_m_sd, total_assets_m_sd, scr_m_sd, net_combined_ratio_sd, na.rm= T)) %>%
  dplyr::arrange(desc(size_median_sd)) %>%
  utils::head(10) %>%
  dplyr::select(firm, nwp_m_sd, equity_m_sd, total_assets_m_sd, scr_m_sd, net_combined_ratio_sd)

?head
