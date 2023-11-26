
lag = df %>%
  dplyr::filter(value_type == 'time_series_value') %>%
  dplyr::group_by(firm) %>%
  dplyr::mutate(across(
    .cols = where(is.numeric),
    # find the % change
    .fns = ~ (.x - lag(.x)) / lag(.x), 
    .names = '{.col}_pct_change')
  )

# find outliers that are outsider the
outliers = df %>%
  dplyr::filter(value_type == 'time_series_value') %>%
  dplyr::group_by(firm, .drop = F) %>%
  dplyr::summarise(
  across(
    .cols = where(is.numeric),
    # find the % change
    .fns = ~ case_when(
      .x < quantile(.x, 0.25, na.rm = T) - (1.5 * IQR(.x, na.rm = T)) |
      .x > quantile(.x, 0.75, na.rm = T) + (1.5 * IQR(.x, na.rm = T)) ~ 'IQR Outlier',
      .x < quantile(.x, 0.1, na.rm = T) |
      .x > quantile(.x, 0.9, na.rm = T) ~ 'Decile Outlier',
      
      .default = 'No Flag'
      ),
    .names = '{.col}_outlier_flag'
    )
  )

outliers$year = df[df$value_type == 'time_series_value',]$year

# i look at each time series for each firm and using IQR I decide whether the value presented
# is an outlier

outliers$year = df %>%
  dplyr::filter(value_type == 'time_series_value') %>%
  dplyr::pull('year')

df %>%
  dplyr::filter(value_type == 'time_series_value') %>%
  dplyr::group_by(year, .drop = F) %>%
  dplyr::summarise(
    across(
      .cols = where(is.numeric),
      # find the % change
      .fns = ~ quantile(.x, 0.25, na.rm = T) - (1.5 * IQR(.x, na.rm = T)),
      .names = '{.col}_iqr_lt'
      ),
    across(
      .cols = where(is.numeric),
      # find the % change
      .fns = ~ quantile(.x, 0.75, na.rm = T) + (1.5 * IQR(.x, na.rm = T)),
      .names = '{.col}_iqr_ut'
    )) %>%
  tidyr::pivot_longer(
    cols = -year,
    names_to = 'metric',
    values_to = 'threshold'
  )

thresholds_df = list(
  # IQR Upper
  df %>%
    dplyr::filter(value_type == 'time_series_value') %>%
    dplyr::group_by(year, .drop = F) %>%
    dplyr::summarise(
      across(
        .cols = where(is.numeric),
        # find the % change
        .fns = ~ quantile(.x, 0.25, na.rm = T) - (1.5 * IQR(.x, na.rm = T))
        )
      ) %>%
    tidyr::pivot_longer(
      cols = -year,
      names_to = 'metric',
      values_to = 'iqr_lt'
    ),
  # IQR Lower
  df %>%
    dplyr::filter(value_type == 'time_series_value') %>%
    dplyr::group_by(year, .drop = F) %>%
    dplyr::summarise(
      across(
        .cols = where(is.numeric),
        # find the % change
        .fns = ~ quantile(.x, 0.75, na.rm = T) + (1.5 * IQR(.x, na.rm = T))
      )
    ) %>%
    tidyr::pivot_longer(
      cols = -year,
      names_to = 'metric',
      values_to = 'iqr_ut'
    ),
  # Decile Upper
  df %>%
    dplyr::filter(value_type == 'time_series_value') %>%
    dplyr::group_by(year, .drop = F) %>%
    dplyr::summarise(
      across(
        .cols = where(is.numeric),
        # find the % change
        .fns = ~ quantile(.x, 0.8, na.rm = T)
      )
    ) %>%
    tidyr::pivot_longer(
      cols = -year,
      names_to = 'metric',
      values_to = 'd_ut'
    ),
  # Decile Lower
  df %>%
    dplyr::filter(value_type == 'time_series_value') %>%
    dplyr::group_by(year, .drop = F) %>%
    dplyr::summarise(
      across(
        .cols = where(is.numeric),
        # find the % change
        .fns = ~ quantile(.x, 0.2, na.rm = T)
      )
    ) %>%
    tidyr::pivot_longer(
      cols = -year,
      names_to = 'metric',
      values_to = 'd_lt'
    )
  ) %>% 
  purrr::reduce(
    left_join, 
    by = c('year', 'metric'))


outliers_df = df %>%
  dplyr::filter(value_type == 'time_series_value')%>%
  tidyr::pivot_longer(
    cols = -c('year', 'firm', 'value_type'),
    names_to = 'metric',
    values_to = 'value'
  ) %>%
  dplyr::left_join(
    thresholds_df,
    by = c('year', 'metric')
  ) %>%
  dplyr::mutate(
    flag = case_when(
      value > iqr_ut | value < iqr_lt ~ 'IQR Outlier',
      value > d_ut | value < d_lt ~ 'Decile Outlier',
      .default = 'No Flag'
    )
  ) %>%
  dplyr::select(firm, year, metric, flag) %>%
  tidyr::pivot_wider(
    names_from = 'metric',
    values_from = 'flag'
  )

filtered_df = anti_join(
  x = df %>%
    filter(value_type == 'time_series_value') %>%
    pivot_longer(cols = -c('firm', 'year', 'value_type')),
  y = outliers_df %>%
    pivot_longer(cols = -c('firm', 'year')) %>%
    filter(value == 'IQR Outlier'),
  by = c('firm', 'year')
  ) %>%
  # we are going to describe the whole time series so just need firm and metric
  dplyr::group_by(firm, name) %>%
  dplyr::summarise(
    # this are the variables I will use (for now)
    median_value = median(value, na.rm = T),
    # mad_value = mad(value, na.rm = T)
  )
  
  




