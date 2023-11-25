
source('load.R')
library(ggplot2)
library(ggthemes)
library(ggrepel)

# size charts

metric = 'total_assets_m'
type = 'median_value'
n = 5



# find biggest median asset firms
grouped_df = df %>%
  dplyr::filter(value_type == type) %>%
  dplyr::select(all_of(c('firm', metric))) %>%
  dplyr::group_by(firm) %>%
  dplyr::summarise(m = max(.data[[metric]]))

# find the top 3 firms
topn_firms = grouped_df %>%
  dplyr::slice_max(m, n = n) %>%
  dplyr::pull('firm')

# find the top 20pct of firms
top20pct_firms = grouped_df %>%
  dplyr::slice_max(m, prop = -.80) %>%
  dplyr::pull('firm')

# produce color vectors for the top three firms
# this gives the colour values of the vector
cols = c(ggthemes::wsj_pal()(length(topn_firms)), 
         rep('black', length(unique(df$firm)) - length(topn_firms)))

# by then naming the vector we can set the color
names(cols) = c(topn_firms, unique(df$firm)[!unique(df$firm) %in% topn_firms])

# graph
ggplot2::ggplot(
  data = df %>%
    # helps to read and debug having a smaller dataset
    dplyr::select(all_of(c('firm', 'year', 'value_type', metric))) %>%
    # we want to use the time series value as this one changes over time and we only want top 20%
    # of firms to compare this to
    dplyr::filter(value_type == 'time_series_value', firm %in% top20pct_firms) %>%
    dplyr::mutate(
      alpha = case_when(
        firm %in% topn_firms ~ 1,
        .default = 0.9
        ),
      # width = case_when(
      #   firm %in% topn_firms ~ 0.00021,
      #   .default = 0.0002
      # ),
      text = case_when(
        firm %in% topn_firms & year == '2020' ~ firm,
        .default = ''
        )
    ),
  mapping = aes(
    x = year, 
    y = .data[[metric]], 
    # linewidth = width,
    group = firm,
    alpha = alpha,
    color = firm,
    label = text)
)  +
  # ggthemes::theme_fivethirtyeight() +
  geom_line() + 
  geom_label_repel(aes(label = text),
                   nudge_x = 1,
                   na.rm = TRUE) +
  scale_color_manual(
    values = cols
  ) + 
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_short_scale(),
                                  accuracy = 1)) + 
  theme(legend.position = "none") +
  labs(
    title = glue::glue('{metric} over time'),
    subtitle = glue::glue('3 largest medians highlighted'),
    x = 'Year End',
    y = case_when(
      grepl(pattern = '_m$', x = metric) ~ 'GBP (M)',
      grepl(pattern = 'ratio$', x = metric) ~ 'Ratio'
    )
    ) +
  # xlab('Year End') +
  ylab(case_when(
    grepl(pattern = '_m$', x = metric) ~ 'GBP (M)',
    grepl(pattern = 'ratio$', x = metric) ~ 'Ratio'
  ))

df_ = df %>%
  dplyr::filter(value_type == 'time_series_value') %>%
  tidyr::pivot_longer(
    cols = -c('firm', 'year', 'value_type')
    ) %>%
  dplyr::group_by(year, name) %>%
  dplyr::summarise(
    rsd = sd(value, na.rm = T) * 100 / mean(value, na.rm = T)
  )

top3 = df_ %>%
  dplyr::group_by(year, name) %>%
  dplyr::summarise(rsd = max(rsd)) %>%
  dplyr::slice_max(rsd, n = 3) %>%
  dplyr::mutate(text = name) %>%
  dplyr::select(-rsd)

ggplot(
  df_ %>%
    dplyr::left_join(
      top3, by = c('year', 'name')
    ), 
  aes(x = fct_reorder(name, rsd),
      y = rsd,
      fill = name,
      text = text)) + 
  geom_col() + 
  facet_wrap(~year, ncol = 2) + 
  geom_label_repel(
    aes(label = text),
    nudge_x = 1,
    na.rm = TRUE) + 
  theme(legend.position = "none", axis.text.x = element_blank()) +
  labs(
    title = glue::glue('RSD of Variables'),
    subtitle = glue::glue('Split by year'),
    x = 'Variables',
    y = 'Relative Standard Deviation'
    )


df %>%
  dplyr::filter(value_type == 'time_series_value') %>%
  dplyr::select(all_of(c('year', 'firm', 'net_expense_ratio', 'net_combined_ratio', 'pure_net_claims_ratio'))) %>%
  tidyr::pivot_longer(
    cols = -c('firm', 'year')
  ) %>%
  #pivot long then merge 
  dplyr::left_join(rasd_metrics_df,
                   by = c('year', 'name')) %>%
  dplyr::group_by(firm, year, name) %>%
  dplyr::summarise(
    val = value,
    mean = mean,
    number = number,
    sd = sqrt((value - mean)^2 / number)
  )

expense_ratio = df %>% 
  dplyr::filter(value_type == 'median_value') %>% 
  dplyr::pull('net_expense_ratio') 

upper_limit = quantile(expense_ratio, 0.75, na.rm = T) + (1.5 * IQR(expense_ratio, na.rm = T))

firms = df %>%
  dplyr::filter(value_type == 'time_series_value' &
                net_expense_ratio < upper_limit &
                net_expense_ratio > 0) %>%
  group_by(firm) %>%
  dplyr::filter(n() == 5) %>%
  dplyr::pull(firm)

ts= df %>%
  dplyr::filter(firm %in% firms)








