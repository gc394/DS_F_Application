
source('load.R')
library(ggplot2)
library(ggthemes)

ggplot2::ggplot(
  data = output_df %>% 
    dplyr::filter(metric == 'NWP.(£m)', 
                  firm %in% c('Firm 1', 'Firm 2', 'Firm 3')),
  mapping = aes(
    x = .data[['year']], 
    y = .data[['value']], 
    group = firm, 
    col = firm)
) + 
  geom_line()
