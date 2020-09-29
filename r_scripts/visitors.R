library(plyr)
library(dplyr)
library(readxl)
library(tidyr)
library(reshape2)
library(stringr)
library(stringi)
library(ggplot2)
library(janitor)

setwd("/Users/mohsennazemi/Dropbox/Airport Experience Project/2020 COVID-19 Survey/")

#### read data
df_data <- read_excel("pilot/V10+Survey+3+(Visitors)_September+15,+2020_11.27.xlsx", col_names = TRUE, skip = 1) %>%
  clean_names()

df_visitors <- df_data %>%
  select(-contains("timing"))

df_timings_visitors <- df_data %>%
  select("response_id", contains("submit")) %>%
  dplyr::mutate(mean_timing = rowMeans(dplyr::select(df_timings_visitors, contains("submit")), na.rm = TRUE))

# define the plot in a function
my_plot <- function(dataframe, column) {
  distribution <- ggplot(dataframe, aes(x = column)) +
    geom_density() +
    xlab("Time (s)") +
    ylab("Density") +
    theme_bw()
  return(distribution)
}

my_plot(df_timings_visitors, df_timings_visitors$timing_page_submit_20)




