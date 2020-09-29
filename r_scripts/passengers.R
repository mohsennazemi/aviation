library(plyr)
library(dplyr)
library(readxl)
library(tidyr)
library(reshape2)
library(stringr)
library(stringi)
library(ggplot2)
library(janitor)
library(ggrepel)

setwd("/Users/mohsennazemi/Dropbox/Airport Experience Project/2020 COVID-19 Survey/")

#### read data
df_data <- read_excel("pilot/V10+Survey+1+(Pax)_September+14,+2020_11.55.xlsx", col_names = TRUE, skip = 1) %>%
  clean_names()

df_passengers <- df_data %>%
  select(-contains("timing"))

df_timings_passengers <- df_data %>%
  select("response_id", contains("submit")) %>%
  dplyr::mutate(mean_timing = rowMeans(dplyr::select(df_timings_passengers, contains("submit")), na.rm = TRUE))

# define the plot for average participants' response time
response_times <- ggplot(df_timings_passengers, aes(x=0, y=mean_timing)) +
  geom_boxplot(width=0.25) +
  stat_summary(fun = mean, geom="point", shape=8, size=6, color="red") +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5) +
  geom_text_repel(aes(y = mean_timing, x = 1, label = response_id),
                  arrow = arrow(length = unit(0.03, "npc"), type = "closed", ends = "first"),
                  force = 10,
                  xlim = c(0, 0.75),
                  direction = "y"
                  ) +
  ylab("Response time (s)") +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

response_times

# define the plot in a function
my_plot <- function(dataframe, column) {
  distribution <- ggplot(dataframe, aes(x = column)) +
    geom_density() +
    xlab("Time (s)") +
    ylab("Density") +
    theme_bw()
  return(distribution)
}

my_plot(df_timings_passengers, df_timings_passengers$timing_page_submit_20)




