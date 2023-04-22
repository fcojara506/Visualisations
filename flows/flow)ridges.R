setwd("~/Documents/GitHub/Visualisations/Visualisations/flows/")
library(data.table)
library(dplyr)
library(tidyr)
library(ggridges)
library(ggplot2)
q_mm_df = data.table::fread("CAMELS_CL/q_mm_day.csv")


# Calculate the proportion of NaN values in each column
nan_proportions <- colMeans(is.na(q_mm))

# Find the columns with more than 50% NaN values
columns_to_remove <- names(nan_proportions[nan_proportions > 0.5])

# Remove those columns from the data table
q_mm <- q_mm_df %>% select(!all_of(columns_to_remove))

# Melt the data to a long format
q_mm_long <- melt(q_mm,
                  id.vars = c("date", "year", "month", "day"),
                  variable.name = "gauge_id",
                  value.name = "flow") %>% 
  mutate(doy = as.integer(format(as.Date(date), "%j"))) %>% 
  select(-c(date,year,day))

# Group the data by gauge_id and DOY, and compute the average flow for each group
q_mm_avg_doy <- q_mm_long %>%
  group_by(gauge_id, doy) %>%
  summarize(avg_flow = mean(flow, na.rm = TRUE))


# Group by month and gauge_id, then calculate the average monthly flow
q_mm_avg_monthly <- q_mm_long %>%
  group_by(gauge_id, month) %>%
  summarise(avg_flow = mean(flow, na.rm = TRUE)) %>%
  ungroup()

# Create a ridgeline plot
#ridgeline_plot <- 
    ggplot(data = q_mm_avg_monthly, aes(x=month,
                                 y =gauge_id,
                                 group = gauge_id,
                                 height=avg_flow)) +
    geom_density_ridges(stat = "identity",
                        #col = NA,
                        fill=NA,
                        scale = 10,
                        rel_min_height = 0.01,
                        alpha = 0.6)+
  scale_x_continuous(breaks = seq(1, 12, 1), labels = month.abb) +
  labs(x = "Month", y = "Gauge ID", title = "Average Monthly Flow by Gauge") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

    ggplot(data = q_mm_avg_doy, aes(x= doy,
                                        y = - as.numeric(gauge_id),
                                        group = gauge_id,
                                        height=avg_flow)) +
      geom_density_ridges(stat = "identity",
                          #col = NA,
                          fill=NA,
                          scale = 1,
                          rel_min_height = 0.01,
                          alpha = 0.6)+
      #scale_x_continuous(breaks = seq(1, 12, 1), labels = month.abb) +
      labs(x = "DOY", y = "Gauge ID", title = "Average Monthly Flow by Gauge") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 6))
    
    
    
