library(tidyverse)
library(nycflights13)

# Install scales if not already installed
install.packages("scales")  

# Load the package
library(scales)


setwd("C:/Users/asado/Desktop/R/Ggplot")
flights <- nycflights13::flights
airports <- nycflights13::airports
planes <- nycflights13::planes

airlines <-  nycflights13::airlines
weather <- nycflights13::weather


# data exploration
colnames(weather)


data(package = 'nycflights13')
# Questions.
# 1. What is the average airtime for each origin airport (names) in each of the months of 2013.

flights %>% 
  left_join(airports, by = c('origin' = 'faa')) %>% 
  group_by(name, month) %>% 
  summarise(mean_airtime = mean(air_time, na.rm = TRUE)) %>% 
  arrange(mean_airtime,name) %>% 
  View()


# 2. In the winter months show top 5 carrier type of airplane that was late (arrival)
# at least for 15 minutes.

flights %>% 
  left_join(airlines, by = 'carrier') %>% 
  filter(arr_delay >= 15, month %in% c(12,1,2)) %>%
  arrange(desc(arr_delay)) %>% 
  slice_head(n = 5) %>% 
  select(name, carrier, month, arr_delay)
  


# 3. Show the top airport that was destination of an airplane that was most late in each of the time zone.
flights %>% 
  left_join(planes, by = 'tailnum') %>% 
  left_join(airports, by = c('dest' = 'faa')) %>% 
  group_by(tzone) %>% 
  slice_max(order_by = arr_delay, n = 1) %>% 
  select(dest, tailnum, name, arr_delay, tzone) %>% 
  arrange(desc(arr_delay)) %>% 
  View()



# 4. Observe if being late for arrival has some kind of significant relationship
# to the type of an airplane. (Show average arr_delay for each).

flights %>% 
  left_join(planes, by = 'tailnum') %>% 
  group_by(type) %>% 
  summarise(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(desc(avg_arr_delay)) %>% 
  View()


# 5. See if being late for arrival has something to do with wind speed.
flights %>% 
  left_join(weather, by = c('origin', 'year', 'month', 'day', 'hour')) %>% 
  filter(!is.na(wind_speed), !is.na(arr_delay)) %>% 
  ggplot(aes(x = wind_speed, y = arr_delay)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = 'lm', col = 'red') + 
  labs(
    title = 'Dependence of arrival delay on wind speed',
    x = 'Wind Speed (mph)',
    y = 'Arrival Delay (minutes)'
  ) + theme_minimal()





# 6. Show the share of top 3 manufacturers for the number of flights in 2013.

flights %>% 
  filter(year == 2013) %>% 
  inner_join(planes, by = 'tailnum') %>% 
  group_by(manufacturer, .groups='drop') %>% 
  summarise(num_of_flights = n(), .groups = 'drop') %>% 
  # top 3
  mutate(rank = dense_rank(desc(num_of_flights)),
         manufacturer = ifelse(rank > 3, 'Others', manufacturer)) %>% 
  
  # sum Others
  group_by(manufacturer, .groups = 'drop') %>% 
  summarise(num_of_flights = sum(num_of_flights)) %>% 
  ungroup() %>% 
  # percentage of total
  mutate(percent = num_of_flights/sum(num_of_flights)*100) %>% 
  ggplot(aes(x = "", y = percent, fill = manufacturer)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("red", "blue", "green", "gray")) +  # <- custom colors
  labs(title = "Top 3 Aircraft Manufacturers by Flight Share (2013)") +
  theme_void()



# 7. Observe in which season planes are more likely to be late for arrival.
flights %>% 
  mutate(
    seasons = case_when(
      month %in% 3:5   ~ 'Spring',
      month %in% 6:8   ~ 'Summer', 
      month %in% 9:11  ~ 'Autumn',
      month %in% c(12,1,2) ~ 'Winter'
    )
  ) %>% 
  group_by(seasons) %>% 
  summarise(avg_arr_delay = mean(arr_delay, na.rm = TRUE), .groups = "drop") %>% 
    ggplot(aes(x = seasons, y = avg_arr_delay)) +             
    geom_bar(stat = 'identity', color = '#0DFBBA', 
             fill = '#0DFBBA', width = 0.7) + 
    geom_text(aes(label = round(avg_arr_delay, 2)),           
              vjust = -0.5, size = 4, color = 'black') +    
    theme_minimal() +
    labs(title = "Average Arrival Delay by Season", x = "Season", y = "Average Arrival Delay")



# 8. Find the manufacturer that has fastest airplanes according to average air_time per distance of each manufacturer.

flights %>% 
  left_join(planes, by = 'tailnum') %>%
  mutate(
    airtime_per_dist = air_time/distance
  ) %>% 
  group_by(manufacturer) %>% 
  summarise(avg_airtime_per_dist = mean(airtime_per_dist, na.rm = TRUE)) %>% 
  arrange(avg_airtime_per_dist) %>% 
  slice_min(order_by = avg_airtime_per_dist, n = 3) %>% 
  
  ggplot(aes(x=manufacturer, y = avg_airtime_per_dist)) +
  geom_bar(stat = 'identity', na.rm = TRUE, color = 'blue', fill = 'skyblue') + 
  geom_text(aes(label = round(avg_airtime_per_dist,2))) + 
  scale_fill_brewer(palette = "Set2") + 
  labs(title = "Vertical Bar Chart", x = "Manufacturer", y = "Airtime Per Distance") + 
  theme_minimal()




# 9. Visualize average air time of airplanes flying from JFK airport for each month in order (Jan-Dec).
flights %>% 
  filter(origin == "JFK") %>% 
  group_by(month) %>% 
  summarise(avg_airtime = round(mean(air_time, na.rm = TRUE),2)) %>% 
  arrange(month) %>% 
  
  ggplot(
    aes(x = month, y = avg_airtime)
  ) + 
  
  geom_bar(
    stat = 'identity', color = 'green', fill = 'skyblue', width = 0.5
  ) + 

  scale_x_continuous(
    breaks = 1:12,
    labels = c("January", "February", "March", "April", "May", "June", "July",
               "August","September", "October", "November", "December")
  ) +
  
  labs(
    title = "AVG Airtime for JRK airplanes per month", 
    x = "Month", 
    y = "Average Airtime"
  ) + 
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # rotate labels
  )




# 10. Airport Connection Network.  Which airports are the most connected hubs (serving the widest variety of
# destinations)?
  

flights %>% 
  inner_join(airports, by = c('origin' = 'faa')) %>% 
  select(origin,name, dest) %>% 
  
  group_by(name) %>% 
  
  summarise(
    connected_dests = n_distinct(dest)
  ) %>% 
  
  ggplot(
    aes(x = connected_dests, y = reorder(name, connected_dests))
  ) + 
  
  geom_col(color = 'steelblue', fill = 'skyblue') + 
  
  geom_text(aes(label = connected_dests), hjust = -0.4, color = 'steelblue') 




  
# 11. Delay Trends by Airline Over the Year
# Compute average arrival delay by airline and month. Then visualize the trend
# lines to compare how different airlines perform across the year.
colors_ <- c('red', 'blue', 'green', 'purple', 'orange','brown',
            'pink', 'cyan', 'darkgreen', 'gold', 'navy', 'magenta', 
            'darkorange', '#1e8a54', 'gray40', '#688036')


flights %>% 
  inner_join(airlines, by = 'carrier') %>% 
  group_by(name, month) %>% 
  summarise(avg_arr_delay = mean(arr_delay, na.rm=TRUE)) %>% 
  
  ggplot(aes(x = month, y = avg_arr_delay, color = name, group = name)) +
    geom_line(size = 1.2) + 
    geom_point(size = 3, shape = 10) + 
    labs(title = 'Avg Arrival Delay by Airlines', x = 'Month', 
         y = "Avg Arrival Delay", color = "Airlines") +
    scale_color_manual(values = colors_) + 
    scale_x_continuous(
      breaks = 1:12,
      labels = c("Jan","Feb","Mar","Apr","May","Jun",
                 "Jul","Aug","Sep","Oct","Nov","Dec")
    ) + theme_classic()



# 12. Weather Impact on Delays
# Calculate average departure delay vs. temperature buckets (e.g., cut into bins of 5°C). 
# Visualize with a scatter + smoothed regression line

weather_range <-  range(weather$temp, na.rm =TRUE)

flights %>% 
  inner_join(weather, c('year', 'month', 'day', 'hour', 'origin')) %>% 
  filter(!is.na(dep_delay), !is.na(temp)) %>% 
  mutate(
    temp_bins = cut(
      temp,
      breaks = seq(floor(weather_range[1]), ceiling(weather_range[2]), by = 5)
    )
  ) %>%
  group_by(temp_bins) %>% 
  summarise(avg_dep_delay = mean(dep_delay, na.rm = T), 
            n = n()) %>% 
  
  ggplot(aes(x=avg_dep_delay, y=temp_bins, size = n)) + 
  geom_point() + 
  geom_smooth(
    aes(x = avg_dep_delay, y = as.numeric(temp_bins)),
    method = 'lm', color = 'red'
  ) + 
  theme_bw()





# 13. Flight Distance vs. Air Time by Manufacturer
# Plot distance vs. air_time, colored by manufacturer
# (e.g., Boeing vs. Airbus). Add regression lines for each manufacturer.


distance_range = range(flights$distance, na.rm = T)
# join flights and planes to have manufacturers column
flights %>% 
  inner_join(planes, by = 'tailnum') %>% 
  filter(!is.na(distance), !is.na(air_time)) %>% 
  
  # bin the distance 
  mutate(
    dist_bins = cut(
      distance,
      breaks = seq(floor(distance_range[1]), ceiling(distance_range[2]), by = 200)
      )
    ) %>% 
  
  group_by(dist_bins, manufacturer) %>% 
  summarise(
    avg_air_time = mean(air_time, na.rm = T),
    n = n()
  ) %>% 
  
  filter(!is.na(avg_air_time), !is.na(dist_bins)) %>% 
  # visualize the results
  ggplot(
    aes(x = avg_air_time, y = dist_bins,size = n, color = manufacturer)
  ) + 

  # scatter plot
  geom_point(alpha = 0.5) + 
  theme_gray()



# 14.  compute average arrival delay by destination
# airport. Plot the top 15 airports as a horizontal bar chart (ordered by delay).

flights %>% 
  inner_join(airports, by = c('dest' = 'faa')) %>% 
  group_by(name) %>% 
  summarise(
    avg_arr_delay = mean(arr_delay, na.rm = T), 
  ) %>% 
  filter(!is.na(avg_arr_delay)) %>% 
  slice_max(order_by = avg_arr_delay, n = 10) %>% 
  arrange(desc(avg_arr_delay)) %>% 
  
  ggplot(aes(y = reorder(name, avg_arr_delay), x = avg_arr_delay)) + 
  geom_bar(stat  = 'identity', color = 'blue', fill = 'skyblue') + 
  theme_gray()


# 15. For each airline, visualize the distribution of arrival delays as a boxplot. This
# allows comparison of not just averages but variability and outliers.
flights %>% 
  inner_join(airlines, by = 'carrier') %>% 
  group_by(name) %>% 
  filter(!is.na(carrier), !is.na(arr_delay)) %>% 
  
  ggplot(
    aes(x = name, y = arr_delay)
  ) + 
  geom_boxplot(color = 'black', alpha = 0.7, 
               outlier.color = 'red', outlier.size = 3) +
  coord_flip() +
  theme_minimal()
  
  

# 16. Create a heatmap of average departure delay by day and hour across all 
# flights.

flights %>% 
  filter(!is.na(dep_delay), !is.na(day), !is.na(hour)) %>% 
  group_by(day, hour) %>% 
  summarise(
    avg_dep_delay = mean(dep_delay, na.rm = TRUE)
  ) %>% 
  
  ggplot(
    aes(x = hour, y = factor(day), fill = avg_dep_delay)
  ) + 
  
  geom_tile(color = 'white') + 
  
  scale_fill_gradient(low = 'skyblue', high = 'red') + 
  
  labs(
    title = 'Average Departure Delay by Day and Hour',
    x = 'Hour of Day', 
    y = 'Day of Month',
    fill = 'Avg Delay(min)'
  ) +
  
  theme_minimal()



# 17.  Plot air_time vs. wind_speed to check if high winds shorten
# or lengthen flights. Use scatterplot + smoothing curve.

flights %>% 
  left_join(weather, by = c('origin' ,'year', 'month', 'day', 'hour')) %>% 
  filter(!is.na(air_time), !is.na(wind_speed)) %>% 
  select(air_time, wind_speed) %>% 
  sample_frac(0.01) %>% 

  ggplot(
    aes(x = wind_speed, y = air_time)
  ) + 
  
  geom_point(
    alpha = 0.7, color = 'darkgreen'
  ) +
  
  geom_smooth(
    method = 'lm', color = 'red'
  ) +
  
  labs(
    title = 'Air time vs Wind speed', 
    x = 'Wind speed', 
    y = 'Air time'
  ) +
  
  theme_minimal() 


# 18.   Compute the percentage of flights by airline across the whole dataset. Display
# as a bar chart sorted from largest to smallest.

flights %>% 
  left_join(airlines, by = 'carrier') %>% 
  group_by(name) %>% 
  summarise(
    count = n()
  ) %>% 
  
  mutate(
    perc_of_total = count / sum(count) * 100 
  ) %>% 
  
  ggplot(
    aes(x = perc_of_total, y = reorder(name, perc_of_total))
  ) + 
  
  geom_bar(
    stat = 'identity', color = 'blue', fill = 'skyblue'
  ) +
  
  geom_text(
    data = . %>%  filter(perc_of_total >= 5), 
    aes(label = round(perc_of_total,2)), hjust = -0.5 ,size = 4, color = 'skyblue'
  ) + 
  
  labs(
    title = 'Percent of flights by airline', 
    y = 'Airline', 
    x = 'Percentage of total'
  ) + 
  
  theme_dark()


# 19. plot average arrival delay vs average distance for for each airline. 
# Use facets (facet_wrap) so each airline gets its own panel
flights %>% 
  left_join(airlines, by = 'carrier') %>% 
  select(carrier, name, arr_delay, distance) %>% 
  group_by(name) %>% 
  filter(!is.na(arr_delay), !is.na(distance)) %>% 
  slice_sample(prop = 0.08) %>% 
  
  ggplot(
    aes(x = arr_delay, y = distance)
  ) + 
  
  geom_point(alpha = 0.7, color = 'skyblue') +
  
  facet_wrap(~name) +
  
  theme_minimal()
  











# 20.  For each month, compute number of flights + average delay. Create a dual
# visualization: bar chart for flights count + line for average delay

flights %>% 
  select(year, month, flight, arr_delay, dep_delay) %>% 
  group_by(month) %>% 
  summarise(
    num_of_flights = n(),
    avg_delay = mean(arr_delay, na.rm = TRUE) + mean(dep_delay, na.rm = TRUE)
  ) %>% 
  mutate(scale_factor = max(num_of_flights) / max(avg_delay)) %>% 
  { 
    scale_val <- unique(.$scale_factor)  # extract scalar
    ggplot(., aes(x = month)) +
      geom_col(aes(y = num_of_flights), fill = "skyblue", alpha = 0.7) +
      geom_line(aes(y = avg_delay * scale_val), color = "red", size = 1) +
      geom_point(aes(y = avg_delay * scale_val), color = "red", size = 2) +
      scale_y_continuous(
        name = "Number of Flights",
        sec.axis = sec_axis(~ . / scale_val, name = "Average Delay (minutes)")
      ) +
      labs(title = "Flight Volume & Delays Over Time", x = "Month") +
      theme_minimal()
  }



















