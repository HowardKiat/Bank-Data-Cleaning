# Q1
library(ggplot2)
library(dplyr)
data("economics")

# i 
# Line Plot
ggplot(economics, aes(x = date, y = pop)) +
  geom_line() +
  labs(x = "Date", y = "Population", title = "Total Population Over Time")

# ii 
# Filter the data after 2005-01-01
filtered_data <- economics %>%
  filter(date >= as.Date("2005-01-01"))

# Plot the filtered data
ggplot(filtered_data, aes(x = date, y = pop)) +
  geom_line() +
  labs(x = "Date", y = "Population", title = "Total Population After 2005-01-01")


# Q2
library(nycflights13)

# Load the flights dataset
data("flights")

# a. Use a proper plot to display the data in carrier name column 
ggplot(flights, aes(x = carrier)) +
  geom_bar() +
  labs(x = "Carrier", y = "Count", title = "Number of Flights by Carrier")

# b. Use proper plots to display the data in departure delays
ggplot(flights, aes(x = dep_delay)) +
  geom_histogram(binwidth = 10) +
  labs(x = "Departure Delay (minutes)", y = "Count", title = "Distribution of Departure Delays")

# c. Use proper plots to display the data in arrival delays 
ggplot(flights, aes(x = arr_delay)) +
  geom_histogram(binwidth = 10) +
  labs(x = "Arrival Delay (minutes)", y = "Count", title = "Distribution of Arrival Delays")

# d. Use a proper plot to display the co-variation between departure delays  and arrival delays 
ggplot(flights, aes(x = dep_delay, y = arr_delay)) +
  geom_point() +
  labs(x = "Departure Delay", y = "Arrival Delay", title = "Departure Delay and Arrival Delay")

# e. Use a proper plot to display the co-variation between origin and destination
ggplot(flights, aes(x = origin, y = dest)) +
  geom_point() +
  labs(x = "Origin", y = "Destination", title = "Covariation between Origin and Destination")

# f. Use a proper plot to display the co-variation between origin  and departure delays
ggplot(flights, aes(x = origin, y = dep_delay)) +
  geom_boxplot() +
  labs(x = "Origin", y = "Departure Delay (minutes)", title = "Covariation between Origin and Departure Delay")

# g. grouping the plots by carrier name (carrier)
ggplot(flights, aes(x = dep_delay, fill = carrier)) +
  geom_histogram(binwidth = 10) +
  labs(x = "Departure Delay (minutes)", y = "Count", title = "Distribution of Departure Delays by Carrier")

# h. facetting the plots by carrier name (carrier)
ggplot(flights, aes(x = dep_delay)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~ carrier) +
  labs(x = "Departure Delay (minutes)", y = "Count", title = "Distribution of Departure Delays Facetted by Carrier")

