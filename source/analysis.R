library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(usmap)


#organized the data frame with only the variable needs for the visualization.
new_data <- {
  original_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
  organized_data <- select(original_data, "year", "state","county_name", "total_pop", "total_jail_pop","land_area" )
}

#stated the top five states with the greatest land area of the most current date
top_five <- new_data %>% filter(year == max(year)) %>%group_by(state) %>%summarise(land_area = sum(land_area, na.rm = TRUE))%>%arrange(desc(land_area)) %>% slice(1:5)%>% pull(state)


#displays the trend of total jail population within the states owns the largest land area.
time_trend_chart <- {
  max_land_area <- new_data %>% group_by(state,year)%>%filter(between(year, 1988, 2018))%>%summarise(total = sum(total_jail_pop, na.rm = TRUE))%>% 
    filter(state == "AK"|state == "TX"|state == "CA"|state == "MT"|state == "NM")
  names(max_land_area)[names(max_land_area) == "total"] <- "Total Jail Population"
  chart1 <- max_land_area %>% plot_ly(x= ~year, y = ~`Total Jail Population`, 
                                      type = "scatter", mode="lines", color =~state) %>% 
    layout(title = "Total Jail Population in TOP 5 states with largest land area")
  
  print(chart1)
}

#compare the relationship in between the total population in US and its jail population.
comparision_chart <- {
  total_vs_jail <- new_data %>% group_by(year) %>%
    summarise(total_pop = sum(total_pop, na.rm = TRUE), total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  year <- total_vs_jail$year
  chart2 <- plot_ly(x = ~total_vs_jail$total_pop, y =~total_vs_jail$total_jail_pop,
                    mode = "markers", color = ~year)%>%
  layout(title="relationship in population",legend_title= list(title = "Year"), xaxis=list(title="Total Population in US"), yaxis=list(title="Total Jail Population"))
  
  print(chart2)
}


#use map to shows the total jail population in each states in the US.
map <- {
  state_jail_pop <- new_data%>%group_by(state)%>%summarise(total=sum(total_jail_pop, na.rm = TRUE))
  
  chart3 <- plot_usmap(data = state_jail_pop, values = "total", color = "red") +
    scale_fill_continuous(low = "white", high = "red", name = "Jail Population(2018)", label = scales::comma) +
    labs(title = "The Jail Population in US", subtitle = "from 1970 to 2018 ")+theme(legend.position = "right")
  
  print(chart3)
}

