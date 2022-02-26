library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(usmap)



#organized data
new_data <- {
  original_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
  organized_data <- select(original_data, "year", "state","county_name",
                           "total_pop", "total_jail_pop","land_area",
                           "white_jail_pop", "black_jail_pop", "total_jail_from_fed")
}


#SUMMARY INFORMATION

land_area <- new_data$land_area
#1
#stated the top five states with the greatest land area of the most current date
top_five_land_area <- new_data %>% filter(year == max(year)) %>%group_by(state) %>%
  summarise(land_area = sum(land_area, na.rm = TRUE))%>%
  arrange(desc(land_area)) %>% slice(1:5)%>% pull(state)

#2
#Which state has the highest population of most recent date?
highest_population_state <- new_data %>% filter(year == max(year)) %>% group_by(state)%>%
  summarise(total_population = sum(total_pop)) %>%
  filter(total_population == max(total_population))%>%
  pull(state)

#3
#which state has the highest jail population of most recent date? 
higest_jail_population_state <- new_data %>% filter(year == max(year)) %>% group_by(state)%>%
  summarise(total_jail_population = sum(total_jail_pop, na.rm = TRUE)) %>%
  filter(total_jail_population == max(total_jail_population))%>%
  pull(state)

#4
#Which county of California has the largest population in jail?
highest_jail_county <- new_data %>% filter(year == max(year)) %>% filter(state == "CA")%>%
  group_by(county_name) %>% summarise(total_jail = sum(total_jail_pop, na.rm = TRUE)) %>%
  filter(total_jail == max(total_jail)) %>% pull(county_name)


#5
#Which year and state has the highest jail population ratio (total_jail_pop / total_pop)?
highest_jail_ratio <- new_data %>%mutate(ratio = total_jail_pop/total_pop) %>%
  filter(total_pop >0) %>%  arrange(desc(ratio)) %>% slice(1)



#Trend over time chart
#displays the trend of total jail population within the states owns the largest land area.

time_trend_chart<- {
  black_vs_white <- new_data %>% group_by(year) %>%
    summarise(total_pop = sum(total_pop, na.rm = T),
              black = sum(black_jail_pop, na.rm = T),
              white = sum(white_jail_pop, na.rm = T))
  
  chart1 <- black_vs_white %>% ungroup() %>% plot_ly( x=~total_pop)
  chart1 <- chart1 %>% add_trace(y = ~black, name = "black",type = "scatter", mode="lines", line = list(color = "red"))
  chart1 <- chart1 %>% add_trace(y = ~white, name = "white", type = "scatter", mode="lines", line = list(color = "green"))
  chart1 <- chart1 %>% layout(title="The Jail Population vs U.S. Population",
                              legend=list(title=list(text="Race")), xaxis=list(title="Population in US"),
                              yaxis=list(title="The Jail Population"))
  print(chart1)
}


#Variable comparison chart
#compare the relationship in between the total population in US and its jail population.
comparision_chart <- {
  fed_vs_black <- new_data %>% group_by(year) %>%
    summarise(fed_jail_pop = sum(total_jail_from_fed, na.rm = T),
              black_jail_pop = sum(black_jail_pop, na.rm = T),
              white_jail_pop = sum(white_jail_pop, na.rm = T))
  
  chart2 <- fed_vs_black %>% plot_ly(x = ~fed_jail_pop)
  chart2 <- chart2 %>% add_trace(y = ~black_jail_pop, mode = "markers", name = "Black People")
  chart2 <- chart2 %>% add_trace(y = ~white_jail_pop, mode = "markers", name = "White People")
  chart2 <- chart2 %>% layout(title="Relationship Between Different Race in Jail and Cases Held by Federal ",
                              legend=list(title=list(text="Race: ")), xaxis=list(title="Number of Cases held by Federal Agencies"),
                              yaxis=list(title="Population in Jail"))
  
  print(chart2)
}



#Map
#use map to shows the total jail population in each states in the US.
map <- {
  state_jail_pop <- new_data%>%group_by(state)%>%summarise(total=sum(total_jail_pop, na.rm = TRUE))
  
  chart3 <- plot_usmap(data = state_jail_pop, values = "total", color = "red") +
    scale_fill_continuous(low = "white", high = "red", name = "Jail Population(2018)", label = scales::comma) +
    labs(title = "The Jail Population in US", subtitle = "from 1970 to 2018 ")+theme(legend.position = "right")
  
  print(chart3)
}

