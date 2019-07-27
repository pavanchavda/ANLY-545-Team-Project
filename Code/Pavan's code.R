library(dplyr); library(ggplot2); library(plotly); library(lubridate); library(leaflet); library(rgdal)

data <- read.csv("C:/Users/Pavan/Documents/GitHub/ANLY-545-Team-Project/Data/gun-violence-data.csv")
str(data)
data$date <- mdy(data$date)
#No. of incidents by State
incidents_by_state <- data %>%
  group_by(state) %>%
  summarise(total_incidents = n(),
            total_injured = sum(n_injured),
            total_killed = sum(n_killed)) %>%
  arrange(desc(total_injured,total_killed))
  
statesUS <- readOGR("tl_2018_us_state", layer = "tl_2018_us_state", GDAL1_integer64_policy = TRUE)
incidents_by_state2 <- merge(statesUS, incidents_by_state, by.x="NAME", by.y="state")
bins <- c(0, 5000, 7500, 10000, 15000, 20000)
pal <- colorBin("YlOrRd", domain = incidents_by_state2$total_incidents, bins = bins)

labels <- paste0("<strong>State: </strong>", 
                 incidents_by_state2$NAME, 
                 "<br><strong>Total Incidents </strong>", 
                 incidents_by_state2$total_incidents,
                 "<br><strong>Total Injured </strong>", 
                 incidents_by_state2$total_injured,
                 "<br><strong>Total Killed </strong>", 
                 incidents_by_state2$total_killed) %>% lapply(htmltools::HTML)

leaflet(data = incidents_by_state2) %>%
  setView(-96, 37.8, 4) %>%
  addTiles() %>%
  addProviderTiles("MapBox", options = providerTileOptions(id = "mapbox.light",accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
    addPolygons(
    fillColor = ~pal(total_incidents),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~total_incidents, opacity = 0.7, title = "Total Incidents", position = "bottomright")

#No. of Deaths by State
deaths_by_state <- data %>%
  group_by(state) %>%
  summarise(total_killed = sum(n_killed)) %>%
  arrange(desc(total_killed))

g1 <- ggplot(deaths_by_state, aes(x=reorder(state, total_killed),y=total_killed, fill=  total_killed, text=state)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_gradient(low="yellow", high = "red")+
  labs(x="State" , y = "Number of Deaths") +
  theme_classic()
ggplotly(g1)

#Top 10 states with highest gun incidents
top10_by_state <- data %>%
  group_by(state=state) %>%
  summarise(total_incidents = n(),
            total_injured = sum(n_injured),
            total_killed = sum(n_killed)) %>%
  arrange(desc(total_injured,total_killed)) %>%
  top_n(10, total_incidents)

g2 <- ggplot(top10_by_state, aes(x=reorder(state,total_incidents), y=total_incidents, text=state))+
  geom_bar(stat="identity", fill="red") + 
  coord_flip() +
  labs(x="State", y="Number of Incidents", title = "Top 10 States with Highest Gun Incidents")
ggplotly(g2)


#Top 10 cities with highest gun incidents
top10_by_city <- data %>%
  group_by(city=city_or_county) %>%
  summarise(total_incidents = n(),
            total_injured = sum(n_injured),
            total_killed = sum(n_killed)) %>%
  arrange(desc(total_injured,total_killed)) %>%
  top_n(10, total_incidents)

g3 <- ggplot(top10_by_city, aes(x=reorder(city,total_incidents), y=total_incidents, text=city))+
  geom_bar(stat="identity", fill="red") + 
  coord_flip() +
  labs(x="City", y="Number of Incidents", title = "Top 10 Cities with Highest Gun Incidents")
ggplotly(g3)
