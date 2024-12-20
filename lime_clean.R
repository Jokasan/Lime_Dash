# Final Plots:
# Wrangling:
pacman::p_load(tidyverse, lubridate, readr,highcharter,echarts4r,echarts4r.maps,leaflet,ggmap,leaflet.extras,htmltools,broom,flexdashboard)

limebike_data <- read_csv("TRIPS.csv")

limebike_data %>% 
  filter(STATUS == "completed" & !is.na(STARTED_AT)) %>% 
  mutate(STARTED_AT = substr(STARTED_AT,start=1,stop=19),
         COMPLETED_AT = substr(COMPLETED_AT,start=1,stop=19)) %>%
  mutate(start_date = lubridate::as_datetime(STARTED_AT),
         end_date = lubridate::as_datetime(COMPLETED_AT),
         day_of_week = wday(start_date, label = TRUE),
         month = month(start_date, label = TRUE)) %>% 
  mutate(time = as.integer(difftime(end_date,start_date,units = "mins"))) %>% 
  select(start_date, end_date, DISTANCE_METERS,COST_AMOUNT_CENTS,time) %>% 
 mutate(total_distance = DISTANCE_METERS/1000,
            total_cost = COST_AMOUNT_CENTS/100,
            total_time = time/60,
            speed = ((DISTANCE_METERS/1000)/time)*60) -> summarised_dist_time

limebike_data %>% 
  # select(STARTED_AT,COMPLETED_AT) %>% 
  filter(STATUS == "completed" & !is.na(STARTED_AT)) %>% 
  mutate(STARTED_AT = substr(STARTED_AT,start=1,stop=19),
         COMPLETED_AT = substr(COMPLETED_AT,start=1,stop=19)) %>%
  mutate(start_date = lubridate::as_datetime(STARTED_AT),
         end_date = lubridate::as_datetime(COMPLETED_AT),
         day_of_week = wday(start_date, label = TRUE),
         month = month(start_date, label = TRUE),
         quarter = quarter(start_date)) %>% 
  mutate(time = as.integer(difftime(end_date,start_date,units = "mins")),
         COST=COST_AMOUNT_CENTS/100,
         speed = ((DISTANCE_METERS/1000)/time)*60) %>%
  filter(speed != Inf & COST>0) %>%
  select(start_date,end_date,time,DISTANCE_METERS,COST,speed) -> to_model

# Top line metrics:

valueBox(scales::number(round(sum(summarised_dist_time$total_distance)),suffix =  "Km"), caption = "Total Distance Cycled", color = "success", icon = "glyphicon glyphicon-circle-arrow-up")
valueBox(scales::dollar(round(sum(summarised_dist_time$total_cost)),prefix = "£ "), caption = "Total Cost", color = "orange", icon = "glyphicon glyphicon-circle-arrow-down")
valueBox(scales::number(round(sum(summarised_dist_time$total_time)),suffix = " hrs "), caption = "Total Time", color = "steelblue", icon = "glyphicon glyphicon-time")
valueBox(scales::number(round(mean(to_model$speed)),suffix = " Km/hr"), caption = "Total Time", color = "purple", icon = "glyphicon glyphicon-time")


# Plot 1:
limebike_data %>% 
  # select(STARTED_AT,COMPLETED_AT) %>% 
  filter(STATUS == "completed" & !is.na(STARTED_AT)) %>% 
  mutate(STARTED_AT = substr(STARTED_AT,start=1,stop=19),
         COMPLETED_AT = substr(COMPLETED_AT,start=1,stop=10)) %>%
  mutate(start_date = lubridate::as_datetime(STARTED_AT),
         end_date = lubridate::as_datetime(COMPLETED_AT),
         day_of_week = wday(start_date, label = TRUE),
         month = month(start_date, label = TRUE)) %>% 
  select(day_of_week, month, DISTANCE_METERS) %>% 
  group_by(month,day_of_week) %>% 
  summarise(total_distance = sum(DISTANCE_METERS)) %>% 
  ungroup()-> summarised_dist_time

limebike_data %>% 
  filter(STATUS == "completed" & !is.na(STARTED_AT)) %>% 
  mutate(STARTED_AT = substr(STARTED_AT,start=1,stop=19),
         COMPLETED_AT = substr(COMPLETED_AT,start=1,stop=19)) %>%
  mutate(start_date = lubridate::as_datetime(STARTED_AT),
         end_date = lubridate::as_datetime(COMPLETED_AT),
         day_of_week = wday(start_date, label = TRUE),
         month = month(start_date, label = TRUE)) %>% 
  mutate(time = as.integer(difftime(end_date,start_date,units = "mins"))) %>% 
  select(month, day_of_week, DISTANCE_METERS,COST_AMOUNT_CENTS,time) %>% 
  mutate(total_distance = DISTANCE_METERS/1000,
         total_cost = COST_AMOUNT_CENTS/100,
         total_time = time/60,
         speed = ((DISTANCE_METERS/1000)/time)*60) %>% 
  group_by(month,day_of_week) %>%
  summarise(total_distance = sum(total_distance)) %>% 
  ungroup()-> summarised_dist_time

fntltp <- JS("function(){
  return this.series.yAxis.categories[this.point.y] + ': ' +
  Highcharts.numberFormat(this.point.value, 0);
}")

plotline <- list(
  color = "#fde725", value = 8.5, width = 2, zIndex = 5,
  label = list(
    text = "Summer Ends", verticalAlign = "top",
    style = list(color = "#606060"), textAlign = "left",
    rotation = 0, x =-10
  )
)
plotline <- list(
  color = "#fde725",
  value = 8.5,  # September is the 9th month, but arrays are 0-indexed
  width = 2,
  zIndex = 5,
  label = list(
    text = "Summer Ends",
    align = "right",
    style = list(color = "white",
                 fontWeight = "bold"),
    x = -6,  # Adjust this value to move the label horizontally
    y = 12,   # Adjust this value to move the label vertically
    useHTML = TRUE,
    verticalAlign = "middle"
  )
)

hchart(
  summarised_dist_time %>% 
    rename("Day of Week" = day_of_week, "Month" = month), 
  "heatmap", 
  hcaes(
    x = `Day of Week`,
    y = Month, 
    value = total_distance)) %>%
  hc_colorAxis(
    stops = color_stops(10, viridisLite::inferno(10, direction = -1))) %>% 
  hc_yAxis(
    title = list(text = ""),
    reversed = TRUE, 
    offset = -10,
    tickLength = 0,
    gridLineWidth = 0, 
    minorGridLineWidth = 0,
    labels = list(style = list(fontSize = "9px"))
  ) %>%
  hc_tooltip(
    formatter = fntltp
  ) %>% 
  hc_yAxis(
    plotLines = list(plotline),
    labels = list(
      style = list(fontSize = "14px")  # Adjust this value for x-axis label size
    )
  ) %>% 
  hc_xAxis(
    labels = list(
      style = list(fontSize = "14px")  # Adjust this value for x-axis label size
    )
  ) %>% 
  hc_title(
    text = "What time of the year does Fawaz cycle the most?",
    align = "left",
    x=12) %>%
  hc_subtitle(
    text = "Fawaz cycled the most on Sundays in October\n
      and on Tuesdays in August",
    align = "left",
    x=12
  ) %>%
hc_legend(
    align = "right",
    verticalAlign = "top",
    layout = "vertical",
    symbolWidth = 10,
    symbolHeight = 200,
    symbolPadding = 0,
    padding = 20,
    itemStyle = list(fontSize = "12px"),
    title = list(text = "Distance (Km)"),
    y=20
  )
hc_size(height = 700) %>% 
  hc_credits(enabled = FALSE) %>% 
  hc_chart(style = list(fontFamily = "Arial, Helvetica, sans-serif"))

# Plot 2:

limebike_data %>% 
  filter(STATUS == "completed" & !is.na(STARTED_AT)) %>% 
  mutate(STARTED_AT = substr(STARTED_AT,start=1,stop=19),
         COMPLETED_AT = substr(COMPLETED_AT,start=1,stop=10)) %>%
  mutate(start_date = lubridate::as_datetime(STARTED_AT),
         end_date = lubridate::as_datetime(COMPLETED_AT),
         day_of_week = wday(start_date, label = TRUE),
         month = month(start_date, label = TRUE)) %>% 
  select(month, DISTANCE_METERS) %>% 
  group_by(month) %>% 
  summarise(total_distance = sum(DISTANCE_METERS)/1000) %>% 
  ungroup() %>% 
  mutate(label = scales::number(total_distance,scale=2,suffix = " Km")) -> summarised_dist_month

summarised_dist_time <- summarised_dist_time %>% 
  mutate(label = scales::number(round(total_distance),suffix = " Km"))

day_drill_down <-summarised_dist_time %>% 
  group_nest(month) %>% 
  mutate(
    id = month,
    type = "column",
    data = map(data, mutate, name = day_of_week, y  = total_distance),
    data = map(data, list_parse)
  )

x <- c("Total Distance: ")
y <- c("{point.label}")

tt <- tooltip_table(x, y)
cols <- viridis(12,option = "rocket")

hchart(
  summarised_dist_month,
  "column",
  hcaes(x = month, y = total_distance, name = month, drilldown = month),
  name = "Total Distance by Month",
  colorByPoint = TRUE
) |>
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(day_drill_down)
  ) |>
  hc_tooltip(
    pointFormat = tt, # "{point.name} {point.pop}"
    useHTML = TRUE,
    valueDecimals = 0,
    style = list(fontSize = '12px')
  ) |>
  hc_yAxis(
    title = list(text = "Total Distance")
  ) |>
  hc_xAxis(
    title = "Total Distance by Month & Day of Week",
    labels = list(style = list(fontSize = '14px'))
  ) %>% 
  hc_chart(
    style = list(fontFamily = "Verdana")) %>% 
  hc_colors(cols)

hchart(
  summarised_dist_month,
  "column",
  hcaes(x = month, y = total_distance, name = month, drilldown = month),
  name = "Total Distance by Month",
  colorByPoint = TRUE
) |>
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(day_drill_down)
  ) |>
  hc_tooltip(
    pointFormat = tt, # "{point.name} {point.pop}"
    useHTML = TRUE,
    valueDecimals = 0,
    style = list(fontSize = '12px')
  ) |>
  hc_yAxis(
    title = list(text = "Total Distance")
  ) |>
  hc_xAxis(
    title = "Total Distance by Month & Day of Week",
    labels = list(style = list(fontSize = '14px'))
  ) %>% 
  hc_chart(
    style = list(fontFamily = "Verdana")) %>% 
  hc_colors(cols)
# Plot 3:


limebike_data %>% 
  filter(STATUS == "completed" & !is.na(STARTED_AT)) %>% 
  mutate(STARTED_AT = substr(STARTED_AT,start=1,stop=19),
         COMPLETED_AT = substr(COMPLETED_AT,start=1,stop=10)) %>%
  mutate(start_date = lubridate::as_datetime(STARTED_AT),
         end_date = lubridate::as_datetime(COMPLETED_AT),
         day_of_week = wday(start_date, label = TRUE),
         month = month(start_date, label = TRUE),
         quarter = quarter(start_date),
         quarter_label = case_when(quarter == 1 ~ "Q1",
                                   quarter == 2 ~ "Q2",
                                   quarter == 3 ~ "Q3",
                                   quarter == 4 ~ "Q4")) %>% 
  select(quarter_label, month, DISTANCE_METERS) %>% 
  group_by(quarter_label,month) %>% 
  summarise(total_distance = sum(DISTANCE_METERS)) %>% 
  ungroup()-> summarised_dist_quart

summarised_dist_quart %>%
  group_by(quarter_label) %>%
  summarise(dist=sum(total_distance)) %>% 
  mutate(parents="Everything") %>% 
  rename(labels=quarter_label,value=dist) %>% 
  select(parents,labels,value) %>% 
  rbind(summary_df) %>% 
  rbind(df %>% 
          slice_head(n=1))-> summary_df

df <- data.frame(
  parents = c("","earth", "earth", "mars", "mars", "land", "land", "ocean", "ocean", "fish", "fish", "Everything", "Everything", "Everything"),
  labels = c("Everything", "land", "ocean", "valley", "crater", "forest", "river", "kelp", "fish", "shark", "tuna", "venus","earth", "mars"),
  value = c(0, 30, 40, 10, 10, 20, 10, 20, 20, 8, 12, 10, 70, 20)
)

summary_df <- summarised_dist_quart%>% 
  mutate(parents = quarter_label,
         labels = month,,
         value = total_distance) %>% 
  select(parents, labels, value)

# We need an extra step:

layer_1 <-  slice_head(df, n=1)

summarised_dist_quart %>% 
  group_by(quarter_label) %>% 
  summarise(dist=sum(total_distance)) %>% 
  mutate(parents="Everything") %>% 
  select(parents,quarter_label,dist) %>% 
  rename(labels=quarter_label,value=dist) -> parent_layer

summary_df %>% 
  select(parents,labels,value) %>% 
  rbind(parent_layer) %>% 
  rbind(layer_1) %>% 
  mutate(value = round(value)/1000)-> summary_df

universe <- data.tree::FromDataFrameNetwork(summary_df)

# use it in echarts4r
universe |> 
  e_charts() |> 
  e_sunburst()

# Plot 4:

limebike_data %>% 
  filter(STATUS == "completed" & !is.na(STARTED_AT)) %>% 
  mutate(STARTED_AT = substr(STARTED_AT,start=1,stop=19),
         COMPLETED_AT = substr(COMPLETED_AT,start=1,stop=10)) %>%
  mutate(start_date = lubridate::as_date(STARTED_AT),
         end_date = lubridate::as_datetime(COMPLETED_AT),
         day_of_week = wday(start_date, label = TRUE),
         month = month(start_date, label = TRUE),
         year = year(start_date),
         COST_AMOUNT_CENTS = COST_AMOUNT_CENTS/100) %>% 
  select(STARTED_AT, year,DISTANCE_METERS,COST_AMOUNT_CENTS) %>%
  group_by(STARTED_AT,year) %>%
  summarise(total_distance = sum(DISTANCE_METERS),
            total_cost = sum(COST_AMOUNT_CENTS)) %>% 
  ungroup() %>% 
  mutate(date=lubridate::as_date(STARTED_AT) %>% floor_date(unit = "month")) %>% 
  select(-STARTED_AT) %>% 
  group_by(date,year) %>% 
  summarise(total_distance = sum(total_distance),
            Cost = sum(total_cost)) %>% 
  ungroup()-> for_time_series

for_time_series %>% 
  mutate(month = month(date,label=TRUE)) %>%
  group_by(year) %>% 
  e_charts(date,timeline = TRUE) %>% 
  e_area(Cost, smooth = TRUE) %>% 
  e_theme("westeros") %>% 
  e_axis_labels(y = "Cost in GBP") %>%
  e_title("Total Cost by Date") %>% 
  e_legend(right = 0) %>% 
  e_mark_line(
    data = list(type = "average"),
    label = list(
      normal = list(
        show = TRUE,
        formatter = htmlwidgets::JS(
          "function(params) {
                var Cost = (params.value).toFixed(1);
                return params.name + '£ ' + Cost;
              }"
        ),
        color = "orange"  # Set the color of the line and text to red
      )
    ),
    lineStyle = list(
      color = "orange"  # Set the color of the average line to red
    )
  ) %>% 
  e_format_y_axis(suffix = "£") %>% 
  e_timeline_opts(
    autoPlay = TRUE,
    playInterval = 2000)

# Plot 5 & 6:

geo_coded_start %>% 
  leaflet() %>% 
  addTiles() %>% 
  addHeatmap(~START_LONGITUDE,~START_LATITUDE,blur = 20,
             group = "Heatmap") %>% 
  addMarkers(~START_LONGITUDE,~START_LATITUDE,label=~htmlEscape(postcode),
             clusterOptions = markerClusterOptions(),
             group = "Point") %>%
  addLayersControl(
    overlayGroups = c("Heatmap","Point"),
    options = layersControlOptions(collapsed = FALSE)
  )


geo_coded_end %>% 
  leaflet() %>% 
  addTiles() %>% 
  addHeatmap(~END_LONGITUDE,~END_LATITUDE,blur = 20,
             group = "Heatmap") %>% 
  addMarkers(~END_LONGITUDE,~END_LATITUDE,label=~htmlEscape(postcode),
             clusterOptions = markerClusterOptions(),
             group = "Point") %>%
  addLayersControl(
    overlayGroups = c("Heatmap","Point"),
    options = layersControlOptions(collapsed = FALSE))

# Plot 6:

limebike_data %>% 
  # select(STARTED_AT,COMPLETED_AT) %>% 
  filter(STATUS == "completed" & !is.na(STARTED_AT)) %>% 
  mutate(STARTED_AT = substr(STARTED_AT,start=1,stop=19),
         COMPLETED_AT = substr(COMPLETED_AT,start=1,stop=19)) %>%
  mutate(start_date = lubridate::as_datetime(STARTED_AT),
         end_date = lubridate::as_datetime(COMPLETED_AT),
         day_of_week = wday(start_date, label = TRUE),
         month = month(start_date, label = TRUE),
         quarter = quarter(start_date)) %>% 
  mutate(time = as.integer(difftime(end_date,start_date,units = "mins")),
         COST=COST_AMOUNT_CENTS/100,
         speed = ((DISTANCE_METERS/1000)/time)*60) %>%
  filter(speed != Inf & COST>0) %>%
  select(start_date,end_date,time,DISTANCE_METERS,COST,speed) -> to_model

modlss <- loess(COST ~ speed, data = to_model)

fit <- arrange(augment(modlss), speed) |> 
  mutate(.se = predict(modlss, se = TRUE)$se.fit)


qtint <- round(qt(0.85, predict(modlss, se = TRUE)$df),2)

fit %>% 
  mutate_if(is.numeric, round, 2) -> fit

hchart(
  fit,
  type = "spline",
  hcaes(x = speed, y = .fitted),
  name = "Estimated Cost",
  id = "fit",
  lineWidth = 1,
  color = "#1F77B4",
  showInLegend = TRUE # You can change this color as desired
) |> 
  hc_add_series(
    fit,
    type = "arearange",
    name = "Confidence Interval",
    hcaes(x = speed, low = .fitted - qtint*.se, high = .fitted + qtint*.se),
    linkedTo = "fit",
    color = hex_to_rgba("indianred", 0.2),  # Semi-transparent color matching the line
    zIndex = -1
  ) |>
  hc_xAxis(title = list(text = "Speed")) |>
  hc_yAxis(title = list(text = "Cost")) |>
  hc_title(text = "Estimated Cost by Speed")%>% 
  hc_tooltip(
    crosshairs = TRUE,
    borderWidth = 5,
    sort = TRUE,
    table = TRUE,
    style = list(
      fontSize = "10px"
    )
    ) 



