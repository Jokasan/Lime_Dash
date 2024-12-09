pacman::p_load(tidyverse, lubridate, readr,highcharter,echarts4r,echarts4r.maps,leaflet,ggmap,leaflet.extras,htmltools,broom)

fawaz_lime <- read_csv("TRIPS.csv")


fawaz_lime %>% 
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
    stops = color_stops(10, viridisLite::inferno(10, direction = -1)),
    type="logarithmic") %>% 
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
  # hc_legend(
  #   layout = "vertical",
  #   verticalAlign = "top",
  #   align = "right",
  #     # symbolWidth = 10,
  #     # symbolHeight = 200,
  #     # symbolPadding = 0,
  #     # padding = 20,
  #     itemStyle = list(fontSize = "9px"),
  #     title = list(text = "Distance (m)"))%>%
  # 
  hc_legend(
    align = "right",
    verticalAlign = "top",
    layout = "vertical",
    symbolWidth = 10,
    symbolHeight = 200,
    symbolPadding = 0,
    padding = 20,
    itemStyle = list(fontSize = "12px"),
    title = list(text = "Distance (m)"),
    y=20
  )
  hc_size(height = 700) %>% 
  hc_credits(enabled = FALSE) %>% 
  hc_chart(style = list(fontFamily = "Arial, Helvetica, sans-serif"))

# Bar chart first:

  fawaz_lime %>% 
    filter(STATUS == "completed" & !is.na(STARTED_AT)) %>% 
    mutate(STARTED_AT = substr(STARTED_AT,start=1,stop=19),
           COMPLETED_AT = substr(COMPLETED_AT,start=1,stop=10)) %>%
    mutate(start_date = lubridate::as_datetime(STARTED_AT),
           end_date = lubridate::as_datetime(COMPLETED_AT),
           day_of_week = wday(start_date, label = TRUE),
           month = month(start_date, label = TRUE)) %>% 
    select(month, DISTANCE_METERS) %>% 
    group_by(month) %>% 
    summarise(total_distance = sum(DISTANCE_METERS)) %>% 
    ungroup() %>% 
    mutate(label = scales::number(total_distance,scale = 1e-3,suffix = " Km")) -> summarised_dist_month
  
summarised_dist_time <- summarised_dist_time %>% 
  mutate(label = scales::number(total_distance,scale = 1e-3,suffix = " Km"))
  
day_drill_down <-summarised_dist_time %>% 
  group_nest(month) %>% 
  mutate(
    id = month,
    type = "column",
    # in the drilldown we'll give the mapping via creating the columns
    data = map(data, mutate, name = day_of_week, y  = total_distance),
    data = map(data, list_parse)
  )

x <- c("Total Distance: ")
y <- c("{point.label}")

tt <- tooltip_table(x, y)

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
    style = list(fontSize = '16px')
  ) |>
  hc_yAxis(
    title = list(text = "Total Distance")
  ) |>
  hc_xAxis(
    title = "Total Distance by Month & Day of Week",
    labels = list(style = list(fontSize = '14px'))
  ) %>% 
  hc_chart(
    style = list(fontFamily = "Verdana"))
  
# Chart 1 Done ---

# Chart 1 Alt: Sunburst

# We need to do this by Quarter:

fawaz_lime %>% 
  # select(STARTED_AT,COMPLETED_AT) %>% 
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
  mutate(value = value /1000)-> summary_df


universe <- data.tree::FromDataFrameNetwork(summary_df)

# use it in echarts4r
universe |> 
  e_charts() |> 
  e_sunburst() %>% 
  e_tooltip(
    formatter = htmlwidgets::JS("
      function(params) {
        var value = params.value;
        var name = params.name;
        var formattedValue = value.toLocaleString() + ' Km';
        return name + ': ' + formattedValue;
      }
    ")
  )


# Chart 2:

fawaz_lime %>% 
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
  # filter(date >= '2022-01-01') %>% 
  ungroup()-> for_time_series

for_time_series %>% 
  mutate(month = month(date,label=TRUE)) %>%
  # mutate(date = as.Date(date)) %>% 
  group_by(year) %>% 
  e_charts(date,timeline = TRUE) %>% 
  e_area(Cost, smooth = TRUE) %>% 
  # e_charts(date,time = TRUE) 
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
    playInterval = 3000)
  
  # e_tooltip(
  #   trigger = "axis",
  #   formatter = e_tooltip_pointer_formatter(
  #     style = "currency",
  #     digits = 2,
  #     local = NULL,
  #     currency = "GBP"
  #   ),
  #   axisPointer = list(
  #     type = "cross"
  #   )
  # )


# Chart 3 Idea:
# Map:
fawaz_lime %>% 
  filter(STATUS == "completed" & !is.na(STARTED_AT))  %>%
  select(START_LATITUDE,START_LONGITUDE,END_LATITUDE,END_LONGITUDE)-> to_map

to_map %>% 
e_charts() %>% 
  e_geo(
    roam = TRUE,
    boundingCoords = list(
      c(-0.5, 51.7),  # Top-left corner
      c(0.3, 51.3)    # Bottom-right corner
    )
  ) %>% 
  e_lines(
    START_LONGITUDE,
    START_LATITUDE,
    END_LONGITUDE,
    END_LATITUDE)

# Mapping:

leaflet(to_map %>% 
          select(START_LATITUDE,START_LONGITUDE)) %>% 
  addTiles() %>% 
  addMarkers(~START_LONGITUDE,~START_LATITUDE,
             clusterOptions = markerClusterOptions())

leaflet(to_map %>% 
          select(END_LATITUDE,END_LONGITUDE)) %>% 
  addTiles() %>% 
  addMarkers(~END_LONGITUDE,~END_LATITUDE,
             clusterOptions = markerClusterOptions()) 

# For the labels we need to reverse geocode them:

to_map %>% 
  select(START_LATITUDE,START_LONGITUDE) %>%
  slice_head(n=1) %>% 
  mutate(geo = revgeocode(cbind(START_LONGITUDE,START_LATITUDE))) 

# Also want to add heatmap:

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

# to_map %>% 
geo_coded_end %>% 
  leaflet() %>% 
  addTiles() %>% 
  addHeatmap(~END_LONGITUDE,~END_LATITUDE,blur = 20,
             group = "Heatmap") %>% 
  # addMarkers(~END_LONGITUDE, ~END_LATITUDE,
  #            label = ~lapply(address_found, function(addr) {
  #              HTML(gsub(",\\s*", ",<br>", htmlEscape(addr)))
  #            }),
  #            clusterOptions = markerClusterOptions(),
  #            group = "Point View") %>% 
  addMarkers(~END_LONGITUDE,~END_LATITUDE,label=~htmlEscape(postcode),
             clusterOptions = markerClusterOptions(),
             group = "Point") %>%
  addLayersControl(
    overlayGroups = c("Heatmap","Point"),
    options = layersControlOptions(collapsed = FALSE)
  )
 
# Need to geocode for the point names:
to_map %>% 
  select(START_LATITUDE,START_LONGITUDE) %>% mutate_if(is.double,as.numeric) %>% 
  mutate(geo = revgeocode(c(START_LONGITUDE,START_LATITUDE))) 

# Geocoding:

to_map %>% 
  select(END_LATITUDE,END_LONGITUDE) %>%
  reverse_geocode(lat=END_LATITUDE,
                  long = END_LONGITUDE,
                  method='osm',
                  address = address_found,
                  full_result = TRUE)->geo_coded_end_locations


tt -> geo_coded_start_locations

geo_coded_start_locations %>% 
  select(START_LONGITUDE,START_LATITUDE,address_found) ->smaller_list
# Can we do something more with cost/speed?

# Need a few calculations first:

fawaz_lime %>% 
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

head(fit)

hchart(
  to_model,
  type = "scatter",
  hcaes(x = speed, y = COST)
) -> hc

qtint <- qt(0.975, predict(modlss, se = TRUE)$df)

hc |>
  hc_add_series(
    fit,
    type = "spline",
    hcaes(x = speed, y = .fitted),
    name = "Fit",
    id = "fit", # this is for link the arearange series to this one and have one legend
    lineWidth = 1,
    showInLegend = TRUE
  ) |> 
  hc_add_series(
    fit,
    type = "arearange",
    name = "SE",
    hcaes(x = speed, low = .fitted - qtint*.se, high = .fitted + qtint*.se),
    linkedTo = "fit", # here we link the legends in one.
    showInLegend = FALSE,
    color = hex_to_rgba("gray", 0.2),  # put a semi transparent color
    zIndex = -3 # this is for put the series in a back so the points are showed first
  )

hchart(
  fit,
  type = "spline",
  hcaes(x = speed, y = .fitted),
  name = "Estimated Cost",
  id = "fit",
  lineWidth = 2,
  color = "#1F77B4"  # You can change this color as desired
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
  hc_title(text = "Estimated Cost by Speed") |>
  hc_tooltip(
    crosshairs = TRUE,
    shared = TRUE,
    table = TRUE,
    style = list(fontSize = "10px")
  )

# Relationship between cost and speed?

https://echarts4r.john-coene.com/articles/stats

# Lay out:

https://testing-apps.shinyapps.io/flexdashboard-highcharter/




  