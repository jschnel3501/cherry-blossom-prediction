#access cherry data
library(tidyverse)
cherry <- read.csv("data/washingtondc.csv") |> 
  bind_rows(read.csv("data/liestal.csv")) |> 
  bind_rows(read.csv("data/kyoto.csv")) |> 
  bind_rows(read.csv("data/vancouver.csv")) |> 
  bind_rows(read.csv("data/nyc.csv"))

##############access historical temperature data########################

#reworked only slightly from demo analysis to change range of years

NOAA_WEB_API_TOKEN <- 'TAIJjIkcwAjdHGYRYomBaLWGEPjeQPiN'
Sys.setenv(NOAA_WEB_API_TOKEN = "TAIJjIkcwAjdHGYRYomBaLWGEPjeQPiN")
NOAA_WEB_API_TOKEN <- Sys.getenv("NOAA_WEB_API_TOKEN")

install.packages("httr2")
install.packages("jsonlite")
library(httr2)
library(jsonlite)

NOAA_API_BASE_URL <- "https://www.ncei.noaa.gov/cdo-web/api/v2/data"

# Define the station IDs for the specified locations
stations <- c(
  "washingtondc" = "GHCND:USW00013743",
  "vancouver"    = "GHCND:CA001108395",
  "newyorkcity"  = "GHCND:USW00014732",
  "liestal"      = "GHCND:SZ000001940",
  "kyoto"        = "GHCND:JA000047759")

nested_to_tibble <- function (x) {
  # Determine the variable names in the response
  variable_names <- map(x, names) |> 
    unlist(use.names = FALSE) |> 
    unique()
  
  names(variable_names) <- variable_names

  # Reshape the response from a nested list into a table
  map(variable_names, \(i) {
    map(x, \(y) {
      if (is.null(y[[i]])) {
        NA_character_
      } else {
        y[[i]]
      }
    }) |> 
      unlist(use.names = FALSE)
  }) |> 
    as_tibble()
}

get_daily_avg_temp <- function(station_id, start_date, end_date,
                               api_key, base_url, window_size = 300) {
  windows <- seq(as_date(start_date),
                 as_date(end_date) + days(window_size + 1),
                 by = sprintf("%d days", window_size))
  
  batches <- map2(windows[-length(windows)], windows[-1] - days(1), \(from, to) {
    if (from > Sys.Date()) {
      return(NULL)
    }
    response <- tryCatch(
      request(base_url) |> 
        req_headers(token = api_key) |> 
        req_url_query(
          datasetid = "GHCND",
          stationid = station_id,
          datatypeid = "TAVG",
          startdate = from,
          enddate = min(as_date(to), Sys.Date()),
          units = "metric",
          limit = 1000
        ) |> 
        req_retry(max_tries = 10) |> 
        req_perform() |> 
        resp_body_json(),
      
      httr2_http = \(cnd) {
        rlang::warn(sprintf("Failed to retrieve data for station %s in time window %s--%s",
                            station_id, from, to),
                    parent = cnd)
        NULL
      })
  })
  
  map(batches, \(x) nested_to_tibble(x$results)) |> 
    list_rbind() |> 
    mutate(date = as_date(date))
}

#cache: true
historic_temperatures <- cherry |> 
  group_by(location) |> 
  summarize(start_date = sprintf('%d-01-01', pmax(2001, min(year)) - 1)) |> 
  left_join(tibble(location = names(stations),
                   station_id = stations),
            by = 'location') |> 
  group_by(location) |> 
  group_modify(\(x, gr) {
    get_daily_avg_temp(station_id = x$station_id,
                       start_date = x$start_date,
                       end_date = Sys.Date(),
                       api_key = NOAA_WEB_API_TOKEN,
                       base_url = NOAA_API_BASE_URL)
  })

####################load 2025 temp data######################
library("rvest")

get_weather_table <- function(url)
  read_html(url) %>% 
  html_nodes("div.monthly-calendar") %>% 
  html_text2() %>%
  str_replace("N/A", "N/A N/A") %>%
  str_remove_all("°|Hist. Avg. ") %>%
  str_split(" ", simplify = TRUE) %>%
  parse_number() %>%
  matrix(ncol = 4, 
         byrow = TRUE,
         dimnames = list(NULL, c("day", "tmax", "tmin", "tavg"))) %>%
  as_tibble() %>%
  filter(
    row_number() %in%
      (which(diff(day) < 0) %>% (function(x) if(length(x) == 1) seq(1, x[1], 1) else seq(x[1] + 1, x[2], 1))))

kyoto <-
  tibble(
    base_url = "https://web.archive.org/web/20250225/https://www.accuweather.com/en/jp/arashiyama/2334469/",
    month = month.name[1:4],
    year = 2025,
    url = str_c(base_url, tolower(month), "-weather/2334469?year=", year)) %>%
  mutate(temp = map(url, get_weather_table)) %>%
  pull(temp) %>%
  reduce(bind_rows) %>%
  transmute(date = seq(as.Date("2025-01-01"), as.Date("2025-04-30"), 1),
            year = parse_number(format(date, "%Y")),
            tmax,
            tmin,
            tavg,
            temp = (tmax + tmin) / 2)

liestal <-
  tibble(
    base_url = "https://web.archive.org/web/20250225/https://www.accuweather.com/en/ch/liestal/311994/",
    month = month.name[1:4],
    year = 2025,
    url = str_c(base_url, tolower(month), "-weather/311994?year=", year)) %>%
  mutate(temp = map(url, get_weather_table)) %>%
  pull(temp) %>%
  reduce(bind_rows) %>%
  transmute(date = seq(as.Date("2025-01-01"), as.Date("2025-04-30"), 1),
            year = parse_number(format(date, "%Y")),
            tmax,
            tmin,
            temp = (tmax + tmin) / 2)  

newyork <-
  tibble(
    base_url = "https://web.archive.org/web/20250225/https://www.accuweather.com/en/us/new-york/10021/",
    month = month.name[1:4],
    year = 2025,
    url = str_c(base_url, tolower(month), "-weather/349727?year=", year)) %>%
  mutate(temp = map(url, get_weather_table)) %>%
  pull(temp) %>%
  reduce(bind_rows) %>%
  transmute(date = seq(as.Date("2025-01-01"), as.Date("2025-04-30"), 1),
            year = parse_number(format(date, "%Y")),
            tmax,
            tmin,
            temp = (tmax + tmin) / 2)

washington <-
  tibble(
    base_url = "https://web.archive.org/web/20250225/https://www.accuweather.com/en/us/washington/20006/",
    month = month.name[1:4],
    year = 2025,
    url = str_c(base_url, tolower(month), "-weather/18-327659_1_al?year=", year)) %>%
  mutate(temp = map(url, get_weather_table)) %>%
  pull(temp) %>%
  reduce(bind_rows) %>%
  transmute(date = seq(as.Date("2025-01-01"), as.Date("2025-04-30"), 1),
            year = parse_number(format(date, "%Y")),
            tmax,
            tmin,
            temp = (tmax + tmin) / 2)  
    
vancouver <-
  tibble(
    base_url = "https://web.archive.org/web/20250225/https://www.accuweather.com/en/us/vancouver/98661/",
    month = month.name[1:4],
    year = 2025,
    url = str_c(base_url, tolower(month), "-weather/331419?year=", year)) %>%
  mutate(temp = map(url, get_weather_table)) %>%
  pull(temp) %>%
  reduce(bind_rows) %>%
  transmute(date = seq(as.Date("2025-01-01"), as.Date("2025-04-30"), 1),
            year = parse_number(format(date, "%Y")),
            tmax,
            tmin,
            temp = (tmax + tmin) / 2) 


#adjust historic to farenheit and keep only january to april to match 2025 data
temp_adj <- historic_temperatures %>%
  mutate(value = (value * 9/5) + 32) %>%  # Convert Celsius to Fahrenheit
  filter(format(as.Date(date), "%m") %in% c("01", "02", "03", "04"))

#calculate average and cumualtive temperatures on historical temps
temp_summ <- temp_adj %>%
  mutate(year = format(as.Date(date), "%Y")) %>%  # Extract year from date
  group_by(location,year) %>%  # Group by year
  summarize(
    avg_temp = mean(value, na.rm = TRUE),  # Compute average temperature
    cum_temp = sum(value, na.rm = TRUE)  # Compute cumulative temperature
  )

#remove limited actual 2025 data to add 2025 temps including the scraped predictions
temp_summ <- temp_summ %>%
  filter(year != "2025")

#add locations to dataframes to be able to group later
kyoto <- kyoto %>% mutate(location = "kyoto")
liestal <- liestal %>% mutate(location = "liestal")
vancouver <- vancouver %>% mutate(location = "vancouver")
washington <- washington %>% mutate(location = "washingtondc")
newyork <- newyork %>% mutate(location = "newyorkcity")

#calculate average and cumulative temperature from the separate 2025 dataframes
df_2025_summary <- bind_rows(kyoto, liestal,vancouver, washington, newyork) %>%
  group_by(location, year) %>%
  summarize(
    avg_temp = mean(temp, na.rm = TRUE),  # Compute average temperature
    cum_temp = sum(temp, na.rm = TRUE)  # Compute cumulative temperature
  ) %>%
  ungroup()

#create final temperature table with predicted 2025 data for each location for years pulled that align with available bloom data (excludes certain years)
temp_final <- bind_rows(temp_summ, df_2025_summary) %>%
  arrange(location, year)

#################fitting model#######################
library(caret)

#create train data which excludes 2025 since we do not have bloom data
train_data <- cherry %>%
  inner_join(temp_final,
             by = c("location","year")) %>%
  select(location, year, bloom_doy, avg_temp, cum_temp)

#establish a LOOCV method for fitting models
train_control <- trainControl(method = "LOOCV")

#train base models using LOOCV
set.seed(123)
lm_model <- train(bloom_doy ~ location + cum_temp*avg_temp, data = train_data, method = "lm", trControl = train_control)
rf_model <- train(bloom_doy ~ location + cum_temp*avg_temp, data = train_data, method = "rf", trControl = train_control, ntree = 500)
xgb_model <- train(bloom_doy ~ location + cum_temp*avg_temp, data = train_data, method = "xgbTree", trControl = train_control,
                   tuneGrid = expand.grid(nrounds = 100, #adjusted to account for long runtime
                                          max_depth = 3,
                                          eta = 0.1, 
                                          gamma = 0, 
                                          colsample_bytree = 1, 
                                          min_child_weight = 1, 
                                          subsample = 1))

#create predictions from base models for training set
train_data <- train_data |> 
  mutate(
    lm_pred = predict(lm_model, train_data),
    rf_pred = predict(rf_model, train_data),
    xgb_pred = predict(xgb_model, train_data)
  )

#train stacked model using the linear, random forest, and xgboost models
meta_model <- lm(bloom_doy ~ lm_pred + rf_pred + xgb_pred, 
                    data = train_data)

#create prediction grid for all years (including 2025)
pred_grid <- expand_grid(location = unique(cherry$location),
                         year = 2000:2025) |> 
  inner_join(temp_final, by = c("location", "year"))

# Generate base model predictions for 2025
pred_grid <- pred_grid |> 
  mutate(
    lm_pred = predict(lm_model, pred_grid),
    rf_pred = predict(rf_model, pred_grid),
    xgb_pred = predict(xgb_model, pred_grid)
  )

# Apply stacking model to get final bloom predictions for all years including 2025
pred_grid <- pred_grid |> 
  mutate(final_pred = round(predict(meta_model, pred_grid)))

#calculate 95% prediction intervals
pred_intervals <- predict(meta_model, newdata = pred_grid, interval = "prediction", level = 0.95)

#add rounded intervals to prediction table
pred_grid <- pred_grid |> 
  mutate(
    lower_PI = floor(pred_intervals[, "lwr"]),
    upper_PI = ceiling(pred_intervals[, "upr"])
  )

#create final prediction table with only necessary columns and properly named
final_pred <- pred_grid %>%
  filter(year == 2025) %>%
  select(location, final_pred, lower_PI, upper_PI) %>%
  rename(prediction = final_pred,
         lower = lower_PI,
         upper = upper_PI)

write.csv(final_pred, "final-predictions.csv", row.names = FALSE)


    