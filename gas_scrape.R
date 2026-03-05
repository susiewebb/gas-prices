#####For adding to the existing csv initiall scraped

#Loading libraries
library(tidyverse)
library(jsonlite)
library(rvest)
library(DatawRappr)
library(httr)
library(V8)
library(tidycensus)

##Loading in DW cues
api_key <- Sys.getenv("API_KEY")
gasTable <- Sys.getenv("GAS_TABLE_KEY")
gasMap <- Sys.getenv("GAS_MAP_KEY")

datawrapper_auth(api_key =  api_key, overwrite=TRUE)


#Marking today's date
today_head <- format(as.POSIXct(Sys.time(), tz = "America/New_York"), "%b. %d")
today_head <- sub("\\. 0", ". ", today_head)
today_head

yesterday <- format(as.Date(with_tz(Sys.time(), tz = 'America/New_York')) - 1, "%b. %d")
yesterday <- sub("\\. 0", ". ", yesterday)

#Loading in the old data
old_data <- read_csv('gas_prices.csv')

#Scraping today's gas data
parse_url <- function(map_id) {
  url <- paste0(
    "https://gasprices.aaa.com/index.php?premiumhtml5map_js_data=true&map_id=",
    map_id
  )
  
  response <- httr::GET(url)
  httr::stop_for_status(response)
  
  data <- httr::content(response, "text", encoding = "UTF-8")
  
  #Pulling out the JS
  js_block <- stringr::str_extract(
    data,
    "(?s)var\\s+premiumhtml5map_map_cfg_\\d+\\s*=\\s*\\{.*?\\};"
  )
  
  if (is.na(js_block)) {
    return(tibble::tibble(name = character(), comment = character(), state_id = integer()))
  }
  
  #Cleaning JS
  js_block <- gsub(":\\s*,", ": null,", js_block, perl = TRUE)
  
  #Removing trailing commas
  js_block <- gsub(",\\s*([}\\]])", "\\1", js_block, perl = TRUE)
  
  #More cleaning
  js_block <- gsub("[\u2028\u2029]", " ", js_block, perl = TRUE)
  
  var_name <- stringr::str_match(js_block, "var\\s+(premiumhtml5map_map_cfg_\\d+)\\s*=")[, 2]
  
  ctx <- V8::v8()
  
  #Test catch
  tryCatch(
    ctx$eval(js_block),
    error = function(e) {
      message("\nV8 failed while parsing map_id = ", map_id)
      message("First 1500 chars of cleaned js_block:\n")
      message(substr(js_block, 1, 1500))
      stop(e)
    }
  )
  
  json_map <- ctx$eval(paste0("JSON.stringify(", var_name, ".map_data)"))
  map_data <- jsonlite::fromJSON(json_map)
  
  map_data %>%
    purrr::map_dfr(~tibble::tibble(
      name    = .x$name    %||% NA_character_,
      comment = .x$comment %||% NA_character_
    )) %>%
    dplyr::mutate(state_id = map_id)
}


#Run it through each state
all_data <- map_dfr(setdiff(1:52, 17), parse_url)


##Join to FIPS

#Normalize the data
norm_nm <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("\\.", "") %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish() %>%
    str_replace_all("\\bsaint\\b", "st") %>%  # Saint -> st (matching)
    str_replace("\\s+(county|parish|borough|census area|municipality)$", "") %>%
    str_squish()
}

data("fips_codes", package = "tidycensus")
fips_ref <- fips_codes %>%
  transmute(
    state_name       = state,
    statefp          = state_code,
    county_name      = county,
    countyfp         = county_code,
    GEOID            = paste0(state_code, county_code),
    county_name_norm = norm_nm(county)
  ) %>%
  distinct()

#Infer state name
state_guess <- all_data %>%
  mutate(name_norm = norm_nm(name)) %>%
  distinct(state_id, name_norm) %>%
  inner_join(
    fips_ref %>% distinct(state_name, county_name_norm),
    by = c("name_norm" = "county_name_norm")
  ) %>%
  count(state_id, state_name, name = "hits", sort = TRUE) %>%
  group_by(state_id) %>%
  slice_max(hits, n = 1, with_ties = FALSE) %>%
  ungroup()

final_df <- all_data %>%
  mutate(
    name_norm  = norm_nm(name),
    county     = str_replace(name, "^Saint\\b", "St.")  # display: Saint -> St.
  ) %>%
  left_join(state_guess, by = "state_id") %>%
  left_join(
    fips_ref %>% select(state_name, county_name_norm, GEOID, statefp),
    by = c("state_name", "name_norm" = "county_name_norm")
  ) %>%
  mutate(
    price = as.numeric(gsub("\\$", "", comment))
  ) %>%
  select(
    state_id,
    state_name,
    statefp,
    GEOID,
    county,      
    comment,
    price
  )

new_data <- final_df

###Datawrapper table

dw_edit_chart(
  chart_id = gasTable,
  title = paste0('Current gas prices by county across the U.S. as of ', today_head),
  intro = "Search by county in the table below to see what the gas prices are in your area. The table is sorted from most to least expensive.",
  byline = 'Susie Webb/Get the Facts Data Team',
  source_name = 'AAA',
  source_url = 'aaa.com',
  annotate = paste0("<i>Data will update daily and represents the previous day's average cost for regular gas. Any NA values mean that there was not enough data to calculate the price.")
  )

#Adding data to the chart
dw_data_to_chart(new_data,
                 chart_id = gasTable
)

#Republishing the chart
dw_publish_chart(gasTable)

####Editing data for map


new_final_df <- final_df %>%
  mutate(
    GEOID = case_when(
      state_name == "FL" & county == "De Soto" ~ "12027", 
      state_name == "AL" & county == "De Kalb" ~ "01049", 
      state_name == "AK" & county == "Juneau" ~ "02110", 
      state_name == "AK" & county == "Prince Wales Ketchikan" ~ "02201", 
      state_name == "AK" & county == "Sitka" ~ "02220", 
      state_name == "AK" & county == "Skagway Hoonah Angoon" ~ "02232", 
      state_name == "AK" & county == "Yakutat" ~ "02282", 
      state_name == "IL" & county == "Dewitt" ~ "17039", 
      state_name == "IL" & county == "De Kalb" ~ "17037", 
      state_name == "IL" & county == "Du Page" ~ "17043", 
      state_name == "IL" & county == "La Salle" ~ "17099", 
      state_name == "IN" & county == "De Kalb" ~ "18033", 
      state_name == "IN" & county == "La Porte" ~ "18091", 
      state_name == "MD" & county == "Prince Georges" ~ "24033", 
      state_name == "MD" & county == "Queen Annes" ~ "24035", 
      state_name == "MD" & county == "St. Marys" ~ "24037", 
      state_name == "MS" & county == "De Soto" ~ "28033", 
      state_name == "TX" & county == "De Witt" ~ "48123", 
      state_name == "VA" & county == "Bristol" ~ "51520", 
      state_name == "VA" & county == "Salem" ~ "51775", 
      state_name == "MO" & county == "Sainte Genevieve" ~ "29186", 
      TRUE ~ GEOID
    ),
    county = case_when(
      state_name == "FL" & county == "De Soto" ~ "DeSoto",
      state_name == "AL" & county == "De Kalb" ~ "DeKalb",
      state_name == "IL" & county == "Dewitt" ~ "De Witt", 
      state_name == "IL" & county == "De Kalb" ~ "DeKalb",
      state_name == "IL" & county == "Du Page" ~ "DuPage", 
      state_name == "MD" & county == "Prince Georges" ~ "Prince George", 
      state_name == "MD" & county == "Queen Annes" ~ "Queen Anne", 
      state_name == "MD" & county == "St. Marys" ~ "St. Mary", 
      state_name == "MS" & county == "De Soto" ~ "DeSoto", 
      state_name == "TX" & county == "De Witt" ~ "DeWitt", 
      
      TRUE ~ county
    ))
  




###Datawrapper map
dw_edit_chart(
  chart_id = gasMap,
  title = paste0('Gas prices as of ', today_head),
  intro = 'Here are the latest gas prices by county or equivalent across the U.S.',
  byline = 'Susie Webb/Get the Facts Data Team',
  source_name = 'AAA',
  source_url = 'aaa.com',
  annotate = paste0("<i>Data will update daily and represents the previous day's average cost for regular gas. AAA only has gas data for Connecticut's old county boundaries. Any unknown or missing values means that there was not enough data to calculate the price.")
)

#Adding data to the chart
dw_data_to_chart(new_final_df,
                 chart_id = gasMap
)

#Republishing the chart
dw_publish_chart(gasMap)

###Now for state-by-state maps
dw_codes <- read_csv('codes.csv')

state_maps <- new_final_df %>%
  inner_join(dw_codes, by = c('state_name' = 'state_name'))

state_amend <- function(i){
  state_data <- state_maps %>% filter(state_name == i)
  
  code <- as.character(state_data$code[1])
  full <- state_data$name[1]
  
  
  dw_data_to_chart(state_data, code)
  
  dw_edit_chart(
    chart_id = code,
    title = paste0(full, " gas prices as of ", today_head),
    intro = paste0("Here are the latest gas prices by county or equivalent in ", full, "."),
    byline = "Susie Webb/Get the Facts Data Team",
    source_name = "AAA",
    source_url = "aaa.com",
    annotate = "<i>Data will update daily and represents the previous day's average cost for regular gas. Any unknown or missing values mean that there was not enough data to calculate the price.</i>"
  )
  
  dw_publish_chart(code)
}

for (i in unique(state_maps$state_name)) {
  state_amend(i)
}
  

#####Updating each of the tables

table_codes <- read_csv('table_codes.csv')

money_to_num <- function(x) as.numeric(str_remove(x, "\\$"))

parse_mixed_date <- function(x) {
  # handles Date, "YYYY-MM-DD", "3/5/26", "3-5-26", etc.
  if (inherits(x, "Date")) return(x)
  
  x <- as.character(x) %>% str_trim()
  x[x == ""] <- NA_character_
  
  d <- suppressWarnings(ymd(x))
  need <- is.na(d) & !is.na(x)
  
  if (any(need)) {
    x2 <- x[need] %>% str_replace_all("-", "/")
    d[need] <- suppressWarnings(mdy(x2))
  }
  d
}

scrape_state_aaa <- function(st) {
  url <- paste0("https://gasprices.aaa.com/?state=", st)
  
  page <- httr::GET(url) %>%
    httr::content("text", encoding = "UTF-8") %>%
    read_html()
  
  tab <- page %>%
    html_elements("table") %>%
    map(~ html_table(.x, fill = TRUE)) %>%
    keep(~ ncol(.x) >= 5 && any(.x[[1]] == "Current Avg.")) %>%
    pluck(1)
  
  names(tab) <- paste0("V", seq_along(tab))
  
  tab %>%
    filter(V1 %in% c("Current Avg.", "Yesterday Avg.", "Week Ago Avg.", "Month Ago Avg.", "Year Ago Avg.")) %>%
    transmute(
      state  = st,
      metric = V1,
      regular = money_to_num(V2),
      diesel  = money_to_num(V5)
    )
}

metrics_map <- function(df) {
  df %>%
    mutate(key = recode(metric,
                        "Current Avg."   = "Current",
                        "Yesterday Avg." = "Yesterday",
                        "Week Ago Avg."  = "Week",
                        "Month Ago Avg." = "Month",
                        "Year Ago Avg."  = "Year"
    )) %>%
    select(key, regular, diesel) %>%
    pivot_wider(names_from = key, values_from = c(regular, diesel)) %>%
    as.list()
}

update_one_chart_and_return_logrow <- function(state, chart_id, name) {
  Sys.sleep(0.5)
  
  m <- scrape_state_aaa(state) %>%
    metrics_map()
  
  dw_edit_chart(
    chart_id = chart_id,
    title = sprintf("%s gas prices as of %s", name, today_head),
    byline = "Susie Webb/Get the Facts Data Team",
    source_name = "AAA",
    source_url = "aaa.com",
    annotate = "<i>Data will update daily and represents the previous day's average cost for regular gas."
  )
  
  tibble(
    date = today_date,
    state = state,
    name = name,
    chart_id = chart_id,
    regular = m$regular_Current,
    diesel  = m$diesel_Current
  )
}


if (!"chart_id" %in% names(table_codes) && "chart_ids" %in% names(table_codes)) {
  table_codes <- table_codes %>% rename(chart_id = chart_ids)
}
stopifnot(all(c("state", "chart_id", "name") %in% names(table_codes)))


log_path <- "gas_data_log.csv"

gas_data_log <- if (file.exists(log_path)) {
  read_csv(log_path, show_col_types = FALSE) %>%
    mutate(date = parse_mixed_date(date))
} else {
  tibble(
    date = as.Date(character()),
    state = character(),
    name = character(),
    chart_id = character(),
    regular = double(),
    diesel = double()
  )
}


daily_log <- table_codes %>%
  select(state, chart_id, name) %>%
  pmap_dfr(~ update_one_chart_and_return_logrow(..1, ..2, ..3))

daily_new <- daily_log %>%
  anti_join(gas_data_log, by = c("date", "state"))

gas_data_log_updated <- bind_rows(gas_data_log, daily_new) %>%
  mutate(date = parse_mixed_date(date)) %>%
  arrange(date, state)

write_csv(gas_data_log_updated, log_path)


table_codes %>%
  select(state, chart_id) %>%
  pwalk(function(state, chart_id) {
    chart_data <- gas_data_log_updated %>%
      filter(state == !!state) %>%
      arrange(date) %>%
      transmute(
        Date = date,
        Regular = regular,
        Diesel = diesel
      )
    
    dw_data_to_chart(chart_data, chart_id, parse_dates = TRUE)
  })
