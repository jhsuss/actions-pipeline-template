# library(tidyverse)
# library(gt)
# library(gtExtras)
# library(jsonlite)
# library(httr)

fetch_calendar <- function() {
    
  # get calendar for today and coming week
  FMP_KEY <- Sys.getenv("FMP")
  
  # if today is Sat or Sun
  # if (wday(Sys.Date()) %in% c(1,7)) {
  #   days_add <- 8
  # } else {
  #   days_add <- 6
  # }
  days_add <- 14
  
  url <- paste0(
    "https://financialmodelingprep.com/api/v3/economic_calendar?",
    "from=",Sys.Date(),
    "&to=",Sys.Date() + days_add, 
    "&apikey=",
    FMP_KEY # key saved as repo secret 
  )
  headers <- add_headers("Content-Type" = "application/json")
  
  # Send request
  tryCatch({
    response <- GET(
      url,
      headers,
      timeout = 60)},
    error = function(c) {
      stop(c)
    })
  
  # Get response text
  response_text <- content(
    response, 
    as = "text", 
    encoding = "utf-8")
  
  # Check the response
  if (response$status_code != 200) {
    stop(str_glue(
      "The server responded with the error message: {response_text}"))
  }  
  
  
  calendar <- fromJSON(response_text)
  
  # filter to our countries and key events
  calendar <- calendar %>% 
    filter(
      country %in% c("EU", "US", "UK", "JP"),
      # take out auctions # take out government budget to GDP
      !str_detect(str_to_lower(event), "baker hughes total rigs|public sector net borrowing|stock investment by foreigners|(new|existing) home sales|building permits|auction|natural gas|mortgage rate|^(mba|ism|eia|children|greenery) | (oil|rig) ")
    ) %>% 
    mutate(
      importance = factor(impact, levels = c("High", "Medium", "Low", "None")),
      date = with_tz(date, tzone = "Europe/London"), #change time zone to current London
      time = str_remove(date, "^.{10} "),
      date = str_extract(date, "^.{10}") %>% ymd
    ) %>% 
    relocate(time, .after = date) %>% 
    arrange(
      date, time
    ) %>% 
    # filter out known mistakes 
    filter(
      !str_detect(
        str_to_lower(event),
        "fed kroszner|fed chair yellen"
      )
    )
  
  calendar_long <- calendar
  
  # define our important events per central bank
  # TODO improve this
  keep <- list(
    ecb = c(
      "retail sales yoy", 
      "consumer inflation expectations"
    ),
    boj = c(
      "jibun bank composite pmi",
      "summary of opinions",
      "average cash earnings yoy",
      "household spending yoy"
    ),
    fed = c(
      "economic optimism index",
      "michigan consumer sentiment",
      "jolts job"
    ),
    boe = c(
      "monetary policy report",
      "gdp 3-month avg",
      "gdp growth rate yoy"
    )
  )
  
  calendar <- calendar %>% 
    filter(
      importance == "High" |
        str_detect(str_to_lower(event), keep %>% unlist %>% str_c(collapse = "|")) |
        str_detect(
          str_to_lower(event), 
          "interest rate decision|speech" 
        ),

    ) 
  
  calendar <- calendar %>% 
    # TODO add actual for value
    select(date:event,previous:estimate) %>% 
    mutate(
      country = case_when(
        country == "US" ~ "Fed",
        country == "UK" ~ "BoE",
        country == "EU" ~ "ECB",
        country == "JP" ~ "BoJ"
      )
    ) %>% 
    rename_all( ~ str_to_title(.)) %>% 
    rename(
      `Central bank` = Country
    ) 
  
  calendar <- calendar %>%
    mutate(
      # remove some superfluous text
      Event = str_remove_all(Event, "^(Fed|BoE|BoJ|ECB) |YoY "),
      Category = case_when(
        str_detect(Event, "Speech") ~ "Comms",
        str_detect(Event, "Decision") ~ "Decision",
        TRUE ~ "Data"
      ),
      Date = format(Date,"%B %d %Y"),
      Time = paste0(str_extract(Time,"\\d{2}:\\d{2}"), " (BST)")
    ) %>% 
    mutate_at(
      vars(Previous, Estimate),
      ~ ifelse(is.na(.), "", .)
    ) %>% 
    arrange()
  
  # save to github folder
  write_csv(calendar, file = "dist/calendar_this_week.csv")
  
  calendar_long
  
  
}



prettify_calendar <- function(calendar_long) {
  
  tab <- calendar_long |> 
    select(-c(currency, unit, changePercentage,impact)) %>% 
    mutate(country = factor(country, levels = c("US","EU","UK","JP"))) %>% 
    arrange(country,importance, date) |> 
    gt(groupname_col = "country", rownames_to_stub = TRUE) |> 
    tab_stub_indent(everything(), 5)
  
  #values <- unique(calendar_long$country)
  colours <- c("#EFF3FF", "#BDD7E7","#6BAED6", "#2171B5") #RColorBrewer::brewer.pal(length(values), "Blues")
  
  for (i in seq_along(colours)) {
    tab <- tab |>
      tab_style(cell_fill(colours[i]), cells_row_groups(i))
  }
  
  calendar_pretty <- tab %>% 
    tab_header(
    title = "Calendar for the upcoming week"
    ) %>% 
    fmt_date(
      columns = date, date_style = "wd_m_day_year"
    ) %>% 
    gt_highlight_rows(
    rows = str_detect(importance, "High"),
    fill = "#F2DFCE",
    alpha = 0.8
    ) %>% 
    gt_highlight_rows(
      rows = str_detect(str_to_lower(event), "speech") & !str_detect(importance, "High"),
      fill = "#F2DFCE",
      alpha = 0.4
    ) 
  
  
  
  path <- "dist/calendar_pretty.html"
  # save to file
  gtsave(calendar_pretty, path)
  
  path
}
