# library(tidyverse)
# library(gt)
# library(gtExtras)
# library(jsonlite)

fetch_calendar <- function() {
  
  FMP_KEY <- Sys.getenv("FMP")
  
  # get calendar for today and coming week
  url <- paste0(
    "https://financialmodelingprep.com/api/v3/economic_calendar?",
    "from=",Sys.Date(),
    "&to=",Sys.Date() + 6, 
    "&apikey=",
    FMP_KEY # key saved as repo secret 
  )
  
  calendar <- fromJSON(url)
  
  # filter to our countries and key events
  calendar <- calendar %>% 
    filter(
      country %in% c("EU", "US", "UK", "JP"),
      # take out auctions # take out government budget to GDP
      !str_detect(str_to_lower(event), "auction|natural gas|mortgage rate|^(mba|ism|eia|children|greenery) | (oil|rig) ")
    ) %>% 
    mutate(
      importance = factor(impact, levels = c("High", "Medium", "Low", "None")),
      date = with_tz(date, tzone = "Europe/London"), #change time zone to current London
      time = str_remove(date, "^.{10} "),
      date = str_extract(date, "^.{10}") %>% ymd
    ) %>% 
    relocate(time, .after = date) %>% 
    arrange(
      date, country, importance, time
    ) 
  
  # define our important events per central bank
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
      "michigan consumer sentiment"
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
      # don't keep: some superfluous items
      !str_detect(
        str_to_lower(event),
        "^gdp yoy"
      )
    ) 
  
  calendar <- calendar %>% 
    select(date:event,previous:estimate, importance) %>% 
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
        str_detect(Event, "GDP|Inflation") ~ "Data",
        str_detect(Event, "Decision") ~ "Decision"
      ),
      Time = paste0(Time, " (BST)")
    ) %>% 
    # TODO make category more comprehensive before displaying
    select(-Category)
  
  # save to github folder
  write_csv(calendar, file = "dist/calendar_this_week.csv")
  
  calendar
  
  
}



prettify_calendar <- function(calendar) {
  
  calendar_pretty <- calendar %>%
    gt() %>% 
    tab_header(
      title = "Calendar this week"
    ) %>% 
    fmt_date(
      columns = Date, date_style = "wd_m_day_year"
    ) %>%
    gt_highlight_rows(
      rows = str_detect(str_to_lower(Event), "speech"),
      fill = "#F2DFCE",
      alpha = 0.8
    )
  
  path <- "dist/calendar_pretty.html"
  # save to file
  gtsave(calendar_pretty, path)
  
  path
}