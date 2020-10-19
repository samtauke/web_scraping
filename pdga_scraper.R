###############################################################################################
# Scrape PDGA Website
# Sam Tauke 10/19/20
###############################################################################################
rm(list = ls(all = TRUE)) # clear all variables in the environment
setwd("C:/Users/samta/Documents/R/web_scraping")
source("C:/Users/samta/Documents/R/header.r")
library(V8)
library(usmap)




#Define Dynamic Model to Get All Players
# Iterate over pages 0-642 for 2020


pdga_reader <- function(page_number,year){
  temp_url <- paste0("https://www.pdga.com/players/stats?Year=",year,
                     "&player_Class=1&Gender=All&Bracket=All&continent=All&Country=All&StateProv=All&order=player_Rating&sort=desc&page=",
                     page_number)

  
  print(temp_url)
  
  all_fields <- read_html(temp_url) %>% 
    html_nodes(css='td') %>% 
    html_text() %>% 
    as.tibble() %>% 
    return()
}


# Define how many pages to read per year
pages_per_year <-  2


# Run back to 2000
run_vector <- seq(0,(21*pages_per_year-1)) %>% 
  as_tibble() %>% 
  mutate(
    year = 2000 + value%/%2,
    page_value = value %% 2
  )


combined_results <- mapply(pdga_reader, page_number = run_vector$page_value,year = run_vector$year)

combined_tibble <- data.frame(value=character())


iter_end <- 21*pages_per_year
for(i in 1:iter_end) {
  test <- combined_results[i] %>% bind_rows()
  combined_tibble <- combined_tibble %>% 
    bind_rows(test)
} 





# Transform Results into Usable Shape -------------------------------------
# Clean variable types
# Need to revisit when I have full data


wide_results <- combined_tibble %>% 
  mutate(
    rownum = row_number(),
    row_mod12 = rownum%%12,
    var_name = case_when(
      row_mod12==1 ~ "player",
      row_mod12==2 ~ "pdga_number",
      row_mod12==3 ~ "rating",
      row_mod12==4 ~ "year",
      row_mod12==5 ~ "gender",
      row_mod12==6 ~ "class",
      row_mod12==7 ~ "division",
      row_mod12==8 ~ "country",
      row_mod12==9 ~ "state",
      row_mod12==10 ~ "events",
      row_mod12==11 ~ "points",
      row_mod12==0 ~ "cash",
    ),
    index = (rownum+11)%/%12
  ) %>% 
  select(-rownum,-row_mod12)%>% 
  spread(
    key = "var_name",value = "value"
  ) %>% 
  mutate_at(
    vars(-index),~ trimws(gsub("\n","",.),which = "both")
  ) %>% 
  mutate(
    cash_clean_text = gsub("\\$|,","",cash),
    cash_numeric = as.numeric(cash_clean_text),
    cash_numeric = ifelse(is.na(cash_numeric),0,cash_numeric),
    rating = as.numeric(rating),
    points = as.numeric(points)
  )
  







# Plot on US MAP ----------------------------------------------------------

# Generate vector of state abbreviations


states_data <- state.abb %>% 
  bind_cols(state.name)

names(states_data) <- c("abbreviation","state")


# Filter to Only Known US States
#  -No Armed Forces
map_data <- wide_results %>% 
  filter(country=="United States" & !grepl("armed forces",state,ignore.case = T)) %>% 
  left_join(states_data,by = "state")

# Summarise Data at State Level
state_level <- map_data %>% 
  filter(division=="Open") %>% 
  group_by(state) %>% 
  summarise(
    total_dollars = sum(cash_numeric),
    num_pros = n()
    ) %>% 
  ungroup()





plot_usmap(data = state_level,regions = "states",values = "total_dollars") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Total Cash (2020)", label = scales::comma
  ) + theme(legend.position = "right") +
  ggtitle("Total Open Division Payout - 2020")+
  theme(plot.title = element_text(hjust = 0.5))


plot_usmap(data = state_level,regions = "states",values = "num_pros") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Total Players (2020)", label = scales::comma
  ) + theme(legend.position = "right") +
  ggtitle("Total Open Division Players - 2020")+
  theme(plot.title = element_text(hjust = 0.5))




