
# shiny_dashboard_with_impact_somalia_data --------------------------------

# the data was pulled from the Impact repository below accessed March 11th 2024: 
# https://repository.impact-initiatives.org/document/repository/5d7d7fb6/SOM1901_HSM-in-Hard-to-Reach_DAP_December-2023_External-1.xlsx

# and data methodology can be found here:
# https://repository.impact-initiatives.org/document/reach/b41baab7/SOM1901_MN_HSM-in-Somalia_December-2023_External.pdf



####
# objectives as stated in the methodology note:

# • To understand humanitarian needs (Education, Food Security, Health, Shelter,
#   Protection and WASH needs).
# • To identify which population groups are moving out of hard-to-reach settlements and
#   which population groups are unable to move from hard-to-reach settlements, and the
#   factors influencing movement dynamics.
# • To identify primary livelihoods in hard-to-reach settlements and how climatic hazards
#   (including lack of rain, drought) and economic conditions (including ways to earn
#   income) influence primary livelihoods in hard-to-reach settlements.
# • To understand what services are accessible to households in hard-to-reach settlements
#   and the barriers that impede access to services in hard-to-reach settlements.
# • To identify protection concerns in hard-to-reach settlements.
# • To understand the types of humanitarian assistance and services that people in 
#   hard-to-reach settlement have access to and constraints to accessing humanitarian assistance

####
# research questions as stated in the methodology note:

# • What are the shocks, climatic hazards and economic conditions influencing livelihoods
#   and humanitarian needs in hard-to-reach settlements and movement out of hard-to-reach settlements?
# • What are the humanitarian needs with regards to Education, Food Security, Health,
#   Shelter, Protection and WASH? How are shocks, climatic hazards and economic
#   conditions influencing these humanitarian needs? What are the barriers and negative
#   coping strategies further driving these humanitarian needs?
# • Which population groups are moving out of hard-to-reach settlements? What are the
#   factors that are driving movement out of hard-to-reach settlements? Are some population
#   groups unable to move, but would like to do so? If yes, whom?
# • To which services and types of humanitarian assistance, if any, do populations in 
#   hard-to-reach settlements have access to? What are the constraints to accessing services and
#   humanitarian assistance?

#' acronyms:
#' pnta: prefer not to answer 
#' pwd: people with disabilities
#' dnk: don't know (?) 


# libraries ---------------------------------------------------------------
library(openxlsx)
library(tidyverse)
library(shiny)
library(stringr)
library(bs4Dash)
library(reactable)
library(thematic)
library(readr)
library(readxl)
library(leaflet)
#library(tmap)
library(lubridate)
library(gtExtras)
library(geodata)
library(janitor)
library(sf)
library(ggiraph)


# reading data
somalia_data = read.xlsx('REACH_SOM_HSM-Clean-Dataset_December-2023.xlsx', sheet = 2, colNames = TRUE)

# indices of which columns contain characters.
names_char_somalia = somalia_data %>% 
  select(where(is.character)) %>% 
  names()

# this function separates the column ranges by group depending on whether the it 
# contains character variables and then includes in the range the following columns
# containing number variables. This enables splitting the data up wrt the one-hot-encoding
# and simplifies the overview of the data in view of the research questions. 

find_groups = function(data) {
  group_names = data %>% 
    select(where(is.character)) %>% 
    names()
  
  group_ranges = list()
  for (i in seq_along(group_names)) {
    start = which(names(data) == group_names[i])    # which returns the index of the matching value
    if (i < length(group_names)) {
      next_start = which(names(data) == group_names[i + 1]) - 1  # goes until next matching char_col with i + 1, then subtracts 1 to end at the right numeric
      group_ranges[[group_names[i]]] = start:next_start
    } else {
      group_ranges[[group_names[i]]] = start:ncol(data) # handles the end case
    }
  }
  
  return(group_ranges)
}


# finding groups and their ranges
somalia_group_ranges = find_groups(somalia_data)

# splitting data based on group
# the lapply function allows the dynamic sub-setting of column intervals (x being the column indices)
# drop = FALSE keeps it as dataframe. Otherwise it gets turned into a list or characters
somalia_data_grouped = lapply(somalia_group_ranges, function(x) { 
  somalia_data[, x, drop = FALSE]
})

# subsetting the data further to sort out dataframes containing only
# 1 column. This simplifies the dataset to mainly include the dataframes
# with one-hot-encoding.
# the above was the initial test but it turns out it was the dataframes with
# more that 3 columns. 
# NB ncol can be used here in sapply because it is a function over each element 
# in list, not the variable itself, where it would return NULL

over_one_col = sapply(somalia_data_grouped, function(x) ncol(x)>3)
somalia_data_grouped_onehot = somalia_data_grouped[over_one_col]

# working with lists, it is tempted to return a vector, so drop is often needed
# to keep the initial dataframe class (which often makes it easier to work with).
# in the code below if drop is changed to TRUE it will give an error because
# then it becomes a vector which drop_na() can't perform operations on. 
somalia_data_grouped[["country_moved_past30d"]][,1, drop = FALSE] %>% 
  drop_na()

# However, it is also because the 1 indexes the list why the drop is needed.
# It could also be accessed directly in the dataframe inside the list. The
# drop argument therefore seems to be specific to list syntax: 
somalia_data_grouped[["country_moved_past30d"[1]]] %>% 
  drop_na()

test = somalia_data_grouped_onehot[["main_shelter"]] %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm=TRUE)))

somalia_col_names = names(somalia_data_grouped_onehot)

somalia_data_means = list()

for (i in somalia_col_names) {
  # Calculate mean of numeric columns, removing NA values
  temp_means = somalia_data_grouped_onehot[[i]] %>%
    summarise(across(where(is.numeric), ~mean(.x, na.rm=TRUE))) %>%
    mutate(data_frame_name = i) %>%
    pivot_longer(
      cols = -data_frame_name, 
      names_to = "variable", 
      values_to = "mean_value"
    )
  
  # Store the pivoted tibble in the list
  somalia_data_means[[i]] <- temp_means
}

somalia_data_means

names(somalia_data_means) = str_replace_all(names(somalia_data_means), "_", " ")

names(somalia_data_means) = str_to_title(names(somalia_data_means))

test_name = 'reason_moved'

####
# graphics tests
somalia_data_means[['Reason Moved']] %>% 
  mutate(variable = str_replace_all(variable, "^.*/", "")) %>% 
  mutate(variable = str_replace_all(variable, "_", " ")) %>% 
  mutate(variable = case_when(
    variable == "dnk" ~"Don't know",
    variable == "pnta" ~"Prefer not to answer",
    .default = as.character(variable)
  )) %>% 
  arrange(desc(mean_value)) %>% 
  ggplot(aes(x = mean_value*100, y = reorder(variable, mean_value), fill = variable)) +
  geom_col(fill = "maroon", width = 0.7) +
  scale_x_continuous(
    limits = c(0,100), breaks = seq(0, 100, by = 10),
    labels = scales::label_number(suffix = "%")
  ) +
  labs(
    title = paste0("Bar Chart of Reason Moved"),
    x = "Category", 
    y = element_blank(),
  ) +
  theme_minimal(base_size = 20) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.title = element_text(size = 12)
  ) +
  guides(fill = FALSE) 


str_replace(somalia_data_means[['reason_moved']], "_", " ")

str_replace('reason_moved', "^.*/","")

# Example ggplot usage

# shiny_test --------------------------------------------------------------

# minimal tema som default i shiny app'en.
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 40))
thematic_shiny()

ui <- fluidPage(
  titlePanel("Share of respondant answers to the category"),
  sidebarLayout(
    sidebarPanel(
      selectInput("datasetInput", "Choose a dataset:",
                  choices = names(somalia_data_means))  
    ),
    mainPanel(
      plotOutput("barChart")
    )
  )
)


####
# Data cleaning before displaying in app.
somalia_data_means_clean = somalia_data_means

for (i in seq_along(somalia_data_means_clean)) {
  somalia_data_means_clean[[i]] = somalia_data_means_clean[[i]] %>% 
    mutate(
      variable = variable %>% str_remove("^.*/"),
      variable = variable %>% str_replace_all("_", " "),
      variable = variable %>% str_to_sentence()
    ) %>% 
    mutate(
      variable = case_when(
        variable == "Dnk" ~"Don't know",
        variable == "Pnta" ~"Prefer not to answer",
        .default = as.character(variable)
      )
    )
}


####
# server function

server <- function(input, output) {
  # Reactive expression to get the selected dataset
  selected_data <- reactive({
    data <- somalia_data_means[[input$datasetInput]] %>%
      arrange(desc(mean_value))
    
    # Modify variable to remove everything up to and including the '/'
    # NB: maybe it is better to do this kind of data-processing before the app. 
    data$variable = str_remove(data$variable, "^.*/")
    data$variable = str_replace_all(data$variable, "_", " ")
    data$variable = str_to_title(data$variable)
    
    data
  })
  
  # Render the bar chart based on the selected dataset
  output$barChart <- renderPlot({
    data <- selected_data()
    ggplot(data, aes(x = mean_value*100, y = reorder(variable, mean_value), fill = variable)) +
      geom_col(width = 0.7, fill = 'cornflowerblue') +
      scale_x_continuous(
        limits = c(0,100), breaks = seq(0, 100, by = 10),
        labels = scales::label_number(suffix = "%")
      ) +
      labs(
        title = paste(input$datasetInput),
        x = element_blank(),
        y = element_blank()
      ) +
      theme_minimal(base_size = 20) +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank()
      ) +
      guides(fill = FALSE)
  })
}


shinyApp(ui = ui, server = server)



# Map of somalia  ---------------------------------------------------------


# Get the administrative boundaries for Somalia
somalia_districts <- geodata::gadm(country = "SOM", level = 1, path = tempdir()) |> 
  # Convert to an sf object
  st_as_sf() |> 
  # Nicer output
  as_tibble() |> 
  janitor::clean_names()

# Display the data frame
somalia_districts

# Plot the map
somalia_districts |> 
  ggplot(aes(geometry = geometry)) +
  geom_sf(
    aes(fill = name_1),  # Assuming 'name_2' contains the district names
    color = 'black',
    linewidth = 0.2
  ) +
  geom_sf(
    fill = NA,
    color = 'black',
    linewidth = 0.1
  ) +
  labs(title = "Districts of Somalia")

