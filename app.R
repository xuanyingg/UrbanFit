library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(fontawesome)# To use Font Awesome icons
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(ggrepel)
library(geosphere)  # for calculating distances between latitude/longitude points
library(jsonlite)
library(plotly)
library(RColorBrewer)
library(DT)

# Load and clean the datasets
classpass <- read.csv("classpass_dataset.csv", stringsAsFactors = FALSE)
othergyms <- read.csv("gyms_dataset.csv", stringsAsFactors = FALSE)
simulation_result <- read.csv("Simulation_result_by_pa.csv")
district_data <- st_read("district_and_planning_area.geojson")
sports_data <- read.csv("Project_Sports_Simulation_Data.csv")

# Merge the two datasets by matching 'PA' with 'planning_area'
entrepreneurs_data <- merge(district_data, simulation_result, by.x = "planning_area", by.y = "PA")

# Standardize column names in both datasets
names(classpass) <- tolower(names(classpass))
names(othergyms) <- tolower(names(othergyms))
names(classpass) <- gsub("\\.", "_", names(classpass))  # Replace dots with underscores
names(othergyms) <- gsub("\\.", "_", names(othergyms))  # Replace dots with underscores

# Correct column names in 'othergyms' to match 'classpass'
if ("gym_name" %in% names(othergyms)) {
  names(othergyms)[names(othergyms) == "gym_name"] <- "gymname"
}

# Determine common activity columns in both datasets
activity_columns <- c("barre", "boxing", "cycling", "dance", "gym_time", 
                      "martial_arts", "pilates", "strength_training", 
                      "stretching", "yoga")

# Define icons for each activity
activity_icons <- list(
  "barre" = "dumbbell",
  "boxing" = "hand-rock",
  "cycling" = "bicycle",
  "dance" = "music",
  "gym_time" = "dumbbell",
  "martial_arts" = "hand-rock",
  "pilates" = "spa",
  "strength_training" = "weight-hanging",
  "stretching" = "running",
  "yoga" = "spa"
)


# Define major gym brands in Othergyms
major_brands <- c("FitnessFirst", "ActiveSG Gym", "Anytime Fitness")
brand_colors <- c("FitnessFirst" = "purple", "ActiveSG Gym" = "deeppink", 
                  "Anytime Fitness" = "cyan")

# Assign colors based on major brands
othergyms <- othergyms %>%
  mutate(
    source = "Othergyms",
    brand = ifelse(gymname %in% major_brands, gymname, "Other"),
    color = ifelse(brand %in% names(brand_colors), brand_colors[brand], "red")
  )

# Ensure selected columns exist in both datasets
existing_columns <- intersect(activity_columns, names(classpass))

# Combine datasets into one unified dataset
combined_gyms <- bind_rows(
  classpass %>%
    select(gymname, region, all_of(existing_columns),  lat, long, nearest_mrt_station, nearest_bus_stop, distance_from_mrt_km, distance_from_busstop_km) %>%
    mutate(source = "Classpass"),
  othergyms %>%
    select(gymname, region, all_of(existing_columns), lat, long, student_price, adult_price, senior_price, source, brand, color, nearest_mrt_station, nearest_bus_stop, distance_from_mrt_km, distance_from_busstop_km) %>%
    mutate(source = "Othergyms")
)
combined_gyms$Activities <- apply(combined_gyms[activity_columns], 1, function(row) {
  # Get the names of activities that have a value of 1 in the row
  selected_activities <- activity_columns[row == 1]
  # Convert to Title Case and paste with commas
  paste(tools::toTitleCase(gsub("_", " ", selected_activities)), collapse = ", ")
})

# List of all activities for filtering
all_activities <- gsub("_", " ", existing_columns)
all_activities <- tools::toTitleCase(all_activities)

# Get unique regions for the region filter
all_regions <- unique(combined_gyms$region)

# Define credit values for each activity
activity_credits <- c(
  "barre" = 3,
  "boxing" = 5,
  "cycling" = 3,
  "dance" = 4,
  "gym_time" = 1,
  "martial_arts" = 5,
  "pilates" = 2,
  "strength_training" = 3,
  "stretching" = 3,
  "yoga" = 4
)

# Define broad regions and their corresponding planning areas
area_mapping <- list(
  "North" = c("Woodlands", "Yishun", "Sembawang"),
  "North-East" = c("Hougang", "Sengkang", "Punggol", "Serangoon", "Ang Mo Kio"),
  "East" = c("Bedok", "Tampines", "Pasir Ris", "Marine Parade"),
  "West" = c("Jurong East", "Jurong West", "Bukit Batok", "Choa Chu Kang", "Clementi"),
  "Central" = c("Orchard", "Toa Payoh", "Bukit Timah", "Novena", "Queenstown", "Downtown Core")
)

ui <- dashboardPage(
  dashboardHeader(
    title = div(
      style = "font-family: 'Lilita One', sans-serif; font-weight: bold; font-size: 24px;",
      HTML("<span style='color: black;'>Urban</span><span style='color: grey;'>Fit</span>")
    ),
    tags$li(class = "dropdown",
            style = "position: absolute; right: 160px; top: 10px;",
            actionButton("go_gym_goers", 
                         label = HTML("<i class='fa fa-heart'></i> Gym-goers"),
                         class = "btn btn-primary",
                         style = "margin-right: 10px;")
    ),
    tags$li(class = "dropdown",
            style = "position: absolute; right: 20px; top: 10px;",
            actionButton("go_entrepreneurs", 
                         label = HTML("<i class='fa fa-briefcase'></i> Entrepreneurs"),
                         class = "btn btn-primary")
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Gym-goers", tabName = "gym_goers", icon = icon("heart")),
      menuItem("Entrepreneurs", tabName = "entrepreneurs", icon = icon("briefcase")),
      menuItem("ClassPass Calculator", tabName = "classpass_calculator", icon = icon("calculator")),
      menuItem("Further Analysis", tabName = "further_analysis", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
      tags$link(rel = "stylesheet", 
                href = "https://fonts.googleapis.com/css2?family=Lilita+One&family=Montserrat:wght@400;700&display=swap"),
      tags$style(HTML("
        body, .content-wrapper, .right-side, .main-footer {
          font-family: 'Montserrat', sans-serif;
        }
        h1, h2, h3, h4, h5, h6, label {
          font-weight: bold;
        }
        .btn-primary {
          background-color: #333333;
          color: #ffffff;
          font-weight: bold;
          border: none;
          border-radius: 5px;
          padding: 5px 10px;
        }
        .btn-primary:hover {
          background-color: #555555;
        }
        .gym-grid {
          display: grid;
          grid-template-columns: repeat(2, 1fr);
          gap: 15px;
          overflow-y: auto;
          max-height: 600px;
        }
        .gym-card {
          display: flex;
          align-items: center;
          padding: 10px;
          border: 1px solid #ccc;
          border-radius: 5px;
          background-color: #f8f8f8;
        }
        .gym-logo {
          width: 60px;
          height: 60px;
          margin-right: 10px;
          background-color: #ccc;
          border-radius: 50%;
          display: flex;
          justify-content: center;
          align-items: center;
          font-weight: bold;
          color: #fff;
          font-size: 18px;
        }
        .gym-info {
          flex: 1;
        }
      "))
    ),
    tabItems(
        # Dashboard tab-----------------------------------------------------------------------
      tabItem(tabName = "dashboard",
              h2("Dashboard Main Page"),
              textInput("search", "Search for gyms:", value = "", placeholder = "e.g., Real Yoga, Pure Fitness"),
              selectInput("region_filter", "Select Region:", choices = c("All Region" = "All Region", unique(combined_gyms$region))),
              checkboxGroupInput("activity_filter", "Filter by Activities:", 
                                 choices = all_activities, inline = TRUE),
              actionButton("clear_filters", "Clear All Filters", icon = icon("times"), 
                           style = "margin-bottom: 20px; background-color: red; color: white; border: none;"),
              textOutput("gym_count"),  # Text output to show the number of gyms
              div(class = "gym-grid", uiOutput("gym_cards"))
      ),
      
      # Gym-goers tab---------------------------------------------------------------------
      tabItem(tabName = "gym_goers", 
              h2("Gym-goers"), 
              actionButton("clear_filters", "Clear All Filters", icon = icon("times"), 
                           style = "margin-bottom: 20px; background-color: red; color: white; border: none;"),
              # Top row: Filters for Region, MRT and Price
              fluidRow(
                column(4,
                       box(
                         title = "Filter by Region", width = NULL, status = "primary",
                         selectInput("region_filter", "Select Region:", 
                                     choices = c("All Region" = "All Region", unique(combined_gyms$region)),
                                     selected = "All Region")
                       )
                ),
                column(4, 
                       box(
                         title = "Filter by Nearest MRT", width = NULL, status = "primary",
                         selectInput("mrt_filter", "Select Nearest MRT:", 
                                     choices = c("All MRT", unique(combined_gyms$nearest_mrt_station)),
                                     selected = "All MRT")  
                       )
                ),
                column(4, 
                       box(title = "Filter by Price", width = NULL, status = "primary",
                           sliderInput("priceRange", 
                                       label = "Price Range", 
                                       min = 0, max = 500, 
                                       value = c(0,500), 
                                       step = 1)
                       )
                )
              ),
                
              # Bottom Row: Map on the left and other filters on the right
              fluidRow(
                column(8, 
                       leafletOutput("gymMap", height = 600),
                       p("*All prices are per month", style = "text-align: right; font-style: italic; color: red;")
                ),
                column(4, 
                       box(
                         title = "Filter by Gym Brands", width = NULL, status = "primary",
                         checkboxGroupInput("brand_filter", "Select Gym Brands:", 
                                            choices = c("Classpass",unique(combined_gyms$brand[!is.na(combined_gyms$brand)])))
                       ), 
                       box(
                         title = "Filter by Activities", width = NULL, status = "primary",
                         checkboxGroupInput("activity_filter", "Select Activities:", 
                                            choices = all_activities)
                       ), 
                  
                       box(
                         title = "Classpass Credit Plans", width = NULL, status = "primary",
                         p("8 credits - $20/month"),
                         p("27 credits - $65/month"),
                         p("45 credits - $99/month"),
                         p("68 credits - $149/month"),
                         p("90 credits - $189/month"),
                         p("142 credits - $299/month"),
                         p("150 credits - $315/month")
                       )
                       )
              )
      ),
                       
      # Entrepreneur tab---------------------------------------------------------------------
      tabItem(
        tabName = "entrepreneurs", 
        h2("Entrepreneurs' Planning Region Heatmap"),
        
        # Custom CSS to style the box header with a dark gray background
        tags$style(HTML("
    .box-header {
      background-color: #505050 !important;  /* Dark Gray background */
      color: white !important;                /* White text color */
    }
    
    /* Styling for the slider */
    .irs-bar, .irs-bar-edge {
      background: #505050 !important;  /* Dark Gray for the slider track */
    }
    .irs-single {
      background: #505050 !important;  /* Dark Gray for the slider handle */
    }
    .irs-from, .irs-to {
      background: #505050 !important;  /* Dark Gray for the 'from' and 'to' labels */
    }
    .irs-slider {
      border: 2px solid #505050 !important;  /* Dark Gray border for the slider */
    }
  ")),
        
        # Top row: Total visits, Price
        fluidRow(
          column(6, # First filter column with a 6 width (half of the row)
                 box(
                   title = "Select Total Visits Range", width = NULL, status = "primary", solidHeader = TRUE, 
                   sliderInput("visits_filter", "Select Total Visits Range:",
                               min = min(entrepreneurs_data$Total_Visits, na.rm = TRUE),
                               max = max(entrepreneurs_data$Total_Visits, na.rm = TRUE),
                               value = range(entrepreneurs_data$Total_Visits, na.rm = TRUE))
                 )
          ),
          column(6, # Second filter column with a 6 width (half of the row)
                 box(
                   title = "Filter by Price", width = NULL, status = "primary", solidHeader = TRUE,
                   sliderInput("entrepreneur_price_filter",  # Changed to entrepreneur_price_filter
                               label = "Price Range", 
                               min = 0, max = 300, 
                               value = c(0, 300), 
                               step = 1)
                 )
          )
        ),
        
        
        # Bottom Row: Map on the left and other filters on the right
        fluidRow(
          column(8, 
                 # Leaflet output for the heat map
                 leafletOutput("entrepreneurs_map", height = "600px"),
                 p("*All prices are per month", style = "text-align: right; font-style: italic; color: red;")
          ),
          column(4, 
                 box(
                   title = "Filter by Region", width = NULL, status = "primary", solidHeader = TRUE,
                   selectInput("entrepreneur_region_filter", 
                               "Select Region:",
                               choices = c("All Regions", unique(c(othergyms$region, classpass$region))), 
                               selected = "All Regions")
                 ),
                 box(
                   title = "Filter by Nearest MRT", width = NULL, status = "primary", solidHeader = TRUE,
                   selectInput("entrepreneur_mrt_filter", "Select Nearest MRT:", 
                               choices = c("All MRT", unique(combined_gyms$nearest_mrt_station)),
                               selected = "All MRT")  # Set default value to "All MRT"
                 ), 
                 box(
                   title = "Filter by Gym Brands", width = NULL, status = "primary", solidHeader = TRUE,
                   checkboxGroupInput("entrepreneur_brand_filter", "Select Gym Brands:", 
                                      choices = c("Classpass", "Gym"),
                                      selected = c("Classpass", "Gym"))  # Set default selection to both options
                 ),
                 
                 box(
                   title = "Filter by Activities", width = NULL, status = "primary", solidHeader = TRUE,
                   checkboxGroupInput("activity_filter", "Select Activities:", 
                                      choices = all_activities)
                 ), 
                 
                 box(
                   title = "Classpass Credit Plans", width = NULL, status = "primary", solidHeader = TRUE,
                   p("8 credits - $20/month"),
                   p("27 credits - $65/month"),
                   p("45 credits - $99/month"),
                   p("68 credits - $149/month"),
                   p("90 credits - $189/month"),
                   p("142 credits - $299/month"),
                   p("150 credits - $315/month")
                 )
          )
        )
      ),
      
      # Classpass Calculator--------------------------------------------------------
      tabItem(
        tabName = "classpass_calculator",
        h2("ClassPass Calculator"),
        p("Want to know whether your monthly ClassPass is worth it?"),
        
        # CSS for the recommendation box
        tags$style(HTML("
    .recommendation-box {
      padding: 15px;
      border: 2px solid #007bff; /* Blue border */
      background-color: #e9f5ff; /* Light blue background */
      color: #007bff; /* Text color */
      font-weight: bold;
      font-size: 16px;
      margin-top: 10px;
      border-radius: 5px;
    }
  ")),
        
        fluidRow(
          column(6, 
                 numericInput("monthly_visits", 
                              "Estimated Monthly Gym Visits:", 
                              value = 8, 
                              min = 1)),
          column(6, 
                 tags$div(
                   tags$label("Select Your Preferred Activities:"),
                   fluidRow(
                     column(6, checkboxGroupInput("selected_activities_left", 
                                                  label = NULL,
                                                  choices = names(activity_credits)[1:5])),
                     column(6, checkboxGroupInput("selected_activities_right", 
                                                  label = NULL,
                                                  choices = names(activity_credits)[6:10]))
                   )
                 )
          )
        ),
        
        actionButton("calculate", "Calculate"),
        br(),
        
        h3("Recommendations"),
        p("Play around with the filters to see whether your ClassPass plan is worth it."),
        textOutput("non_classpass_cost"),
        textOutput("total_credits_needed"),  # Display the total credits needed for visits
        uiOutput("recommendation_message"),  # Change to uiOutput for HTML formatting
        
        # Display ClassPass plans
        h4("Available ClassPass Plans"),
        tableOutput("classpass_plans_table"),  # Table to display the plans
        
        # Plot for cost per gym visit by activity
        plotOutput("activity_cost_plot")
      ), 
      
      
      # Further Analysis tab----------------------------------------------------------------------
      tabItem(tabName = "further_analysis", 
              h2("Further Analysis"),
              tabsetPanel(
                tabPanel("Population", 
                         selectInput("ageGroup", "Select Age Group:", choices = c("All" = "All", "0-19" = "0-19", "20-54" = "20-54", "55+" = "55+")),
                         
                         h4("Top Regions by Age Groups"),
                         leafletOutput("populationMap"),
                         br(),
                         h4("Population Distribution by Age Groups"),
                         plotOutput("populationBarChart"),
                         uiOutput("agegroupStatement")
                ),
                
                
                
                tabPanel("Regions", 
                         h3("Gym Locations"),
                         
                         # Dropdown to select a broad area (North, East, etc.), including "All"
                         fluidRow(
                           column(4,
                                  selectInput("area_filter", "Select Area:", 
                                              choices = c("All", names(area_mapping)),
                                              selected = "All")
                           )
                         ),
                         br(),
                         
                         # Plot for the number of gyms in each selected area
                         h4("Total Number of Gyms in Selected Area"),
                         plotlyOutput("totalGymsChart"),
                         p("Central region has the most number of gyms, followed by North-East, West and East. North region has the least number of gyms."),
                         br(),
                         
                         # Plot for gym distribution within the selected area
                         h4("Gym Distribution by Region"),
                         plotlyOutput("regionDistributionChart"),
                         uiOutput("regionDistributionStatement"),
                         br(),
                         
                         # Table for the top 5 gyms
                         h4("Top Regions by Gym Count"),
                         DT::dataTableOutput("topGymTable"),
                         br(),
                         
                        
                         h4("Top Gym Names in Selected Regions"),
                         DT::dataTableOutput("topGymNamesTable"),
                         br(),
                         
                         h4("Average Gym Membership Price by Region"),
                         plotlyOutput("priceChart"),
                         uiOutput("priceStatement"),
                         br(),
                         
                         h4("Total Activities Distribution in Selected Regions"),
                         plotlyOutput("activitiesChart"),
                         uiOutput("activitiesStatement"),
                         br(),
                         
                         # Plot for activities by region
                         h4("Distribution of Activities by Region"),
                         plotlyOutput("activitiesByRegionChart"),
                         uiOutput("activitiesByRegionStatement")
                ),
                
                tabPanel(
                  "Activities",
                  h3("Gym Activities and Prices Insights"),
                  
                  sidebarLayout(
                    position = "right",  # Move sidebar to the right
                    
                    sidebarPanel(
                      # Dropdown for selecting activities
                      checkboxGroupInput(
                        inputId = "selected_activities",
                        label = "Select Activities:",
                        choices = tools::toTitleCase(gsub("_", " ", activity_columns)),
                        selected = NULL
                      ),
                      
                      # Checkbox for selecting price types
                      checkboxGroupInput(
                        inputId = "selected_price_types",
                        label = "Select Price Types:",
                        choices = c("Student Price", "Adult Price", "Senior Price"),
                        selected = c("Student Price", "Adult Price", "Senior Price")  # Default: select all
                      )
                    ),
                    
                    mainPanel(
                      plotOutput("activityPricePlot"),
                      br(),
                      
                      h3("Gyms Names with pricing List"),
                      DTOutput("activityPriceTable"),
                      br(),
                      
                      h3("Recommended Pricing"),
                      DTOutput("recommendedPricingTable")
                    )
                  )
                ) #end of Regions tab
                  
                
   
              )
      )
    ) # Close tabItems
  ), # Close dashboardBody 
  skin = "black"
)


server <- function(input, output, session) {
  observeEvent(input$go_gym_goers, {
    updateTabItems(session, "tabs", "gym_goers")
  })
  observeEvent(input$go_entrepreneurs, {
    updateTabItems(session, "tabs", "entrepreneurs")
  })
  
  observeEvent(input$clear_filters, {
    updateSelectInput(session, "region_filter", selected = "All Region")
    updateSelectInput(session, "mrt_filter", selected = "All MRT")
    updateSliderInput(session, "priceRange", value = c(0, 500))
    updateCheckboxGroupInput(session, "brand_filter", selected = character(0))
    updateCheckboxGroupInput(session, "activity_filter", selected = character(0))
    updateTextInput(session, "search", value = "")
  })
  
  filtered_gyms <- reactive({
    # Capture search term, selected region, selected activities, selected brands, selected MRT and selected price from user input
    selected_region <- input$region_filter
    search_term <- tolower(input$search)
    selected_activities <- input$activity_filter
    selected_brands <- input$brand_filter
    selected_mrt <- input$mrt_filter  
    selected_price_range <- input$priceRange
    
    # Start with the entire dataset
    gyms <- combined_gyms
    
    print("Number of gyms before filtering:")
    print(nrow(gyms))
    
    
    # Step 2: Apply region filter if a specific region is selected
    if (selected_region != "All Region" && selected_region != "") {
      gyms <- gyms %>% filter(region == selected_region)
      print("After region filter:")
      print(nrow(gyms))
    }
    
    
    # Step 1: Apply search term filter
    if (search_term != "") {
      gyms <- gyms %>%
        filter(grepl(search_term, tolower(gymname)))
      print("After search term filter:")
      print(nrow(gyms))
    }
    
    
    
    # Step 3: Apply brand filter if specific brands are selected
    if (!is.null(selected_brands) && length(selected_brands) > 0) {
      gyms <- gyms %>%
        filter((brand %in% selected_brands) | (source == "Classpass" & "Classpass" %in% selected_brands))
      print("After brand filter:")
      print(nrow(gyms))
    }
    
    # Step 4: Apply activity filters only if specific activities are selected
    if (length(selected_activities) > 0) {
      # Convert selected activities to corresponding column names
      selected_activity_columns <- gsub(" ", "_", tolower(selected_activities))
      selected_activity_columns <- selected_activity_columns[selected_activity_columns %in% names(gyms)]
      
      if (length(selected_activity_columns) > 0) {
        # Filter gyms where all selected activity columns have a value of 1
        gyms <- gyms %>%
          filter(if_any(all_of(selected_activity_columns), ~ . == 1))
        print("After activity filter:")
        print(nrow(gyms))
      }
    }
    
    # Step 5: Apply Nearest MRT filter if a specific MRT station is selected
    if (selected_mrt != "All MRT" && selected_mrt != "") {
      gyms <- gyms %>% filter(nearest_mrt_station == selected_mrt)
      print("After MRT filter:")
      print(nrow(gyms))
    }
  
    
    # Step 6: Apply Price Range filter for adult prices
    if (!is.null(selected_price_range) && length(selected_price_range) == 2) {
      # Include gyms with NA prices if the slider is set to the full range
      if (selected_price_range[1] == 0 && selected_price_range[2] == 500) {
        gyms <- gyms  # No price filtering if the full range is selected
      } else {
        gyms <- gyms %>% filter(
          is.na(adult_price) |  # Include NA prices
            (adult_price >= selected_price_range[1] & adult_price <= selected_price_range[2])
        )
      }
      print("After price range filter:")
      print(nrow(gyms))
    }
    
    gyms  # Return the filtered gyms dataset
  })
  
  
  
  
  # Display the count of filtered gyms
  output$gym_count <- renderText({
    paste("Total gyms found:", nrow(filtered_gyms()))
  })
  
  # Display gym cards with listed activities and relevant icons
  output$gym_cards <- renderUI({
    gyms <- filtered_gyms()
    
    if (nrow(gyms) == 0) {
      return(div("No results found", style = "color: red; font-weight: bold;"))
    }
    
    # Create gym cards with activities listed
    gym_boxes <- lapply(1:nrow(gyms), function(i) {
      # Extract activities where the value is 1 for the current gym
      gym_activities <- activity_columns[gyms[i, activity_columns] == 1]
      gym_activities <- gsub("_", " ", gym_activities)  # Replace underscores with spaces
      gym_activities <- tools::toTitleCase(gym_activities)  # Capitalize activity names
      
      # Choose an icon for the first activity listed
      primary_activity <- activity_columns[gyms[i, activity_columns] == 1][1]
      activity_icon <- activity_icons[[primary_activity]]
      
      div(class = "gym-card",
          div(class = "gym-logo", icon(activity_icon, class = "fa-lg")),  # Show icon based on activity
          div(class = "gym-info",
              p(strong(gyms$gymname[i])),
              p(paste("Region:", gyms$region[i])),
              p(paste("Nearest MRT:", gyms$nearest_mrt_station[i])),  # Add Nearest MRT info
              p(strong("Activities:"), paste(gym_activities, collapse = ", ")),
              actionButton(inputId = paste0("details_", i), label = "View Details", onclick = sprintf("Shiny.setInputValue('selected_gym', %d)", i))
          )
      )
    })
    
    do.call(tagList, gym_boxes)
  })
  
  # Update the gym map based on selected gym
  observeEvent(input$selected_gym, {
    req(input$selected_gym)
    
    # Retrieve the selected gym's information
    gym <- filtered_gyms()[input$selected_gym, ]
    
    # Switch to the gym_goers tab
    updateTabItems(session, "tabs", "gym_goers")
    
    # Update the map to highlight the selected gym
    output$gymMap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addCircleMarkers(
          lng = as.numeric(gym$long), lat = as.numeric(gym$lat),
          color = "red", radius = 8, fillOpacity = 1,
          popup = paste("<strong>Gym Name:</strong>", gym$gymname, "<br>",
                        "<strong>Region:</strong>", gym$region, "<br>",
                        "<strong>Nearest MRT Station:</strong>", gym$nearest_mrt_station, "<br>",
                        "<strong>Nearest Bus Stop:</strong>", gym$nearest_bus_stop)
        ) %>%
        setView(lng = as.numeric(gym$long), lat = as.numeric(gym$lat), zoom = 15)
    })
  })
  
  # Observe filter changes and reset the gym map
  observe({
    # Trigger this observer when any of the filters are changed
    input$region_filter
    input$mrt_filter
    input$priceRange
    input$activity_filter
    
    # Reset the map to show all gyms based on the filtered data
    gyms <- filtered_gyms()
      
    
    output$gymMap <- renderLeaflet({
      leaflet(gyms) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~as.numeric(long), lat = ~as.numeric(lat),
          color = ~ifelse(source == "Classpass", "blue", color),
          radius = 6,
          popup = ~paste(
            "<strong>Gym Name:</strong> ", gymname, "<br>",
            "<strong>Region:</strong> ", region, "<br>",
            "<strong>Student Price:</strong> ", student_price, "<br>",
            "<strong>Adult Price:</strong> ", adult_price, "<br>",
            "<strong>Senior Price:</strong> ", senior_price, "<br>",
            "<strong>Nearest MRT Station:</strong> ", round(distance_from_mrt_km, 2), "km from", nearest_mrt_station, "<br>",
            "<strong>Nearest Bus Stop:</strong> ", round(distance_from_busstop_km, 2), "km from", nearest_bus_stop, "<br>",
            "<strong>Activities:</strong>", Activities
          ),
          label = ~gymname
        ) %>%
        addLegend(position = "bottomright",
                  colors = c("blue", "purple", "deeppink", "cyan", "red"),
                  labels = c("Classpass", "Fitness First", "ActiveSG", "Anytime Fitness", "Other"),
                  title = "Gym Types")
    })
  })
  
  # Gym-goers Analysis TAB-----------------------------------------------------------------------
  
  
  
  
  # Render Leaflet map on Gym-goers tab with filtered data
  output$gymMap <- renderLeaflet({
    gyms <- filtered_gyms()
    
    # Check if the gyms data is empty
    if (nrow(gyms) == 0) {
      # If no gyms match the filter, show a basic map with a default view (centered on Singapore)
      leaflet() %>%
        addTiles() %>%
        setView(lng = 103.8198, lat = 1.3521, zoom = 12) %>%
        addPopups(lng = 103.8198, lat = 1.3521, popup = "No gyms found matching the filter criteria.")
    } else {
      # If there are gyms, show them on the map with the filtered data
    leaflet(filtered_gyms()) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~long, lat = ~lat,
        color = ~ifelse(source == "Classpass", "blue", color),
        radius = 6,
        popup = ~paste("<strong>Gym Name:</strong>", gymname, "<br>",
                       "<strong>Region:</strong>", region, "<br>",
                       ifelse(source == "Classpass", "<strong>Pricing:</strong> Refer to Classpass credits",
                              paste("<strong>Student Price:</strong> $", student_price, "<br>",
                                    "<strong>Adult Price:</strong> $", adult_price, "<br>",
                                    "<strong>Senior Price:</strong> $", senior_price,"<br>",
                                    "<strong>Nearest MRT Station:</strong> ", round(distance_from_mrt_km, 2), "km from", nearest_mrt_station, "<br>",
                                    "<strong>Nearest Bus Stop:</strong> ", round(distance_from_busstop_km, 2), "km from", nearest_bus_stop))),
        label = ~ifelse(source == "Classpass",
                        paste("Gym Name:", gymname, "(", region, ")"),
                        paste("Gym Name:", gymname, "(", region, ")", "- Adult Price: $", adult_price)),
        labelOptions = labelOptions(direction = "auto", opacity = 0.8)
      ) %>%
      addLegend(position = "bottomright",
                colors = c("blue", "purple", "deeppink", "cyan", "red"),
                labels = c("Classpass", "Fitness First", "ActiveSG", "Anytime Fitness", "Other"),
                title = "Gym Types")
    }
  })
  
  # Entrepreneurs -------------------------------------------------------------------------------
  
  # Clean data (ensure no missing lat/long values)
  gyms_clean <- othergyms %>% filter(!is.na(lat), !is.na(long))
  classpass_clean <- classpass %>% filter(!is.na(lat), !is.na(long))
  
  gyms_clean$Activities <- apply(gyms_clean[activity_columns], 1, function(row) {
    selected_activities <- activity_columns[row == 1]
    paste(tools::toTitleCase(gsub("_", " ", selected_activities)), collapse = ", ")
  })
  
  classpass_clean$Activities <- apply(classpass_clean[activity_columns], 1, function(row) {
    selected_activities <- activity_columns[row == 1]
    paste(tools::toTitleCase(gsub("_", " ", selected_activities)), collapse = ", ")
  })
  
  # Reactive expression to filter data
  filtered_entrepreneurs_data <- reactive({
    # Step 1: Filter entrepreneurs_data based on Total Visits range
    entrepreneurs_filtered <- entrepreneurs_data %>%
      filter(Total_Visits >= input$visits_filter[1],
             Total_Visits <= input$visits_filter[2])
    
    # Step 2: Filter based on selected region (if not "All Regions")
    if (input$entrepreneur_region_filter != "All Regions") {
      entrepreneurs_filtered <- entrepreneurs_filtered %>%
        filter(planning_area == input$entrepreneur_region_filter)
    }
    
    # Step 3: Filter gyms based on price range
    gyms_filtered_by_price <- gyms_clean %>%
      filter(adult_price >= input$entrepreneur_price_filter[1],
             adult_price <= input$entrepreneur_price_filter[2])
    
    # Step 4: Filter gyms based on nearest MRT station
    if (input$entrepreneur_mrt_filter != "All MRT") {
      gyms_filtered_by_price <- gyms_filtered_by_price %>%
        filter(nearest_mrt_station == input$entrepreneur_mrt_filter)
      classpass_filtered <- classpass_clean %>%
        filter(nearest_mrt_station == input$entrepreneur_mrt_filter)
    } else {
      # If "All MRT" is selected, do not apply the MRT filter
      classpass_filtered <- classpass_clean
    }
    
    # Step 5: Filter gyms and classpass based on selected activities
    if (length(input$activity_filter) > 0) {
      # Filter gyms by activities: Check if any selected activity exists in the Activities column
      gyms_filtered_by_price <- gyms_filtered_by_price %>%
        filter(sapply(Activities, function(activity) {
          any(sapply(input$activity_filter, function(selected_activity) {
            grepl(selected_activity, activity, ignore.case = TRUE)
          }))
        }))
      
      # Filter classpass by activities
      classpass_filtered <- classpass_filtered %>%
        filter(sapply(Activities, function(activity) {
          any(sapply(input$activity_filter, function(selected_activity) {
            grepl(selected_activity, activity, ignore.case = TRUE)
          }))
        }))
    }
    
    
    # Step 5: Extract valid regions that meet the filter criteria
    valid_regions <- entrepreneurs_filtered$planning_area
    
    # Step 6: Filter gyms and classpass datasets based on valid regions and price & MRT station
    othergyms_filtered <- gyms_filtered_by_price %>% 
      filter(region %in% valid_regions)
    classpass_filtered <- classpass_filtered %>% 
      filter(region %in% valid_regions)
    
    list(entrepreneurs_filtered = entrepreneurs_filtered, 
         othergyms = othergyms_filtered, 
         classpass = classpass_filtered)
  })
  
  # Render Leaflet Map
  output$entrepreneurs_map <- renderLeaflet({
    data <- filtered_entrepreneurs_data()  # Fetch filtered data
    
    # Base leaflet map with OpenStreetMap tiles
    map <- leaflet(data$entrepreneurs_filtered) %>%
      addTiles()
    
    # Add polygons for planning regions with color intensity based on Total_Visits
    map <- map %>%
      addPolygons(
        fillColor = ~colorNumeric("YlOrRd", entrepreneurs_data$Total_Visits)(Total_Visits), 
        color = "#BDBDC3", weight = 1, fillOpacity = 0.7, 
        popup = ~paste(planning_area, ": ", Total_Visits, " visits")
      ) %>%
      addLegend(
        pal = colorNumeric("YlOrRd", entrepreneurs_data$Total_Visits), 
        values = entrepreneurs_data$Total_Visits, 
        title = "Total Visits", opacity = 1
      )
    
    # Check if "Classpass" is selected
    if ("Classpass" %in% input$entrepreneur_brand_filter) {
      map <- map %>%
        addCircleMarkers(
          data = data$classpass, 
          ~long, ~lat, 
          radius = 6, 
          color = "orange", 
          fillColor = "orange", 
          fillOpacity = 0.7, 
          popup = ~paste(
            "<strong>Gym Name:</strong> ", gymname, "<br>",
            "<strong>Region:</strong> ", region, "<br>",
            "<strong>Pricing:</strong> ClassPass Credits<br>",
            "<strong>Nearest MRT:</strong> ", nearest_mrt_station, "<br>",
            "<strong>Nearest Bus Stop:</strong> ", nearest_bus_stop, "<br>",
            "<strong>Activities:</strong>", Activities
          )
        )
    }
    
    # Check if "Gym" is selected
    if ("Gym" %in% input$entrepreneur_brand_filter) {
      map <- map %>%
        addCircleMarkers(
          data = data$othergyms, 
          ~long, ~lat, 
          radius = 6, 
          color = "purple", 
          fillColor = "purple", 
          fillOpacity = 0.7, 
          popup = ~paste(
            "<strong>Gym Name:</strong> ", gymname, "<br>",
            "<strong>Region:</strong> ", region, "<br>",
            "<strong>Student Price:</strong> ", student_price, "<br>",
            "<strong>Adult Price:</strong> ", adult_price, "<br>",
            "<strong>Senior Price:</strong> ", senior_price, "<br>",
            "<strong>Nearest MRT Station:</strong> ", round(distance_from_mrt_km, 2), "km from", nearest_mrt_station, "<br>",
            "<strong>Nearest Bus Stop:</strong> ", round(distance_from_busstop_km, 2), "km from", nearest_bus_stop, "<br>",
            "<strong>Activities:</strong>", Activities
          )
        )
    }
    
    map  # Return the updated map object
  })
  
  # Classpass Calculator-------------------------------------------------------
  # ClassPass plans as a data frame for easy display
  classpass_plans_df <- data.frame(
    Plan = c("8 credits - $20/month", "27 credits - $65/month", "45 credits - $99/month",
             "68 credits - $149/month", "90 credits - $189/month", "142 credits - $299/month",
             "150 credits - $315/month"),
    Credits = c(8, 27, 45, 68, 90, 142, 150),
    Cost = c(20, 65, 99, 149, 189, 299, 315)
  )
  
  # Reactive expression for total credits needed
  total_credits_needed <- reactive({
    if (length(selected_activities_combined()) == 0) {
      return(list(total = "N/A", min = NA, max = NA, avg = NA))
    }
    
    monthly_visits <- input$monthly_visits
    activity_credits_selected <- unlist(activity_credits[names(activity_credits) %in% selected_activities_combined()])
    
    if (length(activity_credits_selected) == 1) {
      total_credits <- activity_credits_selected * monthly_visits
      list(total = total_credits, min = total_credits, max = total_credits, avg = total_credits)
    } else {
      min_credits <- min(activity_credits_selected) * monthly_visits
      max_credits <- max(activity_credits_selected) * monthly_visits
      avg_credits <- (min_credits + max_credits) / 2
      list(total = paste(min_credits, "to", max_credits), min = min_credits, max = max_credits, avg = avg_credits)
    }
  })
  
  # Reactive expression for average non-ClassPass cost
  avg_non_classpass_cost <- reactive({
    if (length(selected_activities_combined()) == 0) {
      return(NA)
    }
    
    filtered_data() %>%
      filter(if_any(all_of(tolower(selected_activities_combined())), ~ . == 1)) %>%
      summarise(avg_cost = mean(adult_price, na.rm = TRUE)) %>%
      pull(avg_cost)
  })
  
  # Display ClassPass plans in a table with recommendation checkmark if available
  output$classpass_plans_table <- renderTable({
    # Avoid calculations if no activities are selected
    if (is.na(total_credits_needed()$min)) {
      return(classpass_plans_df %>% 
               mutate(`Estimated Spare Credits Left` = NA, Recommended = ""))
    }
    
    # Calculate spare credits based on the average needed credits
    avg_credits_needed <- total_credits_needed()$avg
    if (is.na(avg_credits_needed)) {
      return(classpass_plans_df %>% 
               mutate(`Estimated Spare Credits Left` = NA, Recommended = ""))
    }
    
    # Determine if a recommendation exists
    recommended_plan <- classpass_plans_df %>%
      mutate(`Estimated Spare Credits Left` = Credits - avg_credits_needed) %>%
      filter(Cost < avg_non_classpass_cost() & Credits >= avg_credits_needed) %>%
      arrange(abs(`Estimated Spare Credits Left`)) %>%
      head(1)
    
    # If there's no recommendation, return the table without any ticks
    if (nrow(recommended_plan) == 0) {
      return(classpass_plans_df %>% 
               mutate(`Estimated Spare Credits Left` = Credits - avg_credits_needed, Recommended = ""))
    }
    
    # Add a checkmark to the recommended plan
    classpass_plans_df %>%
      mutate(
        `Estimated Spare Credits Left` = Credits - avg_credits_needed,
        Recommended = ifelse(Plan == recommended_plan$Plan, "✔", "")
      ) %>%
      select(Plan, Credits, Cost, `Estimated Spare Credits Left`, Recommended)
  })
  
  # Display the recommendation message
  output$recommendation_message <- renderUI({
    avg_credits_needed <- total_credits_needed()$avg
    
    # Check if `avg_credits_needed` is available before proceeding
    if (is.na(avg_credits_needed)) {
      return(HTML("<div class='recommendation-box'>Please select at least one activity to calculate the recommendation.</div>"))
    }
    
    # Filter and recommend the closest plan to the average credits needed
    recommended_plan <- classpass_plans_df %>%
      mutate(`Estimated Spare Credits Left` = Credits - avg_credits_needed) %>%
      arrange(abs(`Estimated Spare Credits Left`)) %>%
      filter(Cost < avg_non_classpass_cost() & Credits >= avg_credits_needed) %>%
      head(1) %>%
      pull(Plan)
    
    if (length(recommended_plan) > 0) {
      HTML(paste0(
        "<div class='recommendation-box'>",
        "Based on your inputs, we recommend the <b>", recommended_plan, "</b> ClassPass plan.",
        "</div>"
      ))
    } else {
      HTML("<div class='recommendation-box'>Non-ClassPass may be more economical based on your estimated visits.</div>")
    }
  })
  
  
  # Display the total credits needed
  output$total_credits_needed <- renderText({
    total_credits <- total_credits_needed()$total
    if (total_credits == "N/A") {
      "Please select at least one activity to calculate total credits needed."
    } else {
      paste("Total Credits Needed for Visits:", total_credits)
    }
  })
  
  # Display the non-ClassPass cost
  output$non_classpass_cost <- renderText({
    if (is.na(avg_non_classpass_cost())) {
      "Please select at least one activity to calculate the non-ClassPass cost."
    } else {
      paste("Non-ClassPass Monthly Cost: $", round(avg_non_classpass_cost(), 2))
    }
  })
  
  
  
  
  # Combine selected activities from both columns into a single reactive vector
  selected_activities_combined <- reactive({
    c(input$selected_activities_left, input$selected_activities_right)
  })
  
  # Reactive data for filtered gyms based on selected activities
  filtered_data <- reactive({
    req(selected_activities_combined())  # Require at least one activity selected
    
    # Filter gyms with selected activities and non-missing prices
    combined_gyms %>%
      filter(if_any(all_of(tolower(selected_activities_combined())), ~ . == 1)) %>%  # Filter gyms with selected activities
      filter(!is.na(adult_price))  # Remove gyms without a price
  })
  
  # Render the boxplot for average monthly cost by activity across all gyms
  output$activity_cost_plot <- renderPlot({
    # Check if there are any selected activities
    if (is.null(selected_activities_combined()) || length(selected_activities_combined()) == 0) {
      return(NULL)  # Return NULL if no activities are selected
    }
    
    # Transform the data for selected activities, without filtering by gym availability
    activity_data <- combined_gyms %>%
      select(gymname, adult_price, all_of(tolower(selected_activities_combined()))) %>%
      pivot_longer(cols = tolower(selected_activities_combined()), names_to = "activity", values_to = "available") %>%
      filter(available == 1, !is.na(adult_price))  # Filter for selected activities and non-missing prices
    
    # Generate boxplot for each activity across all gyms
    ggplot(activity_data, aes(x = activity, y = adult_price, fill = activity)) +
      geom_boxplot() +
      labs(
        x = "Activity",
        y = "Monthly Cost ($)",
        title = "Monthly Cost by Activity Across All Gyms"
      ) +
      theme_minimal() +
      theme(legend.position = "none")  # Hide legend as it's redundant with x-axis labels
  })
  

  
  
  
  
  #Further Analysis TAB--------------------------------------------------------------------------
  
  # Population Tab -------------------------------------------------------------------------------
  # Load and preprocess the population data for Leaflet map
  population <- read.csv("./Population.csv")
  population <- population %>%
    mutate(Age_Group_Number = case_when(
      Age_Group == "0-19" ~ "0-19",
      Age_Group == "20-54" ~ "20-54",
      Age_Group == "55 and above" ~ "55+",
      TRUE ~ NA_character_
    ))
  
  # Aggregate population by Planning Area (PA) and Age Group, including "All" for total population
  population_agg <- population %>%
    group_by(PA, Age_Group_Number) %>%
    summarise(Total_Pop = sum(Pop, na.rm = TRUE), .groups = "drop")  # Ensure groups are dropped
  
  # Add total population for each Planning Area
  total_population <- population %>%
    group_by(PA) %>%
    summarise(Total_Pop = sum(Pop, na.rm = TRUE)) %>%
    mutate(Age_Group_Number = "All")
  
  # Combine age group data with total population data
  population_agg <- bind_rows(population_agg, total_population)
  
  # Load spatial data for Singapore planning areas
  sg_areas <- st_read("./2-planning-area.geojson")
  population_agg$PA <- toupper(trimws(population_agg$PA))
  sg_areas$name <- toupper(trimws(sg_areas$name))
  
  # Perform a left join to merge population data into spatial data
  sg_population <- sg_areas %>%
    left_join(population_agg, by = c("name" = "PA")) %>%
    filter(!is.na(Total_Pop))  # Remove any areas with no population data if necessary
  
  # Render the Leaflet map
  output$populationMap <- renderLeaflet({
    age_group_label <- input$ageGroup  # "All" can be used to indicate total population
    if (is.null(age_group_label)) return(NULL)
    
    age_group_data <- sg_population %>%
      filter(Age_Group_Number == age_group_label)
    
    palette <- colorNumeric(palette = "Blues", domain = age_group_data$Total_Pop, na.color = "#D3D3D3")
    
    leaflet(age_group_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~palette(Total_Pop),
        color = "black",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~paste0("Planning Area: ", name),
        popup = ~paste0("<strong>Planning Area:</strong> ", name, "<br>",
                        "<strong>Total Population:</strong> ", format(Total_Pop, big.mark = ","))
      ) %>%
      addLegend(pal = palette, values = ~Total_Pop, opacity = 0.7,
                title = "Population Density", position = "bottomright")
  })
  

  # Add Bar graph below the map----------
  # Render the bar chart for population
  output$populationBarChart <- renderPlot({
    age_group_label <- input$ageGroup  # "All" can be used to indicate total population
    if (is.null(age_group_label)) return(NULL)
    
    # Filter the data based on the selected age group or total population
    bar_chart_data <- sg_population %>%
      filter(Age_Group_Number == age_group_label) %>%
      arrange(desc(Total_Pop))
    
    # Identify the top 5 regions by population
    bar_chart_data$highlight <- ifelse(bar_chart_data$name %in% head(bar_chart_data$name, 5), "Top 5", "Other")
    
    # Create the ggplot bar chart, highlighting the top 5 regions
    ggplot(bar_chart_data, aes(x = reorder(name, -Total_Pop), y = Total_Pop, fill = highlight)) +
      geom_bar(stat = "identity") +
      # Use hex codes for custom colors
      scale_fill_manual(values = c("Top 5" = "#FFB6C1", "Other" = "#A9A9A9")) +  # Light pink for top 5, dark gray for others
      labs(
        title = paste("Population Distribution by Region for Age Group:", age_group_label),
        x = "Planning Area",
        y = "Total Population"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +  # Hide legend
      # Format the y-axis labels to display in K, M, or B
      scale_y_continuous(labels = function(x) {
        ifelse(x >= 1e9, paste0(round(x / 1e9, 1), "B"),
               ifelse(x >= 1e6, paste0(round(x / 1e6, 1), "M"),
                      ifelse(x >= 1e3, paste0(round(x / 1e3, 1), "K"), as.character(x))))
      })
  })
  
  # Age group statement --------------
  output$agegroupStatement <- renderUI({
    age_group_label <- input$ageGroup
    
    # Handle the case when "All" is selected or when a specific age group is selected
    if (age_group_label == "All") {
      # Summarize population for all age groups combined
      top_regions_data <- population_agg %>%
        filter(Age_Group_Number == "All") %>%
        arrange(desc(Total_Pop)) %>%
        slice(1:5)
      
      age_group_text <- "all age groups combined"
    } else {
      # Summarize population for the selected age group
      top_regions_data <- population_agg %>%
        filter(Age_Group_Number == age_group_label) %>%
        arrange(desc(Total_Pop)) %>%
        slice(1:5)
      
      age_group_text <- paste("age group", age_group_label)
    }
    
    # Extract the names of the top 5 regions
    top_regions <- top_regions_data$PA
    
    # Create the dynamic statement
    if (length(top_regions) > 0) {
      statement <- paste(
        "For <span style='color:#00008B; font-weight: bold;'>", age_group_text, 
        "</span>, the top 5 regions with the highest population are: <span style='color:#00008B; font-weight: bold;'>", 
        paste(top_regions, collapse = ", "), "</span>."
      )
    } else {
      statement <- "<span style='color:#00008B; font-weight: bold;'>No population data available for the selected age group.</span>"
    }
    
    # Return the statement as HTML
    HTML(statement)
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Regions filter Gyms TAB-------------------------------------------------------------------------------
  
  
  
  # Define a unique color for each area
  area_colors <- list(
    "North" = "rgb(144, 238, 144)",      # Pastel green
    "North-East" = "rgb(173, 216, 230)", # Pastel blue
    "East" = "rgb(255, 228, 181)",       # Mustard yellow
    "West" = "rgb(255, 182, 193)",       # Pastel pink
    "Central" = "rgb(221, 160, 221)"     # Pastel purple
  )
  
  # Helper function to get the color for each region
  get_region_color <- function(region) {
    for (area in names(area_mapping)) {
      if (region %in% area_mapping[[area]]) {
        return(area_colors[[area]])
      }
    }
    return("rgb(169, 169, 169)")  # Grey if not in any specific area
  }
  
  # Filtered data based on the selected area
  filtered_data <- reactive({
    if (input$area_filter == "All") {
      othergyms
    } else {
      selected_area <- area_mapping[[input$area_filter]]
      othergyms %>%
        filter(region %in% selected_area)
    }
  })
  
  
  # Plot for gym distribution by specific region within the selected area
  output$regionDistributionChart <- renderPlotly({
    gym_distribution <- filtered_data() %>%
      group_by(region) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    # Calculate the top 25% of regions by gym count
    num_regions <- nrow(gym_distribution)
    num_highlight <- max(1, round(num_regions * 0.25))  # At least 1 region is highlighted
    top_regions <- gym_distribution$region[1:num_highlight]  # Get top regions' names
    
    # Assign colors based on area for top regions, grey for others
    gym_distribution <- gym_distribution %>%
      mutate(color = ifelse(region %in% top_regions, 
                            sapply(region, get_region_color), 
                            "rgb(169, 169, 169)"))  # Grey for non-highlighted regions
    
    plot_ly(gym_distribution, x = ~region, y = ~count, type = "bar", 
            marker = list(color = ~color)) %>%
      layout(
        title = paste("Gym Distribution in", input$area_filter, "Area"),
        xaxis = list(title = "Region"),
        yaxis = list(title = "Number of Gyms")
      )
  })
  output$regionDistributionStatement <- renderUI({
    region_data <- filtered_data() %>% group_by(region) %>% summarise(count = n())
    highest <- region_data %>% filter(count == max(count)) %>% pull(region)
    lowest <- region_data %>% filter(count == min(count)) %>% pull(region)
    avg_count <- round(mean(region_data$count), 0)
    
    statement <- paste(
      "In the <span style='color:#00008B; font-weight: bold;'>", input$area_filter, "</span> area, ",
      "<span style='color:#00008B; font-weight: bold;'>", paste(highest, collapse = ", "), "</span> has the most gyms, and ",
      "<span style='color:#00008B; font-weight: bold;'>", paste(lowest, collapse = ", "), "</span> has the fewest gyms. ",
      "The average number of gyms is <span style='color:#00008B; font-weight: bold;'>", avg_count, "</span>."
    )
    
    HTML(statement)
  })
  
  # Table for the top regions by gym count (highlighted regions only)
  output$topGymTable <- DT::renderDataTable({
    gym_distribution <- filtered_data() %>%
      group_by(region) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    # Calculate the top 25% of regions
    num_highlight <- max(1, round(nrow(gym_distribution) * 0.25))
    top_gym_distribution <- head(gym_distribution, num_highlight)
    
    # Render the top regions in a DataTable format
    DT::datatable(top_gym_distribution, 
                  options = list(pageLength = 5, autoWidth = TRUE),
                  colnames = c("Region", "Number of Gyms"))
  })
  
  # Plot for total number of gyms in all regions with highlighted selected area
  output$totalGymsChart <- renderPlotly({
    total_gyms <- othergyms %>%
      mutate(area = case_when(
        region %in% area_mapping$North ~ "North",
        region %in% area_mapping$`North-East` ~ "North-East",
        region %in% area_mapping$East ~ "East",
        region %in% area_mapping$West ~ "West",
        region %in% area_mapping$Central ~ "Central",
        TRUE ~ "Other"
      )) %>%
      group_by(area) %>%
      summarise(total_count = n()) %>%
      filter(area != "Other")  # Remove any unmatched areas
    
    # Highlight selected area in pastel blue and others in grey
    total_gyms <- total_gyms %>%
      mutate(color = ifelse(
        area == input$area_filter,
        "rgb(173, 216, 230)",  # Pastel blue for selected area
        "rgb(169, 169, 169)"   # Grey for others
      ))
    
    plot_ly(total_gyms, x = ~area, y = ~total_count, type = "bar", 
            marker = list(color = ~color)) %>%
      layout(
        title = paste("Total Number of Gym in", input$area_filter, "Area"),
        xaxis = list(title = "Area"),
        yaxis = list(title = "Total Gyms")
      )
  })
  

# Plot for average price in selected regions with top highlighted
output$priceChart <- renderPlotly({
  price_data <- filtered_data() %>%
    group_by(region) %>%
    summarise(average_price = mean(adult_price, na.rm = TRUE)) %>%
    arrange(desc(average_price))
  
  # Determine the top regions to highlight
  num_highlight <- max(1, round(nrow(price_data) * 0.25))  # Highlight top 25%
  top_regions <- price_data$region[1:num_highlight]
  
  # Assign color to each region
  price_data <- price_data %>%
    mutate(color = ifelse(region %in% top_regions, 
                          sapply(region, get_region_color), 
                          "rgb(169, 169, 169)"))  # Grey for non-highlighted regions
  
  plot_ly(price_data, x = ~region, y = ~average_price, type = "bar",
          marker = list(color = ~color)) %>%
    layout(
      title = paste("Average Gym Membership Price in", input$area_filter, "Area"),
      xaxis = list(title = "Region"),
      yaxis = list(title = "Average Price")
    )
})


output$priceStatement <- renderUI({
  price_data <- filtered_data() %>% group_by(region) %>% summarise(average_price = mean(adult_price, na.rm = TRUE))
  highest <- price_data %>% filter(average_price == max(average_price)) %>% pull(region)
  lowest <- price_data %>% filter(average_price == min(average_price)) %>% pull(region)
  avg_price <- round(mean(price_data$average_price, na.rm = TRUE), 0)
  
  statement <- paste(
    "The highest average gym membership price in <span style='color:#00008B; font-weight: bold;'>", input$area_filter, "</span> is in ",
    "<span style='color:#00008B; font-weight: bold;'>", paste(highest, collapse = ", "), "</span>, and the lowest is in ",
    "<span style='color:#00008B; font-weight: bold;'>", paste(lowest, collapse = ", "), "</span>. The average price across regions is $",
    "<span style='color:#00008B; font-weight: bold;'>", avg_price, "</span>."
  )
  
  HTML(statement)
})

# Plot for activities distribution in selected regions with top highlighted
output$activitiesChart <- renderPlotly({
  # Check if the activity columns exist in the filtered data
  available_activity_columns <- intersect(activity_columns, names(filtered_data()))
  
  # Proceed only if there are matching activity columns in the filtered data
  if (length(available_activity_columns) > 0) {
    activities_data <- filtered_data() %>%
      select(all_of(available_activity_columns)) %>%
      summarise(across(everything(), sum, na.rm = TRUE)) %>%
      pivot_longer(cols = everything(), names_to = "activity", values_to = "count") %>%
      arrange(desc(count))
    
    # Determine the top activities to highlight
    num_highlight <- max(1, round(nrow(activities_data) * 0.25))  # Highlight top 25%
    top_activities <- activities_data$activity[1:num_highlight]
    
    # Determine color for highlighting based on the selected region
    if (input$area_filter == "All") {
      highlight_color <- "rgb(255, 182, 193)"  # Light grey for "All"
    } else {
      highlight_color <- area_colors[[input$area_filter]]
    }
    
    # Assign color to each activity based on top highlight status and area color
    activities_data <- activities_data %>%
      mutate(color = ifelse(activity %in% top_activities, 
                            highlight_color, "rgb(169, 169, 169)"))  # Highlighted with region color, grey for others
    
    plot_ly(activities_data, x = ~activity, y = ~count, type = "bar",
            marker = list(color = ~color)) %>%
      layout(
        title = paste("Activities Distribution in", input$area_filter, "Area"),
        xaxis = list(title = "Activity"),
        yaxis = list(title = "Number of Gyms Offering Activity")
      )
  } else {
    # Return an empty plot if no activity columns are found
    plot_ly() %>%
      layout(
        title = paste("Activities Distribution in", input$area_filter, "Area"),
        xaxis = list(title = "Activity"),
        yaxis = list(title = "Number of Gyms Offering Activity"),
        annotations = list(
          list(x = 0.5, y = 0.5, text = "No activities data available", showarrow = FALSE)
        )
      )
  }
})
output$activitiesStatement <- renderUI({
  available_activity_columns <- intersect(activity_columns, names(filtered_data()))
  if (length(available_activity_columns) > 0) {
    activities_data <- filtered_data() %>%
      select(all_of(available_activity_columns)) %>%
      summarise(across(everything(), sum, na.rm = TRUE)) %>%
      pivot_longer(cols = everything(), names_to = "activity", values_to = "count") %>%
      arrange(desc(count))
    
    highest <- activities_data %>% filter(count == max(count)) %>% pull(activity)
    lowest <- activities_data %>% filter(count == min(count)) %>% pull(activity)
    avg_count <- round(mean(activities_data$count), 0)
    
    statement <- paste(
      "In the selected area, the most common activity is <span style='color:#00008B; font-weight: bold;'>", 
      paste(highest, collapse = ", "), "</span> and the least common is <span style='color:#00008B; font-weight: bold;'>", 
      paste(lowest, collapse = ", "), "</span>. On average, there are <span style='color:#00008B; font-weight: bold;'>", 
      avg_count, "</span> gyms offering each activity."
    )
    HTML(statement)
  } else {
    HTML("<span style='color:#00008B; font-weight: bold;'>No activities data available.</span>")
  }
})


# Table for top gym names in selected regions
output$topGymNamesTable <- DT::renderDataTable({
  top_gyms <- filtered_data() %>%
    group_by(region, gymname) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
  
  # Render the top gym names in a DataTable format, 5 entries per page
  DT::datatable(top_gyms, 
                options = list(pageLength = 5, autoWidth = TRUE),
                colnames = c("Region", "Gym Name", "Number of Locations"))
})

# Define the server logic for the plot and statement
# Plot for activities distribution by region as a stacked bar chart
output$activitiesByRegionChart <- renderPlotly({
  # Check if the activity columns exist in the filtered data
  available_activity_columns <- intersect(activity_columns, names(filtered_data()))
  
  if (length(available_activity_columns) > 0) {
    # Summarize the count of each activity for each region
    activities_data <- filtered_data() %>%
      group_by(region) %>%
      summarise(across(all_of(available_activity_columns), sum, na.rm = TRUE)) %>%
      pivot_longer(cols = available_activity_columns, names_to = "activity", values_to = "count")
    
    # Create a stacked bar chart where x-axis is region, y-axis is count, and fill is by activity
    plot_ly(activities_data, x = ~region, y = ~count, type = "bar", color = ~activity,
            colors = "Set3", hoverinfo = "text", text = NULL,  # Remove any top text on the bars
            hovertext = ~paste("Activity:", activity, "<br>Count:", count),
            showlegend = TRUE) %>%
      layout(
        barmode = "stack",  # Stack the bars by activity
        title = paste("Distribution of Activities by Region in", input$area_filter, "Area"),
        xaxis = list(title = "Region"),
        yaxis = list(title = "Number of Gyms Offering Activity", standoff = 20),
        legend = list(orientation = "v", x = 1.05, y = 1, xanchor = "left"),  # Position the legend on the right side
        annotations = list(
          list(
            x = 1.05, y = 1, xref = "paper", yref = "paper",
            text = "Tip: Double-click to isolate an activity, Shift-click to compare multiple activities",
            showarrow = FALSE, font = list(size = 10, color = "gray")
          )
        )
      )
  } else {
    # Return an empty plot if no activity columns are found
    plot_ly() %>%
      layout(
        title = paste("Distribution of Activities by Region in", input$area_filter, "Area"),
        xaxis = list(title = "Region"),
        yaxis = list(title = "Number of Gyms Offering Activity"),
        annotations = list(
          list(x = 0.5, y = 1.5, text = "No activities data available", showarrow = FALSE)
        )
      )
  }
})



# Generate insights for the selected area
output$activitiesByRegionStatement <- renderUI({
  available_activity_columns <- intersect(activity_columns, names(filtered_data()))
  
  if (length(available_activity_columns) > 0) {
    activities_data <- filtered_data() %>%
      group_by(region) %>%
      summarise(across(all_of(available_activity_columns), sum, na.rm = TRUE)) %>%
      pivot_longer(cols = -region, names_to = "activity", values_to = "count") %>%
      arrange(desc(count))
    
    # Find the region with the highest total count of activities
    region_summary <- filtered_data() %>%
      group_by(region) %>%
      summarise(total_count = sum(across(all_of(available_activity_columns)), na.rm = TRUE)) %>%
      arrange(desc(total_count))
    
    highest_region <- region_summary %>% slice(1) %>% pull(region)
    
    # Find the top 3 most common activities overall
    top_activities <- activities_data %>%
      group_by(activity) %>%
      summarise(total_count = sum(count, na.rm = TRUE)) %>%
      arrange(desc(total_count)) %>%
      slice(1:3) %>%
      pull(activity)
    
    # Find the majority activity across regions (most frequent activity)
    majority_activity <- activities_data %>%
      group_by(activity) %>%
      summarise(frequency = sum(count > 0)) %>%  # Count regions where each activity is present
      arrange(desc(frequency)) %>%
      slice(1) %>%
      pull(activity)
    
    statement <- paste(
      "In the <span style='color:#00008B; font-weight: bold;'>", input$area_filter, 
      "</span> area, the region with the highest number of activities is <span style='color:#00008B; font-weight: bold;'>",
      highest_region, "</span>. The top activities are <span style='color:#00008B; font-weight: bold;'>", 
      paste(top_activities, collapse = ", "), "</span>. The majority of regions have <span style='color:#00008B; font-weight: bold;'>", 
      majority_activity, "</span> as a common activity."
    )
    
    HTML(statement)
  } else {
    HTML("<span style='color:#00008B; font-weight: bold;'>No activities data available.</span>")
  }
})





# Activities ----------------------------------------------------------------------------
# Server Part: Plot for Activity Prices
output$activityPricePlot <- renderPlot({
  # Filter the gym_prices_long based on user input
  gym_prices_long_filtered <- combined_gyms %>%
    pivot_longer(cols = c(student_price, adult_price, senior_price), 
                 names_to = "price_type", 
                 values_to = "price") %>%
    filter(!is.na(price))
  
  # If no activity is selected, display all activities
  selected_activities <- if (length(input$selected_activities) == 0) {
    tools::toTitleCase(gsub("_", " ", activity_columns))
  } else {
    input$selected_activities
  }
  
  # Filter activities based on user input
  activity_data_long_filtered <- combined_gyms %>%
    pivot_longer(cols = all_of(activity_columns), 
                 names_to = "activity", 
                 values_to = "has_activity") %>%
    filter(has_activity == 1, 
           tools::toTitleCase(gsub("_", " ", activity)) %in% selected_activities)
  
  # Merge price data and activity data
  activity_price_data_filtered <- gym_prices_long_filtered %>%
    inner_join(activity_data_long_filtered, by = "gymname") %>%
    mutate(price_type = case_when(
      price_type == "student_price" ~ "Student Price",
      price_type == "adult_price" ~ "Adult Price",
      price_type == "senior_price" ~ "Senior Price"
    )) %>%
    filter(price_type %in% input$selected_price_types)  # Filter price type based on user input
  
  # Define pastel color palette for the price types
  pastel_colors <- c("Student Price" = "#FFD1DC",   # Pastel Pink
                     "Adult Price" = "#AEC6CF",     # Pastel Blue
                     "Senior Price" = "#FFB347")    # Pastel Orange
  
  # Plot the filtered data with pastel colors
  ggplot(activity_price_data_filtered, aes(x = activity, y = price, fill = price_type)) +
    geom_bar(stat = "summary", fun = "mean", position = "dodge") +
    scale_fill_manual(values = pastel_colors) +
    labs(
      title = "Average Price by Activity and Price Type",
      x = "Activity",
      y = "Average Price",
      fill = "Price Type"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
})

# Server Part: Table for Activity Prices
output$activityPriceTable <- renderDT({
  # If no activity is selected, display all activities
  selected_activities <- if (length(input$selected_activities) == 0) {
    tools::toTitleCase(gsub("_", " ", activity_columns))
  } else {
    tools::toTitleCase(gsub("_", " ", input$selected_activities))
  }
  
  if (length(selected_activities) > 0) {
    
    # Combine data for gymname, activity, and prices
    combined_data <- combined_gyms %>%
      pivot_longer(cols = all_of(activity_columns), names_to = "activity", values_to = "has_activity") %>%
      filter(has_activity == 1, tools::toTitleCase(gsub("_", " ", activity)) %in% selected_activities) %>%
      select(gymname, activity, student_price, adult_price, senior_price) %>%
      filter(!is.na(student_price) | !is.na(adult_price) | !is.na(senior_price)) %>%
      distinct() %>%
      group_by(gymname) %>%
      summarise(
        activities = paste(activity, collapse = ", "), 
        student_price = paste(unique(student_price), collapse = ", "),
        adult_price = paste(unique(adult_price), collapse = ", "),
        senior_price = paste(unique(senior_price), collapse = ", "),
        .groups = 'drop'
      ) %>%
      arrange(gymname)
    
    # Render the combined table with activities grouped by gymname
    datatable(combined_data, options = list(pageLength = 5), rownames = FALSE,
              caption = "Gyms offering selected activities with Prices (Student, Adult, Senior)")
    
  } else {
    NULL
  }
})


# Server Part: Recommended Pricing Table for Selected Activities
output$recommendedPricingTable <- renderDT({
  # If no activity is selected, display all activities
  selected_activities <- if (length(input$selected_activities) == 0) {
    tools::toTitleCase(gsub("_", " ", activity_columns))
  } else {
    tools::toTitleCase(gsub("_", " ", input$selected_activities))
  }
  
  if (length(selected_activities) > 0) {
    
    # Combine data for gymname, activity, and prices
    combined_data <- combined_gyms %>%
      pivot_longer(cols = all_of(activity_columns), names_to = "activity", values_to = "has_activity") %>%
      filter(has_activity == 1, tools::toTitleCase(gsub("_", " ", activity)) %in% selected_activities) %>%
      select(gymname, activity, student_price, adult_price, senior_price) %>%
      filter(!is.na(student_price) | !is.na(adult_price) | !is.na(senior_price)) %>%
      distinct()
    
    # Add a new column for human-readable activity names
    combined_data <- combined_data %>%
      mutate(activity_name = tools::toTitleCase(gsub("_", " ", activity)))
    
    # Group activities and calculate Min-Max Price Ranges
    activity_price_ranges <- combined_data %>%
      summarise(
        activities = paste(unique(activity_name), collapse = ", "),
        student_price_range = paste0("$", min(student_price, na.rm = TRUE), " - $", max(student_price, na.rm = TRUE)),
        adult_price_range = paste0("$", min(adult_price, na.rm = TRUE), " - $", max(adult_price, na.rm = TRUE)),
        senior_price_range = paste0("$", min(senior_price, na.rm = TRUE), " - $", max(senior_price, na.rm = TRUE)),
        .groups = 'drop'
      )
    
    # Render the recommended pricing table
    datatable(activity_price_ranges, options = list(pageLength = 5), rownames = FALSE,
              caption = "Recommended Pricing Range (Min-Max) for Selected Activities (Student, Adult, Senior)")
    
  } else {
    NULL
  }
})



}

shinyApp(ui, server)
