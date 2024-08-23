library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)

# Read the data
data <- read.csv("Bengaluru_House_Data.csv")

# Cleaning of data
# Remove rows with NA values
data = na.omit(data)

# Remove duplicate rows based on all columns
data <- data[!duplicated(data), ]

# Modify the 'size' variable to 'bedroom'
data$bedroom <- as.numeric(gsub("[^0-9]", "", data$size))

# Remove the variable 'size'
data <- data[, -which(names(data) == "size")]

# Convert total_sqft to float
data$total_sqft <- as.numeric(gsub("[^0-9.-]", "", data$total_sqft))

# Convert bedroom to integer
data$bedroom <- as.integer(data$bedroom)

# Remove outliers based on square feet per bedroom and bathroom-bedroom ratio
data <- data %>%
  filter(total_sqft / bedroom >= 300) %>%
  filter(bath <= bedroom + 2)

# Remove the variable 'society' since it is not of importance
data <- data[, -which(names(data) == "society")]

# Clean the 'location' variable
# Remove leading and trailing white spaces
data$location <- gsub("^\\s+|\\s+$", "", data$location) 

# Convert to lowercase
data$location <- tolower(data$location)  

# Remove the variable 'availability' since without year it doesn't have much info
data <- data[, -which(names(data) == "availability")]


suppressWarnings({
  # Convert different units to square feet and extract numeric part
  data$total_sqft <- sapply(data$total_sqft, function(x) {
    # Extract numeric part
    numeric_part <- as.numeric(gsub("[^0-9.-]", "", x))
    
    # Check the unit and convert to square feet
    if (grepl("Sq. Yards", x)) {
      numeric_part * 9  # 1 Sq. Yard = 9 Sq. Feet
    } else if (grepl("Grounds", x)) {
      numeric_part * 2400  # 1 Ground = 2400 Sq. Feet
    } else if (grepl("Guntha", x)) {
      numeric_part * 1089  # 1 Guntha = 1089 Sq. Feet
    } else if (grepl("Acres", x)) {
      numeric_part * 43560  # 1 Acre = 43560 Sq. Feet
    } else if (grepl("Sq. Meter", x)) {
      numeric_part * 10.7639  # 1 Sq. Meter = 10.7639 Sq. Feet
    } else {
      numeric_part  # No conversion needed for other units
    }
  })
})

# Handle ranges
data$total_sqft <- sapply(strsplit(gsub("\\s*-\\s*", "+", data$total_sqft), "\\+"), function(x) mean(as.numeric(x)))

# Remove rows with NA values
data <- na.omit(data)

# Calculate price per square foot
data$price_per_sqft <- data$price / data$total_sqft

# Calculate location statistics after cleaning
location_stats <- data %>%
  count(location)

# Identify locations with less than or equal to 10 occurrences
location_stats_less_than_10 <- location_stats %>%
  filter(n <= 10) %>%
  pull(location)

# Replace locations with less than or equal to 10 occurrences with 'other'
data <- data %>%
  mutate(location = ifelse(location %in% location_stats_less_than_10, "other", location))

# Check the number of unique locations after transformation
unique_location_count <- data %>%
  distinct(location) %>%
  nrow()

# Calculate the average price 
average_price_lakhs <- round(mean(data$price, na.rm = TRUE), 2)

# Remove rows with NA values
data <- na.omit(data)

# Calculate the average price 
average_price_lakhs = round(mean(data$price, na.rm = TRUE), 2)

# Calculate frequencies for each area type outside the server function
area_type_counts <- data %>%
  count(area_type) %>%
  mutate(percentage = n / sum(n) * 100)

ui = dashboardPage(
  dashboardHeader(title = div("MYHOME - Bengaluru House Prices Dashboard", style = "text-align: right;", align = "right"),
                  titleWidth = 920), 
  
  dashboardSidebar(
    # Dropdown menu for selecting x-axis
    selectInput("x_axis", "Select From Bath,Bed,Balcony:",
                choices = c("Bathrooms", "Bedrooms", "Balconies"),
                selected = "Bathrooms"),
    sliderInput("top_locations", "Number of Top Locations to Include:",
                min = 1, max = 15, value = 5)
  ),
  
  dashboardBody(
    tags$style(HTML('
      .skin-blue .main-header .logo {
        background-color: #008080;
      }
      .skin-blue .main-header .navbar {
        background-color: #008080;
      }
    ')),
    # Description
    tags$div(
      class = "box",
      tags$p(
        style = "font-size: 18px; font-weight: bold; color: #333;",
        "Welcome to the Bengaluru House Prices Dashboard! As a potential home buyer, numerous factors come into play when considering the purchase of a property, but the most pivotal factor is price."
      ),
      tags$p(
        style = "font-size: 16px; color: #555; margin-top: 10px;",
        "Explore this dashboard to gain insights into Bengaluru's housing market. Delve into the distribution of house prices, understand the relationship between price and property size, and analyze how the number of bathrooms, bedrooms, or balconies influences pricing. Whether you prioritize location, amenities, or affordability, this dashboard offers valuable information to aid your home-buying journey."
      )
    ),
    
    # Info boxes
    fluidRow(
      column(width = 5, offset = 1, # Adjusted width
             infoBox("No. of Houses Available", nrow(data), icon = icon("home"), title = span(""), width = NULL)), 
      column(width = 5, # Adjusted width
             infoBox("Average Price of a House", paste0("₹", average_price_lakhs, " Lakhs"), icon = icon("inr"), color = "purple", title = span(""), width = NULL)), 
      
      # Add space between the info boxes and the plots
      tags$hr()
    ),
    
    # Graphs row
    fluidRow(
      # Price density plot
      column(width = 4,
             plotOutput("price_density"),
             # Description
             p("This plot displays the density distribution of house prices.")
      ),
      
      # Pie chart of area types
      column(width = 4,
             plotOutput("pie_chart"),
             # Description
             tags$p("This pie chart shows the distribution of different area types."),
             tags$p("The majority of the data is regarding", area_type_counts$area_type[which.max(area_type_counts$percentage)])
      ),
      
      
      # Distribution plots of bedrooms, bathrooms, and balconies
      column(width = 4,
             plotOutput("distribution_plot"),
             # Description
             p("This plot visualizes the distribution of the selected variable (e.g., bathrooms, bedrooms, or balconies).")
      )
    ),
    
    # Add space between the rows of plots
    tags$hr(),
    
    fluidRow(
      column(width=4,
             plotOutput("location_barplot",height='400px'),
             #Description
             p("This bar plot shows the Distribution of locations.")
      ),
      
      # Scatter plot of price VS total sq_feet
      column(width = 4,
             plotOutput("scatter_plot"),
             # Description
             p("This scatter plot illustrates the relationship between house prices and total square feet.")
      ),
      
      # Box plot of price by selected variable
      column(width = 4,
             plotOutput("box_plot"),
             # Description
             p("This box plot shows the distribution of house prices based on the selected variable (e.g., bathrooms, bedrooms, or balconies).")
      )
    )
  )
)



# Define server logic
server <- function(input, output) {
  
  # Reactive function to create plots based on selected x-axis
  output$distribution_plot <- renderPlot({
    # Choose x-axis based on selected input
    x_var <- switch(input$x_axis,
                    "Bathrooms" = "bath",
                    "Bedrooms" = "bedroom",
                    "Balconies" = "balcony")
    
    # Define color mapping for bars
    color_map = c(Bathrooms = "darkred", Bedrooms = "darkblue", Balconies = "darkgreen") 
    
    # Ensure x_var is treated as factor
    data[[x_var]] = factor(data[[x_var]])
    
    # Create bar plot with color based on selected x-axis
    ggplot(data, aes_string(x = x_var, fill = factor(input$x_axis))) +
      geom_bar() +
      scale_fill_manual(values = color_map) +  # Map colors based on selection
      labs(title = paste("Distribution of Number of", input$x_axis),
           x = paste("Number of", input$x_axis),
           y = "Frequency") +
      theme(plot.title = element_text(hjust = 0.5)) +
      guides(fill = FALSE)  # Remove legend
  })
  
  
  output$price_density = renderPlot({
    # Density plot of price
    ggplot(data, aes(x = price)) +
      geom_density(color = "blue") +
      labs(x = "Price", y = "Density") +
      ggtitle("Density Plot of Price") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  # Create pie chart with percentage labels
  output$pie_chart = renderPlot({
    ggplot(area_type_counts, aes(x = "", y = n, fill = area_type)) +
      geom_bar(stat = "identity", width = 1) +
      geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) +
      coord_polar("y", start = 0) +
      labs(title = "Distribution of Area Types",
           fill = "Area Type",
           x = NULL,
           y = NULL) +
      theme_void() +
      theme(legend.position = "right",plot.title = element_text(hjust = 0.5))
  })
  
  output$scatter_plot = renderPlot({
    # Scatter plot of price VS total sq_feet
    ggplot(data, aes(x = total_sqft, y = price)) +
      geom_point(color = "blue") +
      labs(x = "Total Square Feet", y = "Price") +
      ggtitle("Scatter Plot of Price VS Total square feet") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position="none")
  })
  
  output$box_plot = renderPlot({
    # Box plot of price by selected variable
    x_var = switch(input$x_axis,
                    "Bathrooms" = "bath",
                    "Bedrooms" = "bedroom",
                    "Balconies" = "balcony")
    
    # Define a single color based on selected variable
    color_map = c(Bathrooms = "darkred", Bedrooms = "darkblue", Balconies = "darkgreen")  
    box_color = color_map[[input$x_axis]]  
    
    # Ensure the variable used for box fill is a factor
    data[[x_var]] = factor(data[[x_var]])
    
    ggplot(data, aes_string(x = x_var, y = data$price, fill = factor(1))) +  # Fixed fill aesthetic
      geom_boxplot(fill = box_color) +  # Set box fill color directly
      labs(x = paste("No. of", input$x_axis), y = "Price") +
      ggtitle(paste("Box Plot of Price by", input$x_axis)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position="none")
  })
  
  # Reactive function to calculate location statistics
  location_stats = reactive({
    data %>%
      count(location) %>%
      mutate(n = as.numeric(n))  # Convert n to numeric
  })
  
  # Reactive function to create bar plot based on selected number of top locations
  output$location_barplot = renderPlot({
    # Select the top locations based on user input
    top_locations <- location_stats() %>%
      arrange(desc(n)) %>%
      slice(1:input$top_locations) %>%
      pull(location)
    
    # Filter data for top locations
    data_top_locations = data %>%
      filter(location %in% top_locations)
    
    # Reorder locations based on frequency
    data_top_locations$location = factor(data_top_locations$location, levels = top_locations)
    
    # Create bar plot
    ggplot(data_top_locations, aes(x = location)) +
      geom_bar(fill="#800080") +
      labs(title = paste("Top", input$top_locations, "Locations by Frequency"),
           x = "Location",
           y = "Frequency") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)