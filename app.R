#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
#library(plotly)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinythemes)

library(BVAR)
library(readxl) 
library(dplyr)

library(DT)

# Read the Excel file using the correct file path 
gdp <- read_excel("ROUTPUTQvQd.xlsx", col_names = TRUE)
# Convert all columns except the first one (which contains dates) to numeric, keeping NA values as NA 
gdp_num <- gdp[, 2:ncol(gdp)] 
gdp_num <- mutate_all(gdp_num, as.numeric) 
gdp_date <-gdp[,1]


# Repeat the transformation code for each column in gdp_num 
transformation_codes <- rep(5, ncol(gdp_num))  
# Apply the fred_transform function with the transformation codes 
stat_gdp <- fred_transform(data = gdp_num, type = "fred_qd", code = transformation_codes, na.rm= FALSE)



# importing data
df = read_excel("ROUTPUTQvQd.xlsx", col_names = TRUE)
df
summary(df)

# data processing

# converting the DATE col to be datetime in terms of quarters
df$DATE = zoo::as.yearqtr(df$DATE, format = "%Y:Q%q")
df 

df = df %>%
  mutate_if(is.character, as.double)

#earliest_date = df$DATE[1]
#latest_date = df$DATE[nrow(df)]
earliest_year = min(as.integer(format(df$DATE, "%Y")))
latest_year = max(as.integer(format(df$DATE, "%Y")))

df %>%
  select(DATE, ROUTPUT24Q1) %>%
  mutate(lag_ROUTPUT24Q1 = lag(ROUTPUT24Q1)) %>%
  mutate(log_ROUTPUT24Q1 = log(ROUTPUT24Q1, base = exp(1)), 
         log_lag_ROUTPUT24Q1 = log(lag_ROUTPUT24Q1, base = exp(1))) %>%
  mutate(log_diff = log_ROUTPUT24Q1 - log_lag_ROUTPUT24Q1)

# time series plot
df %>%
  select(DATE, ROUTPUT24Q1) %>%
  ggplot(aes(x=DATE, y=ROUTPUT24Q1)) +
  geom_line() +
  ggtitle("Change in Real GDP Across Time")

df %>%
  select(DATE,  "ROUTPUT22Q1") %>%
  ggplot(aes(x=DATE, y="ROUTPUT22Q1")) +
  geom_line() +
  labs(title = "Change in Real GDP Across Time", x = "Time", y = "Real GDP") + 
  theme(plot.title = element_text(hjust = 0.5))

# df %>%
#   select(DATE, ROUTPUT24Q1) %>%
#   plot_ly(x = DATE, y = ROUTPUT24Q1, type="scatter", mode="lines")

# autocorrelation plot across time
acf_values = acf(df$ROUTPUT24Q1, lag.max=150)
acf_values = acf(df$ROUTPUT24Q1, lag.max=150, plot = FALSE)
acf_df = data.frame(lag = acf_values$lag, acf = acf_values$acf)
ggplot(acf_df, aes(x = lag, y = acf)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Correlogram of Real GDP", x = "Lag", y = "Autocorrelation of Real GDP") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(acf_df, aes(x = lag, y = acf)) + 
  geom_segment(aes(xend = lag, yend = 0), color = "blue") +
  geom_point(color = "red", size = 1, shape = 18) + 
  labs(title = "Correlogram of Real GDP", x = "Lag", y = "Autocorrelation of Real GDP") + 
  theme(plot.title = element_text(hjust = 0.5))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Macroeconomic Forecasting of Real GDP"),
    
    theme = shinythemes::shinytheme("united"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          sliderInput("year", "Select Year", min=earliest_year, max=latest_year, step=1, value=2010),
          selectInput("quarter", "Select Quarter", choices=c("Q1", "Q2", "Q3", "Q4")),
          selectInput("forecast_horizon", "Select Forecast Horizon", choices=c("1Q", "2Q", "3Q", "4Q", "2 Years", "3 Years"), selected="1Q"),
          selectInput("model_choice", "Choose Model to show", choices = c("AR", "ADL", "Random Forest", "Combined", "Most Optimal"), selected = "AR")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Time Series", plotOutput("line_plot")), 
            tabPanel("Correlogram", plotOutput("correlogram")),
            tabPanel("Table", DT::DTOutput("dt_table"))
          )
           
        )
    ),
    actionButton("show_about", "About")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$line_plot = renderPlot({
    col_prefix = "ROUTPUT"
    #to get the last two digit of the user's input year
    reference_year = substr(input$year, start = 3, stop = 4)
    #to get the chosen quarter by the user
    reference_quarter = input$quarter
    #to form the colname required to subset from the dataframe
    reference_col = paste(col_prefix, reference_year, reference_quarter, sep="")
    
    print(reference_col)
    
    subset_df = df %>%
      select(DATE, reference_col)
    
    print(subset_df)
    
    ggplot(subset_df) +
      geom_line(aes(x=DATE, y=!!sym(reference_col))) +
      labs(title = "Change in Real GDP Across Time", x = "Time", y = "Real GDP") + 
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$correlogram = renderPlot({
    #acf_values = acf(df$ROUTPUT24Q1, lag.max=150)
    #acf_values = acf(df$ROUTPUT24Q1, lag.max=150, plot = FALSE)
    
    col_prefix = "ROUTPUT"
    #to get the last two digit of the user's input year
    reference_year = substr(input$year, start = 3, stop = 4)
    #to get the chosen quarter by the user
    reference_quarter = input$quarter
    #to form the colname required to subset from the dataframe
    reference_col = paste(col_prefix, reference_year, reference_quarter, sep="")
    
    print(df$reference_col)

    acf_values = acf(df$reference_col, lag.max=150, plot = FALSE)
    print(acf_values)
    
    acf_df = data.frame(lag = acf_values$lag, acf = acf_values$acf)
    
    # ggplot(acf_df, aes(x = lag, y = acf)) + 
    #   geom_segment(aes(xend = lag, yend = 0), color = "blue") +
    #   geom_point(color = "red", size = 1, shape = 18) + 
    #   labs(title = "Correlogram of Real GDP", x = "Lag", y = "Autocorrelation of Real GDP") + 
    #   theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$dt_table = DT::renderDT({
    col_prefix = "ROUTPUT"
    #to get the last two digit of the user's input year
    reference_year = substr(input$year, start = 3, stop = 4)
    #to get the chosen quarter by the user
    reference_quarter = input$quarter
    #to form the colname required to subset from the dataframe
    reference_col = paste(col_prefix, reference_year, reference_quarter, sep="")
    
    print(reference_col)
    
    df %>%
      select(DATE, reference_col) %>%
      DT::datatable()
  })
  
  observeEvent(input$show_about, {
    text_about = "As one of the fundamental indicators of economic activity and a pivotal metric guiding policy decisions, precise forecasting of GDP is imperative for policymakers, businesses, and individuals. However, GDP figures often undergo revisions, resulting in disparities between initial projections and final numbers. These revisions can profoundly influence the dependability and efficacy of macroeconomic forecasting models when operating with real-time data.
Hence, our project endeavors to construct and assess resilient forecasting models adept at integrating updated GDP data to deliver precise predictions."
    showModal(modalDialog(text_about, title = "About"))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)




 
