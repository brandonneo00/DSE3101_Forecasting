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
gdp <- read_excel("../ROUTPUTQvQd.xlsx", col_names = TRUE)
# Convert all columns except the first one (which contains dates) to numeric, keeping NA values as NA 
gdp_num <- gdp[, 2:ncol(gdp)]
gdp_num <- mutate_all(gdp_num, as.numeric) 
gdp_date <-gdp[,1]


# Repeat the transformation code for each column in gdp_num 
transformation_codes <- rep(5, ncol(gdp_num))

# Apply the fred_transform function with the transformation codes 
stat_gdp <- fred_transform(data = gdp_num, type = "fred_qd", code = transformation_codes, na.rm= FALSE)

stat_df = data.frame(gdp_date, stat_gdp)
stat_df$DATE = zoo::as.yearqtr(stat_df$DATE, format = "%Y:Q%q")
stat_df

earliest_year = min(as.integer(format(stat_df$DATE, "%Y")))
latest_year = max(as.integer(format(stat_df$DATE, "%Y")))

stat_df %>% slice(-1)
stat_df %>% 
  slice(-1) %>%
  select(where(~ sum(is.na(.)) > 130))

stat_df %>% select(DATE, "ROUTPUT65Q4") %>%
  filter(complete.cases(.)) %>%
  tail(16)

test = as.yearqtr("2022 Q2")
seq_dates = seq(test, length.out = 8, by = 1/4)
seq_dates

stat_df %>% 
  select(DATE, ROUTPUT24Q1) %>%
  filter(DATE %in% seq_dates)

# getting the last available vintage for comparing against fan chart
library(stringr)
get_last_available_vintage = function(stat_df_w_date){
  last_available_datapt = stat_df_w_date %>%
    select(DATE) %>%
    tail(1) %>%
    pull()
  last_available_vintage = seq(last_available_datapt, length.out = 2, by = 1/4)[2]
  last_available_vintage
  last_available_vintage_year = str_split(last_available_vintage, pattern=" ")[[1]][1]
  last_available_vintage_year_prefix = substr(last_available_vintage_year, start = 3, stop = 4)
  last_available_vintage_quarter = str_split(last_available_vintage, pattern=" ")[[1]][2]
  col_prefix = "ROUTPUT"
  last_available_vintage_col_name = paste(col_prefix, last_available_vintage_year_prefix, last_available_vintage_quarter, sep="")
  return(last_available_vintage_col_name)
}
a = get_last_available_vintage(stat_df)
a

true_df = stat_df %>% 
  select(DATE, a) %>%
  filter(DATE %in% seq_dates) %>%
  mutate(DATE = zoo::as.yearqtr(DATE))
true_df

test = stat_df %>%
  select(DATE, ROUTPUT22Q1) %>%
  filter(complete.cases(.)) %>% #removing the "future" rows
  tail(16)

a

ggplot() + 
  geom_line(data = test, aes(x=DATE, y=ROUTPUT22Q1), color="red") +
  geom_line(data = true_df, aes(x=DATE, y=!!as.name(a)), color="blue")

# Calling the fitAR function
#library(import)
#library(sys)
#sys.source("AR model_5April2024.r")
#mymodule = file.path("AR model_5April2024.r")
#import::from(mymodule, "fitAR", .character_only=TRUE)

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
    
    subset_df = stat_df %>%
      select(DATE, reference_col) %>%
      filter(complete.cases(.)) %>% #removing the "future" rows
      tail(16) %>% #showing the last 16 quarters of data 
      mutate(Category = "A")
    
    #print(subset_df)
    
    reference_quarter_numeric = as.numeric(substr(input$quarter, star = nchar(input$quarter), stop = nchar(input$quarter)))
    if (reference_quarter_numeric == 1){
      x_intercept_quarter = 4
      x_intercept_quarter = paste("Q", x_intercept_quarter)
      x_intercept_year = input$year - 1
    } else{
      x_intercept_quarter = reference_quarter_numeric - 1
      x_intercept_quarter = paste("Q", x_intercept_quarter)
      x_intercept_year = input$year
    }
    
    x_intercept = zoo::as.yearqtr(paste(x_intercept_year, x_intercept_quarter, sep=" "))
    x_intercept_numeric = as.numeric(x_intercept) 
    
    
    # showing the "true" values for 8 steps ahead from user's vintage point
    
    # very last col available in the data 
    #true_value_start_date = zoo::as.yearqtr(paste(input$year, input$quarter, sep=" "))
    #true_value_seq_dates = seq(true_value_start_date, length.out = 8, by = 1/4)
    
    true_value_start_date = zoo::as.yearqtr(paste(x_intercept_year, x_intercept_quarter, sep=" "))
    true_value_seq_dates = seq(true_value_start_date, length.out = 9, by = 1/4)
    true_value_seq_dates
    
    last_available_vintage = get_last_available_vintage(stat_df)
    true_df = stat_df %>% 
      select(DATE, last_available_vintage) %>%
      filter(DATE %in% true_value_seq_dates) %>%
      mutate(Category = "B")
    
    print(true_df)
    
    # making point forecasts using AR model
    
    
    ggplot() +
      geom_line(data = subset_df, aes(x=DATE, y=!!sym(reference_col), color=Category), show.legend=TRUE) +
      geom_line(data = true_df, aes(x=DATE, y=!!as.name(last_available_vintage), color=Category), show.legend = TRUE) +
      geom_vline(xintercept = x_intercept_numeric, color="red", linetype="dashed") + #where the fanchart should start 
      labs(title = "Change in Real GDP Across Time", x = "Time", y = "Real GDP") + 
      scale_x_yearqtr(format = "%Y Q%q") + #to change x ticks to be in yearqtr format
      scale_color_manual(values = c("A" = "black", "B" = "chartreuse2")) + #manually set colors of line
      theme(plot.title = element_text(hjust = 0.5), 
            legend.position = "bottom")
    
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
    
    print(stat_df$reference_col)
    
    acf_values = acf(stat_df$reference_col, lag.max=150, plot = FALSE)
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
    
    stat_df %>%
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




# Dumpster - for debugging purposes

# importing data
# df = read_excel("ROUTPUTQvQd.xlsx", col_names = TRUE)
# df
# summary(df)
# 
# # converting the DATE col to be datetime in terms of quarters
# df$DATE = zoo::as.yearqtr(df$DATE, format = "%Y:Q%q")
# df 
# 
# df = df %>%
#   mutate_if(is.character, as.double)
# 
# df %>%
#   select(DATE, ROUTPUT24Q1) %>%
#   mutate(lag_ROUTPUT24Q1 = lag(ROUTPUT24Q1)) %>%
#   mutate(log_ROUTPUT24Q1 = log(ROUTPUT24Q1, base = exp(1)), 
#          log_lag_ROUTPUT24Q1 = log(lag_ROUTPUT24Q1, base = exp(1))) %>%
#   mutate(log_diff = log_ROUTPUT24Q1 - log_lag_ROUTPUT24Q1)


# time series plot 
# df %>%
#   select(DATE, ROUTPUT24Q1) %>%
#   ggplot(aes(x=DATE, y=ROUTPUT24Q1)) +
#   geom_line() +
#   ggtitle("Change in Real GDP Across Time")
# 
# df %>%
#   select(DATE,  "ROUTPUT22Q1") %>%
#   ggplot(aes(x=DATE, y="ROUTPUT22Q1")) +
#   geom_line() +
#   labs(title = "Change in Real GDP Across Time", x = "Time", y = "Real GDP") + 
#   theme(plot.title = element_text(hjust = 0.5))

# df %>%
#   select(DATE, ROUTPUT24Q1) %>%
#   plot_ly(x = DATE, y = ROUTPUT24Q1, type="scatter", mode="lines")


# autocorrelation plot across time
# acf_values = acf(df$ROUTPUT24Q1, lag.max=150)
# acf_values = acf(df$ROUTPUT24Q1, lag.max=150, plot = FALSE)
# acf_df = data.frame(lag = acf_values$lag, acf = acf_values$acf)
# ggplot(acf_df, aes(x = lag, y = acf)) + 
#   geom_bar(stat = "identity") + 
#   labs(title = "Correlogram of Real GDP", x = "Lag", y = "Autocorrelation of Real GDP") + 
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ggplot(acf_df, aes(x = lag, y = acf)) + 
#   geom_segment(aes(xend = lag, yend = 0), color = "blue") +
#   geom_point(color = "red", size = 1, shape = 18) + 
#   labs(title = "Correlogram of Real GDP", x = "Lag", y = "Autocorrelation of Real GDP") + 
#   theme(plot.title = element_text(hjust = 0.5))



 
