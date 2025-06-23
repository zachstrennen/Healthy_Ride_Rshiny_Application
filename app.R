library(shiny)
library(DT)
library(lubridate)
library(shinyWidgets)
library(ggplot2)
library(prophet)
library(xgboost)
library(rsconnect)

# Call Data Cleaning file
source("data_cleaning.R")

# UI
ui <- fluidPage(

  # Specified styling for the tabs and data table
  tags$head(tags$style(HTML("
  .nav-tabs > li > a {
    font-size: 13px !important;
  }
  .dataTables_wrapper {
    font-size: 12px;
  }
  table.dataTable td {
    padding: 4px 6px;
  }
"))),
  
  # Title header
  titlePanel(
    tags$h1("Pittsburgh Healthy Ride Ride Data",
      style = "font-size: 42px; font-weight: bold;"
    )
  ),
  sidebarLayout(
    # Filtering by quarter or date
    sidebarPanel(
      radioButtons("filter_type", "Filter by:", choices = c("Quarter", "Date Range")),

      # Quarter filtering selection
      conditionalPanel(
        condition = "input.filter_type == 'Quarter'",
        checkboxGroupInput("quarter", "Select Quarter(s):", choices = unique(data$quarter))
      ),

      # Date filtering selection
      conditionalPanel(
        condition = "input.filter_type == 'Date Range'",
        dateRangeInput("date_range", "Choose Start and End Date:",
          start = min(data$starttime), end = max(data$starttime)
        )
      ),

      # User Type drop down
      selectInput("usertype", "User Type:",
        choices = c("All", unique(data$usertype)),
        selected = "All"
      ),

      # Filtering station by id or name
      radioButtons("from_station_type",
        "Start Station Search Type:",
        choices = c("Name", "ID"),
        inline = TRUE
      ),

      # Name or id entry for station
      conditionalPanel(
        condition = "input.from_station_type == 'Name'",
        selectInput("from_station_name",
          "Start Station (Name):",
          choices = c("All", sort(unique(data$from_station_name))),
          selected = "All"
        )
      ),
      conditionalPanel(
        condition = "input.from_station_type == 'ID'",
        selectInput("from_station_id",
          "Start Station (ID):",
          choices = c("All", sort(unique(data$from_station_id))),
          selected = "All"
        )
      ),

      # Filtering station by id or name
      radioButtons("to_station_type",
        "End Station Search Type:",
        choices = c("Name", "ID"),
        inline = TRUE
      ),

      # Name or id entry for station
      conditionalPanel(
        condition = "input.to_station_type == 'Name'",
        selectInput("to_station_name",
          "End Station (Name):",
          choices = c("All", sort(unique(data$to_station_name))),
          selected = "All"
        )
      ),
      conditionalPanel(
        condition = "input.to_station_type == 'ID'",
        selectInput("to_station_id",
          "End Station (ID):",
          choices = c("All", sort(unique(data$to_station_id))),
          selected = "All"
        )
      ),
      # Select specific bike by id
      selectInput("bikeid",
        "Bike ID:",
        choices = c("All", sort(unique(data$bikeid))),
        selected = "All"
      ),

      # Filter for duration
      numericRangeInput("duration_range", "Ride Duration (sec):",
        value = c(min(data$tripduration), max(data$tripduration)),
        min = min(data$tripduration), max = max(data$tripduration),
        separator = " to "
      )
    ),

    # Main application page
    mainPanel(
      # Filtered Data Summary
      h3("Filtered Data Summary"),

      # Make two columns for summary
      fluidRow(
        column(6, tableOutput("summary_table_left")),
        column(6, tableOutput("summary_table_right"))
      ),
      br(),

      # Tabs for visualizations
      tabsetPanel(

        # Tabs for Temporal Analysis
        tabPanel(
          tags$b("Temporal Analysis"),

          # Rides Over Time tab
          tabsetPanel(
            tabPanel(
              "Rides Over Time",

              # Check box to enable forecast
              checkboxInput("enable_forecast", "Enable Forecast", value = FALSE),

              # Call forecast visualization and give update plot with diagnostics
              conditionalPanel(
                condition = "input.enable_forecast == true",
                selectInput("forecast_period", "Forecast Horizon:",
                  choices = c("1 Year" = 365, "5 Years" = 365 * 5, "10 Years" = 365 * 10),
                  selected = 365
                )
              ),
              plotOutput("daily_plot"),
              conditionalPanel(
                condition = "input.enable_forecast == true",
                h4("Forecast Diagnostics"),
                verbatimTextOutput("forecast_diagnostics")
              )
            ),

            # Hourly Distribution Tab
            tabPanel("Hourly Distribution", plotOutput("hourly_plot")),

            # Weekday Distribution Tab
            tabPanel("Weekday Distribution", plotOutput("weekday_plot"))
          )
        ),

        # Tabs for Station Rankings
        tabPanel(
          tags$b("Station Rankings"),
          tabsetPanel(

            # Top 10 Start Stations Tab
            tabPanel("Top 10 Start Stations", plotOutput("top_from_stations")),

            # Top 10 End Stations Tab
            tabPanel("Top 10 End Stations", plotOutput("top_to_stations")),

            # Top 10 Station Pairs Tab
            tabPanel("Top 10 Station Pairs", plotOutput("top_station_pairs"))
          )
        ),

        # Tabs for Duration Analysis/Prediction
        tabPanel(
          tags$b("Duration Analysis/Prediction"),
          tabsetPanel(

            # Ride Duration Distribution Tab
            tabPanel("Ride Duration Distribution", plotOutput("duration_histogram")),

            # Ride Duration Prediction Tab
            tabPanel(
              "Ride Duration Prediction",
              fluidRow(
                column(
                  6,
                  checkboxGroupInput("xgb_features", "Select Features:",
                    choices = c(
                      "User Type" = "usertype",
                      "Start Station ID" = "from_station_id",
                      "End Station ID" = "to_station_id",
                      "Start Hour" = "starttime"
                    ),
                    selected = c("usertype", "from_station_id", "to_station_id")
                  ),
                  uiOutput("feature_inputs"),
                  actionButton("predict_duration", "Predict Duration")
                ),
                column(
                  6,
                  verbatimTextOutput("duration_model_summary"),
                  verbatimTextOutput("predicted_duration")
                )
              )
            )
          )
        ),

        # Tabs for User Type Comparison
        tabPanel(
          tags$b("User Type Comparison"),
          tabsetPanel(

            # Ride Duration by User Type Tab
            tabPanel("Ride Duration by User Type", plotOutput("duration_by_usertype")),

            # Rides by User Type Tab
            tabPanel("Rides by User Type", plotOutput("trips_by_usertype"))
          )
        ),

        # Tabs for Quarterly Analysis
        tabPanel(
          tags$b("Quarterly Analysis"),
          tabsetPanel(

            # Avg Duration per Quarter Tab
            tabPanel("Ride Duration per Quarter", plotOutput("avg_duration_per_quarter")),

            # Rides per Quarter Tab
            tabPanel("Rides per Quarter", plotOutput("trips_per_quarter"))
          )
        ),

        # Tab for Data Table
        tabPanel(tags$b("Data Table"), DTOutput("data_table"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Conditionals for filtering
  filtered_data <- reactive({
    df <- data
    if (input$filter_type == "Quarter" && !is.null(input$quarter)) {
      df <- df %>% filter(quarter %in% input$quarter)
    } else if (!is.null(input$date_range)) {
      df <- df %>% filter(starttime >= as.POSIXct(input$date_range[1]) &
        starttime <= as.POSIXct(input$date_range[2]))
    }
    if (input$usertype != "All") df <- df %>% filter(usertype == input$usertype)
    if (input$from_station_type == "Name" && input$from_station_name != "All") {
      df <- df %>%
        filter(from_station_name == input$from_station_name)
    }
    if (input$from_station_type == "ID" && input$from_station_id != "All") {
      df <- df %>%
        filter(from_station_id == as.numeric(input$from_station_id))
    }
    if (input$to_station_type == "Name" && input$to_station_name != "All") {
      df <- df %>%
        filter(to_station_name == input$to_station_name)
    }
    if (input$to_station_type == "ID" && input$to_station_id != "All") {
      df <- df %>%
        filter(to_station_id == as.numeric(input$to_station_id))
    }
    if (input$bikeid != "All") {
      df <- df %>%
        filter(bikeid == as.numeric(input$bikeid))
    }
    df <- df %>% filter(tripduration >= input$duration_range[1], tripduration <= input$duration_range[2])
    return(df)
  })

  # Filtered Data Summary Table left side
  output$summary_table_left <- renderTable({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(data.frame(Statistic = "No data selected", Value = ""))
    }
    data.frame(
      Statistic = c(
        "Number of Rides",
        "Mean Ride Duration (sec)",
        "Median Ride Duration (sec)",
        "Min Ride Duration (sec)",
        "Max Ride Duration (sec)",
        "Standard Deviation (sec)"
      ),
      Value = c(
        nrow(df),
        round(mean(df$tripduration, na.rm = TRUE), 2),
        round(median(df$tripduration, na.rm = TRUE), 2),
        round(min(df$tripduration, na.rm = TRUE), 2),
        round(max(df$tripduration, na.rm = TRUE), 2),
        round(sd(df$tripduration, na.rm = TRUE), 2)
      )
    )
  })

  # Filtered Data Summary Table right side
  output$summary_table_right <- renderTable({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(data.frame(Statistic = "No data selected", Value = ""))
    }
    data.frame(
      Statistic = c(
        "Number of Unique Bikes",
        "Unique Start Station IDs",
        "Unique End Station IDs",
        "Most Common User Type",
        "Earliest Ride Start",
        "Latest Ride Start"
      ),
      Value = c(
        length(unique(df$bikeid)),
        length(unique(df$from_station_id)),
        length(unique(df$to_station_id)),
        names(sort(table(df$usertype), decreasing = TRUE))[1],
        format(min(df$starttime, na.rm = TRUE), "%Y-%m-%d %H:%M:%S"),
        format(max(df$starttime, na.rm = TRUE), "%Y-%m-%d %H:%M:%S")
      )
    )
  })

  # Data table output
  output$data_table <- renderDT({
    datatable(
      filtered_data(),
      options = list(
        scrollX = TRUE,
        pageLength = 10
      ),
      class = "compact stripe hover"
    )
  })

  # Output for time series plot with forecasting
  output$daily_plot <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(NULL)
    }

    daily <- df %>%
      mutate(day = as.Date(starttime)) %>%
      count(day) %>%
      rename(ds = day, y = n)

    if (input$enable_forecast) {
      horizon_days <- as.numeric(input$forecast_period)

      # Modify data for prophet
      prophet_df <- daily %>%
        mutate(ds = as.POSIXct(ds)) %>%
        filter(!is.na(ds), !is.na(y))

      # Build model for yearly seasonality
      model <- prophet(prophet_df, yearly.seasonality = TRUE)
      future <- make_future_dataframe(model, periods = horizon_days)
      forecast <- predict(model, future)

      # Forecasted and orginal data to plot
      forecast_plot <- forecast %>%
        select(ds, yhat) %>%
        mutate(type = "Forecast")
      historical_plot <- prophet_df %>%
        select(ds, y) %>%
        rename(yhat = y) %>%
        mutate(type = "Historical")
      combined_plot <- bind_rows(historical_plot, forecast_plot)

      ggplot(combined_plot, aes(x = ds, y = yhat, color = type)) +
        geom_line(size = 1) +
        scale_color_manual(values = c("Historical" = "steelblue", "Forecast" = "darkred")) +
        labs(title = "Rides Per Day (with Forecast)", x = "Date", y = "Number of Rides", color = "") +
        theme_minimal()
    } else {
      # Default non-forecasted plot
      ggplot(daily, aes(x = ds, y = y)) +
        geom_line(color = "steelblue", size = 1) +
        labs(title = "Rides Per Day", x = "Date", y = "Number of Rides") +
        theme_minimal()
    }
  })

  # Diagnostics for forecast
  output$forecast_diagnostics <- renderPrint({
    req(input$enable_forecast)

    df <- filtered_data()
    if (nrow(df) < 730) {
      cat("Not enough data for diagnostics (need at least 2 years of data).")
      return()
    }

    daily <- df %>%
      mutate(day = as.Date(starttime)) %>%
      count(day) %>%
      rename(ds = day, y = n) %>%
      mutate(ds = as.POSIXct(ds)) %>%
      filter(!is.na(ds), !is.na(y))

    # Fit Prophet model
    m <- prophet(daily, yearly.seasonality = TRUE, verbose = FALSE)

    # Cross-validation and performance
    df_cv <- cross_validation(m,
      initial = 365 * 86400,
      period = 90 * 86400,
      horizon = 180 * 86400,
      units = "secs"
    )
    df_p <- performance_metrics(df_cv)

    # Keep only final horizon row and key metrics
    last_row <- tail(df_p, 1)
    brief <- last_row[, c("horizon", "rmse", "mae", "mape")]
    print(brief)
  })

  # Hour plot output
  output$hourly_plot <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(NULL)
    }

    df %>%
      mutate(hour = hour(starttime)) %>%
      count(hour) %>%
      ggplot(aes(x = hour, y = n)) +
      geom_col(fill = "steelblue", color = "white") +
      scale_x_continuous(
        breaks = 0:23,
        # Actual time of day displays in plot
        labels = format(strptime(0:23, format = "%H"), "%I %p")
      ) +
      labs(
        title = "Rides by Hour of Day",
        x = "Time of Day",
        y = "Number of Rides"
      ) +
      theme_minimal()
  })

  # Ouput bar chart for day of the week
  output$weekday_plot <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(NULL)
    }
    df %>%
      mutate(weekday = wday(starttime, label = TRUE, abbr = FALSE)) %>%
      count(weekday) %>%
      ggplot(aes(x = weekday, y = n)) +
      geom_col(fill = "steelblue") +
      labs(title = "Rides by Weekday", x = "Day of Week", y = "Number of Rides") +
      theme_minimal()
  })

  # Output plot for top start stations
  output$top_from_stations <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(NULL)
    }
    df %>%
      count(from_station_id, sort = TRUE) %>%
      top_n(10, n) %>%
      ggplot(aes(x = reorder(from_station_id, n), y = n)) +
      geom_col(fill = "skyblue") +
      coord_flip() +
      labs(title = "Top 10 Start Station IDs", x = "Start Station ID", y = "Number of Rides") +
      theme_minimal()
  })

  # Output plot for top end stations
  output$top_to_stations <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(NULL)
    }
    df %>%
      count(to_station_id, sort = TRUE) %>%
      top_n(10, n) %>%
      ggplot(aes(x = reorder(to_station_id, n), y = n)) +
      geom_col(fill = "mediumseagreen") +
      coord_flip() +
      labs(title = "Top 10 End Station IDs", x = "End Station ID", y = "Number of Rides") +
      theme_minimal()
  })

  # Output plot for pairs of stations
  output$top_station_pairs <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(NULL)
    }
    df %>%
      # Count the number of pairs
      count(from_station_id, to_station_id, sort = TRUE) %>%
      top_n(10, n) %>%
      mutate(pair = paste(from_station_id, "→", to_station_id)) %>%
      ggplot(aes(x = reorder(pair, n), y = n)) +
      geom_col(fill = "coral") +
      coord_flip() +
      labs(title = "Top 10 Station Pairs", x = "Station Pair", y = "Number of Rides") +
      theme_minimal()
  })

  # Duration Distribution
  output$duration_histogram <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(NULL)
    }
    ggplot(df, aes(x = tripduration)) +
      geom_histogram(bins = 30, fill = "seagreen", color = "white") +
      labs(title = "Ride Duration Distribution", x = "Ride Duration (sec)", y = "Number of Rides") +
      theme_minimal()
  })

  # XGBoost model input
  output$feature_inputs <- renderUI({
    req(input$xgb_features)
    inputs <- list()

    if ("usertype" %in% input$xgb_features) {
      inputs[[length(inputs) + 1]] <- selectInput("input_usertype",
        "Select User Type",
        choices = unique(data$usertype)
      )
    }
    if ("from_station_id" %in% input$xgb_features) {
      inputs[[length(inputs) + 1]] <- numericInput("input_from_station_id",
        "Choose Start Station ID",
        value = 1000
      )
    }
    if ("to_station_id" %in% input$xgb_features) {
      inputs[[length(inputs) + 1]] <- numericInput("input_to_station_id",
        "Choose End Station ID",
        value = 1000
      )
    }
    if ("starttime" %in% input$xgb_features) {
      inputs[[length(inputs) + 1]] <- numericInput("input_hour",
        "Choose Start Hour (From 0–23 with 0 being midnight)",
        value = 12,
        min = 0,
        max = 23
      )
    }

    tagList(inputs)
  })

  # XGBoost model summary output
  output$duration_model_summary <- renderPrint({
    df <- filtered_data()
    req(input$xgb_features)
    if (nrow(df) < 100) {
      return("Not enough data to train model.")
    }

    df <- df %>% filter(!is.na(tripduration), tripduration > 0)

    if ("starttime" %in% input$xgb_features) {
      df$starttime <- hour(df$starttime)
      colnames(df)[colnames(df) == "starttime"] <- "hour"
    }

    if ("usertype" %in% input$xgb_features) {
      df$usertype <- as.numeric(factor(df$usertype))
    }

    features <- input$xgb_features
    features <- gsub("starttime", "hour", features) # adjust feature name

    # Define model features and label
    df <- df %>% filter(if_any(all_of(features), ~ !is.na(.x)))
    X <- as.matrix(df[, features, drop = FALSE])
    y <- df$tripduration

    xgb_model <- xgboost(data = X, label = y, nrounds = 30, verbose = 0)
    assign("xgb_model_trained", xgb_model, envir = .GlobalEnv)

    cat("XGBoost Model Trained.\nFeatures used:\n", paste(features, collapse = ",\n"))
  })

  observeEvent(input$predict_duration, {
    req(input$xgb_features)

    input_values <- sapply(input$xgb_features, function(f) {
      if (f == "usertype") {
        return(as.numeric(factor(input$input_usertype, levels = unique(data$usertype))))
      }
      if (f == "from_station_id") {
        return(input$input_from_station_id)
      }
      if (f == "to_station_id") {
        return(input$input_to_station_id)
      }
      if (f == "starttime") {
        return(input$input_hour)
      }
    })

    X_input <- matrix(as.numeric(input_values), nrow = 1)
    pred <- predict(get("xgb_model_trained", envir = .GlobalEnv), X_input)

    output$predicted_duration <- renderPrint({
      paste("Predicted Ride Duration:", round(pred, 2), "seconds")
    })
  })


  # Output User Type Duration chart
  output$duration_by_usertype <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(NULL)
    }

    df %>%
      group_by(usertype) %>%
      summarize(avg_duration = mean(tripduration, na.rm = TRUE)) %>%
      ggplot(aes(x = usertype, y = avg_duration, fill = usertype)) +
      geom_col() +
      scale_fill_brewer(palette = "Set2") +
      labs(
        title = "Average Ride Duration by User Type",
        x = "User Type",
        y = "Avg Ride Duration (sec)",
        fill = "User Type"
      ) +
      theme_minimal()
  })

  # Output User Type Ride Count chart
  output$trips_by_usertype <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(NULL)
    }

    df %>%
      count(usertype) %>%
      ggplot(aes(x = usertype, y = n, fill = usertype)) +
      geom_col() +
      scale_fill_brewer(palette = "Set2") +
      labs(
        title = "Rides by User Type",
        x = "User Type",
        y = "Number of Rides",
        fill = "User Type"
      ) +
      theme_minimal()
  })

  # Output chart for number of Rides per quarter
  output$trips_per_quarter <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0 || !"quarter" %in% names(df)) {
      return(NULL)
    }
    df %>%
      count(quarter) %>%
      ggplot(aes(x = quarter, y = n)) +
      geom_col(fill = "cornflowerblue") +
      labs(title = "Rides per Quarter", x = "Quarter", y = "Number of Rides") +
      theme_minimal()
  })

  # Output chart od duration per quarter
  output$avg_duration_per_quarter <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0 || !"quarter" %in% names(df)) {
      return(NULL)
    }
    df %>%
      group_by(quarter) %>%
      summarize(avg_duration = mean(tripduration, na.rm = TRUE)) %>%
      ggplot(aes(x = quarter, y = avg_duration)) +
      geom_col(fill = "seagreen") +
      labs(title = "Average Ride Duration per Quarter", x = "Quarter", y = "Avg Duration (sec)") +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
