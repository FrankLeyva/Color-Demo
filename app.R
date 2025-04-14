library(shiny)
library(shinyWidgets)
library(plotly)
library(RColorBrewer)
library(htmltools)
library(leaflet)
library(dplyr)
library(sf)

# Load geographical data
geo_data <- reactive({
  tryCatch({
    sf::st_read('data/geo/Jrz_Map.geojson', quiet = TRUE)
  }, error = function(e) {
    # Fallback to sample data if file not found
    showNotification(paste("Using sample geo data:", e$message), type = "warning")
    create_sample_geo_data()
  })
})

# Function to create sample geo data if the file is not found
create_sample_geo_data <- function() {
  # Create a simple rectangular geometry for each district
  districts <- 1:9
  
  # Create a grid of 3x3 rectangles centered around Ciudad Juarez approximate coordinates
  lat_base <- 31.7
  lng_base <- -106.5
  
  rectangles <- list()
  index <- 1
  
  for(i in 1:3) {
    for(j in 1:3) {
      # Calculate coordinates for this rectangle
      lng_min <- lng_base - 0.3 + (j-1) * 0.2
      lng_max <- lng_base - 0.3 + j * 0.2
      lat_min <- lat_base - 0.3 + (i-1) * 0.2
      lat_max <- lat_base - 0.3 + i * 0.2
      
      # Create polygon
      poly <- sf::st_polygon(list(rbind(
        c(lng_min, lat_min),
        c(lng_max, lat_min),
        c(lng_max, lat_max),
        c(lng_min, lat_max),
        c(lng_min, lat_min)
      )))
      
      rectangles[[index]] <- poly
      index <- index + 1
    }
  }
  
  # Create sf object
  geo_df <- sf::st_sf(
    No_Distrit = districts,
    geometry = sf::st_sfc(rectangles, crs = 4326)
  )
  
  return(geo_df)
}

# Generate sample district data
generate_sample_district_data <- function(num_districts = 9, num_samples = 500) {
  # Create data frame with random district assignments
  set.seed(123)
  
  data <- data.frame(
    district = sample(1:num_districts, num_samples, replace = TRUE),
    value_num = sample(1:10, num_samples, replace = TRUE),
    gender = sample(c("Hombre", "Mujer"), num_samples, replace = TRUE),
    age_group = sample(c("18-24", "25-34", "35-44", "45-54", "55+"), num_samples, replace = TRUE)
  )
  
  # Calculate district statistics
  district_stats <- data %>%
    group_by(district) %>%
    summarise(
      mean_value = mean(value_num),
      n = n(),
      .groups = 'drop'
    )
  
  # Return both the raw data and the district stats
  return(list(
    data = data,
    stats = district_stats
  ))
}

# Simplified version of create_interval_district_map function
create_district_map <- function(geo_data, district_stats = NULL, use_gradient = FALSE, district_palette) {
  # Check if we have data
  if (is.null(geo_data)) {
    return(leaflet() %>% 
      addTiles() %>%
      setView(lng = -106.5, lat = 31.7, zoom = 11) %>%
      addControl(html = "No map data available", position = "topright"))
  }
  
  # If no stats provided, create sample data
  if (is.null(district_stats)) {
    sample_data <- generate_sample_district_data()
    district_stats <- sample_data$stats
  }
  
  # Calculate centroids for label placement
  geo_data$centroid <- sf::st_centroid(geo_data$geometry)
  centroids <- sf::st_coordinates(geo_data$centroid)
  geo_data$lng <- centroids[,1]
  geo_data$lat <- centroids[,2]
  
  # Pre-calculate all values needed for the map
  geo_data$fill_color <- "#CCCCCC"  # Default gray
  geo_data$hover_label <- ""
  
  # Calculate fill colors based on gradient or categorical
  if (use_gradient) {
    # Create color palette based on the data range
    value_range <- range(district_stats$mean_value, na.rm = TRUE)
    pal <- colorNumeric(palette = "Blues", domain = value_range)
    
    # Apply to each district
    for (i in 1:nrow(geo_data)) {
      dist_num <- geo_data$No_Distrit[i]
      match_idx <- which(district_stats$district == dist_num)
      
      if (length(match_idx) > 0) {
        mean_val <- district_stats$mean_value[match_idx]
        if (!is.na(mean_val)) {
          geo_data$fill_color[i] <- pal(mean_val)
        }
      }
    }
    
    # Store the palette for legend
    palette_function <- pal
    palette_values <- value_range
    legend_title <- "Valor Promedio"
  } else {
    # Use categorical district colors
    for (i in 1:nrow(geo_data)) {
      dist_num <- geo_data$No_Distrit[i]
      # Convert to 1-based index for district palette (if districts are 1-9)
      district_index <- as.numeric(dist_num)
      
      if (!is.na(district_index) && district_index >= 1 && district_index <= length(district_palette)) {
        geo_data$fill_color[i] <- district_palette[district_index]
      }
    }
    
    # No legend for categorical colors
    palette_function <- NULL
  }
  
  # Create hover labels
  for (i in 1:nrow(geo_data)) {
    dist_num <- geo_data$No_Distrit[i]
    match_idx <- which(district_stats$district == dist_num)
    
    if (length(match_idx) > 0) {
      geo_data$hover_label[i] <- sprintf(
        "Distrito: %s<br>Valor Promedio: %.2f<br>N: %d",
        dist_num,
        district_stats$mean_value[match_idx],
        district_stats$n[match_idx]
      )
    } else {
      geo_data$hover_label[i] <- sprintf("Distrito: %s<br>Sin datos", dist_num)
    }
  }
  
  # Create the base map
  map <- leaflet(geo_data) %>%
    addProviderTiles(providers$Stadia.StamenTonerLite) %>% 
    addPolygons(
      fillColor = ~fill_color,
      fillOpacity = 0.7,
      weight = 1,
      color = "#666666",
      dashArray = "3",
      highlight = highlightOptions(
        weight = 2,
        color = "#000000",
        dashArray = "",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = ~lapply(hover_label, HTML)
    )
  
  # Add legend if using gradient
  if (use_gradient && !is.null(palette_function)) {
    map <- map %>% addLegend(
      position = "bottomright",
      pal = palette_function,
      values = palette_values,
      title = legend_title,
      opacity = 0.7
    )
  }
  
  # Add district labels
  for (i in 1:nrow(geo_data)) {
    dist_num <- geo_data$No_Distrit[i]
    match_idx <- which(district_stats$district == dist_num)
    
    if (length(match_idx) > 0) {
      # Add label with district number
      label_html <- sprintf(
        '<div style="background-color: white; color: black; padding: 3px 8px; border-radius: 3px; font-weight: bold; text-align: center;">Distrito %s</div>',
        dist_num
      )
      
      # Add label 
      map <- map %>% addLabelOnlyMarkers(
        lng = geo_data$lng[i],
        lat = geo_data$lat[i],
        label = lapply(list(label_html), HTML),
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE
        )
      )
    }
  }
  
  return(map)
}

# Proposed color palettes with updates
proposed_palettes <- list(
  # Base colors
  base = list(
    primary = "#0D6EFD",
    secondary = "#6C757D",
    success = "#198754",
    warning = "#FFC107",
    danger = "#DC3545",
    info = "#0DCAF0",
    light = "#F8F9FA",
    dark = "#212529",
    background = "#FFFFFF",
    text = "#212529",
    neutral = "#DEE2E6",
    accent = "#FFD700"
  ),
  
  # District colors (more pastel-like)
  district = c("#88BDBC", "#6E9887", "#BECC92", "#FDD692", "#F1BB87", 
              "#F28A80", "#D1A5C6", "#9CADCE", "#B6C5D1", "#D3D9E0"),
  
  sections = c("#1E88E5","#43A047","#5E35B1","#8E24AA","#F57C00"),
  
  # Section-specific palettes
  bienestar = list(
    primary = "#1E88E5",
    secondary = "#90CAF9",
    accent = "#FFA000",
    sequential = c("#BFD3E6", "#9EBCDA", "#8C96C6", "#6BAED6", "#4292C6", "#2171B5", "#084594"),
    categorical = c("#1E88E5", "#42A5F5", "#90CAF9", "#FFA000", "#FFCA28", "#FFE082"),
    diverging = colorRampPalette(c("#DC3545", "#DEE2E6", "#1E88E5"))(9)
  ),
  
  movilidad = list(
    primary = "#43A047",
    secondary = "#A5D6A7",
    accent = "#052F5F",
    sequential = c("#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#006D2C", "#00441B"),
    categorical =c("#43A047", "#66BB6A", "#A5D6A7", "#052F5F", "#2D4A70", "#5788A0"),
    diverging = colorRampPalette(c("#DC3545", "#DEE2E6", "#43A047"))(9)
  ),
  
  gobierno = list(
    primary = "#5E35B1",
    secondary = "#B39DDB",
    accent = "#FB8C00",
    sequential = c("#CAB2D6", "#9E9AC8", "#756BB1", "#5E35B1", "#54278F", "#41236C", "#3A1E61"),
    categorical = c("#5E35B1", "#7E57C2", "#B39DDB", "#FB8C00", "#FFA726", "#FFCC80"),
    diverging = colorRampPalette(c("#DC3545", "#DEE2E6", "#5E35B1"))(9)
  ),
  
  infraestructura = list(
    primary = "#F57C00",  # Changed from E64A19 to be more distinct from danger color
    secondary = "#FFCC80",
    accent = "#039BE5",
    sequential = c("#FBB582", "#FA954F", "#F57C00", "#E66D00", "#D45F00", "#B65200", "#8F3F00"),
    categorical = c("#F57C00", "#FB8C00", "#FFCC80", "#039BE5", "#29B6F6", "#81D4FA"),
    diverging = colorRampPalette(c("#DC3545", "#DEE2E6", "#F57C00"))(9)
  ),
  
  participacion = list(
    primary = "#8E24AA",
    secondary = "#CE93D8",
    accent = "#00ACC1",
    sequential = c("#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#8E24AA", "#980043", "#67001F"),
    categorical = c("#8E24AA", "#AB47BC", "#CE93D8", "#00ACC1", "#26C6DA", "#80DEEA"),
    diverging = colorRampPalette(c("#DC3545", "#DEE2E6", "#8E24AA"))(9)
  )
)

# Define UI
ui <- fluidPage(
  titlePanel("Color Palette Preview for Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("palette_type", "Palette Type:",
                  choices = c("Base Colors", "Core Palettes", "Section Palettes"),
                  selected = "Base Colors"),
      
      conditionalPanel(
        condition = "input.palette_type == 'Section Palettes'",
        radioButtons("section", "Section:",
                    choices = c("Bienestar", "Movilidad", "Gobierno", "Infraestructura", "Participacion"),
                    selected = "Bienestar")
      ),
      
      conditionalPanel(
        condition = "input.palette_type == 'Core Palettes'",
        radioButtons("core_palette", "Core Palette:",
                    choices = c("Section Colors", "District Colors", "Gender Colors", "Age Group Colors"),
                    selected = "District Colors")
      ),
      
      conditionalPanel(
        condition = "input.palette_type == 'Section Palettes'",
        radioButtons("section_palette", "Section Palette Type:",
                    choices = c("Main Colors", "Sequential", "Categorical", "Diverging"),
                    selected = "Main Colors")
      ),
      
      # New UI control for map visualization when District Colors is selected
      conditionalPanel(
        condition = "input.palette_type == 'Core Palettes' && input.core_palette == 'District Colors'",
        checkboxInput("use_color_gradient", "Use Color Gradient", value = FALSE),
        helpText("Toggle between categorical district colors and a gradient based on values.")
      ),
      
      width = 3
    ),
    
    mainPanel(
      fluidRow(
        column(12, 
          h4("Palette Preview"),
          plotlyOutput("palette_plot", height = "200px"),
          hr(),
          h4("Example Visualization"),
          # Conditional UI for the different plot types based on selection
          conditionalPanel(
            condition = "!(input.palette_type == 'Core Palettes' && input.core_palette == 'District Colors')",
            plotlyOutput("example_plot", height = "400px")
          ),
          conditionalPanel(
            condition = "input.palette_type == 'Core Palettes' && input.core_palette == 'District Colors'",
            leafletOutput("district_map", height = "400px")
          ),
          hr(),
          h4("Color Values"),
          verbatimTextOutput("color_values")
        )
      ),
      width = 9
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Get the currently selected palette
  selected_palette <- reactive({
    if (input$palette_type == "Base Colors") {
      return(proposed_palettes$base)
    } else if (input$palette_type == "Core Palettes") {
      if (input$core_palette == "District Colors") {
        return(proposed_palettes$district)
      } else if (input$core_palette == "Gender Colors") {
        return(c("#FF7BAC", "#007BFF"))  # Pink, Blue
      } else if (input$core_palette == "Section Colors") {
        return(proposed_palettes$sections)
      }else {
        return(c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")) # Age groups
      }
    } else {
      # Section palettes
      section <- tolower(input$section)
      section_palette_type <- tolower(input$section_palette)
      
      if (section_palette_type == "main colors") {
        return(c(
          proposed_palettes[[section]]$primary,
          proposed_palettes[[section]]$secondary,
          proposed_palettes[[section]]$accent
        ))
      } else {
        return(proposed_palettes[[section]][[section_palette_type]])
      }
    }
  })
  
  # Generate sample data for district map
  sample_data <- reactive({
    generate_sample_district_data()
  })
  
  # Generate palette preview
  output$palette_plot <- renderPlotly({
    palette <- selected_palette()
    
    if (input$palette_type == "Base Colors") {
      # Create a named vector for base colors
      colors <- unlist(palette)
      labels <- names(colors)
      
      # Create data frame for plotting
      df <- data.frame(
        x = 1:length(colors),
        y = rep(1, length(colors)),
        color = colors,
        label = labels
      )
      
      # Create plot
      plot_ly(df, x = ~x, y = ~y, color = I(colors), text = ~paste(label, ":", color)) %>%
        add_bars(width = 0.8) %>%
        layout(
          showlegend = FALSE,
          xaxis = list(
            tickvals = df$x,
            ticktext = df$label,
            tickangle = 45
          ),
          yaxis = list(
            showticklabels = FALSE,
            showgrid = FALSE
          )
        )
    } else {
      # For other palettes, create a simple color bar
      if (length(palette) > 0) {
        df <- data.frame(
          x = 1:length(palette),
          y = rep(1, length(palette)),
          color = palette
        )
        
        plot_ly(df, x = ~x, y = ~y, color = I(palette), 
                text = ~paste("Color:", color)) %>%
          add_bars(width = 0.8) %>%
          layout(
            showlegend = FALSE,
            xaxis = list(
              showticklabels = FALSE,
              showgrid = FALSE
            ),
            yaxis = list(
              showticklabels = FALSE,
              showgrid = FALSE
            )
          )
      } else {
        # Handle empty palette case
        plot_ly() %>%
          layout(
            title = "No colors to display",
            xaxis = list(showticklabels = FALSE, showgrid = FALSE),
            yaxis = list(showticklabels = FALSE, showgrid = FALSE)
          )
      }
    }
  })
  
  # Generate example visualization
  output$example_plot <- renderPlotly({
    palette <- selected_palette()
    
    # Check if palette is empty
    if (length(palette) == 0) {
      return(plot_ly() %>%
        layout(title = "No data to display"))
    }
    
    # Create some example data
    set.seed(123)
    if (input$palette_type == "Base Colors") {
      # Bar chart with base colors
      categories <- names(palette)[1:6]  # First 6 base colors
      values <- round(runif(6, 10, 100))
      colors <- unlist(palette)[1:6]
      
      plot_ly() %>%
        add_bars(x = categories, y = values, marker = list(color = colors),
                 text = ~paste(categories, ":", values),
                 hoverinfo = "text") %>%
        layout(
          title = "Example Bar Chart with Base Colors",
          xaxis = list(title = "Categories"),
          yaxis = list(title = "Values")
        )
      
    } else if (input$palette_type == "Core Palettes") {
      # Group comparison
      if (input$core_palette == "District Colors") {
        categories <- paste("District", 1:length(palette))
        values <- round(runif(length(palette), 10, 100))
        
        plot_ly() %>%
          add_bars(x = categories, y = values, marker = list(color = palette),
                  text = ~paste(categories, ":", values),
                  hoverinfo = "text") %>%
          layout(
            title = "Values by District",
            xaxis = list(title = "District"),
            yaxis = list(title = "Value")
          )
      } else if (input$core_palette == "Gender Colors") {
        # Gender comparison
        plot_ly() %>%
          add_pie(labels = c("Female", "Male"), values = c(55, 45), 
                 marker = list(colors = palette),
                 textinfo = "label+percent") %>%
          layout(title = "Gender Distribution")
      } else {
        # Age groups
        age_groups <- c("18-24", "25-34", "35-44", "45-54", "55+")
        values <- c(15, 25, 30, 20, 10)
        
        plot_ly() %>%
          add_pie(labels = age_groups, values = values, 
                 marker = list(colors = palette),
                 textinfo = "label+percent") %>%
          layout(title = "Age Group Distribution")
      }
    } else {
      # Section visualizations
      section <- tolower(input$section)
      section_palette_type <- tolower(input$section_palette)
      
      if (section_palette_type == "main colors") {
        # Main colors visualization: Primary, Secondary, Accent
        labels <- c("Primary", "Secondary", "Accent")
        values <- c(50, 30, 20)
        
        plot_ly() %>%
          add_pie(labels = labels, values = values,
                 marker = list(colors = palette),
                 textinfo = "label+percent") %>%
          layout(title = paste(input$section, "Main Colors"))
      } else if (section_palette_type == "sequential") {
        # Sequential data visualization
        x <- 1:length(palette)
        y <- seq(10, 100, length.out = length(palette))
        
        plot_ly() %>%
          add_bars(x = x, y = y, marker = list(color = palette),
                  text = ~paste("Value:", y),
                  hoverinfo = "text") %>%
          layout(
            title = paste(input$section, "Sequential Scale"),
            xaxis = list(title = "Categories", showticklabels = FALSE),
            yaxis = list(title = "Values")
          )
      } else if (section_palette_type == "categorical") {
        # Categorical data
        categories <- paste("Category", 1:length(palette))
        values <- round(runif(length(palette), 10, 100))
        
        plot_ly() %>%
          add_bars(x = categories, y = values, marker = list(color = palette),
                  text = ~paste(categories, ":", values),
                  hoverinfo = "text") %>%
          layout(
            title = paste(input$section, "Categorical Colors"),
            xaxis = list(title = "Categories"),
            yaxis = list(title = "Values")
          )
      } else if (section_palette_type == "diverging") {
        # Diverging data
        x <- 1:length(palette)
        # Create data that diverges from a central point
        mid_point <- ceiling(length(palette) / 2)
        y <- c(seq(-50, -10, length.out = mid_point - 1), 0, 
               seq(10, 50, length.out = length(palette) - mid_point))
        
        plot_ly() %>%
          add_bars(x = x, y = y, marker = list(color = palette),
                  text = ~paste("Value:", y),
                  hoverinfo = "text") %>%
          layout(
            title = paste(input$section, "Diverging Scale"),
            xaxis = list(title = "Categories", showticklabels = FALSE),
            yaxis = list(title = "Values")
          )
      }
    }
  })
  
  # District map output 
  output$district_map <- renderLeaflet({
    req(input$palette_type == "Core Palettes" && input$core_palette == "District Colors")
    
    palette <- selected_palette()
    
    # Create district map
    create_district_map(
      geo_data(),
      district_stats = sample_data()$stats,
      use_gradient = input$use_color_gradient,
      district_palette = palette
    )
  })
  
  # Display color values
  output$color_values <- renderPrint({
    palette <- selected_palette()
    
    if (input$palette_type == "Base Colors") {
      # For base colors, show named list
      cat("Base Colors:\n")
      for (name in names(palette)) {
        cat(sprintf("%s: \"%s\"\n", name, palette[[name]]))
      }
    } else {
      # For other palettes, show vector
      if (is.list(palette)) {
        cat("Color List:\n")
        for (name in names(palette)) {
          if (length(palette[[name]]) == 1) {
            cat(sprintf("%s: \"%s\"\n", name, palette[[name]]))
          } else {
            cat(sprintf("%s: c(\"%s\")\n", name, paste(palette[[name]], collapse = "\", \"")))
          }
        }
      } else {
        cat("Color Vector:\n")
        cat(sprintf("c(\"%s\")", paste(palette, collapse = "\", \"")))
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)