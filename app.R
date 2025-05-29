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
  
  # Calculate fill colors based on gradient or Categorica
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
    # Use Categorica Colores de Distritos
    for (i in 1:nrow(geo_data)) {
      dist_num <- geo_data$No_Distrit[i]
      # Convert to 1-based index for district palette (if districts are 1-9)
      district_index <- as.numeric(dist_num)
      
      if (!is.na(district_index) && district_index >= 1 && district_index <= length(district_palette)) {
        geo_data$fill_color[i] <- district_palette[district_index]
      }
    }
    
    # No legend for Categorica colors
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

# Create HTML for section nav cards
create_section_nav_cards <- function(section_colors, section_names) {
  html_content <- '<div style="display: flex; flex-wrap: wrap; justify-content: space-around; margin-top: 20px;">'
  
  for (i in 1:length(section_colors)) {
    card_html <- sprintf(
      '<div style="width: 150px; margin: 10px; border-radius: 8px; overflow: hidden; box-shadow: 0 4px 8px rgba(0,0,0,0.1);">
        <div style="height: 80px; background-color: %s;"></div>
        <div style="padding: 10px; text-align: center; background-color: white;">
          <strong>%s</strong><br>
          <small>%s</small>
        </div>
      </div>',
      section_colors[i],
      section_names[i],
      section_colors[i]
    )
    html_content <- paste0(html_content, card_html)
  }
  
  html_content <- paste0(html_content, '</div>')
  return(HTML(html_content))
}

# Create HTML for section nav bar
create_section_nav_bar <- function(section_colors, section_names) {
  html_content <- '<div style="margin-top: 20px;">'
  
  # Main navigation bar
  html_content <- paste0(html_content, 
    '<ul style="display: flex; list-style: none; padding: 0; margin: 0; overflow: hidden; background-color: #f8f9fa; border-radius: 4px;">'
  )
  
  # Add tabs for each section
  for (i in 1:length(section_colors)) {
    tab_html <- sprintf(
      '<li style="flex: 1; text-align: center;">
        <a href="#" style="display: block; color: white; background-color: %s; padding: 14px 16px; text-decoration: none; font-weight: bold;">
          %s
        </a>
      </li>',
      section_colors[i],
      section_names[i]
    )
    html_content <- paste0(html_content, tab_html)
  }
  
  html_content <- paste0(html_content, '</ul>')
  
  # Add color information below
  html_content <- paste0(html_content, '<div style="display: flex; margin-top: 10px; text-align: center;">')
  
  for (i in 1:length(section_colors)) {
    color_info <- sprintf(
      '<div style="flex: 1;"><small>%s</small></div>',
      section_colors[i]
    )
    html_content <- paste0(html_content, color_info)
  }
  
  html_content <- paste0(html_content, '</div></div>')
  return(HTML(html_content))
}

# Predefined color palettes for quick selection
predefined_palettes <- list(
  "Original" = c("#1E88E5","#43A047","#423629","#8E24AA","#F57C00"),
  "Warm Orange" = c("#FFA058"),
  "Yellow" = c("#FCEE44"),
  "Clear Turquoise" = c("#00dbeb"),
  "Lilac Clear" = c("#d7a4d2"),
  "Pastel Pink" = c("#ffc9de"),
  "Blue Green" = c("#00c5c1"),
  "Strong Pink" = c("#ff769d"),
  "Neutral Pink" = c("#f3d4d1")
)

# Create visual palette selector
create_palette_selector <- function(inputId, palettes, selected_palette = "Original") {
  palette_cards <- ""
  
  for (palette_name in names(palettes)) {
    palette_colors <- palettes[[palette_name]]
    
    # Create color swatches for the palette
    color_swatches <- ""
    colors_to_show <- if(length(palette_colors) > 5) palette_colors[1:5] else palette_colors
    
    for (color in colors_to_show) {
      color_swatches <- paste0(color_swatches, 
        sprintf('<div style="width: 15px; height: 15px; background-color: %s; display: inline-block; margin: 1px;"></div>', color)
      )
    }
    
    # Add more indicator if there are more than 5 colors
    if (length(palette_colors) > 5) {
      color_swatches <- paste0(color_swatches, '<div style="display: inline-block; margin: 1px; font-size: 10px;">...</div>')
    }
    
    # Create the card
    is_selected <- ifelse(palette_name == selected_palette, "border: 3px solid #007bff;", "border: 1px solid #ddd;")
    
    palette_cards <- paste0(palette_cards, sprintf(
      '<div class="palette-card" data-palette="%s" style="cursor: pointer; padding: 8px; margin: 5px; border-radius: 8px; background: white; %s transition: all 0.2s;">
        <div style="font-weight: bold; font-size: 12px; margin-bottom: 5px;">%s</div>
        <div>%s</div>
      </div>',
      palette_name, is_selected, palette_name, color_swatches
    ))
  }
  
  # Return the HTML with JavaScript for interactivity
  tagList(
    HTML(sprintf('<div id="%s-container" style="display: flex; flex-wrap: wrap; max-height: 200px; overflow-y: auto;">%s</div>', inputId, palette_cards)),
    tags$script(HTML(sprintf('
      $(document).ready(function() {
        $("#%s-container .palette-card").click(function() {
          $("#%s-container .palette-card").css("border", "1px solid #ddd");
          $(this).css("border", "3px solid #007bff");
          Shiny.setInputValue("%s", $(this).data("palette"), {priority: "event"});
        });
      });
    ', inputId, inputId, inputId)))
  )
}

# Create visual color selector for individual sections
create_section_color_selector <- function(section_name, palettes, default_color = "#1E88E5") {
  inputId <- paste0(tolower(section_name), "_color_selector")
  
  # Create color options from all palettes
  color_options <- ""
  
  for (palette_name in names(palettes)) {
    palette_colors <- palettes[[palette_name]]
    
    for (color in palette_colors) {
      is_selected <- ifelse(color == default_color, "border: 3px solid #007bff;", "border: 1px solid #ddd;")
      
      color_options <- paste0(color_options, sprintf(
        '<div class="color-option" data-color="%s" data-section="%s" data-palette="%s" style="width: 25px; height: 25px; background-color: %s; cursor: pointer; margin: 2px; display: inline-block; border-radius: 4px; %s transition: all 0.2s;" title="%s from %s"></div>',
        color, section_name, palette_name, color, is_selected, color, palette_name
      ))
    }
  }
  
  tagList(
    div(
      h6(section_name, style = "margin-bottom: 8px; font-weight: bold;"),
      div(
        id = paste0(inputId, "_container"),
        style = "max-height: 120px; overflow-y: auto; border: 1px solid #ddd; padding: 8px; border-radius: 4px; background: #f8f9fa;",
        HTML(color_options)
      ),
      style = "margin-bottom: 15px;"
    )
  )
}

# Function to get individual section colors with fallbacks
get_individual_section_colors <- function(bienestar_color, movilidad_color, gobierno_color, infraestructura_color, participacion_color) {
  return(c(
    ifelse(is.null(bienestar_color), "#1E88E5", bienestar_color),
    ifelse(is.null(movilidad_color), "#43A047", movilidad_color),
    ifelse(is.null(gobierno_color), "#423629", gobierno_color),
    ifelse(is.null(infraestructura_color), "#8E24AA", infraestructura_color),
    ifelse(is.null(participacion_color), "#F57C00", participacion_color)
  ))
}

# NEW FUNCTION: Generate palette based on base color
generate_palette_from_color <- function(base_color) {
  # Convert hex to RGB
  rgb_vals <- col2rgb(base_color)
  r <- rgb_vals[1, 1] / 255
  g <- rgb_vals[2, 1] / 255
  b <- rgb_vals[3, 1] / 255
  
  # Convert to HSV for easier manipulation
  hsv_vals <- rgb2hsv(r, g, b)
  h <- hsv_vals[1, 1]
  s <- hsv_vals[2, 1]
  v <- hsv_vals[3, 1]
  
  # Generate palette structure similar to existing ones
  palette <- list(
    primary = base_color,
    secondary = hsv(h, s * 0.6, min(v + 0.2, 1)),
    accent = hsv((h + 0.5) %% 1, s * 0.8, min(v + 0.1, 1))
  )
  
  # Generate Sequential palette (7 colors from light to dark)
  palette$Secuencial <- c(
    hsv(h, s * 0.2, min(v + 0.4, 1)),
    hsv(h, s * 0.3, min(v + 0.3, 1)),
    hsv(h, s * 0.5, min(v + 0.2, 1)),
    hsv(h, s * 0.7, min(v + 0.1, 1)),
    base_color,
    hsv(h, s, max(v - 0.1, 0.2)),
    hsv(h, s, max(v - 0.3, 0.1))
  )
  
  # Generate Categorica palette (6 colors with variations)
  palette$Categorica <- c(
    base_color,
    hsv((h + 0.15) %% 1, s, v),
    hsv(h, s * 0.7, min(v + 0.2, 1)),
    hsv((h + 0.5) %% 1, s * 0.8, v),
    hsv((h + 0.3) %% 1, s * 0.9, min(v + 0.1, 1)),
    hsv(h, s * 0.5, min(v + 0.3, 1))
  )
  
  # Generate Divergente palette (9 colors)
  palette$Divergente <- c(
    hsv((h + 0.5) %% 1, s, max(v - 0.2, 0.2)),
    hsv((h + 0.5) %% 1, s * 0.8, max(v - 0.1, 0.3)),
    hsv((h + 0.5) %% 1, s * 0.6, v),
    hsv((h + 0.5) %% 1, s * 0.4, min(v + 0.1, 1)),
    "#DEE2E6",  # Neutral center
    hsv(h, s * 0.4, min(v + 0.1, 1)),
    hsv(h, s * 0.6, v),
    hsv(h, s * 0.8, max(v - 0.1, 0.3)),
    hsv(h, s, max(v - 0.2, 0.2))
  )
  
  return(palette)
}

# Base proposed palettes (unchanged structure but will be dynamically updated)
base_proposed_palettes <- list(
  # Colores Base
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
  
  # Colores de Distritos (more pastel-like)
  district = c("#88BDBC", "#6E9887", "#BECC92", "#FDD692", "#F1BB87", 
              "#F28A80", "#D1A5C6", "#9CADCE", "#B6C5D1", "#D3D9E0"),
  
  sections = c("#1E88E5","#43A047","#423629","#8E24AA","#F57C00"),
  
  section_names = c("Bienestar", "Movilidad", "Gobierno", "Infraestructura", "Participacion"),
  
  # Section-specific palettes (these will be dynamically updated)
  bienestar = list(
    primary = "#1E88E5",
    secondary = "#90CAF9",
    accent = "#FFA000",
    Secuencial = c("#BFD3E6", "#9EBCDA", "#8C96C6", "#6BAED6", "#4292C6", "#2171B5", "#084594"),
    Categorica = c("#1E88E5", "#42A5F5", "#90CAF9", "#FFA000", "#FFCA28", "#FFE082"),
    Divergente = colorRampPalette(c("#DC3545", "#DEE2E6", "#1E88E5"))(9)
  ),
  
  movilidad = list(
    primary = "#43A047",
    secondary = "#A5D6A7",
    accent = "#052F5F",
    Secuencial = c("#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#006D2C", "#00441B"),
    Categorica =c("#43A047", "#66BB6A", "#A5D6A7", "#052F5F", "#2D4A70", "#5788A0"),
    Divergente = colorRampPalette(c("#DC3545", "#DEE2E6", "#43A047"))(9)
  ),
  
  gobierno = list(
    primary = "#423629",
    secondary = "#8A8178",
    accent = "#FB8C00",
    Secuencial = c("#8A8178", "#756C62", "#605A4D", "#4B4238", "#423629", "#372D22", "#2C241C"),
    Categorica = c("#423629", "#605A4D", "#8A8178", "#FB8C00", "#FFA726", "#FFCC80"),
    Divergente = colorRampPalette(c("#DC3545", "#DEE2E6", "#423629"))(9)
  ),
  
  participacion = list(
    primary = "#F57C00",
    secondary = "#FFCC80",
    accent = "#039BE5",
    Secuencial = c("#FBB582", "#FA954F", "#F57C00", "#E66D00", "#D45F00", "#B65200", "#8F3F00"),
    Categorica = c("#F57C00", "#FB8C00", "#FFCC80", "#039BE5", "#29B6F6", "#81D4FA"),
    Divergente = colorRampPalette(c("#DC3545", "#DEE2E6", "#F57C00"))(9)
  ),
  
  infraestructura = list(
    primary = "#8E24AA",
    secondary = "#CE93D8",
    accent = "#00ACC1",
    Secuencial = c("#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#8E24AA", "#980043", "#67001F"),
    Categorica = c("#8E24AA", "#AB47BC", "#CE93D8", "#00ACC1", "#26C6DA", "#80DEEA"),
    Divergente = colorRampPalette(c("#DC3545", "#DEE2E6", "#8E24AA"))(9)
  ),

  # AEJ 2025 color palettes
  Warm_Orange = list(
    primary ="#FFA058",
    secondary = "#FEC08E",
    accent = "#58b7ff",
    Secuencial = c("#FFF1E7","#FED8BA","#FEC08E","#FEA970","#FD9F5D","#FFA058","#DF864C","#C16D3A","#A3552D","#854122"),
    Categorica  = c("#FFA058","#58fff4","#5863ff","#fff458","#ff5863","#58b7ff"),
    Divergente = colorRampPalette(c("#DC3545", "#DEE2E6", "#FFA058"))(9)
  ),
  Yellow = list(
    primary ="#FCEE44",
    secondary = "#D9CD59",
    accent = "#9244fc",
    Secuencial = c("#FFFDE2","#FFF7A5","#FFF175","#FBE857","#FCED58","#FCEE44","#D9CD59","#B9AD4C","#9A903F","#7B7333"),
    Categorica  = c("#FCEE44","#4452fc","#44aefc","#aefc44","#9244fc","#fc9244"),
    Divergente = colorRampPalette(c("#DC3545", "#DEE2E6", "#FCEE44"))(9)
  ),
  Clear_Turquoise = list(
    primary ="#00dbeb",
    secondary = "#97e6e6",
    accent = "#0065eb",
    Secuencial = c("#E5FBFC","#BFF0F1","#97e6e6","#71dcdc","#64dceb","#00dbeb","#52bebe","#3fa1a1","#2f8585","#226a6a"),
    Categorica  = c("#00dbeb","#eb1000","#eb0065","#eb8500","#00eb85","#0065eb"),
    Divergente = colorRampPalette(c("#DC3545", "#DEE2E6", "#00dbeb"))(9)
  ),
  Lilac_clear = list(
    primary ="#d7a4d2",
    secondary = "#eccfe9",
    accent = "#a4d7a9",
    Secuencial = c("#f8ebf6","#eccfe9","#dfb4dd","#d19ad0","#d6a4d0","#d7a4d2","#bb89b6","#9f709c","#845581","#6c3d67"),
    Categorica  = c("#d7a4d2","#a4d7a9","#b9d7a4","#a4d7c2","#d7a4b9","#c2a4d7"),
    Divergente = colorRampPalette(c("#DC3545", "#DEE2E6", "#d7a4d2"))(9)
  ),
  Pastel_Pink = list(
    primary ="#ffc9de",
    secondary = "#dfadc0",
    accent = "#c9ffcf",
    Secuencial = c("#fff0f5","#fedce7","#fec8d8","#fdb4c9","#fec9dd","#ffc9de","#dfadc0","#c092a3","#a27787","#855c6c"),
    Categorica  = c("#ffc9de","#c9ffea","#c9f9ff","#c9ffcf","#ffeac9","#ffc9f9"),
    Divergente = colorRampPalette(c("#DC3545", "#DEE2E6", "#ffc9de"))(9)
  ),
  Blue_Green = list(
    primary ="#00c5c1",
    secondary = "#87d7d6",
    accent = "#0066c5",
    Secuencial = c("#e0f6f6","#b4e7e6","#87d7d6","#5bc6c5","#3ac5c2","#00c5c1","#30a7a4","#288a86","#1f6e6a","#175350"),
    Categorica  = c("#00c5c1","#c50004","#c50066","#c55f00","#00c55f","#0066c5"),
    Divergente = colorRampPalette(c("#DC3545", "#DEE2E6", "#00c5c1"))(9)
  ),
  Strong_Pink = list(
    primary ="#ff769d",
    secondary = "#fdb8ca",
    accent = "#76ffd8",
    Secuencial = c("#fee5ec","#fdb8ca","#fd8aa6","#fc6b90","#fc749a","#ff769d","#e15d82","#c5486b","#a93455","#8d2340"),
    Categorica  = c("#ff769d","#76ffd8","#76ff94","#76e1ff","#ff9476","#ff76e1"),
    Divergente = colorRampPalette(c("#DC3545", "#DEE2E6", "#ff769d"))(9)
  ),
  Neutral_Pink = list(
    primary ="#f3d4d1",
    secondary = "#f3e3e2",
    accent = "#d1f0f3",
    Secuencial = c("#fbf6f6","#f3e3e2","#e9cfcc","#e1bbb7","#ead4d2","#f3d4d1","#caafac","#ab8b87","#8c6863","#6f4944"),
    Categorica  = c("#f3d4d1","#d1f0f3","#d1f3e5","#d1dff3","#d1f0f3","#d4d1f3"),
    Divergente = colorRampPalette(c("#DC3545", "#DEE2E6", "#f3d4d1"))(9)
  )
)

# Define UI
ui <- fluidPage(
  titlePanel("Dashboard de diseño"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("palette_type", "Tipo de Paleta:",
                  choices = c("Colores Base", "Paletas principales", "Paletas de Seccion"),
                  selected = "Paletas principales"),
      
      conditionalPanel(
        condition = "input.palette_type == 'Paletas de Seccion'",
        radioButtons("section", "Section:",
                    choices = c("Bienestar", "Movilidad", "Gobierno", "Infraestructura", "Participacion"),
                    selected = "Bienestar")
      ),
      
      conditionalPanel(
        condition = "input.palette_type == 'Paletas principales'",
        radioButtons("core_palette", "Core Palette:",
                    choices = c("Todas las secciones", "Colores de Distritos", "Colores de genero", "Colores de Grupo de Edad"),
                    selected = "Todas las secciones")
      ),
      
      conditionalPanel(
        condition = "input.palette_type == 'Paletas de Seccion'",
        radioButtons("section_palette", "Tipo de Paleta:",
                    choices = c("Colores Principales", "Secuencial", "Categorica", "Divergente"),
                    selected = "Colores Principales")
      ),
      
      # Enhanced UI controls for section color customization
      conditionalPanel(
        condition = "input.palette_type == 'Paletas principales' && input.core_palette == 'Todas las secciones'",
        radioButtons("sections_view_style", "Display Style:",
                    choices = c("Nav Cards", "Nav Bar"),
                    selected = "Nav Cards"),
        hr(),
        h5("Color Selection Method:"),
        radioButtons("color_selection_method", "",
                    choices = c("Preset Palettes" = "preset", "Individual Colors" = "individual"),
                    selected = "preset"),
        
        # Preset palette selection
        conditionalPanel(
          condition = "input.color_selection_method == 'preset'",
          h6("Choose a Preset Palette:"),
          div(id = "palette_selector_container"),
          uiOutput("palette_selector_ui")
        ),
        
        # Individual color selection
        conditionalPanel(
          condition = "input.color_selection_method == 'individual'",
          h6("Choose Colors for Each Section from Palettes:"),
          div(id = "individual_color_selectors"),
          uiOutput("individual_color_selectors_ui")
        )
      ),
      
      # UI control for map visualization when Colores de Distritos is selected
      conditionalPanel(
        condition = "input.palette_type == 'Paletas principales' && input.core_palette == 'Colores de Distritos'",
        checkboxInput("use_color_gradient", "Usar Gradiente", value = FALSE),
        helpText("Toggle between Categorica Colores de Distritos and a gradient based on values.")
      ),
      
      width = 3
    ),
    
    mainPanel(
      fluidRow(
        column(12, 
          h4("Previo de la Paleta"),
          plotlyOutput("palette_plot", height = "200px"),
          hr(),
          h4("Visualizacion Muestra"),
          # Conditional UI for the different plot types based on selection
          conditionalPanel(
            condition = "!(input.palette_type == 'Paletas principales' && (input.core_palette == 'Colores de Distritos' || input.core_palette == 'Todas las secciones'))",
            plotlyOutput("example_plot", height = "400px")
          ),
          conditionalPanel(
            condition = "input.palette_type == 'Paletas principales' && input.core_palette == 'Colores de Distritos'",
            leafletOutput("district_map", height = "400px")
          ),
          conditionalPanel(
            condition = "input.palette_type == 'Paletas principales' && input.core_palette == 'Todas las secciones'",
            htmlOutput("sections_overview")
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
  
  # Reactive values to store dynamic palettes
  proposed_palettes <- reactiveValues()
  
  # Initialize with base palettes
  observe({
    proposed_palettes$base <- base_proposed_palettes$base
    proposed_palettes$district <- base_proposed_palettes$district
    proposed_palettes$sections <- base_proposed_palettes$sections
    proposed_palettes$section_names <- base_proposed_palettes$section_names
    proposed_palettes$bienestar <- base_proposed_palettes$bienestar
    proposed_palettes$movilidad <- base_proposed_palettes$movilidad
    proposed_palettes$gobierno <- base_proposed_palettes$gobierno
    proposed_palettes$participacion <- base_proposed_palettes$participacion
    proposed_palettes$infraestructura <- base_proposed_palettes$infraestructura
    
    # Copy AEJ palettes
    for(palette_name in names(base_proposed_palettes)) {
      if(palette_name %in% c("Warm_Orange", "Yellow", "Clear_Turquoise", "Lilac_clear", 
                            "Pastel_Pink", "Blue_Green", "Strong_Pink", "Neutral_Pink")) {
        proposed_palettes[[palette_name]] <- base_proposed_palettes[[palette_name]]
      }
    }
  })
  
  # NEW: Function to map colors to their corresponding AEJ palettes
  get_aej_palette_from_color <- function(color) {
    # Create mapping from colors to AEJ palette names
    color_to_palette_map <- list(
      "#FFA058" = "Warm_Orange",
      "#FCEE44" = "Yellow", 
      "#00dbeb" = "Clear_Turquoise",
      "#d7a4d2" = "Lilac_clear",
      "#ffc9de" = "Pastel_Pink",
      "#00c5c1" = "Blue_Green",
      "#ff769d" = "Strong_Pink",
      "#f3d4d1" = "Neutral_Pink"
    )
    
    # Check if the color matches any AEJ palette
    palette_name <- color_to_palette_map[[color]]
    
    if(!is.null(palette_name)) {
      return(base_proposed_palettes[[palette_name]])
    } else {
      # If not an AEJ color, generate palette from the color
      return(generate_palette_from_color(color))
    }
  }

  # NEW: Observer to update section palettes when colors are selected
  observe({
    # Only update if we're in individual color selection mode
    if(!is.null(input$color_selection_method) && input$color_selection_method == "individual") {
      
      # Update each section palette when its color changes
      section_colors <- list(
        bienestar = input$bienestar_selected_color,
        movilidad = input$movilidad_selected_color,
        gobierno = input$gobierno_selected_color,
        infraestructura = input$infraestructura_selected_color,
        participacion = input$participacion_selected_color
      )
      
      for(section_name in names(section_colors)) {
        selected_color <- section_colors[[section_name]]
        if(!is.null(selected_color)) {
          # Use AEJ palette if available, otherwise generate from color
          new_palette <- get_aej_palette_from_color(selected_color)
          proposed_palettes[[section_name]] <- new_palette
        }
      }
    }
  })
  
  # NEW: Observer to update section palettes when preset palette changes
  observe({
    # Only update if we're in preset mode and a palette is selected
    if(!is.null(input$color_selection_method) && input$color_selection_method == "preset" &&
       !is.null(input$palette_selector)) {
      
      if(input$palette_selector != "Original") {
        # Find the palette information (check if it's one of the AEJ palettes)
        selected_palette_info <- NULL
        
        # Check if it's one of the AEJ palettes that has full palette structure
        aej_palettes <- c("Warm_Orange", "Yellow", "Clear_Turquoise", "Lilac_clear", 
                         "Pastel_Pink", "Blue_Green", "Strong_Pink", "Neutral_Pink")
        
        if(input$palette_selector %in% aej_palettes) {
          selected_palette_info <- base_proposed_palettes[[input$palette_selector]]
          
          # Update all section palettes with this information
          section_names <- c("bienestar", "movilidad", "gobierno", "infraestructura", "participacion")
          for(section_name in section_names) {
            proposed_palettes[[section_name]] <- selected_palette_info
          }
        } else {
          # For simple color palettes, generate palettes from the main color
          palette_colors <- predefined_palettes[[input$palette_selector]]
          if(length(palette_colors) > 0) {
            main_color <- palette_colors[1]  # Use first color as main
            new_palette <- generate_palette_from_color(main_color)
            
            # Update all section palettes
            section_names <- c("bienestar", "movilidad", "gobierno", "infraestructura", "participacion")
            for(section_name in section_names) {
              proposed_palettes[[section_name]] <- new_palette
            }
          }
        }
      } else {
        # Reset to original palettes
        proposed_palettes$bienestar <- base_proposed_palettes$bienestar
        proposed_palettes$movilidad <- base_proposed_palettes$movilidad
        proposed_palettes$gobierno <- base_proposed_palettes$gobierno
        proposed_palettes$participacion <- base_proposed_palettes$participacion
        proposed_palettes$infraestructura <- base_proposed_palettes$infraestructura
      }
    }
  })
  
  # Get the currently selected palette
  selected_palette <- reactive({
    if (input$palette_type == "Colores Base") {
      return(proposed_palettes$base)
    } else if (input$palette_type == "Paletas principales") {
      if (input$core_palette == "Colores de Distritos") {
        return(proposed_palettes$district)
      } else if (input$core_palette == "Colores de genero") {
        return(c("#81375D", "#03458C"))  # Pink, Blue
      } else if (input$core_palette == "Section Colors" || input$core_palette == "Todas las secciones") {
        # Use the selected section colors based on method
        if (input$color_selection_method == "individual") {
          return(get_individual_section_colors(
            input$bienestar_selected_color, input$movilidad_selected_color, input$gobierno_selected_color, 
            input$infraestructura_selected_color, input$participacion_selected_color
          ))
        } else {
          # Use preset palette
          if (!is.null(input$palette_selector) && input$palette_selector %in% names(predefined_palettes)) {
            selected_palette <- predefined_palettes[[input$palette_selector]]
            if (input$palette_selector == "Original") {
              return(selected_palette)
            } else {
              # For other palettes, use the same color for all sections or use the first color
              if (length(selected_palette) >= 1) {
                return(rep(selected_palette[1], 5))
              }
            }
          } else {
            # Default to original
            return(predefined_palettes[["Original"]])
          }
        }
      } else {
        return(c("#463285", "#553EA3", "#674EBC", "#816BC7", "#B7ABDF")) # Age groups
      }
    } else {
      # Paletas de Seccion - now uses dynamic palettes
      section <- tolower(input$section)
      section_palette_type <- (input$section_palette)
      
      section_data <- proposed_palettes[[section]]
      
      if (section_palette_type == "Colores Principales") {
        return(c(
          section_data$primary,
          section_data$secondary,
          section_data$accent
        ))
      } else {
        return(section_data[[section_palette_type]])
      }
    }
  })
  
  # Generate sample data for district map
  sample_data <- reactive({
    generate_sample_district_data()
  })
  
  # Generate Previo de la Paleta
  output$palette_plot <- renderPlotly({
    palette <- selected_palette()
    
    if (input$palette_type == "Colores Base") {
      # Create a named vector for Colores Base
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
  
  # Generate Visualizacion Muestra
  output$example_plot <- renderPlotly({
    palette <- selected_palette()
    
    # Check if palette is empty
    if (length(palette) == 0) {
      return(plot_ly() %>%
        layout(title = "No data to display"))
    }
    
    # Create some example data
    set.seed(123)
    if (input$palette_type == "Colores Base") {
      # Bar chart with Colores Base
      categories <- names(palette)[1:6]  # First 6 Colores Base
      values <- round(runif(6, 10, 100))
      colors <- unlist(palette)[1:6]
      
      plot_ly() %>%
        add_bars(x = categories, y = values, marker = list(color = colors),
                 text = ~paste(categories, ":", values),
                 hoverinfo = "text") %>%
        layout(
          title = "Example Bar Chart with Colores Base",
          xaxis = list(title = "Categories"),
          yaxis = list(title = "Values")
        )
      
    } else if (input$palette_type == "Paletas principales") {
      # Group comparison
      if (input$core_palette == "Colores de Distritos" || input$core_palette == "Todas las secciones") {
        # This is now handled in separate outputs
        return(NULL)
      }  else if (input$core_palette == "Colores de genero") {
        # Gender comparison
        plot_ly() %>%
          add_pie(labels = c("Mujer", "Hombre"), values = c(55, 45), 
                 marker = list(colors = palette),
                 textinfo = "label+percent") %>%
          layout(title = "Distribucion de genero")
      } else {
        # Age groups
        age_groups <- c("18-24", "25-34", "35-44", "45-54", "55+")
        values <- c(15, 25, 30, 20, 10)
        
        plot_ly() %>%
          add_pie(labels = age_groups, values = values, 
                 marker = list(colors = palette),
                 textinfo = "label+percent") %>%
          layout(title = "Distribucion por grupo de edad")
      }
    } else {
      # Section visualizations
      section <- tolower(input$section)
      section_palette_type <- (input$section_palette)
      
      if (section_palette_type == "Colores Principales") {
        # Main colors visualization: Primary, Secondary, Accent
        labels <- c("Primary", "Secondary", "Accent")
        values <- c(50, 30, 20)
        
        plot_ly() %>%
          add_pie(labels = labels, values = values,
                 marker = list(colors = palette),
                 textinfo = "label+percent") %>%
          layout(title = paste(input$section, "Colores Principales"))
      } else if (section_palette_type == "Secuencial") {
        # Secuencial data visualization
        x <- 1:length(palette)
        y <- seq(10, 100, length.out = length(palette))
        
        plot_ly() %>%
          add_bars(x = x, y = y, marker = list(color = palette),
                  text = ~paste("Value:", y),
                  hoverinfo = "text") %>%
          layout(
            title = paste(input$section, "Secuencial Scale"),
            xaxis = list(title = "Categories", showticklabels = FALSE),
            yaxis = list(title = "Values")
          )
      } else if (section_palette_type == "Categorica") {
        # Categorica data
        categories <- paste("Category", 1:length(palette))
        values <- round(runif(length(palette), 10, 100))
        
        plot_ly() %>%
          add_bars(x = categories, y = values, marker = list(color = palette),
                  text = ~paste(categories, ":", values),
                  hoverinfo = "text") %>%
          layout(
            title = paste(input$section, "Categorica Colors"),
            xaxis = list(title = "Categories"),
            yaxis = list(title = "Values")
          )
      } else if (section_palette_type == "Divergente") {
        # Divergente data
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
            title = paste(input$section, "Divergente Scale"),
            xaxis = list(title = "Categories", showticklabels = FALSE),
            yaxis = list(title = "Values")
          )
      }
    }
  })
  
  # District map output 
  output$district_map <- renderLeaflet({
    req(input$palette_type == "Paletas principales" && input$core_palette == "Colores de Distritos")
    
    palette <- selected_palette()
    
    # Create district map
    create_district_map(
      geo_data(),
      district_stats = sample_data()$stats,
      use_gradient = input$use_color_gradient,
      district_palette = palette
    )
  })
  
  # Generate individual color selectors UI
  output$individual_color_selectors_ui <- renderUI({
    section_names <- c("Bienestar", "Movilidad", "Gobierno", "Infraestructura", "Participacion")
    default_colors <- c("#1E88E5", "#43A047", "#423629", "#8E24AA", "#F57C00")
    
    selectors <- tagList(
      lapply(1:length(section_names), function(i) {
        create_section_color_selector(section_names[i], predefined_palettes, default_colors[i])
      }),
      # Add JavaScript for color selection
      tags$script(HTML('
        $(document).ready(function() {
          $(".color-option").click(function() {
            var section = $(this).data("section");
            var color = $(this).data("color");
            var palette = $(this).data("palette");
            var container = $(this).parent();
            
            // Remove selection from all options in this container
            container.find(".color-option").css("border", "1px solid #ddd");
            
            // Add selection to clicked option
            $(this).css("border", "3px solid #007bff");
            
            // Set Shiny input value
            var inputName = section.toLowerCase() + "_selected_color";
            Shiny.setInputValue(inputName, color, {priority: "event"});
            
            // Also set the palette name for reference
            var paletteInputName = section.toLowerCase() + "_selected_palette";
            Shiny.setInputValue(paletteInputName, palette, {priority: "event"});
          });
        });
      '))
    )
    
    return(selectors)
  })
  
  # Generate palette selector UI
  output$palette_selector_ui <- renderUI({
    create_palette_selector("palette_selector", predefined_palettes, "Original")
  })
  
  # Section overview output
  output$sections_overview <- renderUI({
    req(input$palette_type == "Paletas principales" && input$core_palette == "Todas las secciones")
    
    # Get section colors based on selection method
    if (input$color_selection_method == "individual") {
      section_colors <- get_individual_section_colors(
        input$bienestar_selected_color, input$movilidad_selected_color, input$gobierno_selected_color, 
        input$infraestructura_selected_color, input$participacion_selected_color
      )
    } else {
      # Use preset palette
      if (!is.null(input$palette_selector) && input$palette_selector %in% names(predefined_palettes)) {
        selected_palette <- predefined_palettes[[input$palette_selector]]
        if (input$palette_selector == "Original") {
          section_colors <- selected_palette
        } else {
          # For other palettes, use the first color for all sections
          if (length(selected_palette) >= 1) {
            section_colors <- rep(selected_palette[1], 5)
          } else {
            section_colors <- rep(selected_palette[1], 5)
          }
        }
      } else {
        section_colors <- predefined_palettes[["Original"]]
      }
    }
    
    section_names <- proposed_palettes$section_names
    
    # Display based on selected style
    if (input$sections_view_style == "Nav Cards") {
      create_section_nav_cards(section_colors, section_names)
    } else {
      create_section_nav_bar(section_colors, section_names)
    }
  })
  
  # Display color values
  output$color_values <- renderPrint({
    palette <- selected_palette()
    
    if (input$palette_type == "Colores Base") {
      # For Colores Base, show named list
      cat("Colores Base:\n")
      for (name in names(palette)) {
        cat(sprintf("%s: \"%s\"\n", name, palette[[name]]))
      }
    } else if (input$palette_type == "Paletas principales" && input$core_palette == "Todas las secciones") {
      # For section overview, show current selection
      if (input$color_selection_method == "individual") {
        cat("Individual Section Colors:\n")
        section_names <- proposed_palettes$section_names
        colors <- c(input$bienestar_selected_color, input$movilidad_selected_color, input$gobierno_selected_color, 
                   input$infraestructura_selected_color, input$participacion_selected_color)
        for (i in 1:length(section_names)) {
          color_val <- ifelse(is.null(colors[i]), "Not selected", colors[i])
          cat(sprintf("%s: \"%s\"\n", section_names[i], color_val))
        }
        
        # Show that section palettes have been updated
        cat("\n--- Section Palettes Updated ---\n")
        cat("When you go to 'Paletas de Seccion', each section will now use palettes based on the selected colors.\n")
        
      } else {
        cat("Selected Preset Palette:", ifelse(is.null(input$palette_selector), "Original", input$palette_selector), "\n\n")
        
        if (!is.null(input$palette_selector) && input$palette_selector != "Original") {
          # Show the full palette and highlight the main color
          full_palette <- predefined_palettes[[input$palette_selector]]
          cat("Full Palette:\n")
          cat(sprintf("c(\"%s\")\n\n", paste(full_palette, collapse = "\", \"")))
          cat("Main Color (1st position):", full_palette[1], "\n")
          cat("Used for all sections:", full_palette[1])
          cat("\n\n--- Section Palettes Updated ---\n")
          cat("All section palettes have been updated based on the selected preset.\n")
        } else {
          section_colors <- predefined_palettes[["Original"]]
          section_names <- proposed_palettes$section_names
          cat("Section Colors:\n")
          for (i in 1:length(section_colors)) {
            cat(sprintf("%s: \"%s\"\n", section_names[i], section_colors[i]))
          }
        }
      }
    } else if (input$palette_type == "Paletas de Seccion") {
      # Show current section palette information
      section <- tolower(input$section)
      section_data <- proposed_palettes[[section]]
      
      cat(sprintf("=== %s Section Palette ===\n", input$section))
      cat(sprintf("Primary: \"%s\"\n", section_data$primary))
      cat(sprintf("Secondary: \"%s\"\n", section_data$secondary))
      cat(sprintf("Accent: \"%s\"\n", section_data$accent))
      cat("\nSecuencial:\n")
      cat(sprintf("c(\"%s\")\n", paste(section_data$Secuencial, collapse = "\", \"")))
      cat("\nCategorica:\n")
      cat(sprintf("c(\"%s\")\n", paste(section_data$Categorica, collapse = "\", \"")))
      cat("\nDivergente:\n")
      cat(sprintf("c(\"%s\")\n", paste(section_data$Divergente, collapse = "\", \"")))
      
      # Note if this palette was dynamically generated
      aej_palette_used <- NULL
      color_to_palette_map <- list(
        "#FFA058" = "Warm_Orange",
        "#FCEE44" = "Yellow", 
        "#00dbeb" = "Clear_Turquoise",
        "#d7a4d2" = "Lilac_clear",
        "#ffc9de" = "Pastel_Pink",
        "#00c5c1" = "Blue_Green",
        "#ff769d" = "Strong_Pink",
        "#f3d4d1" = "Neutral_Pink"
      )
      
      # Check if this section is using an AEJ palette
      for(color in names(color_to_palette_map)) {
        if(section_data$primary == color) {
          aej_palette_used <- color_to_palette_map[[color]]
          break
        }
      }
      
      if(!identical(section_data, base_proposed_palettes[[section]])) {
        if(!is.null(aej_palette_used)) {
          cat(sprintf("\n*** This section is using the %s AEJ palette ***\n", aej_palette_used))
        } else {
          cat("\n*** This palette was dynamically generated based on your color selection ***\n")
        }
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