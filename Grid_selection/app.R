# Load necessary libraries
library(shiny)
library(leaflet)
library(sf)
library(utils)   # For zip function

# Load the shapefile (replace with your actual file path)
shapefile_path <- "Data/Polycheate_reef_grid.shp"
grid_data <- st_read(shapefile_path)

# Ensure 'Used' and 'Group' columns exist before converting to reactiveValues
if (!'Used' %in% names(grid_data)) {
    grid_data$Used <- 1  # Set default as appropriate
}
if (!'Group' %in% names(grid_data)) {
    grid_data$Group <- NA
}

# Convert grid_data to a reactiveValues object to allow modification
grid_data_reactive <- reactiveValues(data = grid_data)

# Define the UI
ui <- fluidPage(
    # Fullscreen leaflet map output
    leafletOutput("mymap", width = "100%", height = "100%"),
    
    # Add a panel for user controls
    absolutePanel(
        top = 10, right = 10,
        width = "300px",
        style = "background-color: white; padding: 10px; border-radius: 10px;",
        # UI elements
        uiOutput("controls_ui")
    ),
    
    # Remove extra margin/padding and ensure full-screen layout
    tags$style(
        HTML("
              html, body {
                height: 100%;
                width: 100%;
                margin: 0;
                padding: 0;
                overflow: hidden;
              }
              #mymap {
                height: 100vh;
                width: 100vw;
                position: absolute;
                top: 0;
                left: 0;
              }
            ")
    )
)

server <- function(input, output, session) {
    # Reactive value to track if we're in edit mode
    rv <- reactiveValues(editMode = FALSE)
    
    # Function to assign groups to used cells
    assign_groups <- function() {
        isolate({
            # Get the number of groups
            num_groups <- input$num_groups
            
            # Subset the used grid cells
            used_cells <- grid_data_reactive$data[grid_data_reactive$data$Used == 1, ]
            
            # Number of used cells
            num_cells <- nrow(used_cells)
            
            if (num_cells > 0) {
                # Sort used_cells by 'Maille_2' in alphabetical order
                used_cells <- used_cells[order(used_cells$Maille_2), ]
                
                # Calculate number of cells per group
                cells_per_group <- ceiling(num_cells / num_groups)
                
                # Assign group numbers sequentially based on the sorted order
                group_numbers <- rep(1:num_groups, each=cells_per_group)[1:num_cells]
                
                # Assign the group numbers to the 'Group' column of the used cells
                used_cells$Group <- group_numbers
                
                # Update the 'Group' column in grid_data_reactive$data
                grid_data_reactive$data$Group[grid_data_reactive$data$Used == 1] <- used_cells$Group
            } else {
                # If no used cells, set Group to NA
                grid_data_reactive$data$Group <- NA
            }
        })
    }
    
    # Function to update the map
    update_map <- function() {
        isolate({
            if (!rv$editMode) {
                grid_data_to_display <- if (input$grid_choice == "filtered") {
                    subset(grid_data_reactive$data, Used == 1)
                } else {
                    grid_data_reactive$data
                }
                
                # If grid_choice is 'filtered' and num_groups >= 1, use group colors
                if (input$grid_choice == "filtered" && input$num_groups >= 1) {
                    # Define colors for the groups
                    num_groups <- input$num_groups
                    group_colors <- colorFactor(rainbow(num_groups), grid_data_to_display$Group)
                    
                    leafletProxy("mymap") %>%
                        clearShapes() %>%
                        addPolygons(
                            data = grid_data_to_display,
                            color = ~group_colors(Group),
                            weight = 2,
                            opacity = 1.0,
                            fillOpacity = input$opacity_slider,
                            layerId = ~Maille_2,
                            label = ~paste("Maille_2:", as.character(Maille_2), "<br>Group:", Group),
                            labelOptions = labelOptions(
                                style = list(
                                    "color" = "black",
                                    "font-size" = "14px",
                                    "font-weight" = "bold",
                                    "background-color" = "white",
                                    "border" = "1px solid black",
                                    "padding" = "5px"
                                ),
                                direction = "auto",
                                textOnly = FALSE,
                                offset = c(20, -20)
                            )
                        )
                } else {
                    # Use default blue color
                    leafletProxy("mymap") %>%
                        clearShapes() %>%
                        addPolygons(
                            data = grid_data_to_display,
                            color = "blue",
                            weight = 2,
                            opacity = 1.0,
                            fillOpacity = input$opacity_slider,
                            layerId = ~Maille_2,
                            label = ~as.character(Maille_2),
                            labelOptions = labelOptions(
                                style = list(
                                    "color" = "black",
                                    "font-size" = "14px",
                                    "font-weight" = "bold",
                                    "background-color" = "white",
                                    "border" = "1px solid black",
                                    "padding" = "5px"
                                ),
                                direction = "auto",
                                textOnly = TRUE,
                                offset = c(20, -20)
                            )
                        )
                }
            }
        })
    }
    
    # Render the initial leaflet map
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("Esri.WorldImagery") %>%
            setView(lng = -2.1773, lat = 46.904043 , zoom = 16) %>%
            # Display the All Grid by default
            addPolygons(
                data = grid_data_reactive$data,
                color = "blue",
                weight = 2,
                opacity = 1.0,
                fillOpacity = ifelse(is.null(input$opacity_slider), 0.5, input$opacity_slider),
                layerId = ~Maille_2,
                label = ~as.character(Maille_2),
                labelOptions = labelOptions(
                    style = list(
                        "color" = "black",
                        "font-size" = "14px",
                        "font-weight" = "bold",
                        "background-color" = "white",
                        "border" = "1px solid black",
                        "padding" = "5px"
                    ),
                    direction = "auto",
                    textOnly = TRUE,
                    offset = c(20, -20)
                )
            )
    })
    
    # UI elements based on edit mode
    output$controls_ui <- renderUI({
        if (rv$editMode) {
            # Editing UI
            tagList(
                actionButton("save_button", "Save"),
                selectInput("selected_cell", "Select Cell (Maille_2):",
                            choices = grid_data_reactive$data$Maille_2,
                            selected = NULL),
                actionButton("modify_button", "Modify")
            )
        } else {
            # Controls when not in edit mode
            tagList(
                radioButtons("grid_choice", "Choose grid to display:",
                             choices = list("All Grid" = "all", "Used Grid" = "filtered"),
                             selected = "all"),
                # Conditionally show the "Edit the grid" button only when "Used Grid" is selected
                conditionalPanel(
                    condition = "input.grid_choice == 'filtered'",
                    actionButton("used_grid_button", "Edit the grid")
                ),
                sliderInput("opacity_slider", "Adjust Grid Opacity:",
                            min = 0, max = 1, value = 0.5, step = 0.1),
                sliderInput("num_groups", "Number of Groups:",
                            min = 1, max = 20, value = 1, step = 1),
                # Add the Export GPX Files button
                downloadButton("export_button", "Export GPX Files")
            )
        }
    })
    
    # Observe the opacity slider and update the map
    observeEvent(input$opacity_slider, {
        req(input$opacity_slider)
        update_map()
    })
    
    # Observe the number of groups slider and update group assignments and map
    observeEvent(input$num_groups, {
        assign_groups()
        update_map()
    })
    
    # Observe the "Edit the grid" button click
    observeEvent(input$used_grid_button, {
        rv$editMode <- TRUE  # Enter edit mode
        
        # Update the map to show Used grid in green and Raw grid in red
        leafletProxy("mymap") %>%
            clearShapes() %>%
            addPolygons(
                data = grid_data_reactive$data,
                color = ~ifelse(Used == 1, "green", "red"),
                weight = 2,
                opacity = 1.0,
                fillOpacity = 0.5,
                layerId = ~Maille_2,
                label = ~as.character(Maille_2),
                labelOptions = labelOptions(
                    style = list(
                        "color" = "black",
                        "font-size" = "14px",
                        "font-weight" = "bold",
                        "background-color" = "white",
                        "border" = "1px solid black",
                        "padding" = "5px"
                    ),
                    direction = "auto",
                    textOnly = TRUE,
                    offset = c(20, -20)
                )
            )
    })
    
    # Observe the "Modify" button click
    observeEvent(input$modify_button, {
        req(input$selected_cell)  # Ensure a cell is selected
        
        # Find the index of the selected cell
        idx <- which(grid_data_reactive$data$Maille_2 == input$selected_cell)
        
        # Toggle the "Used" value
        grid_data_reactive$data$Used[idx] <- ifelse(grid_data_reactive$data$Used[idx] == 1, 0, 1)
        
        # Recompute group assignments
        assign_groups()
        
        # Update the map to show all cells with updated colors
        leafletProxy("mymap") %>%
            clearShapes() %>%
            addPolygons(
                data = grid_data_reactive$data,
                color = ~ifelse(Used == 1, "green", "red"),
                weight = 2,
                opacity = 1.0,
                fillOpacity = 0.5,
                layerId = ~Maille_2,
                label = ~as.character(Maille_2),
                labelOptions = labelOptions(
                    style = list(
                        "color" = "black",
                        "font-size" = "14px",
                        "font-weight" = "bold",
                        "background-color" = "white",
                        "border" = "1px solid black",
                        "padding" = "5px"
                    ),
                    direction = "auto",
                    textOnly = TRUE,
                    offset = c(20, -20)
                )
            )
    })
    
    # Observe the "Save" button click
    observeEvent(input$save_button, {
        rv$editMode <- FALSE  # Exit edit mode
        
        # Recompute group assignments
        assign_groups()
        
        # Update the map based on the grid_choice
        update_map()
    })
    
    # Observe the grid_choice radio buttons
    observeEvent(input$grid_choice, {
        # Check if in edit mode
        if (rv$editMode) {
            if (input$grid_choice == "all") {
                # Show a message to the user
                showModal(modalDialog(
                    title = "Please Save Before Switching",
                    "Please save your changes before switching to All Grid.",
                    easyClose = TRUE,
                    footer = NULL
                ))
                
                # Reset the grid_choice back to "filtered"
                updateRadioButtons(session, "grid_choice", selected = "filtered")
            }
        } else {
            update_map()
        }
    })
    
    # Download handler for exporting GPX files
    # Download handler for exporting GPX files
    output$export_button <- downloadHandler(
        filename = function() {
            paste("GPX_files_", Sys.Date(), ".zip", sep = "")
        },
        content = function(file) {
            # Create a temporary directory
            temp_dir <- tempdir()
            
            # Ensure Group assignments are up-to-date
            assign_groups()
            
            # Subset the used grid cells
            used_data <- grid_data_reactive$data[grid_data_reactive$data$Used == 1, ]
            
            num_groups <- input$num_groups
            gpx_files <- c()
            
            for (g in 1:num_groups) {
                group_data <- used_data[used_data$Group == g, ]
                if (nrow(group_data) > 0) {
                    # Calculate point within each polygon
                    centroids <- st_point_on_surface(group_data)
                    
                    # Ensure centroids are in WGS84 (EPSG:4326)
                    centroids <- st_transform(centroids, crs = 4326)
                    
                    # Create a 'name' field for GPX
                    centroids$name <- as.character(centroids$Maille_2)
                    
                    # Keep only necessary fields
                    centroids <- centroids[, c("name", "geometry")]
                    
                    # Define file path
                    gpx_file <- file.path(temp_dir, paste0("Group_", g, ".gpx"))
                    
                    # Delete the file if it exists
                    if (file.exists(gpx_file)) {
                        file.remove(gpx_file)
                    }
                    
                    # Write GPX file
                    st_write(
                        centroids,
                        gpx_file,
                        driver = "GPX",
                        layer = "waypoints",
                        delete_dsn = TRUE,
                        quiet = TRUE
                    )
                    gpx_files <- c(gpx_files, gpx_file)
                }
            }
            # Zip the GPX files
            zip(zipfile = file, files = gpx_files, flags = "-j")
        },
        contentType = "application/zip"
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
