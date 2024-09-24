# Load necessary libraries
library(shiny)
library(leaflet)
library(sf)
library(openxlsx)   # For writing Excel files
library(zip)        # For creating ZIP files cross-platform

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
    
    # Add a panel for user controls (top right)
    absolutePanel(
        top = 10, right = 10,
        width = "300px",
        style = "background-color: white; padding: 10px; border-radius: 10px;",
        # UI elements
        uiOutput("controls_ui")
    ),
    
    # Add a panel for group counts (bottom left)
    absolutePanel(
        bottom = 10, left = 10,
        width = "300px",
        style = "background-color: white; padding: 10px; border-radius: 10px;",
        uiOutput("group_counts_ui")
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
    
    # Reactive value to track if the app has finished initializing
    appInitialized <- reactiveVal(FALSE)
    
    # Set appInitialized to TRUE after the initial flush of the reactive system
    session$onFlushed(function() {
        appInitialized(TRUE)
    }, once = TRUE)
    
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
                
                # Calculate cells per group to ensure the difference is at most 1
                cells_per_group <- num_cells %/% num_groups  # Integer division
                extra_cells <- num_cells %% num_groups       # Modulo operation
                
                # Create a vector of group sizes
                group_sizes <- rep(cells_per_group, num_groups)
                if (extra_cells > 0) {
                    group_sizes[1:extra_cells] <- group_sizes[1:extra_cells] + 1
                }
                
                # Assign group numbers based on group sizes
                group_numbers <- rep(1:num_groups, times = group_sizes)
                
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
                            label = ~paste(as.character(Maille_2), ";\n Group:", Group),
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
                selectInput("selected_cell", "Select Cell :",
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
                            min = 1, max = 20, value = 6, step = 1),
                # Add the Export button
                downloadButton("export_button", "Export")
            )
        }
    })
    
    # Output for group counts
    output$group_counts_ui <- renderUI({
        used_data <- grid_data_reactive$data[grid_data_reactive$data$Used == 1, ]
        if (nrow(used_data) > 0 && !all(is.na(used_data$Group))) {
            counts <- table(used_data$Group)
            counts_text <- paste("Group", names(counts),":",counts, "cells" )
            # Create a div with the text
            tagList(
                lapply(counts_text, function(txt) {
                    div(txt)
                })
            )
        } else {
            div("No groups available.")
        }
    })
    
    # Observe the opacity slider and update the map
    observeEvent(input$opacity_slider, {
        req(input$opacity_slider)
        update_map()
    })
    
    # Modified observeEvent for input$num_groups
    observeEvent(input$num_groups, {
        # Only proceed if the app has initialized
        if (appInitialized()) {
            # If the grid choice is 'all', switch to 'filtered' and update radio buttons
            if (input$grid_choice == 'all') {
                updateRadioButtons(session, "grid_choice", selected = "filtered")
                # Show a notification or modal dialog if desired
                showModal(modalDialog(
                    title = "Switching to Used Grid",
                    "To display group assignments, the app is displaying only selected cells.",
                    easyClose = TRUE,
                    footer = NULL
                ))
            }
            assign_groups()
            update_map()
        }
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
                    "Please save your changes before switching mode to see the entire grid.",
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
    
    # Download handler for exporting files
    output$export_button <- downloadHandler(
        filename = function() {
            paste("Export_", Sys.Date(), ".zip", sep = "")
        },
        content = function(file) {
            # Create a temporary directory
            temp_dir <- tempdir()
            export_dir <- file.path(temp_dir, "Export_Files")
            dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)
            
            # Ensure Group assignments are up-to-date
            assign_groups()
            
            # Fix invalid geometries
            grid_data_reactive$data <- st_make_valid(grid_data_reactive$data)
            
            # Subset the used grid cells
            used_data <- grid_data_reactive$data[grid_data_reactive$data$Used == 1, ]
            
            num_groups <- input$num_groups
            gpx_dir <- file.path(export_dir, "GPX_Files")
            dir.create(gpx_dir, showWarnings = FALSE, recursive = TRUE)
            gpx_files <- c()
            
            # Define a suitable projected CRS (e.g., UTM zone appropriate for your data)
            # Replace 'your_epsg_code' with the appropriate EPSG code
            projected_crs <- 2154  # Example: RGF93 / Lambert-93 for France
            
            # Generate GPX files for each group
            for (g in 1:num_groups) {
                group_data <- used_data[used_data$Group == g, ]
                if (nrow(group_data) > 0) {
                    # Transform to projected CRS
                    group_data_proj <- st_transform(group_data, crs = projected_crs)
                    
                    # Calculate point within each polygon
                    centroids_proj <- st_point_on_surface(group_data_proj)
                    
                    # Transform centroids back to WGS84
                    centroids <- st_transform(centroids_proj, crs = 4326)
                    
                    # Create a 'name' field for GPX
                    centroids$name <- as.character(centroids$Maille_2)
                    
                    # Keep only necessary fields
                    centroids <- centroids[, c("name", "geometry")]
                    
                    # Define file path
                    gpx_file <- file.path(gpx_dir, paste0("Group_", g, ".gpx"))
                    
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
            
            # Save the shapefile of the grid with group information
            grid_shapefile_dir <- file.path(export_dir, "Grid_Shapefile")
            dir.create(grid_shapefile_dir, showWarnings = FALSE, recursive = TRUE)
            grid_shapefile <- file.path(grid_shapefile_dir, "Grid_with_Groups.shp")
            
            # Write the shapefile
            st_write(
                grid_data_reactive$data,
                grid_shapefile,
                driver = "ESRI Shapefile",
                delete_layer = TRUE,
                quiet = TRUE
            )
            
            # Prepare the Excel file
            # Transform used_data to projected CRS
            used_data_proj <- st_transform(used_data, crs = projected_crs)
            
            # Calculate centroids
            centroids_proj <- st_centroid(used_data_proj)
            
            # Transform centroids back to WGS84
            centroids <- st_transform(centroids_proj, crs = 4326)
            
            coords <- st_coordinates(centroids)
            
            # Create a data frame
            excel_data <- data.frame(
                Maille_2 = centroids$Maille_2,
                X = coords[, "X"],
                Y = coords[, "Y"],
                Group = centroids$Group
            )
            
            # Write to Excel file
            excel_file <- file.path(export_dir, "Grid_Centroids.xlsx")
            write.xlsx(excel_data, excel_file, overwrite = TRUE)
            
            # Create the ZIP file
            zip_file <- file.path(temp_dir, paste("Export_", Sys.Date(), ".zip", sep = ""))
            zip::zipr(
                zipfile = zip_file,
                files = list.files(export_dir, full.names = TRUE, recursive = TRUE),
                root = export_dir
            )
            
            # Send the ZIP file to the user
            file.copy(zip_file, file)
        },
        contentType = "application/zip"
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
