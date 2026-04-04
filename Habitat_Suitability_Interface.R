
### Habitat Suitability Interface

##### Packages #####
library(shiny)
library(ggplot2) 
library(dplyr)
library(terra) 
library(sf)
library(tigris)
library(tidyterra)
library(landscapemetrics)
library(tidyr)
library(leaflet)
library(progress)
library(patchwork)
library(openxlsx)
library(readxl)
library(tmap)


# ---- LOAD GRID ONCE ----
County_metrics_sf <- st_read(
  "https://raw.githubusercontent.com/huh0001/Habitat_Suitability_Model/main/Pennsylvania.gpkg",
  quiet = TRUE
)

# ---- UI ----
ui <- fluidPage(
  
  
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Times New Roman', Times, serif;
      background-color: #CD853F;
        margin-bottom: 60px; /* prevent footer overlap */
      }
      
      .footer {
        position: fixed;
        bottom: 0;
        left: 0;
        width: 100%;
        height: 40px;
        background-color: #001f3f;
        color: white;
        display: flex;
        align-items: center;
        padding-left: 15px;
        font-size: 14px;
      }
    "))
  ),
  
  tags$div(
    style = "
      text-align: center;
      font-weight: bold;
      font-size: 30px;
      padding: 15px;
      background-color: #001f3f;
      color: white;
      margin-bottom: 20px;
    ",
    "Habitat Suitability Model"
  ),
  
  uiOutput("page_ui"), 
  
  tags$div(
    class = "footer",
    "Questions? Contact: huh0001@auburn.edu"
  )
)


tmap_mode("plot")

# ---- SERVER ----
server <- function(input, output, session) {
  
  # Reactive state manager
  rv <- reactiveValues(
    page = 1,
    selected_state = NULL,
    selected_county = NULL,
    sample_cells = NULL,
    current_index = 1,
    results = data.frame(cell_id = integer(), prob = numeric()),
    assess_prob2 = NULL,
    County_predictions = NULL,
    completed_cells = FALSE, 
    instructions_seen = FALSE, 
    sample_size = 10
  )
  
  # ---- PAGE UI SWITCH ----
  output$page_ui <- renderUI({
    
    if (rv$page == 1) {
      
      fluidPage(
        titlePanel("Step 1: Select State"),
        
        selectInput("state", "Choose State:", choices = state.name),
        
        actionButton("state_next", "Submit")
      )
      
    } else if (rv$page == 2) {
      
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            uiOutput("county_ui"),
            actionButton("submit_county", "Submit County")
          ),
          mainPanel(
            tmapOutput("map_county")
          )
        )
      )
      
    } else if (rv$page == 3) {
      
      fluidPage(
        titlePanel("Step 3: Habiat Training"),
        
        sidebarLayout(
          sidebarPanel(
            
            conditionalPanel(
              condition = "output.instructions_seen == true",
              
              numericInput("prob", "Enter probability (0–1):",
                           value = NA, min = 0, max = 1, step = 0.01),
              
              actionButton("next_btn", "Next"),
              actionButton("add_cells", "Add Cells"),  # ✅ NEW BUTTON
              
              br(), br(),
              
              textOutput("progress"),
              
              uiOutput("analysis_ui")
            )
          ),
          
          mainPanel(
            tmapOutput("map")
          )
        )
      )
      
    } else if (rv$page == 4) {
      
      fluidPage(
        titlePanel("Step 4: Predicted Habitat Probability"),
        
        tmapOutput("map_prob")
      )
    }
  })
  
  # ---- PAGE 1 → PAGE 2 ----
  observeEvent(input$state_next, {
    
    req(input$state)
    
    # Convert full name → abbreviation
    abbr <- state.abb[match(input$state, state.name)]
    
    # Pull counties for that state
    rv$State <- counties(state = abbr, cb = TRUE)
    
    # Save to global environment if you want
    assign("State", rv$State, envir = .GlobalEnv)
    
    rv$page <- 2
  })
  
  # ---- PAGE 2 MAP ----
  output$county_ui <- renderUI({
    
    req(rv$State)
    
    selectInput(
      "county",
      "Select County:",
      choices = sort(unique(rv$State$NAME))
    )
  })
  
  # output$map_county <- renderTmap({
  #   
  #   req(rv$State_subset)
  #   
  #   tm_shape(st_transform(rv$State_subset, 3857)) +
  #     tm_basemap("Esri.WorldImagery") +
  #     tm_borders(col = "black", lwd = 3) +
  #     tm_layout(frame = FALSE)
  # })
  
  # ---- PAGE 2 → PAGE 3 ----
 
  
   observeEvent(input$submit_county, {
    
    req(rv$State, input$county)
    
    # Subset selected county
    rv$State_subset <- rv$State %>%
      dplyr::filter(NAME == input$county)
    
    # ---- CRS ALIGNMENT ----
    county_poly <- st_transform(rv$State_subset, st_crs(County_metrics_sf))
    
    # ---- CLIP GRID ----
    rv$county_grid <- st_intersection(
      County_metrics_sf,
      county_poly
    )
    
    # ---- SAFETY CHECK ----
    req(nrow(rv$county_grid) > 0)
    
    # ---- SAMPLE CELLS ----
    rv$sample_cells <- rv$county_grid %>%
      dplyr::slice_sample(n = min(rv$sample_size, nrow(rv$county_grid)))
    
    rv$current_index <- 1
    
    # Move to Page 3
    rv$page <- 3
  })
  
  
  observeEvent(rv$page, {
    
    if (rv$page == 3 && !rv$instructions_seen) {
      
      showModal(modalDialog(
        
        title = "Instructions:",
        
        tags$div(
          tags$p("A. The following grid cells are 1 mile^2 each. Imagine you are a deer that has the ability to live anywhere on the landscape, but you cannot leave grid cell. Enter the liklihood (0-1) that you would live in the following.")
        ),
        tags$p("B. Select the Next to move to the next cell. Select the Add Cells button to add more cells you inspect. The more cells inspected the more reliable the results. "),
        tags$p("C. After you inspect the desired number of cells click the Run Analysis button to contine."),
        
        easyClose = FALSE,
        
        footer = actionButton("dismiss_instructions", "Dismiss")
      ))
    }
  })
  
  observeEvent(input$dismiss_instructions, {
    rv$instructions_seen <- TRUE
    removeModal()
  })
  
  output$instructions_seen <- reactive({
    rv$instructions_seen
  })
  
  observeEvent(input$add_cells, {
    
    req(rv$county_grid)
    
    # Identify already used cell IDs
    used_ids <- rv$sample_cells$cell_id
    
    # Find remaining unused cells
    remaining_cells <- rv$county_grid %>%
      dplyr::filter(!cell_id %in% used_ids)
    
    # If no more cells available
    if (nrow(remaining_cells) == 0) {
      showNotification("No more new cells available.", type = "warning")
      return()
    }
    
    # Sample up to 5 new cells
    new_cells <- remaining_cells %>%
      dplyr::slice_sample(n = min(5, nrow(remaining_cells)))
    
    # Append to existing sample
    rv$sample_cells <- dplyr::bind_rows(rv$sample_cells, new_cells)
    
    showNotification(
      paste("Added", nrow(new_cells), "new cells. Total cells:", nrow(rv$sample_cells)),
      type = "message"
    )
  })
  
  outputOptions(output, "instructions_seen", suspendWhenHidden = FALSE)
  # ---- CURRENT CELL ----
  current_cell <- reactive({
    rv$sample_cells[rv$current_index, ]
  })
  
  # ---- MAP (PAGE 3) ----
  output$map <- renderTmap({
    
    req(current_cell())
    
    

    cell <- current_cell()
    
    tm_shape(cell) +
      tm_basemap("Esri.WorldImagery") +
      #tm_polygons(alpha = 0.6) +
      tm_borders(col = "black", lwd = 5) +
      tm_view(
        bbox = st_bbox(cell),
        set.zoom.limits = c(10, 18)
      )
  })
  
  # ---- PROGRESS ----
  output$progress <- renderText({
    paste("Cell", rv$current_index, "of", nrow(rv$sample_cells))
  })
  
  # ---- NEXT BUTTON ----
  observeEvent(input$next_btn, {
    
    req(input$prob)
    
    new_row <- data.frame(
      cell_id = current_cell()$cell_id,
      prob = input$prob
    )
    
    rv$results <- bind_rows(rv$results, new_row)
    
    if (rv$current_index < nrow(rv$sample_cells)) {
      
      rv$current_index <- rv$current_index + 1
      updateNumericInput(session, "prob", value = NA)
      
    } else {
      
      showNotification("All cells completed!", type = "message")
      
      rv$assess_prob2 <- County_metrics_sf %>%
        st_transform(st_crs(County_metrics_sf)) %>%
        left_join(rv$results, by = "cell_id") %>%
        filter(!is.na(prob))
      
      rv$completed_cells <- TRUE
    }
      
    
  })
  
  # ---- SHOW RUN ANALYSIS BUTTON ----
  output$analysis_ui <- renderUI({
    
    req(rv$page == 3)
    
    if (isTRUE(rv$completed_cells)) {
      actionButton("run_analysis", "Run Analysis")
    }
    
  })
  
  # ---- RUN ANALYSIS ----
  observeEvent(input$run_analysis, {
    
    req(rv$assess_prob2)
    
    model_df <- rv$assess_prob2 %>%
      st_drop_geometry() %>%
      select(
        prob,
        contiguity, core_area, disjunct_den, division,
        Edge.Density, gyrate, near_neighbor_dist,
        Patch.Density, Total.Edge, total_Class_Area,
        total_Patch_Area
      ) %>%
      na.omit()
    
    newdata <- County_metrics_sf %>%
      st_drop_geometry() %>%
      select(
        contiguity, core_area, disjunct_den, division,
        Edge.Density, gyrate, near_neighbor_dist,
        Patch.Density, Total.Edge, total_Class_Area,
        total_Patch_Area
      )
    
    # Adjust probabilities
    n <- nrow(model_df)
    model_df$prob_adj <- (model_df$prob * (n - 1) + 0.5) / n
    model_df$logit_prob <- log(model_df$prob_adj / (1 - model_df$prob_adj))
    
    model <- lm(
      logit_prob ~ contiguity + core_area + disjunct_den + division +
        Edge.Density + gyrate + near_neighbor_dist +
        Patch.Density + Total.Edge + total_Class_Area +
        total_Patch_Area,
      data = model_df
    )
    
    logit_pred <- predict(model, newdata = newdata)
    
    stopifnot(length(logit_pred) == nrow(County_metrics_sf))
    
    prob_pred <- exp(logit_pred) / (1 + exp(logit_pred))
    
    rv$County_predictions <- County_metrics_sf
    rv$County_predictions$pred_prob <- prob_pred
    
    rv$page <- 4
  })
  # ---- PAGE 4 MAP ----
  output$map_prob <- renderTmap({
    
    req(rv$County_predictions)
    req(rv$State)
    
    tm_shape(rv$County_predictions) +
      tm_basemap("Esri.WorldImagery") +
      tm_polygons(
        col = "pred_prob",
        palette = "viridis",
        alpha = 0.6
      ) +
      tm_shape(rv$State) +
      tm_borders(col = "black", lwd = 3)
  })

  
}



# ---- RUN APP ----
shinyApp(ui, server)
