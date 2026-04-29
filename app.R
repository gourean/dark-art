# ==============================================================================
# 0. SETUP & PACKAGE INSTALLATION
# Ensure all required packages are installed before loading.
# ==============================================================================

# Load libraries using require() to bypass Shinylive's static dependency preloading
# This helps resolve the "rlang 1.1.6 already loaded" conflict in the browser.
suppressPackageStartupMessages({
  require(shiny)
  require(rlang)        # Load early to prevent version conflicts in WebR
  require(bslib)        # For modern, clean UI
  require(colourpicker) # For custom color selection
  require(dplyr)        # For data manipulation
  require(DT)           # For interactive tables
  require(dunn.test)    # For non-parametric pairwise comparisons
  require(ggplot2)      # For plotting
  require(ggsignif)     # For significance brackets
  require(glue)         # For smart text generation
  require(munsell)      # Explicitly load to fix ggplot2 dependency error
  require(scales)       # Explicitly load for formatting
  require(svglite)      # Explicitly load for SVG export
  require(tidyr)        # For data reshaping
})

# ==============================================================================
# 1. THE FRONTEND (UI)
# This section defines what the user sees.
# ==============================================================================
ui <- page_sidebar(
   # A. The Theme (Medical/Clean look)
  theme = bs_theme(bootswatch = "minty", version = 5), 
  tags$head(
    tags$link(rel = "icon", href = "favicon.ico"),
    tags$style(HTML("
      /* Make Accordion Transparent in Sidebar */
      .accordion-button {
        background-color: transparent !important;
        box-shadow: none !important;
      }
      .accordion-item {
        background-color: transparent !important;
        border: none !important;
      }
      .accordion-button:not(.collapsed) {
         background-color: rgba(0,0,0,0.05) !important;
      }

      /* High Z-Index for Modals & Notifications to appear above Full Screen cards */
      .modal-backdrop { z-index: 10000 !important; }
      .modal { z-index: 10001 !important; }
      .shiny-notification { z-index: 10002 !important; }

      /* Mobile Layout Adjustments */
      @media (max-width: 767.98px) {
        /* Force outer sidebar to top */
        .bslib-page-sidebar > .bslib-sidebar-layout {
            display: flex !important;
            flex-direction: column !important;
        }
        /* Left Sidebar (Input) */
        .bslib-page-sidebar > .bslib-sidebar-layout > .sidebar {
            order: -1 !important; /* Move to Top */
            width: 100% !important;
            max-height: 30vh !important;
            overflow-y: auto !important;
            margin-bottom: 20px !important;
        }
        /* Main Content */
        .bslib-page-sidebar > .bslib-sidebar-layout > .main {
            order: 1 !important; /* Middle */
        }
        
        /* INNER Sidebar (Right Sidebar - Settings) */
        #settings_sidebar.bslib-sidebar-layout {
            display: flex !important;
            flex-direction: column !important;
        }
        #settings_sidebar > .main {
            order: 1 !important; /* Visualization Top */
        }
        #settings_sidebar > .sidebar {
            order: 2 !important; /* Settings Bottom */
        }
      }
    ")),
  ), 
  title = div(img(src = "logo.svg", height = "40px", style = "margin-right: 10px; vertical-align: middle;"), "DARK-ART: Data Analysis Rapid Kit-Automated R Tool"),
  
  # B. The Sidebar (Inputs)
  sidebar = sidebar(
    fileInput("file", "Step 1: Upload Data (CSV)", accept = ".csv"),
    actionLink("demo_data", "Load dummy dataset", icon = icon("table"), style = "font-size: 0.9em; margin-top: -10px; margin-bottom: 20px; display: inline-block;"),
    
    # Analysis Type Toggle
    radioButtons("analysis_complexity", "Analysis Type:", 
                 choices = c("Basic", "Advanced"), 
                 inline = TRUE, selected = "Basic"),
    hr(),
    
    radioButtons("analysis_type", "Step 2: Analysis Mode",
                 choices = c("Independent Groups" = "Independent", 
                             "Paired Groups" = "Paired")),
    
    # Conditional UI for Independent Analysis
    conditionalPanel(
      condition = "input.analysis_type == 'Independent'",
      selectInput("outcome_var", "Step 3: Outcome (Measure)", choices = NULL),
      selectInput("group_var", "Step 4: Grouping (Category)", choices = NULL)
    ),
    
    # Conditional UI for Paired Analysis
    conditionalPanel(
      condition = "input.analysis_type == 'Paired'",
      selectInput("paired_vars", "Step 3: Select Matched Columns", choices = NULL, multiple = TRUE),
      helpText("Select 2 columns for Paired T-test/Wilcoxon/McNemar. Select >2 for RM ANOVA/Friedman.")
    ),

    # Conditional UI for Correlation Analysis (Added)
    conditionalPanel(
      condition = "input.analysis_type == 'Correlation'",
      selectInput("corr_var1", "Step 3: Variable 1 (Numeric)", choices = NULL),
      selectInput("corr_var2", "Step 3: Variable 2 (Numeric)", choices = NULL)
    ),
    

    # Advanced Options Panel (Visible only in Advanced Mode)
    conditionalPanel(
      condition = "input.analysis_complexity == 'Advanced'",
      
      div(style = "padding-top: 10px;",
        checkboxInput("enable_split", "Split Group", value = FALSE),
        conditionalPanel(
          condition = "input.enable_split == true",
          selectInput("split_var", "Split By Variable:", choices = NULL) 
        ),
        
        checkboxInput("enable_filter", "Filter Data", value = FALSE),
        conditionalPanel(
           condition = "input.enable_filter == true",
           selectInput("filter_var", "Filter Variable:", choices = NULL),
           uiOutput("filter_ui")
        ),
        
        hr(),
        tags$label("Variable Type Overrides", class = "control-label"),
        helpText("Force variables to be treated as specific types if auto-detection fails."),
        selectInput("force_outcome_type", "Force Outcome Type:", choices = c("Auto", "Numeric", "Categorical")),
        selectInput("force_group_type", "Force Grouping Type:", choices = c("Auto", "Categorical"))
      )
    ),
    
    
    actionButton("analyze", "Run Analysis", class = "btn-success", width = "100%"),
    
    # Graph Customization Button
    actionButton("show_graph_settings", "Settings & Export", icon = icon("gear"), class = "btn-secondary"),
    
    hr(), # Horizontal line

    # Note
    helpText("Built by Wong GR. v1.3.0 available on ", a(href="https://github.com/gourean/dark-art", "Github", target="_blank"))
  ),
  
  # C. The Main Display (Outputs) -> Nested Sidebar Layout
  layout_sidebar(
    id = "settings_sidebar",
    fillable = TRUE, # Allow filling the space
    
    # --- RIGHT SIDEBAR (Graph Settings) ---
    sidebar = sidebar(
      id = "right_sidebar", # Explicit ID for toggling
      position = "right",
      open = FALSE, # Hidden by default
      title = "Settings",
      width = 320,  # Wider for options
      
      # --- GRAPH CONTROLS ---
      conditionalPanel(
        condition = "input.viz_view === 'Visualization'",
        h5("Graph Options"),
        checkboxInput("show_plot_title", "Show Plot Title", value = TRUE),
        checkboxInput("show_footnote_viz", "Show Test Name (Footnote)", value = TRUE),
        checkboxInput("show_jitter", "Show Data Points (Jitter)", value = FALSE),
        checkboxInput("show_boxplot_caps", "Show Boxplot Caps", value = FALSE),
        checkboxInput("remove_axis_gap", "Remove Gap from X-Axis", value = FALSE),
        checkboxInput("remove_axis_gap_y", "Remove Gap from Y-Axis", value = TRUE),
        
        div(style = "margin-bottom: 10px;",
            checkboxInput("add_ref_line", "Add Reference Line", value = FALSE),
            conditionalPanel(
               condition = "input.add_ref_line == true",
               numericInput("ref_line_val", "Reference y=", value = 0)
            )
        ),
        
        accordion(
           open = FALSE,
           accordion_panel(
             "Appearance",
             textInput("custom_title", "Plot Title", placeholder = "Auto"),
             textInput("custom_x", "X Axis Label", placeholder = "Auto"),
             textInput("custom_y", "Y Axis Label", placeholder = "Auto"),
             
             h6("Y-Axis Scale"),
             div(style="display: flex; gap: 5px;",
                 numericInput("y_min", "Min", value = NULL, width = "33%"),
                 numericInput("y_max", "Max", value = NULL, width = "33%"),
                 numericInput("y_step", "Step", value = NULL, width = "33%")
             ),
             actionLink("reset_y_axis", "Reset to Auto", icon = icon("rotate-left"), style="font-size: 0.8em; margin-bottom: 10px; display: inline-block;"),
             

             
             hr(),
             h6("Colour"),
             radioButtons("color_mapping", "Colouring Mode", 
                          choices = c("By Group" = "group", "One Colour" = "single"), 
                          inline = TRUE),
             
             conditionalPanel(
                condition = "input.color_mapping == 'group'",
                checkboxInput("use_palette", "Use Color Palette", value = TRUE),
                conditionalPanel(
                   condition = "input.use_palette == true",
                   selectInput("color_palette", "Palette", choices = c("Pastel1", "Set1", "Set2", "Dark2", "Paired"))
                ),
                conditionalPanel(
                   condition = "input.use_palette == false",
                   helpText("Choose a color for each group:"),
                   uiOutput("group_color_pickers")
                ),
             ),
             conditionalPanel(
                condition = "input.color_mapping == 'single'",
                colourInput("solid_color", "Solid Colour", value = "#5DA5DA")
             ),   
             hr(),
             h6("Text"),
             # Font Settings
             numericInput("font_size", "Font Size (pt)", value = 15, min = 10, step = 1),
             selectInput("font_family", "Font Type", choices = c("Arial/Helvetica (Sans)"="sans", "Times/Georgia (Serif)"="serif", "Courier/Mono (Mono)"="mono")),
             selectInput("font_style", "Font Style", choices = c("Plain"="plain", "Bold"="bold", "Italic"="italic", "Bold Italic"="bold.italic")),
             
             hr(),
             h6("Elements"),
             numericInput("point_size", "Point Size", value = 2, min = 0.1, step = 0.5),
             numericInput("stroke_width", "Stroke Width", value = 0.5, min = 0.1, step = 0.1),
             
             div(style="margin-top: 15px;",
                 actionButton("reset_style", "Reset Appearance", icon = icon("rotate-left"), class = "btn-warning btn-sm", width = "100%")
             )
          ),
          accordion_panel(
             "Advanced",
             actionButton("sidebar_reorder_btn", "Reorder Groups", icon = icon("sort"), width = "100%", class = "btn-secondary"), # Unified reorder logic
             helpText("Note: This changes the underlying data factor levels."),
             
             hr(),
             h6("Plot Override"),
             selectInput("force_plot_type", "Plot Type Override", choices = c("Auto (Based on Normality)" = "auto", "Bar Chart (Mean ± SD)" = "bar", "Boxplot (Median + IQR)" = "box")),
             
             hr(),
             h6("Significance Analysis"),
             checkboxInput("show_signif", "Show Significance Lines", value = TRUE),
             conditionalPanel(
               condition = "input.show_signif == true",
               radioButtons("signif_format", "Significance Format",
                           choices = c("P-Value" = "p.value", "Asterisk (*)" = "stars"),
                           inline = TRUE),
                numericInput("signif_font_size", "Significance Font Size", value = 5, min = 1, step = 0.5),
                numericInput("signif_tip_length", "Tip Length", value = 0.05, min = 0, step = 0.01)
             ),
             
             hr(),
             h6("Dimensions"),
             checkboxInput("use_fixed_size", "Enable Fixed Dimensions", value = FALSE),
             conditionalPanel(
                condition = "input.use_fixed_size == true",
                numericInput("plot_width", "Width (px)", value = 800, min = 400, step = 50),
                numericInput("plot_height", "Height (px)", value = 600, min = 300, step = 50),
                numericInput("export_dpi", "Export DPI", value = 300, min = 72, step = 10)
             )
          )
        ),
        hr(),
        h5("Export Plot"),
        div(style="display: flex; gap: 5px; flex-wrap: wrap;",
            downloadButton("dl_pdf", "PDF", class="btn-sm"),
            downloadButton("dl_svg", "SVG", class="btn-sm"),
            downloadButton("dl_jpeg", "JPEG", class="btn-sm")
        )
      ),
      
      # --- TABLE CONTROLS ---
      conditionalPanel(
        condition = "input.viz_view === 'Result Table'",
        h5("Table Options"),
        textInput("table_title", "Table Title", placeholder = "Auto"),
        checkboxInput("show_table_title", "Show Table Title", value = TRUE),
        checkboxInput("show_footnote_table", "Show Test Name (Footnote)", value = TRUE),
        checkboxInput("show_shapiro", "Show Normality (Separate Table)", value = FALSE),
        checkboxInput("show_pairwise", "Show Pairwise Comparisons", value = TRUE),
        
        accordion(
           open = FALSE,
           accordion_panel(
              "Advanced",
              numericInput("decimal_places", "Decimal Places", value = 2, min = 0, max = 5),
              helpText("Reorder groups via the 'Reorder Groups' button in visualization settings or Data Editor.")
           )
        ),
        
        hr(),
        h5("Export Table"),
        helpText("Tip: Add .doc in file name to save as Microsoft Word file.", style = "font-size: 0.8em; color: #666;"),
        div(style="display: flex; gap: 5px; flex-wrap: wrap;",
            downloadButton("dl_table_word", "HTML/Word", class="btn-sm")
        )
      ),

      # --- DATASET VIEWER PLACEHOLDER ---
      conditionalPanel(
        condition = "input.viz_view === 'Dataset Viewer'",
        helpText("No specific settings available for the Dataset Viewer.")
      )
    ),
    
    # --- MAIN CONTENT ---
    layout_columns(
      col_widths = c(12, 12, 12),
      
      # 1. Dynamic Warning Banner (Hidden by default)
      uiOutput("normality_warning"),
      
      # 2. The Smart Plot
      navset_card_tab(
        id = "viz_view",
        full_screen = TRUE,
        nav_panel("Visualization", 
          uiOutput("plot_container")
        ),
        nav_panel("Result Table", 
           div(style = "min-height: 30vh;",
               uiOutput("table_container")
           )
        ),
        nav_panel("Data Editor", 
           div(class = "container-fluid", style = "padding-top: 10px;",
               div(class = "d-flex justify-content-between align-items-center mb-2",
                   div(
                       h5("Edit Dataset", style="margin-bottom: 0; display: inline-block; margin-right: 10px;"),
                       span("(Double-click cells to edit)", style="font-size: 0.9em; color: #666;")
                   ),
                   div(class = "d-flex gap-2 align-items-center",
                       # Edit Controls
                       div(class = "btn-group btn-group-sm", role = "group",
                           actionButton("add_row_btn", "Row", icon = icon("plus"), class = "btn-secondary btn-sm"),
                           actionButton("add_col_btn", "Col", icon = icon("plus"), class = "btn-secondary btn-sm")
                       ),
                       div(class = "btn-group btn-group-sm", role = "group",
                           actionButton("rename_col_btn", "Col", icon = icon("pen"), class = "btn-secondary btn-sm"),
                           actionButton("reorder_col_btn", "Cols", icon = icon("sliders"), class = "btn-secondary btn-sm"),
                           actionButton("reorder_levels_btn", "Group", icon = icon("sort"), class = "btn-primary btn-sm"),
                           actionButton("delete_row_btn", "Row", icon = icon("trash"), class = "btn-danger btn-sm")
                       ),
                       div(class = "btn-group btn-group-sm", role = "group",
                           downloadButton("download_csv", "CSV", class = "btn-info btn-sm")
                       )
                   )
               ),
               div(style = "min-height: 24vh;",
                   uiOutput("data_table_ui") # Dynamic container for re-render
               )
           )
        ),
        nav_spacer(),
        nav_item(
           uiOutput("export_warning")
        )
      ),
      
      # 3. The Methodology & Interpretation
      card(
        card_header("Statistical Interpretation"),
        verbatimTextOutput("methodology"), # Monospace font for technical details
        textOutput("interpretation")       # Regular font for plain English
      )
    )
  )
)

# ==============================================================================
# 2. THE BACKEND (SERVER)
# ==============================================================================
server <- function(input, output, session) {
  
  # A. Load Data & Update Dropdowns
  # Store the last selected group variable to manage resets
  last_group_var <- reactiveVal(NULL)

  # Store data in reactiveValues to support multiple sources (Upload vs Demo)
  vals <- reactiveValues(data = NULL)

  # 1. Load Data from File Upload
  observeEvent(input$file, {
    vals$data <- read.csv(input$file$datapath, stringsAsFactors = TRUE, check.names = FALSE)
    vals$editing_data <- vals$data
    vals$dt_trigger <- vals$dt_trigger + 1
  })

  # 2. Generate Dummy Data (On Click)
  observeEvent(input$demo_data, {
    set.seed(123) # Reproducible
    n <- 100
    
    # Create robust dummy data
    # Scenario: Clinical trial with 3 groups, pre-post scores, and demographics
    df <- data.frame(
      ID = 1:n,
      Group = sample(c("Control", "Treatment A", "Treatment B"), n, replace = TRUE),
      Gender = sample(c("Male", "Female"), n, replace = TRUE),
      Age = round(rnorm(n, mean = 45, sd = 10)),
      Score_Pre = round(rnorm(n, mean = 60, sd = 15))
    )
    
    # Create Post scores with some effect logic
    # Control: No change, Treatment A: +5, Treatment B: +10
    df$Score_Post <- df$Score_Pre + rnorm(n, mean = 0, sd = 5)
    df$Score_Post[df$Group == "Treatment A"] <- df$Score_Post[df$Group == "Treatment A"] + 5
    df$Score_Post[df$Group == "Treatment B"] <- df$Score_Post[df$Group == "Treatment B"] + 10
    
    # Round Post scores
    df$Score_Post <- round(df$Score_Post)
    
    # Binary Outcome (correlated with Post score)
    # Higher score -> Higher chance of "Cured"
    probs <- plogis(0.1 * (df$Score_Post - 65))
    df$Outcome <- ifelse(runif(n) < probs, "Cured", "Not Cured")
    
    # Convert Strings to Factors (Crucial for App detection)
    df$Group <- as.factor(df$Group)
    df$Gender <- as.factor(df$Gender)
    df$Outcome <- as.factor(df$Outcome)
    
    vals$data <- df
    vals$editing_data <- df
    vals$dt_trigger <- vals$dt_trigger + 1
    
    # Notification
    showNotification("Dummy dataset loaded! Select your variables and hit run!", type = "message")
  })

  data <- reactive({
    req(vals$data)
    vals$data
  })
  
  observeEvent(data(), {
    # Detect numeric vs categorical columns to help the user
    # logic: "Categorical" includes Factors, Characters, and Numerics with few unique levels (e.g. <= 20)
    # This acts as a "Safe Filter" to prevent users from accidentally selecting continuous vars (like ID or Age) as groups.
    
    df <- data()
    all_cols <- names(df)
    
    is_safe_cat <- function(x) {
       if(is.factor(x) || is.character(x)) return(TRUE)
       # For numerics, check distinct count
       if(is.numeric(x)) {
          n_uniq <- length(unique(na.omit(x)))
          return(n_uniq <= 10) # Threshold for "Categorical-like" numeric
       }
       return(FALSE)
    }
    
    nums <- all_cols[sapply(df, is.numeric)]
    cats <- all_cols[sapply(df, is_safe_cat)]
    
    # Auto-fill dropdowns
    updateSelectInput(session, "outcome_var", choices = all_cols, selected = nums[1])
    updateSelectInput(session, "group_var", choices = cats, selected = cats[1])
    updateSelectInput(session, "paired_vars", choices = all_cols)
    updateSelectInput(session, "corr_var1", choices = nums, selected = nums[1])
    updateSelectInput(session, "corr_var2", choices = nums, selected = if(length(nums)>1) nums[2] else nums[1])
    
    # Update Split Variable Dropdown (Restrict to safe categories too)
    updateSelectInput(session, "split_var", choices = cats)
  })

  # --- DYNAMIC FILTER UPDATES ---
  observeEvent(data(), {
     req(data())
     updateSelectInput(session, "filter_var", choices = names(data()))
  })

  # Render the Filter UI based on variable type
  output$filter_ui <- renderUI({
     req(input$filter_var, data())
     df <- data()
     if(input$filter_var %in% names(df)) {
        col_data <- df[[input$filter_var]]
        
        # Categorical / Factor
        if(is.factor(col_data) || is.character(col_data)) {
           vals <- if(is.factor(col_data)) levels(col_data) else sort(unique(col_data))
           return(selectizeInput("filter_val", "Select Value(s):", choices = vals, multiple = TRUE))
        } 
        
        # Numeric
        if(is.numeric(col_data)) {
           tagList(
              selectInput("filter_op", "Operator:", 
                          choices = c("One of (Exact)" = "in", 
                                      "Greater Than (>)" = "gt", 
                                      "Less Than (<)" = "lt",
                                      "Greater or Equal (>=)" = "ge",
                                      "Less or Equal (<=)" = "le",
                                      "Equal To (=)" = "eq",
                                      "Not Equal To (!=)" = "ne")),
              uiOutput("filter_val_ui")
           )
        }
     }
  })
  
  # Render the specific value input based on Operator
  output$filter_val_ui <- renderUI({
     req(input$filter_var, data())
     # Default to "in" if not yet initialized
     op <- if(is.null(input$filter_op)) "in" else input$filter_op
     
     df <- data()
     req(input$filter_var %in% names(df))
     col_data <- df[[input$filter_var]]
     
     if(op == "in") {
        # Discrete selection for numeric
        vals <- sort(unique(col_data))
        selectizeInput("filter_val", "Select Value(s):", choices = vals, multiple = TRUE)
     } else {
        # Continuous input
        # Set default value to median or min?
        def_val <- median(col_data, na.rm=TRUE)
        numericInput("filter_val_num", "Value:", value = def_val)
     }
  })
  
  # Dynamic Analysis Mode Choices
  observeEvent(input$analysis_complexity, {
      if(input$analysis_complexity == "Advanced") {
         updateRadioButtons(session, "analysis_type", 
            choices = c("Independent Groups" = "Independent", 
                        "Paired Groups" = "Paired",
                        "Correlation Analysis" = "Correlation"),
            selected = input$analysis_type # Keep current selection if valid
         )
      } else {
         # Basic Mode: Check if we need to reset from Correlation
         new_sel <- input$analysis_type
         if(is.null(new_sel) || new_sel == "Correlation") new_sel <- "Independent"
         
         updateRadioButtons(session, "analysis_type", 
            choices = c("Independent Groups" = "Independent", 
                        "Paired Groups" = "Paired"),
            selected = new_sel
         )
         
         # Reset Advanced Options
         updateCheckboxInput(session, "enable_split", value = FALSE)
         updateCheckboxInput(session, "enable_filter", value = FALSE)
      }
  })

  # --- DYNAMIC GROUP ORDER UPDATES ---
  observeEvent(list(input$analysis_type, input$group_var, input$paired_vars, data()), {
     req(data(), input$analysis_type)
     df <- data()
     
     # Determine which variable is the "Group" based on analysis mode
     grp_choices <- NULL
     
     if(input$analysis_type == "Independent") {
        req(input$group_var)
        if(isTruthy(input$group_var) && input$group_var %in% names(df)) {
           x <- as.factor(df[[input$group_var]])
           grp_choices <- levels(x)
        }
     } else if (input$analysis_type == "Paired") {
        # For Paired, 'Groups' are usually the timepoints/conditions (Columns)
        # Check if we are doing Paired 2-group or multi-group
        req(input$paired_vars)
        if(length(input$paired_vars) >= 2) {
           grp_choices <- input$paired_vars
        }
     }
     
     # Update Inputs if choices exist
     # FORCE update the selection to "All" whenever the configuration changes.
     if(!is.null(grp_choices)) {
        # updateSelectInput(session, "viz_group_order", choices = grp_choices, selected = grp_choices)
        # updateSelectInput(session, "tbl_group_order", choices = grp_choices, selected = grp_choices)
     }
  })

  # Render Dataset Viewer
  # --- C. DATA EDITOR ENGINE ---
  
  # 1. Initialize Editing Data
  observe({
    req(vals$data)
    if(is.null(vals$editing_data)) {
       vals$editing_data <- vals$data
    }
  })
  
  # Sync Logic: When editing_data changes -> update vals$data (The Analysis Source)
  observe({
     req(vals$editing_data)
     vals$data <- vals$editing_data
  })
  
  # 2. Render DT
  vals$dt_trigger <- 0 # Trigger to force re-render
  
  output$data_table_ui <- renderUI({
    # Dependency on trigger to force rebuild
    val <- vals$dt_trigger
    DTOutput("data_table")
  })
  
  output$data_table <- renderDT({
    req(vals$editing_data)
    
    isolate({
      datatable(vals$editing_data, 
                editable = TRUE, 
                extensions = c("Buttons", "Scroller", "KeyTable"),
                options = list(
                  dom = 'frtip',
                  # buttons = list('copy', 'csv'), # Removed as per v1.2.0 request
                  keys = TRUE,
                  scrollX = TRUE,
                  scrollY = 500,
                  scroller = TRUE,
                  pageLength = 20 # Show more by default
                ),
                selection = "multiple", # Allow row selection
                callback = JS("
                    table.on('key', function(e, datatable, key, cell, originalEvent){
                      var targetName = originalEvent.target.localName;
                      if(key == 13 && targetName == 'body'){
                        $(cell.node()).trigger('dblclick.dt');
                      }
                    });
                    // Global listener for input field to blur on Enter
                    $(document).on('keydown', '.dataTable input', function(e) {
                      if(e.which === 13) {
                         e.preventDefault();
                         $(this).blur();
                      }
                    });
                 ")
      )
    })
  })
  
  proxy_dt <- dataTableProxy("data_table")
  
  # 3. Handle Cell Edits
  observeEvent(input$data_table_cell_edit, {
    info <- input$data_table_cell_edit
    i <- info$row
    j <- info$col 
    v <- info$value
    
    current_val <- vals$editing_data[i, j]
    
    # Type Coercion Safety
    new_val <- tryCatch({
      DT::coerceValue(v, current_val)
    }, error = function(e) v)
    
    # Factor Handling: Add level if new
    if(is.factor(current_val)) {
      if(!(new_val %in% levels(vals$editing_data[[j]]))) {
        # If strict factor, maybe convert to char? Or expand levels.
        # Let's convert column to character to allow flexibility, then re-factor if needed.
        # OR: Just add the level.
        levels(vals$editing_data[[j]]) <- c(levels(vals$editing_data[[j]]), new_val)
      }
    }
    
    vals$editing_data[i, j] <- new_val
  })
  
  # 4. Structure Edits
  
  # Add Row
  observeEvent(input$add_row_btn, {
    req(vals$editing_data)
    df <- vals$editing_data
    new_row <- df[1, ]; is.na(new_row) <- TRUE # Empty row with same types
    
    # Fix factors in new row
    for(k in 1:ncol(new_row)) {
        if(is.factor(new_row[[k]])) new_row[[k]] <- levels(new_row[[k]])[1]
        if(is.numeric(new_row[[k]])) new_row[[k]] <- 0
    }
    
    vals$editing_data <- rbind(df, new_row)
    vals$dt_trigger <- vals$dt_trigger + 1
    showNotification("Row added at bottom.", type = "message")
  })
  
  # Add Column
  observeEvent(input$add_col_btn, {
    showModal(modalDialog(
      title = "Add New Column",
      textInput("new_col_name", "Column Name", placeholder = "NewVar"),
      selectInput("new_col_type", "Type", choices = c("Numeric", "Text")),
      footer = tagList(modalButton("Cancel"), actionButton("confirm_add_col", "Add", class = "btn-primary"))
    ))
  })
  
  observeEvent(input$confirm_add_col, {
    req(input$new_col_name)
    removeModal()
    if(input$new_col_name %in% names(vals$editing_data)) { showNotification("Name exists.", type="error"); return() }
    
    df <- vals$editing_data
    n <- nrow(df)
    new_col <- if(input$new_col_type == "Numeric") rep(0, n) else rep("", n)
    df[[input$new_col_name]] <- new_col
    vals$editing_data <- df
    vals$dt_trigger <- vals$dt_trigger + 1
  })
  
  # Delete Row
  observeEvent(input$delete_row_btn, {
    req(vals$editing_data)
    sel <- input$data_table_rows_selected
    if(length(sel) == 0) { showNotification("Select rows to delete.", type="warning"); return() }
    
    vals$editing_data <- vals$editing_data[-sel, , drop = FALSE]
    vals$dt_trigger <- vals$dt_trigger + 1
    showNotification("Rows deleted.", type = "message")
  })
  
  # CSV Download
  output$download_csv <- downloadHandler(
    filename = function() { paste0("edited_data_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(vals$editing_data, file, row.names = FALSE) }
  )
  
  # Rename Column
  observeEvent(input$rename_col_btn, {
     req(vals$editing_data)
     showModal(modalDialog(
        title = "Rename Column",
        selectInput("rename_target", "Select Column", choices = names(vals$editing_data)),
        textInput("rename_new_name", "New Name"),
        footer = tagList(modalButton("Cancel"), actionButton("confirm_rename", "Rename", class="btn-primary"))
     ))
  })
  observeEvent(input$confirm_rename, {
     req(input$rename_new_name)
     removeModal()
     old <- input$rename_target; new <- input$rename_new_name
     if(new %in% names(vals$editing_data)) { showNotification("Name exists.", type="error"); return() }
     
     df <- vals$editing_data
     names(df)[names(df) == old] <- new
     vals$editing_data <- df
     vals$dt_trigger <- vals$dt_trigger + 1
  })
  
  # Reorder Columns
   observeEvent(input$reorder_col_btn, {
    req(vals$editing_data)
    cols <- names(vals$editing_data)
    showModal(modalDialog(
      title = "Reorder Columns",
      selectizeInput("reorder_cols_list", "Drag to Reorder", choices = cols, selected = cols, multiple = TRUE, options = list(plugins = list('drag_drop'))),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reorder_col_struct", "Confirm", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$confirm_reorder_col_struct, {
     req(input$reorder_cols_list)
     removeModal()
     if(length(input$reorder_cols_list) == 0) return()
     vals$editing_data <- vals$editing_data[, input$reorder_cols_list, drop=FALSE]
     vals$dt_trigger <- vals$dt_trigger + 1
  })

  # --- D. ROBUST GROUP ORDERING (Global) ---
  
  # Helper to identify Categorical Variables
  get_cats <- reactive({
     req(vals$editing_data)
     df <- vals$editing_data
     names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
  })
  
  # Unified Trigger: Sidebar OR Editor Button
  observeEvent(list(input$reorder_levels_btn, input$sidebar_reorder_btn), {
     req(vals$editing_data)
     cats <- get_cats()
     if(length(cats) == 0) { showNotification("No categorical variables found.", type="warning"); return() }
     
     showModal(modalDialog(
        title = "Select Variable to Reorder",
        selectInput("reorder_target_var", "Variable", choices = cats),
        footer = tagList(modalButton("Cancel"), actionButton("goto_reorder_step2", "Next", class="btn-primary"))
     ))
  }, ignoreInit = TRUE)
  
  observeEvent(input$goto_reorder_step2, {
     req(input$reorder_target_var)
     removeModal()
     target <- input$reorder_target_var
     
     # Get unique levels
     col_data <- vals$editing_data[[target]]
     lvls <- if(is.factor(col_data)) levels(col_data) else unique(as.character(col_data))
     
     showModal(modalDialog(
        title = paste("Reorder Levels:", target),
        helpText("Drag and drop to set the desired order. Top item = First Group."),
        selectizeInput("new_level_order", NULL, choices = lvls, selected = lvls, multiple = TRUE, options = list(plugins = list('drag_drop'))),
        footer = tagList(modalButton("Cancel"), actionButton("confirm_level_reorder", "Apply", class="btn-success"))
     ))
  })
  
  observeEvent(input$confirm_level_reorder, {
     req(input$new_level_order)
     removeModal()
     
     target <- input$reorder_target_var
     new_ord <- input$new_level_order
     
     # Apply to Global Data
     df <- vals$editing_data
     # Check consistency
     current_items <- if(is.factor(df[[target]])) levels(df[[target]]) else unique(as.character(df[[target]]))
     # Ensure we have all items (user might accidentally delete one in selectize)
     if(!all(current_items %in% new_ord)) {
        showNotification("Error: You dropped some levels. Please include all.", type="error")
        return()
     }
     
     # Refactor
     df[[target]] <- factor(df[[target]], levels = new_ord)
     vals$editing_data <- df
     vals$dt_trigger <- vals$dt_trigger + 1 # Force redraw of table
     
     showNotification(paste("Updated order for", target), type = "message")
  })

  # B. Smart Analysis Engine
  results <- eventReactive(input$analyze, {
    # 1. INITIAL SETUP & TYPE OVERRIDES
    req(input$analysis_type)
    df_raw <- data()
    
    # [NEW] Filter Logic (Applies before analysis)
    if(isTRUE(input$enable_filter) && !is.null(input$filter_var)) {
       req(input$filter_var %in% names(df_raw))
       
       f_var <- df_raw[[input$filter_var]]
       
       # Determine Filter Type
       # If filter_op is present, we use it. If not (Categorical), we assume "in".
       op <- if(!is.null(input$filter_op)) input$filter_op else "in"
       
       keep_rows <- TRUE # Default
       
       if(op == "in") {
          req(input$filter_val)
          keep_rows <- f_var %in% input$filter_val
       } else {
          req(input$filter_val_num)
          val <- input$filter_val_num
          if(op == "gt") keep_rows <- f_var > val
          if(op == "lt") keep_rows <- f_var < val
          if(op == "ge") keep_rows <- f_var >= val
          if(op == "le") keep_rows <- f_var <= val
          if(op == "eq") keep_rows <- f_var == val # Exact numeric match
          if(op == "ne") keep_rows <- f_var != val
       }
       
       # Apply Filter
       keep_rows[is.na(keep_rows)] <- FALSE
       df_raw <- df_raw[keep_rows, , drop = FALSE]
       
       if(nrow(df_raw) == 0) {
          showNotification("Filter results in empty dataset. Check your selection.", type = "error")
          return(list(test="Error", note="No data after filtering.")) 
       } else {
          showNotification(paste("Filtered dataset: N =", nrow(df_raw)), type = "message")
       }
       
       # Optional: Drop unused levels to clean up plots/tables
       if(input$analysis_complexity == "Advanced") {
          df_raw <- droplevels(df_raw)
       }
    }
    
    vals$filtered_data <- df_raw
    
    # helper to safely retrieve inputs
    a_type <- input$analysis_type
    p_outcome <- input$outcome_var
    p_group <- input$group_var
    p_paired <- input$paired_vars
    p_corr1 <- input$corr_var1
    p_corr2 <- input$corr_var2
    
    # Apply Variable Type Overrides
    df <- df_raw
    if(input$force_outcome_type != "Auto" && !is.null(p_outcome) && p_outcome %in% names(df)) {
       if(input$force_outcome_type == "Numeric") {
          df[[p_outcome]] <- as.numeric(as.character(df[[p_outcome]]))
       } else if(input$force_outcome_type == "Categorical") {
          df[[p_outcome]] <- as.factor(df[[p_outcome]])
       }
    }
    if(input$force_group_type != "Auto" && !is.null(p_group) && p_group %in% names(df)) {
       if(input$force_group_type == "Categorical") {
          df[[p_group]] <- as.factor(df[[p_group]])
       }
    }
    
    # -------------------------------------------------------------------------
    # FUNCTION: CORE ANALYSIS (Independent / Paired)
    # -------------------------------------------------------------------------
    do_core_analysis <- function(sub_df, analysis_mode, out_var, grp_var, pair_vars) {
       res <- list()
       # Snapshot params
       res$params <- list(type = analysis_mode, outcome = out_var, group = grp_var, paired_vars = pair_vars)
       
       if(analysis_mode == "Independent") {
          req(out_var, grp_var)
          y <- sub_df[[out_var]]
          x <- as.factor(sub_df[[grp_var]])
          x <- droplevels(x) # Clean unused levels
          groups <- levels(x)
          res$pairwise_text <- NULL
          
          # Branch 1: Categorical Outcome
          if(is.factor(y) || is.character(y)) {
             y <- as.factor(y); y <- droplevels(y)
             res$mode <- "categorical"
             tbl <- table(y, x)
             res$props <- prop.table(tbl, 2) * 100
             
             # Exact 2x2 OR (SPSS Match: Sample Odds Ratio)
             res$or_data <- NULL
             if(nrow(tbl) == 2 && ncol(tbl) == 2) {
                try({
                   # Sample Odds Ratio ((a*d)/(b*c))
                   a <- tbl[1,1]; b <- tbl[1,2]
                   c <- tbl[2,1]; d <- tbl[2,2]
                   
                   # Handle potential division by zero
                   if(b == 0 || c == 0) {
                   # Use raw calculation; allow Inf for division by zero.
                      or_val <- (a*d) / ((b + 1e-9) * (c + 1e-9)) 
                   } else {
                      or_val <- (a * d) / (b * c)
                   }
                   
                   # Wald 95% CI
                   # SE(ln(OR)) = sqrt(1/a + 1/b + 1/c + 1/d)
                   # Only valid if no zeros. If zeros, standard adjustments needed.
                   if(a>0 && b>0 && c>0 && d>0) {
                      se_log_or <- sqrt(1/a + 1/b + 1/c + 1/d)
                      log_or <- log(or_val)
                      ci_lo <- exp(log_or - 1.96 * se_log_or)
                      ci_hi <- exp(log_or + 1.96 * se_log_or)
                   } else {
                      ci_lo <- NA; ci_hi <- NA # SPSS often omits
                   }
                   
                   res$or_data <- list(or_val = or_val, ci_lo = ci_lo, ci_hi = ci_hi)
                }, silent=TRUE)
             }
             
             # Test Selection
             row_totals <- margin.table(tbl, 1); col_totals <- margin.table(tbl, 2); n <- sum(tbl)
             expected <- outer(row_totals, col_totals) / n
             percent_small <- (sum(expected < 5) / length(expected)) * 100
             
             res$is_normal <- NA
             
             if(nrow(tbl) < 2) {
                # Constant Outcome Case
                res$p <- 1.000
                res$test <- "Pearson Chi-Square Test"
                res$stat <- "N/A (Constant)"
                res$note <- "Outcome variable is constant across all groups. No difference in proportion exists."
             } else if(percent_small > 20) {
                tryCatch({
                   test <- fisher.test(tbl, workspace = 2e8, alternative = "two.sided")
                   res$test <- "Fisher's Exact Test"
                   res$p <- test$p.value
                   res$stat <- "N/A (Exact)"
                   res$note <- glue("{round(percent_small, 1)}% of cells have expected count < 5. Using Fisher's Exact Test.")
                }, error = function(e) {
                   test <- chisq.test(tbl, simulate.p.value = TRUE)
                   res$test <- "Chi-Square (Monte Carlo)"
                   res$p <- test$p.value
                   res$stat <- paste("X-sq =", round(test$statistic, 2))
                   res$note <- "Fisher's test failed. Used Chi-square with MC simulation."
                })
             } else {
                # SPSS Match: No continuity correction
                test <- chisq.test(tbl, correct = FALSE)
                res$test <- "Pearson Chi-Square Test"
                res$p <- test$p.value
                res$stat <- paste("X-sq =", round(test$statistic, 2))
                res$note <- glue("{round(percent_small, 1)}% of cells have expected count < 5. Assumption met.")
             }
             
          } else {
             # Branch 2: Numeric Outcome
             
             # Filter out NAs in outcome and drop empty groups to allow correct test selection (e.g. 2 vs >2)
             valid_idx <- !is.na(y)
             y <- y[valid_idx]
             x <- x[valid_idx]
             x <- droplevels(x)
             groups <- levels(x)

             # Check normality PER GROUP instead of on pooled data
             is_normal <- TRUE
             for(g in groups) {
                # Subset data for this group
                sub_data <- y[x == g]
                sub_data <- na.omit(sub_data)
                
                # Shapiro-Wilk needs 3-5000 observations
                if(length(sub_data) >= 3 && length(sub_data) <= 5000) {
                   sw_p <- tryCatch(shapiro.test(sub_data)$p.value, error = function(e) 0)
                   if(sw_p < 0.05) {
                      is_normal <- FALSE
                      break # One non-normal group is enough to switch to non-parametric (conservative)
                   }
                }
             }
             res$is_normal <- is_normal
             
             means <- tapply(y, x, mean, na.rm=TRUE)
             sds <- tapply(y, x, sd, na.rm=TRUE)
             medians <- tapply(y, x, median, na.rm=TRUE)
             q1 <- tapply(y, x, quantile, probs=0.25, na.rm=TRUE)
             q3 <- tapply(y, x, quantile, probs=0.75, na.rm=TRUE)
             res$desc_param <- list(means=means, sds=sds)
             res$desc_nonparam <- list(medians=medians, q1=q1, q3=q3)
             res$high_group <- names(which.max(means))
             
             # Levene Test (Only if Normal)
             res$levene_note <- ""
             equal_var <- FALSE 
             
             if(is_normal && length(groups) >= 2) {
                 levene_p <- tryCatch({
                    df_c <- data.frame(y=y, x=x); df_c <- na.omit(df_c)
                    df_c$z <- abs(df_c$y - ave(df_c$y, df_c$x, FUN=mean))
                    anova(lm(z ~ x, data=df_c))$`Pr(>F)`[1]
                 }, error=function(e) NA)
                 
                 equal_var <- if(!is.na(levene_p) && levene_p > 0.05) TRUE else FALSE
                 res$levene_note <- ""
                 if(!is.na(levene_p)) {
                    p_disp <- if(levene_p < 0.001) "< 0.001" else paste("=", formatC(levene_p, format='f', digits=3))
                    res$levene_note <- glue(", equal variance {if(equal_var) 'assumed' else 'not assumed'} (Levene's Test P {p_disp})")
                 }
             }
             
             if(length(groups) == 2) {
                res$mode <- "2group"
                # Removed early return on zero variance to force run analysis
                
                if(is_normal) {
                   test <- t.test(y ~ x, var.equal = equal_var, alternative = "two.sided")
                   res$test <- if(equal_var) "Independent T-Test" else "Welch T-Test"
                   res$p <- test$p.value
                   res$stat <- paste("t =", round(test$statistic, 2))
                   res$note <- "Data is normally distributed. A parametric test was used."
                   res$ttest_data <- list(diff = test$estimate[1] - test$estimate[2], ci_lo = test$conf.int[1], ci_hi = test$conf.int[2])
                } else {
                   test <- suppressWarnings(wilcox.test(y ~ x, alternative = "two.sided", correct = FALSE))
                   res$test <- "Mann-Whitney U Test"
                   res$p <- test$p.value
                   res$stat <- paste("W =", round(test$statistic, 2))
                   res$note <- "Data is not normally distributed. A non-parametric test was used."
                }
                
                res$pairwise_table <- NULL
                # For plotting: Create a specific data frame for 2-group significance
                res$plot_signif_data <- data.frame(
                   "Group 1" = groups[1],
                   "Group 2" = groups[2],
                   "Adjusted P-value" = res$p,
                   check.names = FALSE
                )
                
             } else if(length(groups) > 2) {
                res$mode <- "multigroup"
                if(is_normal) {
                   if(equal_var) {
                      test <- aov(y ~ x); s <- summary(test)[[1]]
                      res$test <- "One-Way ANOVA"; res$p <- s$`Pr(>F)`[1]; res$stat <- paste("F =", round(s$`F value`[1], 2))
                   } else {
                      test <- oneway.test(y ~ x, var.equal=FALSE)
                      res$test <- "Welch's ANOVA"; res$p <- test$p.value; res$stat <- paste("F =", round(test$statistic, 2))
                   }
                   res$note <- "Data is normally distributed. A parametric test was used."
                   
                   # Post-hoc
                   if(res$p < 0.05) {
                      ph <- pairwise.t.test(y, x, p.adjust.method="bonferroni", pool.sd=equal_var, alternative = "two.sided")
                      pw_df <- as.data.frame(as.table(ph$p.value))
                      pw_df <- na.omit(pw_df)
                      colnames(pw_df) <- c("Group 1", "Group 2", "Adjusted P-value")
                      res$pairwise_table <- pw_df
                      res$plot_signif_data <- pw_df # Use same data for plotting
                      
                      # Text Gen
                      sig <- pw_df[pw_df$"Adjusted P-value" < 0.05,]
                      if(nrow(sig) > 0) res$pairwise_text <- paste(nrow(sig), "pair(s) show significant difference after Bonferroni adjustment.")
                      
                      # Format P
                      res$pairwise_table[[3]] <- sapply(res$pairwise_table[[3]], function(p) if(p<0.001)"< 0.001" else formatC(p, format="f", digits=3))
                      res$posthoc_test <- "Pairwise T-tests (Bonferroni)"
                   }
                } else {
                   test <- kruskal.test(y ~ x)
                   res$test <- "Kruskal-Wallis Test"; res$p <- test$p.value; res$stat <- paste("Chi-sq =", round(test$statistic, 2))
                   res$note <- "Data is not normally distributed. A non-parametric test was used."
                   if(res$p < 0.05) {
                      # Capture output to silence the "Reject Ho" message
                      capture.output(
                        dt <- dunn.test::dunn.test(y, x, method="bonferroni", kw=FALSE, table=FALSE)
                      )
                      
                      # Dunn test returns "LevelA - LevelB" in 'comparisons'
                      # We need to split this into two columns for the plotter
                      comps <- strsplit(as.character(dt$comparisons), " - ")
                      
                      g1 <- sapply(comps, `[`, 1)
                      g2 <- sapply(comps, `[`, 2)
                      
                      # RECALCULATE P-VALUES TO ENSURE 2-SIDED CONSISTENCY WITH SPSS
                      # dunn.test can return 1-sided p-values by default in some versions.
                      # We manually calculate 2-sided p-values from Z to guarantee a match.
                      p_raw <- 2 * (1 - pnorm(abs(dt$Z)))
                      p_adj <- p.adjust(p_raw, method="bonferroni")
                      
                      res$pairwise_table <- data.frame(
                         "Group 1" = g1,
                         "Group 2" = g2,
                         "Z" = dt$Z, 
                         "Adjusted P-value" = p_adj, 
                         check.names=FALSE
                      )
                      res$plot_signif_data <- res$pairwise_table
                      res$pairwise_table[[4]] <- sapply(res$pairwise_table[[4]], function(p) if(p<0.001)"< 0.001" else formatC(p, format="f", digits=3))
                      res$pairwise_text <- "Post-hoc Dunn's test performed."
                      res$posthoc_test <- "Dunn's Test (Bonferroni)"
                   }
                }
             } else { res$test <- "Error"; res$note <- "Too few groups (0 or 1)." }
          }
          
       } else if(analysis_mode == "Paired") {
          req(pair_vars)
          if(length(pair_vars) < 2) return(list(test="Error", note="Need >= 2 vars"))
          sub_df <- sub_df[, pair_vars]; sub_df <- na.omit(sub_df)
          n_cols <- ncol(sub_df)
          
          if(!is.numeric(sub_df[[1]])) {
             # Paired Categorical
             if(n_cols != 2) return(list(test="Error", note="McNemar supports 2 vars only"))
             res$mode <- "paired_categorical"
             v1 <- factor(sub_df[[1]]); v2 <- factor(sub_df[[2]], levels=levels(v1))
             tbl <- table(v1, v2)
             res$props <- tbl
             tryCatch({
                m <- mcnemar.test(tbl)
                res$test <- "McNemar's Test"
                res$p <- m$p.value; res$stat <- paste("Chi-sq =", round(m$statistic, 2))
                
                # Kappa
                N <- sum(tbl); Po <- sum(diag(tbl))/N
                Pe <- sum(margin.table(tbl,1)*margin.table(tbl,2))/(N^2)
                res$kappa <- (Po-Pe)/(1-Pe)
             }, error=function(e) { res$test <- "Error (McNemar)"; res$note <- "Sparse data." })
             
          } else {
             # Paired Numeric
             res$desc_param <- list(means = colMeans(sub_df))
             res$desc_nonparam <- list(medians = apply(sub_df, 2, median))
             res$high_group <- names(which.max(res$desc_param$means))
             
             if(n_cols == 2) {
                res$mode <- "paired_2group"
                diffs <- sub_df[[1]] - sub_df[[2]]
                is_normal <- TRUE
                if(length(diffs) >= 3 && length(diffs) <= 5000) {
                   sw_p <- tryCatch(shapiro.test(diffs)$p.value, error = function(e) 0)
                   if(sw_p < 0.05) is_normal <- FALSE
                   res$diff_shapiro_p <- sw_p
                }
                res$is_normal <- is_normal
                
                if(is_normal) {
                   test <- t.test(sub_df[[1]], sub_df[[2]], paired=TRUE, alternative = "two.sided")
                   res$test <- "Paired T-Test"; res$p <- test$p.value; res$stat <- paste("t =", round(test$statistic, 2))
                   res$ttest_data <- list(diff = test$estimate, ci_lo = test$conf.int[1], ci_hi = test$conf.int[2]) 
                   res$note <- "Diffs are normal."
                } else {
                   # Fix for floating-point precision issues vs SPSS
                   # Round differences to ensure ties are handled consistently (Safe at 10 decimals)
                   diffs_rounded <- round(sub_df[[1]] - sub_df[[2]], 10)
                   test <- suppressWarnings(wilcox.test(diffs_rounded, alternative = "two.sided", correct = FALSE))
                   res$test <- "Wilcoxon Signed-Rank"; res$p <- test$p.value; res$stat <- paste("V =", round(test$statistic, 2))
                   res$note <- "Diffs not normal."
                }
                
                # Add Plotting Data for Paired 2-Group
                res$plot_signif_data <- data.frame(
                   "Group 1" = names(sub_df)[1],
                   "Group 2" = names(sub_df)[2],
                   "Adjusted P-value" = res$p,
                   check.names = FALSE
                )
             } else {
                res$mode <- "paired_multigroup"
                long_df <- stack(sub_df); names(long_df) <- c("Value", "Group"); long_df$ID <- factor(rep(1:nrow(sub_df), n_cols))
                
                # Check Assumption: Normality of Residuals
                lm_res <- lm(Value ~ Group + ID, data=long_df)
                residuals_vec <- residuals(lm_res)
                is_normal <- TRUE
                res$diff_shapiro_p <- NA
                
                if(length(residuals_vec) >= 3 && length(residuals_vec) <= 5000) {
                   sw_p <- tryCatch(shapiro.test(residuals_vec)$p.value, error = function(e) 0)
                   res$diff_shapiro_p <- sw_p
                   if(sw_p < 0.05) is_normal <- FALSE
                }
                res$is_normal <- is_normal
                
                if(is_normal) {
                   tryCatch({
                      # 1. Run Standard Uncorrected ANOVA first
                      fit <- aov(Value ~ Group + Error(ID/Group), data=long_df)
                      s <- summary(fit)
                      target <- s[[length(s)]][[1]] # Extract Error:Group table
                      
                      F_val <- target$`F value`[1]
                      df1 <- target$Df[1]
                      df2 <- target$Df[2]
                      p_val <- target$`Pr(>F)`[1]
                      
                      # 2. Sphericity Check & Correction (Manual Implementation to match SPSS/standard)
                      # Need Wide Format (Complete Cases only)
                      wide_mat <- as.matrix(na.omit(sub_df)) 
                      k <- ncol(wide_mat)
                      N <- nrow(wide_mat)
                      
                      sphericity_note <- "Sphericity assumed."
                      test_name <- "Repeated Measures ANOVA"
                      
                      if(k >= 3 && N > k) {
                        try({
                           # A. Calculate Covariance Matrix S
                           S <- cov(wide_mat)
                           
                           # B. Mauchly's Test W
                           # Create Orthonormal Contrasts (k x k-1)
                           M <- contr.poly(k)
                           # Transform S to orthonormal contrasts covariance
                           S_orth <- t(M) %*% S %*% M
                           eigen_vals <- eigen(S_orth)$values
                           
                           # W = prod(lambda) / (mean(lambda))^d
                           d <- k - 1
                           W <- prod(eigen_vals) / (mean(eigen_vals)^d)
                           
                           # Chi-Sq Approximation for P-value
                           # df = d(d+1)/2 - 1 = k(k-1)/2 - 1
                           chi_df <- (d * (d + 1) / 2) - 1
                           # Bartlett's approximation
                           chi_stat <- -(N - 1 - (2*d + 5)/6) * log(W)
                           mauchly_p <- pchisq(chi_stat, df = chi_df, lower.tail = FALSE)
                           
                           # C. Greenhouse-Geisser Epsilon
                           # Box (1954) formula on original S
                           mean_diag <- mean(diag(S))
                           mean_grand <- mean(S)
                           row_means <- rowMeans(S)
                           
                           num <- k^2 * (mean_diag - mean_grand)^2
                           den <- (d) * (sum(S^2) - 2*k*sum(row_means^2) + k^2*mean_grand^2)
                           gg_eps <- num / den
                           
                           # Logic: Match SPSS 0.05 cutoff
                           p_disp_mauchly <- if(mauchly_p < 0.001) "< 0.001" else paste("=", formatC(mauchly_p, format='f', digits=3))
                           
                           if(!is.na(mauchly_p) && mauchly_p < 0.05) {
                              # Sphericity Violated -> Correct
                              df1 <- df1 * gg_eps
                              df2 <- df2 * gg_eps
                              p_val <- pf(F_val, df1, df2, lower.tail = FALSE)
                              
                              test_name <- "Repeated Measures ANOVA (Greenhouse-Geisser)"
                              sphericity_note <- glue::glue("Sphericity violated (Mauchly's P {p_disp_mauchly}), correction applied (epsilon = {formatC(gg_eps, format='f', digits=3)}).")
                           } else {
                              sphericity_note <- glue::glue("Sphericity assumed (Mauchly's P {p_disp_mauchly}).")
                           }
                        }, silent = TRUE)
                      }
                      
                      res$test <- test_name
                      res$p <- p_val
                      res$stat <- paste("F =", round(F_val, 2))
                      res$note <- glue::glue("Data is normally distributed. {sphericity_note}")
                      res$sphericity_note <- glue::glue(", {tolower(substr(sphericity_note, 1, 1))}{substring(sphericity_note, 2)}") # Format for footnote (starts with comma, lower case)
                      
                      # -----------------------------------------------------------
                      # POST-HOC: PAIRWISE PAIRED T-TESTS (Bonferroni)
                      # -----------------------------------------------------------
                      if(res$p < 0.05) {
                         # Transform data for pairwise.t.test
                         # long_df has 'Value', 'Group', 'ID'
                         
                         ph <- pairwise.t.test(long_df$Value, long_df$Group, 
                                               p.adjust.method = "bonferroni", 
                                               paired = TRUE, 
                                               alternative = "two.sided")
                         
                         # Convert output matrix to dataframe
                         pw_df <- as.data.frame(as.table(ph$p.value))
                         pw_df <- na.omit(pw_df)
                         colnames(pw_df) <- c("Group 1", "Group 2", "Adjusted P-value")
                         
                         res$pairwise_table <- pw_df
                         res$plot_signif_data <- pw_df # Use same data for plotting
                         
                         # Format P-values for text output if needed
                         # (The main rendering loop handles table formatting, but we prep internal data here)
                         
                         # Text Gen
                         sig_count <- sum(pw_df$"Adjusted P-value" < 0.05)
                         if(sig_count > 0) {
                            res$pairwise_text <- paste(sig_count, "pair(s) show significant difference after Bonferroni adjustment.")
                         } else {
                            res$pairwise_text <- "No specific pairs were significant after adjustment."
                         }
                         
                         # Format P for table display (Internal storage, though rendering often re-formats)
                         res$pairwise_table[[3]] <- sapply(res$pairwise_table[[3]], function(p) {
                            if(p < 0.001) "< 0.001" else formatC(p, format="f", digits=3)
                         })
                         
                         res$posthoc_test <- "Pairwise Paired T-tests (Bonferroni)"
                      }

                   }, error=function(e) { res$test <- "Error (RM ANOVA)"; res$note <- e$message })
                } else {
                   test <- friedman.test(as.matrix(sub_df))
                   res$test <- "Friedman Test"; res$p <- test$p.value; res$stat <- paste("Chi-sq =", round(test$statistic, 2))
                   res$note <- "Data is not normally distributed. A non-parametric test was used."
                   
                   if(res$p < 0.05) {
                      # -----------------------------------------------------------
                      # CUSTOM MEAN-RANK PAIRWISE (Dunn-Bonferroni)
                      # -----------------------------------------------------------
                      
                      # 1. Rank data within each Subject/Row
                      mat <- as.matrix(sub_df)
                      # t(apply(...)) ensures ranks are computed row-wise
                      ranks_mat <- t(apply(mat, 1, rank))
                      
                      # 2. Calculate Mean Ranks
                      N <- nrow(mat)
                      k <- ncol(mat)
                      mean_ranks <- colMeans(ranks_mat)
                      
                      # 3. Standard Error
                      # SE = sqrt( k(k+1) / (6N) )
                      sigma <- sqrt((k * (k + 1)) / (6 * N))
                      
                      # 4. Pairwise Comparisons
                      grps <- names(mean_ranks)
                      combos <- combn(grps, 2)
                      
                      pw_list <- list()
                      
                      m <- (k * (k - 1)) / 2 # Number of comparisons for Bonferroni
                      
                      for(i in 1:ncol(combos)) {
                         g1 <- combos[1, i]
                         g2 <- combos[2, i]
                         
                         mr1 <- mean_ranks[[g1]]
                         mr2 <- mean_ranks[[g2]]
                         
                         # Z statistic
                         z <- (mr1 - mr2) / sigma
                         
                         # Unadjusted P (2-tailed)
                         p_raw <- 2 * (1 - pnorm(abs(z)))
                         
                         # Bonferroni Adj
                         p_adj <- p_raw * m
                         if(p_adj > 1) p_adj <- 1
                         
                         pw_list[[i]] <- data.frame(
                            "Group 1" = g1,
                            "Group 2" = g2,
                            "Z" = z,
                            "Mean Rank Diff" = mr1 - mr2,
                            "Adjusted P-value" = p_adj,
                            check.names = FALSE
                         )
                      }
                      
                      res$pairwise_table <- do.call(rbind, pw_list)
                      res$plot_signif_data <- res$pairwise_table
                      
                      # Format P
                      res$pairwise_table[["Adjusted P-value"]] <- sapply(res$pairwise_table[["Adjusted P-value"]], function(p) {
                         if(p < 0.001) "< 0.001" else formatC(p, format = "f", digits = 3)
                      })
                      
                      res$pairwise_text <- "Post-hoc pairwise comparisons (Dunn-Bonferroni) revealed significant differences."
                      res$posthoc_test <- "Dunn-Bonferroni"
                   }
                }
             }
          }
       }
       if(!is.null(res$p) && (is.na(res$p) || is.nan(res$p))) {
           res$p <- 1
       }
       return(res)
    }

    # -------------------------------------------------------------------------
    # FUNCTION: CORRELATION ANALYSIS
    # -------------------------------------------------------------------------
    do_correlation_analysis <- function(sub_df, var1, var2) {
       req(var1, var2)
       # Ensure vars exist
       if(!all(c(var1, var2) %in% names(sub_df))) return(list(test="Error", note="Variables not found"))
       
       df_curr <- sub_df[, c(var1, var2)]
       df_curr <- na.omit(df_curr)
       
       if(nrow(df_curr) < 3) return(list(test="Error", note="Not enough data (n < 3)"))
       
       x <- as.numeric(df_curr[[1]])
       y <- as.numeric(df_curr[[2]])
       
       res <- list(mode = "correlation", params = list(type="Correlation", var1=var1, var2=var2))
       
       # Normality
       n_x <- TRUE; n_y <- TRUE
       p_x <- NA; p_y <- NA
       
       if(length(x) >= 3 && length(x) <= 5000) {
          p_x <- tryCatch(shapiro.test(x)$p.value, error = function(e) 0)
          if(p_x < 0.05) n_x <- FALSE
          p_x <- p_x
       }
       if(length(y) >= 3 && length(y) <= 5000) {
          p_y <- tryCatch(shapiro.test(y)$p.value, error = function(e) 0)
          if(p_y < 0.05) n_y <- FALSE
          p_y <- p_y
       }
       
       res$normality <- list(x_normal = n_x, y_normal = n_y, x_p = p_x, y_p = p_y)
       
       if(n_x && n_y) {
          # Pearson
          test <- tryCatch(cor.test(x, y, method = "pearson", alternative = "two.sided"), error = function(e) list(estimate = c(cor=0), p.value = 1, conf.int = c(0,0)))
          res$test <- "Pearson Correlation"
          res$coeff <- test$estimate
          res$p <- test$p.value
          res$ci <- test$conf.int
          res$note <- "Both variables normally distributed."
       } else {
          # Spearman
          test <- tryCatch(cor.test(x, y, method = "spearman", exact=FALSE, alternative = "two.sided"), error = function(e) list(estimate = c(rho=0), p.value = 1))
          res$test <- "Spearman Correlation"
          res$coeff <- test$estimate
          res$p <- test$p.value
          res$ci <- NULL 
          res$note <- "One or both variables not normally distributed."
       }
       
       # Interpretation Text
       r <- res$coeff
       strength <- case_when(
          abs(r) < 0.2 ~ "very weak",
          abs(r) < 0.4 ~ "weak",
          abs(r) < 0.6 ~ "moderate",
          abs(r) < 0.8 ~ "strong",
          TRUE ~ "very strong"
       )
       direction <- if(r > 0) "positive" else "negative"
       
       p_txt <- if(res$p < 0.001) "< 0.001" else paste("=", formatC(res$p, format="f", digits=3))
       ci_txt <- if(!is.null(res$ci)) glue(", 95% CI: {round(res$ci[1], 2)} to {round(res$ci[2], 2)}") else ""
       
       if(!is.null(res$p) && (is.na(res$p) || is.nan(res$p))) {
           res$p <- 1
       }
       res$interpretation <- glue("There was a {strength} {direction} correlation between {var1} and {var2} (r = {round(r, 2)}, P {p_txt}{ci_txt}).")
       return(res)
    }

    # -------------------------------------------------------------------------
    # 2. SELECTION LOGIC
    # -------------------------------------------------------------------------
    
    # A. CORRELATION ANALYSIS
    # A. CORRELATION ANALYSIS
    if(a_type == "Correlation" && (!isTRUE(input$enable_split) || is.null(input$split_var) || input$split_var == "")) {
       req(p_corr1, p_corr2)
       df_curr <- df[, c(p_corr1, p_corr2)]
       df_curr <- na.omit(df_curr)
       
       x <- as.numeric(df_curr[[1]])
       y <- as.numeric(df_curr[[2]])
       
       res <- list(mode = "correlation", params = list(type="Correlation", var1=p_corr1, var2=p_corr2))
       
       # Normality
       n_x <- TRUE; n_y <- TRUE
       p_x <- NA; p_y <- NA
       
       if(length(x) >= 3 && length(x) <= 5000) {
          p_x <- tryCatch(shapiro.test(x)$p.value, error = function(e) 0)
          if(p_x < 0.05) n_x <- FALSE
       }
       if(length(y) >= 3 && length(y) <= 5000) {
          p_y <- tryCatch(shapiro.test(y)$p.value, error = function(e) 0)
          if(p_y < 0.05) n_y <- FALSE
       }
       
       res$normality <- list(x_normal = n_x, y_normal = n_y, x_p = p_x, y_p = p_y)
       
       if(n_x && n_y) {
          # Pearson
          test <- tryCatch(cor.test(x, y, method = "pearson", alternative = "two.sided"), error = function(e) list(estimate = c(cor=0), p.value = 1, conf.int = c(0,0)))
          res$test <- "Pearson Correlation"
          res$coeff <- test$estimate
          res$p <- test$p.value
          res$ci <- test$conf.int
          res$note <- "Both variables normal."
       } else {
          # Spearman
          test <- tryCatch(cor.test(x, y, method = "spearman", exact=FALSE, alternative = "two.sided"), error = function(e) list(estimate = c(rho=0), p.value = 1))
          res$test <- "Spearman Correlation"
          res$coeff <- test$estimate
          res$p <- test$p.value
          res$ci <- NULL # Spearman doesn't produce CI easily in base R
          res$note <- "One or both variables not normal."
       }
       
       # Interpretation Text
       r <- res$coeff
       strength <- case_when(
          abs(r) < 0.2 ~ "very weak",
          abs(r) < 0.4 ~ "weak",
          abs(r) < 0.6 ~ "moderate",
          abs(r) < 0.8 ~ "strong",
          TRUE ~ "very strong"
       )
       direction <- if(r > 0) "positive" else "negative"
       
       p_txt <- if(res$p < 0.001) "< 0.001" else paste("=", formatC(res$p, format="f", digits=3))
       ci_txt <- if(!is.null(res$ci)) glue(", 95% CI: {round(res$ci[1], 2)} to {round(res$ci[2], 2)}") else ""
       
       if(!is.null(res$p) && (is.na(res$p) || is.nan(res$p))) {
           res$p <- 1
       }
       res$interpretation <- glue("There was a {strength} {direction} correlation between {p_corr1} and {p_corr2} (r = {round(r, 2)}, P {p_txt}{ci_txt}).")
       
       return(res)
    }
    
    # B. SPLIT ANALYSIS
    if(isTRUE(input$enable_split) && !is.null(input$split_var) && input$split_var != "") {
       s_var <- input$split_var
       
       # Override type for split_var?
       s_vec <- df[[s_var]]
       
       s_vec <- as.factor(s_vec)
       lvls <- levels(s_vec)
       # Remove empty strings from levels to avoid ghost groups
       lvls <- lvls[lvls != ""]
       
       # Master List
       master_res <- list(
          mode = "split",
          split_var = s_var,
          results = list()
       )
       
       for(lvl in lvls) {
          # Subset (Handle NA explicitly)
          idx <- s_vec == lvl & !is.na(s_vec)
          sub_df <- df[idx, ]
          
          # Skip if empty
          if(nrow(sub_df) < 2) next
          
          # Run Analysis
          # For Independent, the Group Var must exist in subset.
          # For Paired, check rows.
          
          # Catch errors per split
          split_res <- tryCatch({
             if(a_type == "Correlation") {
                do_correlation_analysis(sub_df, p_corr1, p_corr2)
             } else {
                do_core_analysis(sub_df, a_type, p_outcome, p_group, p_paired)
             }
          }, error = function(e) list(test="Error", note=e$message))
          
          master_res$results[[lvl]] <- split_res
       }
       return(master_res)
       
    } else {
       # C. STANDARD ANALYSIS
       return(do_core_analysis(df, a_type, p_outcome, p_group, p_paired))
    }
  })
  
  # C. Render Outputs
  
  output$group_color_pickers <- renderUI({
     req(input$analysis_type)
     df <- data()
     
     # Determine levels based on analysis type
     lvls <- NULL
     
     if(input$analysis_type == "Independent") {
        req(input$group_var)
        if(input$group_var %in% names(df)) {
           x <- as.factor(df[[input$group_var]])
           lvls <- levels(x)
           
           # Respect Viz Group Order
           if(!is.null(input$viz_group_order) && length(input$viz_group_order) > 0) {
              valid <- intersect(input$viz_group_order, lvls)
              if(length(valid) > 0) lvls <- valid
           }
        }
     } else if (input$analysis_type == "Paired") {
        req(input$paired_vars)
        if(length(input$paired_vars) >= 2) {
           lvls <- input$paired_vars # Already guaranteed to be column names
           
           # Respect Viz Group Order (if applicable to Paired)
           if(!is.null(input$viz_group_order) && length(input$viz_group_order) > 0) {
              valid <- intersect(input$viz_group_order, lvls)
              if(length(valid) > 0) lvls <- valid
           }
        }
     }
     
     if(is.null(lvls)) return(NULL)
     
     # Default colors for better UX
     # Try to inherit from the current palette selection if possible
     pal_name <- input$color_palette %||% "Pastel1"
     n_needed <- length(lvls)
     
     # Get colors from RColorBrewer
     # brewer.pal typically needs at least 3 colors, max varies (8-12)
     # We handle this by using colorRampPalette to interpolate if needed
     base_cols <- tryCatch({
        RColorBrewer::brewer.pal(n = max(3, min(n_needed, 8)), name = pal_name)
     }, error = function(e) scales::hue_pal()(n_needed))
     
     if(n_needed > length(base_cols)) {
        def_cols <- colorRampPalette(base_cols)(n_needed)
     } else {
        def_cols <- base_cols[1:n_needed]
     }
     
     # Generate Pickers
     ui_list <- lapply(seq_along(lvls), function(i) {
        # Re-rendering resets UI; using simple approach.
        
        div(style="margin-bottom: 5px;",
           colourInput(paste0("col_", i), label = lvls[i], value = def_cols[i])
        )
     })
     
     do.call(tagList, ui_list)
  })
  
  # Warning Banner
  # Warning Banner
  # Warning Banner
  output$normality_warning <- renderUI({
    req(results())
    res <- results()
    
    warnings <- c()
    
    # Helper to check normality in standard result
    check_norm <- function(r) {
       if(is.null(r$is_normal) || is.na(r$is_normal)) return(FALSE)
       return(!r$is_normal)
    }
    
    if(!is.null(res$mode) && res$mode == "split") {
       for(n in names(res$results)) {
          if(check_norm(res$results[[n]])) warnings <- c(warnings, paste0("Group '", n, "': Data is not normally distributed."))
       }
    } else if(!is.null(res$mode) && res$mode == "correlation") {
       if(isFALSE(res$normality$x_normal)) warnings <- c(warnings, paste0("'", res$params$var1, "' is not normally distributed."))
       if(isFALSE(res$normality$y_normal)) warnings <- c(warnings, paste0("'", res$params$var2, "' is not normally distributed."))
    } else {
       if(check_norm(res)) warnings <- c(warnings, "Outcome variable is not normally distributed. Switched to non-parametric test.")
    }

    if(length(warnings) > 0) {
      div(class = "alert alert-warning", role = "alert", style = "padding: 8px; font-size: 0.9em; margin-bottom: 10px;",
          HTML(paste("⚠️ <b>Warning:</b>", paste(warnings, collapse="; "))))
    }
  })
  
  # Methodology Block
  # Methodology Block
  output$methodology <- renderText({
    res <- results()
    
    if(!is.null(res$test) && res$test == "Error") return(paste("Error:", res$note))
    
    if(!is.null(res$mode) && res$mode == "split") {
       return(glue("Methodology (Split Group Analysis):\nAnalysis performed separately for each level of '{res$split_var}'. See detailed results below."))
    }
    
    if(!is.null(res$mode) && res$mode == "correlation") {
       p_txt <- if(res$p < 0.001) "< 0.001" else formatC(res$p, format='f', digits=3)
       return(glue("Methodology:\nAssumption Check: {res$note}\nTest Used: {res$test}\nStatistic: {names(res$coeff)} = {round(res$coeff, 2)}\nP-Value: {p_txt}"))
    }
    
    p_txt <- if(res$p < 0.001) "< 0.001" else formatC(res$p, format='f', digits=3)
    glue("Methodology:\n",
         "Assumption Check: {res$note}\n",
         "Test Used: {res$test}\n",
         "Statistic: {res$stat}\n",
         "P-value: {p_txt}")
  })
  
  # Interpretation Block
  output$interpretation <- renderText({
    res <- results()
    
    if(!is.null(res$test) && res$test == "Error") return(paste("Error:", res$note))
    
    # A. SPLIT ANALYSIS
    if(!is.null(res$mode) && res$mode == "split") {
       sv <- res$split_var %||% "Group"
       txt <- glue("Split Group Analysis Results (by {sv}):\n\n")
       for(n in names(res$results)) {
          r <- res$results[[n]]
          
          # SAFEGUARD TEST NAME
          t_name <- r$test %||% "Error"
          
          if(t_name == "Error") {
             note <- r$note %||% "Unknown error"
             txt <- paste0(txt, "[", n, "]: Error - ", note, "\n\n")
          } else if(!is.null(r$mode) && r$mode == "correlation") {
             # Correlation specific text
             r_val <- r$coeff %||% 0
             strength <- case_when(
                abs(r_val) < 0.2 ~ "very weak",
                abs(r_val) < 0.4 ~ "weak",
                abs(r_val) < 0.6 ~ "moderate",
                abs(r_val) < 0.8 ~ "strong",
                TRUE ~ "very strong"
             )
             direction <- if(r_val > 0) "positive" else "negative"
             
             p_val <- r$p %||% 1
             p_txt <- if(p_val < 0.001) "< 0.001" else paste("=", formatC(p_val, format='f', digits=3))
             
             v1 <- r$params$var1 %||% "Var1"
             v2 <- r$params$var2 %||% "Var2"
             
             txt <- paste0(txt, "In group '", n, "', there was a ", strength, " ", direction, " correlation between ", v1, " and ", v2, " (r = ", round(r_val, 2), ", P ", p_txt, ").\n\n")
          } else {
             # SAFEGUARD P-VALUE
             if(is.null(r$p)) {
                 txt <- paste0(txt, "[", n, "]: Result available but P-value missing.\n\n")
             } else {
                 sig <- if(r$p < 0.05) "statistically significant" else "non-significant"
                 p_txt <- if(r$p < 0.001) "< 0.001" else paste("=", formatC(r$p, format='f', digits=3))
                 txt <- paste0(txt, "In group '", n, "', the analysis showed a ", sig, " result (", t_name, ", P ", p_txt, ").\n\n")
             }
          }
       }
       return(txt)
    }
    
    # B. CORRELATION ANALYSIS
    if(!is.null(res$mode) && res$mode == "correlation") {
       return(res$interpretation)
    }
    
    # C. STANDARD ANALYSIS
    if(is.null(res$test) || res$test == "Error") return(paste("Error:", res$note %||% "Unknown error"))
    
    # Safety Check for P-value
    if(is.null(res$p)) return("Error: P-value could not be calculated.")

    p_val_text <- if(res$p < 0.001) "< 0.001" else paste("=", formatC(res$p, format="f", digits=3))
    sig_status <- if(res$p < 0.05) "statistically significant" else "not statistically significant"
    
    # Base Conclusion
    text <- glue("Conclusion:\nThe analysis revealed a {sig_status} difference ({res$test}, P {p_val_text}). ")
    
    or_string_val <- NULL
    
    # Logic: "Group A had X times higher odds of reporting [Outcome] compared to Group B (OR=...)."
    # Calculate this string regardless of significance for 2x2 categorical
    if(res$mode == "categorical" && !is.null(res$or_data)) {
       or_val <- res$or_data$or_val
       ci_lo <- res$or_data$ci_lo
       ci_hi <- res$or_data$ci_hi
       
       # Extract group names from the table dimension names
       row_names <- rownames(res$props) # Outcome levels
       col_names <- colnames(res$props) # Group levels
       
       # Assume we describe the odds of the FIRST outcome level
       outcome_target <- row_names[1] 
       
       if(or_val > 1) {
          # Direct
          grp_focus <- col_names[1]
          grp_comp  <- col_names[2]
          final_or  <- or_val
          final_lo  <- ci_lo
          final_hi  <- ci_hi
       } else {
          # Inverted
          grp_focus <- col_names[2]
          grp_comp  <- col_names[1]
          final_or  <- 1 / or_val
          final_lo  <- 1 / ci_hi # Swap limits and invert
          final_hi  <- 1 / ci_lo
       }
       
       or_string_val <- glue("{grp_focus} had over {round(final_or, 1)} times higher odds of reporting '{outcome_target}' in {res$params$outcome} compared to {grp_comp} ",
                      "(OR = {round(final_or, 2)}; 95% CI: {round(final_lo, 2)} - {round(final_hi, 2)})")
    }

    if(grepl("paired", res$mode)) {
       # ------------------------------------------------------------------
       # PAIRED ANALYSIS INTERPRETATION
       # ------------------------------------------------------------------
       term <- if(res$mode == "paired_categorical") "association/change" else "difference"
       
       if(res$p >= 0.05) {
         text <- paste(text, "There was no statistically significant change or difference between the paired groups/time-points.")
       } else {
         text <- paste(text, "There was a statistically significant change or difference between the paired groups/time-points.")
       }
       
       if(res$mode == "paired_categorical") {
          # McNemar Interpretation
          text <- paste(text, "\nMcNemar's test assesses whether the proportion of outcomes changed significantly between the two conditions.")
          
          # Kappa Interpretation
          if(!is.null(res$kappa)) {
             k <- res$kappa
             k_interp <- dplyr::case_when(
                k < 0 ~ "Poor",
                k <= 0.2 ~ "Slight",
                k <= 0.4 ~ "Fair",
                k <= 0.6 ~ "Moderate",
                k <= 0.8 ~ "Substantial",
                TRUE ~ "Almost Perfect"
             )
             text <- paste(text, glue("\n\nAgreement Analysis:\nCohen's Kappa = {round(k, 2)} ({k_interp} Agreement). \n(Note: High agreement means individual subjects tended to have similar results in both conditions, regardless of whether the overall proportions changed.)"))
          }
       } else if(res$mode == "paired_2group") {
          # Paired T-test / Wilcoxon
          if(!is.null(res$ttest_data)) {
             d_val <- round(res$ttest_data$diff, 2)
             ci_l <- round(res$ttest_data$ci_lo, 2)
             ci_h <- round(res$ttest_data$ci_hi, 2)
             text <- paste(text, glue("\nThe mean difference (Group 1 - Group 2) was {d_val} (95% CI: {ci_l} to {ci_h})."))
          }
          if(!is.null(res$desc_param)) {
              m <- round(res$desc_param$means, 2)
              text <- paste(text, glue("\nMeans: {paste(names(m), m, sep='=', collapse=', ')}"))
          }
       } else if(res$mode == "paired_multigroup") {
           # RM ANOVA / Friedman
           if(!is.null(res$pairwise_text)) text <- paste(text, "\n\n", res$pairwise_text)
       }
       
    } else {
       # ------------------------------------------------------------------
       # INDEPENDENT ANALYSIS INTERPRETATION
       # ------------------------------------------------------------------
        if(res$p >= 0.05) {
           text <- paste(text, glue("There was no significant difference found in {res$params$outcome} between the groups. Therefore, we cannot conclude that one group was higher or differently distributed than the other."))
           if(!is.null(or_string_val)) {
             text <- paste(text, "\n\n", "However, for descriptive purposes: ", or_string_val)
          }
       } else {
           # Significant - Describe direction and stats
           if(res$mode == "categorical") {
              text <- paste(text, glue("There was a significant association between the groups and {res$params$outcome}."))
              if(!is.null(or_string_val)) {
                text <- paste(text, "\n\n", or_string_val)
             }
             if(!is.null(res$props)) {
                prop_txts <- c()
                for(g in colnames(res$props)) {
                   p_vec <- res$props[, g]
                   outcome_strs <- paste0(names(p_vec), " (", round(p_vec, 1), "%)")
                   g_str <- paste0(g, ": [", paste(outcome_strs, collapse=", "), "]")
                   prop_txts <- c(prop_txts, g_str)
                }
                text <- paste(text, "\nDetails:", paste(prop_txts, collapse="; "))
             }
          } else if (res$mode == "2group") {
             g_names <- names(res$desc_param$means)
             g1 <- g_names[1]
             g2 <- g_names[2]
             
             if(res$is_normal) {
                m1 <- round(res$desc_param$means[1], 2); sd1 <- round(res$desc_param$sds[1], 2)
                m2 <- round(res$desc_param$means[2], 2); sd2 <- round(res$desc_param$sds[2], 2)
                winner <- if(m1 > m2) g1 else g2
                loser  <- if(m1 > m2) g2 else g1
                val_winner <- if(m1 > m2) paste0(m1, " ± ", sd1) else paste0(m2, " ± ", sd2)
                val_loser  <- if(m1 > m2) paste0(m2, " ± ", sd2) else paste0(m1, " ± ", sd1)
                
                text <- paste(text, glue("{winner} (Mean = {val_winner}) had a significantly higher value than {loser} (Mean = {val_loser})."))
                
                if(!is.null(res$ttest_data)) {
                   raw_diff <- res$ttest_data$diff
                   if(winner == g1) {
                      final_diff <- raw_diff
                      final_ci_lo <- res$ttest_data$ci_lo
                      final_ci_hi <- res$ttest_data$ci_hi
                   } else {
                      final_diff <- -raw_diff
                      final_ci_lo <- -res$ttest_data$ci_hi
                      final_ci_hi <- -res$ttest_data$ci_lo
                   }
                   text <- paste(text, glue("\nThe mean difference is {round(final_diff, 2)} (95% CI: {round(final_ci_lo, 2)} to {round(final_ci_hi, 2)})."))
                }
             } else {
                med1 <- round(res$desc_nonparam$medians[1], 2)
                iqr1 <- paste0(round(res$desc_nonparam$q1[1], 2), "-", round(res$desc_nonparam$q3[1], 2))
                med2 <- round(res$desc_nonparam$medians[2], 2)
                iqr2 <- paste0(round(res$desc_nonparam$q1[2], 2), "-", round(res$desc_nonparam$q3[2], 2))
                winner <- if(med1 > med2) g1 else g2
                loser  <- if(med1 > med2) g2 else g1
                val_winner <- if(med1 > med2) paste0(med1, ", IQR=", iqr1) else paste0(med2, ", IQR=", iqr2)
                val_loser  <- if(med1 > med2) paste0(med2, ", IQR=", iqr2) else paste0(med1, ", IQR=", iqr1)
                text <- paste(text, glue("{winner} (Median={val_winner}) had a significantly higher value than {loser} (Median={val_loser})."))
             }
          } else {
             # Multigroup
             if(res$is_normal) {
                text <- paste(text, glue("The group '{res$high_group}' had the highest mean values."))
             } else {
                text <- paste(text, glue("The group '{res$high_group}' had the highest values (rank/median)."))
             }
             if(!is.null(res$pairwise_text)) text <- paste(text, "\n\n", res$pairwise_text)
          }
       }
    }
    
    return(text)
  })
    

  
  # D. Table Logic
  table_data_list <- reactive({
    req(results())
    res <- results()
    df <- vals$filtered_data %||% data()
    
    # --- MATCHING VAR TYPE OVERRIDES ---
    p_outcome <- input$outcome_var
    p_group <- input$group_var
    
    if(input$force_outcome_type != "Auto" && !is.null(p_outcome) && p_outcome %in% names(df)) {
       if(input$force_outcome_type == "Numeric") {
          df[[p_outcome]] <- as.numeric(as.character(df[[p_outcome]]))
       } else if(input$force_outcome_type == "Categorical") {
          df[[p_outcome]] <- as.factor(df[[p_outcome]])
       }
    }
    if(input$force_group_type != "Auto" && !is.null(p_group) && p_group %in% names(df)) {
       if(input$force_group_type == "Categorical") {
          df[[p_group]] <- as.factor(df[[p_group]])
       }
    }
    # -----------------------------------
    
    # SAFETY CHECK: Ensure columns exist in current dataset
    # This prevents crash when switching datasets while analysis results from previous dataset persist
    needed_cols <- NULL
    
    if(!is.null(res$mode) && res$mode == "split") {
       # For split analysis, we need to check the split variable AND the inner params
       # We assume homogeneity, so checking the first valid result is sufficient
       first_res <- NULL
       for(r in res$results) { if(r$test != "Error") { first_res <- r; break } }
       
       if(!is.null(first_res)) {
          if(!is.null(first_res$mode) && first_res$mode == "correlation") {
             needed_cols <- c(res$split_var, first_res$params$var1, first_res$params$var2)
          } else {
             needed_cols <- c(res$split_var, first_res$params$outcome, first_res$params$group, first_res$params$paired_vars)
          }
       } else {
          needed_cols <- c(res$split_var)
       }
    } else {
       # Standard Analysis
       needed_cols <- c(res$params$outcome, res$params$group, res$params$paired_vars, res$params$var1, res$params$var2)
    }
    
    needed_cols <- needed_cols[!is.null(needed_cols) & needed_cols != "" & !is.na(needed_cols)]
    
    if(length(needed_cols) > 0 && !all(needed_cols %in% names(df))) {
       return(NULL) 
    }
    
    # User Preferences
    dp_in <- input$decimal_places
    dp <- if(is.null(dp_in) || is.na(dp_in)) 0 else dp_in
    
    # Helpers
    fmt <- function(x) formatC(x, format="f", digits=dp)
    fmt_p_3 <- function(p) {
       if(is.na(p) || is.null(p)) return("")
       if(is.numeric(p) && p < 0.001) "< 0.001" else formatC(p, format="f", digits=3)
    }

    # Internal function to generate DF for one result
    get_one_table_pack <- function(r, sub_df) {
        if(is.null(r) || is.null(r$params)) return(NULL)
        if(r$test == "Error") return(NULL)
        
        main_df <- NULL
        norm_df <- NULL
        pw_df <- r$pairwise_table
        
        # 1. Correlation
        if(!is.null(r$mode) && r$mode == "correlation") {
            main_df <- data.frame(
               "Variable 1" = r$params$var1,
               "Variable 2" = r$params$var2,
               "Coefficient" = paste0(round(r$coeff, 3)),
               "95% CI" = if(!is.null(r$ci)) paste0(round(r$ci[1], 3), " to ", round(r$ci[2], 3)) else "-",
               "P-value" = fmt_p_3(r$p),
               check.names=FALSE
            )
            
            # Normality Table for Correlation
            norm_df <- data.frame(
               "Variable" = c(r$params$var1, r$params$var2),
               "Shapiro P-value" = c(fmt_p_3(r$normality$x_p), fmt_p_3(r$normality$y_p)),
               check.names = FALSE
            )
            
            return(list(main=main_df, norm=norm_df, pairwise=NULL))
        }

        # 2. Standard / Split Item
        p_outcome <- r$params$outcome
        p_group <- r$params$group
        p_paired <- r$params$paired_vars
        
        # Categorical
        if(!is.null(r$mode) && r$mode == "categorical") {
           raw_tab <- table(sub_df[[p_outcome]], sub_df[[p_group]])
           
           # Order
           grps <- colnames(raw_tab)
           if(!is.null(input$tbl_group_order)) {
              valid <- intersect(input$tbl_group_order, grps)
              if(length(valid) > 0) grps <- valid
           }
           raw_tab <- raw_tab[, grps, drop=FALSE]
           
           main_df <- data.frame(Level = rownames(raw_tab))
           col_totals <- colSums(raw_tab)
            for(g in grps) {
               cnts <- raw_tab[, g]
               N <- col_totals[g]
               pcts <- (cnts / N) * 100
               new_col_name <- paste0(g, " (N=", N, ")")
               main_df[[new_col_name]] <- paste0(cnts, " (", formatC(pcts, format="f", digits=1), "%)")
            }
           main_df[["P-value"]] <- ""
           main_df[1, "P-value"] <- fmt_p_3(r$p)
           
           if(!is.null(r$or_data) && nrow(main_df)==2) {
               or_txt <- paste0(round(r$or_data$or_val, 2), " (", round(r$or_data$ci_lo, 2), "-", round(r$or_data$ci_hi, 2), ")")
               main_df$"OR (95% CI)" <- ""
               main_df[1, "OR (95% CI)"] <- or_txt 
           }
           
        } else if (grepl("paired", r$mode)) {
           # Paired
           nms <- p_paired
           main_df <- data.frame(Comparison = paste(nms, collapse=" vs "))
           
           sub_df_clean <- na.omit(sub_df[, nms])
           
            for(n in nms) {
               yg <- sub_df_clean[[n]]
               N <- length(yg)
               new_col_name <- paste0(n, " (N=", N, ")")
               
               if(is.numeric(yg)) {
                   m <- mean(yg); s <- sd(yg)
                   med <- median(yg); q1 <- quantile(yg, 0.25); q3 <- quantile(yg, 0.75)
                   val_str <- if(isTRUE(r$is_normal)) paste0(fmt(m), " \u00B1 ", fmt(s)) else paste0(fmt(med), " (", fmt(q1), "-", fmt(q3), ")")
                   main_df[[new_col_name]] <- val_str
               } else {
                   main_df[[new_col_name]] <- paste("N=", length(yg))
               }
            }
            
            # Paired Assumption: Normality of Difference (Single P)
            norm_df <- data.frame(
               "Difference/Residuals" = if(grepl("multigroup", r$mode)) "Model Residuals" else "Difference",
               "Shapiro P-value" = fmt_p_3(r$diff_shapiro_p),
               check.names = FALSE
            )
           main_df[["P-value"]] <- fmt_p_3(r$p)
           if(length(nms)==2 && is.numeric(sub_df_clean[[1]]) && !is.null(r$ttest_data)) {
               md <- r$ttest_data$diff
               main_df$"Mean Diff (95% CI)" <- paste0(fmt(md), " (", fmt(r$ttest_data$ci_lo), " to ", fmt(r$ttest_data$ci_hi), ")")
           }
           
        } else if (grepl("2group", r$mode) || grepl("multigroup", r$mode)) {
           # Independent Numeric
           main_df <- data.frame(Variable = p_outcome)
           norm_df <- data.frame(Variable = p_outcome)
           
           x <- sub_df[[p_group]]
           y <- sub_df[[p_outcome]]
           valid <- !is.na(x) & !is.na(y)
           x <- x[valid]; y <- y[valid]
           x <- as.factor(x) # Ensure it's a factor (numeric groups would crash droplevels)
           x <- droplevels(x)
           
           grps <- levels(x)
           if(!is.null(input$tbl_group_order)) { # Use shared input? Yes
              valid <- intersect(input$tbl_group_order, grps)
              if(length(valid) > 0) grps <- valid
           }
           
            for(g in grps) {
               yg <- y[x == g]
               N <- length(yg)
               new_col_name <- paste0(g, " (N=", N, ")")
               
               # Normality
               sw_p <- NA
               if(length(yg) >= 3 && length(yg) <= 5000) sw_p <- tryCatch(shapiro.test(yg)$p.value, error = function(e) 0)
               norm_df[[new_col_name]] <- fmt_p_3(sw_p)
               
               # Main Stats
               m <- mean(yg); s <- sd(yg)
               med <- median(yg); q1 <- quantile(yg, 0.25); q3 <- quantile(yg, 0.75)
               val_str <- if(isTRUE(r$is_normal)) paste0(fmt(m), " \u00B1 ", fmt(s)) else paste0(fmt(med), " (", fmt(q1), "-", fmt(q3), ")")
               main_df[[new_col_name]] <- val_str
            }
           main_df[["P-value"]] <- fmt_p_3(r$p)
           
           if(length(grps) == 2 && isTRUE(r$is_normal) && !is.null(r$ttest_data)) {
               md <- r$ttest_data$diff
               ci_str <- paste0(fmt(md), " (", fmt(r$ttest_data$ci_lo), " to ", fmt(r$ttest_data$ci_hi), ")")
               main_df$"Mean Diff (95% CI)" <- ci_str
           }
        }
           

        
        return(list(main=main_df, norm=norm_df, pairwise=pw_df))
    }

    # Main Rendering Logic
    tables_out <- list()
    
    # 1. SPLIT Mode
    if(!is.null(res$mode) && res$mode == "split") {
       s_var <- res$split_var
       
       for(lvl in names(res$results)) {
          r <- res$results[[lvl]]
          
          # Subset data
          idx <- df[[s_var]] == lvl & !is.na(df[[s_var]])
          sub_df <- df[idx, ]
          
          pack <- get_one_table_pack(r, sub_df)
          if(is.null(pack)) next
          
          # Title
          title <- paste0("Comparison of ", r$params$outcome, " by ", r$params$group, ": ", lvl)
          
          tables_out[[length(tables_out)+1]] <- list(
             id = paste0("split_", length(tables_out)+1),
             title = title,
             main = pack$main,
             norm = pack$norm,
             pairwise = pack$pairwise,
             test_note = r$test,
             levene = r$levene_note,
             sphericity = r$sphericity_note,
             posthoc = r$posthoc_test
          )
       }
       
    } else {
       # 2. STANDARD / CORRELATION Mode
       pack <- get_one_table_pack(res, df)
       
       # Title Logic
       title <- ""
       if(!is.null(res$mode) && res$mode == "correlation") {
          title <- paste("Correlation Analysis:", res$params$var1, "vs", res$params$var2)
       } else if(!is.null(res$params$type) && res$params$type == "Independent") {
          if(!is.null(res$mode) && res$mode == "categorical") {
             title <- paste("Proportion of", res$params$outcome, "by", res$params$group)
          } else {
             title <- paste("Comparison of", res$params$outcome, "by", res$params$group)
          }
       } else {
          title <- "Paired Analysis Results"
       }
       final_title <- if(nchar(input$table_title %||% "") > 0) input$table_title else title
       
       if(!is.null(pack)) {
           tables_out[[1]] <- list(
             id = "main_1",
             title = final_title,
             main = pack$main,
             norm = pack$norm,
             pairwise = pack$pairwise,
             test_note = res$test,
             levene = res$levene_note,
             sphericity = res$sphericity_note,
             posthoc = res$posthoc_test
           )
       }
    }
    
    return(list(tables = tables_out, mode = res$mode))
  })
  
  # Dynamic Table Observers
   observe({
      dat <- table_data_list()
      req(dat)
      
      for(i in seq_along(dat$tables)) {
         local({
            my_i <- i
            tbl <- dat$tables[[my_i]]
            
            # 1. Filename (Matches HTML export format style)
            fname <- paste0("biostat_result_", Sys.Date())
            
            # 2. Footnote for Export (messageBottom)
            # Combine Test Note + Levene + Sphericity + Posthoc (if applicable)
            ft_export <- paste("Test:", tbl$test_note)
            if(!is.null(tbl$levene) && tbl$levene != "") ft_export <- paste(ft_export, tbl$levene)
            if(!is.null(tbl$sphericity) && tbl$sphericity != "") ft_export <- paste(ft_export, tbl$sphericity)
            if(!is.null(tbl$posthoc)) ft_export <- paste(ft_export, "\nPost-hoc:", tbl$posthoc)
            
            # 3. Button Configuration
            # dom='Bfrtip' places Buttons (B) above the Table. 
            # The Table includes the Caption (Title), so Buttons -> Caption -> Table.
            
            dt_opts <- list(
               dom = 'Bfrtip', 
               buttons = list(
                  list(extend = 'copy', messageBottom = ft_export, title = ""),
                  list(extend = 'csv', filename = fname, messageBottom = ft_export),
                  list(extend = 'excel', filename = fname, messageBottom = ft_export, title = NULL) 
                  # title=NULL to rely on Caption (avoids double title)
               ),
               paging = FALSE,
               scrollX = TRUE,
               ordering = FALSE,
               searching = FALSE,
               info = FALSE
            )
            
            # 4. Render Main Table
            # Use Caption for Title ("Buttons above Title" effect)
            cap_main <- if(isTRUE(input$show_table_title)) {
               htmltools::tags$caption(style = 'caption-side: top; text-align: left; color: black; font-weight: bold; font-size: 1.1em;', tbl$title)
            } else NULL
            
            output[[paste0("dyn_main_", my_i)]] <- renderDT({ 
                datatable(tbl$main, extensions = 'Buttons', options = dt_opts, caption = cap_main) %>%
                formatRound(columns = names(tbl$main)[sapply(tbl$main, is.numeric)], digits = input$decimal_places %||% 2)
            })
            
            # 5. Render Normality Table
            # Custom title and simple footnote for this specific table if needed, 
            # but usually global footnote is fine or we keep it simple.
            dt_opts_norm <- dt_opts
            dt_opts_norm$buttons <- list(
               list(extend='copy', title=""),
               list(extend='csv', filename=paste0(fname, "_norm"), title=NULL),
               list(extend='excel', filename=paste0(fname, "_norm"), title=NULL)
            )
            cap_norm <- if(input$show_shapiro) {
                htmltools::tags$caption(style = 'caption-side: top; text-align: left; color: black; font-weight: bold;', "Shapiro-Wilk Test for Normality")
            } else NULL
            
            if(!is.null(tbl$norm)) {
               output[[paste0("dyn_norm_", my_i)]] <- renderDT({ 
                   datatable(tbl$norm, extensions = 'Buttons', options = dt_opts_norm, caption = cap_norm) 
               })
            }
            
            # 6. Render Pairwise Table
            dt_opts_pw <- dt_opts # Reuse export footnote
            dt_opts_pw$buttons <- list(
               list(extend='copy', messageBottom = ft_export, title = ""),
               list(extend='csv', filename=paste0(fname, "_pairwise"), messageBottom = ft_export),
               list(extend='excel', filename=paste0(fname, "_pairwise"), messageBottom = ft_export, title=NULL)
            )
            cap_pw <- if(input$show_pairwise) {
                 htmltools::tags$caption(style = 'caption-side: top; text-align: left; color: black; font-weight: bold;', "Pairwise Comparisons")
            } else NULL
            
            if(!is.null(tbl$pairwise)) {
               output[[paste0("dyn_pw_", my_i)]] <- renderDT({ 
                   datatable(tbl$pairwise, extensions = 'Buttons', options = dt_opts_pw, caption = cap_pw) 
               })
            }
         })
      }
   })

  output$table_container <- renderUI({
     dat <- table_data_list()
     
     # Placeholder if no results yet
     # Placeholder if no results yet
     if(is.null(dat)) {
        return(div(style="min-height: 30vh; display: flex; align-items: center; justify-content: center; color: #aaa; text-align: center;",
           div(
              icon("chart-simple", class = "fa-3x", style = "margin-bottom: 15px; display: block;"),
              h4("Run Analysis to view results")
           )
        ))
     }

     
     ui_list <- lapply(seq_along(dat$tables), function(i) {
        tbl <- dat$tables[[i]]
        
        # Footnotes
        main_foot <- if(isTRUE(input$show_footnote_table)) {
           txt <- paste("Test Used:", tbl$test_note)
           if(!is.null(tbl$levene) && tbl$levene != "") {
              txt <- paste0(txt, tbl$levene) 
           }
           if(!is.null(tbl$sphericity) && tbl$sphericity != "") {
              txt <- paste0(txt, tbl$sphericity) 
           }
           p(txt, style="font-style: italic; color: #666; font-size: 0.9em;")
        } else NULL
        
        ph_foot <- if(isTRUE(input$show_footnote_table) && !is.null(tbl$posthoc)) {
           p(paste("Post-hoc Test:", tbl$posthoc), style="font-style: italic; color: #666; font-size: 0.9em;")
        } else NULL

        div(class = "card", style = "margin-bottom: 20px;",
           div(class = "card-body",
              # if(isTRUE(input$show_table_title)) h5(tbl$title, class="card-title") else NULL, # Title moved to DT Caption
              DTOutput(paste0("dyn_main_", i)),
              main_foot,
              
              if(input$show_shapiro && !is.null(tbl$norm)) tagList(
                 # h6("Shapiro-Wilk Test for Normality", style="margin-top: 15px;"), # Title moved to DT Caption
                 DTOutput(paste0("dyn_norm_", i))
              ) else NULL,
              
              if(input$show_pairwise && !is.null(tbl$pairwise)) tagList(
                 # h6("Pairwise Comparisons", style="margin-top: 15px;"), # Title moved to DT Caption
                 DTOutput(paste0("dyn_pw_", i)),
                 ph_foot
              ) else NULL
           )
        )
     })
     
     do.call(tagList, ui_list)
  })
  
  # Export Handlers

  
  output$dl_table_word <- downloadHandler(
    filename = function() { paste0("biostat_report_", Sys.Date(), ".html") },
    content = function(file) {
      dat <- table_data_list()
      req(dat)

      # Helper for HTML table
      df_to_html <- function(df, caption=NULL, footnote=NULL) {
         if(is.null(df)) return("")
         cols <- colnames(df)
         header <- paste0("<tr>", paste0("<th style='background:#eee; padding:5px;'>", cols, "</th>", collapse=""), "</tr>")
         rows <- apply(df, 1, function(x) paste0("<tr>", paste0("<td style='padding:5px;'>", x, "</td>", collapse=""), "</tr>"))
         cap <- if(!is.null(caption)) paste0("<h3>", caption, "</h3>") else ""
         foot <- if(!is.null(footnote)) paste0("<p style='font-style:italic; color:#666;'>", footnote, "</p>") else ""
         
         paste0(cap, "<table border='1' style='border-collapse:collapse; width:100%; font-family:Arial;'>", header, paste(rows, collapse=""), "</table>", foot)
      }
      
      all_html <- ""
      
      for(tbl in dat$tables) {
         # Footnotes
         main_foot_txt <- if(isTRUE(input$show_footnote_table)) {
            t <- paste0("Test Used: ", tbl$test_note)
            if(!is.null(tbl$levene) && tbl$levene != "") t <- paste0(t, tbl$levene)
            t
         } else NULL
         
         posthoc_foot_txt <- if(isTRUE(input$show_footnote_table) && !is.null(tbl$posthoc)) paste0("Post-hoc Test: ", tbl$posthoc) else NULL
         
         main_html <- df_to_html(tbl$main, tbl$title, main_foot_txt)
         norm_html <- if(input$show_shapiro) df_to_html(tbl$norm, "Shapiro-Wilk Test for Normality") else ""
         pw_html <- if(input$show_pairwise && !is.null(tbl$pairwise)) df_to_html(tbl$pairwise, "Pairwise Comparisons", posthoc_foot_txt) else ""
         
         all_html <- paste(all_html, main_html, norm_html, pw_html, "<hr>", sep="\n")
      }

      html_content <- glue("
        <html xmlns:o='urn:schemas-microsoft-com:office:office' xmlns:w='urn:schemas-microsoft-com:office:word' xmlns='http://www.w3.org/TR/REC-html40'>
        <head>
           <meta charset='utf-8'>
           <title>BioStat Report</title>
           <style>body {{ font-family: Arial, sans-serif; }}</style>
        </head>
        <body>
          <h2>Statistical Report</h2>
          <p>Generated by DARK-ART on {Sys.Date()}</p>
          {all_html}
        </body>
        </html>
      ")
      writeLines(html_content, file)
    }
  )
  
  # Toggle Right Sidebar
  observeEvent(input$show_graph_settings, {
    sidebar_toggle("right_sidebar")
  })

  # --- SMART AXIS LABEL RESET ---
  # Automatically clear custom axis labels when the underlying variables change
  
  # 1. Independent Analysis (Group = X, Outcome = Y)
  observeEvent(input$group_var, { 
    updateTextInput(session, "custom_title", value = "")
    updateTextInput(session, "custom_x", value = "") 
    updateNumericInput(session, "y_min", value = NA)
    updateNumericInput(session, "y_max", value = NA)
    updateNumericInput(session, "y_step", value = NA)
  })
  observeEvent(input$outcome_var, { 
    updateTextInput(session, "custom_title", value = "")
    updateTextInput(session, "custom_y", value = "") 
    updateNumericInput(session, "y_min", value = NA)
    updateNumericInput(session, "y_max", value = NA)
    updateNumericInput(session, "y_step", value = NA)
  })
  
  # 2. Paired Analysis (Paired Vars handle both, usually)
  observeEvent(input$paired_vars, { 
     updateTextInput(session, "custom_title", value = "") 
     updateTextInput(session, "custom_x", value = "") 
     updateTextInput(session, "custom_y", value = "") 
     updateNumericInput(session, "y_min", value = NA)
     updateNumericInput(session, "y_max", value = NA)
     updateNumericInput(session, "y_step", value = NA)
  })
  
  # 3. Correlation Analysis (Var1 = X, Var2 = Y)
  observeEvent(input$corr_var1, { 
    updateTextInput(session, "custom_title", value = "")
    updateTextInput(session, "custom_x", value = "") 
    updateNumericInput(session, "y_min", value = NA)
    updateNumericInput(session, "y_max", value = NA)
    updateNumericInput(session, "y_step", value = NA)
  })
  observeEvent(input$corr_var2, { 
    updateTextInput(session, "custom_title", value = "")
    updateTextInput(session, "custom_y", value = "") 
    updateNumericInput(session, "y_min", value = NA)
    updateNumericInput(session, "y_max", value = NA)
    updateNumericInput(session, "y_step", value = NA)
  })
  
  # 4. Global Analysis Mode Switch
  observeEvent(input$analysis_type, {
     updateTextInput(session, "custom_title", value = "") 
     updateTextInput(session, "custom_x", value = "") 
     updateTextInput(session, "custom_y", value = "") 
     updateNumericInput(session, "y_min", value = NA)
     updateNumericInput(session, "y_max", value = NA)
     updateNumericInput(session, "y_step", value = NA)
  })
  # ------------------------------

  # Reset Y-Axis Triggers
  observeEvent(list(input$reset_y_axis, input$analyze), {
     updateNumericInput(session, "y_min", value = NA)
     updateNumericInput(session, "y_max", value = NA)
     updateNumericInput(session, "y_step", value = NA)
  })

  # Smart Plot Container
  output$plot_container <- renderUI({
     # If Fixed Size is enabled, use specific pixel dimensions and scroll
     # If Disabled (default), use responsive 100% width
     
     if(isTRUE(input$use_fixed_size)) {
        w <- input$plot_width %||% 800
        h <- input$plot_height %||% 600
        div(style = "overflow-x: auto;",
           plotOutput("smart_plot", width = paste0(w, "px"), height = paste0(h, "px"))
        )
     } else {
        # Responsive default - Fill space but keep min 30vh
        div(style = "height: 100%; min-height: 30vh;",
            plotOutput("smart_plot", width = "100%", height = "100%", fill = TRUE)
        )
     }
  })

  # Dynamic Export Warning
  output$export_warning <- renderUI({
     req(input$viz_view)
     msg <- ""
     if(input$viz_view == "Visualization") {
        msg <- "Export figure may differ from the view. Set 'Fixed Dimensions' for accuracy."
     } else if(input$viz_view == "Result Table") {
        msg <- "Export formatting may differ from the view."
     }
     
     if(msg != "") {
        span(msg, style = "color: #666; font-size: 0.8em; font-style: italic; padding-right: 10px;")
     } else {
        NULL
     }
  })

  # Reset Appearance Logic
  observeEvent(input$reset_style, {
      showModal(modalDialog(
          title = "Confirm Reset",
          "Reset all appearance settings to default?",
          footer = tagList(
             modalButton("Cancel"),
             actionButton("confirm_reset_style", "Reset", class = "btn-warning")
          )
      ))
  })
  
  observeEvent(input$confirm_reset_style, {
      # 1. Elements
      updateNumericInput(session, "point_size", value = 2)
      updateNumericInput(session, "stroke_width", value = 0.5)
      
      # 2. Text
      updateNumericInput(session, "font_size", value = 15)
      updateSelectInput(session, "font_family", selected = "sans")
      updateSelectInput(session, "font_style", selected = "plain")
      
      # 3. Colours
      updateRadioButtons(session, "color_mapping", selected = "group")
      updateCheckboxInput(session, "use_palette", value = TRUE)
      updateSelectInput(session, "color_palette", selected = "Pastel1")
      colourpicker::updateColourInput(session, "solid_color", value = "#5DA5DA")
      
      # 4. Axis & Labels
      updateTextInput(session, "custom_title", value = "")
      updateTextInput(session, "custom_x", value = "")
      updateTextInput(session, "custom_y", value = "")
      updateNumericInput(session, "y_min", value = NA)
      updateNumericInput(session, "y_max", value = NA)
      updateNumericInput(session, "y_step", value = NA)
      updateCheckboxInput(session, "remove_axis_gap", value = FALSE)
      updateCheckboxInput(session, "remove_axis_gap_y", value = TRUE)
      
      # 5. Other Elements
      updateCheckboxInput(session, "show_plot_title", value = TRUE)
      updateCheckboxInput(session, "show_footnote_viz", value = TRUE)
      updateCheckboxInput(session, "show_jitter", value = FALSE)
      updateCheckboxInput(session, "add_ref_line", value = FALSE)
      updateNumericInput(session, "ref_line_val", value = 0)
      updateCheckboxInput(session, "show_boxplot_caps", value = FALSE)
      updateSelectInput(session, "force_plot_type", selected = "auto")
      
      # 6. Significance
      updateCheckboxInput(session, "show_signif", value = TRUE)
      updateRadioButtons(session, "signif_format", selected = "p.value")
      updateNumericInput(session, "signif_font_size", value = 5)
      updateNumericInput(session, "signif_tip_length", value = 0.05)
      
      removeModal()
      showNotification("Appearance settings reset to defaults.", type = "message")
  })

  # Reactive Plot Object (Extracted for re-use in Export)
  final_plot <- reactive({
    req(input$analyze)
    res <- results()
    df <- vals$filtered_data %||% data()
    
    # --- MATCHING VAR TYPE OVERRIDES ---
    p_outcome <- input$outcome_var
    p_group <- input$group_var
    
    if(input$force_outcome_type != "Auto" && !is.null(p_outcome) && p_outcome %in% names(df)) {
       if(input$force_outcome_type == "Numeric") {
          df[[p_outcome]] <- as.numeric(as.character(df[[p_outcome]]))
       } else if(input$force_outcome_type == "Categorical") {
          df[[p_outcome]] <- as.factor(df[[p_outcome]])
       }
    }
    if(input$force_group_type != "Auto" && !is.null(p_group) && p_group %in% names(df)) {
       if(input$force_group_type == "Categorical") {
          df[[p_group]] <- as.factor(df[[p_group]])
       }
    }
    # -----------------------------------
    
    # SAFETY CHECK: Ensure columns exist in current dataset
    needed_cols <- NULL
    
    if(!is.null(res$mode) && res$mode == "split") {
       # For split analysis, we need to check the split variable AND the inner params
       first_res <- NULL
       for(r in res$results) { if(r$test != "Error") { first_res <- r; break } }
       
       if(!is.null(first_res)) {
          needed_cols <- c(res$split_var, first_res$params$outcome, first_res$params$group, first_res$params$paired_vars)
       } else {
          needed_cols <- c(res$split_var)
       }
    } else {
       needed_cols <- c(res$params$outcome, res$params$group, res$params$paired_vars, res$params$var1, res$params$var2)
    }
    
    needed_cols <- needed_cols[!is.null(needed_cols) & needed_cols != "" & !is.na(needed_cols)]
    
    if(length(needed_cols) > 0 && !all(needed_cols %in% names(df))) {
       return(NULL) 
    }
    
    # Safe Inputs (Handle NULLs from modal)
    in_title <- input$custom_title %||% ""
    in_x <- input$custom_x %||% ""
    in_y <- input$custom_y %||% ""
    in_palette <- input$color_palette %||% "Pastel1"
    in_solid <- input$solid_color %||% "#E8F4F8"
    
    # Font Settings
    fs <- input$font_size %||% 15
    ff <- input$font_family %||% "sans"
    fst <- input$font_style %||% "plain"
    
    # Style Settings
    ps <- input$point_size %||% 2
    sw <- input$stroke_width %||% 0.5
    
    # Conditional Border Color: If stroke width is 0, make it invisible (NA)
    bar_border <- if(sw == 0) NA else "black"
    
    # X-Axis Expansion Logic (Controlled by remove_axis_gap)
    x_expansion <- if(isTRUE(input$remove_axis_gap)) expansion(mult = c(0, 0.05)) else waiver()
    
    # Y-Axis Expansion Logic (Controlled by remove_axis_gap_y)
    y_expansion <- if(isTRUE(input$remove_axis_gap_y)) expansion(mult = c(0, 0.05)) else waiver()
    
    # --- COLOR HELPER ---
    # Safe interpolation for palettes to avoid warning/error
    get_safe_scale_fill <- function(pal_name, groups) {
         n_needed <- length(groups)
         if(n_needed == 0) return(NULL)
         
         # Base colors
         # RColorBrewer max varies (Set1=9, Pastel1=9, etc.)
         base_cols <- tryCatch({
              RColorBrewer::brewer.pal(n = max(3, min(n_needed, 8)), name = pal_name)
         }, error = function(e) scales::hue_pal()(n_needed))
         
         if(n_needed > length(base_cols)) {
              cols <- colorRampPalette(base_cols)(n_needed)
         } else {
              cols <- base_cols[1:n_needed]
         }
         # Map specifically to the levels to ensure consistency
         names(cols) <- groups
         return(scale_fill_manual(values = cols))
    }
    
    # Base Theme
    my_theme <- theme_classic(base_size = fs, base_family = ff) +
                theme(text = element_text(face = fst),
                      plot.title = element_text(hjust = 0.5, face = "bold"),
                      plot.caption = element_text(hjust = 0.5, color = "gray30"))
    
    caption_txt <- if(isTRUE(input$show_footnote_viz)) {
       # Handle split mode
       if(!is.null(res$mode) && res$mode == "split") {
          # Extract test name from first valid result
          test_names <- c()
          for(r in res$results) {
             if(r$test != "Error") {
                test_names <- c(test_names, r$test)
             }
          }
          unique_tests <- unique(test_names)
          
          if(length(unique_tests) > 0) {
             if(length(unique_tests) == 1) {
                txt <- paste("Test:", unique_tests[1])
             } else {
                txt <- paste("Tests:", paste(unique_tests, collapse = "; "))
             }
          } else {
             txt <- NULL
          }
       } else {
          # Standard mode
          txt <- paste("Test:", res$test)
           if(!is.null(res$levene_note) && res$levene_note != "") {
               txt <- paste0(txt, res$levene_note)
           }
           if(!is.null(res$sphericity_note) && res$sphericity_note != "") {
               txt <- paste0(txt, res$sphericity_note)
           }
       }
       txt
    } else NULL

    # -------------------------------------------------------------
    # 1. CORRELATION PLOT
    # -------------------------------------------------------------
    if(!is.null(res$mode) && res$mode == "correlation") {
      df_plot <- df[, c(res$params$var1, res$params$var2)]
       # FIX: Force numeric conversion for plotting to safe-guard geom_smooth against Factors
       df_plot[[res$params$var1]] <- as.numeric(df_plot[[res$params$var1]])
       df_plot[[res$params$var2]] <- as.numeric(df_plot[[res$params$var2]])
       df_plot <- na.omit(df_plot)
       
       # Determine point and line colors based on color settings
       # SAFEGUARD: color_mapping might be NULL
       cm <- input$color_mapping %||% "group"
       point_color <- if(cm == "single") in_solid else "#4E84C4"
       line_color <- if(cm == "single") in_solid else "#D16103"
       line_fill <- line_color
       
       p <- ggplot(df_plot, aes(x = .data[[res$params$var1]], y = .data[[res$params$var2]])) +
          geom_point(alpha=0.6, color=point_color, size=ps) +
          geom_smooth(method=if(res$test=="Pearson Correlation") "lm" else "loess", se=TRUE, color=line_color, fill=line_fill, alpha=0.2, linewidth=sw) +
          my_theme
       
       # Apply axis labels
       final_x <- if(nchar(in_x) > 0) in_x else res$params$var1
       final_y <- if(nchar(in_y) > 0) in_y else res$params$var2
       
       # Apply reference line if enabled
       # SAFEGUARD: ref_line_val can be NULL
       if(isTRUE(input$add_ref_line) && !is.null(input$ref_line_val) && !is.na(input$ref_line_val)) {
          p <- p + geom_hline(yintercept = input$ref_line_val, linetype = "dashed", color = "black", linewidth = sw)
       }
       
       # Apply title
       final_title <- if(nchar(in_title) > 0) in_title else paste("Correlation:", res$params$var1, "vs", res$params$var2)
       if(!isTRUE(input$show_plot_title)) final_title <- NULL
       
       p <- p + labs(title = final_title, x = final_x, y = final_y, caption = caption_txt) +
          scale_x_continuous(expand = x_expansion)
       return(p)
    }

       # -------------------------------------------------------------
       # 2. SPLIT ANALYSIS PLOT
       # -------------------------------------------------------------
       if(!is.null(res$mode) && res$mode == "split") {
          # Params
          p_split <- res$split_var
          
          # We need to dig into the first valid result to get params (assuming homogenous)
          first_res <- NULL
          valid_results <- list()
          for(r in res$results) { 
             if(r$test != "Error") { 
                if(is.null(first_res)) first_res <- r
                valid_results[[length(valid_results)+1]] <- r
             } 
          }
          
          if(is.null(first_res)) return(NULL) # All errors
          
          # Initialize variables
          p_outcome <- NULL
          p_group <- NULL
          df_plot <- NULL
          
          # CHECK: Is this Paired Analysis or Independent?
          is_paired_split <- grepl("paired", first_res$mode)
          message("[DEBUG SPLIT] is_paired_split=", is_paired_split, " mode=", first_res$mode)
          
          if(is_paired_split) {
             # --- PAIRED SPLIT PREP ---
             # Reshape Wide -> Long so it mimics Independent structure for plotting (Group vs Split)
             p_paired <- first_res$params$paired_vars
             message("[DEBUG SPLIT] p_paired=", paste(p_paired, collapse=","), " p_split=", p_split)
             
             # Filter & Reshape
             # We take Split Var + Paired Columns
             sub_cols <- c(p_split, p_paired)
             message("[DEBUG SPLIT] sub_cols=", paste(sub_cols, collapse=","), " df_cols=", paste(names(df)[1:min(10,ncol(df))], collapse=","))
             
             # Check columns exist before subsetting
             missing_cols <- sub_cols[!sub_cols %in% names(df)]
             if(length(missing_cols) > 0) {
                message("[DEBUG SPLIT] MISSING columns: ", paste(missing_cols, collapse=","))
                return(NULL)
             }
             
             df_temp <- df[, sub_cols, drop = FALSE]
             df_temp <- na.omit(df_temp)
             message("[DEBUG SPLIT] df_temp nrow=", nrow(df_temp), " ncol=", ncol(df_temp))
             
             if(nrow(df_temp) == 0) { message("[DEBUG SPLIT] df_temp empty after na.omit"); return(NULL) }
             
             # Safe column name to avoid collision when p_split == "Group"
             paired_grp_col <- if(p_split == "Group") ".PairedGroup" else "Group"
             
             # Reshape Wide -> Long using base R (robust for column names with spaces)
             long_parts <- lapply(p_paired, function(col_name) {
                 temp_part <- df_temp[, p_split, drop = FALSE]
                 temp_part[[paired_grp_col]] <- col_name
                 temp_part$Value <- df_temp[[col_name]]
                 temp_part
              })
              df_plot <- do.call(rbind, long_parts)
              message("[DEBUG SPLIT] manual reshape OK nrow=", nrow(df_plot), " cols=", paste(names(df_plot), collapse=","))
             
             # Set "Virtual" Param Names for the Plotter
             p_outcome <- "Value"
             p_group <- paired_grp_col
             
             # Ensure Factor Order Matches Input Order
             df_plot[[paired_grp_col]] <- factor(df_plot[[paired_grp_col]], levels = p_paired)
             
          } else {
             # --- INDEPENDENT SPLIT PREP ---
             p_outcome <- first_res$params$outcome
             p_group <- first_res$params$group
             
             # Prepare Data
             if(!is.null(first_res$mode) && first_res$mode == "correlation") {
                df_plot <- df
             } else {
                # Robust Filtering
                # Start with split variable (must exist)
                keep_idx <- !is.na(df[[p_split]])
                
                # Add Group criteria if exists
                if(!is.null(p_group) && p_group %in% names(df)) {
                   keep_idx <- keep_idx & !is.na(df[[p_group]])
                }
                
                # Add Outcome criteria if exists
                if(!is.null(p_outcome) && p_outcome %in% names(df)) {
                   keep_idx <- keep_idx & !is.na(df[[p_outcome]])
                }
                
                df_plot <- df[keep_idx, ]
             }
          }
          
          # FORCE SPLIT VARIABLE TO FACTOR (CRITICAL FIX)
          if(!is.null(df_plot) && p_split %in% names(df_plot)) {
             df_plot[[p_split]] <- as.factor(df_plot[[p_split]])
          }
          
          # Remove old viz_group_order filter - robust global order is used instead
          
          # CHECK GLOBAL NORMALITY FOR SPLIT
          # If ANY group in any split is non-normal -> Use Non-Parametric (Boxplot)
          # If ALL groups in ALL splits are normal -> Use Parametric (Bar Chart)
          
          is_global_parametric <- TRUE
          for(r in valid_results) {
             # Check if this specific split result was parametric
             # In standard analysis: res$is_normal tells us
             if(is.null(r$is_normal) || isFALSE(r$is_normal)) {
                is_global_parametric <- FALSE
                break 
             }
          }
          
          if(!is.null(input$force_plot_type) && input$force_plot_type != "auto") {
             is_global_parametric <- (input$force_plot_type == "bar")
          }
          
          if(!is.null(first_res$mode) && first_res$mode == "correlation") {
             # Correlation: Faceted Scatter Plot
             p_var1 <- first_res$params$var1
             p_var2 <- first_res$params$var2
             
             if(length(p_split) != 1 || p_split == "" || !p_split %in% names(df)) return(NULL)
             if(length(p_var1) != 1 || p_var1 == "" || !p_var1 %in% names(df)) return(NULL)
             if(length(p_var2) != 1 || p_var2 == "" || !p_var2 %in% names(df)) return(NULL)
             
             # Re-filter for correlation vars (Robust)
             keep_idx <- !is.na(df[[p_split]])
             keep_idx <- keep_idx & !is.na(df[[p_var1]])
             keep_idx <- keep_idx & !is.na(df[[p_var2]])
             
             df_plot <- df[keep_idx, ]
             
             # ROBUSTNESS: Filter out groups with insufficient data (N < 3)
             # Use safe column extraction for table
             vals <- df_plot[[p_split]]
             t_counts <- table(vals)
             valid_grps <- names(t_counts[t_counts >= 3])

             if(length(valid_grps) == 0) return(NULL)
             
             df_plot <- df_plot[df_plot[[p_split]] %in% valid_grps, ]
             
             # Clean factor levels
             if(is.factor(df_plot[[p_split]])) {
                df_plot[[p_split]] <- factor(df_plot[[p_split]], levels = intersect(levels(df_plot[[p_split]]), valid_grps))
             } else {
                df_plot[[p_split]] <- factor(df_plot[[p_split]], levels = valid_grps)
             }
             
             # Determine point and line colors based on color settings
             # SAFEGUARD: color_mapping
             cm <- input$color_mapping %||% "group"
             point_color <- if(cm == "single") in_solid else "#4E84C4"
             line_color <- if(cm == "single") in_solid else "#D16103"
             line_fill <- line_color
             
             p <- ggplot(df_plot, aes(x = .data[[p_var1]], y = .data[[p_var2]])) +
                geom_point(alpha=0.6, color=point_color, size=ps) +
                geom_smooth(method="lm", formula= y~x, se=TRUE, color=line_color, fill=line_fill, alpha=0.2, linewidth=sw) +
                facet_wrap(as.formula(paste0("~ `", p_split, "`"))) +
                my_theme
             
             # Apply axis labels
             final_x <- if(nchar(in_x) > 0) in_x else p_var1
             final_y <- if(nchar(in_y) > 0) in_y else p_var2
             
             # Apply reference line if enabled
             if(isTRUE(input$add_ref_line) && !is.null(input$ref_line_val) && !is.na(input$ref_line_val)) {
                p <- p + geom_hline(yintercept = input$ref_line_val, linetype = "dashed", color = "black", linewidth = sw)
             }
             
             # Apply title
             final_title <- if(nchar(in_title) > 0) in_title else paste("Split Correlation:", p_var1, "vs", p_var2, "by", p_split)
             if(!isTRUE(input$show_plot_title)) final_title <- NULL
             
             p <- p + labs(title = final_title, x = final_x, y = final_y, caption = caption_txt) +
                 scale_x_continuous(expand = x_expansion)
             message("[DEBUG SPLIT] Returning p, class=", paste(class(p), collapse=","))
        return(p)
             
          } else if(grepl("categorical", first_res$mode)) {
             # Categorical Outcome: Stacked Bar Faceted
             p <- ggplot(df_plot, aes(x = .data[[p_group]], fill = .data[[p_outcome]])) +
                geom_bar(position = "fill", color=bar_border, size=sw) +
                facet_wrap(as.formula(paste0("~ `", p_split, "`"))) +
                scale_y_continuous(labels = scales::percent, expand = y_expansion) +
                my_theme + get_safe_scale_fill(in_palette, levels(as.factor(df_plot[[p_outcome]])))
             
             final_title <- if(nchar(in_title) > 0) in_title else paste("Split Analysis:", p_outcome, "by", p_group, "and", p_split)
             if(!isTRUE(input$show_plot_title)) final_title <- NULL
             
             final_x <- if(nchar(in_x) > 0) in_x else p_group
             final_y <- if(nchar(in_y) > 0) in_y else "Proportion"
             
             p <- p + labs(title = final_title, caption = caption_txt, x = final_x, y = final_y) +
                 scale_x_discrete(expand = x_expansion)
                
          } else {
             # Numeric Outcome
             
              # --- Define Color Scale for Split Analysis ---
              
              # Determine levels for color mapping
              g_split_lvls <- levels(as.factor(df_plot[[p_group]]))
              split_fill_scale <- get_safe_scale_fill(in_palette, g_split_lvls)
             
              cm <- input$color_mapping %||% "group"
             if(cm == 'single') {
                 # Single Color Mode
                 # Since aes(fill=group), we map all groups to the same solid color
                 g_lvls <- levels(as.factor(df_plot[[p_group]]))
                 split_fill_scale <- scale_fill_manual(values = rep(in_solid, length(g_lvls)))
             } else if(isFALSE(input$use_palette)) {
                 # Manual Color Mode
                 g_lvls <- levels(as.factor(df_plot[[p_group]]))
                 # Respect group order if set
                 if(!is.null(input$viz_group_order) && length(input$viz_group_order) > 0) {
                     valid <- intersect(input$viz_group_order, g_lvls)
                     if(length(valid) > 0) g_lvls <- valid
                 }
                 
                 custom_cols <- sapply(seq_along(g_lvls), function(i) {
                    input[[paste0("col_", i)]] %||% "#999999"
                 })
                 split_fill_scale <- scale_fill_manual(values = custom_cols)
             }
             
             if(is_global_parametric) {
                # --------------------------------
                # PARAMETRIC: Bar Chart (Mean + SD)
                # --------------------------------
                summ_df <- df_plot %>%
                   group_by(.data[[p_split]], .data[[p_group]]) %>%
                   summarise(
                      Mean = mean(.data[[p_outcome]]),
                      SD = sd(.data[[p_outcome]]),
                      .groups = "drop"
                   )
                
                # Base Aes
                p <- ggplot(summ_df, aes(x = .data[[p_split]], y = Mean, fill = .data[[p_group]])) +
                   geom_errorbar(aes(
                      ymin = case_when(Mean >= 0 ~ Mean, TRUE ~ Mean - SD),
                      ymax = case_when(Mean >= 0 ~ Mean + SD, TRUE ~ Mean)
                   ), position = position_dodge(width = 0.9), width = 0.25, linewidth=sw) +
                   geom_bar(stat = "identity", position = position_dodge(width = 0.9), color=bar_border, size=sw) +
                   my_theme + 
                   split_fill_scale
                
             } else {
                # --------------------------------
                # NON-PARAMETRIC: Boxplot (Median + IQR)
                # --------------------------------
                p <- ggplot(df_plot, aes(x = .data[[p_split]], y = .data[[p_outcome]], fill = .data[[p_group]]))
                if(isTRUE(input$show_boxplot_caps)) {
                   p <- p + stat_boxplot(geom = "errorbar", width = 0.2, color = bar_border, linewidth = sw)
                }
                p <- p + geom_boxplot(outlier.shape = NA, color=bar_border, linewidth=sw) +
                   my_theme +
                   split_fill_scale
                
                if(isTRUE(input$show_jitter)) {
                   p <- p + geom_point(position = position_jitterdodge(), alpha=0.3, size=ps*0.5) 
                   # Note: Split jitter is usually smaller, so scaling relative to ps
                }
             }
             
             # ---------------------------
             # Y-AXIS & LABEL LOGIC (Split)
             # ---------------------------
             if(is_global_parametric) {
                 # Recalculate range from summary for scaling
                 summ_range <- df_plot %>% group_by(.data[[p_split]], .data[[p_group]]) %>%
                    summarise(M=mean(.data[[p_outcome]]), S=sd(.data[[p_outcome]]), .groups="drop")
                 data_max <- max(summ_range$M + summ_range$S, na.rm=TRUE)
                 data_min <- min(df_plot[[p_outcome]], na.rm=TRUE) 
             } else {
                 data_min <- min(df_plot[[p_outcome]], na.rm=TRUE)
                 data_max <- max(df_plot[[p_outcome]], na.rm=TRUE)
             }
             
             # 1. Zero Line
             if(data_min < 0 && data_max > 0) {
               p <- p + geom_hline(yintercept=0, linetype="dashed", color="gray50", linewidth=sw)
             }
             
             if(isTRUE(input$add_ref_line) && !is.null(input$ref_line_val) && !is.na(input$ref_line_val)) {
                p <- p + geom_hline(yintercept = input$ref_line_val, linetype = "dashed", color = "black", linewidth = sw)
             }

             
             # 2. Limits & Breaks
             lim_min <- if(!is.null(input$y_min) && !is.na(input$y_min)) input$y_min else if(data_min >= 0) 0 else NA
             lim_max <- if(!is.null(input$y_max) && !is.na(input$y_max)) input$y_max else NA
             
             my_breaks <- waiver()
             if(!is.null(input$y_step) && !is.na(input$y_step)) {
                b_start <- if(!is.na(lim_min)) lim_min else floor(data_min)
                b_end   <- if(!is.na(lim_max)) lim_max else ceiling(data_max * 1.5)
                my_breaks <- seq(from = b_start, to = b_end, by = input$y_step)
             }
             
             p <- p + scale_y_continuous(limits = c(lim_min, lim_max), breaks = my_breaks, expand = y_expansion)

             # 3. Labels & Title
             base_title <- if(is_global_parametric) paste("Split Analysis :", p_outcome, "by", p_group) else paste("Split Analysis:", p_outcome, "by", p_group)
             final_title <- if(nchar(in_title) > 0) in_title else base_title
             
             if(!isTRUE(input$show_plot_title)) final_title <- NULL

             final_x <- if(nchar(in_x) > 0) in_x else p_split
             final_y <- if(nchar(in_y) > 0) in_y else p_outcome
             
             p <- p + labs(title = final_title, x = final_x, y = final_y, caption = caption_txt)
             
             # Apply X Expansion: Check if X is Discrete or Continuous (Boxplot/Bar vs others)
             # Logic: If using geom_bar/boxplot, X is likely discrete (p_split or p_group)
             # However, p_split might be converted to factor locally
             if(is.factor(df_plot[[p_split]]) || is.character(df_plot[[p_split]])) {
                p <- p + scale_x_discrete(expand = x_expansion)
             } else {
                p <- p + scale_x_continuous(expand = x_expansion)
             }
          }
          
          # --- ADD SIGNIFICANCE LINES (Split Group) ---
          if(isTRUE(input$show_signif) && is.numeric(df_plot[[p_outcome]])) {
             # Helper to convert p-value from string
             p_to_num <- function(x) {
                if(is.numeric(x)) return(x)
                if(grepl("<", x)) return(0.0009)
                as.numeric(x)
             }

             # Global range for step calculation (keeps spacing consistent visually)
             g_max_global <- max(df_plot[[p_outcome]], na.rm=TRUE)
             g_min_global <- min(df_plot[[p_outcome]], na.rm=TRUE)
             y_bump <- (g_max_global - g_min_global) * 0.1
             if(y_bump == 0) y_bump <- 1 # Safety

             for(lvl_name in names(res$results)) {
                r <- res$results[[lvl_name]]
                if(r$test == "Error") next
                
                # Check if there are significant comparisons
                target_pw <- r$plot_signif_data
                if(is.null(target_pw)) target_pw <- r$pairwise_table
                
                if(!is.null(target_pw)) {
                   pw <- target_pw
                   sig_rows <- c()
                   for(row_i in 1:nrow(pw)) {
                      val <- pw[row_i, "Adjusted P-value"]
                      num_val <- p_to_num(val)
                      if(!is.na(num_val) && num_val < 0.05) sig_rows <- c(sig_rows, row_i)
                   }
                   
                   if(length(sig_rows) > 0) {
                      pw_sig <- pw[sig_rows, ]
                      
                      # 1. Determine Local Max for this Split Group
                      # Filter data for this split
                      is_in_split <- df_plot[[p_split]] == lvl_name
                      split_data <- df_plot[is_in_split, ]
                      
                      # Default Local Max
                      local_max <- max(split_data[[p_outcome]], na.rm=TRUE)
                      
                      # If Parametric (Bar Chart), we need to account for Error Bars (Mean + SD)
                      if(is_global_parametric) {
                          summ_local <- split_data %>%
                             group_by(.data[[p_group]]) %>%
                             summarise(
                                Mean = mean(.data[[p_outcome]]),
                                SD = sd(.data[[p_outcome]]),
                                .groups = "drop"
                             )
                          local_max <- max(summ_local$Mean + summ_local$SD, na.rm=TRUE)
                      }
                      
                      # Initialize Y Start at 105% of Local Max
                      curr_y <- local_max * 1.05
                      
                      # 2. Iterate Logic
                      lvls_split <- levels(as.factor(df_plot[[p_split]]))
                      x_idx <- which(lvls_split == lvl_name)
                      
                      grp_lvls <- levels(as.factor(df_plot[[p_group]]))
                      n_grps <- length(grp_lvls)
                      w <- 0.8 # approx total width
                      
                      # --- RAINBOW STACKING (SORT BY DISTANCE) ---
                      # Collect valid comparisons first
                      comp_list <- list()
                      
                      for(row_i in 1:nrow(pw_sig)) {
                         g1 <- as.character(pw_sig[row_i, 1])
                         g2 <- as.character(pw_sig[row_i, 2])
                         
                         idx1 <- which(grp_lvls == g1)
                         idx2 <- which(grp_lvls == g2)
                         
                         if(length(idx1) > 0 && length(idx2) > 0) {
                            dist <- abs(idx1 - idx2)
                            
                            # Label Logic
                            pval_num <- p_to_num(pw_sig[row_i, "Adjusted P-value"])
                            sf <- input$signif_format %||% "p.value"
                            lab <- if(sf == "stars") {
                               case_when(pval_num < 0.001 ~ "***", pval_num < 0.01 ~ "**", pval_num < 0.05 ~ "*", TRUE ~ "ns")
                            } else {
                               raw_val <- pw_sig[row_i, "Adjusted P-value"]
                               if(is.numeric(raw_val)) {
                                  p_txt <- if(raw_val < 0.001) "< 0.001" else paste0("=", formatC(raw_val, format="f", digits=3))
                               } else {
                                  p_txt <- if(grepl("<", raw_val)) raw_val else paste0("=", raw_val)
                               }
                               paste0("P", p_txt)
                            }
                            
                            # Calc Coords
                            offset1 <- (idx1 - (n_grps+1)/2) * (w / n_grps)
                            offset2 <- (idx2 - (n_grps+1)/2) * (w / n_grps)
                            x_start <- x_idx + offset1
                            x_end   <- x_idx + offset2
                            
                            comp_list[[length(comp_list)+1]] <- list(
                               dist = dist,
                               xmin = x_start,
                               xmax = x_end,
                               label = lab
                            )
                         }
                      }
                      
                      # Sort by Distance (Shortest First = Rainbow Stacking)
                      if(length(comp_list) > 0) {
                         dists <- sapply(comp_list, function(x) x$dist)
                         sorted_comps <- comp_list[order(dists)]
                         
                         for(cp in sorted_comps) {
                             # Manual bracket: bypass ggsignif stat conflicts
                             tip_len_s <- input$signif_tip_length %||% 0.05
                             tip_h_s <- y_bump * tip_len_s * 10
                             if(tip_h_s == 0) tip_h_s <- 0.1
                             
                             seg_df <- data.frame(
                                x = c(cp$xmin, cp$xmin, cp$xmax),
                                xend = c(cp$xmin, cp$xmax, cp$xmax),
                                y = c(curr_y - tip_h_s, curr_y, curr_y - tip_h_s),
                                yend = c(curr_y, curr_y, curr_y)
                             )
                             lab_df <- data.frame(x = (cp$xmin + cp$xmax) / 2, y = curr_y, label = cp$label)
                             
                             p <- p + geom_segment(data = seg_df, aes(x = x, xend = xend, y = y, yend = yend),
                                                   inherit.aes = FALSE, linewidth = sw) +
                                      geom_text(data = lab_df, aes(x = x, y = y, label = label),
                                                inherit.aes = FALSE, vjust = -0.3, size = input$signif_font_size %||% 5)
                             curr_y <- curr_y + y_bump
                         }
                      }
                   }
                }
             }
          }
       
       message("[DEBUG SPLIT] Final return of p")
       return(p)
    }

    # -------------------------------------------------------------
    # 3. INDEPENDENT ANALYSIS (Standard)
    # -------------------------------------------------------------
    if(is.null(res$params$type)) return(NULL)
    
    if(res$params$type == "Independent") {
      # SNAPSHOT PARAMETERS
      p_outcome <- res$params$outcome
      p_group <- res$params$group


      
      y <- df[[p_outcome]]
      x <- as.factor(df[[p_group]])
      
      # Group Ordering
      if(!is.null(input$viz_group_order) && length(input$viz_group_order) > 0) {
         valid_grps <- intersect(input$viz_group_order, levels(x))
         if(length(valid_grps) > 0) {
            x <- factor(x, levels = valid_grps)
         }
      }
      
      # Prepare Data Frame for Plotting
      df_plot <- df[!is.na(df[[p_group]]) & !is.na(df[[p_outcome]]), ]
      df_plot[[p_group]] <- factor(df_plot[[p_group]], levels = levels(x))
      
      # Color Mapping Logic
      lvl_count <- length(levels(x))
      
      # SAFEGUARD
      cm <- input$color_mapping %||% "group"
      is_single_color <- (cm == "single")
      
      fill_scale <- NULL
      base_aes <- aes(x = .data[[p_group]], y = .data[[p_outcome]])
      
      if(!is_single_color) {
         # Group Coloring
         base_aes <- aes(x = .data[[p_group]], y = .data[[p_outcome]], fill = .data[[p_group]])
         if(isTRUE(input$use_palette)) {
            fill_scale <- get_safe_scale_fill(in_palette, levels(x))
         } else {
            custom_cols <- sapply(1:lvl_count, function(i) {
               input[[paste0("col_", i)]] %||% "#999999"
            })
            fill_scale <- scale_fill_manual(values = custom_cols)
         }
      }
      
      # Helper for Single Color
      my_fill <- if(is_single_color) in_solid else NULL
      
       # ----------------------------------
       # CORRELATION PLOT
       # ----------------------------------


        if(is.factor(y) || is.character(y)) {
        # ----------------------------------
        # CATEGORICAL OUTCOME (Stacked Bar)
        # ----------------------------------
        
        p <- ggplot(df_plot, aes(x = .data[[p_group]], fill = .data[[p_outcome]])) +
          geom_bar(position = "fill", color=bar_border, size=sw) +
          scale_y_continuous(labels = scales::percent, expand = y_expansion) +
          my_theme +
          get_safe_scale_fill(in_palette, levels(as.factor(df_plot[[p_outcome]]))) # Keep palette for outcome levels
         
         # Labels
        final_title <- if(nchar(in_title) > 0) in_title else paste("Proportion of", p_outcome, "by", p_group)
        if(!isTRUE(input$show_plot_title)) final_title <- NULL
        final_x <- if(nchar(in_x) > 0) in_x else p_group
        final_y <- if(nchar(in_y) > 0) in_y else "Proportion"
        
        p <- p + labs(title = final_title, caption = caption_txt, x = final_x, y = final_y, fill = p_outcome) +
           scale_x_discrete(expand = x_expansion)
        
      } else {
        # ----------------------------------
        # NUMERIC OUTCOME
        # ----------------------------------
        is_parametric <- isTRUE(res$is_normal)
        
        if(!is.null(input$force_plot_type) && input$force_plot_type != "auto") {
           is_parametric <- (input$force_plot_type == "bar")
        }
        
        # Determine global max for significance lines later
        # Default max
        global_max <- max(df_plot[[p_outcome]], na.rm=TRUE)
        
        if(is_parametric) {
           # PARAMETRIC: Bar Chart with Mean + SD
           summ_df <- df_plot %>%
              group_by(.data[[p_group]]) %>%
              summarise(
                 Mean = mean(.data[[p_outcome]]),
                 SD = sd(.data[[p_outcome]])
              )
           
           # Update global max to include error bars
           global_max <- max(summ_df$Mean + summ_df$SD, na.rm=TRUE)
           
           # Base Aes for Summary
           p_aes <- aes(x = .data[[p_group]], y = Mean)
           if(!is_single_color) p_aes <- aes(x = .data[[p_group]], y = Mean, fill = .data[[p_group]])
           
           # Start with Error Bars
           p <- ggplot(summ_df, p_aes) + 
              geom_errorbar(aes(
                 ymin = case_when(Mean >= 0 ~ Mean, TRUE ~ Mean - SD),
                 ymax = case_when(Mean >= 0 ~ Mean + SD, TRUE ~ Mean)
              ), width = 0.2, linewidth=sw)
              
           # Geom Bar (drawn over error bars to hide overlapping bottom cap)
           if(is_single_color) {
              p <- p + geom_bar(stat = "identity", color=bar_border, width=0.7, fill=in_solid, linewidth=sw)
           } else {
              p <- p + geom_bar(stat = "identity", color=bar_border, width=0.7, linewidth=sw) + fill_scale
           }
           
           p <- p + my_theme
              
        } else {
           # NON-PARAMETRIC: Boxplot
           # Base Aes
           p_aes <- aes(x = .data[[p_group]], y = .data[[p_outcome]])
           if(!is_single_color) p_aes <- aes(x = .data[[p_group]], y = .data[[p_outcome]], fill = .data[[p_group]])

           p <- ggplot(df_plot, p_aes)
           if(isTRUE(input$show_boxplot_caps)) {
              p <- p + stat_boxplot(geom = "errorbar", width = 0.2, color = bar_border, linewidth = sw)
           }

           if(is_single_color) {
              p <- p + geom_boxplot(outlier.shape = NA, fill=in_solid, color=bar_border, linewidth=sw)
           } else {
              p <- p + geom_boxplot(outlier.shape = NA, color=bar_border, linewidth=sw) + fill_scale
           }
           
           p <- p + my_theme
        }

        if(isTRUE(input$show_jitter)) {
           p <- p + geom_jitter(width = 0.2, alpha = 0.3, size=ps)
        }
        
        # ---------------------------
        # Y-AXIS & ZERO LINE LOGIC
        # ---------------------------
        data_min <- min(df_plot[[p_outcome]], na.rm=TRUE)
        data_max <- max(df_plot[[p_outcome]], na.rm=TRUE)
        
        # 1. Zero Reference Line
        if(data_min < 0 && data_max > 0) {
           p <- p + geom_hline(yintercept=0, linetype="dashed", color="gray50", linewidth=sw)
        }
        
        if(isTRUE(input$add_ref_line) && !is.null(input$ref_line_val) && !is.na(input$ref_line_val)) {
           p <- p + geom_hline(yintercept = input$ref_line_val, linetype = "dashed", color = "black", linewidth = sw)
        }
        
        # 2. Limits & Breaks
        # Default Auto-Zero for positive data
        lim_min <- if(data_min >= 0) 0 else NA   
        if(!is.null(input$y_min) && !is.na(input$y_min)) lim_min <- input$y_min
        
        lim_max <- if(!is.null(input$y_max) && !is.na(input$y_max)) input$y_max else NA
        
        my_breaks <- waiver()
        if(!is.null(input$y_step) && !is.na(input$y_step)) {
           # Robust sequence generation requires definite start/end
           # If NA, we approximate from data
           b_start <- if(!is.na(lim_min)) lim_min else floor(data_min)
           b_end   <- if(!is.na(lim_max)) lim_max else ceiling(data_max * 1.5)
           my_breaks <- seq(from = b_start, to = b_end, by = input$y_step)
        }
        
        
        # REMOVE FLOOR GAP if requested
        p <- p + scale_y_continuous(limits = c(lim_min, lim_max), breaks = my_breaks, expand = y_expansion)
          
        # Labels
        final_title <- if(nchar(in_title) > 0) in_title else paste("Comparison of", p_outcome, "by", p_group)
        if(!isTRUE(input$show_plot_title)) final_title <- NULL
        final_x <- if(nchar(in_x) > 0) in_x else p_group
        final_y <- if(nchar(in_y) > 0) in_y else p_outcome
        
        p <- p + labs(title = final_title, caption = caption_txt, x = final_x, y = final_y) +
           theme(legend.position = "none") +
           scale_x_discrete(expand = x_expansion) 
           
        # Significance Lines (ggsignif)
        target_pw <- res$plot_signif_data
        if(is.null(target_pw)) target_pw <- res$pairwise_table
        
        if(isTRUE(input$show_signif) && !is.null(target_pw)) {
           pw <- target_pw
           p_to_num <- function(x) {
              if(grepl("<", x)) return(0.0009)
              as.numeric(x)
           }
           
           sig_rows <- c()
           for(r in 1:nrow(pw)) {
              val <- pw[r, "Adjusted P-value"]
              num_val <- p_to_num(val)
              if(!is.na(num_val) && num_val < 0.05) sig_rows <- c(sig_rows, r)
           }
           
           if(length(sig_rows) > 0) {
              pw_sig <- pw[sig_rows, ]
              comps <- list()
              annotations <- c()
              
              for(r in 1:nrow(pw_sig)) {
                  g1 <- as.character(pw_sig[r, 1])
                  g2 <- as.character(pw_sig[r, 2])
                  if(g1 %in% levels(x) && g2 %in% levels(x)) {
                     comps[[length(comps)+1]] <- c(g1, g2)
                     pval_num <- p_to_num(pw_sig[r, "Adjusted P-value"])
                     sf <- input$signif_format %||% "p.value"
                     if(sf == "stars") {
                        lab <- case_when(pval_num < 0.001 ~ "***", pval_num < 0.01 ~ "**", pval_num < 0.05 ~ "*", TRUE ~ "ns")
                     } else {
                        # Use capital P as requested
                        raw_val <- pw_sig[r, "Adjusted P-value"]
                        if(is.numeric(raw_val)) {
                           p_txt <- if(raw_val < 0.001) "< 0.001" else paste0("=", formatC(raw_val, format="f", digits=3))
                        } else {
                           p_txt <- if(grepl("<", raw_val)) raw_val else paste0("=", raw_val)
                        }
                        lab <- paste0("P", p_txt)
                     }
                     annotations <- c(annotations, lab)
                  }
              }
              
              if(length(comps) > 0) {
                 # RAINBOW STACKING LOGIC
                 # 1. Get Factor Levels to determine index
                 lvl_order <- levels(df_plot[[p_group]])
                 
                 # 2. Calculate Distance for each comparison
                 comp_distances <- sapply(comps, function(pair) {
                    idx1 <- which(lvl_order == pair[1])
                    idx2 <- which(lvl_order == pair[2])
                    abs(idx1 - idx2)
                 })
                 
                 # 3. Sort Comparisons and Annotations (Shortest Distance First)
                 ord <- order(comp_distances)
                 comps_sorted <- comps[ord]
                 annots_sorted <- annotations[ord]
                 
                 # 4. Calculate Stacked Y Positions
                 # Base Start (Global max + 5% buffer)
                 y_start <- global_max * 1.05
                 if(y_start == 0) y_start <- 1 # Safety
                 
                 
                 # Step size: Increase to 15% of range for clearance
                 range_y <- global_max - min(df_plot[[p_outcome]], na.rm=TRUE)
                 y_step <- range_y * 0.30 
                 if(y_step == 0) y_step <- 0.5
                 
                 y_pos_sorted <- seq(from = y_start, by = y_step, length.out = length(comps_sorted))
                 
                 # Manual significance brackets (bypass ggsignif stat conflicts)
                 tip_len <- input$signif_tip_length %||% 0.05
                 txt_size <- input$signif_font_size %||% 5
                 sig_range <- max(df_plot[[p_outcome]], na.rm=TRUE) - min(df_plot[[p_outcome]], na.rm=TRUE)
                 if(sig_range == 0) sig_range <- 1
                 tip_h <- sig_range * tip_len
                 
                 for(si in seq_along(comps_sorted)) {
                    xi1 <- which(lvl_order == comps_sorted[[si]][1])
                    xi2 <- which(lvl_order == comps_sorted[[si]][2])
                    yp <- y_pos_sorted[si]
                    
                    # Bracket: left tip, horizontal bar, right tip
                    seg_df <- data.frame(
                       x = c(xi1, xi1, xi2),
                       xend = c(xi1, xi2, xi2),
                       y = c(yp - tip_h, yp, yp - tip_h),
                       yend = c(yp, yp, yp)
                    )
                    lab_df <- data.frame(x = (xi1 + xi2) / 2, y = yp, label = annots_sorted[si])
                    
                    p <- p + geom_segment(data = seg_df, aes(x = x, xend = xend, y = y, yend = yend),
                                          inherit.aes = FALSE, linewidth = sw) +
                             geom_text(data = lab_df, aes(x = x, y = y, label = label),
                                       inherit.aes = FALSE, vjust = -0.3, size = txt_size)
                 }
              }
           }
        }
      }
      
    } else {
      # -------------------------------------------------------------
      # 4. PAIRED ANALYSIS (Standardized)
      # SNAPSHOT
      p_paired <- res$params$paired_vars
      
      # Point Size and Stroke Width
      ps <- input$point_size %||% 1.5
      sw <- input$stroke_width %||% 0.5

      # Reshape Wide to Long for Plotting
      sub_df <- df[, p_paired]
      sub_df <- na.omit(sub_df)
      lvls <- p_paired 
      
      # Respect Group Order for Paired Analysis if set
      if(!is.null(input$viz_group_order) && length(input$viz_group_order) > 0) {
          valid_cols <- intersect(input$viz_group_order, lvls)
          # Only reorder if valid intersection
          if(length(valid_cols) > 0) lvls <- valid_cols
      }
      
      long_df <- tryCatch({
         sub_df %>%
            tidyr::pivot_longer(
               cols = dplyr::all_of(p_paired),
               names_to = "Group",
               values_to = "Value"
            )
      }, error = function(e) stack(sub_df))
      
      # Ensure factor levels
      long_df$Group <- factor(long_df$Group, levels = lvls)
      
      # Filter if needed (based on order)
      long_df <- long_df[long_df$Group %in% lvls, ]
      
      if(res$mode == "paired_categorical") {
         # Paired Categorical (Stacked Bar)
         p <- ggplot(long_df, aes(x = Group, fill = as.factor(Value))) +
           geom_bar(position = "fill", color=bar_border, size=sw) +
           scale_y_continuous(labels = scales::percent, expand = y_expansion) +
           my_theme +
           get_safe_scale_fill(in_palette, levels(as.factor(long_df$Value)))
           
         # Labels
         final_title <- if(nchar(in_title) > 0) in_title else "Paired Proportions (Changes)"
         final_x <- if(nchar(in_x) > 0) in_x else "Group"
         final_y <- if(nchar(in_y) > 0) in_y else "Proportion"

         p <- p + labs(title = final_title, subtitle = paste(res$test, "\n(Marginal Homogeneity Check)"), x = final_x, y = final_y, fill = "Outcome") +
             scale_x_discrete(expand = x_expansion)
           
      } else {
         # Paired Numeric (Bar or Boxplot)
         # Using logic similar to Independent Analysis
         is_parametric <- isTRUE(res$is_normal)
         
         if(!is.null(input$force_plot_type) && input$force_plot_type != "auto") {
            is_parametric <- (input$force_plot_type == "bar")
         }
         # SAFEGUARD
         cm <- input$color_mapping %||% "group"
         is_single_color <- (cm == "single")
         
         # Define Color Scale
         g_lvls <- levels(long_df$Group)
         fill_scale <- NULL
         
         if(!is_single_color) {
            if(isTRUE(input$use_palette)) {
               fill_scale <- get_safe_scale_fill(in_palette, g_lvls)
            } else {
               # Manual Color
               custom_cols <- sapply(seq_along(g_lvls), function(i) {
                   input[[paste0("col_", i)]] %||% "#999999"
               })
               fill_scale <- scale_fill_manual(values = custom_cols)
            }
         }
         
         # Base Aes
         p_aes <- aes(x = Group, y = Value)
         if(!is_single_color) p_aes <- aes(x = Group, y = Value, fill = Group)
         
         # Global Max for Significance Lines
         global_max <- max(long_df$Value, na.rm=TRUE)
         
         if(is_parametric) {
             # PARAMETRIC: Bar Chart with Mean + SD
             summ_df <- long_df %>%
                group_by(Group) %>%
                summarise(
                   Mean = mean(Value),
                   SD = sd(Value)
                )
             
             # Update global max
             global_max <- max(summ_df$Mean + summ_df$SD, na.rm=TRUE)
             
             p_aes_summ <- aes(x = Group, y = Mean)
             if(!is_single_color) p_aes_summ <- aes(x = Group, y = Mean, fill = Group)
             
             # Start with Error Bars
             p <- ggplot(summ_df, p_aes_summ) + 
                geom_errorbar(aes(
                   ymin = case_when(Mean >= 0 ~ Mean, TRUE ~ Mean - SD),
                   ymax = case_when(Mean >= 0 ~ Mean + SD, TRUE ~ Mean)
                ), width = 0.2, linewidth=sw)
                
             # Geom Bar
             if(is_single_color) {
                p <- p + geom_bar(stat = "identity", color=bar_border, width=0.7, fill=in_solid, linewidth=sw)
             } else {
                p <- p + geom_bar(stat = "identity", color=bar_border, width=0.7, linewidth=sw) + fill_scale
             }
             
             p <- p + my_theme
             
         } else {
             # NON-PARAMETRIC: Boxplot
             p <- ggplot(long_df, p_aes)
             if(isTRUE(input$show_boxplot_caps)) {
                p <- p + stat_boxplot(geom = "errorbar", width = 0.2, color = bar_border, linewidth = sw)
             }
             
             if(is_single_color) {
                p <- p + geom_boxplot(outlier.shape = NA, fill=in_solid, color=bar_border, linewidth=sw)
             } else {
                p <- p + geom_boxplot(outlier.shape = NA, color=bar_border, linewidth=sw) + fill_scale
             }
             p <- p + my_theme
         }
         
         if(isTRUE(input$show_jitter)) {
            p <- p + geom_jitter(width = 0.2, alpha = 0.3, size=ps)
         }

         # ---------------------------
         # Y-AXIS & LABEL LOGIC
         # ---------------------------
         data_min <- min(long_df$Value, na.rm=TRUE)
         data_max <- max(long_df$Value, na.rm=TRUE)
         
         if(data_min < 0 && data_max > 0) {
            p <- p + geom_hline(yintercept=0, linetype="dashed", color="gray50", linewidth=sw)
         }
         
         if(isTRUE(input$add_ref_line) && !is.null(input$ref_line_val) && !is.na(input$ref_line_val)) {
            p <- p + geom_hline(yintercept = input$ref_line_val, linetype = "dashed", color = "black", linewidth = sw)
         }
         
         # Limits
         lim_min <- if(data_min >= 0) 0 else NA   
         if(!is.null(input$y_min) && !is.na(input$y_min)) lim_min <- input$y_min
         lim_max <- if(!is.null(input$y_max) && !is.na(input$y_max)) input$y_max else NA
         
         my_breaks <- waiver()
         if(!is.null(input$y_step) && !is.na(input$y_step)) {
            b_start <- if(!is.na(lim_min)) lim_min else floor(data_min)
            b_end   <- if(!is.na(lim_max)) lim_max else ceiling(data_max * 1.5)
            my_breaks <- seq(from = b_start, to = b_end, by = input$y_step)
         }
         
         p <- p + scale_y_continuous(limits = c(lim_min, lim_max), breaks = my_breaks, expand = y_expansion)
         
         # Labels
         final_title <- if(nchar(in_title) > 0) in_title else "Paired Comparison"
         final_x <- if(nchar(in_x) > 0) in_x else "Group"
         final_y <- if(nchar(in_y) > 0) in_y else "Value"
         if(!isTRUE(input$show_plot_title)) final_title <- NULL
           
         p <- p + labs(title = final_title, x = final_x, y = final_y, caption = caption_txt) +
            theme(legend.position = "none") +
            scale_x_discrete(expand = x_expansion) 
            
         # ---------------------------
         # SIGNIFICANCE LINES (Rainbow Stacked)
         # ---------------------------
         target_pw <- res$plot_signif_data
         
         if(isTRUE(input$show_signif) && !is.null(target_pw)) {
            pw <- target_pw
            
            # Helper
            p_to_num <- function(x) {
                if(grepl("<", x)) return(0.0009)
                as.numeric(x)
            }
            
            sig_rows <- c()
            for(r in 1:nrow(pw)) {
               val <- pw[r, "Adjusted P-value"]
               num_val <- p_to_num(val)
               if(!is.na(num_val) && num_val < 0.05) sig_rows <- c(sig_rows, r)
            }
            
            if(length(sig_rows) > 0) {
               pw_sig <- pw[sig_rows, ]
               comps <- list()
               annotations <- c()
               
               for(r in 1:nrow(pw_sig)) {
                   g1 <- as.character(pw_sig[r, 1])
                   g2 <- as.character(pw_sig[r, 2])
                   
                   # Since we forced long_df$Group levels = input$viz_group_order or paired_vars
                   # We check against current levels
                   if(g1 %in% levels(long_df$Group) && g2 %in% levels(long_df$Group)) {
                      comps[[length(comps)+1]] <- c(g1, g2)
                      
                      pval_num <- p_to_num(pw_sig[r, "Adjusted P-value"])
                      sf <- input$signif_format %||% "p.value"
                      if(sf == "stars") {
                         lab <- case_when(pval_num < 0.001 ~ "***", pval_num < 0.01 ~ "**", pval_num < 0.05 ~ "*", TRUE ~ "ns")
                      } else {
                         raw_val <- pw_sig[r, "Adjusted P-value"]
                         if(is.numeric(raw_val)) {
                            p_txt <- if(raw_val < 0.001) "< 0.001" else paste0("=", formatC(raw_val, format="f", digits=3))
                         } else {
                            p_txt <- if(grepl("<", raw_val)) raw_val else paste0("=", raw_val)
                         }
                         lab <- paste0("P", p_txt)
                      }
                      annotations <- c(annotations, lab)
                   }
               }
               
               if(length(comps) > 0) {
                  # Sort by distance
                  lvl_order <- levels(long_df$Group)
                  comp_distances <- sapply(comps, function(pair) {
                     idx1 <- which(lvl_order == pair[1])
                     idx2 <- which(lvl_order == pair[2])
                     abs(idx1 - idx2)
                  })
                  
                  ord <- order(comp_distances)
                  comps_sorted <- comps[ord]
                  annots_sorted <- annotations[ord]
                  
                  # Stack Y
                  y_start <- global_max * 1.05
                  if(y_start == 0) y_start <- 1 # Safety
                  range_y <- global_max - min(long_df$Value, na.rm=TRUE)
                  y_step <- range_y * 0.30 
                  if(y_step == 0) y_step <- 0.5
                  
                  y_pos_sorted <- seq(from = y_start, by = y_step, length.out = length(comps_sorted))
                  
                  # Manual significance brackets (bypass ggsignif stat conflicts)
                  tip_len <- input$signif_tip_length %||% 0.05
                  txt_size <- input$signif_font_size %||% 5
                  sig_range_p <- max(long_df$Value, na.rm=TRUE) - min(long_df$Value, na.rm=TRUE)
                  if(sig_range_p == 0) sig_range_p <- 1
                  tip_h <- sig_range_p * tip_len
                  
                  for(si in seq_along(comps_sorted)) {
                     xi1 <- which(lvl_order == comps_sorted[[si]][1])
                     xi2 <- which(lvl_order == comps_sorted[[si]][2])
                     yp <- y_pos_sorted[si]
                     
                     seg_df <- data.frame(
                        x = c(xi1, xi1, xi2),
                        xend = c(xi1, xi2, xi2),
                        y = c(yp - tip_h, yp, yp - tip_h),
                        yend = c(yp, yp, yp)
                     )
                     lab_df <- data.frame(x = (xi1 + xi2) / 2, y = yp, label = annots_sorted[si])
                     
                     p <- p + geom_segment(data = seg_df, aes(x = x, xend = xend, y = y, yend = yend),
                                           inherit.aes = FALSE, linewidth = sw) +
                              geom_text(data = lab_df, aes(x = x, y = y, label = label),
                                        inherit.aes = FALSE, vjust = -0.3, size = txt_size)
                  }
               }
            }
         }
       }
    }
    return(p)
  })

  # Render Plot
  output$smart_plot <- renderPlot({
    final_plot()
  })
  
  # Download Handlers
  output$dl_pdf <- downloadHandler(
    filename = function() { paste("plot", Sys.Date(), ".pdf", sep="") },
    content = function(file) {
      w_in <- 8
      h_in <- 6
      dpi_val <- 300
      if(isTRUE(input$use_fixed_size)) {
         w_in <- (input$plot_width %||% 800) / 96
         h_in <- (input$plot_height %||% 600) / 96
         dpi_val <- input$export_dpi %||% 300
      }
      ggsave(file, plot = final_plot(), device = "pdf", width = w_in, height = h_in, units = "in", dpi = dpi_val)
    }
  )
  
  output$dl_svg <- downloadHandler(
    filename = function() { paste("plot", Sys.Date(), ".svg", sep="") },
    content = function(file) {
      w_in <- 8
      h_in <- 6
      dpi_val <- 300
      if(isTRUE(input$use_fixed_size)) {
         w_in <- (input$plot_width %||% 800) / 96
         h_in <- (input$plot_height %||% 600) / 96
         dpi_val <- input$export_dpi %||% 300
      }
      ggsave(file, plot = final_plot(), device = "svg", width = w_in, height = h_in, units = "in", dpi = dpi_val)
    }
  )
  
  output$dl_jpeg <- downloadHandler(
    filename = function() { paste("plot", Sys.Date(), ".jpeg", sep="") },
    content = function(file) {
      w_in <- 8
      h_in <- 6
      dpi_val <- 300
      if(isTRUE(input$use_fixed_size)) {
         w_in <- (input$plot_width %||% 800) / 96
         h_in <- (input$plot_height %||% 600) / 96
         dpi_val <- input$export_dpi %||% 300
      }
      ggsave(file, plot = final_plot(), device = "jpeg", width = w_in, height = h_in, units = "in", dpi = dpi_val)
    }
  )
}

# Run the App
shinyApp(ui, server)