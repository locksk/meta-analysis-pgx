# setwd("C:/Users/siobh/OneDrive - Cardiff University/phd/PhD/Y2/pgx-smd/PGx Effect Sizes/app/pgx_app/pgx_shiny/R")  
# to install waffle first:
# remotes::install_github("wch/extrafont")
# devtools::install_github("wch/Rttf2pt1")
# remotes::install_github("hrbrmstr/hrbrthemes")
# remotes::install_github("hrbrmstr/waffle")
# remotes::install_github("dreamRs/shinypop")

#for orchard
#install.packages("pacman")
#pacman::p_load(devtools, tidyverse, metafor, patchwork, R.rsp, emmeans)
#devtools::install_github("daniel1noble/orchaRd", force = TRUE)


library(shiny)
library(shinyWidgets)
library(DT)
library(countrycode)
library(ggbeeswarm)
library(stringr)
library(leaflet)
library(maps)
library(sf)
library(dplyr)
library(ggplot2)
library(plotly)
library(bslib)
library(pwr)
library(gghighlight)
library(ggpubr)
library(shinycssloaders)
library(tidyr)
library(metafor)
library(clubSandwich)
library(orchaRd)
library(dplyr)
library(tidytext)
library(waffle)
library(hrbrthemes)
library(Cairo)
library(RColorBrewer)
library(paletteer)
library(shinyglide)
library(shinypop)
library(gsubfn)
library(meta)
library(flexmix)
library(gridExtra)


options(shiny.usecairo=T)

# load functions
source("functions.R")

# Define UI for application 
ui <- fluidPage(
  theme = bs_theme(
    bg = "#F8F5E6",
    fg = "#A41623",
    primary = "#474044",
    base_font = font_google("Roboto"),
    code_font = font_google("Roboto Mono"),
    heading_font = font_google("Abril Fatface"),
    font_scale = 1.2
  ), 
  shinyjs::useShinyjs(),
  br(),
 # titlePanel(HTML('<img src="CNGG CU.png"/ height="70"> '), "PGx Effect Size Explorer for Psychiatric Drugs."),
  titlePanel(title = span(img(src = "CNGG CU.png", height = 70), " PGx Effect Size Explorer for Psychiatric Drugs."),   windowTitle = "PGx Effect Size Explorer"), 
  windowTitle = "PGx Effect Size Explorer",
  tags$style(HTML("
    .shiny-input-container { margin-bottom: 10px; }
    .plot-title { font-size: 16px; font-weight: bold; margin-bottom: 15px; }
    .dropdown-menu {
        padding: 5px;
    }
    .bs-select-all:hover, .bs-deselect-all:hover {
      background-color: #e9ecef;
    }
    .bs-select-all, .bs-deselect-all {
        text-align: left; 
        padding: 8px 12px; 
        width: 100%;
        font-size: 14px;
        border: 1px solid #ccc;
        background-color: #f8f9fa;
        color: #333; 
        margin: 2px 0;
    }
      /* this will affect only the pre elements under the class myclass */
      .myclass pre {
        color: #A41623;
        font-family: Roboto Mono; 
        background-color: #F8F5E6;
        border-style: hidden;
        font-weight: 550;
      }
      
      .myclass2 pre {
        color: #A41623;
        font-family: Roboto Mono; 
        background-color: #F8F5E6;
        border-style: hidden;
        white-space: normal; 
        font-weight: 550;
      }
  ")),
  br(),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filters"),
      h6("Select analyses to include:"),
      # overall drug type
      radioButtons(
        inputId = "drug_type_select",
        label = "Drug type:",
        choices = c("All", "Antipsychotic", "Antidepressant"),
        selected = "All"
      ),
      br(),
      # Drug selection
      pickerInput(
        inputId = "drug_select",
        label = "Select drug(s):",
        choices = NULL, # Populated server-side
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ),
      br(),
      # enzyme select
      pickerInput(
        inputId = "enzyme_select",
        label = "Select enzyme(s):",
        choices = NULL, # Populated server-side
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          size = 10,            
          style = "btn-default", 
          `dropdown-align-right` = TRUE  
        ),
        width = "100%"
      ),
      br(),
      h6("Select outcome information:"),
      # Proximal/Distal filter
      radioButtons(
        inputId = "outcome_type_select",
        label = "Outcome type:",
        choices = c("All", "Proximal", "Distal"),
        selected = "All"
      ),
      br(),
      # Keyword search
      pickerInput(
        inputId = "outcome_select",
        label = "Select outcome(s):",
        choices = NULL, # Populated server-side
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ),
      br(),
      h6("Select effect information:"),
      radioButtons("effect_type", "Effect size type:",
                   choices = c("Absolute SMD" = "absmd", "SMD" = "smd"),
                   selected = "absmd"),
      br(),
      h6("Select analysis information:"),
      # Sample size slider
      sliderInput(
        inputId = "sample_size",
        label = "Sample size range:",
        min = 0, max = 10000,  # Updated server-side
        step = 10, ticks = F, 
        value = c(0, 10000)
      ),
      br(),
      #  Effect size range
      sliderInput(
        inputId = "effect_size",
        label = "Effect size range:",
        min = -15, max = 15, # Updated server-side
        value = c(-15, 15),
        step = 1
      ),
      br(),
      div(style="display:inline-block;width:100%;text-align: center;",actionButton("do", label = "Update data", icon = icon("bolt-lightning"))),

    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        type = "tabs", id = "tabswitch", 
        tabPanel("About",   
                 br(),
                 h4("About."),
                 p("This shiny dashboard allows you to explore the effect sizes calculated as part of 'Near, far, wherever you are: Phenotype-related variation in pharmacogenomic effect sizes across the psychiatric drug literature' (doi: 10.1101/2025.09.23.25336442)."),
                 br(),
                 h5("Visualisations."),
                 p("The sidebar allows you to filter the dataset by drug, enzyme, and outcome. Outcomes may be filtered generally (i.e., 'proximal' vs 'distal') or through a free-text search if interested in specific outcomes. Either the absolute Standardised Mean Difference (SMD) may be used to filter data, and in the meta analysis, or the untransformed SMD. Clicking the update button applies these changes to the dataset."),
                 p("The second tab gives a summary of the effect sizes included in the working dataset. It allows you to visualise the distribution of effect sizes with a density plot, alongside looking at the make up of the dataset through waffle plots (i.e., the different enzymes, drugs, and outcomes included. This tab also has a map which displays the number of effect sizes coming from each country."),
                 br(),
                 h5("Analyses."),
                 p("The third tab allows you to run a correlated and heirarchical effects model on the filtered data. It displays the model structure for clarity and a summary of results, which are also displayed as an Orchard plot. Funnel plots are shown to allow an assessment of asymmetry in effect sizes."),
                 p("The fourth tab allows you to calculate the minimum per-group sample size at a given effect size."),
                 br(),
                 h5("Database."),
                 p("Finally, the last tab shows the selected input data. The table is interactive, and the data can be downloaded as a csv. References for studies included can also be downloaded in BibTeX format."),
                 br(),
                 h5("Acknowledgements."),
                 p("This dashboard was built by Jude Hutton and Siobhan Lock."),
                 # a(href="https://dataviz.shef.ac.uk/blog/05/02/2021/Shiny-Template#use-templates", "This Shiny App was built using code from Dataviz.shef"),
                 br(),
                 br(),
                 br(),
        ),
        # Visualisation Tab
        tabPanel(br(),
                 title = "Info",
                 h4("Info."),
                 p("This tab shows descriptive information about the effect sizes in the working dataset."),
                 p("It also shows the distribution of those effect sizes, and the make up of the dataset, in terms of the different drugs, enzymes, and outcomes analysed."),
                 br(),
                 p("A density plot is used to visualise the distribution of effect sizes in the working dataset. The data is split by outcome type, with separate peaks for proximal and distal effects. Both the absolute SMD and untransformed SMD can be shown dependent on the option selected in the side bar."),
                 p("Waffle plots are used to visualise the drug, enzyme, and outcome variables in the dataset. Each square represents 1% of the total effect sizes, with squares colour-coded by variables. For example, in the total sample there are 7 blocks representative of CYP1A2, this means that 7% of all effect sizes in the total dataset look at pharmacogenomic variation in CYP1A2. Plots can be cycled through using the <next> and <back> buttons below the plot."),
                 p("Finally, the chlorpleth/map shows the number of effect sizes (as opposed to studies) coming from each country. Effect sizes from multiple countries are displayed in the caption beneath. Countries are coloured based on quartiles; where quartiles are not unique (as is the case when the dataset is heavily filtered and numbers of studies and effect sizes are low) then countries are coloured by number of effect sizes."),
                 br(),
                 h5("Effect Size Information."),
                 fluidRow(
                  column(width = 10,
                         div(class = "myclass2", verbatimTextOutput("data_info"))),
                  br(),
                  column(
                    width = 10,
                   # div(class = "plot-title", "Distribution of Effect Sizes"),
                    plotOutput("density_plot", height = "400px") %>%  withSpinner(image = "spin-trans.gif", caption = NULL)
                  ),
                  br(),
                  div(class = "plot-title", ""),
                  br(),
                  br(),
                  h5("Dataset Composition."),
                  column(
                    width = 10,
                    glide(
                      screen(
                        h5("Enzymes."),
                        plotOutput("waffle_enzymes", height = "400px") %>%  withSpinner(image = "spin-trans.gif", caption = NULL)
                      ),
                      screen(
                        h5("Outcomes."),
                        plotOutput("waffle_outcomes", height = "400px") %>%  withSpinner(image = "spin-trans.gif", caption = NULL)
                      ),
                      screen(
                        h5("Drugs."),
                        plotOutput("waffle_drugs", height = "400px") %>%  withSpinner(image = "spin-trans.gif", caption = NULL)
                      )
                    )
                  ),
                  div(class = "plot-title", ""),
                  br(),
                  h5("Effect Size Origin."),
                  column(width = 10,
                         leafletOutput("world_map", height = "600px") %>%  withSpinner(image = "spin-trans.gif", caption = NULL)),
                  br(),
                  div(class = "myclass2", verbatimTextOutput("map_info") %>% withSpinner(image = "spin-trans.gif", caption = NULL)),
                  br(),
                 ), 
                 br(),
                 br(),
                 br(),
        ),
        tabPanel(br(),
                 title = "Meta Analysis",
                 h4("Meta Analysis."),
                 p("This tab computes a simplified three level meta analysis based on the data selected in the sidebar menu.
                   The user can specify the type of meta analysis performed, namely wwhether to use a correlated and hierarchical effects model and whether to apply robust variance estimation. 
                   More information about these methods can be found by pressing the '?' links. 
                   Pressing the <Run Analysis> button will commence the analysis (note: this may take time in large or unfiltered datasets and must be clicked to update the meta analysis if additional filters are applied). 
                   A brief summary of the results is provided, however, more information (model call, full results output) can be toggled by pressing the <Details> button."),
                 p("An Orchard plot shows individual effect sizes and the pooled estimate from the meta analysis. Funnel plots are displayed below this to assess asymmetry."),
                 br(),
                 h5("Model Specification."), 
                 column(width = 11,
                   #    tags$style(HTML(".switch-inline {margin-right: 42px;}")),
                       materialSwitch(inputId = "che.id", label = "Use a Correlated and Hierarchical Effects Model?", value = TRUE, status = "danger"),
                       shiny::actionLink("che.help", label = "?"),
                       materialSwitch(inputId = "rve.id", label = "Apply Robust Variance Estimation?", value = TRUE, status = "danger"),
                       shiny::actionLink("rve.help", label = "?"),
                       br(), 
        
                       #htmlOutput("rve.id"), 
                     
                       actionButton("run", label = "Run Analysis", icon = icon("bolt-lightning"))),
                       
                 br(),
                 br(),
                 shinyjs::hidden(tags$div(id = "meta_res_group",
                 h5("Summary of Results."),
                 fluidRow(
                   column(
                    width = 10, 
                    # div(class = "plot-title", "Meta-analysis statistics"),
                   shinypop::use_notiflix_notify(position = "right-bottom", closeButton = TRUE),
                   div(class = "myclass2", verbatimTextOutput("tidymeta") %>% withSpinner(image = "spin-trans.gif", caption = NULL)),
                   actionButton("details", label = "Details", icon = icon("eye-low-vision"))),
                   ), 
                   br(),
                   column(
                   width = 7, 
                   shinyjs::hidden(tags$div(id = "details_group",
                   div(class = "myclass2", verbatimTextOutput("metacall") %>% withSpinner(image = "spin-trans.gif", caption = NULL)),
                   div(class = "myclass", verbatimTextOutput("metastats") %>% withSpinner(image = "spin-trans.gif", caption = NULL))
                   ))
                   ),
                   div(class = "plot-title", " "),
                   br(),
                   br(),
                   p("OrchaRd plot showing results of a simplified three-level meta analysis (random effect = 1 | Study ID / Effect ID) of Pharmacogenomic effect sizes (SMDs). Diamond reflects the point estimate for the overall effect of pharmacogenomic variation on outcomes, branches include 95% prediction intervals around this estimate (annotated on plot alongside robust 95% confidence intervals). Individual datapoints reflect individual effect sizes, scaled by a measure of precision (inverse of the square root of the sample size, N)."),
                   br(),
                   br(),
                   column(
                     width = 10,
                    # div(class = "plot-title", "Orchard Plot of Effect Sizes"),
                     plotOutput("orchard_plot_ma", height = "400px") %>% withSpinner(image = "spin-trans.gif", caption = NULL)
                    
                   ), 
                   br(),
                   br(),
                   div(class = "plot-title", " "),
                   br(),
                   br(),
                   h5("Assessing Asymmetry."),
                   p("Funnel plots used to visualise asymmetry in effect sizes, often interpreted as a marker of biases such as publication bias (/small study effects). Funnel plots show the effect size against a measure of precision; in the first plot against Standard Error as traditionally seen. Second, against inverse of the square root of N, as is suggested in cases where the effect size is the SMD (as SMD and SE are closely related)."),
                   br(),
                   br(),
                  column(
                   width = 10,
                   splitLayout(cellWidths = c("50%", "50%"), plotOutput("funnel_plot1"), plotOutput("funnel_plot2"))
                   ), 
                   br(),
                   br(),
                 ))
                 
                 #), 
                 #br(),
                 #br(),
        ),
        
        # Power Calculator Tab
        tabPanel(br(),
                 title = "Power Analysis",
                 h4("Power Analysis."),
                 fluidRow(
                   column(
                     width = 10,
                   #  div(class = "plot-title", "Power Curves"),
                     p("The below plot allows you to see how estimated sample size required to attain 80% power varies based on the expected effect size. Plot shows power curves at effect size of 0.1 - 0.9 (at 0.1 intervals) in grey. The highlighted power curve (red) is calculated based on user input. Note: calculations are done on the basis of a two-sample t-test and use the `pwr` R package (Champely, 2020)."),
                     numericInput("effect_size_p", "Expected effect size:", value = 0.3, min = 0, max = 1, step = 0.01), # want this to update server side
                     plotOutput("power_curve", height = "400px") %>%  withSpinner(image = "spin-trans.gif", caption = NULL),
                     #htmlOutput("sample_size_calculation"),
                     br(),
                     div(class = "myclass2", verbatimTextOutput("expected_sample")),
                     br(),
                     br(),
                   )
                 ),
        ),
        
        # Antonio's Bayesian Analysis 
        tabPanel(br(),
                 title = "Winner's Curse",
                 id = "inflation",
                 h4("Accounting for Winner's Curse (effect size exaggeration)"),
                 fluidRow(
                   column(
                     width = 10,
                     p("This tab uses the effect sizes in the working dataset to model a distribution of signal-to-noise ratios using z-scores.
                       This is then used as a prior to generate posterior estimates of β and SE which are adjusted for exaggeration. Original β and SE
                       can be input below, the dataset used to generate the prior can be adjusted using the sidebar (note: if additional filters are applied then the <update> button must be clicked to apply those changes to the working dataset). For more information, see van Zwet & Gelman (2022)."),
                         numericInput("unadj.b", "Original β:", value = NULL, min = -Inf, max = Inf, step = 0.00001),
                         numericInput("unadj.se", "Original SE:", value = NULL, min = -Inf, max = Inf, step = 0.00001),
                         actionButton("calc", label = "Calculate", icon = icon("bolt-lightning")),
                     shiny::actionLink("calc.help", label = "?"),
                     
                     br(),
                     br(),
                     shinyjs::hidden(tags$div(id = "inflation_group",
                                              h5("Summary of Results."),
                                              br(),
                                              div(class = "myclass2", verbatimTextOutput("adjusted.output")),
                                              plotOutput("distro.plots", height = "550px") %>%  withSpinner(image = "spin-trans.gif", caption = NULL),
                                              br(),
                                              br(),
                                              ))
                     )
                   ),
                 ),
        
        # Data Table Tab
        tabPanel(br(),
                 title = "Database",
                 h4("Database."),
                 fluidRow(
                 #  column(width = 10,
                #          verbatimTextOutput("data_info")),
                 #  br(),
                   column(
                     width = 11, 
                    # div(class = "table-title", "Database"), 
                     DTOutput("effect_table") %>%  withSpinner(image = "spin-trans.gif", caption = NULL),
                     br(),
                     downloadButton("download_data", "Download Filtered Data"),
                     downloadButton("download_bibtex", "Download BibTeX"),
                     br(),
                     br()
                     
                   )),
        ),
      )
    )
  )
)




server <- function(input, output, session) {
  
  observeEvent(input$che.help, {
    showModal(modalDialog(
      title = "CHE information",
      "Selecting a CHE (correlated and hierarchical) model uses an imputed variance-covariance matrix, calculated using metafor::vcalc(), instead of the extracted variances. 
       This allows correlations between sampling errors to be accounted for, assuming rho = 0.6. 
       While this helps to account for the dependencies between effects in the dataset, its a rough approximation given that a constant correlation is unlikely.
       
       Where possible, we recommend to use both CHE and RVE for the meta analysis. More information can be found in the metafor and clubSandwich documentation. 
      "
    ))
  })
  
  observeEvent(input$rve.help, {
    showModal(modalDialog(
      title = "RVE information",
      "Robust variance estimation (RVE) can be used to obtain cluster-robust standard errors based on the clubSandwich package. This is done using metafor::robust() 
       which implements this via a sandwich estimator and bias-reduced linearisation for small sample correction.
      
       Please note, that RVE does not work at very low sample/analysis sizes, and so has been disabled when  N = 1.
      
       Where posisible, we recommend to use both CHE and RVE for the meta analysis. More information can be found in the metafor and clubSandwich documentation."
    ))
  })
  
  observeEvent(input$calc.help, {
    showModal(modalDialog(
      title = "Inflation Information",
      "This function is restricted to filtered datasets with at least 150 effect sizes. This is because distributions might behave strangely when corpora are too small.
      If the <Calculate> button is disabled, try adding more studies to the dataset."
    ))
  })
  
  load("data/all_smds.RData")
  all_smd$index <- 1:nrow(all_smd)
  all_smd$url <- paste0("<a href='https://doi.org/",all_smd$doi,"' target='_blank'>",all_smd$pmid,"</a>")
  all_smd[sapply(all_smd, is.infinite)] <- NA
  
  all_smd <- all_smd %>% drop_na(SMD) 

  pal <- c("#6BD8BA", "#A8A8FF")
  
  my_palette = c("#355070",
                 "#6d597a",
                 "#b56576",
                 "#e56b6f",
                 "#eaac8b")
  
  cols_waf <- c("#FF6666", "#FF6699", "#FF9999", "#FF9977","#CC6600",
                "#FF9900", "#FFCC00", "#CCCF44","#66CC00", "#009936",
                "#339999", "#2299FC", "#33CCff", "#6699CC", "#66CCCF",
                "#11FCCF", "#88FFCC", "#88FFFF", "#99CCFF","#CCCCFF",
                "#FFCCFF",  "#CC99CC",  "#CC99FC", "#FF99FC", "#FF60CC", "#CC66DD")
  

  
  # Load the meta-analysis dataset
  data <- reactive({
    # Replace with your dataset path
    all_smd %>%
      mutate(
       # drug = as.factor(drug),
        enzyme = as.factor(enzyme),
        outcome_clean = as.factor(outcome_clean),
        enzyme_type = as.factor(enzyme_type),
        OutcomeType = factor(pdr, levels = c(1, 2), labels = c("Proximal", "Distal"))) 
    
  })
  
  
  
  # Dynamically update filter choices for drugs/enzymes
  observe({
    # Get the current drug and enzyme selections
    current_drug_type <- input$drug_type_select
    current_drug <- input$drug_select
    current_enzyme <- input$enzyme_select
    current_scale <- input$data_output
    current_outcome_type <- input$outcome_type_select
    current_outcome <- input$outcome_select
    
    #current_estimate <- meta.res()$beta[1]

     # Update drug choices based on selected drug category
    if (input$drug_type_select != "All") {
      # Filter enzymes based on the selected drug(s)
      available_drugs <- data() %>%
        filter(drug_type %in% current_drug_type) %>%
        pull(drug_id) %>%
        unique() %>%
        sort() %>%
        as.character()
    
      # Update drug selection box (only if the choices have changed)
      updatePickerInput(session, "drug_select", choices = available_drugs, selected = current_drug)
    } else {
      # If no drug is selected, show all drugs
      updatePickerInput(session, "drug_select", choices = levels(data()$drug_id), selected = current_drug)
    }
     
    # Update enzyme choices based on selected drug type
    if (input$drug_type_select != "All") {
      # Filter enzymes based on the selected drug type
      available_enzymes <- data() %>%
        filter(drug_type %in% current_drug_type) %>%
        pull(enzyme_type) %>%
        unique() %>%
        sort() %>%
        as.character()
      
      # Update enzyme selection box (only if the choices have changed)
      updatePickerInput(session, "enzyme_select", choices = available_enzymes, selected = current_enzyme)
    } else {
      # If no drug is selected, show all enzymes
      updatePickerInput(session, "enzyme_select", choices = levels(data()$enzyme_type), selected = current_enzyme)
    }
    
    # Update enzyme choices based on selected drug
    if (!is.null(current_drug)) {
      # Filter enzymes based on the selected drug(s)
      available_enzymes <- data() %>%
        filter(drug_id %in% current_drug) %>%
        pull(enzyme_type) %>%
        unique() %>%
        sort() %>%
        as.character()
      
      # Update enzyme selection box (only if the choices have changed)
      updatePickerInput(session, "enzyme_select", choices = available_enzymes, selected = current_enzyme)
    } else {
      # If no drug is selected, show all enzymes
      updatePickerInput(session, "enzyme_select", choices = levels(data()$enzyme_type), selected = current_enzyme)
    }
      
    
    # Update outcome choices based on selected outcome category
    if (input$outcome_type_select != "All") {
      # Filter enzymes based on the selected outcome type(s)
      available_outcomes <- data() %>%
        filter(rating %in% current_outcome_type) %>%
        pull(outcome_clean) %>%
        unique() %>%
        sort() %>%
        as.character()
      
      # Update outcome selection box (only if the choices have changed)
      updatePickerInput(session, "outcome_select", choices = available_outcomes, selected = current_outcome)
    } else {
      available_outcomes <- data() %>%
        pull(outcome_clean) %>%
        unique() %>%
        sort() #%>%
        #as.character()
      # If no outcome is selected, show all outcomes
      updatePickerInput(session, "outcome_select", choices = levels(data()$outcome_clean), selected = current_outcome)
    }
    
    
    
    # Update sample size slider
   updateSliderInput(session, "sample_size", max = max(data()$Total_N, na.rm = TRUE))
   
   # Update effect range slider based on type of effect size
   if (input$effect_type != "absmd") {
     # If SMD is selected then have min and max bounded by SMD values
     updateSliderInput(session, "effect_size",
                       min = floor(min(data()$SMD, na.rm = TRUE)),  max = ceiling(max(data()$SMD, na.rm = TRUE)),
                       value = c(-5, 5) )
   } else {
     # if absmd is selected, then have a max bounded by absmd
     updateSliderInput(session, "effect_size",
                       min = 0,  max = ceiling(max(data()$absmd, na.rm = TRUE)),
                       value = c(0, 5) )
   }
  })
  

  
  shiny::observeEvent(input$do, ignoreNULL = TRUE, {
    
    # Disable button.
    shinyjs::disable("do")
    
    # Update button.
    shiny::updateActionButton(session, inputId = "do", label = "", icon = shiny::icon("sync", class = "fa-spin"))
    
    # Add delay. 
    shinyjs::delay(ms = 100, expr = {
      
      filtered_data <- data() %>%
          filter(
            # Drug/enzyme filters
            if (input$drug_type_select != "All") drug_type == input$drug_type_select else TRUE,
            if (!is.null(input$drug_select)) drug_id %in% input$drug_select else TRUE,
            if (!is.null(input$enzyme_select)) enzyme_type %in% input$enzyme_select else TRUE,
           
            # Outcome type filter (pdr = 1 for proximal, 2 for distal)
            if (input$outcome_type_select != "All") OutcomeType == input$outcome_type_select else TRUE,
            if (!is.null(input$outcome_select)) outcome_clean %in% input$outcome_select else TRUE,
            
            #if (!is.null(input$outcome_select)) outcome_clean %in% input$outcome_select else TRUE,
            
            # Sample size and SMD range
            Total_N >= input$sample_size[1] & Total_N <= input$sample_size[2]#,
          ) %>%
          mutate(
            EffectSize = if (input$effect_type == "absmd") abs(SMD) else SMD,
          ) %>%
          filter(EffectSize >= input$effect_size[1] & EffectSize <= input$effect_size[2])
      
      Sys.sleep(time = 1)  
     
      # Re-enable button.
      shinyjs::enable("do")
      
      # Update button.
      shiny::updateActionButton(session, "do", label = "Update data", icon = shiny::icon("bolt-lightning"))
      
    })
    
  })
  
  
  filtered_data <-  eventReactive(input$do, ignoreNULL = FALSE, {
    data() %>%
      filter(
        # Outcome type filter (pdr = 1 for proximal, 2 for distal)
        if (input$outcome_type_select != "All") OutcomeType == input$outcome_type_select else TRUE,
        if (!is.null(input$outcome_select)) outcome_clean %in% input$outcome_select else TRUE,
        
        # Drug/enzyme filters
        if (input$drug_type_select != "All") drug_type == input$drug_type_select else TRUE,
        if (!is.null(input$drug_select)) drug_id %in% input$drug_select else TRUE,
        if (!is.null(input$enzyme_select)) enzyme_type %in% input$enzyme_select else TRUE,
        # Sample size and SMD range
        Total_N >= input$sample_size[1] & Total_N <= input$sample_size[2],
        # Keyword search in analysis
        # grepl(input$keyword_search, Analysis, ignore.case = TRUE)
      ) %>%
      mutate(
        EffectSize = if (input$effect_type == "absmd") abs(SMD) else SMD,
      ) %>%
      filter(EffectSize >= input$effect_size[1] & EffectSize <= input$effect_size[2])
    
  })
  
  
  # dataset information
  output$data_info <- renderText({
    ne <- nrow(filtered_data())
    ns <- nrow(filtered_data()[!duplicated(filtered_data()$Study),])
    np <- nrow(filter(filtered_data(), rating == "Proximal"))
    nd <- nrow(filter(filtered_data(), rating == "Distal"))
    
    m <- round(mean(filtered_data()$EffectSize),2)
    s <- round(sd(filtered_data()$EffectSize), 2)
    med <- round(median(filtered_data()$EffectSize), 2)
    iqr <- quantile(filtered_data()$EffectSize)
    effect1 <- paste0(m, " (", s, ")")
    effect2 <- paste0(med, " [", round(iqr[[2]],2), " - ", round(iqr[[4]],2), "]")
    print(paste0("There are ", ne, " effect sizes from ", ns, " studies in this dataset, of which ", np, " are proximal and ", nd, " are distal. \n The mean (SD) effect size of this dataset is ", effect1, ". \n The median [IQR] effect size of this dataset is ", effect2, "."))
  })
  
   output$effect_table <- renderDT({
    displayed_data <- filtered_data() %>%
      select(outcome_clean, rating, drug_id, enzyme, mp, url, SMD, SE, Var, CI_low, CI_up, Total_N) #%>%
    
    DT::datatable(
      displayed_data, escape = FALSE,
      options = list(pageLength = 10,
                     scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets = 5:12))),
      colnames = c("Outcome" = "outcome_clean", "Rating" = "rating" , "Drug" = "drug_id", "Enzyme" = "enzyme", "Metabolism Phenotype" = "mp", "PMID" = "url", "Lower 95% CI" = "CI_low", "Upper 95% CI" = "CI_up", "Total N" = "Total_N")  # Rename OutcomeType
    ) %>%
      formatRound(columns = c("SMD", "SE", "Var", "Lower 95% CI", "Upper 95% CI"), digits = 3)  # Round numeric columns
  })
  
  all_smd$ascale <- (1/sqrt(all_smd$Var))
  
  output$waffle_enzymes <- renderPlot({
    filtered_data() %>%
      count(enzyme) %>%
      mutate(percent = round_percent(n/sum(n)*100)) %>%
      ggplot(
        aes(fill = enzyme, values = percent)
      ) +
      geom_waffle(
        n_rows = 5,
        size = 1, 
        colour = "#F8F5E6",
        flip = FALSE) + 
      scale_fill_manual(values = paletteer_dynamic("cartography::turquoise.pal", length(unique(filtered_data()$enzyme))), name = "Enzyme") +
      coord_equal() +
      theme_ipsum_rc(grid="") +
      theme_enhance_waffle() + theme(legend.key.size = unit(5, "mm")) + theme(legend.position = "bottom") + theme(legend.text=element_text(size=14))+
      theme(plot.background=element_rect(fill='#F8F5E6'), rect=element_rect(fill='#F8F5E6', color='#F8F5E6'))
  }, bg="transparent")
  
  output$waffle_outcomes <- renderPlot({
    filtered_data() %>%
      count(outcome_clean) %>%
      mutate(percent = round_percent(n/sum(n)*100)) %>%
      ggplot(
        aes(fill = outcome_clean, values = percent)
      ) + 
      geom_waffle(
        n_rows = 5,
        size = 1, 
        colour = "#F8F5E6",
        flip = FALSE) +
      scale_fill_manual(values = paletteer_dynamic("cartography::harmo.pal", length(unique(filtered_data()$outcome_clean)), direction = -1), name = "Outcome") +
      coord_equal() +
      theme_ipsum_rc(grid="") +
      theme_enhance_waffle() + theme(legend.key.size = unit(5, "mm")) + theme(legend.position = "bottom") + theme(legend.text=element_text(size=14))+
      theme(plot.background=element_rect(fill='#F8F5E6'), rect=element_rect(fill='#F8F5E6', color='#F8F5E6'))
  }, bg="transparent")
  
  output$waffle_drugs <- renderPlot({
    filtered_data() %>%
      count(drug_id) %>%
      mutate(percent = round_percent(n/sum(n)*100)) %>%
      ggplot(
        aes(fill = drug_id, values = percent)
      ) +
      geom_waffle(
        n_rows = 5,
        size = 1, 
        colour = "#F8F5E6",
        flip = FALSE      ) + 
     # scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Set2"))(25), name = "Drugs") +
      scale_fill_manual(values = paletteer_c("grDevices::Purple-Orange", length(unique(filtered_data()$drug_id)), direction = -1), name = "Drugs") +
      
      coord_equal() +
      theme_ipsum_rc(grid="") +
      theme_enhance_waffle() + theme(legend.key.size = unit(5, "mm")) + theme(legend.position = "bottom") + theme(legend.text=element_text(size=14))+
      theme(plot.background=element_rect(fill='#F8F5E6'), rect=element_rect(fill='#F8F5E6', color='#F8F5E6'))
  }, bg="transparent")
  
  
  pal <- c("#6BD8BA", "#A8A8FF")
  
  output$density_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = EffectSize, fill = OutcomeType, colour = OutcomeType)) +
      geom_density(aes(y = after_stat(density)), alpha = 0.5) +
      labs(x = "Effect Size", y = "Density") +
      scale_colour_manual(values = c("Proximal" = pal[1], "Distal" = pal[2])) +
      scale_fill_manual(values = c("Proximal" = pal[1], "Distal" = pal[2])) +
      theme_classic() +
      theme(axis.title = element_text(colour = "#474044", size = 14),
            axis.text.x = element_text(colour = "#474044", size = 11),
            axis.text.y = element_text(colour = "#474044", size = 11),
            axis.ticks = element_line(colour = "#474044"),
            plot.title = element_text(colour = "#474044"),
            plot.background = element_rect(fill = "#F8F5E6"), 
            panel.background = element_rect(fill = "#F8F5E6"),
            legend.background = element_rect(fill="#FBF5E6"), 
            legend.key = element_rect(fill = "#F8F5E6"))
      
  } , bg="transparent")
  
  
  # warning pop up when opening meta-analysis tab with N = 1 study
  observeEvent(input$do,{
      if (length(unique(filtered_data()$Study)) == 1) {
      nx_notify_warning(text = "Only one study left after filtering - RVE disabled on the meta analysis tab.")}
    }
  )
 
# if N too small then switch rve to false and disable switch. 
observeEvent(input$do, {
    if (length(unique(filtered_data()$Study)) <= 1){
      updateMaterialSwitch(session = session, "rve.id", value = FALSE)
      shinyjs::disable("rve.id")
    } else {
      updateMaterialSwitch(session = session, "rve.id", value = TRUE)
      shinyjs::enable("rve.id")
  }
  })
  
  # fit model based on ui input (default = che w/ rve)
  meta.res <- eventReactive(input$run, ignoreNULL = TRUE, {
    if (input$che.id==TRUE) {
      V <- metafor::vcalc(vi = Var, cluster = Study, obs = es_id, rho = 0.6, data = filtered_data())
      tmp <- metafor::rma.mv(yi = EffectSize, V = V,
                                          random = list(~ 1 | Study/es_id),
                                          method = "REML", test = "t", dfs = 'contain',
                                          data = filtered_data(),
                                          sparse = TRUE)
      if (input$rve.id==TRUE){
        tmp <- robust(tmp, cluster = Study, adjust = "CR2", clubSandwich = T)
        tmp
      }
      else { 
        tmp
        }
      } else {
        tmp <- metafor::rma.mv(yi = EffectSize, V = Var,
                                          random = list(~ 1 | Study/es_id),
                                          method = "REML", test = "t", dfs = 'contain',
                                          data = filtered_data(),
                                          sparse = TRUE)
        if (input$rve.id==TRUE){
          tmp <- robust(tmp, cluster = Study, adjust = "CR2", clubSandwich = T)
          tmp
        }
        else { 
          tmp
        }
        }})
  

  # prep output 

  output$tidymeta <- renderText({
    bw <- format_p(meta.res()$sigma2[1])
    win <- format_p(meta.res()$sigma2[2])
    k <- meta.res()$k
    est <- round(meta.res()$beta[1], 3)
    se <- round(meta.res()$se[1], 3)
    or <- round(meta::smd2or(smd = meta.res()$beta[1], se.smd = meta.res()$se[1])$data$OR, 2)
    p <- format_p(meta.res()$pval)
    if (meta.res()$pval < 0.05){
      print(paste0("There is a significant effect of pharmacogenomic variation on the included outcomes (β = ", est, " (SE = ", se, "), p = ", p, "). This is equivalent to an OR = ", or ,  ". This analysis was based on ", k, " effect sizes. Between study variation = ", bw, ", and within-study variation = ", win, "."))
      } else {
        print(paste0("There is no significant effect of pharmacogenomic variation on the included outcomes (β = ", est, " (SE = ", se, "), p = ", p, "). This is equivalent to an OR = ", or, ". This analysis was based on ", k, " effect sizes. Between study variation = ", bw, ", and within-study variation = ", win, "."))
        }
  })
  
  observeEvent(input$run, {
      shinyjs::show("meta_res_group")
    })

  output$metastats <- renderPrint({meta.res()})
  output$metacall <- renderPrint({meta.res()$call})
  
  observeEvent(input$details, {
    shinyjs::toggle("details_group")
  })

  # prep for orchard plot
  meta.prep <- reactive({orchaRd::mod_results(model = meta.res(), group = "Study", N = "Total_N")})
  
  # prep orchard plot 
  output$orchard_plot_ma <- renderPlot({
    custom_orchard_plot(meta.prep(), group = "Study", xlab = "Standardised mean difference", N = "Total_N", 
                        k.pos = "right", trunk.size = 1.5, twig.size = 1, alpha = .2) +
      scale_color_manual(values = pal) + 
      scale_fill_manual(values = pal) + 
      annotate("text", label = paste0("95% Confidence Intervals [", round(meta.res()$ci.lb, 2)[1], ", ",  round(meta.res()$ci.ub, 2)[1], "]"), x = 0.95, y = (meta.res()$beta[1]+0.2)) + 
      annotate("text", label = paste0("95% Prediction Intervals [", round(meta.prep()$mod_table$lowerPR, 2)[1], ", ",  round(meta.prep()$mod_table$upperPR, 2)[1], "]"), x = 0.87, y = (meta.res()$beta[1]+0.2)) + 
      theme_classic() +
      theme(axis.title = element_text(colour = "#474044", size = 14),
            axis.text.x = element_text(colour = "#474044", size = 11),
            axis.text.y = element_text(colour = "#474044", size = 11, hjust = 0.5),
            axis.ticks = element_line(colour = "#474044"),
            plot.title = element_text(colour = "#474044"),
            plot.background = element_rect(fill = "#F8F5E6"), 
            panel.background = element_rect(fill = "#F8F5E6"),
            legend.background = element_rect(fill="#FBF5E6"), 
            legend.key = element_rect(fill = "#F8F5E6"))
            #panel.border = element_rect(colour = "#F8F5E6"))
  }, bg="transparent")
  
  output$funnel_plot1 <- renderPlot({
   with(filtered_data(), funnel(EffectSize, sqrt(filtered_data()$Var), yaxis = "sei", level=c(90, 95, 99), shade=c("white", "gray55", "gray75"),
                         legend=F, xlab = "Effect Size", ylab = "SE", back = "#F8F5E6"))
  }, bg="transparent")
  
  output$funnel_plot2 <- renderPlot({
    with(filtered_data(), funnel(EffectSize, 1/sqrt(filtered_data()$Total_N), yaxis = "sei", level=c(90, 95, 99), shade=c("white", "gray55", "gray75"),
                                 legend=F, xlab = "Effect Size", ylab = expression(1/sqrt(N)), back = "#F8F5E6"))  
  }, bg="transparent")
  
  # Download Filtered Data
  output$download_data <- downloadHandler(
    filename = function() { paste0("pgx_data_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(filtered_data(), file, row.names = FALSE) }
  )

  output$download_bibtex <- downloadHandler(
    filename = function() {
      paste0("pgx_studies_", Sys.Date(), ".bib")  # Name of the BibTeX file
    },
    content = function(file) {
      # get unique lines
      unique_dat <- filtered_data()[!duplicated(filtered_data()$Study),]
      # Generate BibTeX content
      bibtex_content <- generate_bibtex(unique_dat)
      
      # Write the BibTeX content to the file
      writeLines(bibtex_content, file)
    }
  )
  
  # new power mod
  output$power_curve <- renderPlot({
    req(input$effect_size_p)
    effect_range <- seq(0.1, 0.9, by = 0.1)
    check <- input$effect_size_p
    effect_sizes = c(check, effect_range)
    sample_sizes = seq(10, 500, 10)
    input_df <- crossing(effect_sizes, sample_sizes)

    power_data <- input_df %>%
      do(get_power(.)) 
    
    ggplot(power_data, aes(x = sample_sizes, y = power, group = effect_sizes)) +
      geom_line(color = "red", size = 1) +
      labs(title = paste("Expected power across Sample Sizes where Effect Size =", input$effect_size_p),
           x = "Sample size", y = "Power") +
      gghighlight(effect_sizes == check) +
      geom_hline(yintercept = 0.8) +
      theme_classic() +
      theme(axis.title = element_text(colour = "#474044", size = 14),
            axis.text.x = element_text(colour = "#474044", size = 11),
            axis.text.y = element_text(colour = "#474044", size = 11),
            axis.ticks = element_line(colour = "#474044"),
            plot.title = element_text(colour = "#474044"),
            plot.background = element_rect(fill = "#F8F5E6"), 
            panel.background = element_rect(fill = "#F8F5E6"),
            legend.background = element_rect(fill="#FBF5E6"), 
            legend.key = element_rect(fill = "#F8F5E6"))

  }, bg="transparent")
  
  output$expected_sample <- renderText({
    req(input$effect_size_p)
    effect_sizes <- input$effect_size_p
    sample_sizes = seq(10, 10000, 1)
    input_df <- crossing(effect_sizes, sample_sizes)
    
    samp <- input_df %>%
      do(get_power(.)) %>% 
      filter(power >= 0.8)
    
    n <- min(samp$sample_sizes)[1]
    
    print(paste0("Based on an effect size of ", effect_sizes, ", you should aim to include at least ", n, " participants per group to achieve 80% power (assuming a two-sample t-test)."))
  })

  # if N too small then switch rve to false and disable switch. 
   observeEvent(input$do, {
      if (nrow(filtered_data()) < 150){
        shinyjs::disable("calc")
      } else {
        shinyjs::enable("calc")
      }
    })
  
    all_inflation <- eventReactive(input$calc, ignoreNULL = TRUE, {
       bmod.fit=flexmix(z ~ 0|Study, data=filtered_data(), k = 2)
       for (i in 1:10){ # try few restarts 
         tmp.fit=flexmix(z ~ 0|Study, data=filtered_data(), k = 2)
         if (tmp.fit@logLik > bmod.fit@logLik) bmod.fit = tmp.fit}
   
       bmod.p=summary(bmod.fit)@comptab$prior
       bmod.ind=order(parameters(bmod.fit))
       bmod.sd1=parameters(bmod.fit)[bmod.ind[1]]
       bmod.sd2=parameters(bmod.fit)[bmod.ind[2]]
       bmod.p=bmod.p[bmod.ind[1]]
       bmod.tau1=sqrt(bmod.sd1^2 - 1)
       bmod.tau2=sqrt(bmod.sd2^2 - 1)
   
   # Z-score distribution parameters for outcomes
       bmod.pvec=c(bmod.p,1-bmod.p) # Mixture proportions
       bmod.tauvec=c(bmod.tau1,bmod.tau2) # Standard deviation of the mixture components
   
      # apply to new data
       ee.post=posterior(input$unadj.b,input$unadj.se,bmod.pvec,bmod.tauvec) # Posterior distribution of effects (regularized)
       ee.betahat=sum(ee.post$q * ee.post$pm)
       new.or <- exp(ee.betahat) # Posterior mean of the OR
       ee.shrinkage=input$unadj.b/ee.betahat # Shrinkage factor
       ee.ci2<-qmix(c(0.025,0.975),ee.post$q,ee.post$pm,ee.post$ps) 
       new.se <- ((ee.ci2[2] - ee.ci2[1]) / 3.92)
       new.ci <- exp(ee.ci2) # Posterior CI of the OR
       ee.psign <- pmix(0,ee.post$q,ee.post$pm,ee.post$ps) # Probability of sign error
       ee.prob <- 1 - ee.psign # Probability sign is correct
       
       # everything for the text results
       tmp <- data.frame(ee.b<-input$unadj.b,
                         ee.se<-input$unadj.se,
                         ind<-c(bmod.ind[1]),
                         sd1<-bmod.sd1[[1]],
                         sd2<-bmod.sd2[[1]],
                         p<-bmod.p[1],
                         tau1<-bmod.tau1[[1]],
                         tau2<-bmod.tau2[[1]],
                         shrinkage<-ee.shrinkage[1],
                         betahat<-ee.betahat[1],
                         new_se=new.se[1],
                         new_or=new.or[1],
                         new_ci.u=new.ci[2],
                         new_ci.l=new.ci[1],
                        # sd1=bmod.tauvec[[1]],
                       #  sd2=bmod.tauvec[[2]],
                         w1=bmod.pvec[1],
                         w2=bmod.pvec[2])
       
       colnames(tmp) <- c("b", "se", "ind", "sd1", "sd2", "p", "tau1", "tau2", "shrinkage", "betahat", "new_se", "new_or", "new_ci_u", "new_ci_l", "w1", "w2")
   
       tmp
       })
    
    observeEvent(input$calc, {
      shinyjs::show("inflation_group")
    })

    
      output$adjusted.output <- renderText({
        print(paste0("The original effect is β = ", round(all_inflation()$b,3), ", SE = ", round(all_inflation()$se,3), 
        ". From the filtered data (", length(unique(filtered_data()$Study)), " studies, ", nrow(filtered_data()), " effect sizes), a two-component mixture of zero-mean normal distributions was modelled.
        This distribution is a prior for the signal-to-noise ratio.  The mixture distribution had SD1 = ", round(all_inflation()$tau1,3), " and SD2 = ", round(all_inflation()$tau2,3),
        " with mixture weights W1 = ", round(all_inflation()$w1, 3), " and W2 = ", round(all_inflation()$w2, 3), ". Based on a shrinkage factor of ",
        round(all_inflation()$shrinkage, 3), ", the posterior estimate is β = ", round(all_inflation()$betahat, 3), ", SE = ", round(all_inflation()$new_se, 2), 
        ". This corresponds to OR = ", round(all_inflation()$new_or, 3), " (95% CI = ", round(all_inflation()$new_ci_l, 3), " – ", round(all_inflation()$new_ci_u, 3), ")."))
        })
    
  
  
       output$distro.plots <- renderPlot({
          # plot 1
          x=seq(-10,10,0.01)
          breaks=seq(-10,10,length=31)
          
          bmod.f=all_inflation()$p*dnorm(x,0,all_inflation()$sd1) + (1-all_inflation()$p)*dnorm(x,0,all_inflation()$sd2)
          bmod.df1=data.frame(z=filtered_data()$z)
          bmod.df2=data.frame(z=x,f=bmod.f)
          
          combined.p1=ggplot(bmod.df1,aes(x=z)) +
            geom_histogram(aes(y = after_stat(density)),breaks=breaks, colour="#c8656c", fill="#e4959a") +
            geom_line(data=bmod.df2,aes(x=z,y=f), linewidth = 1.5, linetype = "dotdash", colour = "#975b60")+
            labs(x='',y='') +
            coord_cartesian(xlim=c(-10,10))+ ylim(0,0.325)+
            ggtitle(paste0("z-scores (n=", nrow(filtered_data()),")")) + 
            xlab("Standardised Mean Difference") +
            ylab("Density") +
            theme_classic() +
            theme(axis.title = element_text(colour = "#474044", size = 14),
                  axis.text.x = element_text(colour = "#474044", size = 11),
                  axis.text.y = element_text(colour = "#474044", size = 11),
                  axis.ticks = element_line(colour = "#474044"),
                  plot.title = element_text(colour = "#474044", size = 14),
                  plot.background = element_rect(fill = "#F8F5E6"), 
                  panel.background = element_rect(fill = "#F8F5E6"),
                  legend.background = element_rect(fill="#FBF5E6"), 
                  legend.key = element_rect(fill = "#F8F5E6"))
          
          
      # Distribution of the SNR
          x=seq(-10,10,0.01)
          n=length(x)
          
          bmod.f2=all_inflation()$p*dnorm(x,0,all_inflation()$tau1) + (1-all_inflation()$p)*dnorm(x,0,all_inflation()$tau2)
          combined.df=data.frame(x=rep(x,1),f=c(bmod.f2))
          
          combined.p3=ggplot(combined.df,aes(x=x,y=f,color="#c8656c")) + geom_line(show=FALSE, linewidth = 1.5) +
            scale_colour_manual(values = "#c8656c") +
            labs(x='Standardised Mean Difference',y='Density') +
            ggtitle("Estimated Distribution of the SNR") + 
            theme_classic() +
            theme(axis.title = element_text(colour = "#474044", size = 14),
                  axis.text.x = element_text(colour = "#474044", size = 11),
                  axis.text.y = element_text(colour = "#474044", size = 11),
                  axis.ticks = element_line(colour = "#474044"),
                  plot.title = element_text(colour = "#474044", size = 14),
                  plot.background = element_rect(fill = "#F8F5E6"), 
                  panel.background = element_rect(fill = "#F8F5E6"),
                  legend.background = element_rect(fill="#FBF5E6"), 
                  legend.key = element_rect(fill = "#F8F5E6"))
          
          z=seq(-5,5,0.01)
          n2=length(z)
          
          bmod.shrink=sapply(z,function(z){shrink(b=z,s=1,p=c(all_inflation()$p,1-all_inflation()$p),tau=c(all_inflation()$tau1,all_inflation()$tau2))})
          combined.df2=data.frame(z=rep(z,1),shrink=c(bmod.shrink))
          
          combined.p4=ggplot(combined.df2,aes(x=z,y=shrink,color="#c8656c")) + geom_line(linewidth = 1.5) +
            scale_colour_manual(values = "#c8656c") +
            labs(x='z-score',y='Shrinkage Factor') +
            theme_minimal() +
            scale_y_continuous(breaks = seq(1,2.5,0.5),minor_breaks=seq(1,2.5,0.25),
                               limits = c(1,2.5)) +
            ggtitle("Shrinkage factor") +
            guides(color="none") +
            theme_classic() +
            theme(axis.title = element_text(colour = "#474044", size = 14),
                  axis.text.x = element_text(colour = "#474044", size = 11),
                  axis.text.y = element_text(colour = "#474044", size = 11),
                  axis.ticks = element_line(colour = "#474044"),
                  plot.title = element_text(colour = "#474044", size = 14),
                  plot.background = element_rect(fill = "#F8F5E6"), 
                  panel.background = element_rect(fill = "#F8F5E6"),
                  legend.background = element_rect(fill="#FBF5E6"), 
                  legend.key = element_rect(fill = "#F8F5E6"))
          
          grid.arrange(arrangeGrob(combined.p1, combined.p3), combined.p4, ncol = 2)
        }, bg="transparent")
  
  
  
  
  
  
  
  ### world map
  
  # create map data
  world_map <- maps::map("world", plot = FALSE, fill = TRUE)
  world_map <- st_as_sf(world_map)
  world_map <- st_transform(world_map, 4326)
    
  # Get study loc counts, split if country / continent
  tmp <- reactive({filtered_data() %>%
      filter(country != "Mixed") %>%
      group_by(country) %>%
      summarise(num_studies = n(), .groups = "drop")})
  
  
  study_counts <- reactive({tmp() %>%
      mutate(
        type = ifelse(
          country %in% unique(world_map$ID), 
          "country", 
          "continent"
        )
      ) 
  })
  
  country_data <- reactive({world_map %>%
      left_join(
        study_counts() %>% filter(type == "country"), 
        by = c("ID" = "country")
      )
  })
  
  # Define continent centroids for labeling
  continent_centroids <- data.frame(
    country = c("Europe", "Asia", "North America", "South America", "Africa", "Oceania"),
    lat = c(54.5260, 34.0479, 54.5260, -14.2350, -8.7832, -25.2744),
    lng = c(15.2551, 100.6197, -105.2551, -51.9253, 34.5085, 133.7751)
  )
  
  # Merge study counts with continent centroids
  continent_data <- reactive({study_counts() %>%
      filter(type == "continent") %>%
      left_join(continent_centroids, by = "country")
  })
  

  output$world_map <- renderLeaflet({
    if (length(unique(quantile(country_data()$num_studies, na.rm = TRUE))) == 5) { # this makes it so you do not get the breaks issue at low samples/ES
      leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap tiles
        # Add country polygons
        addPolygons(
          data = country_data(),
          fillColor = ~colorQuantile("YlOrRd", num_studies)(num_studies),  # Choropleth coloring
          fillOpacity = 0.7,
          color = "#BDBDC3",
          weight = 1,
          label = ~paste(ID, ": ", num_studies, "effect sizes"),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#666",
            fillOpacity = 0.7,
            bringToFront = TRUE
          )
        ) %>%
        # Add continent labels
        addLabelOnlyMarkers(
          data = continent_data(),
          lng = ~lng, lat = ~lat,
          label = ~paste(country, ": ", num_studies, "studies"),
          labelOptions = labelOptions(
            noHide = TRUE,  # Always show labels
            textOnly = TRUE,
            style = list(
              "font-size" = "14px",
              "font-weight" = "bold",
              "color" = "black"
            )
          )
        ) %>%
        setView(lng = 0, lat = 30, zoom = 2)
    } else {
      leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap tiles
        # Add country polygons
        addPolygons(
          data = country_data(),
          fillColor = ~colorNumeric("YlOrRd", domain = num_studies)(num_studies),  # Choropleth coloring
          fillOpacity = 0.7,
          color = "#BDBDC3",
          weight = 1,
          label = ~paste(ID, ": ", num_studies, "effect sizes"),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#666",
            fillOpacity = 0.7,
            bringToFront = TRUE
          )
        ) %>%
        # Add continent labels
        addLabelOnlyMarkers(
          data = continent_data(),
          lng = ~lng, lat = ~lat,
          label = ~paste(country, ": ", num_studies, "studies"),
          labelOptions = labelOptions(
            noHide = TRUE,  # Always show labels
            textOnly = TRUE,
            style = list(
              "font-size" = "14px",
              "font-weight" = "bold",
              "color" = "black"
            )
          )
        ) %>%
        setView(lng = 0, lat = 30, zoom = 2)
    }
    
  })
  
  output$map_info <- renderText({
    n <- nrow(filter(filtered_data(), country == "Mixed"))
    print(paste0("Overall there are ", n  ," effect sizes from studies conducted across a mixed sample or multiple countries that are not included on this map."))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
