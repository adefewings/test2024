
### Benthic Impacts Tool: RShiny Application developed for the Marine Stewardship Council on the Habitats Assessments project, 2018 - 2021 ###
### Authors: Jenny Shepperson, Lowri Evans, Jan Geert Hiddink, Bangor University ###
### Contact: Jenny Shepperson, j.shepperson@bangor.ac.uk or Jan Hiddink, j.hiddink@bangor.ac.uk ###

# Install Required Packages - only use for first use
# install.packages("shiny", "plyr", "dplyr", "rgdal", "leaflet", "raster", "rgeos", "ggplot2", "rhandsontable", 
# "stats", "gridExtra", "tidyverse", "shinyWidgets", "shinythemes",  "shinydashboard", "logitnorm", "tcltk", #
# "utils", "shinyBS", "tools", "rmarkdown", "SpatialEpi",  "mapview", "data.table", "webshot")

# Clear environment
rm(list = ls(all.names = TRUE))

#Load Required Packages
library(shiny)
library(plyr)
library(dplyr)
#library(rgdal)
library(leaflet) # for interactive maps
library(raster)
library(rgeos)
library(ggplot2)
library(rhandsontable)  # for interactive table input
library(stats) #  for weighted.mean() function
library(gridExtra) # grid.arrange() function to make combined ggplot for exporting
library(tidyverse) # for dplyr, ggplot, uncount() function
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(logitnorm)
library(tcltk)
library(utils)
library(shinyBS)
library(tools)
library(rmarkdown)
library(SpatialEpi)
library(mapview)
library(data.table)
rasterOptions(maxmemory = 5e+10)
library(webshot)
library(sf)

library(lwgeom)
library(units)

### This part sets up the user interface of the tool. 
### Explanatory text is entered here, along with defining user inputs, and presenting outputs.

ui <- fluidPage(theme = shinytheme("flatly"),
  navbarPage("Benthic Impacts Tool",
             
             tabPanel("1.Information",
                      setBackgroundColor("LightSkyBlue"),
                      
                      # This is the first tab of the tool. It will give users some brief background information about the tool, authors, literature about the methods etc.
                      # Users will also set their 'working directory' by specifying the folder pathway that holds all of their input data
                      
                      br(),
                      img(src='MSC_logo.png', align = "right", width=300),
                      img(src='Bangor_Logo_A1.png', align = "right", width=300),
                      h2("Predicting Benthic Impacts and Recovery after Fishing", style="font-weight:bold"),
                      br(),
                      br(),
                      h4("A tool to assess the benthic impacts caused by a fishery, a predicted recovery trajectory and produce an indicative MSC score (please see the User Manual for information on the scoring issue(s) that the indicative score should inform)."),
                      br(),
                      h5("Jennifer L. Shepperson, Lowri E. Evans, Jan G. Hiddink"),
                      h5("Bangor University, October 2021", style="font-weight:bold"),
                      br(),
                      br(),
                      
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(

                          h4("Start Here: Overview", style="font-weight:bold"),
                          p("Data on the spatial distribution of fishing activity and benthic habitats are used to estimate the relative 
                            benthic status of habitats currently, and a predicted recovery trajectory if fishing were to cease. Please refer to the associated User Manual",strong("(MSC Benthic Impacts Tool User Manual)"), "for further information and detail."),
                          br(),
                          h4("Contents of Each Tab", style="font-weight:bold"),
                          p("1. Information on the tool, and starting an assessment."),
                          p("2. Fishing effort data is input."),
                          p("3. A shapefile of habitat data, and a shapefile delineating the geographic extent of the assessment area are input, allowing an assessment grid to be generated."),
                          p("4. Fishing effort data is converted to swept area ratio (SAR) estimates that indicate how often a particular area 
                            of the seabed is fished per year."),
                          p("5. Depletion rates are generated, either using tool-provided default values or user inputted gear-specific values."),
                          p("6. Recovery rates are generated, either using a tool-provided default value, or estimated from the longevity of species present in each habitat type."),
                          p("7. The swept area ratio values are combined with the depletion and recovery rates 
                            to determine the relative benthic status (RBS) of the seabed in each habitat type. The RBS score indicates to what extent 
                            the benthic biota have been depleted [1 = no depletion, 0 = complete depletion]."),
                          p("8. The recovery trajectory for each habitat type is predicted from the current RBS, and the recovery rate and an indicative MSC score calculated."),
                          br()

                          

                        ),
                        
                        mainPanel(
                          p(strong("Read the information on the left hand side, then start with Task 1 and proceed through all 15 Tasks to complete the assessment."), style="color:red"),
                          
                          br(),
                          h4("TASK 1 / 15 - Is the data folder ready?", style="font-weight:bold"),
                          p("All data to be used within the tool is required to exist within a single folder on the user's computer. Please see the User Manual for further details."),
                          br(),
                          h4("TASK 2 / 15 - Start the assessment", style="font-weight:bold"),
                          br(),
                          p("We would like to know what type of users are using this tool, please choose from the dropdown list:"),
                          selectInput("type", "Type of User", c("Please Select", "Conformity Assessment Body", "Fishery Representative","NGO", "Government Body","Researcher","Tool Development", "Other")),
                          selectInput("use", "What will you use the tool to inform?", c("Please Select","MSC fishery assessment or audit","In-transition to MSC Assessment","Fishery Improvement Project","Fishery Management","Research", "Tool Development", "Other")),
                          
                          br(),
                          p("Disclaimer", style="font-weight:bold"),
                          p("This work is provided on a strictly as-is basis only, and no part of this work may be reproduced, stored or transmitted, in any form or by any means including 
                          photocopying and recording, except as expressly permitted under the aforementioned terms. 
                          This work is licensed under this ", a("Creative Commons BY-NC-ND", href = "https://creativecommons.org/licenses/by-nc-nd/4.0/legalcode"), ". By using this app we 
                            assume you have read these ", a("Terms & Conditions.", href = "https://creativecommons.org/licenses/by-nc-nd/4.0/legalcode")),
                          br(),
                          p("By clicking the below button to start the tool, you agree to the above disclaimer."),
                          
                          bsButton("input_data_pathway", label="Click to start", block = F, style = "danger"),
                          bsTooltip(id = "input_data_pathway", title = "Click here", placement = "right", trigger = "hover"),
                          
                          br(),
                          br(),
                          br(),
                          
                          textOutput("wd_text"),
                          tags$head(tags$style("#wd_text{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }")),
                          br(),
                          br(),
                          h4("Tool References", style="font-weight:bold"),
                          
                          p("Below is a list of the main scientific peer-reviewed literature this tool is based on:", style = "font-si22pt"),    
                          
                          p("Hiddink", em("et al"), "(2019) Assessing bottom-trawling impacts based on the longevity of benthic invertebrates. Journal of Applied Ecology, 56(5), 1075-1084. Open access article: https://doi.org/10.1111/1365-2664.13278", style = "font-family: 'times'; font-si22pt"),
                          
                          
                          p("Hiddink", em("et al"), "(2017) Global analysis of depletion and recovery of seabed biota following bottom trawling disturbance. Proceedings of the National Academy of Sciences, 114, 8301-8306. Open access article: https://doi.org/10.1073/pnas.1618858114", style = "font-family: 'times'; font-si22pt"),
                          
                          p("Pitcher", em("et al"), "(2017) Estimating the sustainability of towed fishing-gear impacts on seabed habitats: a simple quantitative risk assessment method applicable to data-limited fisheries. Methods in Ecology and Evolution, 8, 472-480. Open access article: https://doi.org/10.1111/2041-210X.12705", style = "font-family: 'times'; font-si22pt"),
                          
                          p("Pitcher", em("et al"),"(In prep) Trawl impacts and seabed habitat status in 24 regions of the world.", style = "font-family: 'times'; font-si22pt"),
                          
                          p("Rijnsdorp", em("et al"), "(2018) Estimating the sensitivity seafloor habitats to disturbance by bottom trawling impacts based on the longevity of benthic fauna. Ecological Applications, 28, 1302-1312. Open access article: https://doi.org/10.1002/eap.1731", style = "font-family: 'times'; font-si22pt"),
                          
                          p("Sciberras", em("et al"), "(2018) Response of benthic fauna to experimental bottom fishing: a global meta-analysis. Fish and Fisheries, 19, 698-715. Open access article: https://doi.org/10.1111/faf.12283", style = "font-family: 'times'; font-si22pt"),
                          
                          
                          br(),
                          p("If you run into any trouble with accessing the above articles then please contact the MSC. Contact details are available in the User Manual."),
                          br()
                          )
                          
                      )
                      

             ),

                      

             
              tabPanel("2.Effort Data",
                       
                       # In this panel, users will input their fishing effort data. Currently, this is based on inputting a .csv of un-aggregated VMS data.
                       # The process would work fine for an aggregated .csv of vms data provided that the users selects the correct grid cell size in the next TAB.
                       
                       
                       br(),
                       h3("Fishing Effort Data"),
                       br(),
                       
                       conditionalPanel(
                         condition = "input.type == 'Please Select'|input.use == 'Please Select'",
                         
                         p(strong("Please return to Tab 1 and input user and usage types"), style="color:red"),
                         

                       ),
                       
                       conditionalPanel(
                         condition = "input.type != 'Please Select' & input.use != 'Please Select'",
                         
                         p("On this tab you will input your fishing effort data. The fishing effort data needs to be a .csv file, with fishing effort recorded as area swept in km^2, 
                         with the aggregation of the data known. Full details about data formatting can be found in the User Manual", strong("(MSC Benthic Impacts Decision Support Tool User Manual)."),
                           "This .csv file should only contain points that represent fishing activity (i.e. no steaming activity).
                          It must contain the following column headings, formatted in exactly the same way (i.e. use of capitals with no spaces etc):"),
                         p(strong("Longitude"), " - The longitude of the data point in decimal degrees"),
                         p(strong("Latitude"), " - The latitude of the data point in decimal degrees"),
                         p(strong("Gear"), " - The fishing gear type e.g. TBB, OT, DRB, SEINE etc"),
                         p(strong("AreaSwept"), " - The area swept by that fishing point (i.e. speed x gear width x duration in km^2)"),
                         br(),
                         
                         
                         # Sidebar layout with input and output definitions ----
                         
                         
                         
                         
                         sidebarLayout(
                           
                           # Sidebar panel for inputs ----
                           sidebarPanel(
                             
                             # Input: Select a file ----
                             fileInput("vms", h3(" TASK 3/15 - Choose CSV File of aggregated VMS data (click the button once only)"),
                                       multiple = TRUE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")),
                             br(),
                             textOutput("thank_text1"),
                             
                             tags$head(tags$style("#thank_text1{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }")),
                             br(),
                             br(),
                             p("Once the data has successfully uploaded the top 6 lines of the dataset will be displayed to the right."),
                             
                             # Horizontal line ----
                             tags$hr()
                             #,
                             
                           ),
                           
                           mainPanel(
                             selectInput("Gear", h3("TASK 4/15 - Choose a Gear type to assess"), choices = NULL),
                             
                             # Output: Data file ----
                             tableOutput("vms_contents")
                             
                           )
                         )
                         
                       ),
                       
                       
                       br(),
                       
                       
                
                      
                       

              ),
             
             
 
             
             tabPanel("3.Habitat Data",
                      
                      # In this TAB users upload a habitat data shapefile, assessment area shapefile, spatial closure shapefile and choose the grid cell size for assessment. Please check that data has loaded properly before proceeding.
                      
                      
                      br(),
                      h3("Habitat Data"),
                      br(),
                      
                      p("On this tab you will input 2 spatial datasets: an assessment area shapefile, and a habitat data shapefile. Habitat data is a", strong("required input"), "for the tool, because impacts are assessed and scored per habitat type. 
                        You also need to upload a shapefile that delineates the extent of the area under assessment (this should cover the largest extent of the habitat under consideration, 
                        but no additional irrelevant areas (e.g. no areas of land)). Please see the User Manual for further 
                        information"),
                      br(),
                      p(strong("This may take a few minutes to load (please be patient if you are assessing a large area). Click on the button to load the files in - you must select all files that make up the shapefile. 
                               When the data has loaded in, a map will appear on the right hand side of the window.")),
                      br(),
                      sidebarLayout(
                        
                        sidebarPanel(

                          # Input: Select a file ----
                          br(),
                          
                          fileInput(inputId = "select_UoA", #select_UoA
                                    label = h3("TASK 5/15 - Upload map of assessment area boundary. Choose shapefile and ALL accessory files."),
                                    multiple = TRUE,
                                    accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg')),
                          
                          

                          fileInput(inputId = "select_habitat", #select_habitat
                                    label = h3("TASK 6/15 - Upload map of habitats within the assessment area boundary. Choose shapefile and ALL accessory files."),
                                    multiple = TRUE,
                                    accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg')),
                          
                        numericInput("gridcellsize", label = h3("Task 7/15 - Input a grid cell size in degrees - which must match the spatial aggregation of your fishing effort data"), value = 0.05, min=0.01, max=5),
                          
                          br(),
                        p("Depending on the size of your assessment area, this may take several minutes to run - please click the button once and then wait"),
                        br(),
                          # Button
                          bsButton("load_habitat", label="Click to load files and plot map", block = F, style = "danger"),
                          bsTooltip(id = "load_habitat", title = "Click here", placement = "right", trigger = "hover"),
                          
                          br(),
                          br(),
                          
                          # Thank you text
                          textOutput("thank_text2"),
                          tags$head(tags$style("#thank_text2{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }")),
                          br()
                          
                        ),
                        
                        mainPanel(
                          leafletOutput("habitat_map"),
                          downloadButton("downloadhabitatmap", "Download the habitat map (click button once and wait for save window to appear)"),


                        )
                      )
                      ),
             
             tabPanel("4.Swept Area Ratio",
                      br(),
                      
                      h3("Swept Area Ratio"),
                      br(),

                      p("Fishing effort frequency is defined as the swept area ratio, which is the area swept by the fishing gear within a grid cell per year divided by the area of the grid cell i.e.", (HTML(paste("year", tags$sup(-1), sep = "")))), 
                      
                      p("Please see the User Manual for further detail."),
                      br(),
                      br(),

                      sidebarLayout(

                        sidebarPanel(


                          numericInput("yearsofvms", label = h3("Task 8/15 - How many years of fishing effort data are there in the dataset?"), value = 1),
                          
                          
                          bsButton("calculate_sar", label="Click to calculate swept area ratio", block = F, style = "danger"),
                          bsTooltip(id = "calculate_sar", title = "Click here", placement = "right", trigger = "hover"),
                          
                          br(),
                          br(),
                          p(strong("This will take a few moments to run, click the button just once please.")),
                          br(),
                          br(),
                          textOutput("thank_text3"),
                          tags$head(tags$style("#thank_text3{color: red;
                                 font-size: 20px;
                                               font-style: italic;
                                               }")),
                          br(),
                          
                          p("Below, a histogram will appear which will highlight the frequency distribution of the Swept Area Ratio within your assessment area."),
                          br(),
                          plotOutput("sar_histogram"),
                          br()

                        ),

                        mainPanel(
                          leafletOutput("uoa_habitat_map2"),
                          br(),
                          br(),
                          downloadButton("downloadSARhist", "Download the SAR histogram"),
                          downloadButton("downloadSARmap", "Download the SAR map (click button once and wait)")
                        )
                      )
             ),

             
             tabPanel("5.Depletion Values",
                      
                      # In this panel, users will be able to select overall default values of depletion rate per gear type or
                      # choose calculated estimations of seabed penetration for combined gear type and sediment type. This will
                      # be dependent on data availability. Values calculated are based on outcomes from a meta-analysis.
                      # There is also opportunity to highlight any modified gear types, with file input.
                      br(),
                      h3("Depletion Values"),
                      br(),
                      
                      p("On this tab you will set the depletion rates to be used in the assessment. The rate of depletion is the fraction of benthic biota that is removed per trawl pass, which can be estimated from the penetration depth of the gear. Default depletion 
                      rates for the main trawl gear types are available in the tool. The depletion rate can depend on the sediment type as well as the gear type, so the tool can calculate improved estimations of default depletion rates if information about the habitat types input can be categorised as mud, sand or gravel. 
                      Alternatively, instead of using a default gear depletion rate, a user can input a more gear-specific depletion rate or penetration depth, but sufficient justification must be provided in the report for this. For further information and instruction about depletion rates see the User Manual."),
                        br(),
                      h3("TASK 9/15 - Will you be using default gear depletion rates provided by the tool, or inputting your own gear specific values?"),
                      radioButtons("gear_mod_yes",  label=NULL, choices = list("I will be using the standard depletion rates provided by the tool"="standard_depletion", 
                                                                               "I will be inputting my own gear-specific depletion rate"="user_depletion",
                                                                               "I will be inputting my own gear-specific penetration depth"="user_penetration"), selected = "standard_depletion"),
                      
                      br(),
                     
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          h4("Default depletion rates are available for the following gears:"),
                          p(strong("Boat Dredges"), (" - DRB")),
                          p(strong("Otter Trawl"), (" - OT")),
                          p(strong("Beam Trawl"),(" - TBB")),
                          p(strong("Mechanised Dredges (including suction dredges)"), (" - HMD")),
                          p(strong("Seine Netting"), (" - SEINE")),
                          br(),
                          
                          
                          
                          uiOutput("GearSelection"),

                          tableOutput("d_gear_table"),
                          br(),
                          
                          h4("Improved depletion rates are available for the following sediment types:"),
                          p(strong("Mud,"), strong("Sand"), "and", strong("Gravel")),
                          
                          br(),
                          
                          tableOutput("d_gear_habitat_table"),
                          
                          br()
                          
                          
                        ),
                        
                        mainPanel(
                          br(),
                          
                          h3("TASK 11a/15 - If sediment type is known, choose either mud, sand or gravel from the dropdown box next to the associated habitat type. If sediment is unknown, table modification is not required."),
                          rHandsontableOutput("habitat_levels"),
                          br(),
                          
                          br(),

                          
                          h3(" TASK 11b/15 - If you have a gear depletion value specific to the gear being assessed, enter it here. It must be a value between 0 and 1."),
                          numericInput("modified_depletion", "Gear Specific Depletion Rate:", min=0, max=1, value=0),
                          br(),
                          h3(" TASK 11c/15 - If you have a gear penetration depth (cm) specific to the gear being assessed, enter it here. It must be a positive value, in centimetres (cm)."),
                          numericInput("modified_penetration", "Gear Specific Penetration Depth:", min=0, max=1000, value=0),
                          
                          br(),
                          br(),
                          
                          bsButton("merge_tables", label="Click to generate depletion values", block = F, style = "danger"),
                          bsTooltip(id = "merge_tables", title = "Click here", placement = "right", trigger = "hover"),
                          
                          
                          br(),
                          br(),
                          tableOutput("habitat_levels4"),
                          br(),
                          downloadButton("downloadDepletionData", "Download the depletion rates used per habitat"),
                          br(),
                          br(),
                          
                          textOutput("depletion_text"),
                          tags$head(tags$style("#depletion_text{color: red;
                                               font-size: 20px;
                                               font-style: italic;
                                               }")),
                          br(),
                          br(),
                          br()
                          
                        )
                      )
             ),
             
             
             
             tabPanel("6.Recovery Rates",
                      # This tab offers the user 3 conditional choices for recovery rates. 
                      # Option 1: input longevity distribution parameters (user input data)
                      # Option 2: input species data (user input data)
                      # Option 3: use default setting (data within tool, based on meta-analysis)
                      
                      
                      br(),
                      h3("Recovery Rates"),
                      br(),
                      
                      p("On this tab you will set the recovery rates to be used in the assessment. Recovery rates are related to the longevity of an organism; species that live longer have a slower recovery 
                        rate (Hiddink et al., 2018). In the tool you have 3 options available for recovery rates, depending on what data and information you have available about the communities in your assessment area. 
                        Please read the User Manual for further information about the science behind these options, and for instructions about how to use each option."),
                      br(),
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          
                          radioButtons("long_data_type",h3("TASK 12/15 - How will you determine the recovery rates to use?"),
                                       choices = c("Input longevity distribution parameters", "Input species data","Use a default setting"),
                                       selected = "Use the default recovery rate")
                         
                        ),
                        
                        mainPanel(


                          # Input coefficients from logistic fit on cumulative biomass distribution
                          
                          conditionalPanel(
                            condition = "input.long_data_type == 'Input longevity distribution parameters'", 
                            h3("TASK 13a/15: Enter longevity distribution parameters for each habitat type then click to generate the recovery rates"),
                            p("Enter the parameters for your species longevity distribution in the table below. 
                              You must provide evidence in your report to support the use of these values. For details of how to obtain these values, see the User Manual. "),
                            
                            fluidRow(
                              column(8,

                                     br(),
                                     rHandsontableOutput("tableinput_con1"),
                                     br(),
                                     br(),
                                     bsButton("long_con1_button", label="Click to generate recovery rates", block = F, style = "danger"),
                                     bsTooltip(id = "long_con1_button", title = "Click here", placement = "right", trigger = "hover"),
                                     br(),
                                     textOutput("thank_text5"),
                                     tags$head(tags$style("#thank_text5{color: red;
                                                          font-size: 20px;
                                                          font-style: italic;
                                                          }")),
                                     br(),
                                     
                                     br()),
                              column(12,
                                     br(),
                                     p("If you're interested in viewing the potential longevity and recovery distributions with modifications of m-slope and b, then have a play with the sliders below. 
                                       IMPORTANT: Moving these sliders WILL NOT modify recovery rates used in the tool. You must input parameters into the table above to use a modelled longevity distribution 
                                       or select another option for calculating recovery rates."),
                                     br(),
                                     br(),
                                     sliderInput("b.slide", label="b coefficient", min=-10,max=10, value=-5, step=0.01),
                                     sliderInput("m_slope.slide", label= expression("m slope coefficient"), min=0.01,max=5, value=3.429, step=0.001),
                                     br(),
                                     br()
                              ),
                              
                              column(8,
                                     br(),
                                     plotOutput("long_distribution"),
                                     br()
                              )
                            ),
                            
                            
                            
                            br(),
                            p("The species longevity distribution is converted to a distribution of recovery rates, see below."),
                            br(),
                            fluidRow(
                              column(8,
                                     br(),
                                     plotOutput("rec_distribution"),
                                     br()
                              )
                              
                            )
                            ), # end of conditional panel1,
                          
                          
                          
                          conditionalPanel(    # start of conditional panel 2
                            condition = "input.long_data_type == 'Input species data'",
                            h3("TASK 13b/15: Upload a .csv file that contains species longevity data for each habitat type, to generate the recovery rates"),
                            p("Here you can input a .csv file of species longevity data, from unfished locations of each habitat type. It must be formatted as follows, with the bold terms used exactly as column headings (i.e. same capitalisation and spaces):"),
                            p(strong("HabitatName"), " - The habitat name, formatted in exactly the same way as in the habitat shapefile used"),
                            p(strong("SpeciesName"), " - The species name"),
                            p(strong("SpeciesLongevity"), " - The maximum lifespan (longevity) of the species, in years"),
                            p(strong("ProportionOfBiomass"), " - The proportion of total biomass that constitutes this species (value 0 - 1)"),
                            br(),
                            br(),
                            
                            fileInput("species_long", h3("Choose a .csv file of species longevity data (click the button once only)"),
                                      multiple = TRUE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                            
                           # uiOutput("select_long"),
                           
                            br(),
                            
                            bsButton("load_longevity_dist_data", label="Click to load your species longevity distribution data file", block = F, style = "danger"),
                            bsTooltip(id = "load_longevity_dist_data", title = "Click here", placement = "right", trigger = "hover"),
                            
                            
                            br(),
                            br(),
                            p("Once the data is processed, the plot output displays the proportion of species biomass within 4 longevity categories per habitat."),
                            br(),
                            
                            plotOutput("species_longevity_data_histogram"),
                            br(),
                            p("The table below shows the recovery rate per habitat. Displayed is the overall recovery rate, and the recovery rate within confidence intervals of SG60, SG80 and SG100. Please see the User Manual for further details about the confidence intervals or how the recovery rate is calculated."),
                            br(),
                            tableOutput("species_longevity_data_head"),
                            br(),
                            textOutput("thank_text6"),
                            tags$head(tags$style("#thank_text6{color: red;
                                                 font-size: 20px;
                                                 font-style: italic;
                                                 }")),
                            br()
                            
                            ), # end of conditional panel 2
                          
                          conditionalPanel( # start of conditional panel 3
                            condition = "input.long_data_type == 'Use a default setting'", 
                            h3("TASK 13c/15: Click the button below to use the default recovery rates"),
                            br(),
                            br(),
                            p("The default value is 0.42 and this value is the lower confidence interval estimate of r from Hiddink et al (2017). (Further detail: r = 0.42  (median = 0.82, with 5-95% confidence intervals = 0.42 and 1.53))"),
                            br(),
                            br(),
                            bsButton("longevity_default_button", label="Click to choose default recovery rate", block = F, style = "danger"),
                            bsTooltip(id = "longevity_default_button", title = "Click here", placement = "right", trigger = "hover"),
                            br(),
                            
                            br(),
                            br(),
                            br(),
                            textOutput("thank_text7"),
                            tags$head(tags$style("#thank_text7{color: red;
                                                 font-size: 20px;
                                                 font-style: italic;
                                                 }")),
                            br()
                            ) # end of conditional panel3
                          

                          
                          ) # end of mainpanel
        

                        ) # end of sidebar layout
                      ), # end of tabpabel

  
              tabPanel("7.Benthic Status",
                       br(),
                       h3("Benthic Status"),
                       br(),
                       
                       p("On this tab, the tool processes the data inputted on previous tabs to calculate the relative benthic status (RBS) for each grid cell. The RBS considers the ratio between depletion and recovery, 
                         in relation to the amount of fishing effort in each grid cell (see equation below, and the User Manual for more information about the RBS)."),
                      br(),
                      div(img(src='RBS_eq.png', style = "text-align:center", width=600)),
                      br(),
                      br(),
                      br(),
                      
                       sidebarLayout(
                         
                         sidebarPanel(
                           
                           h3("TASK 14/15: Calculate the relative benthic status (RBS) for each habitat type"),

                           br(),
                           
                           bsButton("calculate_benthic_status", label=strong(h4("Click to calculate RBS")), block = F, style = "danger"),
                           bsTooltip(id = "calculate_benthic_status", title = "Click here", placement = "right", trigger = "hover")
                           
                         ),
                         
                         mainPanel(
                             
                          
                           
                           p("Below a map of overall RBS will appear for the area of assessment."),
                           
                           leafletOutput("RBS_map1"),
                           downloadButton("downloadRBSmap", "Download the RBS map (click button once and wait)"),
                           br(),
                           
                         br(),
                           
                           p("The table below shows the overall RBS score per habitat type."),

                           
                           br(),
                                      
                           p("Weighted overall RBS scores per habitat type:"),
                           br(),
                           tableOutput("rbs_summary"),
                           downloadButton("downloadRBStable", "Download the RBS summary table"),
                         br(),

                           br(),
                           p("The plot below illustrates the distribution of RBS scores for each grid cell of each habitat type."),
                           plotOutput("rbs_histogram_weighted"),
                         downloadButton("downloadRBShist", "Download the RBS histogram"),
                         
                           br(),
                         br(),
                         textOutput("thank_text4"),
                         tags$head(tags$style("#thank_text4{color: red;
                                 font-size: 20px;
                                               font-style: italic;
                                               }")),
                         br()
                           
                         )
                         
                       )
  
                       ),
             
             tabPanel("8.Recovery Time and MSC Score",
                      br(),
                      h3("Recovery time if fishing were to cease"),
                      br(),
                      p("Here the tool calculates the time it takes for the community biomass to recover. 
                        The recovery time will be used to generate an indicative MSC score for each habitat. This indicative score is calculated based on the quantitative data inputted into the Benthic Impacts Tool and is intended to aid scoring for Principle 2 of the MSC Fisheries Standard (see the User Manual for information on the scoring issue(s) that the indicative score should inform). 
                        For further guidance with this tab, please see the User Manual."),
                      br(),
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          h3("TASK 15/15: Generate the recovery time trajectories per habitat and the indicative MSC score(s)."),
                          br(),
                          bsButton("calculate_recovery_time", label=("Click to calculate recovery time and MSC Scores"), block = F, style = "danger"),
                          bsTooltip(id = "calculate_recovery_time", title = "Click here", placement = "right", trigger = "hover"),
                          
                          br()
                          
                          
                          ),
                        
                        mainPanel(
                          
                          br(),
                          
                          p("The plot below illustrates the recovery trajectory per habitat type (each habitat type shown in a separate panel).
                           The grey lines illustrate the recovery trajectory for biomass in each individual grid cell. 
                           The black line indicates the mean recovery trajectory of all species biomass together across each habitat. 
                           The dashed line indicates an RBS of 0.8, or 80% carrying capacity.  
                           Each indicative MSC score relates to uncertainty in the recovery trajectory - to score SG100, there must be high certainty that the species biomass would recover to 80% within 20 years."),
                          p(strong("Please see the User Manual for more information on how the recovery trajectories relate to MSC scoring.")),
                          br(),
                          plotOutput("recovery_plot"),
                          br(),
                          
                          br(),
                          downloadButton("downloadrecoveryplot", "Download the Recovery Plot"),
                          
                          br(),
                          br(),
                          p("The table below shows the time required for each habitat type to recover to an RBS score of 0.8, and the associated indicative MSC score."),
                          tableOutput("recoverytime_table"),
                          br(),
                          downloadButton("downloadData", "Download the suggested MSC score table"),
                          br(),
                          br(),
                          textOutput("finished_text"),
                          br(),
                          br(),
                          br(),
                          tags$head(tags$style("#finished_text{color: red;
                                 font-size: 20px;
                                               font-style: italic;
                                               }"))
                          
                          
                        )
                        
                          )
                      
                          )
             
             
  ) # end of navbarpage
  
) # end of fluidpage


server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=110*1024^2)
  

  #### Information Tab ####
  
   observeEvent(input$input_data_pathway, {
     
     if(input$type=="Please Select" | input$use=="Please Select") {
       updateButton(session, "input_data_pathway", label = "Click to Start", block = F, style = "danger")
       
     } else {
       updateButton(session, "input_data_pathway", label = "Thanks", block = F, style = "success")
       
     }
     #updateButton(session, "input_data_pathway", label = "Thanks", block = F, style = "success")
     
     #typegiven <- input$type
     #daterun <- Sys.time()
     #typegiven <- "NGO"
     userdata <- data.frame(type = input$type, use = input$use, date = Sys.time())
     #outputDir <- ("C:/Users/ossa29.AD/OneDrive - Bangor University/Documents/MSC BIT App/responses")
     outputDir <- ("/usage")
     fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
     # Write the file to the local system


     if(input$type=="Please Select" | input$use=="Please Select") {
       output$wd_text <- renderText({paste("ERROR: Please specify the Type of User and your reason for using the tool from the drop down menus above, before you proceed with the assessment. Once you have made a selection, please re-click the 'Click to Start' button.")})
       
     } else {
       output$wd_text <- renderText({paste("Thank you for starting an assessment. Please proceed to the next tab by clicking - 2. Effort Data - at the top of the page.")})
       write.csv(userdata,
                 file = file.path(outputDir, fileName), 
                 row.names = FALSE, quote = TRUE
       )
     }
       
     
     
     # Upload depletion rates per gear per habitat
     #d_gear_habitat <- read.csv("C:/Users/ossa29.AD/OneDrive - Bangor University/Documents/MSC BIT App/d_gear_habitat.csv")
      d_gear_habitat <- read.csv("data/d_gear_habitat.csv")
     # d_gear_habitat <- read.csv("d_gear_habitat.csv")
     
    colnames(d_gear_habitat) <- c("Gear", "Grain", "sediment_d")
    d_gear_habitat<<-d_gear_habitat
    

    })
  
  
  #### Effort Data Tab ####
  
  # Load in a csv file with the vms data in it
   
  vms_inFile <- reactive({
    
    if (is.null(input$vms)) {
      return(NULL)
    } else {
      input$vms
    }
   
  })
  
  myData <- reactive({
    
      if (is.null(vms_inFile())) {
      return(NULL)
    } else {
      
      read.csv(vms_inFile()$datapath)
      # mydata <- read.csv("~/Fisheries/CURRENT MSC Benthic Impacts Tool/Test_Assessment/fishing_effort.csv")
      #read.csv(vms_inFile()$datapath, colClasses=c("numeric","numeric","numeric","character","character"))
    }
    
  })

  # User can select which Gear to display/use
  observe({

      updateSelectInput(
        session,
        "Gear",
        #choices=c(list("All Gear"='All'), unique(myData()$Gear))) # change from levels to unique
    choices=c(unique(myData()$Gear))) # change from levels to unique
  
    #}
  })
  
  # show a table with the vms data for the selected Gear
  output$vms_contents <- renderTable({
    if (is.null(myData())) {
      return(NULL)
    } else {
      mydata <<- myData() 
      mydata <<- dplyr::select(mydata, Gear, Latitude, Longitude, Gear, AreaSwept)

      mydata <<- dplyr::filter(mydata, Gear == input$Gear)
      
      
      
      head(mydata)

    }
    
  }, striped = TRUE, bordered = TRUE,
  hover = TRUE)
  
  observeEvent(input$vms, {
    output$thank_text1 <- renderText({paste("Thank you for your input. Please proceed to the next tab once you have chosen the gear type for assessment by clicking - 3. Habitat Data - at the top of the page.")}) 
  })
  
  
  #------------------------------------------------------------------------------------------------------------------
  
  #### Habitat Tab ####
  
  
  observeEvent(input$load_habitat, {
    

    # load assessment area 
    
    # manual habitat load for offline testing: 
    # assessment area:  map <- readOGR("C:/Users/ossa29/OneDrive - Bangor University/Laptop/Documents/Teaching/2020 - 2021/MSc Projects/Fiona - Benthic tool vs MarESA/Western_Channel", "Western_Channel")
    # habitat data:     map3 <- readOGR("C:/Users/ossa29/OneDrive - Bangor University/Laptop/Documents/Teaching/2020 - 2021/MSc Projects/Fiona - Benthic tool vs MarESA/Western_Channel", "HabitatID_Western_Channel_EUNIS")
    # mydata <- read.csv("~/Teaching/2020 - 2021/MSc Projects/Fiona - Benthic tool vs MarESA/Western_Channel/Data/Final_Western_Channel_Otter_Intensity.csv")
    
    # map <- st_read("C:/Users/ossa29.AD/OneDrive - Bangor University/Documents/MSC BIT App/example_data/Western_Channel", "Western_Channel")
    # map3 <- st_read("C:/Users/ossa29.AD/OneDrive - Bangor University/Documents/MSC BIT App/example_data/Western_Channel", "RCS_Western_Channel_EUNIS")
    # mydata <- read.csv("C:/Users/ossa29.AD/OneDrive - Bangor University/Documents/MSC BIT App/example_data/Western_Channel/Final_Western_Channel_Otter_Intensity.csv")
    
    
    updateButton(session, "load_habitat", label = "Loading... Please wait.", block = F, style = "success")
    #start <<- Sys.time()
    #uoa_extent <- rgdal::readOGR(dsn = path.expand(test),layer = paste(input$uoa_extent_file))
    
    cat(file=stderr(), paste(Sys.time(), "Importing assessment area shapefile: start\n"))
    
    
    #### Loading Assessment Area Data ####
    
    #observe({
      shpdf <- input$select_UoA
      if(is.null(shpdf)){
        return()
      }
      previouswd <- getwd()
      uploaddirectory <- dirname(shpdf$datapath[1])
      setwd(uploaddirectory)
      for(i in 1:nrow(shpdf)){
        file.rename(shpdf$datapath[i], shpdf$name[i])
      }
      setwd(previouswd)
      
      
      #### Changed from readOGR to st_read ####
      
        map <- st_read(paste(uploaddirectory, shpdf$name[grep(pattern="*.shp$", shpdf$name)], sep="/"))#,  delete_null_obj=TRUE)
        
        ##If there is an Id columns, remove it
      #### changed names(map@data) to just names(map)  
      names(map)[names(map)=="Id"]<-"Rename_ID" 
        
        cat(file=stderr(), paste(Sys.time(), "Importing assessment area shapefile: end\n"))
        
        
        cat(file=stderr(), paste(Sys.time(), "Projecting assessment area shapefile: start\n"))
        #### changed to st_transform ####
        map <- st_transform(map, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
        cat(file=stderr(), paste(Sys.time(), "Projecting assessment area shapefile: end\n"))
        
        
        #})
      #})
      
      uoa_extent<-map 
      
      cat(file=stderr(), paste(Sys.time(), "Re-projecting assessment area shapefile: start\n"))
      
      #### New sf package st_transform ####
      #uoa_extent<- spTransform(uoa_extent, CRS("+init=epsg:4326"))
      uoa_extent<- st_transform(uoa_extent, CRS("+init=epsg:4326"))
      
      uoa_extent$in_uoa <- "yes"
      
      cat(file=stderr(), paste(Sys.time(), "Re-projecting assessment area shapefile: end\n"))
      
      
      
  
      
      #### create assessment grid ####
      
      
      
      
      cat(file=stderr(), paste(Sys.time(), "Creating assessment grid: start\n"))
      
  
           gridsize <- as.numeric(input$gridcellsize)
           # gridsize <- 0.05
           
           
           x_min <- min(mydata$Longitude - (input$gridcellsize/2) )
           x_max <- max(mydata$Longitude + (input$gridcellsize/2))
           y_min <- min(mydata$Latitude - (input$gridcellsize/2))
           y_max <- max(mydata$Latitude + (input$gridcellsize/2))
           # x_min <- min(mydata$Longitude - (0.05/2) )
           # x_max <- max(mydata$Longitude + (0.05/2))
           # y_min <- min(mydata$Latitude - (0.05/2))
           # y_max <- max(mydata$Latitude + (0.05/2))
           


           # Extend the grid to the max extent of the assessment area - by adding sufficient whole grid cells so it still aligns with fishing points
           if(st_bbox(uoa_extent)$ymin < y_min){ y_min <- y_min - (ceiling(abs((st_bbox(uoa_extent)$ymin - y_min) / input$gridcellsize)) * input$gridcellsize)}
           if(st_bbox(uoa_extent)$ymax > y_max){ y_max <- y_max + (ceiling(abs((st_bbox(uoa_extent)$ymax - y_max) / input$gridcellsize)) * input$gridcellsize)} # ok
           if(st_bbox(uoa_extent)$xmin < x_min){ x_min <- x_min - (ceiling(abs(st_bbox(uoa_extent)$xmin - x_min) / input$gridcellsize) * input$gridcellsize)} # ok
           if(st_bbox(uoa_extent)$xmax > x_max){ x_max <- x_max + (ceiling(abs((st_bbox(uoa_extent)$xmax - x_max) / input$gridcellsize)) * input$gridcellsize)}
           
           

           # Create empty raster that fits the extent of the uoa_extent shapefile, and uses the grid size defined by the user
           r_grid <- raster(ext = extent(x_min,x_max,y_min,y_max), res=c(gridsize,gridsize))
           # r_grid <- raster(ext = extent(x_min,x_max,y_min,y_max), res=c(0.05,0.05))
           
           # Add ID values to each raster cell
           values(r_grid)<-1:ncell(r_grid)
           
           # Create a version of the raster grid called rA_grid
           rA_grid<-projectRaster(r_grid, crs = "+init=epsg:4326")
           
           # Convert the raster grid to a polygon grid and re-project
           p_grid <- rasterToPolygons(r_grid)
           p_grid <- st_as_sf(p_grid)
           #grd <<- spTransform(p_grid, "+init=epsg:4326")
           grd <<- st_transform(p_grid, "+init=epsg:4326")
           
           ####  keep uoa_extent as the whole grid for now - cut it to the uoa_extent after calculating SAR
           grid_uoa <- grd
  
  
           
      cat(file=stderr(), paste(Sys.time(), "Creating assessment grid: end\n"))
      
  
      
      # Each cell needs a unique Id number
      grid_uoa$Id <- 1:nrow(grid_uoa)    
      
  
      
      grid_uoa$in_uoa <- "YES"
      
  
      
      #### Generate area of each grid cell for calculating the SAR:
      cat(file=stderr(), paste(Sys.time(), "Generate area of each grid cell: start\n"))
      
  
      # get the area of each grid cell and re-check projection
      
      #### change from area to st_area####
      #grid_uoa$area_sqkm <- area(grid_uoa) / 1000000
      grid_uoa$area_sqkm <- st_area(grid_uoa) / 1000000
      
      cat(file=stderr(), paste(Sys.time(), "Generate area of each grid cell: end\n"))
      
      cat(file=stderr(), paste(Sys.time(), "Re-project grid: start\n"))
      #grid_uoa <- spTransform(grid_uoa, CRS("+init=epsg:4326"))
      grid_uoa <- st_transform(grid_uoa, CRS("+init=epsg:4326"))
      
      
      cat(file=stderr(), paste(Sys.time(), "Re-project grid: end\n"))
      
      # Create global version of grid_uoa
      grid_uoa <<- grid_uoa
      

    
    #### Load Habitat Data ####
    
    cat(file=stderr(), paste(Sys.time(), "Load habitat data: start\n"))
    
    shpdf3 <- input$select_habitat
    if(is.null(shpdf3)){
      return()
    }
    previouswd <- getwd()
    uploaddirectory3 <- dirname(shpdf3$datapath[1])
    setwd(uploaddirectory3)
    for(i in 1:nrow(shpdf3)){
      file.rename(shpdf3$datapath[i], shpdf3$name[i])
    }
    setwd(previouswd)
    
    #### change to st_read ####
    map3 <- st_read(paste(uploaddirectory3, shpdf3$name[grep(pattern="*.shp$", shpdf3$name)], sep="/"))#,  delete_null_obj=TRUE)
    cat(file=stderr(), paste(Sys.time(), "Load habitat data: end\n"))
    
    crs(map3)
    
    cat(file=stderr(), paste(Sys.time(), "Project habitat data: start\n"))
    
    #### change to st_transform
    map3 <- st_transform(map3, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    habitat<<-map3

    cat(file=stderr(), paste(Sys.time(), "Project habitat data: end\n"))
    
    observeEvent(input$load_habitat, {
      output$thank_text2 <- renderText({paste("Thank you for your input. Please proceed to the next tab once you have had a look at your assessment area on the map by clicking - 4. Swept Area Ratio - at the top of the page.")}) 
      
      
      })  
    
    # manual habitat load for offline testing: habitat <- readOGR("C:/Users/ossa29/OneDrive - Bangor University/Laptop/Documents/Fisheries/CURRENT MSC Benthic Impacts Tool/Test_Assessment", "eunis_habitat")
    
    #### change to st_transform
    habitat<- st_transform(habitat, CRS("+init=epsg:4326"))
    uoa_extent <<- st_transform(uoa_extent, CRS("+init=epsg:4326"))
    
      
    #### Create a leaflet map of the habitats to display in app
    cat(file=stderr(), paste(Sys.time(), "Create leaflet map of habitat data: start\n"))
    
    output$habitat_map <- renderLeaflet({
      
        
        habitat<- subset(habitat, HabitatID != "NA")
        factpal <- colorFactor(topo.colors(25), habitat$HabitatID)
        
        leaflet(habitat) %>% addTiles() %>%
          addPolygons(data=habitat, fillColor = ~factpal(HabitatID), fillOpacity=0.5, popup= ~HabitatID, color="transparent") %>% 
          addPolygons(data=uoa_extent,  weight = 2, fillColor="transparent") %>%
          #addCircleMarkers(data=mydata, lng = ~Longitude, lat = ~Latitude, color="grey", radius=1) %>% 
          addLegend("bottomright", pal = factpal, values = ~habitat$HabitatID,
                    title = "Habitat Types",
                    opacity = 1)

    })
    #end <<- Sys.time()
    cat(file=stderr(), paste(Sys.time(), "Create leaflet map of habitat data: end\n"))
    
    updateButton(session, "load_habitat", label = "All done!", block = F, style = "success")
    
    
    }) 
  
  
  output$downloadhabitatmap <- downloadHandler(
    filename = function() { paste("Habitatmap-", Sys.Date(), '.png', sep='') },
    content = function(file) {
     
        
        habitat<- subset(habitat, HabitatID != "NA")
        factpal <- colorFactor(topo.colors(25), habitat$HabitatID)
        
        habitatmap <- leaflet(habitat) %>% addTiles() %>%
          addPolygons(data=habitat, fillColor = ~factpal(HabitatID), fillOpacity=0.5, popup= ~HabitatID, color="transparent") %>% 
          addPolygons(data=uoa_extent,  weight = 2, fillColor="transparent") %>%
          #addCircleMarkers(data=mydata, lng = ~Longitude, lat = ~Latitude, color="grey", radius=1) %>% 
          addLegend("bottomright", pal = factpal, values = ~habitat$HabitatID,
                    title = "Habitat Types",
                    opacity = 1)
        

      mapshot(habitatmap, file=file)
      
    }
  )
  
  
  
  
  #------------------------------------------------------------------------------------------------------------------
  
  
  
  #### Swept Area Ratio Tab ####
  
  observeEvent(input$calculate_sar, {
    
    updateButton(session, "calculate_sar", label = "Loading... Please wait.", block = F, style = "success")
    
    
    # mydata <- read.csv("~/Fisheries/CURRENT MSC Benthic Impacts Tool/Test_Assessment/fishing_effort.csv")
    # mydata <- read.csv("~/Teaching/2020 - 2021/MSc Projects/Fiona - Benthic tool vs MarESA/Western_Channel/Data/Final_Western_Channel_Otter_Intensity.csv")
    # mydata <- read.csv("~/Teaching/2020 - 2021/MSc Projects/Fiona - Benthic tool vs MarESA/Benthic_Impacts_Tool_Data/Otter_Trawl/North_Anglesey_Marine/Data/Final_North_Anglesey_Marine_Otter_Intensity.csv")
    # mydata <- read.csv("C:/Users/ossa29.AD/OneDrive - Bangor University/Documents/MSC BIT App/example_data/Western_Channel/Final_Western_Channel_Otter_Intensity.csv")
    

    vms_sp <- sf::st_as_sf(mydata, coords = c("Longitude","Latitude"), crs="+init=epsg:4326")   
    
    # 
    # # Convert tacsat to spatial points
    # cat(file=stderr(), paste(Sys.time(), "Create spatial vms points: start\n"))
    # vms_sp <-   SpatialPoints(mydata[,c("Longitude","Latitude")])
    # cat(file=stderr(), paste(Sys.time(), "Create spatial vms points: end\n"))
    # 
    # cat(file=stderr(), paste(Sys.time(), "Set vms_sp CRS: start\n"))
    # proj4string(vms_sp) <-  CRS("+init=epsg:4326")
    # cat(file=stderr(), paste(Sys.time(), "Set vms_sp CRS: end\n"))
    
    
    cat(file=stderr(), paste(Sys.time(), "VMS data 'over' the uoa_extent: start\n"))
    # Assign each vms point with the attributes from the grid cell that it is located in
    #mydata <-  data.frame(mydata,over(vms_sp,uoa_extent))
    
    #### changeing over to st_join ####
    #mydata <-  data.frame(mydata,st_intersects(vms_sp,uoa_extent))
    mydata <- cbind(mydata, in_uoa=st_join(vms_sp, uoa_extent)$in_uoa)
    
    #mydata <- dplyr::select(mydata, Gear, Latitude, Longitude, AreaSwept, in_uoa)
    
    cat(file=stderr(), paste(Sys.time(), "VMS data 'over' the uoa_extent: end\n"))
    
    
    cat(file=stderr(), paste(Sys.time(), "Recreate vms_sp: start\n"))
    # Recreate spatial points using new 'mydata'
    # vms_sp <-   SpatialPoints(mydata[,c("Longitude","Latitude")])
    # proj4string(vms_sp) <-  CRS("+init=epsg:4326")
    
    #### Use st_as_sf instead of spatial points ####
    vms_sp <- sf::st_as_sf(mydata, coords = c("Longitude","Latitude"), crs="+init=epsg:4326")   
    
    cat(file=stderr(), paste(Sys.time(), "Recreate vms_sp: end\n"))
    
    
  
    # Calculate area of grid cells in kmsq
     #grid_uoa$area_sqkm <- area(grid_uoa) / 1000000
     grid_uoa$area_sqkm <- st_area(grid_uoa) / 1000000
     
     
     cat(file=stderr(), paste(Sys.time(), "vms_sp 'over' grid_uoa: start\n"))
     # Assign each vms point with the attributes from the grid cell that it is located in
     # mydata <- data.frame(mydata,grid=over(vms_sp,grid_uoa))        
     # mydata <<- mydata[!is.na(mydata$grid.area_sqkm),]
     
     mydata <- cbind(mydata,
                     grid.Id = st_join(vms_sp, grid_uoa)$Id, 
                     grid.in_uoa = st_join(vms_sp, grid_uoa)$in_uoa.y,
                     grid.area_sqkm = st_join(vms_sp, grid_uoa)$area_sqkm)
     
     
     cat(file=stderr(), paste(Sys.time(), "vms_sp 'over' grid_uoa: end\n"))
     
    
     
     cat(file=stderr(), paste(Sys.time(), "Get swept area and SAR per grid cell: start\n"))
    # summarise the area swept per grid cell
    area_pergrid_Gear <- dplyr::summarise(group_by(mydata, Gear, grid.Id),    
                                          sum_area = sum(AreaSwept), #sum
                                          grid_area = mean(grid.area_sqkm, na.rm=TRUE)) 
    
    
    # Add back in grid cells with zero area swept
    #grid_ids <- data.frame(grid.Id = grid_uoa@data$Id, area_sqkm=grid_uoa@data$area_sqkm)
    grid_ids <- data.frame(grid.Id = grid_uoa$Id, area_sqkm=grid_uoa$area_sqkm)
    
    area_pergrid_Gear <- merge(grid_ids, area_pergrid_Gear, by="grid.Id", all.x=TRUE, all.y=TRUE)
    
    # Set missing area swept as zero
    area_pergrid_Gear$sum_area[is.na(area_pergrid_Gear$sum_area)] <- 0
    
    # add back in areas of zero sar cells
    area_pergrid_Gear$grid_area <- area_pergrid_Gear$area_sqkm
    
    # Add back in gear type
    area_pergrid_Gear$Gear <- unique(area_pergrid_Gear$Gear)[!is.na(unique(area_pergrid_Gear$Gear))]
    
    
    # Calculate swept area ratio
    area_pergrid_Gear$SAR <- (area_pergrid_Gear$sum_area /  input$yearsofvms) / area_pergrid_Gear$grid_area
    # area_pergrid_Gear$SAR <- (area_pergrid_Gear$sum_area /  1) / area_pergrid_Gear$grid_area
      
    cat(file=stderr(), paste(Sys.time(), "Get swept area and SAR per grid cell: end\n"))
    
    
    
    #### Cut the grid so that each habitat area within a grid cell has its own ID
    
    cat(file=stderr(), paste(Sys.time(), "Re-project habitat and uoa_extent: start\n"))
    
    # Re-project
   # habitat <- spTransform(habitat, CRS("+proj=utm  +datum=WGS84"))
   # uoa_extent <- spTransform(uoa_extent, CRS("+init=epsg:4326"))
    
    # habitat <- st_transform(habitat, CRS("+init=epsg:3857"))
    
    habitat <- st_transform(habitat, CRS("+init=epsg:4326"))
    uoa_extent <- st_transform(uoa_extent, CRS("+init=epsg:4326"))
    
    cat(file=stderr(), paste(Sys.time(), "Re-project habitat and uoa_extent: end\n"))
    
    
    
    cat(file=stderr(), paste(Sys.time(), "Create a zero buffer: start\n"))
    
    # use a zero buffer to solve self-intersecting polygons and re-project
   #habitat <- gBuffer(habitat, byid=TRUE, width=0)
    
    
    # to fix invalid polygons - ADDED 03/11/2023
    sf_use_s2(FALSE)
    
    
    habitat <- st_buffer(habitat,  dist=0)
    
   
    cat(file=stderr(), paste(Sys.time(), "Create a zero buffer: end\n"))
    
    
    cat(file=stderr(), paste(Sys.time(), "Intersect grid with uoa_extent: start\n"))
    #grid_hab_uoa <- intersect(grid_uoa, uoa_extent)
    grid_hab_uoa <- st_intersection(grid_uoa, uoa_extent)
    
    cat(file=stderr(), paste(Sys.time(), "Intersect grid with uoa_extent: end\n"))
    
    
    # Cut the grid by habitat types - so each grid cell is split into multiple areas based on habitat type
    cat(file=stderr(), paste(Sys.time(), "Intersect grid with habitat: start\n"))
    #grid_hab_uoa_indiv <- intersect(grid_hab_uoa, habitat)
    grid_hab_uoa_indiv <- st_intersection(grid_hab_uoa, habitat)
    
    grid_hab_uoa_indiv$hab_cell_id_bgs <- 1:nrow(grid_hab_uoa_indiv)
    grid_hab_uoa_indiv_ids <- grid_hab_uoa_indiv[,"hab_cell_id_bgs"]
    # grid_hab_uoa_indiv <- grid_hab_uoa_indiv[, c("layer","Id",  "in_uoa", "area_sqkm","HabitatID", "hab_cell_id_bgs")]
    grid_hab_uoa_indiv <- grid_hab_uoa_indiv[, c("layer","Id",   "area_sqkm","HabitatID", "hab_cell_id_bgs")]
    
    # plot(grid_hab_uoa_indiv)
    cat(file=stderr(), paste(Sys.time(), "Intersect grid with habitat: end\n"))
    
    
    #create global version
    grid_hab_uoa_indiv_global <<- grid_hab_uoa_indiv
    
    
    # Create hab_uoa_indiv shapefile which has the SAR assigned to each individual habitat fragment within each grid cell. hab_cell_id_bgs is the habitat fragment ID
    cat(file=stderr(), paste(Sys.time(), "Merge grid_hab_uoa_indiv with area_pergrid_Gear: start\n"))
    
    # hab_uoa_indiv <<- merge(grid_hab_uoa_indiv, area_pergrid_Gear, by.x="Id", by.y="grid.Id", all.x=TRUE, all.y=TRUE, duplicateGeoms = TRUE)
    # hab_uoa_indiv <- st_as_sf(hab_uoa_indiv)
    
    hab_uoa_indiv <<- merge(grid_hab_uoa_indiv, area_pergrid_Gear, by.x="Id", by.y="grid.Id", all.x=TRUE, duplicateGeoms = TRUE)

    cat(file=stderr(), paste(Sys.time(), "Merge grid_hab_uoa_indiv with area_pergrid_Gear: end\n"))
    
    

    #hab_uoa_indiv@data$area_sqkm=area(hab_uoa_indiv)/10^6  
    
    hab_uoa_indiv <- drop_units(hab_uoa_indiv)
    hab_uoa_indiv$area_sqkm=as.numeric(st_area(hab_uoa_indiv)/10^6)   
    
    area_pergrid_Gear<-as.data.frame(hab_uoa_indiv)
    area_pergrid_Gear$grid_area=area_pergrid_Gear$area_sqkm 
    
    area_pergrid_Gear <<- as.data.frame(area_pergrid_Gear)   
    
    cat(file=stderr(), paste(Sys.time(), "Create histogram of SAR values: start\n"))
    
    # Create histogram of SAR values
    output$sar_histogram <- renderPlot({
      sar_print<- ggplot(data=area_pergrid_Gear, aes(x=SAR)) + geom_histogram(binwidth=0.1) + theme_bw() + 
        labs(x="Swept Area Ratio (SAR year-1)", y="Count", title="Distribution of SAR Values")
    
      sar_print
      })
    cat(file=stderr(), paste(Sys.time(), "Create histogram of SAR values: end\n"))
    
    
    cat(file=stderr(), paste(Sys.time(), "Create leaflet of SAR values: start\n"))
    # Create interactive map of SAR values
    output$uoa_habitat_map2 <- renderLeaflet({
      
        pal <- colorNumeric(palette = "Reds",  domain = hab_uoa_indiv$SAR, na.color = "white")
        

        # hab_uoa_indiv <- drop_units(hab_uoa_indiv)
        # hab_uoa_indiv <- sf::st_as_sf(hab_uoa_indiv)
        
        # hab_uoa_indiv_backup <- hab_uoa_indiv

        leaflet() %>% 
          addTiles() %>% 
          addPolygons(data=hab_uoa_indiv, weight = 0.8, 
                      color = ~pal(SAR), fillOpacity=0.9) %>% 
          addPolygons(data=hab_uoa_indiv, weight = 0.8,color = "#444444", fillColor = "transparent",
                      popup=as.character(round(hab_uoa_indiv$SAR,2))) %>%
          #addCircleMarkers(data=mydata, lng = ~Longitude, lat = ~Latitude, color="darkgrey", radius=0.5) %>% 
          addLegend("bottomright", pal = pal, values = hab_uoa_indiv$SAR,
                    title = ('Swept Area Ratio, year-1'),
                    opacity = 1,
                    na.label = "")
        

        
      })
    cat(file=stderr(), paste(Sys.time(), "Create leaflet of SAR values: end\n"))
    
    
    
    observeEvent(input$calculate_sar, {

      
      output$thank_text3 <- renderText({paste("Thank you for your input. Please proceed to the next tab once you have had a look at your fishing effort map by clicking - 5. Depletion Values - at the top of the page.")}) 
    }) 
    
    updateButton(session, "calculate_sar", label = "All done!", block = F, style = "success")
    })
  
   # Download for SAR plots # ORIGINAL LOCATION 


  
  
  output$downloadSARmap <- downloadHandler(
    filename = function() { paste("SARmap-", Sys.Date(), '.png', sep='') },
    content = function(file) {
      
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
    
      pal <- colorNumeric(palette = "Reds",  domain = hab_uoa_indiv$SAR, na.color = "white")
      sar_map<-        leaflet(hab_uoa_indiv) %>% 
        addTiles() %>% 
        addPolygons(data=hab_uoa_indiv, weight = 0.8, 
                    color = ~pal(SAR), fillOpacity=0.9) %>% 
        addPolygons(data=hab_uoa_indiv, weight = 0.8,color = "#444444", fillColor = "transparent",
                    popup=as.character(round(hab_uoa_indiv$SAR,2))) %>%
        addLegend("bottomright", pal = pal, values = hab_uoa_indiv$SAR,
                  title = ('Swept Area Ratio, year-1'),
                  opacity = 1,
                  na.label = "")
      
      mapshot(sar_map, file=file)
  
    }
  )
  
  
    # Download for SAR plots   #NEW LOCATION
  output$downloadSARhist <- downloadHandler(
    filename = function() { paste("SARhist-", Sys.Date(), '.png', sep='') },
    content = function(file) {
      sar_hist<-ggplot(data=area_pergrid_Gear, aes(x=SAR)) + geom_histogram(binwidth=0.1) + theme_bw() + 
        labs(x="Swept Area Ratio (SAR, year-1)", y="Count", title="Distribution of SAR Values")
      ggsave(file, plot = sar_hist, type = "cairo-png")
    }
  )

  
  
  #------------------------------------------------------------------------------------------------------------------
  
  
  #### Depletion Values Tab ####
  
  
  output$GearSelection <- renderUI({
    selectInput("gear_type_assessed", label=strong(h3("TASK 10/15 - Select the type of gear being assessed in the drop down box below:")), 
    choices = c(unique(myData()$Gear))) # changed from levels to unique
    })
  
  
  # Display generic gear depletion rates for mud sand gravel
  output$d_gear_table <- renderTable({
    
    lookup.d.gear = data.frame( 
      d.gear=c( 0.2, 0.2, 0.06, 0.14, 0.41, 0.016), 
      gear=c( "DRB","HMD", "OT", "TBB","HD", "SEINE"))
    
    # Obtain depletion rate for the gear being assessed
    d_gear_table <- lookup.d.gear %>% 
      dplyr::filter(gear == input$Gear) %>% 
      dplyr::select(gear, d.gear)
    #     d_gear_table <- lookup.d.gear %>%   dplyr::filter(gear == "OT") %>%   dplyr::select(gear, d.gear)
    colnames(d_gear_table) <- c( "Gear Type", "Default Depletion Rate")
    d_gear_table<<-d_gear_table
    
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # Display generic gear depletion rates for mud sand gravel
  output$d_gear_habitat_table <- renderTable({

    
    # Obtain depletion rates per sediment type for the gear being assessed
    d_gear_habitat2 <- dplyr::filter(d_gear_habitat, Gear == input$Gear)
    # d_gear_habitat2 <- dplyr::filter(d_gear_habitat, Gear == "OT")
    colnames(d_gear_habitat2) <- c( "Gear Type", "Sediment", "Depletion Rate")
    d_gear_habitat2<<-d_gear_habitat2
  
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  
  # Extract the types of habitat in the user inputted dataset
  habitat_level_values <- reactive({
    habitat_level_values <- unique(habitat$HabitatID) #changed from levels to unique
    Sediments <- "No sediment type chosen"
    sed<-c('Choose sediment type...', 'mud','sand','gravel')
    habitat_level_values <- cbind.data.frame(habitat_level_values, Sediments)
    colnames(habitat_level_values) <- c("Habitat.Type", "Sediments")
    habitat_level_values[] <- lapply(habitat_level_values, as.character)
    habitat_level_values
    })
  

    # User can input the generic habitat type (mud sand gravel) to get a more habitat specific depletion rate rather than overall gear rate.
  output$habitat_levels <- renderRHandsontable({
    sed<-c('Choose sediment type...', 'mud','sand','gravel')
    rhandsontable(habitat_level_values()) %>% hot_col(col="Sediments", type="dropdown", source=sed, allowInvalid = TRUE)
        })
  
  # User generates the depletion rates to be used for each habitat, depending on what information they have input.
  # Will use the habitat specific depletion rate where possible, if not known it will default to the generic gear rate.
  observeEvent(input$merge_tables, {
    updateButton(session, "merge_tables", label = "Thanks. See Table Below", block = F, style = "success")
    
    habitat_levels2 <<- hot_to_r(input$habitat_levels)
    habitat_levels3 <<- reactive({habitat_levels2 <- hot_to_r(input$habitat_levels)
    
    
    # Extract depletion rates of just the gear used in the assessment (without changing column headers for display in interface)
    d_gear_habitat3 <- dplyr::filter(d_gear_habitat, Gear == input$gear_type_assessed)
    # d_gear_habitat3 <- dplyr::filter(d_gear_habitat, Gear == "OT")
    

    # Merge the penetration depth data into the habitat types, so each habitat type as a penetration depth (if user has input sediment types)
    merge1 <- merge(habitat_levels2, d_gear_habitat3, by.x="Sediments", by.y="Grain", all.x=TRUE)
    
    
    # If no sediment types have been chosen, there will be no gear types or penetration depths merged
    # So - where sediment type is missing, add in the code for the gear type being assessed
    merge1$Gear[is.na(merge1$Gear)] <- input$gear_type_assessed
    # merge1$Gear[is.na(merge1$Gear)] <- "OT"
    
    
    # Add generic gear depletion rates to table
    lookup.d.gear = data.frame( 
      d.gear=c( 0.2, 0.2, 0.06, 0.14, 0.41, 0.016), 
      gear=c( "DRB","HMD", "OT", "TBB","HD", "SEINE"))
    
    # Extract the generic depletion rate for the gear to be assessed
    # generic_gear_depletion_rate <- dplyr::filter(lookup.d.gear, gear == "OT")[,1]
    generic_gear_depletion_rate <- dplyr::filter(lookup.d.gear, gear == input$gear_type_assessed )[,1]
    merge1$generic_gear_depletion_rate <- generic_gear_depletion_rate
    merge1$user_gear_depletion_rate <- NA
    merge1$user_gear_penetration_depletion_rate <- NA
    
    
    
    #### Setting Depletion Values to use based on settings ####
    
    # If a user has their own modified gear depletion rate:
    if (input$gear_mod_yes == "user_depletion"){
      merge1$user_gear_depletion_rate  <- input$modified_depletion
      merge1$depletion_used_in_tool <- merge1$user_gear_depletion_rate
    } 
    
    
    # If a user has their own modified gear penetration depth:
    if (input$gear_mod_yes == "user_penetration"){
      
      # gear_penetration_depletion_rate <- (0.19 * (log(10 + 1))) - 0.09
      gear_penetration_depletion_rate <- (0.19 * (log(input$modified_penetration + 1))) - 0.09
      
      gear_penetration_depletion_rate[gear_penetration_depletion_rate<0] <- 0
      
      merge1$user_gear_penetration_depletion_rate <- gear_penetration_depletion_rate
      merge1$depletion_used_in_tool <- merge1$user_gear_penetration_depletion_rate
      
      
    }
      
    # If a user is using standard gear depletion rates:
    if (input$gear_mod_yes == "standard_depletion"){
      

      # depletion rate used - sediment specific if input, if not, uses generic gear depletion rate (different order to if user inputs a gear value, which takes preference)
      merge1$depletion_used_in_tool[!is.na(merge1$sediment_d )] <- merge1$sediment_d [!is.na(merge1$sediment_d )] 
      merge1$depletion_used_in_tool[is.na(merge1$sediment_d )] <- merge1$generic_gear_depletion_rate[is.na(merge1$sediment_d )]
      
      
    }
    

    depletionfinal <<- merge1
    
    # dplyr::select(depletionfinal, Gear, Habitat.Type, Sediments,  d.gear, pen_depth, pred_d, depletion)
    dplyr::select(depletionfinal, Gear, Habitat.Type, generic_gear_depletion_rate, Sediments,  sediment_d , user_gear_depletion_rate, user_gear_penetration_depletion_rate, depletion_used_in_tool) 
    
        }) # end of creating habitat_levels3 reactive object
    
   
    # Table provides the depletion rates to be used for each gear:habitat combination
    habitat_levels4 <<- as.data.frame({habitat_levels3()})

    colnames(habitat_levels4)<-c("Gear Type", "Habitat Name(s)", "Generic Gear Depletion Rate", "Sediment Type", "Sediment Specific Depletion Rate",
                                 "User Inputted Gear Depletion Rate", "Depletion Based on User Inputted Penetration", "Depletion Rate used in Assessment")
    habitat_levels4_global <<- habitat_levels4
    
    output$habitat_levels4 <- renderTable({habitat_levels4}, striped = TRUE, bordered = TRUE,
                                          hover = TRUE)
    
    output$depletion_text <- renderText({paste("Depletion values registered. Please proceed to the next tab by clicking - 6. Recovery Rates - at the top of the page.")})
    
  })
  
  output$downloadDepletionData <-downloadHandler(
    filename = function() {
      paste("Depletion-rate-table-", Sys.Date(), ".csv", sep="") 
    },
    content = function(file) {
      
      write.csv(habitat_levels4_global, file, row.names = FALSE)
      
    }
  )
  

  #------------------------------------------------------------------------------------------------------------------
  
  
  
  #### Recovery Rates ####

  # User can either: 
  # 1. input longevity distribution parameters
  # 2. Can input species data 
  # 3. Can use a default setting - this will be for if a fishery does not have any longevity data

  
  #### conditional recovery option 1 - input longevity distribution parameters from model ####
  
  # USER INPUT TABLE - An interactive table for user input of values m_slope and b

  habitat_level_values_con1 <<- reactive({
    habitat_level_values_con1 <- unique(habitat$HabitatID) #changed from levels to unique
    
    m_slope <- "Add value"
    b <- "Add value"
    

    habitat_level_values_con1 <- cbind.data.frame(habitat_level_values_con1, m_slope, b)
    colnames(habitat_level_values_con1) <- c("habitat", "m_slope", "b")
    habitat_level_values_con1[] <- lapply(habitat_level_values_con1, as.vector)
    habitat_level_values_con1
    
  })
  
  output$tableinput_con1 <- renderRHandsontable({
    rhandsontable(habitat_level_values_con1()) %>% hot_col(col="m_slope", allowInvalid = TRUE) %>%
      hot_col("b", allowInvalid = TRUE)
  })
  
  observeEvent(input$long_con1_button, {  
    updateButton(session, "long_con1_button", label = "Thanks. Next tab please", block = F, style = "success") 
    output$thank_text5<- renderText({paste("Thank you for your recovery rates. Please proceed to the next tab by clicking - 7. Benthic Status - at the top of the page.")})  
  
    #### LONGEVITY DISTRIBUTION - creating the longevity distribution based on user input from table
    
    # Example table for testing with western channel data:
    # habitat <- c("A5.45", "A5.15", "A4.27")
    # m_slope <- c(0.3,0.1,0.5)
    # b <- c(-2.3,-7,2.4)
    # test_table <- cbind.data.frame(habitat,m_slope, b)

 habs_long_prop<-reactive({    test_table<<- hot_to_r(input$tableinput_con1)
   #test_table <- habitat_level_values_con1
 sapply(test_table[, c(2:3)], as.numeric)
   
 habitats_long<-data.frame()
 
 for(i in unique(test_table$habitat)){
   
   test_table.temp<-data.frame()
   
   test_table.temp <- dplyr::filter(test_table, habitat == i)
   # test_table.temp <- dplyr::filter(test_table, habitat == "A5.45")
   
   # generate the distribution of longevities from 1-50
   species_longevity_x.i <- seq(.1,50,by=.1)                 
   m_slope.i<-as.numeric(test_table.temp$m_slope)
   b.i<-as.numeric(test_table.temp$b)
   
   # slope.i tells us the proportion of biomass in each longevity
   K = (m_slope.i*exp(m_slope.i * log(species_longevity_x.i )+b.i))     /(species_longevity_x.i  * (exp(m_slope.i*log(species_longevity_x.i ) + b.i) + 1)^2)    
   # K = (a*exp(a * log(longevity)+b))/(longevity * (exp(a*log(longevity) + b) + 1)^2)       # FROM THE FBIT SCRIPT
   
   # Create dataframe with species longevities and the proportion of biomass in each
   long_df1.i <- cbind.data.frame(species_longevity_x.i, K)
   
   # Scales total proportion (slope.i/K) for all longevities to 1, to deal with working with classes
   long_df1.i$freq.i <- (long_df1.i$K*100) / sum(long_df1.i$K*100)         
   
   long_df1.i$habitat <- i
   #  # long_df1.i$habitat <- "A5.45"
   
   long_df1.i<-as.data.frame(long_df1.i)
   habitats_long <- rbind.data.frame(habitats_long, long_df1.i)
   habitats_long
   
 }
 
 habitats_long<<-as.data.frame(habitats_long)
 
 })
 habitats_long<<- as.data.frame({habs_long_prop()})
 habitats_long$K.i <- habitats_long$K
 
 
 ### RECOVERY VALUES PER HABITAT -----------
 
 # RECOVERY VALUES PER HABITAT: convert longevity values to recovery values in same data frame
 habitats_long$recovery_df.i <- (5.31 / habitats_long$species_longevity_x.i)
 habitats_long$recovery_df_lowerCI.i <- (2.31 / habitats_long$species_longevity_x.i)
 habitats_long$recovery_df_SG60.i <- (4.79 / habitats_long$species_longevity_x.i)
 habitats_long$recovery_df_SG80.i <- (4.32 / habitats_long$species_longevity_x.i)
 habitats_long$recovery_df_SG100.i <- (3.81 / habitats_long$species_longevity_x.i)
 
 habitat_rec_rates_1<<- reactive({ 
   
 # Convert to a single recovery rate per habitat type:        
 habitat_rec_rates_con1 <- dplyr::summarise(group_by(habitats_long, habitat),
                                            recovery_rate = weighted.mean(x=recovery_df.i, w= K.i,na.rm=TRUE),              
                                            recovery_rate_SG60 = weighted.mean(recovery_df_SG60.i, w= K.i,na.rm=TRUE), 
                                            recovery_rate_SG80 = weighted.mean(recovery_df_SG80.i, w= K.i,na.rm=TRUE), 
                                            recovery_rate_SG100 = weighted.mean(recovery_df_SG100.i, w= K.i,na.rm=TRUE))
 
 
  habitat_rec_rates_con1                                         
 
  
  habitat_rec_rates1 <<- as.data.frame(habitat_rec_rates_con1)
  
  
  
  })
 
 habitat_rec_rates1 <<- as.data.frame({habitat_rec_rates_1()})
 recoverydata<<-habitat_rec_rates1
 colnames(recoverydata)<-c("habitat", "recovery_dist", "recovery_dist_SG60", "recovery_dist_SG80", "recovery_dist_SG100")
 
 
 })
 
  # PLOTS BASED ON SLIDERS, NOT TABLE - Plot to see distribution of longevities
  # creating the longevity distribution
  long_df <- reactive({
    
    species_longevity_x <- seq(0.1,20,by=.1)

       slope = (input$m_slope.slide*exp(input$m_slope.slide * log(species_longevity_x)+input$b.slide))/(species_longevity_x * (exp(input$m_slope.slide*log(species_longevity_x) + input$b.slide) + 1)^2)      
    
  
  
    
    
    long_df1 <- cbind.data.frame(species_longevity_x, slope)
    long_df1$weights <- (long_df1$slope) / sum(long_df1$slope) # this gives a proportion between 0 and 1    
    long_df1 <<- data.frame(long_df1)
    
  })
  
  
  # Plot to see distribution of longevities
  output$long_distribution <-    renderPlot({
    longevity_dist <<- long_df()
    ggplot(long_df(), aes(species_longevity_x, weights)) + 
      geom_line(color = "coral1") + 
      labs(x="Species Longevity", y="Proportion") +
      geom_point(color="firebrick") +
      ggtitle("Proportional Distribution of Species Longevities") +
      theme(plot.title = element_text(size = 20, face = "bold",
                                      margin = margin(10, 0, 10, 0)))
    
  })
  
  # Generate corresponding recovery rates from longevities
  output$rec_distribution <-  renderPlot({
    
    # convert longevity values to recovery values
    recovery_df.slide <- 5.31 / long_df()$species_longevity_x
    recovery_df_lowerCI.slide <- 2.31 / long_df()$species_longevity_x
    recovery_df_SG60.slide <- 4.79 / long_df()$species_longevity_x
    recovery_df_SG80.slide <- 4.32 / long_df()$species_longevity_x
    recovery_df_SG100.slide <- 3.81 / long_df()$species_longevity_x
    
    
    
    ggplot(long_df(),aes(recovery_df.slide, weights)) +  
      geom_line(color = "coral1") +
      labs(x="Recovery Rate", y="Proportion") +
      geom_point(color="firebrick") +
      ggtitle("Proportional Distribution of Recovery Rates") +
      theme(plot.title = element_text(size = 20, face = "bold",
                                      margin = margin(10, 0, 10, 0)))
    
  })



  #### conditional recovery option 2 - input species data ####
  
  
  species_long_inFile <- reactive({
    
    if (is.null(input$species_long)) {
      return(NULL)
    } else {
      input$species_long
    }
    
  })
  
  species_long_data <- reactive({
    
    if (is.null(species_long_inFile())) {
      return(NULL)
    } else {
      
      read.csv(species_long_inFile()$datapath)

     # species_long_data <- read.csv("~/Teaching/2020 - 2021/MSc Projects/Fiona - Benthic tool vs MarESA/Western_Channel/Data/species_longevity_data.csv")

    }
    
  })
  


  
  observeEvent(input$load_longevity_dist_data, {
  
    species_long_data <- species_long_data()
    
    # Add in the various recovery rates
    species_long_data$RecoveryRate <- 5.31 / species_long_data$SpeciesLongevity
    species_long_data$RecoveryRate_lowerCI <- 2.31 / species_long_data$SpeciesLongevity
    species_long_data$RecoveryRate_SG60 <- 4.79 / species_long_data$SpeciesLongevity
    species_long_data$RecoveryRate_SG80 <- 4.32 / species_long_data$SpeciesLongevity
    species_long_data$RecoveryRate_SG100 <- 3.81 / species_long_data$SpeciesLongevity
    
    recoverydata <-  dplyr::summarise(group_by(species_long_data, HabitatName),
                                       recovery_rate = weighted.mean(x=RecoveryRate , w= ProportionOfBiomass ,na.rm=TRUE),
                                       mean_RecoveryRate_SG60 = weighted.mean(x=RecoveryRate_SG60 , w= ProportionOfBiomass ,na.rm=TRUE),
                                       mean_RecoveryRate_SG80 = weighted.mean(x=RecoveryRate_SG80, w= ProportionOfBiomass ,na.rm=TRUE),
                                       mean_RecoveryRate_SG100 = weighted.mean(x=RecoveryRate_SG100, w= ProportionOfBiomass ,na.rm=TRUE))
    
    recoverydata  <<- recoverydata


  output$species_longevity_data_head <-  renderTable({
  
    recovery_data_display <- dplyr::select(recoverydata, HabitatName,  recovery_rate, mean_RecoveryRate_SG60, mean_RecoveryRate_SG80, mean_RecoveryRate_SG100)
    colnames(recovery_data_display) <- c("Habitat Name",  "Recovery Rate", "SG60 Recovery Rate", "SG80 Recovery Rate", "SG100 Recovery Rate")
    recovery_data_display
     
  }, striped = TRUE, bordered = TRUE, 
  hover = TRUE)
  
  output$species_longevity_data_histogram <-  renderPlot({

    ggplot(data=species_long_data, aes(x=SpeciesLongevity, y=ProportionOfBiomass))+
      geom_bar(stat="identity", width=5)+
      facet_grid(~HabitatName)+
      labs(x="Species Longevity", y="Proportion of Biomass") + theme_bw()

  })
  
  
  }) # end of load species longevity data button
  
  
  

  ### conditional recovery option 3 - default ----------
        
  
  
  observeEvent(input$longevity_default_button, {  
    updateButton(session, "longevity_default_button", label = "Thanks. Next tab please", block = F, style = "success") 
    output$thank_text7<- renderText({paste("Thank you for your recovery rates. Please proceed to the next tab by clicking - 7. Benthic Status - at the top of the page.")})
  
    species_longevity_def <- seq(0,100,by=1)
    recovery_dist_con3<<-0.42
    recovery_dist_SG60_con3 <<- 0.35
    recovery_dist_SG80_con3 <<- 0.29
    recovery_dist_SG100_con3 <<- 0.21
    max_recovery_rate<-0.42
    max_recovery_rate_lowerCI<-0.42
    #recovery_dist_lowerCI<-0.42
    
    
      recovery_dist<<-recovery_dist_con3
      recovery_dist_SG60<<-recovery_dist_SG60_con3
      recovery_dist_SG80<<-recovery_dist_SG80_con3
      recovery_dist_SG100<<-recovery_dist_SG100_con3
    
      
### MAKING A PER HABITAT TABLE FOR RECOVERY RATES CON 3
      
     hab_tab_con3<<-reactive({
      habitat_rec_rates1<-as.data.frame(habitat_levels4$Habitat.Type)
      habitat_rec_rates1$recovery_dist <- rep(recovery_dist_con3,nrow(habitat_rec_rates1))
      habitat_rec_rates1$recovery_dist_SG60 <- rep(recovery_dist_SG60_con3,nrow(habitat_rec_rates1))
      habitat_rec_rates1$recovery_dist_SG80 <- rep(recovery_dist_SG80_con3,nrow(habitat_rec_rates1))
      habitat_rec_rates1$recovery_dist_SG100 <- rep(recovery_dist_SG100_con3,nrow(habitat_rec_rates1))
      colnames(habitat_rec_rates1)<-c("habitat", "recovery_dist", "recovery_dist_SG60", "recovery_dist_SG80", "recovery_dist_SG100")
      habitat_rec_rates<-habitat_rec_rates1
      
     })
    
     
     recoverydata<<-hab_tab_con3()
     # recoverydata <- habitat_rec_rates1
     
  })
    
  
  #------------------------------------------------------------------------------------------------------------------
  
  #### Benthic Status ####
  
  # Using an interactive leaflet map of RBS scores
  
  observeEvent(input$calculate_benthic_status, {
    
    updateButton(session, "calculate_benthic_status", label = "Loading... Please wait.", block = F, style = "success")
    
    cat(file=stderr(), paste(Sys.time(), "Preparing data for RBS calculation: start\n"))
    
    
    depletiondata <- habitat_levels4
    
    depletiondata$depletion <- depletiondata$depletion_used_in_tool
    
    # Extract the depletion value for each gear:habitat combination
    depletiondata$gear_habitat_identified <- paste(depletiondata$Gear, depletiondata$Habitat.Type, sep="_")
    depletiondata <- dplyr::select(depletiondata, gear_habitat_identified, depletion)
    
    #### changed to st_area from 'area' ####
    hab_uoa_indiv$grid_area=st_area(hab_uoa_indiv)/10^6 
    
    #SAR_per_habitat_fragment <- hab_uoa_indiv@data
    SAR_per_habitat_fragment <- hab_uoa_indiv
    
    SAR_per_habitat_fragment <- SAR_per_habitat_fragment[!is.na(SAR_per_habitat_fragment$SAR),]
    
    # overwrite the original aggregated vms data 'mydata' with the data for each habitat grid cell fragment instead of per whole grid cell
    mydata <- SAR_per_habitat_fragment
    
    # Create a column for the gear:habitat combination
    mydata$gear_habitat_identified <- paste(mydata$Gear, mydata$HabitatID, sep="_")
    
    # Add in the depletion rates to be used for each grid cell fragment, based on the hear:habitat combination
    mydata <- merge(mydata, depletiondata, by="gear_habitat_identified", all.x=TRUE)
    
    # Re-name columns in recovery values for each habitat
    colnames(recoverydata)<-c("HabitatID", "recovery_dist", "recovery_dist_SG60", "recovery_dist_SG80", "recovery_dist_SG100")
    recoverydata <<- recoverydata
    
    # Add the recovery values in for each grid cell fragment, based on habitat type.
    mydata <- merge(mydata, recoverydata, by="HabitatID", all.x=TRUE)
    
    mydata <<- mydata

    
    cat(file=stderr(), paste(Sys.time(), "Preparing data for RBS calculation: end\n"))

    
    cat(file=stderr(), paste(Sys.time(), "Calculating RBS values: start\n"))
    
    
    #### Calculating RBS ####
    
    RBS_function <-function(K,SAR,d,r){
      
      B = K * (1-(SAR * (d/r)))
      B[B < 0] <- 0 
      B
      
    }
    
    mydata$k <- 1
    
    mydata <- as.data.frame(mydata)
    mydata <- drop_units(mydata)
    
    # Calculate overall RBS
    RBS.data <- RBS_function(d=mydata$depletion,K=mydata$k,r=mydata$recovery_dist,SAR=mydata$SAR)
    RBS.data <- as.data.frame(RBS.data)
    colnames(RBS.data) <- "B"
    RBS.data <- cbind(mydata, RBS.data)
    
    # Calculate RBS for SG60 --------------------------
    RBS.data$B_SG60 <- RBS_function(d=mydata$depletion,K=mydata$k,r=mydata$recovery_dist_SG60,SAR=mydata$SAR)
    
    # Calculate RBS for SG80
    RBS.data$B_SG80 <- RBS_function(d=mydata$depletion,K=mydata$k,r=mydata$recovery_dist_SG80,SAR=mydata$SAR)
    
    # Calculate RBS for SG100 ------------------------------------
    RBS.data$B_SG100 <- RBS_function(d=mydata$depletion,K=mydata$k,r=mydata$recovery_dist_SG100,SAR=mydata$SAR)
    
    RBS.data <<- RBS.data
    
    # create mean RBS scores
    mean.rbs.scores <- RBS.data %>%
      dplyr::group_by(HabitatID) %>%
      dplyr::summarise(
        #Bt.mean = mean(B, na.rm=TRUE),
        Bt.weighted.mean = stats::weighted.mean(B, grid_area, na.rm=TRUE),
        total.area = sum(grid_area))
    mean.rbs.scores <- as.data.frame(mean.rbs.scores)
    colnames(mean.rbs.scores) <- c("Habitat Type","Mean RBS Score", "Total Area of Habitat Type (km^2)")
    
    
    
    cat(file=stderr(), paste(Sys.time(), "Calculating RBS values: end\n"))
    
    
    cat(file=stderr(), paste(Sys.time(), "Creating plots of RBS data: start\n"))
    

    # weighted histogram of RBS data
    output$rbs_histogram_weighted <- renderPlot({
      RBS_print<-ggplot(data=RBS.data, aes(x=B, weight = grid_area)) + geom_histogram(binwidth=0.05) + theme_bw()+ facet_wrap(~HabitatID, scales="fixed") +
        labs(x="Relative Benthic Status (RBS)", y="Habitat Area in km^2", title="Distribution of RBS Values")
  
      
      RBS_print
      
      })
    
    
    # create summary output table with both longevity distribution and max longevity
    output$rbs_summary <- renderTable({
  

      mean.rbs.scores
      
      }, striped = TRUE, bordered = TRUE,
      hover = TRUE)


  # Download mean RBS score per habitat
  output$downloadRBStable <- downloadHandler(
    filename = function() { paste("RBStable-", Sys.Date(), ".csv", sep='') },
    content = function(file) {
      write.csv(mean.rbs.scores, file=file)
    }
  )
    
   
    # merge hab_uoa_indiv with RBS values
    #hab_uoa_RBS<<- merge(hab_uoa_indiv, RBS.data, by.x="hab_cell_id_bgs", by.y="hab_cell_id_bgs", all.x=TRUE, all.y=TRUE, duplicateGeoms = TRUE)
    hab_uoa_RBS <<- hab_uoa_indiv %>% left_join(RBS.data, by="hab_cell_id_bgs")
    
  # Create interactive map of RBS values
  output$RBS_map1 <- renderLeaflet({

      pal <- colorNumeric(palette = "Reds",  domain = 0:1, na.color = NA, reverse = TRUE)#hab_uoa_RBS$B)

      
      leaflet(hab_uoa_RBS) %>% 
        addTiles() %>% 
        addPolygons(data=hab_uoa_RBS, weight = 0.8, 
                    color = ~pal(B), fillOpacity=0.9) %>% 
        addPolygons(data=hab_uoa_RBS, weight = 0.8,color = "#444444", fillColor = "transparent",
                    popup=as.character(round(hab_uoa_RBS$B,2))) %>%
        addLegend("bottomright", pal = pal, values = ~hab_uoa_RBS$B,
                  title = "Relative Benthic Status (RBS)",
                  opacity = 1)
    
    
  })
  cat(file=stderr(), paste(Sys.time(), "Creating plots of RBS data: end\n"))
  
  updateButton(session, "calculate_benthic_status", label = "All done!", block = F, style = "success")
  
  output$thank_text4 <- renderText({paste("Thank you for your input. Please proceed to the next tab once you have had a look at your Relative Benthic Status values by clicking - 8. Recovery Time - at the top of the page.")})
  }) 
  
  

  # Download for RBS histograms
  output$downloadRBShist <- downloadHandler(
    filename = function() { paste("RBShist-", Sys.Date(), '.png', sep='') },
    content = function(file) {
      RBS_hist<-ggplot(data=RBS.data, aes(x=B, weight = grid_area)) + geom_histogram(binwidth=0.05) + theme_bw()+ facet_wrap(~HabitatID) +
        labs(x="Relative Benthic Status (RBS)", y="Habitat Area in Km^2", title="Distribution of RBS Values")
      ggsave(file, plot = RBS_hist, type = "cairo-png")
    }
  )

  
  
  
  output$downloadRBSmap <- downloadHandler(
    
    filename = function() { paste("RBSmap-", Sys.Date(), '.png', sep='') },
    content = function(file) {
      pal <- colorNumeric(palette = "Reds",  domain = 0:1, na.color = NA, reverse = TRUE)
      rbs_map<-        leaflet(hab_uoa_RBS) %>% 
        addTiles() %>% 
        addPolygons(data=hab_uoa_RBS, weight = 0.8, 
                    color = ~pal(B), fillOpacity=0.9) %>% 
        addPolygons(data=hab_uoa_RBS, weight = 0.8,color = "#444444", fillColor = "transparent",
                    popup=as.character(round(hab_uoa_RBS$B,2))) %>%
        addLegend("bottomright", pal = pal, values = ~hab_uoa_RBS$B,
                  title = "Relative Benthic Status (RBS)",
                  opacity = 1)
      mapshot(rbs_map, file=file)
      
    }
  )
  
  

# ----------------------------------------------------------------------------------------------------------------  
  
  
  #### Recovery Time ####
  
  observeEvent(input$calculate_recovery_time, {

    updateButton(session, "calculate_recovery_time", label = "Loading... Please wait.", block = F, style = "success")
    
    
    # Create row IDs
    RBS.data$value.Id <- 1:nrow(RBS.data)
    
    # If RBS is zero, set to 0.01 to allow for some recovery
    RBS.data$B[RBS.data$B==0]=0.01 
    RBS.data$B_SG60[RBS.data$B_SG60==0]=0.01 
    RBS.data$B_SG80[RBS.data$B_SG80==0]=0.01 
    RBS.data$B_SG100[RBS.data$B_SG100==0]=0.01 
    
    cat(file=stderr(), paste(Sys.time(), "Calculating recovery: start\n"))
    
    
    # Generate mean recovery trajectory
    recovery_function <-function(B, rec_rates){
      
      B0=B
      # B0=RBS.data$B
      
      K <- 1
      time=seq(0,20,0.25) #0,20,0.1
      
      B.start=expand.grid(time,B0,K)
      B.start$cell.ID.long=rep(seq(1:nrow(RBS.data)),rep(length(time),nrow(RBS.data)))    
      
      names(B.start)=c("time","B0","K","cell.ID")
      

      B.start <- merge(B.start, rec_rates, by.x="cell.ID", by.y="value.Id")
      colnames(B.start)[5] <- "r"
      
      B.start$Bt=B.start$B0*B.start$K/{B.start$B0+(B.start$K-B.start$B0)*exp(-B.start$r*B.start$time)}
      
      Bt <<- B.start$Bt
      
      B.start[,c("cell.ID","time", "B0", "r","Bt")]
      
      
    } # end of recovery_function
 

    # Use recovery function to generate recovery trajectyory for mean RBS and recovery rates, and SG 60, SG80 annd SG100 values

    
    recovery.data <- recovery_function(B=RBS.data$B, rec_rates=dplyr::select(RBS.data, value.Id, recovery_dist) )

    recovery.data$Bt_SG60 <- recovery_function(B=RBS.data$B_SG60, rec_rates=dplyr::select(RBS.data, value.Id, recovery_dist_SG60) )[,5]
    recovery.data$Bt_SG80 <- recovery_function(B=RBS.data$B_SG80, rec_rates=dplyr::select(RBS.data, value.Id, recovery_dist_SG80)  )[,5]
    recovery.data$Bt_SG100 <- recovery_function(B=RBS.data$B_SG100, rec_rates=dplyr::select(RBS.data, value.Id, recovery_dist_SG100)  )[,5]
    
    
    

    cat(file=stderr(), paste(Sys.time(), "Calculating recovery: end\n"))
    
    
    
    
    cat(file=stderr(), paste(Sys.time(), "Post-processing of recovery data: start\n"))
    

    
    RBS.data.rec <- dplyr::select(RBS.data, value.Id, hab_cell_id_bgs, HabitatID, grid_area)
    
    recovery.data <- merge(recovery.data, RBS.data.rec, by.x="cell.ID", by.y="value.Id", all.x=TRUE, all.y=TRUE)
    recovery.data <<- recovery.data
    
    # Calculate mean recovery trajectory and for each SG score
    mean.recovery <- recovery.data %>%
      group_by(time, HabitatID) %>%
      dplyr::summarise(Bt.mean = weighted.mean(Bt,grid_area, na.rm=TRUE),
                       Bt.sd = sd(Bt, na.rm=TRUE),
                       Bt.n = n(),
                       Bt.SG60.mean = weighted.mean(Bt_SG60,grid_area, na.rm=TRUE),
                       Bt.SG60.sd = sd(Bt_SG60, na.rm=TRUE),
                       Bt.SG80.mean = weighted.mean(Bt_SG80,grid_area, na.rm=TRUE),
                       Bt.SG80.sd = sd(Bt_SG80, na.rm=TRUE),
                       Bt.SG100.mean = weighted.mean(Bt_SG100,grid_area, na.rm=TRUE),
                       Bt.SG100.sd = sd(Bt_SG100, na.rm=TRUE)
      )
    
    mean.recovery <<- mean.recovery
   
     
     cat(file=stderr(), paste(Sys.time(), "Post-processing of recovery data: end\n"))
     

     cat(file=stderr(), paste(Sys.time(), "Creating plots of recovery data: start\n"))
     
     
      output$recovery_plot <- renderPlot({


        recovery_print<- ggplot() +
          geom_line(data=recovery.data, aes(x=time, y=Bt, group=factor(cell.ID)), col="grey") +
          facet_wrap(~HabitatID) +
          geom_line(data = mean.recovery, aes(x=time, y = Bt.mean, col="Mean"),linewidth=1.5) +
          geom_line(data = mean.recovery, aes(x=time, y = Bt.SG60.mean, col="SG60"), alpha=0.8, linewidth=1.5) +
          geom_line(data = mean.recovery, aes(x=time, y = Bt.SG80.mean, col="SG80"), alpha=0.8, linewidth=1.5) +
          geom_line(data = mean.recovery, aes(x=time, y = Bt.SG100.mean, col="SG100"), alpha=0.8, linewidth=1.5) +
          ylim(0,1) + theme_bw() +
          labs(x="Time (Years)", y="Relative Benthic Status",
               title=paste("Predicted Recovery Trajectories per Habitat Type") ) +
          geom_hline(yintercept=0.8, lty=2) +
          scale_color_manual(values = c(
            'Mean' = 'black',
            'SG60' = 'green',
            "SG80" = "blue",
            "SG100" = "orange")) +
          labs(color = 'Recovery Trajectories') +
          theme(text = element_text(size=16)) 
        
        recovery_print
       
         

       }) # end of recovery_plot
      
      
      cat(file=stderr(), paste(Sys.time(), "Creating plots of recovery data: end\n"))
      

     
      
      
      
      
      cat(file=stderr(), paste(Sys.time(), "Generating time taken to recover for mean, SG60, SG80 and SG100: start\n"))
      
      

        
       
### Calculating the mean recovery values to make final table ####
      
      Tover80 <- data.frame(time=double()) # Create empty dataframe called Tover80 (i.e. showing time taken for the RBS score to reach over 80 or 0.8)
      
      for(i in unique(mean.recovery$HabitatID)){
        Tover80.temp <- data.frame()
        
        # i <- "A5.15"
        mean.recovery.temp <- dplyr::filter(mean.recovery, HabitatID == i)     # Scroll through each habitat type
        
        
        if(mean.recovery.temp$Bt.mean[1] >= 0.8) {   
          Tover80.temp <- data.frame(time = 0)         # if starting rbs is over 0.8 then recovery time is zero
        }   else { 
          Tover80.temp <- dplyr::filter(mean.recovery.temp, (Bt.mean >= 0.8))[1,1]    # extract where the RBS is over 0.8 and extract the time value (i.e. column 1, row 1)
        }
        
        if(mean.recovery.temp$Bt.SG60.mean[1] >= 0.8) {   
          Tover80.temp.SG60 <- data.frame(time_SG60 = 0)         # if starting rbs is over 0.8 then recovery time is zero
        }   else { 
          Tover80.temp.SG60 <- dplyr::filter(mean.recovery.temp, (Bt.SG60.mean >= 0.8))[1,1]    # extract where the RBS is over 0.8 and extract the time value (i.e. column 1, row 1)
          colnames(Tover80.temp.SG60) <- "time_SG60"
        }
        
        if(mean.recovery.temp$Bt.SG80.mean[1] >= 0.8) {   
          Tover80.temp.SG80 <- data.frame(time_SG80 = 0)         # if starting rbs is over 0.8 then recovery time is zero
        }   else { 
          Tover80.temp.SG80 <- dplyr::filter(mean.recovery.temp, (Bt.SG80.mean >= 0.8))[1,1]    # extract where the RBS is over 0.8 and extract the time value (i.e. column 1, row 1)
          colnames(Tover80.temp.SG80) <- "time_SG80"
        }
        
        if(mean.recovery.temp$Bt.SG100.mean[1] >= 0.8) {   
          Tover80.temp.SG100 <- data.frame(time_SG100 = 0)         # if starting rbs is over 0.8 then recovery time is zero
        }   else { 
          Tover80.temp.SG100 <- dplyr::filter(mean.recovery.temp, (Bt.SG100.mean >= 0.8))[1,1]    # extract where the RBS is over 0.8 and extract the time value (i.e. column 1, row 1)
          colnames(Tover80.temp.SG100) <- "time_SG100"
        }
        
        
        Tover80.temp <- cbind.data.frame(Tover80.temp,Tover80.temp.SG60, Tover80.temp.SG80, Tover80.temp.SG100)
        
        #if(!is.na(Tover80.temp)) { Tover80.temp <- "20+"}                           # if RBS doesnt reach 0.8 within 20 years (i.e. as far as model predicts) set as default 999 (you can then process this to whatever you want it to be afterwards)
        Tover80.temp <- as.data.frame(Tover80.temp)
        colnames(Tover80.temp)[1] <- "mean_time"
        Tover80.temp$habitat <- i                                # Create a habitat column with whichever habitat type is currenlty being processed
        Tover80 <- rbind.data.frame(Tover80, Tover80.temp)       # join this habitat type to the whole dataframe of all habitats
        
        # If a habitat doesnt recover in 20 years, would have value of NA - set these values as 21.
        Tover80[is.na(Tover80)]<-21
        
      }
            
            
      finaltable <- Tover80
      colnames(finaltable)<-c("Mean", "SG60", "SG80", "SG100", "Habitat")
      finaltable <- dplyr::select(finaltable, Habitat, Mean, SG60, SG80, SG100)
      
      
      cat(file=stderr(), paste(Sys.time(), "Generating time taken to recover for mean, SG60, SG80 and SG100: end\n"))
        
        
      cat(file=stderr(), paste(Sys.time(), "Calculating MSC scores: start\n"))
        
      
#### Assign MSC Scores to recovery values ####
      
      MSCscore <- data.frame(time=double()) # Create empty dataframe called MSCscore
      
      for(i in unique(finaltable$Habitat)){
        MSCscore.temp <- data.frame()
        
        finaltable.temp <- dplyr::filter(finaltable, Habitat == i)     # Scroll through each habitat type
        
        if (finaltable.temp$SG100[1] <= 20) {    
          MSCscore.temp <- data.frame(MSC_Score = "SG100")} 
        
        if  (finaltable.temp$SG80[1] <= 20 & finaltable.temp$SG100[1] > 20) {
          MSCscore.temp<-data.frame(MSC_Score = "SG80")}
        
        if (finaltable.temp$SG60[1] <= 20 & finaltable.temp$SG80[1] > 20) {
          MSCscore.temp<-data.frame(MSC_Score = "SG60")} 
        
        if (finaltable.temp$SG60[1] > 20) {
          MSCscore.temp<-data.frame(MSC_Score = "Fail") } 
        
        finaltable.temp <- cbind.data.frame(finaltable.temp, MSCscore.temp)
        MSCscore.temp <- finaltable.temp
        
        MSCscore <- rbind.data.frame(MSCscore, MSCscore.temp)       # join this habitat type to the whole dataframe of all habitats
      }
      
      # If recovery is 21 years, change to read as '20+ years'
      MSCscore[(MSCscore)==21]<-"20+ years"
      
      colnames(MSCscore)<-c("Habitat Name", "Mean Time to Recovery (years)", "SG60 time to recovery", "SG80 time to recovery", "SG100 time to recovery",  "Suggested MSC Score")
      
        
        
        output$recoverytime_table<-renderTable ({MSCscore}, striped = TRUE, bordered = TRUE,
                                                hover = TRUE)
        
        updateButton(session, "calculate_recovery_time", label = "All done!", block = F, style = "success")
        output$finished_text <- renderText("Tool Finished Running. Assessment Complete.")
        

        mydatatable<<-as.data.table(MSCscore)
        
        cat(file=stderr(), paste(Sys.time(), "Calculating MSC scores: end\n"))
        
        
  }) # end of reactive last tab  
  

  
   ### download report
   output$downloadData<-downloadHandler(
     filename = function() { paste("MSC-Score-table-", Sys.Date(), ".csv", sep="") },
     content = function(file) {
       
       write.csv(mydatatable, file=file, row.names = FALSE)
       
     }
   )
   
   # Download for recovery plots
   
   output$downloadrecoveryplot <- downloadHandler(
     filename = function() { paste("recovery-plots-", Sys.Date(), '.png', sep='') },
     content = function(file) {
       recovery_plot <-  ggplot() +
         geom_line(data=recovery.data, aes(x=time, y=Bt, group=factor(cell.ID)), col="grey") +
         facet_wrap(~HabitatID) +
         geom_line(data = mean.recovery, aes(x=time, y = Bt.mean, col="Mean"),linewidth=1.5) +
         geom_line(data = mean.recovery, aes(x=time, y = Bt.SG60.mean, col="SG60"), alpha=0.8, linewidth=1.5) +
         geom_line(data = mean.recovery, aes(x=time, y = Bt.SG80.mean, col="SG80"), alpha=0.8, linewidth=1.5) +
         geom_line(data = mean.recovery, aes(x=time, y = Bt.SG100.mean, col="SG100"), alpha=0.8, linewidth=1.5) +
         ylim(0,1) + theme_bw() +
         labs(x="Time (Years)", y="Relative Benthic Status",
              title=paste("Predicted Recovery Trajectories per Habitat Type") ) +
         geom_hline(yintercept=0.8, lty=2) +
         scale_color_manual(values = c(
           'Mean' = 'black',
           'SG60' = 'green',
           "SG80" = "blue",
           "SG100" = "orange")) +
         labs(color = 'Recovery Trajectories') +
         theme(text = element_text(size=12)) 
       
       ggsave(file, plot = recovery_plot, type = "cairo-png", width = 20, height = 20, units = "cm")
     }
   )
  
}

shinyApp(ui = ui, server = server)
