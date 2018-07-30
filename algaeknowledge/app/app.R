#--------------------------------------------------------------------------
#App file for
#www.algaeknowledge.org
#Created By: C Meghan Downes, PhD
#Funded By: USDOE Regional Algal Feedstock Testbed Project
#-------------------------------------------------------------------------

#------------------
#Date: 04-20-2018
#------------------

#-----------------------Start---------------------------------------------


#-----------------------Load Shiny Libraries------------------------------

#load the initial required shiny libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(V8)

#other packages that are required
library(dplyr)
library(tidyverse)

#library(chron)
library(datasets)
library(dygraphs)
library(ggplot2)
library(DT)
library(corrplot)
library(psych)
library(plot3D)
library(scales)
library(stargazer)
library(xts)

library(data.table)
library(RColorBrewer)

library(Hmisc)
library(readxl)
library(tidyxl)
library(unpivotr)

#-----------------------setting up DropBox for Shiny Apps io--------------

library(rdrop2)
library(httpuv)

#save in the app itself
token <- drop_auth()
saveRDS(token, "droptoken.rds")

#--------------------------------------
#This is run once to generate the oAuth
#      --DO NOT RUN--
#token <- drop_auth(new_user = TRUE,
#           key = "mmhfsybffdom42w",
#           secret = "l8zeqqqgm1ne5z0",
#           cache = TRUE, rdstoken = NA)
#saveRDS(token, "droptoken.rds")
#--------------------------------------



#-----------------------global settings-----------------------------------

options(shiny.maxRequestSize = 30 * 1024 ^ 2)
`%then%` <- shiny:::`%OR%`

#-----------------------Set up the directory structure for the app--------

#This is set up for DropBox as the storage for the data
#setwd("algae")
filenames  <- drop_dir(path = "algae") %>%
  select("path_lower")
xlsfilenames <- drop_dir(path = "algae") %>%
  select("name")
dashfilenames <- drop_dir(path = "algaedash") %>%
  select("path_lower")
outputDir <- "algae"
expoutdir <- "algaeoutput"
dashoutdir <- "algae"

#-------------------------------------------------------------------------

#-----------------------Helper Functions----------------------------------




#--------------------------------------------------------------------------------------------------
#----------------------------UI--------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------




header <- dashboardHeader(title = "EASyR")

#----------------------------Sidebar---------------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "Home",
      tabName = "dashboard",
      icon = icon("home")
    ),
    menuItem(
        "Upload",
        tabName = "uploaddata",
        icon = icon("wrench")
      ),
    menuItem(
        "Document",
        tabName = "documentdata",
        icon = icon("bug")
      ),
   menuItem(
        "Extract",
        tabName = "extractdata",
        icon = icon("eye")
      ),
   
      #menuSubItem(
       # "Edit",
      #  tabName = "editdata",
      #  icon = icon("eye")
    #  )
    #),
   menuItem(
        "Visualize",
        tabName = "graphicalexploration",
        icon = icon("eye")
      ),
     # menuSubItem(
      #  "Describe",
    #    tabName = "descriptivestats",
    #    icon = icon("wrench")
    #  ),
    #  menuSubItem(
    #    "Construct",
    #    tabName = "construct",
    #    icon = icon("wrench")
    #  ),
    menuItem(
        "Model",
        tabName = "modeling",
        icon = icon("wrench")
      )
   
    #menuItem(
    #  "Share", 
    #  tabName = "modeling", 
    #  icon = icon("wrench")
    #),
    #menuItem(
    #  "Archive", 
     # tabName = "archive", 
    #  icon = icon("university")
    #)
  )
) #dashboard sidebar end
#----------------------------Body------------------------------------------------------------------

body <-  dashboardBody(
  tabItems(
    #--------------------------Dashboard Main Page---------------------------------------------------
    tabItem(
      tabName = "dashboard",
      fluidRow(
        box(
          width =12,
          h1("Explore-Analyze-Share with R"), 
          p("An Open Source R/Shiny Data Exploration and Visulation Tool")
        )
      ),
      fluidRow(
        box(
          width = 6,
          h1("Explore"),
          h3("Upload-Download-Document-Clean-Extract"),
          tags$ol(
            tags$li("The power of R without ever writing a line of code"),
            tags$li("Find common data cleaning and visualization tools"),
            tags$li("Quick, powerful statistical tools for scientists and students"),
            tags$li("View and explore our experimental data"),
            tags$li("Upload and share your own data, models, and visualizations"),
            tags$li("RStudio's shinyapps.io provides the builds, hosting, and updates for R"),
            tags$li("The app is viewable both on mobile and desktop and most browsers/OS"),
            tags$li("Create new results using our tools and data and export for your use.")
          )
        ),
        box(
          width = 6,
          h1("Analyze"),
          h3("View-Plot-Summarize-Calculate-Model"),
          tags$ol(
            tags$li("Create scatter plots with a variety of options"),
            tags$li("Or use the time plotting features"),
            tags$li("Visualize the shape of your data using density functions"),
            tags$li("Or go `old school` and view historgrams with options"),
            tags$li("If that isn't enough, explore your data with bi-variate plots."),
            tags$li("Run descriptive statistics on your data or ours"),
            tags$li("Clean and create data that R can analyze"),
            tags$li("Run regressions and calculate selected functions for biomass growth.")
          )
        )
      ),
      fluidRow(
        box(
          width = 6,
          h1("Share"),
          h3("Save-Delete-Export-Post"),
          tags$ol(
            tags$li("Upload your data to our DropBox folders to share"),
            tags$li("Download your finished data, plots, and analyses for your own publications"),
            tags$li("Play with our data, our use your own"),
            tags$li("You can annotate and document your data files and save the finished sets"),
            tags$li("You control your data, and you can remove any of your files from our DropBox"),
            tags$li("Each function has a pre-built example that loads when you click on to the page"),
            tags$li("Interactive boxes and selections read the data and respond to user inputs"),
            tags$li("Most pages have some instructions and tips")
          )
        ),
        box(
          width = 6,
          h1("y-R?"),
          h3("We Stand on the Shoulders of Giants"),
          tags$ol(
            tags$li("The power of R, with the power of cloud sharing"),
            tags$li("Using open source to deliver scientific knowledge to the public"),
            tags$li("No paywalls, no subscriptions"),
            tags$li("No advertisements"),
            tags$li("Archived on DropBox and GitHub"),
            tags$li("Active Development of new features, packages and tools"),
            tags$li("Sharing experimental data to expand knowledge"),
            tags$li("Standing on the shoulders of giants, and giving back to the public domain")
          )
        )
      ),
      fluidRow(
        box(
          width = 12,
          h1("Legalese-Ish"),
          p("This is an open source project, and all code and data are free,
            and the code for this project is available on GitHub. No warranty or promises
            are made and users are solely responsible for their conduct and results. 
            C. Meghan Downes is solely reponsible for the development  and maintenance
            of this site. The project was originally funded by the US Department of Energy
            under the RAFT algae project. US DOE, University of Arizona, New Mexico State University
            have not endorsed this and the content and commentary do not reflect any official stance
            of  anyone or any kind. Please contact the author at cara.downes.7@gmail.com")
          
          
          )
        )
      ),
    
    #tabItem dashboard end
    
    
    
    #---------------------------Add New RAFT Data Page-----------------------------------------------
    
    tabItem(
      tabName = "uploaddata",
      fluidRow(
            box(
                width = 4,
                h1("Upload It"),
                tags$strong("Any Excel file with an *.xls or *xlsx extension can be read*"),
                br(),
                h3("Description"),
                p("This page allows the user to read in and store all the information in a
                  spreadsheet.  All tabs/sheets in the file will be imported into the app.
                  The data will be stored as an R data file (*Rda)."),
                p("After you have uploaded it, you can edit and extract data from it.
                  You can use the data to model and visualize, and you can
                  save or export your data, models, and visualizations.
                  The data can be shared publicly or removed after your session is over. 
                  So, upload a file and see your data in action."),
                br(),
                tags$em("*You will  have the ability to delete your file 
                        from the DropBox folder.",
                        style = "color:blue")
                ),
              box(
                width = 4,
                h1("Select It"),
                fileInput(
                  "datafile",
                  "Choose a file to view*",
                  multiple = FALSE
                ),
                h3("Notes"),
                p("You will see a preview of the data below once loaded.
                  All sheets, variables, and formulas are stored and usable."),
                p("The full file is stored with each cell of the spreadsheet as a 
                  separate row with 21 different types of information. 
                  The file is read from the first sheet row by column then
                  all sheets with any data will be read row by column."),
                tags$em("To View the entire file click on the EXTRACT tab",
                        style = "color:blue")
                ),
              box(
                width = 4,
                h1("Name It"),
                textInput("filename", "Provide a descriptive file name*"),
                h3("Important"),
                p("Use a name that provides descriptive information. 
                  Do not add extensions or file type designations.
                  Underscore is the only special character allowed.
                  Lower case and upper case are allowed. Numbers are accepted.  
                  Do not use spaces. Useful names inlcude an author or institution, an experiment or subject,
                  and some type of date reference."),
                tags$strong("For example:"),
                tags$em("`DownesAlgae1501_06192018`"),
                tags$em("would be acceptable", style = "color:green"),
                tags$em("`Downes Algae 06/19/2018 exp. 1501`"),
                tags$em("would not", style = "color:red"),
                br(),
                br(),
                actionButton("submit", "Upload!",
                             icon("bug"), style = "color:white; background-color: blue;
                             border-color: #2e6daf")
                )
              ),
            fluidRow(
              box(
                width = 12,
                title = "Summary of Your Data",
                verbatimTextOutput("uploadtable")
              )
            )
          ), # Add new data tab item end
    
    
    
    #----------------------------Document Experiment Information Tab------------------------------------
    
    tabItem(tabName = "documentdata",
            fluidRow(
              box(
                width = 12,
                title = "Describe Your Experiment",
                
                fluidRow(
                  box(
                    width = 6,
                    textInput("expname", "Experiment Name"),
                    helpText("provide a descriptive experiment title")
                  ),
                  box(
                    width = 6,
                    textInput("batchname", "Batch ID"),
                    helpText("create a unique experiment identifier code")
                  ),
                  box(
                    width = 6,
                    textInput("strain", "Strain ID"),
                    helpText("provide the RAFT common strain identifier")
                  ),
                  
                  box(
                    width = 6,
                    textInput("recipe", "Recipe ID"),
                    helpText("provide the RAFT recipe name/identifier")
                  ),
                  box(
                    width = 6,
                    textInput("reactor", "Reactor ID"),
                    helpText("add the reactor name/ID")
                  ),
                  box(
                    width = 6,
                    textInput("site", "Site ID"),
                    helpText("provide the site identification information")
                  )
                ),
                fluidRow(box(
                  width = 12,
                  textInput("objective", "Briefly Describe the Objectives of the Experiment"),
                  helpText("briefly provide 1-3 objectives/goals of the experiment")
                )),
                fluidRow(box(
                  width = 12,
                  actionButton("saverecord", "Submit?"),
                  helpText(
                    "Clicking Submit will save your entry and load the RAFT experimental records table"
                  )
                )),
                fluidRow(box(
                  width = 12,
                  title = "RAFT Experimental Records",
                  div(style = 'overflow-X: Scroll',
                      DT::dataTableOutput("expresponses"))
                  
                ))
              )
            )),
    # document experiment info tab end
    
    #----------------------------Extract Data------------------------------------------------------
    tabItem(tabName = "extractdata",
            fluidRow(
               box(
                 width = 6,
                 h1("We Can Read...Excel"),
                 p("Use this page to read in an Excel file from DropBox and 
                   extract data to view and analyze.
                   This page uses the R packages (tidyr, tidyxl, unpivotr) and
                   allows you to read in an entire spreadsheet (warts and all)
                   and extract columns and rows to create a dataset suitable
                   for regression, plotting, and calculations. The power of this
                   method is that the entire contents of the spreadsheet are preserved
                   and contained in the cloud and different components can be viewed
                   and analyzed -- including comments, formulas, and descriptions. 
                   You never have to bother with saving each tab/sheet separately
                   or saving as a delimited file or any other nonsense. Make your
                   selections and you will see a preview of the data in the file.")
                 ),
               box(
                 width = 6,
                 h1("How To Use:"),
                 tags$ol(
                   tags$li("Select a file and sheet to view"),
                   tags$li("Define the start and end rows"),
                   tags$li("Define the start and end columns"),
                   tags$li("Tell R if you have labels/headers"),
                   tags$li("You can have up to two headers from
                           which to create variable names"),
                   tags$li("The label location needs a `direction` relative to
                           the selected data"),
                   tags$li("N = Above, E = Right, W = Left, and S = Below"),
                   tags$li("Then you will see a preview of the data available 
                           on that sheet")
                   )
                 
                   )
               ),
             fluidRow(
               box(
                 width = 3,
                 h3("Define your data"),
                 selectInput("xlsnames", "Select Data File to Munge", xlsfilenames, 
                             selected = "08-AMBatchNB-210.xlsx"),
                 uiOutput("sheetnum"),
                 numericInput("rowstart", "Row Start", value = 19),
                 numericInput("rowend", "Row End", value = 30),
                 numericInput("colstart", "Column Start", value = 1),
                 numericInput("colend", "Column End", value = 25),
                 selectInput("extractheader",
                             "Labels?",
                             c("none", "N", "S", "E", "W"),
                             multiple = FALSE,
                             selected = "N"),
                 selectInput("extractheader2",
                             "2nd Labels (units)?",
                             c("none", "N", "S", "E", "W"),
                             multiple = FALSE,
                             selected = "N")
               ),
               box(
                 width = 3,
                 h3("Define the variables"),
                 uiOutput("extractvar1"),
                 uiOutput("extracttype1"),
                 uiOutput("extractvar2"),
                 uiOutput("extracttype2"),
                 uiOutput("extractvar3"),
                 uiOutput("extracttype3"),
                 uiOutput("extractvar4"),
                 uiOutput("extracttype4")
               ),
               box(
                 width = 6,
                 h3("Summary of your Excel Data"),
                 verbatimTextOutput("question")
               )
             ),
             fluidRow(
               box(
                 width = 6,
                 h3("Preview of Selected Data"),
                 #div(style = 'overflow-X:Scroll',
                 #   DT::dataTableOutput("xlsdattable"))
                 verbatimTextOutput("xlsdattable")
               ),
               box(
                 width = 6,
                 h3("Expert Tip"),
                 tags$ol(
                   tags$li("Try changing `variable type` to `formula`
                           to view any formulas that were used."),
                   tags$li("The Excel reader saves each cell of the spreadsheet
                           as a single row in a list with 21 characteristics."),
                   tags$li("The variable type selector changes the type of data
                           extracted from the spreadsheet."),
                   tags$li("You can view if you have blanks, or errors,
                           or formulas, or formatting.")
                   ),
                 h3("Next Step -- Save and Use"),
                 tags$ol(
                   tags$li("If you like what you see in the preview on 
                           the left, then click `Save` to be able to use
                           this data"),
                   tags$li("Once satisfied, click around the app to see your data in 
                           action."),
                   tags$li("If you don't like what you see change your options
                           above."),
                   tags$li("Or, if you really don't like what you see, send an
                           email to the author of this app at cara.downes.7@gmail.com")
                   )
                   )
                 ),
             fluidRow(
               box(
                 width = 3,
                 h3("Save Me!"),
                 actionButton("extractsave", "Save?"),
                 br(),
                 br(),
                 p("This will save your selections as a data file
                               that will be stored in DropBox and accessed by
                               this app.")
               ),
               box(
                 width = 3,
                 h3("Export Me!"),
                 actionButton("extractexport", "Export?"),
                 br(),
                 br(),
                 p("This will save your data creation as an Excel file
                               on your computer to use however you see fit.")
               )
             )
          ), 
    
    
    #tabpanelend xls
    
    
    
    #----------------------------Graphical Analysis Page-----------------------------------------------
    
    tabItem(tabName = "graphicalexploration",
            fluidRow(
              tabBox(
                width = 12,
                id = "tabset1",
                height = "250px",
                side = "left",
                type = "pills",
                tabPanel(
                  "Scatter Plots",
                  title = "Scatter Me",
                  fluidRow(
                    box(width = 12,
                        plotOutput("scatterplot")
                    )
                  ),
                  fluidRow(box(
                    width = 12,
                    br(),
                    actionButton(
                      "gplotgo",
                      "PlotMe!",
                      icon("bug"),
                      style = "color:white; background-color: green;
                      border-color: #2e6daf"
                    ),
                    br(),
                    helpText("Select Options Below then Click to Create Plot.")
                    
                  )),
                  fluidRow(
                    box(
                      width = 6,
                      title = "Choose Data File and Variables",
                      selectInput("sgdat1", "Select A File to View", filenames,
                                  selected = "/algaedash/asamplefile_gplotraft2015data.csv"),
                      uiOutput("gdatvarx"),
                      uiOutput("gdatvary")
                    ),
                    box(
                      width = 6,
                      title = "Choose Options for Plot",
                      uiOutput("colorgroup"),
                      uiOutput("sizegroup"),
                      uiOutput("shapegroup")
                    )
                  ),
                  fluidRow(
                    box(
                      width = 6,
                      title = "Name Your Plot",
                      textInput("dscattername", "Enter Plot Name"),
                      helpText("Choose a Name Wisely, It Doesn't Matter.")
                    ),
                    
                    box(
                      width = 6,
                      title = "Save Your Plot",
                      br(),
                      downloadButton("dscatterplot", "Download Your Scatter Plot"),
                      br(),
                      br(),
                      helpText("To Save a Copy of Your Plot to your Computer Click Download")
                      
                    )
                  ),
                  fluidRow(box(
                    width = 12,
                    title = "Selected Data",
                    div(style = 'overflow-X: Scroll',
                        DT::dataTableOutput("gplotdat"))
                  ))
                ),
                tabPanel(
                  "Time Series Plots",
                  title = "Time Flies",
                  fluidRow(
                    box(width = 12,
                        plotOutput("dyplot")
                    )
                  ),
                  fluidRow(
                    box(
                      width = 12,
                      actionButton("dygo", "Plot!",
                                   icon("bug"), style = "color:white; background-color: green;
                                   border-color: #2e6daf"),
                      br(),
                      helpText("Select Options Below then Click to Plot.")
                      ),
                    box(width = 4,
                        title = "Choose Your Data!",
                        selectInput("sdydash1", "Select a Data File to View", filenames,
                                    selected = "/algaedash/asamplefile_gplotraft2015data.csv"),
                        helpText("This Page Only Creates Time Series Plots.")
                    ),
                    box(width = 4,
                        title = "You Must Choose a Valid Date Variable",
                        uiOutput("datevar1"),
                        helpText("WARNING: Date must be formated as: MM/DD/YYYY H:M:S")
                    ),
                    box(width = 4,
                        title = "Choose WHAT Information You Want Plotted",
                        uiOutput("dvar1"),
                        helpText("You Can Choose Multiple Variables with the Same Unit of Measure.")
                    )
                  ),
                  fluidRow(
                    box(width = 6,
                        title = "Name Me to Save Me!",
                        textInput("dyplotname", "Enter Plot Name"),
                        helpText("Choose A Name Wisely, Or Not.")
                        
                    ),
                    box(width = 6,
                        title = "Save Your Plot Locally If Desired if You are A LocalDataVore.",
                        br(),
                        downloadButton("ddyplot", "Download Plot"),
                        br(),
                        br(),
                        helpText("Clicking Will Save Your Plot as a PDF to Your Computer.")
                    )
                  ),
                  fluidRow(
                    box(width = 12,
                        title = "Selected Data",
                        div(style = 'overflow=x:Scroll',
                            DT::dataTableOutput("selecteddy"))
                    )
                  ),
                  fluidRow(
                    box(width = 12,
                        title = "Transformed Times Series Data",
                        div(style = 'overflow=x:Scroll',
                            DT::dataTableOutput("dytable"))
                    )
                  )
                ), #timeseries end
                tabPanel(
                  "Density Plots",
                  title = "How Dense Are You",
                  fluidRow(
                    box(width = 12,
                        plotOutput("densityplot")
                    )
                  ),
                  fluidRow(
                    box(width = 12,
                        br(),
                        actionButton("dplotgo", "PlotMe!",
                                     icon("bug"),
                                     style = "color:white; background-color: green;
                                     border-color: #2e6daf"),
                        br(),
                        helpText("Make Selections Below then Click to Plot!")
                        )
                  ),
                  fluidRow(
                    box(width = 6,
                        title = "Choose Data and Variable to View.",
                        selectInput("sddat1", "Select A File to View", filenames,
                                    selected = "/algaedash/asamplefile_gplotraft2015data.csv"),
                        uiOutput("ddatvarx")
                    ),
                    box(width = 6,
                        title = "Select A Grouping Variable for Color.",
                        uiOutput("dcolorgroup"),
                        br(),
                        helpText("This Allows You to Look at Grouping in a Distribution."),
                        br()
                    )
                  ),
                  fluidRow(
                    box(width = 6,
                        title = "Give It a Name!",
                        textInput("ddensityname", "Enter Plot Name"),
                        helpText("What's in a Name?")
                    ),
                    box(width = 6,
                        title = "If I Please You, Save Me!",
                        br(),
                        downloadButton("ddensityplot", "Click to Save Local Download"),
                        br(),
                        br(),
                        helpText("If you Love Local Data, Click to Save Plot as a PDF Image.")
                    )
                  ),
                  fluidRow(
                    box(
                      width = 12,
                      title = "Selected Data",
                      div(style = 'overflow-X: Scroll',
                          DT::dataTableOutput("dplotdat")
                      )
                    )
                  )
                ),
                
                
                tabPanel(
                  "Histogram Plots",
                  title = "What Shape Are You",
                  fluidRow(
                    box(width = 12,
                        plotOutput("histogramplot")
                    )
                  ),                  
                  fluidRow(
                    box(
                      width = 12,
                      br(),
                      actionButton(
                        "hplotgo",
                        "PlotMe!",
                        icon("bug"),
                        style = "color:white; background-color: green;
                        border-color: #2e6daf"
                      ),
                      br(),
                      helpText("Select Options then Click to Plot.")
                    )
                  ),
                  fluidRow(
                    box(width = 6,
                        title = "Choose Your Data (Or Poison)",
                        selectInput("shdat1", "Select A File to View", filenames,
                                    selected = "/algaedash/asamplefile_gplotraft2015data.csv"),
                        uiOutput("hdatvarx")
                    ),
                    box(width = 6,
                        title = "Choose a Grouping Variable to Color By, If Desired",
                        uiOutput("hcolorgroup")
                    )
                  ),
                  fluidRow(
                    box(width = 6,
                        title = "Please, Give Me a Name",
                        textInput("dhistname", "Enter a Name"),
                        helpText("I Think You Know the Drill")
                    ),
                    box(width = 6,
                        title = "If You Like Me, Save Me"),
                    downloadButton("dhistogram", "Download Plot")
                  ),
                  fluidRow(
                    box(width = 12,
                        title = "Selected Data",
                        div(style = 'overflow-X: Scroll',
                            DT::dataTableOutput("hplotdat")
                        )
                    )
                  ) 
                ),
                
                tabPanel(
                  "Line Charts",
                  title = "Sometimes we need more than Dygraphs",
                  fluidRow(
                    box(width = 12,
                        plotOutput("lineplot")
                    )
                  ),
                  fluidRow(
                    box(width = 12,
                        br(),
                        actionButton("lplotgo", "PlotMe!", icon("bug"),
                                     style = "color:white; background-color: green;
                                     border-color: #2e6daf"),
                        br(),
                        helpText("Select Your Options, then Click to Plot")
                        )
                  ),
                  fluidRow(
                    box(width = 6,
                        title = "Please Select a Data File and Variables",
                        selectInput("sldat1", "Select A File to View", filenames,
                                    selected = "/algaedash/asamplefile_gplotraft2015data.csv"),
                        uiOutput("ldatvarx"),
                        uiOutput("ldatvary")
                    ),
                    box(
                      width = 6,
                      title = "Select Some Options for Your Plot",
                      uiOutput("lcolorgroup"),
                      uiOutput("lsizegroup"),
                      uiOutput("lshapegroup")
                    )
                  ),
                  fluidRow(
                    box(width = 6,
                        title = "Name Me, Why Don't You",
                        textInput("dlinename", "...Waiting"),
                        helpText("What is In a Name?")
                    ),
                    box(width = 6,
                        title = "Will You Save Me?",
                        downloadButton("dline", "Download Plot"),
                        br(),
                        br(),
                        helpText("To Save Plot as PDF Image Please Click")
                    )
                  ),
                  fluidRow(
                    box(width = 12,
                        title = "Selected Data",
                        div(style = 'overflow-X: Scroll',
                            DT::dataTableOutput("lplotdat")
                        )
                    )
                  )
                )
              )
            )
    ),
    #tab item page Graphical Exploration End
    
    
    
    tabItem(tabName = "modeling",
            
            fluidRow(
              box(
                         width = 12,
                         title = "OLS",
                         selectInput("smlmdat", "Select Data File to View", filenames,
                                     selected = "/algaedash/asamplefile_gplotraft2015data.csv" )
                             
                           )
                         ),
                         fluidRow( 
                           box(
                             width = 4,
                             uiOutput("mlhsvar")
                           ),
                           box(
                             width = 4,
                             uiOutput("mrhsvar1")
                           ),
                           box(
                             width = 4,
                             
                             br(),
                             actionButton("mregressiongo", "RegressMe!", icon("bug"),
                                          style = "color:white; background-color: green;
                                          border-color: #2e6daf"),
                             br()
                             )
                         ),
                         fluidRow(
                           box(
                             width = 6,
                             htmlOutput(outputId = "mregsum"),
                             br()
                           ),
                           box(
                             width = 6,
                             plotOutput("lmplot")
                           )
                         ),
                         fluidRow(
                           box(
                             width = 6,
                             title = "Name Me, Why Don't You",
                             textInput("dmregressname", "...Waiting"),
                             helpText("What is In a Name?"),
                             downloadButton("downloadmregress", "Download Regression"),
                             br(),
                             br(),
                             helpText("To Save Model Results Please Click")
                           ),
                           box(width = 6,
                               title = "Will You Save Me?",
                               textInput("dregressplotname", "...Waiting"),
                               helpText("What is In a Name?"),
                               downloadButton("downloadregressplot", "Download Plot"),
                               br(),
                               br(),
                               helpText("To Save Model Results Please Click")
                           )
                         ),
                         fluidRow(
                           box(
                             width = 12,
                             title = "Selected Data",
                             div(style = 'overflow-X:Scroll',
                                 DT::dataTableOutput("mregressiondat"))
                           )
                         )
                         
                ) #modeling tab end
                  
    
    #-----------------------------End Tabs-------------------------------------------------------------
    
      ) # all tabItems end) #body end
    )

ui <- dashboardPage(header, sidebar, body)

#----------------------------UI End----------------------------------------------------------------


#--------------------------------------------------------------------------------------------------
#----------------------------Server----------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  #----------------------------Data Upload File Functions---------------------------
  
  infile <- eventReactive(input$datafile, {
    infile <- input$datafile
    xlsx_cells(
      infile$datapath
      
    )
    
  })
  
  #save public
  saveData <- function(data) {
    data <- infile()
    fileName <- paste(input$filename, '.rda', sep = '')
    filePath <- file.path(tempdir(), fileName)
    saveRDS(data, filePath)
    drop_upload(filePath, path = outputDir)
  }
  
  observeEvent(input$submit, {
    saveData(infile())
  }) #save public end
  
  output$uploadtable <- renderPrint(infile())
  
  
  #----------------------------Document Experiment Functions-----------------------------------------
  
  # Save an experiment record
  
  
  expfields <-
    c("expname",
      "batchname",
      "strain",
      "recipe",
      "reactor",
      "site",
      "objective")
  
  # Whenever a field is filled, aggregate all form data
  expformData <- eventReactive(input$saverecord, {
    data <- sapply(expfields, function(x)
      input[[x]])
    data
  })
  
  expsaveData <- function(data) {
    data <- t(data)
    fileName <-
      sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
    filePath <- file.path(tempdir(), fileName)
    write.csv(data, filePath, row.names = FALSE, quote = TRUE)
    drop_upload(filePath, path = expoutdir)
  }
  
  
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$saverecord, {
    expsaveData (expformData())
  })
  
  # Load all previous responses
  exploadData <- function() {
    filesInfo <- drop_dir(expoutdir)
    filePaths <- filesInfo$path_lower
    data <- lapply(filePaths, drop_read_csv)
    data <- do.call(rbind, data)
    data
  }
  
  # (update with current response when Submit is clicked)
  expsubmitClicked <-
    eventReactive(input$saverecord, {
      exploadData()
    })
  
  # Show the previous responses
  output$expresponses <- DT::renderDataTable({
    input$saverecord
    exploadData()
  })
  
  
  #----------------------------Graphical Exploration Plot Functions--------------------------------
  
  #Scatter Plots--------------------------------
  
  #create scatter plot graphical exploration file selector
  
  gdat1 <- eventReactive(input$sgdat1, {
    isolate({
      gdat1 <- drop_read_csv(input$sgdat1, stringsAsFactors = FALSE)
      gdat1 <- na.omit(gdat1)
    })
    
    gdat1
  })
  
  #create UI Outputs for variables and plot options
  
  output$gdatvarx <- renderUI({
    df <- gdat1()
    items = names(df)
    names(items) = items
    selectInput("gdatvarx", "Select Your X-Variable:", c("none", items))
  })
  
  output$gdatvary <- renderUI({
    df <- gdat1()
    if (is.null(df))
      return(NULL)
    items = names(df)
    names(items) = items
    selectInput("gdatvary", "Select Your Y-Variable:", c("none", items))
  })
  
  output$colorgroup <- renderUI({
    df <- gdat1()
    if (is.null(df))
      return(NULL)
    items = names(df)
    names(items) = items
    selectInput("colorgroup", "Select a Color Grouping:", c("none", items))
  })
  
  output$sizegroup <- renderUI({
    df <- gdat1()
    if (is.null(df))
      return(NULL)
    items = names(df)
    names(items) = items
    selectInput("sizegroup", "Select a Size Grouping:", c("none", items))
  })
  
  output$shapegroup <- renderUI({
    df <- gdat1()
    if (is.null(df))
      return(NULL)
    items = names(df)
    names(items) = items
    selectInput("shapegroup", "select a shape grouping:", c("none", items))
  })
  
  
  #Render the scatter plot object
  
  #
  default_gdf <- reactive({
    df <-
      drop_read_csv("/algaedash/asamplefile_gplotraft2015data.csv",
                    stringsAsFactors = FALSE)
    df <- na.omit(df)
  })
  
  
  output$gplotdat <- DT::renderDataTable({
    input$sgdat1
    gdat1()
  })
  
  
  output$scatterplot <-
    renderPlot({
      input$gplotgo
      if (input$gplotgo == FALSE) {
        plotdata <- default_gdf()
        p <- ggplot(plotdata, aes(OD750, OD680))
        p + geom_point(aes(color = Batch)) +
          labs(x = "OD750") + labs(y = "OD680") +
          labs(title = "A Sample Plot") +
          labs(color = "Batch")
        
      } else if (!input$gplotgo == "") {
        isolate({
          plotdata <- gdat1()
          p <-
            ggplot(plotdata,
                   aes_string(
                     x = input$gdatvarx,
                     y = input$gdatvary,
                     scale = T
                   ))
          p <- p + geom_point()
          if (input$colorgroup != "none")
            p <- p + aes_string(color = input$colorgroup)
          if (input$sizegroup != "none")
            p <- p + aes_string(size = input$sizegroup)
          if (input$shapegroup != "none")
            p <- p + aes_string(shape = input$shapegroup)
          p
        })
        
        
      } else {
        NULL
      }
    })
  
  
  dscattername <- reactive({
    input$dscattername
  })
  
  output$dscatterplot <- downloadHandler(
    filename = function() {
      paste0(dscattername(), '.pdf', sep = '')
      
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "pdf")
    }
  )
  #----------------------Scatter End--------------------
  
  #------------Dygraph----------------------------------------------
  
  #Create a default dygraph to show while they select items
  
  default_ddf <- reactive({
    df <- drop_read_csv("/algaedash/asamplefile_gplotraft2015data.csv",
                        stringsAsFactors = FALSE)
    df <- na.omit(df) 
    df <- df %>%
      select(OD750, OD680, AFDW, Observation_Date_Time)
    datefmt <- as.POSIXct(df[[4]], origin = "1904-1-01", format = "%m/%d/%Y%H:%M")
    df <- cbind(df, datefmt)
    df <- df %>%
      mutate(
        year = year(datefmt),
        month = month(datefmt),
        week= week(datefmt),
        daymonth = mday(datefmt)
      ) %>%
      select(OD750, OD680, AFDW, datefmt)
    df <- melt(df, id = "datefmt")
    df
  })
  
  #create file selectors
  
  dydat1 <- eventReactive(input$sdydash1, {
    drop_read_csv(input$sdydash1, stringsAsFactors = FALSE)
  })
  
  #munge the file to create xts dygraph
  
  dposix1 <- eventReactive(input$dygo, {
    df <- dydat1()
    df <- na.omit(df)
    df <- df %>% select(input$datevar1, input$dvar1)
    datefmt <- as.POSIXct(df[[1]], origin = "1904-1-01", format = "%m/%d/%Y%H:%M")
    df <- cbind(df, datefmt)
    df <- df %>%
      select(input$dvar1, datefmt)
    df <- melt(df, id = "datefmt")
    df
    
  })
  
  
  
  #Render the date variable and date selector uiOutputs
  
  
  output$datevar1 <- renderUI({
    df <- dydat1()
    if (is.null(df))
      return(NULL)
    items = names(df)
    names(items) = items
    selectInput("datevar1", "select your date variable:", items)
  })
  
  output$dvar1 <- renderUI({
    df <- dydat1()
    if (is.null(df))
      return(NULL)
    items = names(df)
    names(items) = items
    selectInput(
      "dvar1",
      "select time series variables to plot on the vertical axis:",
      items,
      multiple = TRUE
    )
  })
  
  
  
  #Create the Dashboard dygraphs
  
  output$dyplot <- renderPlot({
    input$dygo
    if (input$dygo == FALSE) {
      plotdata <- default_ddf()
      p <- ggplot(plotdata, aes(datefmt, value)) 
      p + geom_line(aes(color = variable)) 
      
    } else if (!input$dygo == "") {
      isolate({
        plotdata <- dposix1()
        p <- ggplot(plotdata, aes_string(x = "datefmt", y = "value")) 
        p <- p + geom_line(aes_string(color = "variable")) 
        p
        
      })
    } else {
      NULL
    }
    
  })
  
  output$dytable <- DT::renderDataTable({
    input$sdydash1
    dposix1()
    
  })
  
  output$selecteddy <- DT::renderDataTable({
    input$sdydash1
    dydat1()
  })
  
  dyplotname <- reactive({
    input$dyplotname
  })
  
  output$ddyplot <- downloadHandler(
    filename = function() {
      paste0(dyplotname(), '.pdf', sep = '')
      
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "pdf")
    }
  )
  
  #-----------------Dygraph End--------------------------
  
  #----------------------------Density Plot-----------------------------------------------
  
  #create density plot graphical exploration file selector
  
  ddat1 <- eventReactive(input$sddat1, {
    df <- drop_read_csv(input$sddat1, stringsAsFactors = FALSE)
    df <- na.omit(df)
  })
  
  #create UI Outputs for variables and plot options
  
  output$ddatvarx <- renderUI({
    df <- ddat1()
    items = names(df)
    names(items) = items
    selectInput("ddatvarx", "Select Your X-Variable:", c("none", items))
  })
  
  output$dcolorgroup <- renderUI({
    df <- ddat1()
    if (is.null(df))
      return(NULL)
    items = names(df)
    names(items) = items
    selectInput("dcolorgroup", "Select a Color Grouping:", c("none", items))
  })
  
  #Render the density plot object
  
  #
  default_gdf <- reactive({
    df <-
      drop_read_csv("/algaedash/asamplefile_gplotraft2015data.csv",
                    stringsAsFactors = FALSE)
    df <- na.omit(df)
  })
  
  output$dplotdat <- DT::renderDataTable({
    input$ddat1
    ddat1()
  })
  
  
  output$densityplot <-
    renderPlot({
      input$dplotgo
      if (input$dplotgo == FALSE) {
        plotdata <- default_gdf()
        p <- ggplot(plotdata, aes(OD750))
        p + geom_density(aes(
          fill = Batch,
          alpha = .3
        )) +
          labs(x = "OD750") +
          labs(title = "A Sample Plot") +
          labs(color = "Batch")
        
      } else if (!input$dplotgo == "") {
        isolate({
          plotdata <- ddat1()
          p <- ggplot(plotdata, aes_string(x = input$ddatvarx))
          p <- p + geom_density()
          if (input$dcolorgroup != "none")
            p <-
            p + aes_string(
              fill = input$dcolorgroup,
              alpha = .3,
              scale = TRUE
            )
          
          p
        })
        
        
      } else {
        NULL
      }
    })
  
  
  
  ddensityname <- reactive({
    input$ddensityname
  })
  
  output$ddensityplot <- downloadHandler(
    filename = function() {
      paste0(ddensityname(), '.pdf', sep = '')
      
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "pdf")
    }
  )
  
  #----------------End Density-----------------
  
  #----------create the histogram plot tab page items------------------------------
  
  #create histogram plot graphical exploration file selector
  
  hdat1 <- eventReactive(input$shdat1, {
    df <- drop_read_csv(input$shdat1, stringsAsFactors = FALSE)
    df <- na.omit(df)
  })
  
  #create UI Outputs for variables and plot options
  
  output$hdatvarx <- renderUI({
    df <- hdat1()
    items = names(df)
    names(items) = items
    selectInput("hdatvarx", "Select Your X-Variable:", c("none", items))
  })
  
  output$hcolorgroup <- renderUI({
    df <- hdat1()
    if (is.null(df))
      return(NULL)
    items = names(df)
    names(items) = items
    selectInput("hcolorgroup", "Select a Color Grouping:", c("none", items))
  })
  
  #Render the histogram plot object
  
  default_gdf <- reactive({
    df <-
      drop_read_csv("/algaedash/asamplefile_gplotraft2015data.csv",
                    stringsAsFactors = FALSE)
    df <- na.omit(df)
  })
  
  output$hplotdat <- DT::renderDataTable({
    input$hdat1
    hdat1()
  })
  
  output$histogramplot <-
    renderPlot({
      input$hplotgo
      if (input$hplotgo == FALSE) {
        plotdata <- default_gdf()
        p <- ggplot(plotdata, aes(OD750))
        p + geom_histogram(aes(
          fill = Batch,
          alpha = .3
          
        )) +
          labs(x = "OD750") +
          labs(title = "A Sample Plot") +
          labs(color = "Batch")
        
      } else if (!input$hplotgo == "") {
        isolate({
          plotdata <- hdat1()
          p <- ggplot(plotdata, aes_string(x = input$hdatvarx))
          p <- p + geom_histogram()
          if (input$hcolorgroup != "none")
            p <-
            p + aes_string(
              fill = input$hcolorgroup,
              alpha = .3,
              scale = TRUE
            )
          
          p
        })
        
        
      } else {
        NULL
      }
    })
  
  
  dhistname <- reactive({
    input$dhistname
  })
  
  output$dhistogram <- downloadHandler(
    filename = function() {
      paste0(dhistname(), '.pdf', sep = '')
      
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "pdf")
    }
    
    
    
  )
  
  #-------------------End Histogram------------
  
  
  #----------Create the line plot tab page items------------------------
  
  #create line plot graphical exploration file selector
  
  ldat1 <- eventReactive(input$sldat1, {
    df <- drop_read_csv(input$sldat1, stringsAsFactors = FALSE)
    df <- na.omit(df)
  })
  
  #create UI Outputs for variables and plot options
  
  output$ldatvarx <- renderUI({
    df <- ldat1()
    items = names(df)
    names(items) = items
    selectInput("ldatvarx", "Select Your X-Variable:", c("none", items))
  })
  
  output$ldatvary <- renderUI({
    df <- ldat1()
    if (is.null(df))
      return(NULL)
    items = names(df)
    names(items) = items
    selectInput("ldatvary", "Select Your Y-Variable:", c("none", items))
  })
  
  output$lcolorgroup <- renderUI({
    df <- ldat1()
    if (is.null(df))
      return(NULL)
    items = names(df)
    names(items) = items
    selectInput("lcolorgroup", "Select a Color Grouping:", c("none", items))
  })
  
  output$lsizegroup <- renderUI({
    df <- ldat1()
    if (is.null(df))
      return(NULL)
    items = names(df)
    names(items) = items
    selectInput("lsizegroup", "Select a Size Grouping:", c("none", items))
  })
  
  output$lshapegroup <- renderUI({
    df <- ldat1()
    if (is.null(df))
      return(NULL)
    items = names(df)
    names(items) = items
    selectInput("lshapegroup", "Select a Shape Grouping:", c("none", items))
  })
  
  #Render the line plot object
  default_gdf <- reactive({
    df <-
      drop_read_csv("/algaedash/asamplefile_gplotraft2015data.csv",
                    stringsAsFactors = FALSE)
    df <- na.omit(df)
  })
  
  output$lplotdat <- DT::renderDataTable({
    input$sldat1
    ldat1()
  })
  
  output$lineplot <-
    renderPlot({
      input$lplotgo
      if (input$lplotgo == FALSE) {
        plotdata <- default_gdf()
        p <- ggplot(plotdata, aes(OD750, OD680))
        p + geom_line(aes(color = Batch)) +
          labs(x = "OD750") + labs(y = "OD680") +
          labs(title = "A Sample Plot") +
          labs(color = "Batch")
        
      } else if (!input$lplotgo == "") {
        isolate({
          plotdata <- ldat1()
          p <-
            ggplot(plotdata,
                   aes_string(
                     x = input$ldatvarx,
                     y = input$ldatvary,
                     scale = T
                   ))
          p <- p + geom_line()
          if (input$lcolorgroup != "none")
            p <- p + aes_string(color = input$lcolorgroup)
          if (input$lsizegroup != "none")
            p <- p + aes_string(size = input$lsizegroup)
          if (input$lshapegroup != "none")
            p <- p + aes_string(shape = input$lshapegroup)
          p
          print(p)
        })
        
        
      } else {
        NULL
      }
    })
  
  dlinename <- reactive({
    input$dlinename
  })
  
  output$dline <- downloadHandler(
    filename = function() {
      paste0(dlinename(), '.pdf', sep = '')
      
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "pdf")
      
    }
  )
  
  
  #--------------End Line Plot
  
  #--------------------------------End Graphical Exploration Page----------------------------
  
  
  
  
  #---------------------Modeling Page-------------------------------------
  
  
  
  #OLS----
  
  #select a data file to view
  mrdat <- eventReactive(input$smlmdat, {
    df <- drop_read_csv(input$smlmdat, stringsAsFactors = FALSE)
  })
  
  #render the summary by variable selector
  output$mlhsvar <- renderUI({
    df <- mrdat()
    items = names(df)
    names(items) = items
    selectInput("mlhsvar",
                "Choose Your Dependent (LHS) Variable:",
                c("none", items),
                multiple = FALSE)
  })#render lhs variable end
  
  output$mrhsvar1 <- renderUI({
    df <- mrdat()
    items = names(df)
    names(items) = items
    selectInput("mrhsvar1",
                "Choose Your Independent (RHS) Variables:",
                c("none", items),
                multiple = FALSE)
  })#render rhs variables end
  
  regresults <- eventReactive(input$mregressiongo, {
    df <- mrdat()
    df <- na.omit(df)
    df <- df %>% select(input$mlhsvar, 
                        input$mrhsvar1)
    df <- as.data.frame(df)
    df
    df <- lm(as.formula(paste(input$mlhsvar, "~",
                              paste(input$mrhsvar1, collapse = "+"),
                              sep = "")
    ),
    data = df
    )
    df
    regresults <- stargazer(df, type = "html",  
                            dep.var.labels = c(input$mlhsvar),
                            title = "Regression Results",
                            float = TRUE)
  })
  
  model_default <- reactive({
    
    df <- drop_read_csv("/algaedash/asamplefile_gplotraft2015data.csv",
                        stringsAsFactors = FALSE)
    df <- na.omit(df)
  })
  
  reg_default <- reactive({
    df <- model_default()
    df <- na.omit(df)
    df <- df %>% select(AFDW, 
                        OD750)
    df <- as.data.frame(df)
    
    df
    
    
  })
  
  
  
  output$lmplot <- renderPlot({
    input$mregressiongo
    if(input$mregressiongo == FALSE) {
      plotdata <- reg_default()
      
      m <- lm(AFDW~OD750, data = plotdata)
      a <- signif(coef(m)[1], digits = 3)
      b <- signif(coef(m)[2], digits = 3)
      r2 = format(summary(m)$r.squared, digits = 3)
      textlab <- paste("Y = ", b, "X +", a, "\n", "", "", "R^2 = ", r2, "     ", sep = "")
      
      p <- ggplot(plotdata, aes(OD750, AFDW))
      p <- p +geom_point()
      p <- p + stat_smooth(method = "lm", color = "red")
      p <- annotate("text", label = paste(textlab), x = -Inf, y = Inf,
                    hjust = -0.5,
                    vjust = 3,
                    color = "blue",
                    size = 5)
      
    } else if (!input$mregressiongo == "") {
      isolate({
        
        plotdata <- mrdat()
        plotdata <- na.omit(plotdata)
        plotdata <- plotdata %>% select(input$mlhsvar, 
                                        input$mrhsvar1)
        plotdata <- as.data.frame(plotdata)
        #plotdata
        
        m <- lm(as.formula(paste(input$mlhsvar, "~",
                                 paste(input$mrhsvar1, collapse = "+"),
                                 sep = "")
        ),
        data = plotdata
        )
        #plotdata
        #plotdata$predicted <- predict(m)
        #plotdata$residuals <- residuals(m)
        #plotdata
        
        a <- signif(coef(m)[1], digits = 2)
        b <- signif(coef(m)[2], digits = 2)
        r2 = format(summary(m)$r.squared, digits = 3)
        textlab <- paste("Y = ", b, "X +", a, "\n", "R^2 = ", r2, "       ", sep = "")
        
        p <- ggplot(plotdata, aes_string(x = input$mrhsvar1, y = input$mlhsvar))
        p <- p + geom_point() 
        p <- p + stat_smooth(method = "lm", color = "blue") 
        p <- p + annotate("text", label = paste(textlab), x = -Inf, y = Inf, 
                          hjust = -0.5,
                          vjust = 3,
                          color = "blue",
                          size = 5) 
        p
        print(p)
        
      })
    } else {
      NULL
    }
  })
  
  
  output$mregsum <- renderPrint({regresults()}) 
  
  output$mregressiondat <- DT::renderDataTable({
    input$smlmdat
    mrdat()
    
  })
  
  
  dregressname <- reactive({
    input$dmregressname
  })
  output$downloadmregress <- downloadHandler(
    filename = function() {
      paste(input$dmregressname, ".html", sep = "")
    },
    content = function(file) {
      write(regresults(), file)
    }
  )
  
  
  dregressplotname <- reactive({
    input$dregressplotname
  })
  output$downloadregressplot <- downloadHandler(
    filename = function() {
      paste(input$dregressplotname, ".pdf", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "pdf")
    }
  )
  #Multi-Variable OLS end------
  
  #----ReadXLSX and Parse------
  
  
  
  
  readxlsdata <- function() {
    x <- input$xlsnames
    x <- sprintf("%s", x)
    my_files <- drop_search(x)
    xx1<- drop_download(my_files$matches[[1]]$metadata$path_lower, 
                        overwrite = TRUE)
    xlsnames <- my_files$matches[[1]]$metadata$path_lower
    name2 <- paste(substring(xlsnames, 8, 50))
    xlsdat<- xlsx_cells(name2)
    xlsdat
  }
  
  
  
  xlsdata <- reactive({
    df <- readxlsdata()
    df
    
  })
  
  xlsdata3 <- reactive({
    df <- xlsdata()
    df
    df1 <- df %>% select(sheet)
    df1 <- unique(df1)
    df1
  })
  
  output$sheetnum <- renderUI({
    df <- xlsdata() %>% 
      select(sheet) 
    df <-  unique(df)
    selectInput("sheetnum",
                "Choose Your Sheet to View:",
                choices = df,
                multiple = FALSE,
                selected = "Raw Data")
  })
  
  xlsdata1 <- reactive({
    df <- xlsdata() %>% 
      filter(row %in% input$rowstart:input$rowend, col %in% input$colstart:input$colend) %>%
      filter(is_blank == FALSE) %>%
      filter(sheet == input$sheetnum) %>%
      behead(input$extractheader, header1) %>%
      behead(input$extractheader2, header2) %>%
      unite(col = header, header1, header2, sep = "_")
    df
  })
  
  xlsdata2 <- reactive({
    df <- xlsdata() %>% 
      filter(row %in% input$rowstart:input$rowend, col %in% input$colstart:input$colend) %>%
      filter(is_blank == FALSE) %>%
      filter(sheet == input$sheetnum) %>%
      behead(input$extractheader, header) %>%
      behead(input$extractheader2, header2) %>%
      select(header, header2, data_type)
    df
    df1 <- unique(df)
    df1 <- as.data.table(df1) 
    df1 <- df1 %>%  arrange(header)
    df1 <- as.data.table(df1)
    df1
  })
  
  output$extracttype1 <- renderUI({
    df <- xlsdata1() 
    items = names(df)
    names(items) = items
    selectInput("extracttype1",
                "What Type of Data is This?",
                c(items),
                multiple = FALSE,
                selected = "date")
  })#render rhs variables end
  
  
  output$extracttype2 <- renderUI({
    df <- xlsdata1() 
    items = names(df)
    names(items) = items
    selectInput("extracttype2",
                "What Type of Data is This?",
                c(items),
                multiple = FALSE,
                selected = "numeric")
  })#render end
  
  output$extracttype3 <- renderUI({
    df <- xlsdata1() 
    items = names(df)
    names(items) = items
    selectInput("extracttype3",
                "What Type of Data is This?",
                c(items),
                multiple = FALSE,
                selected = "numeric")
  })#render end
  
  output$extracttype4 <- renderUI({
    df <- xlsdata1() 
    items = names(df)
    names(items) = items
    selectInput("extracttype4",
                "What Type of Data is This?",
                c(items),
                multiple = FALSE,
                selected = "numeric")
  })#render end
  
  output$extractvar1 <- renderUI({
    df <- xlsdata1() %>% 
      select(header) 
    df<-  unique(df)
    selectInput("extractvar1",
                "Choose Your First Variable:",
                choices = df,
                multiple = FALSE,
                selected = "Date_Date")
  })#r
  
  output$extractvar2 <- renderUI({
    df <- xlsdata1() %>% 
      select(header) 
    df<-  unique(df)
    selectInput("extractvar2",
                "Choose Your Second Variable:",
                choices = df,
                multiple = FALSE,
                selected = "Avg OD_OD 750")
  })#r
  
  output$extractvar3 <- renderUI({
    df <- xlsdata1() %>% 
      select(header) 
    df<-  unique(df)
    selectInput("extractvar3",
                "Choose Your Third Variable:",
                choices = df,
                multiple = FALSE,
                selected = "Biomass _g/L")
  })#r
  
  output$extractvar4 <- renderUI({
    df <- xlsdata1() %>% 
      select(header) 
    df<-  unique(df)
    selectInput("extractvar4",
                "Choose Your Fourth Variable:",
                choices = df,
                multiple = FALSE,
                selected = "DO%_%")
  })#r
  
  tabledf <- function(){
    df <- xlsdata1()
    
    df1 <- df %>%
      filter(header == input$extractvar1) %>%
      spread(header, input$extracttype1) %>%
      select(input$extractvar1)
    
    df2 <- df %>%
      filter(header == input$extractvar2) %>%
      spread(header, input$extracttype2) %>%
      select(input$extractvar2)
    
    df3 <- df %>%
      filter(header == input$extractvar3) %>%
      spread(header, input$extracttype3) %>%
      select(input$extractvar3)
    
    df4 <- df %>%
      filter(header == input$extractvar4) %>%
      spread(header, input$extracttype4) %>%
      select(input$extractvar4)
    
    dft <- cbind(df1, df2, df3, df4)
    
    dft
    
    
  }
  
  xlstable <- reactive ({
    df <- tabledf()
    df
    
  })
  
  
  
  
  output$question <- renderPrint(xlsdata2())
  
  output$atibble <- renderPrint(xlsdata3())
  
  output$xlsdattable <- renderPrint(as.tibble(xlstable()))
  
  
  #output$xlsdattable <- DT::renderDataTable({
  # xlstable()
  #})
  ############################################
  
} #server end?

shinyApp(ui, server)





#-------------------------------------------------------------------------
#-----------------------End-----------------------------------------------
#-------------------------------------------------------------------------

# Run the application
shinyApp(ui = ui, server = server)


#-----------------------References----------------------------------------

#https://stackoverflow.com/questions/3777174/plotting-two-variables-as-lines-using-ggplot2-on-the-same-graph

#organize files and version control for shinyappsio
#http://www.open-meta.org/technology/creating-linked-github-rstudio-and-shinyapps-io-projects/

#update package to weatherdata
#https://github.com/ALShum/rwunderground/blob/master/README.md


#persistent storage with shiny
#https://shiny.rstudio.com/articles/persistent-data-storage.html#dropbox

#shiny apps io deployment
#https://stackoverflow.com/questions/36878256/shiny-app-deployment-error-cannot-change-working-directory
