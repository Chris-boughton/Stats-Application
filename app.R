# Self Service Statistics Application
# Author: Chris Boughton
# Objective: To provide scientific statistical analysis through an easy to use application. Options should be provided  
# to input the data, data types, variables and other metrics in order to correctly select the best statistical test to  
# use. This should then produce a series of graphs, tables and worded paragraphs to justify the test chosen and present
# the data back to the user, in a clear and concise way, that highlights potential findings, significance and outputs.

# Building a function to check of packages in a given list are installed, where if they are not then they are forced to 
# install with the appropriate dependencies to remove package requirement errors
package_check_install <- function(package_list){
  new_package <- package_list[!(package_list %in% installed.packages()[, "Package"])]
  if (length(new_package)) 
    install.packages(new_package,
                     dependencies = TRUE)
  sapply(package_list,
         require,
         character.only = TRUE)
  }
# Selecting the packages we need for the work
packages_required <- c("shiny",
                       "shinydashboard",
                       "shinyjs",
                       "shinyWidgets",
                       "rintrojs",
                       "readxl",
                       "janitor",
                       "rstudioapi",
                       "fontawesome",
                       "dplyr",
                       "tidyr",
                       "openxlsx2",
                       "ggplot2",
                       "scales",
                       "reactable",
                       "plotly")
# Running the function to install and load packages as required
package_check_install(packages_required)
for(pkg in packages_required){
  library(pkg,
          character.only = TRUE)
  }

# Global options to force NA's to be blank in excel workbook outputs
options("openxlsx2.na.strings" = "")

# Global check of active users on the app
users = reactiveValues(count = 0)

# Create button to close the app
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# Customising the UI of the app
ui <- dashboardPage(
  dashboardHeader(
    title = div(style = "display: flex; justify-content: space-between; align-items: center;",
                tags$h2("Self Service Statistics",
                        style = "font-size: 40px; font-weight: bold; margin-top: 0px; margin-left: 300px;")),
    tags$li(class = "dropdown",
            tags$style(".sidebar-toggle{margin-left:-570px;}")),
    titleWidth = "800px",
    tags$li(class = "dropdown",
            tags$h6(uiOutput("connected_users"),
                    style = "font-size: 12px; color: white; margin-top: 17px;")),
    tags$li(actionLink("closeApp",
                       label = "",
                       icon = icon("times")),
            class = "dropdown")
    ),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Home", 
                         tabName = "home",
                         icon = icon("home")),
                menuItem("Theory", 
                         tabName = "theory",
                         icon = icon("book", 
                                     lib = "font-awesome")),
                menuItem("Data", 
                         tabName = "data",
                         icon = icon("database", 
                                     lib = "font-awesome"))
                )
    ),
  dashboardBody(
    # Customising the UI
    tags$head(tags$style(HTML("
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #024c5f;
                              }

                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #024c5f;
                              }

                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #024c5f;
                              }  
                            
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #024c5f;
                              }

                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #024c5f;
                              }

                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #20819a;
                              }

                              /* toggle button when hovered  */                    
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #20819a;
                              }
                              ")
                         )
              ),
    tags$style(
      HTML(".box.box-solid.box-primary>.box-header {
           color:#fff;
           background:#024c5f
           }
           
           .box.box-solid.box-primary {
           border-bottom-color:#024c5f;
           border-left-color:#024c5f;
           border-right-color:#024c5f;
           border-top-color:#024c5f;
           background:#ffffff
           }")
      ),
    useShinyjs(),
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(width = 7,
                    title = "Welcome to the Self Service Stats App",
                    status = "primary",
                    solidHeader = TRUE,
                    HTML("This is an application that has been designed to complete statistical methodology automatically 
                         based upon inputs from the user. Using the navigation panels on the left hand side, a user can 
                         upload the data file that requires analysis and input a series of metrics to ensure the correct 
                         procedures are being applied. The application will then complete the relevant statistical 
                         methodology. Outputs highlight significant results and are supported with detailed explanations 
                         of the statistical methodology including rationale for the approach used. Altogether this will 
                         enable a user to complete analysis in a simple, time-efficient and standardised way, helping with 
                         the consistency and accuracy of statistics.")
                    ),
                box(width = 5,
                    title = "Contact Details",
                    status = "primary",
                    solidHeader = TRUE,
                    HTML("<b><u>Application Developer:</u> Dr. Chris Boughton</b><p>
                         For any queries, including requests of additional features, tests or graphics, please use the 
                         following contact.<p>
                         <b><u>Email Address:</u> Boughtonc28@gmail.com</b></p><br>")
                    )
                ),
              fluidRow(
                box(width = 4,
                    title = "Useful Links",
                    status = "primary",
                    solidHeader = TRUE,
                    HTML("Please find below a number of useful links that can help with research design, statistical 
                         understanding and analysis.<p></p>"),
                    tags$a(href="https://www.scribbr.com/category/statistics/", "This link covers most statistical 
                           knowledge including research design, test application, kurtosis, normality and variables."),
                    HTML("<p></p>"),
                    tags$a(href="https://pmc.ncbi.nlm.nih.gov/articles/PMC2996580/", "This link is a scientific paper that 
                           explores what statistical tests to use in different scenarios for scientific research."),
                    HTML("<p></p>"),
                    tags$a(href="https://pmc.ncbi.nlm.nih.gov/articles/PMC8327789/", "This link is a scientific paper that 
                           also reviews the different tests and when they are most appropriate.")
                    ),
                box(width = 8,
                    img(src = "https://miro.medium.com/v2/resize:fit:1400/1*1ZFX6qQNpWCz2_iywIhUKw.png",
                        width = 800,
                        height = 500))
                )
              ),
      tabItem(tabName = "theory",
              fluidRow(
                box(width = 12,
                    title = "Statistics Background",
                    status = "primary",
                    solidHeader = TRUE,
                    HTML("Statistical tests can be performed on data that has been collected in an appropriate and valid 
                         manner. This means that the data must have a large enough sample size to reflect the true 
                         distribution of the test population. If this crtierion has been met, then the correct statistical 
                         test to use depends on both the assumptions underlying the data as well as the types of variables 
                         that make up the dataset. The underlying assumptions help identify whether parametric or 
                         non-parametric tests may be more/less suitable, while the types of variables control what 
                         specific test suits the data best. Please note that parametric tests are not synonymous with 
                         'assumes normal distribution' with many parametric tests capable and robust enough against 
                         deviations from normality assumptions. Therefore whilst normality testing is provided here, 
                         plotting of residuals, consideration of data skew and familairity with the dataset should be the 
                         principal guiding factor for meeting assumptions.<br>"),
                    div(img(src = "https://miro.medium.com/v2/resize:fit:2000/1*kk8SfzP9MpCXG6v5C6UE0g.png",
                            width = 800),
                        style = "text-align: center;"),
                    
                    tags$a(href="https://medium.com/geekculture/knowing-your-datas-type-28f4b2ad8a21", "The above figure 
                           was taken from this link. Click here for further information on variable types.")
                    )
                ),
              fluidRow(
                box(width = 12,
                    title = "Statistical Tests",
                    status = "primary",
                    solidHeader = TRUE,
                    HTML("There are a number of different tests that can be used to analyse data, each of which have their 
                         specific uses, benefits and drawbacks. The choice of test should always be linked to both the 
                         underlying data that the test will be performed on, as well as the research question being asked. 
                         Below is a summary of some key tests that are commonly used, including why and when they could 
                         be performed.<p>
                         <br><b>Comparison Tests:</b> These tests broadly investigate how the means of groups differ. For 
                         example these types of tests explore how a categorical variable influences the mean value of 
                         another variable. T-tests look at comparisons between specifically two groups, whilst ANOVA/MANOVA 
                         explore more than two groups. E.g. A t-test could explore retinal thickness changes after 
                         consumption of drug A or drug B, whilst an ANOVA/MANOVA could explore how retinal thickness changes 
                         after consumption of drug A, drug B, drug C or drug D. The difference between ANOVA and MANOVA is 
                         simply the number of outputs you require. In the example above, if you wanted to only consider 
                         retinal thickness then an ANOVA would be performed as it is one output. If however you wanted to 
                         explore the effect of drug consumption on retinal thickness, vasculature and rod/cone density, 
                         this has multiple outputs so a MANOVA is most appropriate.<p>
                         <br><b>Regression Tests:</b> These tests investigate cause and effect relationships. Typically they 
                         explore how one or more continuous variables influence another variable. Logistic regression looks 
                         at how a continuous variable influences a binary output. For example the effect of drug dosage on 
                         the surivival of a test subject. Simple linear regression explores how one continuous variable 
                         influences another continuous variable. For example how mouse weight influences longevity. Multiple 
                         linear regression considers how two or more continuous predictor variables influence another 
                         continuous variable, e.g. how mouse weight and fur length influence longevity.<p>
                         <br><b>Correlation Tests:</b> These tests explore relationships without hypothesising a cause and 
                         effect relationship. For example a Pearson's r test can be performed to consider how two continuous 
                         variables may be linked. E.g. How are mouse fur length and weight related?<p>
                         <br><b>Non-Parametric Tests:</b> These tests make no assumptions about the data and are useful when 
                         one or more of the data assumptions discussed above are violated. However, due to their underlying 
                         mathematics, they are less powerful compared to parametric tests and so can fail to find 
                         significant differences within results where there might be should a parametric test be used. 
                         Therefore these tests should be used when you are confident that at least one assumption is clearly 
                         violated and the parametric equivalent would not be robust enough to handle this violation. A 
                         summary of the different non-parametric tests and their parametric equivalents can be found below."),
                    div(reactableOutput("non_parametric_table"),
                        style = "margin-left: 100px;")
                    )
                )
              ),
      tabItem(tabName = "data",
              introjsUI(),
              column(
                width = 6,
                box(width = 12,
                    title = "Welcome to data upload",
                    status = "primary",
                    solidHeader = TRUE,
                    div(HTML("On this page you can upload your data, preview the dataset, assign data types to variables, 
                             perform basic data manipulation methods and select which variables you would like to analyse."),
                        style = "margin-bottom: 30px;"),
                    splitLayout(cellWidths = c("80%", "20%"),
                                introBox(
                                  fileInput("data_upload",
                                            label = "Upload your data here",
                                            placeholder = "Please upload a .csv or .xlsx dataset",
                                            accept = c(".csv", ".xlsx")),
                                  data.step = 1,
                                  data.intro = "It is important that the data is in the correct format when it is uploaded, 
                                                otherwise future analysis will be incorrect. The data must be in a format 
                                                where variables are in different columns. This means that each row should 
                                                be a unique observation, with the different variables for that observation 
                                                as separate columns. For example, if I had a dataset that was looking at 
                                                the height, weight, number of legs and number of teeth of different animals, 
                                                each row of the dataframe should be a different animal, with the different 
                                                variables (e.g. height) as columns."
                                  ),
                                div(actionButton("help_data",
                                                 label = "Click for help"),
                                    style = "margin-top:25px;"
                                    )
                                )
                    ),
                conditionalPanel(
                  condition = "output.user_file_uploaded",
                  box(width = 12,
                      uiOutput("user_data_ui")
                      )
                  )
                ),
              column(
                width = 6,
                div(
                  box(width = 12,
                      title = "Data Preview", 
                      status = "primary",
                      solidHeader = TRUE,
                      reactableOutput("user_data_table")
                      ), 
                  style = "text-align: center;"
                  )
                )
              )
      )
    )
)


##########
# Server #
server <- function(input, output, session) {
  
  # Add one user to the global count of active users on the app
  onSessionStart = isolate({
    users$count = users$count + 1
    })
  
  # Remove one user from the global count of active users on the app
  onSessionEnded(function() {
    isolate({
      users$count = users$count - 1
      })
    })
  
  # Rendering UI to detail the number of active users on the session/app
  output$connected_users = renderUI({
    paste0("There are ", users$count, " active user(s) currently connected")
    })
  
  # Automatically stop app when browser closes
  session$onSessionEnded(function() {
    session$close()
    })
  
  # Close app when close button clicked
  observeEvent(input$closeApp, {
    js$closeWindow()
    session$close()
    })
  
  # Help button for data uploads
  observeEvent(input$help_data,
               introjs(session,
                       events = list(onbeforechange = readCallback("switchTabs"),
                                     options = list("nextLabel"="Next",
                                                    "prevLabel"="Previous",
                                                    "skipLabel"="Data Assignment")
                                     )
                       )
               )

  # Reactable for the non-parametric tests
  output$non_parametric_table <- renderReactable({
    non_para_data <- data.frame(Test = c("Spearman's r",
                                         "Chi Square Test",
                                         "Sign Test",
                                         "Kruskal-Wallis",
                                         "ANOSIM",
                                         "Wilcoxon Rank-Sum",
                                         "Wilcoxon Signed-Rank"),
                                Predictor = c("Quantitative", 
                                              "Categorical",
                                              "Categorical",
                                              "Categorical (3+ Groups)",
                                              "Categorical (3+ Groups)",
                                              "Categorical (2 Groups)",
                                              "Categorical (2 Groups)"),
                                Outcome = c("Quantitative",
                                            "Categorical",
                                            "Quantitative",
                                            "Quantitative", 
                                            "Quantitative (2+ outcome variables)",
                                            "Quantitative (Groups come from different populations)",
                                            "Quantitative (Groups come from the same population)"),
                                Equivalent = c("Pearson's r",
                                               "Pearson's r",
                                               "One-sample t-test",
                                               "ANOVA",
                                               "MANOVA",
                                               "Independent t-test",
                                               "Paried t-test"))
    reactable(non_para_data,
              sortable = FALSE,
              highlight = TRUE,
              striped = TRUE,
              width = 1000,
              defaultColDef = colDef(
                width = 200
                ),
              columns = list(
                Outcome = colDef(
                  name = "Outcome Variable",
                  width = 400
                  ),
                Predictor = colDef(
                  name = "Predictor Variable"
                  ),
                Equivalent = colDef(
                  name = "Parametric Equivalent"
                  )
                )
              )
    })
  
  # Bringing in the user uploaded data frame
  user_data <- reactive({
    req(input$data_upload)
    file_name <- input$data_upload$name
    file_path <- input$data_upload$datapath
    
    if (grepl("\\.csv$", file_name)){
      return(data.frame(read.csv(file_path)))
      } else if (grepl("\\.xlsx$", file_name)){
        return(data.frame(read_excel(file_path)))
        } else{
          stop("Unsupported file type. Please upload a .csv or .xlsx dataset")
          }
    })
  
  # Presenting the user data back to confirm accuracy of upload and that the data uploaded is correct and as expected
  output$user_data_table <- renderReactable({
    reactable(user_data(),
              filterable = TRUE,
              highlight = TRUE,
              searchable = TRUE,
              theme = reactableTheme(
                cellStyle = list(
                  ".rt-tr-highlight:hover &" = list(backgroundColor = "#b2d8d8")
                  )
                )
              )
    })
  
  # Creating a reactive that indicates when the user has uploaded a file successfully
  output$user_file_uploaded <- reactive({
    val <- !is.null(input$data_upload)
    })
  outputOptions(output,
                'user_file_uploaded',
                suspendWhenHidden=FALSE)
  
  # Rendering UI for the dataset uploaded
  output$user_data_ui <- renderUI({
    HTML("This seems to have worked")
    })
  
}
# Run application
shinyApp(ui = ui, server = server)