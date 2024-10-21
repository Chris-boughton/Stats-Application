# Self Service Statistics Application
# Author: Chris Boughton
# Objective: To provide scientific statistical analysis through an easy to use application. Options should be provided to 
# input the data, data types, variables and other metrics in order to correctly select the best statistical test to use. 
# This should then produce a series of graphs, tables and worded paragraphs to justify the test chosen and present the 
# data back to the user, in a clear and concise way, that highlights potential findings, significance and outputs.

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
                         methodology, including normality testing. Breakdowns of each area explored will be provided to 
                         the user including figures and tables that detail these processes. Outputs highlight significant 
                         results and are supported with detailed explanations of the statistical methodology including 
                         rationale for the approach used. Altogether this will enable a user to complete analysis in a 
                         simple, time-efficient and routine way, helping with the consistency and accuracy of statistics.")
                    ),
                box(width = 5,
                    title = "Contact Details",
                    status = "primary",
                    solidHeader = TRUE,
                    HTML("<b><u>Application Developer:</u> Dr. Chris Boughton</b><p>
                         For any queries, including requests of additional features, tests or graphics, please use the 
                         following contact.<p>
                         <b><u>Email Address:</u> Boughtonc28@gmail.com</b></p><br>")
                    ),
              )
      )
    )
  )
)

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
  }

# Run application
shinyApp(ui = ui, server = server)