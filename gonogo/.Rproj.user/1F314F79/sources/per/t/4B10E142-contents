library(shiny)

# ui <- fluidPage(
#   sidebarLayout(
#     sidebarPanel = sidebarPanel(
#       actionButton("button","Click me twice!")
#     ),
#     mainPanel = mainPanel(
#       tableOutput("table"),
#       plotOutput("plot")
#     )
#   )
# )

# Is it possible to get spacebar stuff from shiny?

ui <- fluidPage(
  title = "Go-No Go Task"
)

# Game parameters (don't think these should be set beforehand though? I want participant to decide on them)
interval <- 0.6
n_trial <- 10
prb <- c(0.7, 0.3)
stim <- c("A", "X")
stimuli <- sample(stim, size = n_trial, replace = TRUE)


# ui <- fixedPage(
#
#   title = "Go No-Go Task",
#   uiOutput("MainAction"),
#   #tags$style(type = "text/css", ".recalculating {opacity: 1.0;}") # prevents grey screen during Sys.sleep()
#
# )
#
#
# server <- function(input, output, session) {
#
#   # Define Reactive Values (the main values in the game)
#   CurrentValues <- reactiveValues(page = "welcome",
#                                   training = TRUE,
#                                   lastPress = Sys.time(),
#                                   trial = 0,
#                                   rt = 0,
#                                   stimulus = NULL,
#                                   response = NULL)
#
#   # GameData stores vectors of histories
#   GameData <- reactiveValues(trial = c(),
#                              rt = c(),
#                              stimulus = c(),
#                              lastPress = c(),
#                              response = c())
#
#   # Page Layout
#   PageLayouts <- reactive({
#
#     # Welcome page
#     if (CurrentValues$page == "welcome") {
#       inputLabel <- p("Please enter your name or a unique ID")
#     }
#
#     return(
#       list(
#         h1("Go No-Go Task"),
#         p("This is a version of the Go-No Go Task implemented in Shiny. The task measures inhibitory control, a cognitive process that enables humans to cancel motor activity after its initiation."),
#         p("If you wish to continue, please enter your name or a unique ID number"),
#         textInput(inputId = "workerid",
#                   label = inputLabel,
#                   placeholder = "e.g., participant 1"),
#
#         actionButton(inputId = "gt_inst1",
#                      label = "Start!")
#       )
#     )
#
#     # Instructions
#     if (CurrentValues$page == "inst1") {
#       return(
#         list(
#           h1("The Go-No Go Task"),
#           p("Some instructions go here"),
#           actionButton(inputId = "gt_training",
#                        label = "Start the task")
#         )
#       )}
#   }
#
#     # Game
#     # if (currentValues$page == "game") {
#     #
#     # }
#
#     )
#
# }
#
#
#
# # server <- function(input, output, session) {
# #
# #   click_timestamps <- reactiveVal(NULL)
# #   recorded_rts <- reactiveVal(NULL)
# #
# #   observeEvent(input$button, {
# #     new_val <- append(click_timestamps(), Sys.time())
# #
# #     # set click timestamp
# #     click_timestamps(new_val)
# #
# #     if(length(new_val) == 2) {
# #
# #       duration <- as.numeric(difftime(new_val[2], new_val[1], units = "secs"))
# #       recorded_rts(c(recorded_rts(), duration))
# #
# #       print(recorded_rts)
# #       print(click_timestamps)
# #
# #       # reset click timestamp
# #       click_timestamps(NULL)
# #     }
# #   })
# #
# #   output$table <- renderTable(
# #     if (length(recorded_rts()) > 0) {
# #       return(data.frame(n = seq_along(recorded_rts()), rt = recorded_rts()))
# #     } else {
# #       return(data.frame(n = "-", rt = "-"))
# #     }
# #
# #   )
# #   output$plot <- renderPlot(
# #     if (length(recorded_rts()) > 0)
# #       hist(recorded_rts())
# #     else
# #       plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10))
# #   )
# #
# # }
# #
#
# shinyApp(ui, server)
