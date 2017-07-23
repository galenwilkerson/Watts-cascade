
##################################################################

layout_choices = c("force-directed", "grid", "random")

library(shinydashboard)

ui <- dashboardPage(title = "Network Cascade Simulator by Galen",
                    dashboardHeader(title = "Network Cascades"),
                    dashboardSidebar( # slider to choose N
                      sliderInput("N", "N", min = min_N, max = max_N, value = init_N, ticks = TRUE),
                      sliderInput("p", "p", min = 0.001, max = 1.0, value = .5, ticks = TRUE),
                      sliderInput("n_seeds", "n_seeds", min = min_N, max = max_N, value = init_n_seeds, ticks = TRUE),
                      sliderInput("phi", label = "phi range", min = 0.000, max = 1.0, value = c(init_min_phi, init_max_phi)),
                      selectInput(inputId = "layout_choice", label = "layout", choices = layout_choices, selected = "force-directed"),
                      actionButton("redraw", "Regenerate"),
                      actionButton("run_cascade", "Run Cascade"),
                      sliderInput("speed", "run speed (ms)", min = 0, max = 2000, value = 100)
                    ),
                    dashboardBody(tags$style(type="text/css",
                                             ".recalculating {opacity: 1.0;}"),
                                  # Boxes need to be put in a row (or column)
                                  fluidRow(
                                    box(plotOutput("plot"), width = 12)
                                    # box(textOutput("N"),
                                    # textOutput("p"),
                                    # textOutput("phi"),
                                    # textOutput("seeds"))
                                  ),
                                  fluidRow(
                                    textOutput("vulnerable_nodes")
                                  )
                    )
)