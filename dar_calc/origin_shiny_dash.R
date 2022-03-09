library(shiny)
library(shinydashboard)
library(tidyverse)
library(gghighlight)
library(ggrepel)

ui <- dashboardPage(
    header <- dashboardHeader(title = "Days in AR Analysis"),

    sidebar <- dashboardSidebar(
        sidebarMenu(
            menuItem("DAR Calculator", tabName = "darcalc", icon = ("bar-chart-o"),
                     numericInput(
                            "dart",
                            "Days in AR Target:",
                            min = 1,
                            max = 100,
                            value = 39.445,
                            width ='100%'
                        ),
                        numericInput(
                            "ndip",
                            "Number of Days in Period:",
                            min = 1,
                            max = 365,
                            value = 30,
                            width ='100%'
                        ),
                        numericInput(
                            "gct",
                            "Total Gross Charges:",
                            min = 1,
                            max = 1000000,
                            value = 300000,
                            step = 10,
                            width ='100%'
                        ),
                        numericInput(
                            "earb",
                            "Ending AR Balance:",
                            min = 1,
                            max = 1000000,
                            value = 400000,
                            step = 100,
                            width ='100%'
                        ))
        ),


        body <- dashboardBody(
            fluidRow(
                infoBoxOutput("adcBox")

                        #verbatimTextOutput("dart"),
                        #verbatimTextOutput("ndip"),
                        #verbatimTextOutput("gct"),
                        #verbatimTextOutput("earb"),
                        # verbatimTextOutput("dar"),
                        # verbatimTextOutput("idlrat"),
                        # verbatimTextOutput("actrat"),
                        # verbatimTextOutput("ratdif"),
                        # verbatimTextOutput("earbt"),
                        # verbatimTextOutput("ardif"),
                        # plotOutput(outputId = "scatter2")
                )
            )

        )
    )
)


# Define server logic
server <- function(input, output) {

    output$dart <-renderText({
        paste0("Days in AR Target: ",
               input$dart,
               " days")
    })
    output$ndip <-renderText({
        paste0("Days in Period: ",
               input$ndip,
               " days")
    })
    output$gct <-renderText({
        paste0("Gross Charges: $",
               input$gct)
    })
    output$earb <-renderText({
        paste0("Ending AR Balance: $",
               input$earb)
    })

    # Average Daily Charge
    adc <-reactive({
        input$gct / input$ndip
    })
    output$adc <-renderText({
        paste0("Average Daily Charge: $",
               adc())
    })
    output$adcBox <-renderInfoBox({
        infoBox(
            "Average Daily Charge", paste0("$", adc())
        )
    })

    # Days in AR
    dar <-reactive({
        input$earb / (input$gct / input$ndip)
    })
    output$dar <-renderText({
        paste0("Days in AR: ",
               dar(),
               " days")
    })

    # Ideal Ratio
    irat <-reactive({
        input$dart / input$ndip
    })
    output$idlrat <-renderText({
        paste0("Ideal Ratio: ",
               irat())
    })

    # Actual Ratio
    arat <-reactive({
        input$earb / input$gct
    })
    output$actrat <-renderText({
        paste0("Actual Ratio: ",
               arat())
    })
    output$ratdif <-renderText({
        paste0("Ratio Difference: ",
               arat() - irat(),
               " (Negative = Passing)")
    })

    # EARB Target
    earbtarg <-reactive({
        (input$gct * irat())
    })
    output$earbt <-renderText({
        paste0("Ending AR Target: $",
               earbtarg())
    })

    # EARB Diff Target minus Actual
    rdif <-reactive({
        earbtarg() - input$earb
    })
    output$ardif <-renderText({
        paste0("AR Change Needed: $",
               rdif(),
               " (Negative = Dollar Amount that Must Be Subtracted from AR Balance to Pass, Positive = Passing)")
    })


    # Scatterplot
    color <- reactive({
        dplyr::case_when(dar() >= input$dart ~ "#D50A0A", TRUE ~ "#013369")
    })
    shape <- reactive({
        dplyr::case_when(dar() >= input$dart ~ 17, TRUE ~ 19)
    })
    output$scatter2 <-renderPlot({
        options(scipen = 999)
        ggplot(group=1) +
            geom_point(aes(input$gct, input$earb, color = color(), shape = shape()), size = 7, alpha = 0.8, na.rm = TRUE) +
            geom_abline(intercept = 0, slope = irat(), linetype = 5, size = 1, alpha = 0.7, color = "black") +
            ggrepel::geom_label_repel(aes(input$gct, input$earb, color = color()), label = dar(),  size=7, arrow = arrow(angle = 30, length = unit(0.02, "npc"), type = "closed"),
                                      box.padding = 2) +
            #geom_point(aes(input$gct, earbtarg(), color = color(), shape = shape()), size = 7, alpha = 0.8) +
            scale_y_continuous(labels = scales::dollar_format(prefix="$"), limits = c(0, input$earb * 1.5)) +
            scale_x_continuous(labels = scales::dollar_format(prefix="$"), limits = c(0, input$gct * 1.5))+
            scale_color_identity() +
            scale_shape_identity() +
            labs(x = "Total Gross Charges", y = "Ending AR Balance", title = "Scatterplot of Gross Charges (x) & Ending AR (y)") +
            theme_minimal() +
            theme(legend.position = "none")
    })
}

# Run the application
shinyApp(ui = dashboardPage(header, sidebar, body), server)
