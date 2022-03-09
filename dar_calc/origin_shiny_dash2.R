library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggrepel)
library(highcharter)

header <- dashboardHeader(title = "Days in AR Analysis")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("DAR Calculator", tabName = "darcalc", icon = icon("chart-bar", verify_fa = FALSE),
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
    )
)


body <- dashboardBody(
    fluidRow(
        valueBoxOutput("adcBox", width = 3),
        valueBoxOutput("darBox", width = 3),
        valueBoxOutput("earbtBox", width = 3),
        valueBoxOutput("ardifBox", width = 3),
        # fluidRow(
        #                 # box(title = "DAR Indicator: Above & Left of the Dotted line, DAR is Failing", width = 6, solidHeader = TRUE, status = "primary",
        #                 #     plotOutput(outputId = "scatter2")),
        #                 box(title = "DAR Indicator", width = 6, solidHeader = TRUE, status = "success",
        #                     highchartOutput(outputId = "scatterh")),
        # )
    )
)


ui <- dashboardPage(header,
                    sidebar,
                    body)


# Define server logic
server <- function(input, output) {

    output$dart <-renderText({
        paste0("Days in AR Target: ",
               input$dart,
               " days")
    })
    output$dartBox <-renderInfoBox({
        infoBox(
            "Days in AR Target", paste0(input$dart, " Days")
        )
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
    output$earbBox <-renderInfoBox({
        infoBox(
            "Ending AR Balance", paste0("$", input$earb)
        )
    })

    # Average Daily Charge
    adc <-reactive({
        input$gct / input$ndip
    })
    output$adc <-renderText({
        paste0("Average Daily Charge: $",
               adc())
    })
    output$adcBox <-renderValueBox({
        valueBox(
            value = paste0("$", adc()),
            subtitle = "Average Daily Charge"
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
    output$darBox <-renderValueBox({
        infoBox(
            "Days in AR", paste0(dar(), " Days"), fill = TRUE
        )
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
    output$earbtBox <-renderValueBox({
        infoBox(
            "Ending AR Target", paste0("$", earbtarg()), fill = TRUE
        )
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
    output$ardifBox <-renderValueBox({
        infoBox(
            "AR Change Needed", paste0("$", rdif()), fill = TRUE
        )
    })


    # GGPLOT Scatterplot
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
            labs(x = "Total Gross Charges", y = "Ending AR Balance") +
            theme_minimal() +
            theme(legend.position = "none")
    })

    #Highcharter Scatterplot
    output$scatterh <- renderHighchart({
        hchart("scatter", hcaes(input$gct, input$earb))
    })
}

# Run the application
shinyApp(ui = dashboardPage(header, sidebar, body), server)
