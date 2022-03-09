library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- function(request) {
    fluidPage(
        theme = bslib::bs_theme(version = 4, bootswatch = "darkly"),
        navbarPage("Days in AR Automated Analysis"
               # ,
               # tabPanel("Plot"),
               # tabPanel("Summary"),
               # tabPanel("Table")
    ),

    # Sidebar with a slider input
    sidebarLayout(
        sidebarPanel(
            helpText("Enter Required Variables"),

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
                ),
            bookmarkButton(),
            width = 2
            ),


        # Main Panel
        mainPanel(
            #verbatimTextOutput("dart"),
            #verbatimTextOutput("ndip"),
            #verbatimTextOutput("gct"),
            #verbatimTextOutput("earb"),
            verbatimTextOutput("adc"),
            verbatimTextOutput("dar"),
            verbatimTextOutput("idlrat"),
            verbatimTextOutput("actrat"),
            verbatimTextOutput("ratdif"),
            verbatimTextOutput("earbt"),
            verbatimTextOutput("ardif"),
            plotOutput(outputId = "scatter")
        )
    )
)
}
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
    output$adc <-renderText({
        paste0("Average Daily Charge: $",
               input$gct / input$ndip)
        })
    output$dar <-renderText({
        paste0("Days in AR: ",
               input$earb / (input$gct / input$ndip),
               " days")
        })
    irat <-reactive({
        input$dart / input$ndip
    })
    output$idlrat <-renderText({
        paste0("Ideal Ratio: ",
               irat())
        })
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
               " (Negative = Passing)"
               )
        })
    earbtarg <-reactive({
        input$gct * irat()
    })
    output$earbt <-renderText({
        paste0("Ending AR Target: $",
               earbtarg())
        })
    rdif <-reactive({
         earbtarg() - input$earb
    })
    output$ardif <-renderText({
        paste0("AR Change Needed: $",
               rdif(),
               " (Negative = Dollar Amount that Must Be Subtracted from AR Balance to Pass, Positive = Passing)")
    })
    output$scatter <-renderPlot({
        plot(
            input$gct,
            input$earb,
            xlab="Total Gross Charges ($USD)",
            ylab="Ending AR Balance ($USD)",
            pch = 20,
            cex = 3,
            abline(a = 0, b = irat(),
                   col = "red", lwd = 4)
            )
    })
    }

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")
