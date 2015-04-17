library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
    titlePanel("Happy Planet Index (HPI) Data Visualization"),
    sidebarLayout(
        sidebarPanel( 
            p("Quick Help:"),
            p("In this application you can visualize data related to happiness indicators across the world, as per ",
              a("Happy Planet Index",href="http://www.happyplanetindex.org/data/"),
              " web site."),
            p("You can choose two aspects, explore their correlation, and how countries can be clustered taking these aspects into consideration."),
            p("It's nee written mainly for fun, and show cases a few Shiny features, including Google Charts integration."),
            p("I would value your feedback! ",
              a("Regis",href="http://www.linkedin.com/in/rcoqueret/")),
            tags$ul(
                tags$li(code("First Aspect"), 
                        " tab: Select a first column for the data set."),
                tags$li(code("Second Aspect"), 
                        " tab: Select a second column."),
                tags$li(code("Correlation"), 
                        " tab: Examine the correlation between the two aspects."),
                tags$li(code("Clusters"), 
                        " tab: Visualize a dendrogram of clustered countries."),
                tags$li(code("Fan"), 
                        " tab: Clustered countries displayed as a fan that you can rotate."),
                tags$li(code("Table format"), ": actual data"),
                tags$li(code("Help"), ": more details on the above.")
                )
            ),
        mainPanel(
            tabsetPanel(
                tabPanel("First Aspect",
                    selectInput("aspect1", "Choose a first aspect:",
                        choices = c("HPI Rank","Life Expectancy","Well-being",
                            "Happy Life Years","Footprint","Happy Planet Index",
                            "Population","GDP/capita","Governance Rank"),
                        selected="Well-being"
                        ),
                    htmlOutput("wellbeingChart1")
                    ),
                tabPanel("Second Aspect",
                    selectInput("aspect2", "Choose a first aspect:",
                        choices = c("HPI Rank","Life Expectancy","Well-being",
                            "Happy Life Years","Footprint","Happy Planet Index",
                            "Population","GDP/capita","Governance Rank"),
                        selected="Happy Life Years"
                        ),
                    htmlOutput("wellbeingChart2")
                    ),
                tabPanel("Correlation",
                    selectInput("trendtype", "Choose a type for the trend line:",
                        choices = c("linear","exponential","polynomial"),
                        selected="linear"
                    ),
                    conditionalPanel(
                        condition = "input.trendtype == 'polynomial'",
                        selectInput("degree", "Choose a degree for polynomial type:",
                                    choices = c(1,2,3,4,5),
                                    selected=1
                        )
                    ),
                    htmlOutput("correlationChart"),
                    htmlOutput("correlationText")
                ),
                tabPanel("Clusters",
                     selectInput("nbclusters", "Choose a number of clusters:",
                                 choices = seq(1,50),
                                 selected=5
                     ),
                    uiOutput("hierclustHtmlUI"),
                     htmlOutput("hierclustText"),
                     plotOutput("hierclustChart", height="1500", width = "1200")
                ),
                tabPanel("Fan",
                         selectInput("nbclusters2", "Choose a number of clusters:",
                                     choices = seq(1,50),
                                     selected=5
                         ),
                         sliderInput('rotation', 'Rotation', 0, min = 0, max = 360, step = 5),
                         plotOutput("hierclustFanChart", height="800", width = "800")
                ),
                tabPanel("Table Format",
                        dataTableOutput("dataTable")
                ),
                tabPanel("Help",
                     p("The data visalized in this application originate from the ",
                       a("Happy Planet Index",href="http://www.happyplanetindex.org/data/"),
                       " web site."),
                     p("In this application you can"),
                     tags$ol(
                         tags$li("On tab ", code("First Aspect"), 
                                 ": Select a first column for the data set.",
                                 "A ", a("Google Geo Chart",href="https://developers.google.com/chart/interactive/docs/gallery/geochart"),
                                 " will be displayed, using the selected column's data.",
                                 "It is a chloropleth, where countries are colored in proportion to the country data."),
                         tags$li("On tab ", code("Second Aspect"), 
                                 ": Select a second aspect. The design is the same as on the first tab."),
                         tags$li("On tab ", code("Correlation"), 
                                 ": Examine the correlation between the two aspects, using a ",
                                 a("Google Trendline Chart",href="https://developers.google.com/chart/interactive/docs/gallery/trendlines"),
                                 ". You can choose the type of trend line, as well as the degree if type is polynomial."
                         ),
                         tags$li("On tab ", code("Clusters"), 
                                 ": Visualize a dendrogram of clustered countries.",
                                 "You can choose the number of clusters, but the distance matrix",
                                 " is set based on the two aspects previously selected, ",
                                 " and uses the default complete linkage method."
                         ),
                         tags$li("On tab ", code("Fan"), 
                                 ": The denddrogram of clustered countries ",
                                 " is displayed in fan mode.",
                                 " A slider is available to rotate it.",
                                 " It's quite fun to see this in action ;-) Shiny really rocks!"
                         ),
                         tags$li("The actual data can be seen on ", code("Table format"), "tab")
                     ),
                     p("The default values are:",
                       tags$ul(
                           tags$li("First aspect: ", code("Well-being")),
                           tags$li("Second aspect: ", code("Happy Life Years")),
                           tags$li("Number of clusters: ", code("5")),
                           tags$li("No rotation on the fan")
                           )
                       ),
                     p("You can also consult the ",
                       a("online presentation", href="http://rpubs.com/rljc/HappyIndexApp"))
                 )
            )
        )
    )
    ))
