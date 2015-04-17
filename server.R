library(shiny)
library(xlsx)
library("googleVis")
library(ggdendro)
library(ggplot2)
library(cluster)
library(stringr)
library(ape)

hpiData1 <- read.xlsx("data/hpi-dataset.xlsx", sheetIndex=1, header=TRUE,
    colIndex=seq(1,11),
    colClasses = c("numeric",rep("character",2),rep("numeric",8))
)
names(hpiData1) <- c("HPI Rank","Country","SubRegion","Life Expectancy","Well-being",
    "Happy Life Years","Footprint","Happy Planet Index","Population","GDP per capita",
    "Governance Rank")
hpiData <- hpiData1[complete.cases(hpiData1),]
rownames(hpiData) <- paste(hpiData[,"Country"],seq(1,nrow(hpiData)))

hpiData[,c(5,6,7,8,10)] <- round(hpiData[,c(5,6,7,8,10)],1)

#df=hpiData
#colname1="Wellbeing"
#colname2="Footprint"

addTooltip <- function(df,colname1="Wellbeing",colname2="HappyLifeYears",dispname1="Well-being",dispname2="Happy Life Years") {
    tipcolname <- paste(colname2,".html.tooltip",sep="")
    df[tipcolname] <- NA
    df[tipcolname] <- apply(df[,c(colname1,colname2,"Country")], 1, 
        function(x) { paste(
                "<b>",x[3],"</b>","<br>",
                "<b>",gsub(" ", "&nbsp;", dispname1),"</b>","=",round(as.numeric(x[1]),2),"<br>",
                "<b>",gsub(" ", "&nbsp;", dispname2),"</b>","=",round(as.numeric(x[2]),2),sep="") } )
    df[,c(colname1,colname2,tipcolname)]
}

addTooltipForGeo <- function(df,colname,dispname) {
    tipcolname <- paste(colname,".html.tooltip",sep="")
    df[tipcolname] <- NA
    df[tipcolname] <- apply(df[,c(colname,"Country")], 1, 
                        function(x) { paste(
                            "<b>",x[2],"</b>","<br>",
                            "<b>",gsub(" ", "&nbsp;", dispname),"</b>","=",round(as.numeric(x[1]),2),"aaa",
                            sep="") } )
    df[,c("Country",colname,tipcolname)]
}

changeLabels <- function(df,data,colname1,colname2) {
    df["label"] <- apply(df[,c("x","y","label")], 1, 
        function(x) { strCountry <- strRemoveLast(x[3]); 
                    dataRecord <- data[data$Country == strCountry,c(colname1,colname2)]
                    paste(strCountry,": ",
                          round(as.numeric(dataRecord[1]),2)," / ",
                          round(as.numeric(dataRecord[2]),2),
                          sep="") } )
    df
}

labelsForDF <- function(df,colname1,colname2,dispname1,dispname2) {
    apply(df[,c(colname1,colname2,"Country")], 1, 
        function(x) { paste(
            x[3],": ",
            dispname1,"=",round(as.numeric(x[1]),2)," ",
            dispname2,"=",round(as.numeric(x[2]),2),sep="") } )
}

strRemoveLast <- function(inputString) {
    theList <- strsplit(toString(inputString), split=" ")
    theList2 <- theList[[1]][-length(theList[[1]])]
    theString <- paste(theList2, collapse = " ")
    theString
}

shinyServer(
    function(input, output) {
        aspectInput1 <- reactive({
            switch(input$aspect1,
                    "HPI Rank" = "HPI Rank",
                    "Life Expectancy" = "Life Expectancy",
                    "Well-being" = "Well-being",
                    "Happy Life Years" = "Happy Life Years",
                    "Footprint" = "Footprint",
                    "Happy Planet Index" = "Happy Planet Index",
                    "Population" = "Population",
                    "GDP/capita" = "GDP per capita",
                    "Governance Rank" = "Governance Rank")            
        })
        aspectInput2 <- reactive({
            switch(input$aspect2,
                   "HPI Rank" = "HPI Rank",
                   "Life Expectancy" = "Life Expectancy",
                   "Well-being" = "Well-being",
                   "Happy Life Years" = "Happy Life Years",
                   "Footprint" = "Footprint",
                   "Happy Planet Index" = "Happy Planet Index",
                   "Population" = "Population",
                   "GDP/capita" = "GDP per capita",
                   "Governance Rank" = "Governance Rank")            
        })
        nbClusters <- reactive({
            strtoi(input$nbclusters)
        })
        nbClusters2 <- reactive({
            strtoi(input$nbclusters2)
        })
        
        output$wellbeingChart1 <- renderGvis({
            gvisGeoChart(addTooltipForGeo(hpiData,aspectInput1(),input$aspect1), 
                locationvar = "Country", 
                colorvar = aspectInput1(), 
                options = list(width = 800, height = 400, 
                               titlePosition = 'in', title = "aaaa"))
        })
        
        output$wellbeingChart2 <- renderGvis({
            gvisGeoChart(hpiData, locationvar = "Country", 
                colorvar = aspectInput2(), 
                options = list(width = 800, height = 400))
        })

        output$correlationText <- reactive({
            paste("The columns selected in first (",input$aspect1,
                ") and second (",input$aspect2,") tabs are being used here.")
        })
        
        trendType <- reactive({
            input$trendtype
        })
        degree <- reactive({
            input$degree
        })
        
        output$correlationChart <-renderGvis({
            gvisScatterChart(
                #hpiData[, c("LifeExpectancy","Wellbeing")],
                #hpiData[, c(aspectInput1(),aspectInput2())],
                addTooltip(hpiData, aspectInput1(), aspectInput2(),
                           input$aspect1,input$aspect2),
                options=list(
                    title = paste("Linear trend line for ",input$aspect2,
                        " vs. ",input$aspect1),
                    height=400,
                    hAxis = paste("{title: '",input$aspect1,"'}"),
                    vAxis = paste("{title: '",input$aspect2,"'}"),
                    trendlines=paste("{0: { type: '",trendType(),"',
                        degree: ",degree(),",
                        visibleInLegend: 'true',
                        labelInLegend: 'Trendline',
                        color: 'green', 
                        lineWidth: 3, 
                        opacity: 0.2}}",
                        sep=""),
                    tooltip="{isHtml:'true'}",
                    dataOpacity= 0.5
                    )
                )
        })
        
        output$hierclustHtmlUI <- renderUI({
            p("Clustering of countries according to the values of ",
                  code(input$aspect1), " / ", code(input$aspect2),
              br(),
              "Each country is labeled with the corrsponding values",
              br(),
              "The number of clusters used to color the labels is ", 
              toString(nbClusters())#,
              #br(),
              #tags$b("Note that hierarchical cluster diagram is rather large, so you'll have to scroll down right to see it.")
            )
        })
        output$hierclustChart <- renderPlot({
            hc <- hclust(dist(hpiData[,c(aspectInput1(), aspectInput2())]))
            # see http://stackoverflow.com/questions/24140339/tree-cut-and-rectangles-around-clusters-for-a-horizontal-dendrogram-in-r
            k <- nbClusters()
            clust <- cutree(hc,k=k) # cut tree into k clusters

            #hc <- hclust(dist(hpiData[,c(4,5)]))
            #convert cluster object to use with ggplot
            dendr <- dendro_data(hc, type="rectangle", compress = TRUE)
            clust.df <- data.frame(label=rownames(hpiData), cluster=factor(clust))
            dendr[["labels"]] <- merge(dendr[["labels"]], clust.df, by="label")
            rect <- aggregate(x~cluster,label(dendr),range)
            rect <- data.frame(rect$cluster,rect$x)
            ymax <- mean(hc$height[length(hc$height)-((k-2):(k-1))])
            ggplot() + 
#                ggtitle(paste("Clustering of countries according to the values of ", 
#                              input$aspect1, " / ", input$aspect2, "\n",
#                              "Each country is labeled with the corrsponding values", "\n",
#                              "The number of clusters used to color the labels is ", toString(nbClusters()),
#                              sep="")) +
                geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
                #geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=0), size=3) +
                geom_text(data=changeLabels(label(dendr),hpiData,aspectInput1(), aspectInput2()),
                          aes(x=x, y=y, label=label, hjust=1, color=cluster), size=3) +
                # 0 is left, 1 is right
#                geom_rect(data=rect, aes(xmin=X1-.3, xmax=X2+.3, ymin=0, ymax=ymax), 
#                          color="red", fill=NA) +
                #geom_text(data=labelsForDF(hpiData, aspectInput1(), aspectInput2(), input$aspect1, input$aspect2), aes(x=x, y=y, label=label, hjust=0), size=3) +
                coord_flip() + 
                scale_y_continuous(expand=c(0.3, 0)) + 
                #scale_y_reverse(expand=c(0.3, 0)) + 
                scale_x_continuous(expand=c(0.1, -5)) + 
                theme(axis.line.y=element_blank(),
                      axis.ticks.y=element_blank(),
                      axis.text.y=element_blank(),
                      axis.title.y=element_blank(),
                      panel.background=element_rect(fill="white"),
                      panel.grid=element_blank())
        },)

        output$dataTable <- renderDataTable(hpiData)

        rotationInput <- reactive({
            input$rotation
            })
           
        output$hierclustFanChart <- renderPlot({
            rownames(hpiData) <- paste(hpiData[,"Country"],
                                       hpiData[,aspectInput1()], " / ",
                                       hpiData[,aspectInput2()], sep = " ")
            hc <- hclust(dist(hpiData[,c(aspectInput1(), aspectInput2())]))
            k <- nbClusters2()
            
            #rownames(hpiData) <- paste(hpiData[,"Country"],
            #                           hpiData[,"Wellbeing"], " / ",
            #                           hpiData[,"Footprint"], sep = " ")
            #hc <- hclust(dist(hpiData[,c("Wellbeing", "Footprint")]))
            #k <- 5

            clust <- cutree(hc,k=k) # cut tree into k clusters
            plot(as.phylo(hc), type = "fan", 
                 tip.color=clust, # tip color
                 cex = 0.8, # character expansion
                 label.offset=0.2,
                 rotate.tree=-rotationInput()
                 )
        })

    }
)
