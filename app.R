library(shiny)
library(tidyverse)
library(tidyr)
library(plotly)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(dplyr)

load("unemployment.rda")
load("unemployment_comp.rda")
load("countries.rda")
load("geocolour.rda")
load("inflation.rda")
load("expweights.rda")
load("gdp.rda")
load("gdp_comp.rda")

ui = fluidPage(
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: blue;
      }
    "))
  ),
  
  titlePanel("European Economic Indicators - tracking the impact of COVID19"),
  
  sidebarPanel(selectInput(inputId= "countries", label = "Countries/Regions", choices = c("Please Select", sort(countries$geo)), multiple = TRUE, selected = c("EU27","France","Germany","Italy","Netherlands","Spain")),
               sliderInput("date",
                           "Start date:",
                           min = as.Date("2000-01-01","%Y-%m-%d"),
                           max = as.Date(max("2020-01-01"),"%Y-%m-%d"),
                           value=as.Date("2020-01-01"),
                           timeFormat="%Y-%m"),
               uiOutput("indicator"),
               uiOutput("countries"),
               uiOutput("date"),
               HTML(paste0("This ","<a href=https://shiny.rstudio.com/>Shiny</a>"," application uses key economic data for EU and other European countries, retrieved from the ","<a href=https://michael-jacobs.github.io/europe-economy-tracker-part1/>Eurostat API</a> on ",format(unique(as.Date(gdp$apidate)),"%d-%B-%Y"),". It examines the evolution of these indicators over time, with the aim of visualising the economic impact of the COVID19 pandemic. The data span a period of more than twenty years, between January 2000 and today, with data updates gathered automatically from the API on a weekly basis.")),
               HTML("<br/>"), 
               p("The five largest EU countries by size of economy are included by default. Additional countries can be added to the comparisons, using the country dropdown box. The starting point for the comparisons (set by default to 1 January 2020) can also be amended, using the date slider.")), 
  
           
  mainPanel(tabsetPanel(type = "tabs",
                        tabPanel("Unemployment",HTML("<br/>"),
                                 p("Both seasonally adjusted and unadjusted aggregates are available from Eurostat - the former are included here. They account for differences in the labour market as a result of seasonal events, such as holidays, climatic differences and crop harvests, and are generally considered better for measuring trends in unemployment data."),
                                 tabsetPanel(type = "tabs",
                                             tabPanel("Time Series",HTML("<br/>"),           
                                                      plotlyOutput("line"),
                                                      HTML("<br/>"),
                                                      p("Since January 2020 unemployment rates have increased somewhat across the EU. Nonetheless, levels of joblessness remain significantly lower those that followed the economic crisis that was sparked by the credit crunch of 2008. Financial assistance provided by Governments to employers has played a part in keeping people in work. The future evolution of this important economic indicator will be keenly watched by policy makers as the COVID related support begins to be phased out.")),
                                             tabPanel("Trend",HTML("<br/>"),
                                                      plotlyOutput("slope"),
                                                      HTML("<br/>"),
                                                      p("The chart belows shows the direction of the trend in unemployment rates, from the start date of the comparison, to the date of the most recent available data."),
                                                      p("Differences in the steepness of lines are evident between countries, with flatter lines indicating a smaller difference between the start and end dates.")), 
                                             tabPanel("Age/Gender",HTML("<br/>"),
                                                      p("While headline unemployment rates provide a good overview of the labour market as a whole, differences between specific groups may be masked. Using percentage point differences in unemployment rates between the comparison start date and the most recent available data, the chart below shows the range of change in the level of unemployment according to different demographic groupings."),
                                                      selectInput(inputId= "disaggregation", label = "Disaggregation", choices = c("By age", "By gender", "By age and gender"), selected = c("By age")),
                                                      plotlyOutput("dotplot"),
                                                      HTML("<br/>"),
                                                      p("These dissagregated differences point to a worsening of existing inequalities in unemployment rates, with larger increases seen in particular for under 25 year olds in a number of countries. Unemployment rates among females in this age group also show account for the largest increase in many cases.")))),
                        tabPanel("Inflation",HTML("<br/>"),
                                 p("Inflation, as measured by the annual rate of change in the Harmonised Index of Consumer Prices (HICP), has been low across the EU27 in the last few years. In the second half of 2020 rates approached zero, and even ventured into delfationary territory in some countries: decreased consumer demand - partly a result of lockdowns and other restrcitions on movement within many countries - has lead to decreases in the price of some consumer products. Changes in the habits of consumer are also evident from the expenditure weights used in the calculation of the HICP - the weights introduced in January 2021 show a weakening of expenditures in certain major groups of expenditure when compared to those for 2020."),
                                 tabsetPanel(type = "tabs",
                                             tabPanel("Rate",HTML("<br/>"),
                                                      p(""),
                                                      plotlyOutput("facet")),
                                             tabPanel("Weights",HTML("<br/>"),
                                                      p("The computation of the HICP relies on both average price data for a basket of consumer goods and services, as well as expenditure patterns, or weights, derived from household surveys. These weights, which are updated on an annual basis, give an idication of the relative importance of different types of goods in relation to total household expenditures."),
                                                      selectInput(inputId= "expselect", label = "Major group of expenditure", choices = unique(expweights$indic), selected = c("Alcoholic beverages, tobacco and narcotics")),
                                                      plotlyOutput("weight"),
                                                      HTML("<br/>"),
                                                      p("Based on the latest available weights (implemented in January 2021) the panadmic has lead to some clear changes in spending patterns: the relative importance of transport, restaurants, hotels and other recreation and cultural activities have generally declined across European countries, a direct result of restrictions on movement and limitations on non-essential activities. It remains to be seen whether these chages will become a permanent feature of household expenditure patterns.")))),
                        tabPanel("GDP",HTML("<br/>"),
                                 p("Gross Domestic Product (GDP) is a measure of the size of a country's economy, based on the total value of everything produced with a prescribed geographic area. It is widely used as the measure of output and economic activity."),
                                 p("The two charts below are based a chain-linked series of GDP, which strips out the effect of changes in price, to measure changes in volume only."),
                                 
                                 tabsetPanel(type = "tabs",
                                             tabPanel("Annualised change",HTML("<br/>"),
                                                      plotlyOutput("annual"),
                                                      HTML("<br/>"),
                                                      p("The timeseries below shows the annual growth rate in GDP, namely the percentage increase when compared to the same period 12 months earlier. Throughout 2020 the annual change in GDP shows a decrease in output when compared to the same quarter in 2019. GDP in the second quarter of 2020 in particular was dramatically lower than in the previous year, underlining the economic impact of the pandamic")),
                                             tabPanel("Index",HTML("<br/>"),
                                                      plotlyOutput("index"),
                                                      HTML("<br/>"),
                                                      p("Rebased GDP volumes (with 2015 equivalent to 100) highlight the dramatic drop in GDP that begin in the first quarter of 2020. Based on this index the economic output of the 27 EU member states in the second quarter of 2020 were the same as volumes last seen in 2009. While these volumes have since bounced back to above 2015 levels in most cases, output still remains below that recorded in 2019."))))
                        
  ),
  
  )
)

server = function(input, output, session) {
  
  output$line <-renderPlotly({
    validate(
      need(input$countries != "Please Select", "Select countries to include in the analysis.")
    )
    
    ##Filter all countries for required dataset##
    d <-filter(unemployment, indic == "Total - seasonally adjusted", format(as.Date(time,format="%Y-%m-%d"),"%Y-%m")>=format(as.Date(input$date,format="%Y-%m-%d"),"%Y-%m"),!(geo %in% input$countries))
    if (nrow(d) == 0) return(NULL)
    
    e <-filter(unemployment, indic == "Total - seasonally adjusted", format(as.Date(time,format="%Y-%m-%d"),"%Y-%m")>=format(as.Date(input$date,format="%Y-%m-%d"),"%Y-%m"), geo %in% input$countries)
    if (nrow(e) == 0) return(NULL)
    
    f <- left_join(as.data.frame(unique(e$geo)), countries, by = c("unique(e$geo)" = "geo"))
    colnames(f) <-c("geo","colour")
    geocolour <-f$colour
    names(geocolour)<-f$geo
    p <-ggplot(d, aes(x = time, y = values, group = geo, text = paste0('<b>',geo,'</b>',
                                                                       '<br>',format(as.Date(time,format="%Y-%m-%d"),"%b-%Y"), ':', values,'%'))) + geom_line(size =0.1, colour  = "light grey")
    
    p <-p + geom_line(data = e, aes(x = time, y = values, colour = geo),size =0.5) + scale_colour_manual(breaks = input$countries, labels = input$countries, values = geocolour) + theme_bw()
    p <-p + labs(title=paste0("Harmonised total unemployment rates - seasonally adjusted (monthly, %)\n",input$indicator,"\n"), x = "Period", y = "Unemployment rate (%)", colour = "", size = 8)
    p <-p + theme(plot.title = element_text(size = 10, margin=margin(0,0,30,0)),
                  axis.title=element_text(size = 8),
                  axis.text=element_text(size = 8),
                  legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))
    
    if(format(as.Date("2008-09-15",format="%Y-%m-%d"),"%Y-%m")>=format(as.Date(input$date,format="%Y-%m-%d"),"%Y-%m")){
      p <-p + annotate("text", x = as.Date("2008-11-01",format="%Y-%m-%d"), y = max(d$values), label = "15 Sep 2008: Lehman Brothers", size = 1.5)
      p <-p + annotate("text", x = as.Date("2008-11-01",format="%Y-%m-%d"), y = max(d$values)-0.5, label = "file for bankruptcy.", size = 1.5)
      p <-p + geom_vline(xintercept=as.numeric(as.Date("2008-10-15",format="%Y-%m-%d")), linetype="dotted", colour  = "black",size=.2) 
    }else{
      p <-p
    }
    if(format(as.Date("2020-01-12",format="%Y-%m-%d"),"%Y-%m")>=format(as.Date(input$date,format="%Y-%m-%d"),"%Y-%m")){
      p <-p + annotate("text", x = as.Date("2020-02-20",format="%Y-%m-%d"), y = max(d$values), label = "12 Jan 2020: China shares the", size = 1.5)
      p <-p + annotate("text", x = as.Date("2020-02-20",format="%Y-%m-%d"), y = max(d$values)-0.5, label = " genetic sequence of COVID-19", size = 1.5)
      p <-p + geom_vline(xintercept=as.numeric(as.Date("2020-01-09",format="%Y-%m-%d")), linetype="dotted", size=.2, colour ="black") 
    }else{
      p <-p
    }
    
    ggplotly(p, tooltip = "text") %>% 
      config(displayModeBar = F) %>%
      layout(annotations = list(x = 1, y = -0.12, text = "Source: Eurostat (series = ei_lmhr_m)", showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,font=list(size=9, color="grey"))) %>%
      layout(annotations = list(x = 0.2, y = -0.12, text = paste('<a href=https://michael-jacobs.github.io/>michael-jacobs.github.io</a>'), showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,font=list(size=8, color="grey"))) 
    
  })             
  output$slope <-renderPlotly({
    validate(
      need(input$countries != "Please Select", "Select countries to include in the analysis.")
    )
    slopedata <- filter(unemployment_comp, indic == "Total - seasonally adjusted", geo %in% input$countries, format(as.Date(startdate,format="%Y-%m-%d"),"%Y-%m")==format(as.Date(input$date,format="%Y-%m-%d"),"%Y-%m"))
    
    f <- left_join(as.data.frame(unique(slopedata$geo)), countries, by = c("unique(slopedata$geo)" = "geo"))
    colnames(f) <-c("geo","colour")
    geocolour <-f$colour
    names(geocolour)<-f$geo
    left_label <- slopedata$startvalue
    right_label <- paste0(slopedata$endvalue," (",format(as.Date(unique(slopedata$enddate),format="%Y-%m-%d"),"%b-%y"),")")
    slope <- ggplot(slopedata) + geom_segment(aes(x=1, xend=2, y=startvalue, yend=endvalue, colour = geo), size=.75, show.legend=F) 
    slope <- slope + geom_vline(xintercept=1, linetype="dotted", size=.2) + geom_vline(xintercept=2, linetype="dotted", size=.1) + scale_colour_manual(values = geocolour) + theme_bw()
    slope <- slope + labs(x="", y="") + xlim(0.9, 2.1) + ylim(0,(1.1*(max(slopedata$startvalue, slopedata$endvalue))))
    slope <- slope + geom_text(label=left_label, y=slopedata$startvalue, x=rep(0.95, NROW(slopedata)), hjust=0, size=2.5, check_overlap = TRUE)
    slope <- slope + geom_text(label=right_label, y=slopedata$endvalue, x=rep(2.08, NROW(slopedata)), hjust=0, size=2.5, check_overlap = TRUE)
    slope <- slope + geom_text(label=format(as.Date(input$date,format="%Y-%m-%d"),"%Y-%m"), x=1, y=1.1*(max(slopedata$startvalue, slopedata$endvalue)), hjust=1.2, size=3) 
    slope <- slope + geom_text(label="Latest", x=2, y=1.1*(max(slopedata$startvalue, slopedata$endvalue)), hjust=-0.1, size=3) 
    slope <- slope + labs(title="Harmonised unemployment rates (%)",size = 10)
    slope <- slope + theme(plot.title = element_text(size = 10, margin=margin(0,0,30,0)),
                           panel.grid = element_blank(),
                           axis.ticks = element_blank(),
                           axis.text.x = element_blank(),
                           panel.border = element_blank(),
                           plot.margin = unit(c(1,2,1,2), "cm"),
                           axis.title=element_text(size = 8),
                           axis.text=element_text(size = 8),
                           legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))
    
    ggplotly(slope, tooltip = NULL) %>% 
      config(displayModeBar = F)%>% 
      layout(annotations = list(x = 1, y = -0.11, text = "Source: Eurostat (series = ei_lmhr_m)", showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,font=list(size=9, color="grey"))) %>%
      layout(annotations = list(x = 0.2, y = -0.11, text = '<a href=https://michael-jacobs.github.io/>michael-jacobs.github.io</a>', showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,font=list(size=8, color="grey"))) 
    
  })
  output$dotplot <-renderPlotly({
    validate(
      need(input$countries != "Please Select", "Select countries to include in the analysis.")
    )
    
    if(input$disaggregation =="By age"){
      agegender <- c("Over 25 years - Total","Under 25 years - Total","Total")
      
    }else if(input$disaggregation =="By gender"){
      agegender <- c("Females","Males","Total")
      
    }else{
      agegender <- c("Over 25 years - Females","Over 25 years - Males","Under 25 years - Females","Under 25 years - Males","Total")
    }
    
    f <- filter(unemployment_comp, geo %in% input$countries, format(as.Date(startdate,format="%Y-%m-%d"),"%Y-%m")==format(as.Date(input$date,format="%Y-%m-%d"),"%Y-%m"), grepl("seasonally adjusted",indic))
    f$indic <-gsub(" - seasonally adjusted", "", f$indic)
    f <-filter(f,indic %in% agegender)
    minmax = f %>%
      group_by(geo) %>%
      summarise(
        max = max(difference, na.rm = T),
        min = min(difference, na.rm = T)
      )
    f <- left_join(f,minmax)
    
    g <-data.frame(indic = c("Over 25 years - Total","Under 25 years - Total","Total","Females","Males","Over 25 years - Females","Over 25 years - Males","Under 25 years - Females","Under 25 years - Males"),
                    colour = rainbow(9, rev = TRUE),
                   sizes = c(4,5,6,4,5,4,4.6,4.5,4)
    )
                    
    g <- left_join(as.data.frame(unique(f$indic)), g, by = c("unique(f$indic)" = "indic"))
    colnames(g) <-c("indic","colour","sizes")
    indiccolour <-g$colour
    names(indiccolour)<-g$indic
    indicsize <-g$sizes
    names(indicsize)<-g$indic
    
    
    dotplot <-ggplot()
    dotplot <-dotplot + geom_segment(data = f,aes(x = min, xend = max, y = reorder(geo,max), yend = reorder(geo,max)),color = "gray80", size = 4)+ theme_bw()
    dotplot <-dotplot + geom_point(data = f, aes(x = min, y=reorder(geo,max)), color = "gray80", size = 4) 
    dotplot <-dotplot + geom_point(data = f, aes(x = max, y=reorder(geo,max)), color = "gray80", size = 4) 
    dotplot <-dotplot + geom_point(data = f, aes(x = difference, 
                                                 y=reorder(geo,max), 
                                                 color = indic,
                                                 text = paste0('<b>',geo,'</b>',' - ',indic,
                                                               '<br>',format(as.Date(startdate,format="%Y-%m-%d"),"%b-%Y"),': ',startvalue,
                                                               '<br>',format(as.Date(enddate,format="%Y-%m-%d"),"%b-%Y"),': ',endvalue,
                                                               '<br><b>Diff: ',difference,'</b>'),
                                    size = indic), 
                                   shape = 21) 
    
    dotplot <-dotplot + scale_color_manual(values =indiccolour) 
    dotplot <-dotplot + scale_size_manual(values =indicsize) 
    
    
    dotplot <-dotplot + labs(title=paste0("Change in unemployment rate (percentage points) since ",format(as.Date(input$date,format="%Y-%m-%d"),"%b-%Y"),"\n (Value below 0 implies decrease in rate)\n"),x="Difference",y="",color="", size ="")
    dotplot <-dotplot + theme(plot.title = element_text(size = 10, margin=margin(0,0,30,0)),
                              axis.title=element_text(size = 8),
                              legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))
    
    
    ggplotly(dotplot, tooltip = "text") %>% 
      config(displayModeBar = F) %>% 
      layout(annotations = list(x = 1, y = -0.12, text = "Source: Eurostat (series = ei_lmhr_m)", showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,font=list(size=8, color="grey"))) %>%
      layout(annotations = list(x = 0.2, y = -0.12, text = '<a href=https://michael-jacobs.github.io/>michael-jacobs.github.io</a>', showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,font=list(size=8, color="grey"))) %>% 
      layout(legend = list(font=list(size=6)))
    
  })
  output$facet <-renderPlotly({
    validate(
      need(input$countries != "Please Select", "Select countries to include in the analysis.")

    )
    facetdata <- filter(inflation, indic =="HICP - All items (HICP=Harmonized Index of Consumer Prices) - unadjusted",geo %in% input$countries, format(as.Date(time,format="%Y-%m-%d"),"%Y-%m")>=format(as.Date(input$date,format="%Y-%m-%d"),"%Y-%m"), unit =="Growth rate (t/t-12)")
    
    f <- left_join(as.data.frame(unique(facetdata$geo)), countries, by = c("unique(facetdata$geo)" = "geo"))
    colnames(f) <-c("geo","colour")
    geocolour <-f$colour
    names(geocolour)<-f$geo
    
    p <-ggplot()
    p <-ggplot(data=facetdata,aes(x = time, y = values, fill = geo)) + geom_ribbon(aes(ymax = values, ymin = 0), alpha = 0.2) + scale_fill_manual(breaks = input$countries, labels = input$countries, values = geocolour)+ geom_path()
    p <-p + geom_line(data = facetdata, aes(x = time, y = values, color = geo,text = paste0(format(as.Date(time,format="%Y-%m-%d"),"%b-%Y"),': ',values,'%')),size =0.5) + scale_colour_manual(breaks = input$countries, labels = input$countries, values = geocolour) + theme_bw()
    p <-p + geom_hline(yintercept=0, size=.1)
    p <-p + labs(title="HICP - All items (annual rate of change)\n", x = "", y = "Annual rate (%)", colour = "")
    p <-p + theme(plot.title = element_text(size = 10, margin=margin(0,0,30,0)),
                  axis.title=element_text(size = 6,vjust = 1),
                  axis.text=element_text(size = 5),
                  axis.text.x = element_text(angle = 90, vjust = 1, hjust=.5),
                  legend.position = "none")
    
    p <-p + facet_wrap(~ geo)
    ggplotly(p, tooltip = "text") %>% 
      config(displayModeBar = F) %>%
      layout(annotations = list(x = 1, y = -0.18, text = "Source: Eurostat (series = ei_cphi_m)", showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,font=list(size=8, color="grey"))) %>% 
      layout(annotations = list(x = 0.1, y = -0.18, text = '<a href=https://michael-jacobs.github.io/>michael-jacobs.github.io</a>', showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,font=list(size=8, color="grey"))) 
    
  })
  output$weight <-renderPlotly({
    validate(
      need(input$countries != "Please Select", "Select countries to include in the analysis."),
      need(input$expselect != "", "Select major group of expenditure to plot.")
    )
    e <- filter(expweights, geo %in% input$countries, format(as.Date(time,format="%Y-%m-%d"),"%Y")>=format(as.Date(input$date,format="%Y-%m-%d"),"%Y"), indic %in% input$expselect)
    
    f <- left_join(as.data.frame(unique(expweights$geo)), countries, by = c("unique(expweights$geo)" = "geo"))
    colnames(f) <-c("geo","colour")
    geocolour <-f$colour
    names(geocolour)<-f$geo
    
    p <-ggplot(data = e, aes(x = format(as.Date(time,format="%Y-%m-%d"),"%Y"), y=values, fill= geo, text = paste0(format(as.Date(time,format="%Y-%m-%d"),"%Y"),'<br>',indic,': ',values)))
    p <-p + geom_bar(stat = "identity") + theme_bw() + scale_fill_manual(values = geocolour)
    p <-p + theme(plot.title = element_text(size = 10, margin=margin(0,0,30,0)),
                  axis.title = element_text(size = 6, vjust = 1),
                  axis.text = element_text(size = 6),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  legend.position = "none")
    
    p <-p + facet_wrap(~ geo)
    p <-p + labs(title="HICP - expenditure weights by major group (per 1000)\n", x = "", y = "Weight (per 1000)", fill = "")
    
    ggplotly(p, tooltip = "text") %>% 
      config(displayModeBar = F) %>%
      layout(annotations = list(x = 1, y = -0.15, text = "Source: Eurostat (series = prc_hicp_inw)", showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,font=list(size=8, color="grey"))) %>%
      layout(annotations = list(x = 0.2, y = -0.15, text = '<a href=https://michael-jacobs.github.io/>michael-jacobs.github.io</a>', showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,font=list(size=8, color="grey"))) %>%
      layout(legend = list(font=list(size=6), x = 0.5, y = -0.2, xanchor = "center", orientation = 'h'))
    
    
  })
  output$annual<-renderPlotly({
    validate(
      need(input$countries != "Please Select", "Select countries to include in the analysis.")
    )
    ##Filter all countries for required dataset##
    
    d <-filter(gdp, unit == "Chain linked volumes, percentage change compared to same period in previous year", format(as.Date(time,format="%Y-%m-%d"),"%Y-%m")>=format(as.Date(input$date,format="%Y-%m-%d"),"%Y-%m"))
    if (nrow(d) == 0) return(NULL)
    d$quarter <- paste(format(as.Date(d$time,format = "%Y-%m-%d"),"%Y"),lubridate::quarter(as.Date(d$time,format = "%Y-%m-%d")),sep="-")
    
    e <-filter(gdp, unit == "Chain linked volumes, percentage change compared to same period in previous year", format(as.Date(time,format="%Y-%m-%d"),"%Y-%m")>=format(as.Date(input$date,format="%Y-%m-%d"),"%Y-%m"), geo %in% input$countries)
    if (nrow(e) == 0) return(NULL)
    e$quarter <- paste(format(as.Date(e$time,format = "%Y-%m-%d"),"%Y"),lubridate::quarter(as.Date(e$time,format = "%Y-%m-%d")),sep="-")
    
    f <- left_join(as.data.frame(unique(e$geo)), countries, by = c("unique(e$geo)" = "geo"))
    colnames(f) <-c("geo","colour")
    geocolour <-f$colour
    names(geocolour)<-f$geo
    p <-ggplot(d, aes(x = quarter, y = values, group = geo,text = paste0('<b>',geo,'</b>','<br>',lubridate::quarter(as.Date(time,format = "%Y-%m-%d"), with_year = T),': ',values,'%'))) + geom_line(size =0.1, colour  = "light grey")
    p <-p + geom_line(data = e, aes(x = quarter, y = values, color = geo),size =0.5) + scale_colour_manual(breaks = input$countries, labels = input$countries, values = geocolour) + theme_bw()
    p <-p + geom_hline(yintercept=0, size=.1)
    p <-p + labs(title="Quarterly GDP - Chain linked volumes (change compared to same period in previous year)\n", x = "Year-Quarter", y = "Annual change (%)", colour = "", size = 8)
    p <-p + theme(plot.title = element_text(size = 10, margin=margin(0,0,30,0)),
                  axis.title=element_text(size = 7, ),
                  axis.text.y=element_text(size = 7, vjust = 1),
                  axis.text.x = element_text(size = 4,angle = 90, vjust = 0.5, hjust=1),
                  panel.grid.major.x = element_blank(),
                  legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))
    
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = F) %>%
      layout(annotations = list(x = 1, y = -0.13, text = "Source: Eurostat (series = namq_10_gdp)", showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,font=list(size=9, color="grey"))) %>%
      layout(annotations = list(x = 0.2, y = -0.13, text = '<a href=https://michael-jacobs.github.io/>michael-jacobs.github.io</a>', showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,font=list(size=8, color="grey"))) 
    
  }) 
  output$index<-renderPlotly({
    validate(
      need(input$countries != "Please Select", "Select countries to include in the analysis.")
     )
    
    ##Filter all countries for required dataset##
    e <-filter(gdp, unit == "Chain linked volumes, index 2015=100", format(as.Date(time,format="%Y-%m-%d"),"%Y-%m")>=format(as.Date(input$date,format="%Y-%m-%d"),"%Y-%m"), geo %in% input$countries)
    if (nrow(e) == 0) return(NULL)
    e$quarter <- paste(format(as.Date(e$time,format = "%Y-%m-%d"),"%Y"),lubridate::quarter(as.Date(e$time,format = "%Y-%m-%d")),sep="-")
    
    f <- left_join(as.data.frame(unique(e$geo)), countries, by = c("unique(e$geo)" = "geo"))
    colnames(f) <-c("geo","colour")
    geocolour <-f$colour
    names(geocolour)<-f$geo
    
    p <-ggplot(e, aes(x = quarter, y = values-100, fill = geo,text = paste0('<b>',geo,'</b>',
                                                                            '<br>Quarter: ',lubridate::quarter(as.Date(time,format = "%Y-%m-%d"), with_year = T), 
                                                                            '<br>Index: ', values)))
    p <-p + geom_bar(stat = "identity") + theme_bw() + scale_fill_manual(values = geocolour)
    p <-p + scale_y_continuous(breaks=seq(-30,30,10), labels=seq(70,130,10))
    p <-p + labs(title="Quarterly GDP - Chain linked volumes (index 2015=100)\n", x = "", y = "Index (2015=100)", colour = "", size = 8)
    p <-p + theme(plot.title = element_text(size = 10, margin=margin(0,0,30,0)),
                  axis.title=element_text(size = 7, vjust = 1),
                  axis.text.y=element_text(size = 7),
                  axis.text.x = element_text(size = 3,angle = 90, vjust = 1, hjust=1),
                  panel.grid.major.x = element_blank(),
                  legend.position = "none")
    
    p <-p + facet_wrap(~ geo)
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = F) %>%
      layout(annotations = list(x = 1, y = -0.15, text = "Source: Eurostat (series = namq_10_gdp)", showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,font=list(size=8, color="grey"))) %>%
      layout(annotations = list(x = 0.2, y = -0.15, text = '<a href=https://michael-jacobs.github.io/>michael-jacobs.github.io</a>', showarrow = F, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0,font=list(size=8, color="grey"))) 
    
  }) 
  
}

shinyApp(ui, server)