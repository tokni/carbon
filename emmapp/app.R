#/home/andr31/R/github/carbon/

library(shiny)
library(shinycssloaders)
library(tidyverse)
library(plotly)
library(ggthemes)
library(ggdark)
options(scipen = 999)

#set up IPCC data
ipcc_2018 <- data.frame(degrees=c(rep("<1.5°",3),rep("<2°",3),rep("<3°",3)),
                        tcre_pct=rep(c("66%","50%","33%"),3),
                        ipcc=c(570000,770000,1080000,1320000,1690000,2270000,2400000,2800000,3250000)
) 
#set up weights
#w_pop <- .5
#w_gdp <- 0
#w_emm <- .5
#w_emm_h <- 0
#wgts <- c(.5,0,.5,0)

#testing country
#cntry <- "Afghanistan"

#test year
#year <- 2017

#read in data sources - NOTE: convert this to direct web sources if possible
globe <- read_csv('in/map.csv') %>% 
    rename(Country=`Country names`)

clean_n_gather <- function(path) {
    read_csv(path)[,-1] %>% 
    mutate_all(as.numeric) %>% 
    mutate(Country= read_csv(path)[,1][[1]]) %>% 
    select(Country,everything()) %>% 
    pivot_longer(-Country,names_to="Year")
}
#standardize data
summarize_dat <- function(dat,yr) {
    dat %>% 
        filter(Year==yr,!is.na(value)) %>% 
        mutate(w=value/sum(value)) %>% 
        select(Country,w)
}
pop <- clean_n_gather("in/pop.csv")
gdp <- clean_n_gather("in/gdp.csv")
emm <- clean_n_gather("in/emm.csv")
emm_w_h <- emm %>% 
    filter(!is.na(value)) %>% 
    group_by(Country) %>% 
    summarise(value=sum(value)) %>% 
    mutate(w=value/sum(value)) %>% 
    select(Country,w)

# Define UI for application that draws a histogram
ui <- fluidPage(

    tags$head(
        tags$style(HTML("
        body { 
            background-color: grey;
            color:black
        }
      .shiny-output-error-validation {
        color: red;
      }
        
        <!–– #g1{overflow-y:scroll;} ––>
        
        
                        ")
                   )
    ),
    # Application title
    titlePanel("Carbon Budget Sharing"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("year","Select Reference Year",choices=c(1960:2018),selected=2017),
            column(12,h5("*Select Geographic Location")),
            fluidRow(
                column(4,selectInput("cntnt","Continent",choices=c("World",'All',unique(globe$Continent)),selected='World',multiple=F)),
                column(4,selectInput("region","Region",choices=c('All',unique(globe$Region)),selected='All',multiple=T)),
                column(4,selectInput("cntry","Country",choices=c('All',unique(pop$Country)),selected='All',multiple=T))
            ),
            column(12,h5("*Select Weights (Must add to 100%)")),
            fluidRow(column(3,numericInput("w_pop","% Pop.",value=50,min=0,max=100)),
                     column(3,numericInput("w_gdp","% GDP",value=0,min=0,max=100)),
                     column(3,numericInput("w_emm","% CO2.",value=50,min=0,max=100)),
                     column(3,numericInput("w_emm_h","% CO2 (hist).",value=0,min=0,max=100))
                     )
        ),

        # Show a plot of the generated distribution
        mainPanel(h3("Graph 1:"), textOutput("test"),
            #div(style='max-height:500px; overflow-y: scroll; position: relative',plotlyOutput("g1"))
           column(12,plotlyOutput("g1") %>% withSpinner(color="#1A4C64"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$test <- renderText({
    })
    
    #waterfall logic for geo selections
    observeEvent(input$cntnt,{
      req(input$cntnt,!input$cntnt %in% c("World","All"))  
        
      choices <- globe %>% 
          filter(Continent==input$cntnt) %>% 
          distinct(Region) %>% 
          .[[1]]
      updateSelectInput(session,"region",choices=c('All',choices),selected='All')
    
      choices <- globe %>% 
          filter(Continent==input$cntnt) %>% 
          distinct(Country) %>% 
          .[[1]]
      updateSelectInput(session,"cntry",choices=c('All',choices),selected='All')
    })
    
    observeEvent(input$region,{
      req(input$region,input$region!="All")  
      choices <- globe %>% 
          filter(Region %in% input$region) %>% 
          distinct(Country) %>% 
          .[[1]]
      updateSelectInput(session,"cntry",choices=c('All',choices),selected='All')
    })
    
    data <- reactive({
        wgts <- c(input$w_pop,
                  input$w_gdp,
                  input$w_emm,
                  input$w_emm_h)/100
        validate(
            need(sum(wgts)== 1, "Please make sure all weights add up to 100%"),
            need(length(input$cntnt)>0, "Please select at least one continent to analyze"),
            need(length(input$region)>0, "Please select at least one region to analyze"),
            need(length(input$cntry)>0, "Please select at least one country to analyze")
        )
        
        pop_w <- summarize_dat(pop,input$year)
        gdp_w <- summarize_dat(gdp,input$year)
        emm_w <- summarize_dat(emm,min(input$year,2014)) #emmission data stops at 2014
        #set up factor data
        
        carbon <- pop_w %>%
            rename(pop=w) %>% 
            inner_join(gdp_w %>% rename(gdp=w),by="Country") %>% 
            inner_join(emm_w %>% rename(emm=w),by="Country") %>%
            inner_join(emm_w_h %>% rename(emm_h=w),by="Country") %>% 
            mutate(w_factor=pop*wgts[1]+gdp*wgts[2]+emm*wgts[3]+emm_h*wgts[4]) %>% 
            select(Country,w_factor) %>% 
            crossing(ipcc_2018) %>%  #full join ipcc data
            mutate(co2=ipcc*w_factor) %>% 
            inner_join(globe,by="Country") %>% #map to continents and regions
            filter(input$cntnt %in% c("World",'All')|Continent == input$cntnt)  %>% 
            filter(all(input$region=='All')|Region %in% input$region) %>% 
            filter(all(input$cntry=='All')|Country %in% input$cntry)
        
        carbon <- 
        if(input$cntnt=="World"&all(input$region=="All")&all(input$cntry=="All")) {
          carbon %>% 
            mutate(Continent="World") %>% 
            group_by(Continent,degrees,tcre_pct) %>% 
            summarise(co2=sum(co2)) %>% 
            mutate(total=sum(co2))
        }
        else if(!all(input$region=="All")&input$cntnt!="All"|!all(input$cntry=="All")) {
          carbon %>% 
            mutate(total=sum(co2))
        }
        else if(input$cntnt!="All"|!all(input$region=="All")) {
          carbon %>%
            group_by(Region,degrees,tcre_pct) %>% 
            summarise(co2=sum(co2)) %>% 
            mutate(total=sum(co2))
        }
        else {
          carbon %>%
            group_by(Continent,degrees,tcre_pct) %>% 
            summarise(co2=sum(co2)) %>% 
            mutate(total=sum(co2))
        }
            
    })

    output$g1 <- renderPlotly({
        
        graph1 <- ggplot(data=data() #%>% filter(Country=='China')
                         ) +
            geom_bar(stat="identity",position = 'dodge',aes(x=degrees,y=co2,
                                                            fill=factor(tcre_pct,levels = rev(levels(tcre_pct))),
                                                            text =paste(prettyNum(round(co2),big.mark=","),"metric tons,\n",prettyNum(round(total),big.mark=","),"total")
                                                            )
                     ) +
            dark_mode(theme_fivethirtyeight())+
            labs(title="IPCC 2018 Target",y="CO2 Mt",x="Degrees",fill="Percentile\nTCRE") 
        
        graph1 <- graph1 +
        
        if(!all(input$region=="All")&input$cntnt!="All"|!all(input$cntry=="All")) {
          facet_wrap(vars(Country))
        }
        else if(!input$cntnt %in% c("World","All")|!all(input$region=="All")) {
          facet_wrap(vars(Region))
        }
        else {
          facet_wrap(vars(Continent))
        }
        
        
        
       #if(length(input$cntry)>100|input$cntry=='All') {
       #
       #ggplotly(graph1,autosize = F, width = 1158, height = 2000) %>% 
       #    config(displayModeBar = F) 
       #}
       #else {
            ggplotly(graph1, tooltip = "text") %>% 
                config(displayModeBar = F) 
            
        #}
        
        })
}

# Run the application 
shinyApp(ui = ui, server = server)

#library(rsconnect)
#rsconnect::deployApp("/home/andr31/R/github/carbon/emmapp")
