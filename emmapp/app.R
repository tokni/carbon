#setwd('/home/andr31/R/github/carbon/emmapp/')

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

## Chart 1 Math ##
#Inter-governmental panel on climate change set a target global carbon emmission budget (2018+ inc.)
#distributed according to magnitude of change in temperature (degrees)
#the probability of staying below that target change (TCRE percentile)
#e.g. lowest budget (570k mega ton) need to be in high probability (66%) of staying below best case target (1.5 deg)
#Model assigns individual Continent/Region/Countries shared budgets from this total 
#based on relative population & GDP (up to 2017), emmissions (present & historical, up to 2014)

#set up weights
#w_pop <- .5
#w_gdp <- 0
#w_emm <- .5
#w_emm_h <- 0
#wgts <- c(.5,0,.5,0)

#testing country
##cntry <- "Austria"
#testing forecast
##start <- 2005
##end <- 2100

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
    pivot_longer(-Country,names_to="Year") %>% 
    mutate(Year=as.numeric(Year))
}
#standardize data
summarize_dat <- function(dat,yr) {
    dat %>% 
        mutate(value=if_else(is.na(value),0,value)) %>% 
        filter(Year==yr#,!is.na(value) ## Notes: NA values being treated as 0!!! ##
               ) %>% 
        mutate(w=value/sum(value)) %>% 
        select(Country,w)
}
pop <- clean_n_gather("in/pop.csv")
gdp <- clean_n_gather("in/gdp.csv")
emm <- clean_n_gather("in/emm.csv")
emm_w_h <- emm %>% 
    #filter(!is.na(value)) %>%
    mutate(value=if_else(is.na(value),0,value)) %>% 
    group_by(Country) %>% 
    summarise(value=sum(value)) %>% 
    mutate(w=value/sum(value)) %>% 
    select(Country,w)

## Chart2 Math ##
#project % reductions based on 2005 data in 5 year increments (2020-50) and 25 (2050-100) 
co2_reduction <- data.frame(
  reduc=c(10,15,20,22,30,35,20,20,20), #make these an input
  Year=c(seq(2020,2050,by=5),2075,2100)
) %>% 
mutate(factor=(100-reduc)/100) #forecasting factor to be applied to 2005 emission value
#forecast formula, emission data stops at 2014
#Y_n1 + (delta_time=1year)*(rp-Y_n1)/(rp_y-y_n1)
##Y_n1=carbon in prior year
##rp=reference point, e.g. 2020 for 2015-2019
##y_n1=prior year value
## Linear Interpolation ##


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
    titlePanel("IPCC: Carbon Budget Sharing Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("year","Select Reference Year",choices=c(1960:2017),selected=2017),
            column(12,h5("*Select Geographic Location")),
            fluidRow(
                column(4,selectInput("cntnt","Continent",choices=c("World",'All',unique(globe$Continent)),selected='World',multiple=F)),
                column(4,selectInput("region","Region",choices=c('All',unique(globe$Region)),selected='All',multiple=T)),
                column(4,selectInput("cntry","Country",choices=c('All',unique(pop$Country)),selected='All',multiple=T))
            ),
            column(12,h5("*Select Weights (Must add to 100%)")),
            fluidRow(column(3,numericInput("w_pop","% Pop.",value=50,min=0,max=100,step=10)),
                     column(3,numericInput("w_gdp","% GDP",value=0,min=0,max=100,step=10)),
                     column(3,numericInput("w_emm","% CO2.",value=50,min=0,max=100,step=10)),
                     column(3,numericInput("w_emm_h","% CO2 (hist).",value=0,min=0,max=100,step=10))
                     ),
            numericInput("end","Select Forecast End Date",value=2100,min=2100,max=2100),
            numericInput("start","Select Forecast Baseline Date",value=2005,min=2005,max=2005)
        ),

        # Show a plot of the generated distribution
        mainPanel(h3("IPCC Target(s) vs. Probability: Remaining Carbon Budget")#, textOutput("test"),
            #div(style='max-height:500px; overflow-y: scroll; position: relative',plotlyOutput("g1"))
          ,column(12,plotlyOutput("g1") %>% withSpinner(color="#1A4C64")),
           h3("Actual vs. Forecasted Consumption: Yearly Drilldown"),
           column(12,plotlyOutput("g2") %>% withSpinner(color="#1A4C64"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
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
    
    data2 <- reactive({
      
      fcast <- emm %>%
        drop_na() %>% 
        inner_join(globe,by="Country") %>% #map to continents and regions
        filter(input$cntnt %in% c("World",'All')|Continent == input$cntnt)  %>% 
        filter(all(input$region=='All')|Region %in% input$region) %>% 
        filter(all(input$cntry=='All')|Country %in% input$cntry)
        #filter(Region=="EU28",Country %in% c("Austria","Croatia"))
      
      fcast <- 
        if(input$cntnt=="World"&all(input$region=="All")&all(input$cntry=="All")) { #default
          fcast %>% 
            mutate(Continent="World", Region='All', Country='All') %>% 
            group_by(Continent,Region,Country,Year) %>% 
            summarise(value=sum(value)) %>% 
            mutate(total=sum(value))
        }
      else if(!all(input$region=="All")&input$cntnt!="All"|!all(input$cntry=="All")) {
        fcast %>% 
          group_by(Country) %>% 
          mutate(total=sum(value))
      }
      else if(input$cntnt!="All"|!all(input$region=="All")) {
        fcast %>%
          mutate(Country='All') %>% 
          group_by(Continent,Region,Country,Year) %>% 
          summarise(value=sum(value)) %>% 
          mutate(total=sum(value))
      }
      else {
        fcast %>%
          mutate(Country='All',Region='All') %>% 
          group_by(Continent,Region,Country,Year) %>% 
          summarise(value=sum(value)) %>% 
          mutate(total=sum(value))
      }
    
    fcast <- fcast %>% 
      #filter(Country==cntry) %>% 
      mutate(Co2=value/1000) %>% 
      select(-value) 
      #mutate(Year=as.numeric(Year)) %>% 
  #logic to create forecast rows
    rows <- unique(paste(fcast$Continent,fcast$Region,fcast$Country,sep="-")) %>%
      rep(length(c((max(emm$Year)+1):input$end))) %>% 
      strsplit("-")
    Continent <- sapply(rows, "[[", 1)
    Region <- sapply(rows, "[[", 2)
    Country <- sapply(rows, "[[", 3)
    rows2 <- data.frame(Continent=Continent,Region=Region,Country=Country) %>% 
      arrange(Continent,Region,Country)
    rows2$Year <- rep(c((max(emm$Year)+1):input$end),length(Country)/length(c((max(emm$Year)+1):input$end)))
    rows2 <- rows2 %>% 
      mutate(Co2=NA)
    fcast <- fcast %>% 
      bind_rows(rows2) %>% 
      arrange(Continent,Region,Country,Year) #%>% 
      #group_by(Continent,Region,Country) 
    
    ## Logic for 2005 data projection start ##
    
    baseline <- fcast %>% 
      filter(Year==input$start) %>% 
      rename(Co2_05=Co2) %>% 
      select(Continent,Region,Country,Co2_05)
    
    fcast <- fcast %>% 
      left_join(co2_reduction %>% mutate(year2=Year),by="Year") %>% #add reference points
      left_join(baseline,by=c("Continent","Region","Country")) %>% 
      mutate(Co2=if_else(is.na(Co2),factor*Co2_05#fcast[fcast$Year==2005,]$Co2
                         ,Co2)
      ) %>% #backfill reference points to apply formula
      select(-Co2_05) %>% 
      fill(Co2,.direction="up") %>% 
      fill(year2,.direction="up") 
    #calculate forecast (for year>2014), needs to be in loop because it's rolling
    for (i in max(emm$Year)+1:input$end) {
      fcast <- fcast %>% 
        mutate(Co2=if_else(Year==i
                           ,lag(Co2)+(Year-lag(Year))*(Co2-lag(Co2))/(year2-lag(Year))
                           ,Co2))
    }
    #clean up
    fcast <- fcast %>% 
      select(Continent,Region,Country,Year,Co2) %>% 
      mutate(Projection=if_else(Year>max(emm$Year),"Forecasted","Actual"))
    
    fcast
    
    })
    
    output$test <- renderText({
      event_data("plotly_click") %>% 
        .[[4]] -> budget
      ref_year <- data2() %>% 
        filter(Year>=2018) %>% 
        mutate(Co2_sum=cumsum(Co2),
               flag=if_else(Co2_sum>=budget,1,0)
        ) %>% 
        filter(flag==1) %>%
        slice(1) %>%
        ungroup() %>% 
        select(Year) %>% 
        .[[1]] 
      #budget
      ref_year
    })
    
    output$g2 <- renderPlotly({
    
    graph2 <- ggplot(data2()) +
      geom_bar(stat="identity",aes(x=Year,y=Co2,fill=Projection
                                   ,text =paste(Year, Projection,":\n",prettyNum(round(Co2,2),big.mark=","),"mega tons")
      )
      ) +
      dark_mode(theme_fivethirtyeight())+
      labs(title=#paste(
        "CO2 Emission Projections"
                       #)
        ,y="CO2 Mt",x="Year",fill="Projection") +
      scale_x_continuous(name="Year",breaks = seq(1960,2100,10)) +
      theme(axis.text.x = element_text(angle=45))
    
    graph2 <- graph2 +
      
      if(!all(input$region=="All")&input$cntnt!="All"|!all(input$cntry=="All")) {
        facet_wrap(vars(Country))
      }
    else if(!input$cntnt %in% c("World","All")|!all(input$region=="All")) {
      facet_wrap(vars(Region))
    }
    else {
      facet_wrap(vars(Continent))
    }
    
    #add hover for budget expired
    if(length(event_data("plotly_click") %>% .[[4]]) >0 &
       length(unique(data2()$Continent))==1&
       length(unique(data2()$Region))==1&
       length(unique(data2()$Country))==1
    ) {
      event_data("plotly_click") %>% 
        .[[4]] -> budget
      ref_year <- data2() %>% 
        filter(Year>=2018) %>% 
        mutate(Co2_sum=cumsum(Co2),
               flag=if_else(Co2_sum>=budget,1,0)
        ) %>% 
        filter(flag==1) %>%
        slice(1) %>%
        ungroup() %>% 
        select(Year) %>% 
        .[[1]] 
      if(length(ref_year)==0) {
        ref_year <- 2100
      }
      graph2 <- graph2 + 
        geom_vline(aes(xintercept = ref_year,linetype = "Budget Expiration Year")) + 
        geom_text(aes(ref_year,0,label = ref_year, vjust = -1)) 
    }
    
    
    ggplotly(graph2, tooltip = "text")
    })
    

    output$g1 <- renderPlotly({
        
        graph1 <- ggplot(data=data() #%>% filter(Country=='China')
                         ) +
            geom_bar(stat="identity",position = 'dodge',aes(x=degrees,y=co2,
                                                            fill=factor(tcre_pct,levels = rev(levels(tcre_pct))),
                                                            text =paste(prettyNum(round(co2),big.mark=","),"mega tons,\n",prettyNum(round(total),big.mark=","),"total")
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
