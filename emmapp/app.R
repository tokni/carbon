#setwd('/home/andr31/R/github/carbon/emmapp/')

library(shiny)
library(shinycssloaders)
library("shinyBS")
library("rhandsontable")
library(tidyverse)
library(plotly)
library(ggthemes)
library(ggdark)
options(scipen = 999)

ipcc <- " 520,338      	 720,338      	 1,030,338      	 1,270,338      	 1,640,338      	 2,220,338      	 2,350,338      	 2,750,338      	 3,200,338      
" %>% 
  str_replace_all(",","") %>% 
  str_split("\t",simplify=T) %>% 
  trimws() %>% 
  as.numeric()
#set up IPCC data
ipcc_2018 <- data.frame(degrees=c(rep("<1.5°",3),rep("<2°",3),rep("<3°",3)),
                        tcre_pct=rep(c("66%","50%","33%"),3),
                        ipcc=ipcc#c(570000,770000,1080000,1320000,1690000,2270000,2400000,2800000,3250000)
) 
#set up default weights dataset
default_weights <- data.frame(Factor=c("GDP Reference Year","Inverse GDP Per Capita Reference Year","Population Reference Year"
                                       ,"Population 2050","Emissions <=2014 | Reference Year","Emissions Historical (<=2014)","Emissions Kyoto (1997-2014)"
                                       ,"Fossil Fuels - 2018","Low Bio Resource - 2012","Land Area - 2018"),
                              Weight=c(0,0,0.5,0,0.5,0,0,0,0,0)
                              )

## Chart 1 Math ##
#Inter-governmental panel on climate change set a target global carbon emmission budget (2018+ inc.)
#distributed according to magnitude of change in temperature (degrees)
#the probability of staying below that target change (TCRE percentile)
#e.g. lowest budget (570k mega ton) need to be in high probability (66%) of staying below best case target (1.5 deg)
#Model assigns individual Continent/Region/Countries shared budgets from this total 
#based on RELATIVE population & GDP (up to 2017), emmissions (present & historical, up to 2014) + (fators are weights)

#pop diviser for historical emissions factor inverse
pop_div <- 1000000

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
        select(Country,value,w)
}
pop <- clean_n_gather("in/pop.csv")
gdp <- clean_n_gather("in/gdp.csv")
emm <- clean_n_gather("in/emm.csv")
emm_w_h <- emm %>% 
    mutate(value=if_else(is.na(value),0,value)) %>% 
    group_by(Country) %>% 
    summarise(value=sum(value)) #%>% 
    #mutate(w=value/sum(value)) %>% 
    #select(Country,w)
#kyoto emmissions 1997 forward
emm_w_k <- emm %>% 
  mutate(value=if_else(is.na(value)|Year<1997,0,value)) %>% 
  group_by(Country) %>% 
  summarise(value=sum(value)) #%>% 
  #mutate(w=value/sum(value)) %>% 
  #select(Country,w)

pop50 <- read_csv("in/pop50.csv") %>% 
  select(1,5,6) %>% 
  rename(Country=1,y2020=2,y2050=3) %>% 
  mutate(value=if_else(is.na(y2050),0,y2050)) %>% 
  group_by(Country) %>% 
  summarise(value=sum(value)) %>% 
  mutate(w=value/sum(value)) %>% 
  select(Country,w)

test <- globe %>% 
  anti_join(pop50,by="Country") #Macedonia, FYR
pop50 <- pop50 %>% 
  mutate(Country=if_else(Country=="North Macedonia","Macedonia, FYR",Country))

foss <- read_csv("in/foss.csv") %>% 
  mutate_at(vars(-1),as.numeric) %>% 
  mutate_all(~ifelse(is.na(.),0,.)) %>% 
  rename(Country=1) %>% 
  mutate(value=if_else(is.na(`fossil fuels(MJ)`),0,`fossil fuels(MJ)`)) %>% 
  group_by(Country) %>% 
  summarise(value=sum(value)) %>% 
  mutate(w=value/sum(value)) %>% 
  select(Country,w)

land <- read_csv("in/land.csv") %>% 
  rename(Country=1,land=2) %>% 
  mutate(value=if_else(is.na(land),0,land)) %>% 
  group_by(Country) %>% 
  summarise(value=sum(value)) %>% 
  mutate(w=value/sum(value)) %>% 
  select(Country,w)

#small calculation - convert to getting data from URL
foot <- read_csv("in/foot.csv") %>% 
  rename(Country=1) %>% 
  mutate(value=1/per_capita*capita) %>% 
  mutate(value=if_else(is.na(value),0,value)) %>% 
  group_by(Country) %>% 
  summarise(value=sum(value)) %>% 
  mutate(w=value/sum(value)) %>% 
  select(Country,w)

test <- globe %>% 
  anti_join(foot,by="Country") #Macedonia, FYR
foot <- foot %>% 
  mutate(Country=if_else(Country=="Bahamas","Bahamas, The",Country)) %>% 
  mutate(Country=if_else(Country=="Congo","Congo, Rep.",Country)) %>% 
  mutate(Country=if_else(Country=="Congo, Democratic Republic of","Congo, Dem. Rep.",Country)) %>%
  mutate(Country=if_else(Country=="Côte d'Ivoire","Cote d'Ivoire",Country)) %>%
  mutate(Country=if_else(Country=="Egypt","Egypt, Arab Rep.",Country)) %>%
  mutate(Country=if_else(Country=="Swaziland","Eswatini",Country)) %>%
  mutate(Country=if_else(Country=="Gambia","Gambia, The",Country)) %>%
  mutate(Country=if_else(Country=="Iran, Islamic Republic of","Iran, Islamic Rep.",Country)) %>% 
  mutate(Country=if_else(Country=="Korea, Democratic People's Republic of","Korea, Dem. People’s Rep.",Country)) %>%
  mutate(Country=if_else(Country=="Korea, Republic of","Korea, Rep.",Country)) %>%
  mutate(Country=if_else(Country=="Slovakia","Slovak Republic",Country)) %>%
  mutate(Country=if_else(Country=="Saint Kitts and Nevis","St. Kitts and Nevis",Country)) %>%
  mutate(Country=if_else(Country=="Saint Lucia","St. Lucia",Country)) %>%
  mutate(Country=if_else(Country=="Saint Vincent and Grenadines","St. Vincent and the Grenadines",Country)) %>%
  mutate(Country=if_else(Country=="Tanzania, United Republic of","Tanzania",Country)) %>%
  mutate(Country=if_else(Country=="United States of America","United States",Country)) %>%
  mutate(Country=if_else(Country=="Venezuela, Bolivarian Republic of","Venezuela, RB",Country)) %>%
  mutate(Country=if_else(Country=="Viet Nam","Vietnam",Country)) %>%
  mutate(Country=if_else(Country=="Yemen","Yemen, Rep.",Country))
  
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
  includeHTML("www/basic.html"),
  includeCSS("www/bootstrap.min.css"),

    # Application title
    titlePanel("Carbon Budget Sharing Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(div(class="sidepan",
        sidebarPanel(
          div(class="year",
            selectInput("year","Select Reference Year",choices=c(1960:2017),selected=2017),
            span(class="tooltiptext","Select 'as-of' date for all data, excluding emissions (<=2014), land, footprint, and fossil fuels (2018)")
            )
#see details for styling hovers: https://www.w3schools.com/howto/tryit.asp?filename=tryhow_css_tooltip
            ,h5("Select Geographic Location"),
            fluidRow(
                column(4,selectInput("cntnt","Continent",choices=c("World",'All',unique(globe$Continent)),selected='World',multiple=F)),
                column(4,selectInput("region","Region",choices=c('All',unique(globe$Region)),selected='All',multiple=T)),
                column(4,selectInput("cntry","Country",choices=c('All',unique(pop$Country)),selected='All',multiple=T))
            ),
            h5(HTML("Select Factor Weights | <i>Decimals, must add up to 100%</i>")),
            #fluidRow(
            #         column(3,numericInput("w_pop","% Pop.",value=50,min=0,max=100,step=10)),
            #         column(3,numericInput("w_gdp","% GDP",value=0,min=0,max=100,step=10)),
            #         column(3,numericInput("w_emm","% CO2.",value=50,min=0,max=100,step=10)),
            #         column(3,numericInput("w_emm_h","% CO2 (hist).",value=0,min=0,max=100,step=10))
            #         ),
            #br(),
            fluidRow(
                column(12,rHandsontableOutput("weights"))
               #,textOutput("test")
              ),
            br(),
            numericInput("end","Select Forecast End Date",value=2100,min=2100,max=2100),
            numericInput("start","Select Forecast Baseline Date",value=2005,min=2005,max=2005)
        )
        ),

        # Show a plot of the generated distribution
        mainPanel(h3("IPCC Target(s) vs. Probability: Remaining Carbon Budget")
            #div(style='max-height:500px; overflow-y: scroll; position: relative',plotlyOutput("g1"))
          ,column(12,plotlyOutput("g1") %>% withSpinner(color="#4CAF50")),
           h3("Actual vs. Forecasted Consumption: Yearly Drilldown"),
           column(12,plotlyOutput("g2") %>% withSpinner(color="#4CAF50")),
          fluidRow(style = "padding-bottom:20px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    #create dynamic table for weights
    table_weights <- reactive({
      if (is.null(input$weights)) {
      d <- default_weights
      }
      else {
      d <- hot_to_r(input$weights)
      }
      d 
    })
    output$weights <- renderRHandsontable({
      rhandsontable(table_weights(),rowHeaders = NULL, width =540, stretchH = "all") %>% 
        hot_cols(columnSorting = F) %>% 
        hot_col("Factor", readOnly = TRUE) %>% 
        hot_col("Weight", format = "0%") %>% 
        hot_validate_numeric(cols = "Weight",min = 0, max = 1) #%>%
        #hot_cols("Weight",validator = "
        #     function (value, callback) {
        #      setTimeout(function(){
        #        callback(value != 0);
        #      }, 1000)
        #     }",
        #         allowInvalid = FALSE)
      
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
        wgts <- table_weights()$Weight
        #c(input$w_pop,
        #          input$w_gdp,
        #          input$w_emm,
        #          input$w_emm_h)/100
        shiny::validate(
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
            rename(pop=w,pop_v=value) %>% 
            inner_join(gdp_w %>% rename(gdp=w),by="Country") %>% 
            #formula for inverse gdp factor - note value field is overriden each step so mutates performed between joins
              mutate(value=(1/(value/pop_v))*pop_v) %>% 
              mutate(value=if_else(is.na(value)|is.infinite(value),0,value)) %>% 
              mutate(gdp_i=value/sum(value)) %>%
              select(-value) %>% 
           inner_join(emm_w_h, by="Country") %>% 
           #formula for inverse hist em factor 
             mutate(value=(1/( 
               value/
                 (pop_v/pop_div)
             ))*pop_v
             ) %>% 
             mutate(value=if_else(is.na(value)|is.infinite(value),0,value)) %>% 
             mutate(emm_h=value/sum(value)) %>%
             select(-value) %>% 
           inner_join(emm_w_k, by="Country") %>% 
           #formula for inverse kyoto em factor 
             mutate(value=(1/( 
               value/
                 (pop_v/pop_div)
             ))*pop_v
             ) %>% 
             mutate(value=if_else(is.na(value)|is.infinite(value),0,value)) %>% 
             mutate(emm_k=value/sum(value)) %>%
             select(-value) %>% 
           inner_join(emm_w %>% select(-value) %>% rename(emm=w),by="Country") %>%
           inner_join(pop50 %>% rename(pop50=w),by="Country") %>% 
           inner_join(foss %>% rename(foss=w),by="Country") %>% 
           inner_join(land %>% rename(land=w),by="Country") %>% ## Need to cleanup footprint data !!!
           inner_join(foot %>% rename(lbio=w),by="Country") %>% 
          ##start derivation - order of weight input matters!!!
           mutate(w_factor=#pop*wgts[1]+gdp*wgts[2]+emm*wgts[3]+emm_h*wgts[4]) %>%
                  gdp*wgts[1] +
                  gdp_i*wgts[2] +
                  pop*wgts[3] +
                  pop50*wgts[4] +
                  emm*wgts[5] +
                  emm_h*wgts[6] +
                  emm_k*wgts[7] +
                  foss*wgts[8] +
                  lbio*wgts[9] +
                  land*wgts[10]
          ) %>% 
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
      #event_data("plotly_click") %>% 
      #  .[[4]] -> budget
      #ref_year <- data2() %>% 
      #  filter(Year>=2018) %>% 
      #  mutate(Co2_sum=cumsum(Co2),
      #         flag=if_else(Co2_sum>=budget,1,0)
      #  ) %>% 
      #  filter(flag==1) %>%
      #  slice(1) %>%
      #  ungroup() %>% 
      #  select(Year) %>% 
      #  .[[1]] 
      ##budget
      #ref_year
      #table_weights()$Weight
      #nrow(data())
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
                                                            text =paste(degrees,"deg. |",tcre_pct,"prob.:\n",prettyNum(round(co2),big.mark=","),"Co2 mt (megatons)"#,prettyNum(round(total),big.mark=","),"total"
                                                                        )
                                                            )
                     ) +
            labs(title="IPCC 2018 Target",y="CO2 mt",x="\nDegrees x Probability",fill="Probability of Success\nTCRE") +
            dark_mode(theme_fivethirtyeight()) #+ theme(axis.title.x = element_text(margin=c(10,0,0,0)),axis.title.x = element_text(angle=0))  
        
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
