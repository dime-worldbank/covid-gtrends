# Google Trends Dashboard

# https://shiny.rstudio.com/articles/plot-caching.html

END_DATE_TEXT <- "December 31, 2022"
END_DATE <- "2022-12-31"

# PACKAGES AND SETUP ===========================================================

#### Setting directory so will work locally
if (Sys.info()[["user"]] == "robmarty") {
  setwd("~/Documents/Github/covid-gtrends/Dashboard")
}

#### Pacakges
library(sparkline)
library(shinydashboard)
library(RColorBrewer)
library(shinythemes)
library(dplyr)
library(rmarkdown)
library(lubridate)
library(shiny)
library(ggplot2)
library(tidyr)
library(shinyWidgets)
library(shinyjs)
library(stringr)
library(htmlwidgets)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(formattable)
library(tidyr)
library(htmltools)
library(scales)
library(lubridate)
library(hrbrthemes)
library(cachem)
library(data.table)
#library(viridis)
#library(DT)
#library(bcrypt)
#library(geosphere)
#library(sf)
#library(raster)
#library(rgdal)
#library(rgeos)
#library(geosphere)
#library(ngram)
#library(stringdist)

shinyOptions(cache = cachem::cache_disk("./cache"))

# Functions --------------------------------------------------------------------
as.character.htmlwidget <- function(x, ...) {
  htmltools::HTML(
    htmltools:::as.character.shiny.tag.list(
      htmlwidgets:::as.tags.htmlwidget(
        x
      ),
      ...
    )
  )
}

add_deps <- function(dtbl, name, pkg = name) {
  tagList(
    dtbl,
    htmlwidgets::getDependency(name, pkg)
  )
}

# Prep Objects -----------------------------------------------------------------
cor_after_dates <- readRDS(file.path("data", "begin_date_cor.Rds"))
cor_end_dates <- readRDS(file.path("data", "end_date_cor.Rds"))

# Defaults
gtrends_df       <- readRDS(file.path("data", paste0("gtrends_since_",cor_after_dates[1],"_",max(cor_end_dates),".Rds")))
gtrends_spark_df <- readRDS(file.path("data", paste0("gtrends_spark_since_",cor_after_dates[1],"_",max(cor_end_dates),"_large.Rds")))
cor_df           <- readRDS(file.path("data", paste0("correlations_since_",cor_after_dates[1],"_",max(cor_end_dates),".Rds")))
world            <- readRDS(file.path("data", "world.Rds"))

# Needed for keyword_cor
cor_neg1_pos1_df <- seq(from=-1, to=1, by=.1) %>%
  as.data.frame() %>%
  dplyr::rename(cor = ".") %>%
  mutate(cor = cor %>% as.factor())

LOAD_GTRENDS_INIT <- TRUE

keyword_list <- gtrends_df$keyword_en %>% unique()

keywords_df <- readRDS(file.path("data", "keywords.Rds"))
languges_df <- readRDS(file.path("data", "countries_lang.Rds"))

## Prep keywords
keywords_df$keyword_en <- keywords_df$keyword_en %>% tools::toTitleCase()
keywords_df <- keywords_df[keywords_df$keyword_en %in% gtrends_df$keyword_en,]
keywords_df$category <- keywords_df$category %>% tools::toTitleCase() 

# FUNCTIONS ========
gtpath <- ""

# https://stackoverflow.com/questions/49885176/is-it-possible-to-use-more-than-2-colors-in-the-color-tile-function
color_tile2 <- function (...) {
  formatter("span", style = function(x) {
    style(display = "block",
          padding = "0 4px", 
          font.weight = "bold",
          `border-radius` = "4px", 
          `background-color` = csscolor(matrix(as.integer(colorRamp(...)(normalize(as.numeric(x)))), 
                                               byrow=TRUE, dimnames=list(c("red","green","blue"), NULL), nrow=3)))
  })}

# UI -==========================================================================
ui <- fluidPage(
  
  #tags$head(includeHTML(("google-analytics.html"))),
  
  navbarPage(
    theme = shinytheme("cosmo"), #cosmo, journal, flatly, sandstone
    collapsible = TRUE,
    title = "COVID-19 & Google Trends",
    
    id = "nav",
    
    # ** Country Level ----------------------------------------------------------
    tabPanel(
      id = "country_level",
      "Country Level",
      tags$head(includeCSS("styles.css")),
      
      dashboardBody(
        
        fluidRow(
          column(2, align = "center", offset = 0,
                 
                 uiOutput("select_country_ui")
                 
          ),
          column(2, align = "center", offset = 0,
                 selectInput(
                   "select_covid_type_map",
                   
                   label = strong(HTML("Cases or Deaths")),
                   choices = c("Cases", "Deaths"),
                   selected = "Cases",
                   multiple = F
                 )
          ),
          column(2, align = "center",
                 selectInput(
                   "select_cor_type_country",
                   label = HTML("<b>Correlation:<br>Lead or Lag</b>"),
                   choices = c("Best Lead/Lag", 
                               "No Lead/Lag"),
                   selected = "Best Lead/Lag",
                   multiple = F
                 )
          ),
          column(2, align = "center",
                 selectInput(
                   "select_cor_raw_MA_country",
                   label = HTML("<b>Correlation: Raw vs<br>Moving Average</b>"),
                   choices = c("Raw Value", 
                               "7 Day Moving Average"),
                   selected = "Raw Value",
                   multiple = F
                 )
          ),
          column(2, align = "center",
                 selectInput(
                   "select_begin_date_country",
                   label = strong("Correlation After"),
                   choices = cor_after_dates,
                   selected = "2020-02-01",
                   multiple = F
                 )
          ),
          column(2, align = "center",
                 selectInput(
                   "select_search_category_country",
                   label = strong("Search Term Categories"),
                   choices = c("All",
                               sort(unique(keywords_df$category))),
                   selected = "Symptoms",
                   multiple = F
                 )
          )
        ),
        
        # fluidRow(
        #   column(1, ""),
        #   column(10, align = "center",
        #          em(h5('For "Correlation: Lead/Lag", when selecting "Best Lead/Lag", correlations
        #             are computed when using search interest lead/lag values 
        #             of COVID-19 from -21 to 21 days; among these correlations, 
        #             the largest correlation is used. For "Correlation: Raw vs Moving Average", 
        #             when "Raw Value" is selected, the raw values of search interest 
        #             and COVID-19 are used; when "7 Day Moving Average" is selected,
        #             the correlation is computed using a 7 day moving average of search interest 
        #             and COVID-19.'))),
        #   column(1, "")
        # ),
        
        fluidRow(
          column(12, align = "center",
                 h3(textOutput("country_name"))
          )
        ),
        
        fluidRow(
          column(6, align = "center", offset = 3,
                 strong(htmlOutput("trends_country_subtitle"), align="center"),
          )
        ),
        
        br(),
        
        fluidRow(
          column(8, align = "center", offset = 2,
                 div(style = 'overflow-y: scroll; height:300px', htmlOutput("line_graph_country"))
          )
        ),
        hr(),
        
        wellPanel(
          fluidRow(
            column(4, align = "center", offset = 4,
                   uiOutput("select_keyword_country_ui")
            )
          ),
          
          fluidRow(
            column(6, align = "center", offset = 3,
                   htmlOutput("translation_text")
            )
          ),
          
          fluidRow(
            column(5, align = "center",
                   h3("Historic Trends"),
                   strong(paste0("Data Available Until ", END_DATE_TEXT))
            ),
            column(7, align = "center",
                   h3("Real Time Data: Search Interest in Past 90 Days"),
                   fluidRow(
                     column(8, align = "center", offset = 2,
                            HTML("<strong>Trends at different time spans and at subnational levels can be
                            explored on the 
                            <a href='https://trends.google.com/trends/'>Google Trends website.</a></strong>")
                     )
                   )
            )
          ),
          
          fluidRow(
            column(5, align = "left", 
                   
                   div(style='color:black; font-size:20px; background-color: white; border: 1px solid #ccc; border-radius: 3px; margin: 10px 0; padding-left: 16px; padding-right: 16px; padding-bottom: 5px; padding-top: 0px;',
                       
                       fluidRow(
                         column(12, align = "left", offset = 0,
                                htmlOutput("line_graph_country_key_title_2")
                         )
                       ),
                       
                       fluidRow(
                         plotlyOutput("line_graph_country_key",
                                      height = "270px") # 225
                       ),
                       fluidRow(
                         column(11, offset = 1, align = "left",
                                h6("Original daily values range from 0 to 100, where 100 represents
                               peak search activity for a keyword.") 
                         )
                       )
                       
                   ),
                   
            ),
            column(4, align = "center",
                   tags$div(id="wrapper"),
                   
                   uiOutput("gtrends_html_trends")
            ),
            column(3, align = "center",
                   tags$div(id="wrapper2"),
                   uiOutput("gtrends_html_map")
            )
          )
        )
      )
    ),
    
    # ** Global Level ----------------------------------------------------------
    tabPanel(
      "Global Level",
      tags$head(includeCSS("styles.css")),
      
      dashboardBody(
        
        fluidRow(
          column(2, align = "center", offset = 0,
                 selectInput(
                   "select_continent",
                   label = strong("Continent"),
                   choices = c("All",
                               unique(sort(cor_df$continent))),
                   selected = "All",
                   multiple = F
                 )
          ),
          column(2, align = "center",
                 selectInput(
                   "select_covid_type",
                   label = strong("Cases or Deaths"),
                   choices = c("Cases", "Deaths"),
                   selected = "Cases",
                   multiple = F
                 )
          ),
          column(2, align = "center",
                 selectInput(
                   "select_cor_type",
                   label = HTML("<b>Correlation:<br>Lead or Lag</b>"),
                   choices = c("Best Lead/Lag", "No Lead/Lag"),
                   selected = "Best Lead/Lag",
                   multiple = F
                 )
          ),
          column(2, align = "center",
                 selectInput(
                   "select_cor_raw_MA",
                   label = HTML("<b>Correlation: Raw vs<br>Moving Average</b>"),
                   choices = c("Raw Value", 
                               "7 Day Moving Average"),
                   selected = "Raw Value",
                   multiple = F
                 )
          ),
          column(2, align = "center",
                 selectInput(
                   "select_begin_date",
                   label = strong("Correlation After"),
                   choices = cor_after_dates,
                   selected = "2020-02-01",
                   multiple = F
                 )
          ),
          column(2, align = "center",
                 selectInput(
                   "select_search_category",
                   label = strong("Search Term Categories"),
                   choices = c("All",
                               sort(unique(keywords_df$category))),
                   selected = "Symptoms",
                   multiple = F
                 )
          )
          
          
        ),
        # 
        # fluidRow(
        #   column(1, ""),
        #   column(10, align = "center",
        #          em(h5('For "Correlation: Lead/Lag", when selecting "Best Lead/Lag", correlations
        #             are computed when using search interest lead/lag values 
        #             of COVID-19 from -21 to 21 days; among these correlations, 
        #             the largest correlation is used. For "Correlation: Raw vs Moving Average", 
        #             when "Raw Value" is selected, the raw values of search interest 
        #             and COVID-19 are used; when "7 Day Moving Average" is selected,
        #             the correlation is computed using a 7 day moving average of search interest 
        #             and COVID-19.'))),
        #   column(1, "")
        # ),
        
        
        
        hr(),
        
        h2(textOutput("which_keyword_title"),
           align = "center"),
        
        fluidRow(
          column(6, offset = 3,
                 wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 500px",
                           fluidRow(
                             column(12, align = "center", offset = 0,
                                    htmlOutput("cor_distribution_text"),
                             ),
                           ),
                           fluidRow(
                             column(10, align = "center", offset = 1,
                                    htmlOutput("cor_distribution_text_2"),
                             ),
                             column(1,
                             )
                           ),
                           fluidRow(
                             uiOutput("max_cor_hist_ui")
                           )
                 ),
          ),
        ),
        
        hr(),
        
        fluidRow(
          
          column(4, align = "center", offset = 4,
                 uiOutput("select_keyword_ui")
                 
                 
                 
          )
          
        ),
        
        fluidRow(
          column(4, align = "center", offset = 4,
                 htmlOutput("title_cor_lag")
          )
        ),
        
        fluidRow(
          column(4, align = "center", offset = 2,
                 plotOutput("keyword_cor", height = "130px")
          ),
          column(4, align = "center",
                 plotOutput("keyword_lag", height = "130px")
          )
        ),
        
        tabsetPanel(type = "tabs",
                    
                    tabPanel(tags$div( 
                      
                      HTML(paste(tags$span(style="color:white", "--------------------------------------------------------------------------------------"), sep = "")) 
                      
                    )),
                    
                    tabPanel(id = "map_view",
                             "Map View", 
                             
                             br(),
                             fluidRow(
                               column(12, align = "center",
                                      htmlOutput("map_text")
                               )
                             ),
                             
                             br(),
                             
                             fluidRow(
                               column(12, align = "center", offset = 0,
                                      uiOutput("cor_map_leaflet")
                                      
                               )
                             )
                             
                    ),
                    tabPanel(id = "table_view",
                             "Table View",
                             
                             br(),
                             
                             fluidRow(
                               column(12, align = "center",
                                      htmlOutput("global_table_title")
                               )
                             ),
                             #fluidRow(
                             #   column(12, align = "center",
                             #          HTML(paste0("<em>Trends shown from February 1 - ", END_DATE_TEXT, "</em>"))
                             #   )
                             # ),
                             
                             br(),
                             
                             fluidRow(
                               column(4,
                               ),
                               column(4,align = "center",
                                      uiOutput("ui_select_sort_by")
                               ),
                               column(4,
                               )
                             ),
                             
                             fluidRow(
                               
                               column(8, align = "center", offset = 2,
                                      htmlOutput("line_graph"),
                               )
                               
                             )
                             
                             
                    ), selected = "Map View"),
        
        fluidRow(
          column(12, align = "center",
                 
                 
          )
        )
        
      )
    ),
    
    # ** Landing Page ----------------------------------------------------------
    tabPanel(
      "Information",
      tags$head(includeCSS("styles.css")),
      
      
      dashboardBody(
        
        fluidRow(
          column(12, align = "center",
                 #h1("Google Trends Data for Understanding COVID-19")
                 h1("The Evolution of the Pandemic Through the Lens of Google Searches"),
                 h2("A Global Dashboard for Monitoring COVID-19")
          )
        ),
        
        fluidRow(
          column(6, align = "center", offset = 3,
                 
                 hr(),
                 h2("Overview")
          ),
          column(6, align = "left", offset = 3,
                 
                 HTML(paste0("<h4>When we fall ill, 
                 <a href='https://blog.google/technology/health/using-symptoms-search-trends-inform-covid-19-research/'>many of us turn to Google</a>
                 to understand our symptoms and treatment options.
                    Using data from January 1, 2020 - ",END_DATE_TEXT,", this dashboard illustrates how search interest for specific symptoms
                    strongly matches - and often preceeds - trends in COVID-19 cases.</h4>")),
                 br(),
                 
                 # 
                 HTML("<h4>Trends in search interest in COVID-19 symptoms should not replace
                 administrative data on cases. The relation between the two is strong
                 but <a href='https://www.nature.com/news/when-google-got-flu-wrong-1.12413'>not perfect</a>.
                 Search interest can be driven by 
                 <a href='https://www.sciencedirect.com/science/article/pii/S1201971220304641'>news or other events</a>, 
                 and the usefulness of search interest data depends on geographic characteristics such as internet access.
                 However, Google data can supplement official data.
                This is particularly true in circumstances
                    when testing or data may not be widely available. Moreover, given that
                    Google trends information is updated in real time, sudden increases in 
                    search interest can warn of potential growth in COVID-19 cases.</h4>"),
                 
                 br(),
                 HTML("<h4>Google Trends can also be used to understand impacts of COVID-19 
                      and search interest around prevention measures. Consequently, in addition 
                      to showing search interest in COVID-related symptoms, the dashboard also 
                      shows search interest related to 
                      <a href='https://psycnet.apa.org/fulltext/2020-59192-001.html'>mental health keywords</a> 
                      (e.g., anxiety and loneliness), other potential consequences (e.g., <a href='https://www.chicagofed.org/publications/blogs/chicago-fed-insights/2020/closer-look-google-trends-unemployment'>unemployment</a> and debt), 
                      prevention measures (e.g., face masks) and treatment measures (e.g., teleworking 
                      and ventilators).</h4>")
                 
                 
                 
                 
                 
                 
                 
          )
        ),
        fluidRow(
          column(6, align = "center", offset = 3,
                 hr(),
                 h2("Determining when the correlation between Google search interest and COVID-19 is strongest")
          ),
          column(6, align = "left", offset = 3,
                 HTML("<h4>We compute how strongly different 
                      search terms correlate with COVID-19 cases and deaths. In addition, we determine whether 
                      search interest can help predict future cases or deaths
                      or whether search interest responds or comes after cases/deaths. To determine this, we shift COVID-19 cases/deaths
                      by up to 21 days from its actual date. We calculate the correlation between
                      the shifted COVID-19 and the search interest (this approach follows 
                      <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7176069/'>research</a>
                      <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7438693/'>from</a>
                      <a href='https://www.sciencedirect.com/science/article/pii/S1201971220302496'>others</a>
                      on COVID-19 and Google Trends). Using all these estimated correlations, we determine the 
                      following metrics:</h4>")
          )
        ),
        fluidRow(
          column(6, align = "left", offset = 3,
                 HTML("<ul>
                      <li><h4><b>Maximum Correlation</b></h4></li>
                      <li><h4><b>Lead/Lag Days:</b> The number of days COVID-19 cases/deaths was shifted to obtain the maximum correlation.
                      <b>Negative values</b> mean that search interest comes before COVID-19, helping to predict cases/deaths while
                      <b>positive values</b> indicate the search interest reacts to COVID-19 cases/deaths</h4></li>
                      </ul>"),
                 
                 br()
          )
        ),
        
        fluidRow(
          column(12, align = "center",
                 HTML("<strong>Correlation between <span style='color:orange;'>COVID-19 cases</span> (shifted) and <span style='color:green;'>Search Term Interest</span></strong>")
          )
        ),
        fluidRow(
          column(12, align = "center",
                 img(src="cor.gif", width='70%')
          )
        ),
        
        hr(),
        
        fluidRow(
          column(6, offset = 3,
                 h2("Data", align = "center"),
                 HTML("<h4>We access COVID-19 Cases and Deaths from the
                      <a href='https://covid19.who.int/?gclid=Cj0KCQjw8fr7BRDSARIsAK0Qqr73Wij8AiyjGx8dOs-MYxN7oxF5pzYmurbdVxj-x65Gc8tx1jJykaYaAqQNEALw_wcB'>WHO</a>
                      and download 
                      <a href='https://trends.google.com/trends/'>Google Trends </a>
                      data for all countries. To protect privacy, Google
                      only releases search interest data when there is a large enough search volume for a
                      specific search term. We translate search terms from English into
                      each country's most widely used language using Google Translate.
                      The below table shows which language is used for each country.</h4>"),
                 
          )
        ),
        fluidRow(
          br(),
          column(6, offset = 3, align = "center",
                 div(style = 'overflow-y: scroll; height:300px', tableOutput('language_table'))
          )
        ),
        
        fluidRow(
          column(6, offset = 3,
                 hr(),
                 h2("References", align = "center"),
                 
                 HTML("<h4>This dashboard builds off of a literature that
                      uses Google Trends to provide insight into COVID-19.
                      Articles that were used to inform the dashboard include
                      the following:</h4>"),
                 
                 
                 HTML("<br><ul>
                      <li><a href='https://www.chicagofed.org/publications/blogs/chicago-fed-insights/2020/closer-look-google-trends-unemployment'>A Closer Look at the Correlation Between Google Trends and Initial Unemployment Insurance Claims</a></li>
                      <li><a href='https://www.sciencedirect.com/science/article/pii/S1201971220302496'>Association of the COVID-19 pandemic with Internet Search Volumes: A Google Trends(TM) Analysis</a></li>
                      <li><a href='https://www.washingtonpost.com/politics/2020/10/29/can-google-searches-predict-where-coronavirus-cases-will-soon-emerge/'>Can Google searches predict where coronavirus cases will soon emerge?</a></li>
                      <li><a href='https://www.mayoclinicproceedings.org/article/S0025-6196(20)30934-4/fulltext'>Correlations Between COVID-19 Cases and Google Trends Data in the United States: A State-by-State Analysis</a></li>
                      <li><a href='https://ideas.repec.org/p/cep/cepdps/dp1693.html'>COVID-19, Lockdowns and Well-being: Evidence from Google Trends</a></li>
                      <li><a href='https://europepmc.org/article/pmc/pmc7267744'>Predicting COVID-19 Incidence Using Anosmia and Other COVID-19 Symptomatology: Preliminary Analysis Using Google and Twitter</a></li>
                      <li><a href='https://www.nytimes.com/2020/04/05/opinion/coronavirus-google-searches.html'>Google Searches Can Help Us Find Emerging Covid-19 Outbreaks</a></li>
                      <li><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7189861/'>The COVID-19 outbreak and Google searches: Is it really the time to worry about global mental health?</a></li>
                      <li><a href='https://pubmed.ncbi.nlm.nih.gov/32279437/'>Use of Google Trends to investigate loss-of-smell-related searches during the COVID-19 outbreak</a></li>
                      <li><a href='https://www.medrxiv.org/content/10.1101/2020.05.07.20093955v2'>Utility and limitations of Google searches for tracking disease: the case of taste and smell loss as markers for COVID-19</a></li>
                      </ul>"),
          )
        ),
        
        fluidRow(
          column(6, offset = 3,
                 hr(),
                 h2("Credits", align = "center"),
                 
                 HTML("<h4>The dashboard and analytics were produced by Robert Marty, Manuel Maqueda,
                 Nausheen Khan and Arndt Reichert of the 
                 
                 <a href='https://www.worldbank.org/en/research/dime'>Development Impact Evaluation (DIME)</a>
                Group at the World Bank. The research team has published a 
                <a href='https://blogs.worldbank.org/opendata/pandemic-unfolds-google-part-1-new-global-dashboard-covid-19-monitoring?cid=dec_tt_data_en_ext'>blog</a>
                about the analysis and has also conducted analysis at the subnational level using 
                <a href='https://drive.google.com/file/d/1-DrtOdFdKCv99G-w3zHK0VyDK65GqEpJ/view?usp=sharing'>Brazil as a case study.</a> 
                
                      <br><br>
                      The findings, interpretations, and conclusions expressed in this dashboard are entirely those
                of the authors. They do not necessarily represent the views of the International Bank for Reconstruction and Development/World Bank and
                its affiliated organizations, or those of the Executive Directors of the World Bank or the governments they represent.</h4>")
                 
                 
          )
        ),
        
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br()
        
      )
    )
  )
)

# SERVER =======================================================================
server = (function(input, output, session) {
  
  # GLOBAL FIGURES ***************** -------------------------------------------
  
  # ** Global Correlation Map --------------------------------------------------
  output$cor_map_leaflet <- renderUI({
    
    gtrends_spark_df <- readRDS(file.path("data", paste0("gtrends_spark_since_",
                                                         input$select_begin_date,
                                                         "_",
                                                         max(cor_end_dates),
                                                         "_large.Rds")))
    
    if( (input$select_cor_type %in% "No Lead/Lag") & (input$select_cor_raw_MA %in% "7 Day Moving Average") ){
      gtrends_spark_df$cor_casesMA7_hitsMA7_max <- gtrends_spark_df$cor_casesMA7_hitsMA7_nolag
      gtrends_spark_df$cor_deathMA7_hitsMA7_max <- gtrends_spark_df$cor_deathMA7_hitsMA7_nolag
    }
    
    if( (input$select_cor_type %in% "No Lead/Lag") & (input$select_cor_raw_MA %in% "Raw Value") ){
      gtrends_spark_df$cor_casesMA7_hitsMA7_max <- gtrends_spark_df$cor_cases_hits_nolag
      gtrends_spark_df$cor_deathMA7_hitsMA7_max <- gtrends_spark_df$cor_death_hits_nolag
      
      gtrends_spark_df$cor_casesMA7_hitsMA7_lag <- gtrends_spark_df$cor_cases_hits_lag
      gtrends_spark_df$cor_deathMA7_hitsMA7_lag <- gtrends_spark_df$cor_death_hits_lag
    }
    
    if( (input$select_cor_type %in% "Best Lead/Lag") & (input$select_cor_raw_MA %in% "Raw Value") ){
      gtrends_spark_df$cor_casesMA7_hitsMA7_max <- gtrends_spark_df$cor_cases_hits_max
      gtrends_spark_df$cor_deathMA7_hitsMA7_max <- gtrends_spark_df$cor_death_hits_max
      
      gtrends_spark_df$cor_casesMA7_hitsMA7_lag <- gtrends_spark_df$cor_cases_hits_lag
      gtrends_spark_df$cor_deathMA7_hitsMA7_lag <- gtrends_spark_df$cor_death_hits_lag
    }
    
    gtrends_spark_df$name <- NULL
    
    # Step 1 convert htmlwidget to character representation of HTML components
    
    #### Subset world
    if(input$select_continent != "All"){
      world <- world[world$continent %in% input$select_continent,]
      gtrends_spark_df <- gtrends_spark_df[gtrends_spark_df$continent %in% input$select_continent,]
      
    }
    
    #### Subset Keyword
    gtrends_spark_df <- gtrends_spark_df %>%
      dplyr::filter(keyword_en %in% input$select_keyword) 
    
    #### COVID Type
    if(input$select_covid_type %in% "Cases"){
      gtrends_spark_df$l_covid_hits <- gtrends_spark_df$l_cases_hits
      gtrends_spark_df$cor_covidMA7_hitsMA7_max <- gtrends_spark_df$cor_casesMA7_hitsMA7_max
      gtrends_spark_df$cor_covidMA7_hitsMA7_lag <- gtrends_spark_df$cor_casesMA7_hitsMA7_lag
      #gtrends_spark_df$cor_covidMA7_hitsMA7_zscore <- gtrends_spark_df$cor_casesMA7_hitsMA7_zscore %>% round(3)
    } else{
      gtrends_spark_df$l_covid_hits <- gtrends_spark_df$l_death_hits
      gtrends_spark_df$cor_covidMA7_hitsMA7_max <- gtrends_spark_df$cor_deathMA7_hitsMA7_max
      gtrends_spark_df$cor_covidMA7_hitsMA7_lag <- gtrends_spark_df$cor_deathMA7_hitsMA7_lag
      #gtrends_spark_df$cor_covidMA7_hitsMA7_zscore <- gtrends_spark_df$cor_deathMA7_hitsMA7_zscore %>% round(3)
    }
    
    #### Merge
    gtrends_spark_df <- gtrends_spark_df %>% distinct(geo, .keep_all=T) # TODO
    world_data <- merge(world, gtrends_spark_df, by = "geo", all.x=T, all.y=F)
    world_data <- world_data
    
    #### Prep Correlation
    world_data$cor <- ""
    world_data$cor[!is.na(world_data$cor_covidMA7_hitsMA7_max)] <-
      paste0("<br><b>Correlation:</b> ", world_data$cor_covidMA7_hitsMA7_max[!is.na(world_data$cor_covidMA7_hitsMA7_max)] %>%
               round(3))
    
    world_data$cor_lag <- ""
    world_data$cor_lag[!is.na(world_data$cor_covidMA7_hitsMA7_lag)] <-
      paste0("<br><b>Lead/Lag:</b> ", world_data$cor_covidMA7_hitsMA7_lag[!is.na(world_data$cor_covidMA7_hitsMA7_lag)], " days")
    
    world_data$cor_keyword <- ""
    world_data$cor_keyword[!is.na(world_data$keyword)] <-
      paste0("<b><em>", world_data$keyword[!is.na(world_data$keyword)], "</em></b>")
    
    world_data$l_covid_hits <- world_data$l_covid_hits %>% as.character()
    world_data$l_covid_hits[is.na(world_data$l_covid_hits)] <- "<em>Low Google search interest<br>for this search term</em>"
    
    world_data$popup <- paste0("<h4>", world_data$name, "</h4>", 
                               world_data$cor_keyword,
                               world_data$cor, 
                               world_data$cor_lag, 
                               "<br>",
                               world_data$l_covid_hits)
    
    # https://stackoverflow.com/questions/61175878/r-leaflet-highcharter-tooltip-label
    world_data$cor_covidMA7_hitsMA7_max[world_data$cor_covidMA7_hitsMA7_max %in% c(-Inf, Inf)] <- NA
    
    pal <- colorNumeric(
      palette = "RdYlGn",
      domain = c(world_data$cor_covidMA7_hitsMA7_max[!is.na(world_data$cor_covidMA7_hitsMA7_max)], -1, 1))
    
    pal_rev <- colorNumeric(
      palette = "RdYlGn",
      domain = c(world_data$cor_covidMA7_hitsMA7_max[!is.na(world_data$cor_covidMA7_hitsMA7_max)], -1, 1),
      reverse = T)
    
    leaflet(height = "700px") %>%
      #addTiles() %>%
      addPolygons(data = world_data,
                  label = ~lapply(popup, HTML),
                  popupOptions = popupOptions(minWidth = 200,
                                              maxHeight = 150),
                  stroke = F,
                  smoothFactor = 0,
                  fillOpacity = 1,
                  color = ~pal(cor_covidMA7_hitsMA7_max)) %>%
      onRender("function(el,x) {
      this.on('tooltipopen', function() {HTMLWidgets.staticRender();})
    }") %>%
      #   onRender("function(el,x) {
      #   this.on('popupopen', function() {HTMLWidgets.staticRender();})
      # }") %>%
      addLegend("topright",
                pal = pal_rev,
                values = c(world_data$select_covid_type[!is.na(world_data$select_covid_type)], -1, 1),
                title = paste0("Correlation<br>between<br>",
                               input$select_covid_type,
                               " and<br>Search<br>Interest"),
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                opacity = 1,
                bins = c(-1, -0.5, 0, 0.5, 1)) %>%
      setMapWidgetStyle(list(background= "white")) %>%
      setView(zoom = 2, lat=0, lng=0) %>%
      add_deps("sparkline") %>%
      #add_deps("highchart", 'highcharter') %>%
      browsable()
    #) #%>%
    #add_deps("sparkline") 
    #browsable() %>%
    #
    
  })
  
  # ** Sparkline Table ---------------------------------------------------------
  output$global_table_title <- renderText({
    paste0("<strong>The table shows trends in <span style='color:orange;'>COVID-19 ",
           tolower(input$select_covid_type),
           "</span> and <span style='color:green;'>search interest in '",
           input$select_keyword, "'</span></strong>")
  })
  
  
  output$line_graph <- renderUI({
    
    gtrends_spark_df <- readRDS(file.path("data", paste0("gtrends_spark_since_",
                                                         input$select_begin_date,
                                                         "_",
                                                         max(cor_end_dates),
                                                         "_large.Rds")))
    
    # if(input$select_cor_type %in% "No Lead/Lag"){
    #   gtrends_spark_df$cor_casesMA7_hitsMA7_max <- gtrends_spark_df$cor_casesMA7_hitsMA7_nolag
    #   gtrends_spark_df$cor_deathMA7_hitsMA7_max <- gtrends_spark_df$cor_deathMA7_hitsMA7_nolag
    # }
    
    if( (input$select_cor_type %in% "No Lead/Lag") & (input$select_cor_raw_MA %in% "7 Day Moving Average") ){
      gtrends_spark_df$cor_casesMA7_hitsMA7_max <- gtrends_spark_df$cor_casesMA7_hitsMA7_nolag
      gtrends_spark_df$cor_deathMA7_hitsMA7_max <- gtrends_spark_df$cor_deathMA7_hitsMA7_nolag
    }
    
    if( (input$select_cor_type %in% "No Lead/Lag") & (input$select_cor_raw_MA %in% "Raw Value") ){
      gtrends_spark_df$cor_casesMA7_hitsMA7_max <- gtrends_spark_df$cor_cases_hits_nolag
      gtrends_spark_df$cor_deathMA7_hitsMA7_max <- gtrends_spark_df$cor_death_hits_nolag
      
      gtrends_spark_df$cor_casesMA7_hitsMA7_lag <- gtrends_spark_df$cor_cases_hits_lag
      gtrends_spark_df$cor_deathMA7_hitsMA7_lag <- gtrends_spark_df$cor_death_hits_lag
    }
    
    if( (input$select_cor_type %in% "Best Lead/Lag") & (input$select_cor_raw_MA %in% "Raw Value") ){
      gtrends_spark_df$cor_casesMA7_hitsMA7_max <- gtrends_spark_df$cor_cases_hits_max
      gtrends_spark_df$cor_deathMA7_hitsMA7_max <- gtrends_spark_df$cor_death_hits_max
      
      gtrends_spark_df$cor_casesMA7_hitsMA7_lag <- gtrends_spark_df$cor_cases_hits_lag
      gtrends_spark_df$cor_deathMA7_hitsMA7_lag <- gtrends_spark_df$cor_death_hits_lag
    }
    
    #### Subset
    if(input$select_continent != "All"){
      gtrends_spark_df <- gtrends_spark_df %>%
        dplyr::filter(continent %in% input$select_continent) 
    }
    
    gtrends_spark_df <- gtrends_spark_df %>%
      filter(keyword_en %in% input$select_keyword)
    
    if(input$select_covid_type %in% "Cases"){
      gtrends_spark_df <- gtrends_spark_df %>%
        dplyr::select(name, l_cases_hits, 
                      cor_casesMA7_hitsMA7_max, 
                      cor_casesMA7_hitsMA7_lag,
                      cases_total) %>%
        dplyr::rename(Country = name,
                      Trends = l_cases_hits,
                      "Correlation" = cor_casesMA7_hitsMA7_max,
                      "Lead/Lag" = cor_casesMA7_hitsMA7_lag)
      
    } 
    if(input$select_covid_type %in% "Deaths"){
      
      gtrends_spark_df <- gtrends_spark_df %>%
        dplyr::select(name, l_death_hits,
                      cor_deathMA7_hitsMA7_max, 
                      cor_deathMA7_hitsMA7_lag,
                      death_total) %>%
        dplyr::rename(Country = name,
                      Trends = l_death_hits,
                      "Correlation" = cor_deathMA7_hitsMA7_max,
                      "Lead/Lag" = cor_deathMA7_hitsMA7_lag)
    } 
    
    #### Sort
    if(!is.null(input$select_sort_by)){
      if(input$select_sort_by %in% "Name"){
        gtrends_spark_df <- gtrends_spark_df %>%
          arrange(Country)
      }
      
      if(input$select_sort_by %in% "Correlation"){
        gtrends_spark_df <- gtrends_spark_df %>%
          arrange(-Correlation)
      }
      
      if(input$select_sort_by %in% "Cases"){
        gtrends_spark_df <- gtrends_spark_df %>%
          arrange(-cases_total)
      }
      
      if(input$select_sort_by %in% "Deaths"){
        gtrends_spark_df <- gtrends_spark_df %>%
          arrange(-death_total)
      }
    }
    
    #### Adjust Variables
    gtrends_spark_df$Correlation <- gtrends_spark_df$Correlation %>% round(3)
    gtrends_spark_df$`Lead/Lag` <- paste(gtrends_spark_df$`Lead/Lag`, "days")
    
    #### Remove Unneeded Variables
    gtrends_spark_df$continent <- NULL
    gtrends_spark_df$death_total <- NULL
    gtrends_spark_df$cases_total <- NULL
    gtrends_spark_df$keyword_en <- NULL
    
    #### Make Table
    #Country	Trends	Correlation	Correlation Lag
    
    f_list <- list(
      `Country` = formatter("span", style = ~ style(color = "black", font.weight = "bold", width = "2px")),
      `Lead/Lag` = formatter("span", style = ~ style(color = "black", font.weight = "bold")),
      # `Correlation` = formatter(
      #   "span",
      #   style = x ~ style(
      #     display = "inline-block",
      #     direction = "lft",
      #     font.weight = "bold",
      #     #"border-radius" = "4px",
      #     "padding-left" = "2px",
      #     "background-color" = csscolor(bar_color),
      #     width = percent(proportion(x)),
      #     color = csscolor("black")
      #   )
      # )
      
      `Correlation` = formatter("span", style = ~ style(color = "black", font.weight = "bold")) #,
    )
    
    gtrends_spark_df <- gtrends_spark_df[!is.na(gtrends_spark_df$Correlation),]
    
    table_max <- nrow(gtrends_spark_df)
    
    l <- formattable(
      gtrends_spark_df[1:table_max,] %>% as.data.table(),
      align = c("c", "c", "l"),
      f_list
    ) %>% format_table(align = c("c", "c", "l", "l")) %>%
      htmltools::HTML() %>%
      div() %>%
      # use new sparkline helper for adding dependency
      spk_add_deps() %>%
      # use column for bootstrap sizing control
      # but could also just wrap in any tag or tagList
      {column(width=12, .)}
    
    l
    
    
  })
  
  # **** Title - Map ---------------------------
  output$map_text <- renderText({
    paste0("<strong>Hover over the map to see trends in <span style='color:orange;'>COVID-19 ",
           tolower(input$select_covid_type),
           "</span> and <span style='color:green;'>search interest in '",
           input$select_keyword, "'</span></strong>")
    
  })
  
  # ** Max Correlation Dotplot -------------------------------------------------
  # **** Title ---------------------------
  output$cor_distribution_text <- renderText({
    out <- paste0("<strong>Correlation Between COVID-19 ",
                  tolower(input$select_covid_type) %>% tools::toTitleCase(),
                  " and Google Search Interest</strong><br>")
    
    out
    
  })
  
  output$cor_distribution_text_2 <- renderText({
    
    out <- ""
    
    if(input$select_cor_raw_MA %in% "7 Day Moving Average"){
      out <- paste0(out,
                    " To compute the correlation, 7 day moving averages of COVID-19 ", 
                    tolower(input$select_covid_type), 
                    " and search interest are used.")
    }
    
    if(input$select_cor_type %in% "Best Lead/Lag"){
      out <- paste0(out,
                    " The highest correlation when COVID-19 ", 
                    tolower(input$select_covid_type), 
                    " are shifted -21 to 21 days is shown.")
    }
    
    out <- paste0(out, 
                  " Data after ", 
                  input$select_begin_date, 
                  " is used.")
    out
    
  })
  
  # **** Figure ---------------------------
  output$max_cor_hist <- renderPlotly({
    
    cor_df     <- readRDS(file.path("data", paste0("correlations_since_",
                                                   input$select_begin_date,
                                                   "_",
                                                   max(cor_end_dates),
                                                   ".Rds")))
    
    #if(input$select_cor_type %in% "No Lead/Lag"){
    # cor_df$cor <- cor_df$cor_nolag
    #}
    
    if( (input$select_cor_type %in% "No Lead/Lag") & (input$select_cor_raw_MA %in% "7 Day Moving Average") ){
      cor_df$cor <- cor_df$cor_nolagMA
      cor_df$lag <- cor_df$lagMA7
    }
    
    if( (input$select_cor_type %in% "No Lead/Lag") & (input$select_cor_raw_MA %in% "Raw Value") ){
      cor_df$cor <- cor_df$cor_nolag
    }
    
    if( (input$select_cor_type %in% "Best Lead/Lag") & (input$select_cor_raw_MA %in% "7 Day Moving Average") ){
      cor_df$cor <- cor_df$corMA
      cor_df$lag <- cor_df$lagMA7
    }
    
    if( (input$select_cor_type %in% "Best Lead/Lag") & (input$select_cor_raw_MA %in% "Raw Value") ){
      cor_df$cor <- cor_df$cor
    }
    
    cor_df$cor[cor_df$cor %in% c(-Inf, Inf)] <- NA
    
    if(input$select_search_category != "All"){
      kwords <- keywords_df$keyword_en[keywords_df$category %in% input$select_search_category ]
      cor_df <- cor_df[cor_df$keyword_en %in% kwords,]
    }
    
    if(input$select_continent != "All"){
      cor_df <- cor_df %>%
        dplyr::filter(continent %in% input$select_continent) 
    }
    
    cor_df <- cor_df %>%
      dplyr::filter(type %in% input$select_covid_type) 
    
    cor_df$keyword_en[cor_df$keyword_en %in% "What are the Symptoms of Coronavirus"] <- "What are the Symptoms<br>of Coronavirus"
    cor_df$keyword_en[cor_df$keyword_en %in% "How to Treat Coronavirus"] <- "How to Treat<br>Coronavirus"
    cor_df$keyword_en[cor_df$keyword_en %in% "Unemployment Insurance"] <- "Unemployment<br>Insurance"
    cor_df$keyword_en[cor_df$keyword_en %in% "Unemployment Benefits"] <- "Unemployment<br>Benefits"
    
    
    cor_df$keyword_en <- reorder(cor_df$keyword_en,
                                 cor_df$cor)
    
    # https://plotly.com/r/hover-text-and-formatting/
    colors <- brewer.pal(n = 9, 
                         name = "RdYlGn")
    

    p1 <- cor_df %>%
      plot_ly() %>% 
      add_markers(y = ~jitter(as.numeric(keyword_en)), 
                  x = ~cor, 
                  color = ~cor,
                  marker = list(size = 6,
                                line = list(color = 'black',
                                            width = 1),
                                colorscale = list(list(0,"#D73027"), list(0.5,"#FFFFBF"), list(1,"#1A9850")),
                                cauto = F,
                                cmin = -1,
                                cmax = 1,
                                reversescale =F,
                                opacity = 1),
                  hovertemplate = ~paste('<b>',name,'</b><br>', 
                                         'Correlation: %{x:.2f}<extra></extra>'),

                  showlegend = F) %>% 
      layout(
        xaxis = list(title = "",
                     range = c((min(cor_df$cor)-.1),1.05),
                     showticklabels = T,
                     side ="top")) %>%
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(margin = list(
        l = 50,
        r = 50,
        b = 100,
        t = 10,
        pad = 4
      )) %>%
      plotly::config(displayModeBar = F) %>%
      layout(
        yaxis = list(
          title = "",
          ticktext = as.list(unique(cor_df$keyword_en)), 
          tickvals = as.list(as.numeric(unique(cor_df$keyword_en))),
          tickmode = "array"
        )) %>%
      hide_colorbar() 
    
    p1
    
    
  })
  
  output$max_cor_hist_ui <- renderUI({
    
    height <- "2300px"
    if(input$select_search_category == "Coronavirus General") height <- "600px"
    if(input$select_search_category == "Mental Health") height <- "900px"
    if(input$select_search_category == "Potential Consequences") height <- "500px"
    if(input$select_search_category == "Prevention") height <- "500px"
    if(input$select_search_category == "Symptoms") height <- "970px"
    if(input$select_search_category == "Treatment") height <- "500px"
    if(input$select_search_category == "Vaccine") height <- "700px"
    
    plotlyOutput("max_cor_hist",
                 height = height)
    
  })
  
  # ** Cor/Lag Title -----------------------------------------------------------
  output$title_cor_lag <- renderText({
    
    cor_df     <- readRDS(file.path("data", paste0("correlations_since_",
                                                   input$select_begin_date,
                                                   "_",
                                                   max(cor_end_dates),
                                                   ".Rds")))
    
    #if(input$select_cor_type %in% "No Lead/Lag"){
    #  cor_df$cor <- cor_df$cor_nolag
    #}
    
    if( (input$select_cor_type %in% "No Lead/Lag") & (input$select_cor_raw_MA %in% "7 Day Moving Average") ){
      cor_df$cor <- cor_df$cor_nolagMA
      cor_df$lag <- cor_df$lagMA7
    }
    
    if( (input$select_cor_type %in% "No Lead/Lag") & (input$select_cor_raw_MA %in% "Raw Value") ){
      cor_df$cor <- cor_df$cor_nolag
    }
    
    if( (input$select_cor_type %in% "Best Lead/Lag") & (input$select_cor_raw_MA %in% "7 Day Moving Average") ){
      cor_df$cor <- cor_df$corMA
      cor_df$lag <- cor_df$lagMA7
    }
    
    if( (input$select_cor_type %in% "Best Lead/Lag") & (input$select_cor_raw_MA %in% "Raw Value") ){
      cor_df$cor <- cor_df$cor
    }
    
    cor_df$cor[cor_df$cor %in% c(-Inf, Inf)] <- NA
    
    if(input$select_continent != "All"){
      cor_df <- cor_df %>%
        dplyr::filter(continent %in% input$select_continent) 
    }
    
    cor_df <- cor_df %>%
      dplyr::filter(type %in% input$select_covid_type,
                    keyword_en %in% input$select_keyword) 
    
    cor_df <- cor_df %>%
      filter(!is.na(cor))
    
    paste0("<strong>", input$select_keyword,"</strong><br><b><em>",nrow(cor_df), " countries with available data</em></b>")
    
  })
  
  
  # ** Cor Histogram -----------------------------------------------------------
  output$keyword_cor <- renderCachedPlot({
    
    cor_df     <- readRDS(file.path("data", paste0("correlations_since_",
                                                   input$select_begin_date,
                                                   "_",
                                                   max(cor_end_dates),
                                                   ".Rds")))
    
    #if(input$select_cor_type %in% "No Lead/Lag"){
    #  cor_df$cor <- cor_df$cor_nolag
    #}
    
    if( (input$select_cor_type %in% "No Lead/Lag") & (input$select_cor_raw_MA %in% "7 Day Moving Average") ){
      cor_df$cor <- cor_df$cor_nolagMA
      cor_df$lag <- cor_df$lagMA7
    }
    
    if( (input$select_cor_type %in% "No Lead/Lag") & (input$select_cor_raw_MA %in% "Raw Value") ){
      cor_df$cor <- cor_df$cor_nolag
    }
    
    if( (input$select_cor_type %in% "Best Lead/Lag") & (input$select_cor_raw_MA %in% "7 Day Moving Average") ){
      cor_df$cor <- cor_df$corMA
      cor_df$lag <- cor_df$lagMA7
    }
    
    if( (input$select_cor_type %in% "Best Lead/Lag") & (input$select_cor_raw_MA %in% "Raw Value") ){
      cor_df$cor <- cor_df$cor
    }
    
    cor_df$cor[cor_df$cor %in% c(-Inf, Inf)] <- NA
    
    
    if(input$select_continent != "All"){
      cor_df <- cor_df %>%
        dplyr::filter(continent %in% input$select_continent) 
    }
    
    cor_df <- cor_df %>%
      dplyr::filter(type %in% input$select_covid_type,
                    keyword_en %in% input$select_keyword) 
    
    cor_sum_df <- cor_df %>%
      ungroup() %>%
      mutate(cor = round(cor*10, 0)/10) %>%
      group_by(cor) %>%
      summarise(N = n()) %>%
      mutate(cor = cor %>% as.factor())
    
    p <- cor_neg1_pos1_df %>%
      left_join(cor_sum_df) %>%
      mutate(N = N %>% replace_na(0)) %>%
      mutate(cor = cor %>% as.character() %>% as.numeric()) %>%
      
      ggplot() +
      geom_col(aes(x = factor(cor), y = N),
               color = "black",
               fill = "palegreen3") +
      labs(title = "Correlation",
           subtitle = paste0("Average: ", round(mean(cor_df$cor, na.rm=T), 2)),
           x = NULL,
           y = "Number\nOf\nCountries") +
      scale_x_discrete(labels = c("-1", "",
                                  "-0.8", "",
                                  "-0.6", "",
                                  "-0.4", "",
                                  "-0.2", "",
                                  "0", "",
                                  "0.2", "",
                                  "0.4", "",
                                  "0.6", "",
                                  "0.8", "",
                                  "1", "")) +
      theme_minimal() +
      theme(axis.title.y = element_text(angle = 0,
                                        #family = "Helvetica",
                                        vjust = 0.5),
            plot.title = element_text(face = "bold", size=14,
                                      #family = "Helvetica",
                                      hjust = 0.5),
            plot.subtitle = element_text(face = "bold.italic", 
                                         #family = "Helvetica",
                                         size=12,
                                         hjust = 0.5),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
    p
  }, cacheKeyExpr = { list(input$select_keyword,
                           input$select_begin_date,
                           input$select_cor_type,
                           input$select_search_category,
                           input$select_continent,
                           input$select_covid_type,
                           input$select_cor_type, 
                           input$select_cor_raw_MA)})
  
  # ** Lag Histogram -----------------------------------------------------------
  output$keyword_lag <- renderCachedPlot({
    
    cor_df     <- readRDS(file.path("data", paste0("correlations_since_",
                                                   input$select_begin_date,
                                                   "_",
                                                   max(cor_end_dates),
                                                   ".Rds")))
    
    if( (input$select_cor_type %in% "No Lead/Lag") & (input$select_cor_raw_MA %in% "7 Day Moving Average") ){
      cor_df$cor <- cor_df$cor_nolagMA
      cor_df$lag <- cor_df$lagMA7
    }
    
    if( (input$select_cor_type %in% "No Lead/Lag") & (input$select_cor_raw_MA %in% "Raw Value") ){
      cor_df$cor <- cor_df$cor_nolag
    }
    
    if( (input$select_cor_type %in% "Best Lead/Lag") & (input$select_cor_raw_MA %in% "7 Day Moving Average") ){
      cor_df$cor <- cor_df$corMA
      cor_df$lag <- cor_df$lagMA7
    }
    
    if( (input$select_cor_type %in% "Best Lead/Lag") & (input$select_cor_raw_MA %in% "Raw Value") ){
      cor_df$cor <- cor_df$cor
    }
    
    cor_df$cor[cor_df$cor %in% c(-Inf, Inf)] <- NA
    
    if(input$select_continent != "All"){
      cor_df <- cor_df %>%
        dplyr::filter(continent %in% input$select_continent) 
    }
    
    cor_df <- cor_df %>%
      dplyr::filter(type %in% input$select_covid_type,
                    keyword_en %in% input$select_keyword) 
    
    cor_df %>%
      ggplot() +
      geom_histogram(aes(x = lag),
                     color = "black",
                     fill = "palegreen3",
                     bins = 15) + 
      labs(title = "Lead/Lag with Highest Correlation",
           subtitle = paste0("Average ", round(mean(cor_df$lag, na.rm=T),2), " days"), 
           x = NULL,
           y = "Number\nOf\nCountries") +
      theme_minimal() +
      theme(axis.title.y = element_text(angle = 0,
                                        vjust = 0.5),
            plot.title = element_text(face = "bold", size=14,
                                      hjust = 0.5),
            plot.subtitle = element_text(face = "bold.italic", size=12,
                                         hjust = 0.5),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  }, cacheKeyExpr = { list(input$select_keyword,
                           input$select_begin_date,
                           input$select_cor_type,
                           input$select_search_category,
                           input$select_continent,
                           input$select_covid_type,
                           input$select_cor_type,
                           input$select_cor_raw_MA)})
  
  
  # COUNTRY FIGURES **************** -------------------------------------------
  
  # ** Sparkline Table ---------------------------------------------------------
  #observe({
  output$line_graph_country <- renderUI({
    
    #### Subset
    gtrends_spark_df <- readRDS(file.path("data", 
                                          paste0("gtrends_spark_since_",
                                                 input$select_begin_date_country,
                                                 "_", max(cor_end_dates),
                                                 "_small.Rds"))) %>%
      filter(name %in% input$select_country)
    
    # if(input$select_cor_type_country %in% "No Lead/Lag"){
    #   gtrends_spark_df$cor_casesMA7_hitsMA7_max <- gtrends_spark_df$cor_casesMA7_hitsMA7_nolag
    #   gtrends_spark_df$cor_deathMA7_hitsMA7_max <- gtrends_spark_df$cor_deathMA7_hitsMA7_nolag
    # }
    
    if( (input$select_cor_type_country %in% "No Lead/Lag") & (input$select_cor_raw_MA_country %in% "7 Day Moving Average") ){
      gtrends_spark_df$cor_casesMA7_hitsMA7_max <- gtrends_spark_df$cor_casesMA7_hitsMA7_nolag
      gtrends_spark_df$cor_deathMA7_hitsMA7_max <- gtrends_spark_df$cor_deathMA7_hitsMA7_nolag
    }
    
    if( (input$select_cor_type_country %in% "No Lead/Lag") & (input$select_cor_raw_MA_country %in% "Raw Value") ){
      gtrends_spark_df$cor_casesMA7_hitsMA7_max <- gtrends_spark_df$cor_cases_hits_nolag
      gtrends_spark_df$cor_deathMA7_hitsMA7_max <- gtrends_spark_df$cor_death_hits_nolag
      
      gtrends_spark_df$cor_casesMA7_hitsMA7_lag <- gtrends_spark_df$cor_cases_hits_lag
      gtrends_spark_df$cor_deathMA7_hitsMA7_lag <- gtrends_spark_df$cor_death_hits_lag
    }
    
    if( (input$select_cor_type_country %in% "Best Lead/Lag") & (input$select_cor_raw_MA_country %in% "Raw Value") ){
      gtrends_spark_df$cor_casesMA7_hitsMA7_max <- gtrends_spark_df$cor_cases_hits_max
      gtrends_spark_df$cor_deathMA7_hitsMA7_max <- gtrends_spark_df$cor_death_hits_max
      
      gtrends_spark_df$cor_casesMA7_hitsMA7_lag <- gtrends_spark_df$cor_cases_hits_lag
      gtrends_spark_df$cor_deathMA7_hitsMA7_lag <- gtrends_spark_df$cor_death_hits_lag
    }
    
    if(input$select_search_category_country != "All"){
      kwords <- keywords_df$keyword_en[keywords_df$category %in% input$select_search_category_country]
      gtrends_spark_df <- gtrends_spark_df[gtrends_spark_df$keyword_en %in% kwords,]
    }
    
    #### COVID Names
    if(input$select_covid_type_map %in% "Cases"){
      gtrends_spark_df <- gtrends_spark_df %>%
        dplyr::select(l_cases_hits, keyword_en,
                      cor_casesMA7_hitsMA7_max, 
                      cor_casesMA7_hitsMA7_lag,
                      cases_total) %>%
        dplyr::rename("Search Term" = keyword_en,
                      Trends = l_cases_hits,
                      "Correlation" = cor_casesMA7_hitsMA7_max,
                      "Lead/Lag" = cor_casesMA7_hitsMA7_lag)
      
      
    } 
    if(input$select_covid_type_map %in% "Deaths"){
      
      gtrends_spark_df <- gtrends_spark_df %>%
        dplyr::select(l_death_hits, keyword_en,
                      cor_deathMA7_hitsMA7_max, 
                      cor_deathMA7_hitsMA7_lag,
                      death_total) %>%
        dplyr::rename("Search Term" = keyword_en,
                      Trends = l_death_hits,
                      "Correlation" = cor_deathMA7_hitsMA7_max,
                      "Lead/Lag" = cor_deathMA7_hitsMA7_lag)
    } 
    
    # Without Lead/Lag ------------------------------------------------------------
    if(input$select_cor_type_country %in% "No Lead/Lag"){
      
      gtrends_spark_df <- gtrends_spark_df %>%
        dplyr::select("Search Term", "Trends", "Correlation")
      
      #### Sort
      gtrends_spark_df <- gtrends_spark_df %>%
        arrange(-Correlation)
      
      #### Adjust Variables
      gtrends_spark_df <- gtrends_spark_df %>%
        mutate(Correlation = Correlation %>% round(3))
      
      #### Make Table
      bar_color <- "#FF9999"
      
      f_list <- list(
        `Search Term` = formatter("span", style = ~ style(color = "black", font.weight = "bold", width = "2px")),
        `Correlation` = formatter(
          "span",
          style = x ~ style(
            display = "inline-block",
            direction = "lft",
            font.weight = "bold",
            #"border-radius" = "4px",
            "padding-left" = "2px",
            "background-color" = csscolor(bar_color),
            width = percent(proportion(x)),
            color = csscolor("black")
          )
        )
      )
      
      gtrends_spark_df <- gtrends_spark_df[!is.na(gtrends_spark_df$Correlation),]
      
      table_max <- nrow(gtrends_spark_df)
      
      l <- formattable(
        gtrends_spark_df[1:table_max,] %>% as.data.table(),
        align = c("c", "c"),
        f_list
      ) %>% format_table(align = c("c", "c", "l")) %>%
        htmltools::HTML() %>%
        div() %>%
        # use new sparkline helper for adding dependency
        spk_add_deps() %>%
        # use column for bootstrap sizing control
        # but could also just wrap in any tag or tagList
        {column(width=12, .)}
      
    } else{
      
      gtrends_spark_df <- gtrends_spark_df %>%
        dplyr::select("Search Term", "Trends", "Correlation", "Lead/Lag")
      
      #### Sort
      gtrends_spark_df <- gtrends_spark_df %>%
        arrange(-Correlation)
      
      #### Adjust Variables
      gtrends_spark_df <- gtrends_spark_df %>%
        mutate(Correlation = Correlation %>% round(3),
               `Lead/Lag` = paste(`Lead/Lag`, "days"))
      
      #### Make Table
      bar_color <- "#FF9999"
      
      f_list <- list(
        `Search Term` = formatter("span", style = ~ style(color = "black", font.weight = "bold", width = "2px")),
        `Lead/Lag` = formatter("span", style = ~ style(color = "black", font.weight = "bold")),
        `Correlation` = formatter(
          "span",
          style = x ~ style(
            display = "inline-block",
            direction = "lft",
            font.weight = "bold",
            #"border-radius" = "4px",
            "padding-left" = "2px",
            "background-color" = csscolor(bar_color),
            width = percent(proportion(x)),
            color = csscolor("black")
          )
        )
        
      )
      
      gtrends_spark_df <- gtrends_spark_df[!is.na(gtrends_spark_df$Correlation),]
      
      table_max <- nrow(gtrends_spark_df)
      
      l <- formattable(
        gtrends_spark_df[1:table_max,] %>% as.data.table(),
        align = c("c", "c", "l"),
        f_list
      ) %>% format_table(align = c("c", "c", "l", "l")) %>%
        htmltools::HTML() %>%
        div() %>%
        # use new sparkline helper for adding dependency
        spk_add_deps() %>%
        # use column for bootstrap sizing control
        # but could also just wrap in any tag or tagList
        {column(width=12, .)}
      
    }
    
    l
    
  })
  
  # ** Historic Trends ---------------------------------------------------------
  # **** Title ------------------------------
  output$line_graph_country_key_title_1 <- renderText({
    paste0("<h4>COVID-19 ", input$select_covid_type_map, " and 
           Search Interest in ", input$select_keyword_country, "</h4>")
  })
  
  output$line_graph_country_key_title_2 <- renderText({
    
    cor_df <- readRDS(file.path("data", paste0("correlations_since_",
                                               input$select_begin_date_country,
                                               "_", max(cor_end_dates),
                                               ".Rds")))
    
    #if(input$select_cor_type_country %in% "No Lead/Lag"){
    #  cor_df$cor <- cor_df$cor_nolag
    #}
    
    if( (input$select_cor_type_country %in% "No Lead/Lag") & (input$select_cor_raw_MA_country %in% "7 Day Moving Average") ){
      cor_df$cor <- cor_df$cor_nolagMA
      cor_df$lag <- cor_df$lagMA7
    }
    
    if( (input$select_cor_type_country %in% "No Lead/Lag") & (input$select_cor_raw_MA_country %in% "Raw Value") ){
      cor_df$cor <- cor_df$cor_nolag
    }
    
    if( (input$select_cor_type_country %in% "Best Lead/Lag") & (input$select_cor_raw_MA_country %in% "7 Day Moving Average") ){
      cor_df$cor <- cor_df$corMA
      cor_df$lag <- cor_df$lagMA7
    }
    
    if( (input$select_cor_type_country %in% "Best Lead/Lag") & (input$select_cor_raw_MA_country %in% "Raw Value") ){
      cor_df$cor <- cor_df$cor
    }
    
    cor_df$cor[cor_df$cor %in% c(-Inf, Inf)] <- NA
    
    cor <- cor_df %>%
      filter(name %in% input$select_country) %>%
      filter(type %in% input$select_covid_type_map) %>%
      filter(keyword_en %in% input$select_keyword_country) 
    
    if(input$select_cor_type_country %in% "No Lead/Lag"){
      out <- paste0("<h5><b>Trends in COVID-19 and Search Interest</b><br><br><em>Using data after ",
                    input$select_begin_date_country, 
                    ", the correlation between COVID-19 ",
                    input$select_covid_type_map %>% tolower(), 
                    " and search interest in \"",
                    input$select_keyword_country, 
                    "\" is ",
                    cor$cor %>% round(2),
                    ".</em></h5>")
    } else{
      
      out <- paste0("<h5><b>Trends in COVID-19 and Search Interest</b><br><br><em>Using data after ",
                    input$select_begin_date_country, 
                    ", <span style='color:black;'>the correlation between COVID-19 ",
                    input$select_covid_type_map %>% tolower(), 
                    " and search interest in \"",
                    input$select_keyword_country, 
                    "\"</span> is ",
                    cor$cor %>% round(2),
                    " when shifting COVID-19 ",
                    input$select_covid_type_map %>% tolower(),
                    " ",
                    abs(cor$lag),
                    " days into the ",
                    ifelse(cor$lag <= 0, "past", "future"),
                    ".</em></h5>")
    }
    
    
    out    
    
  })
  
  # **** Figure ------------------------------
  output$line_graph_country_key <- renderPlotly({
    
    fig <- plot_ly()
    
    if(!is.null(input$select_keyword_country)){
      if(input$select_keyword_country != ""){
        if(!is.null(input$select_country)){
          if(!is.null(input$select_covid_type_map)){
            
            gtrends_sub_df <- gtrends_df %>%
              filter(keyword_en %in% input$select_keyword_country) %>%
              filter(name %in% input$select_country)
            
            #### COVID Names
            if(input$select_covid_type_map %in% "Cases"){
              gtrends_sub_df <- gtrends_sub_df %>%
                dplyr::select(keyword_en, date, hits_ma7, cases_new) %>%
                dplyr::rename(covid_new = cases_new)
            } 
            if(input$select_covid_type_map %in% "Deaths"){
              gtrends_sub_df <- gtrends_sub_df %>%
                dplyr::select(keyword_en, date, hits_ma7, death_new) %>%
                dplyr::rename(covid_new = death_new)
            } 
            
            multiplier <- max(gtrends_sub_df$covid_new, na.rm=T) / max(gtrends_sub_df$hits_ma7, na.rm=T)
            gtrends_sub_df$hits_ma7_adj <- gtrends_sub_df$hits_ma7 * multiplier
            
            
            gtrends_sub_df <- gtrends_sub_df %>%
              arrange(date)
            
            fig <- plot_ly(gtrends_sub_df)
            fig <- fig %>% add_trace(x = ~date, y = ~covid_new, type = 'bar', name = input$select_covid_type_map,
                                     marker = list(color = 'orange'),
                                     hoverinfo = "text",
                                     text = ~paste(covid_new %>% prettyNum(big.mark=",",scientific=FALSE), 
                                                   input$select_covid_type_map, 
                                                   "<br>", date))
            fig <- fig %>% add_trace(x = ~date, y = ~hits_ma7, type = 'scatter', mode = 'lines', name = 'Search Interest', yaxis = 'y2',
                                     line = list(color = 'forestgreen'),
                                     hoverinfo = "text",
                                     text = ~paste("Search Interest:", round(hits_ma7, 2),"<br>", date))
            fig <- fig %>% layout(title = '',
                                  xaxis = list(title = ""),
                                  margin = list(l=45, r=45, b=5, t=10, pad=0),
                                  showlegend = F,
                                  #legend = list(orientation = 'h'),
                                  paper_bgcolor = 'transparent',
                                  plot_bgcolor = 'transparent',
                                  yaxis = list(side = 'left', title = input$select_covid_type_map, showgrid = FALSE, zeroline = FALSE, color = "orange"),
                                  yaxis2 = list(side = 'right', 
                                                overlaying = "y", 
                                                rangemode = "tozero",
                                                title = 'Search Interest', 
                                                showgrid = FALSE, 
                                                zeroline = F,
                                                color = "forestgreen")) 
            fig <- fig %>% plotly::config(displayModeBar = F)
            
            
            
          }
        }
      }
    }
    
    fig
    
  }) #  ,bg = "transparent"
  
  
  # ** GTrends HTML ------------------------------------------------------------
  # **** Translation Text ---------------------------
  output$translation_text <- renderText({
    
    geo <- world$geo[world$name %in% input$select_country] %>% as.character()
    
    search_en <- input$select_keyword_country
    language_code <- languges_df$language_best[languges_df$geo %in% geo][1]
    
    if(length(search_en) %in% 0) search_en <- "Loss of Smell"
    if(length(language_code) %in% 0) language_code <- "en"
    
    search <- keywords_df[[paste0("keyword_", language_code)]][tolower(keywords_df$keyword_en) %in% tolower(search_en)]
    search <- search %>% str_replace_all("'", "")
    search_p20 <- search %>% str_replace_all(" ", "%20")
    
    if(language_code %in% "en"){
      out <- ""
    } else{
      out <- paste0("<h4>", "'", search_en[1], "' translated into ",
                    languges_df$Language[languges_df$geo %in% geo][1],": ",
                    search[1],
                    "</h4>")
      
      
    }
    
    out 
    
    
  })
  
  # **** Trends ---------------------------
  output$gtrends_html_trends <- renderUI({
    
    # Do this as doesn't appear on default.
    if(!is.null(input$select_country)){
      if((input$select_country %in% "") & LOAD_GTRENDS_INIT){
        updateSelectInput(session, "select_country",
                          selected = "United States")
        LOAD_GTRENDS_INIT <<- F
      }
    }
    
    ## Country
    geo <- world$geo[world$name %in% input$select_country] %>% as.character()
    
    ## Search term
    search_en <- input$select_keyword_country
    language_code <- languges_df$language_best[languges_df$geo %in% geo][1]
    
    if(length(search_en) %in% 0) search_en <- "Loss of Smell"
    if(length(language_code) %in% 0) language_code <- "en"
    
    search <- keywords_df[[paste0("keyword_", language_code)]][tolower(keywords_df$keyword_en) %in% tolower(search_en)]
    search <- search %>% str_replace_all("'", "")
    search_p20 <- search %>% str_replace_all(" ", "%20")
    
    tags$body(HTML(paste0('<script type="text/javascript" src="https://ssl.gstatic.com/trends_nrtr/2213_RC01/embed_loader.js"></script> <script type="text/javascript"> var divElem = document.getElementById("wrapper"); document.getElementById("wrapper").innerHTML = ""; trends.embed.renderExploreWidgetTo(divElem, "TIMESERIES", {"comparisonItem":[{"keyword":"',search,'","geo":"',geo,'","time":"today 3-m"}],"category":0,"property":""}, {"exploreQuery":"q=',search_p20,'&geo=',geo,'&date=today 3-m","guestPath":"https://trends.google.com:443/trends/embed/"}); </script>')))
  })
  
  # **** Map ---------------------------
  output$gtrends_html_map <- renderUI({
    
    # Do this as doesn't appear on default.
    if(!is.null(input$select_country)){
      if((input$select_country %in% "") & LOAD_GTRENDS_INIT){
        updateSelectInput(session, "select_country",
                          selected = "United States")
        LOAD_GTRENDS_INIT <<- F
      }
    }
    
    ## Country
    geo <- world$geo[world$name %in% input$select_country] %>% as.character()
    
    ## Search term
    search_en <- input$select_keyword_country
    language_code <- languges_df$language_best[languges_df$geo %in% geo][1]
    
    if(length(search_en) %in% 0) search_en <- "Loss of Smell"
    if(length(language_code) %in% 0) language_code <- "en"
    
    search <- keywords_df[[paste0("keyword_", language_code)]][tolower(keywords_df$keyword_en) %in% tolower(search_en)]
    search <- search %>% str_replace_all("'", "")
    search_p20 <- search %>% str_replace_all(" ", "%20")
    
    tags$body(HTML(paste0('<script type="text/javascript" src="https://ssl.gstatic.com/trends_nrtr/2213_RC01/embed_loader.js"></script> <script type="text/javascript"> var divElem = document.getElementById("wrapper2"); document.getElementById("wrapper2").innerHTML = ""; trends.embed.renderExploreWidgetTo(divElem, "GEO_MAP", {"comparisonItem":[{"keyword":"',search,'","geo":"',geo,'","time":"today 3-m"}],"category":0,"property":""}, {"exploreQuery":"q=',search_p20,'&geo=',geo,'&date=today 3-m","guestPath":"https://trends.google.com:443/trends/embed/"}); </script>')))
  })
  
  output$country_name <- renderText({
    input$select_country
  })
  
  # INFORMATION ********************* ------------------------------------------
  
  # ** Language Table ----------------------------------------------------------
  output$language_table <- renderTable({
    
    languges_df %>%
      dplyr::select(Country, Language) 
    
  })
  
  
  # UIS ******************************** ---------------------------------------
  # ** Select Keyword: Global --------------------
  output$select_keyword_ui <- renderUI({
    
    
    out <- selectInput(
      "select_keyword",
      label = strong("Search Term"),
      choices = sort(keyword_list),
      selected = "Loss of Smell",
      multiple = F
    )
    
    if(input$select_search_category != "All"){
      
      keyword_df_i <- keywords_df[keywords_df$category %in% input$select_search_category,]
      
      # If 'Loss of Smell' is a keyword, select that initially
      if("Loss of Smell" %in% keyword_df_i$keyword_en){
        keyword_selected <- "Loss of Smell"
      } else{
        keyword_selected <- sort(keyword_df_i$keyword_en)[1]
      }
      
      out <- selectInput(
        "select_keyword",
        label = strong("Search Term"),
        choices = sort(keyword_df_i$keyword_en),
        selected = keyword_selected,
        multiple = F
      )
      
      
      
    }
    
    out
    
  })
  
  # ** Select Keyword: Country --------------------
  observe({
    
    output$select_keyword_country_ui <- renderUI({
      
      ## Default
      out <- selectInput(
        "select_keyword_country",
        label = strong("Search Term"),
        choices = sort(keyword_list),
        selected = "Loss of Smell",
        multiple = F
      )
      
      ## Restrict to keywords in country
      keywords_in_country <- gtrends_df %>%
        filter(name %in% input$select_country) %>%
        arrange(desc(cor_casesMA7_hitsMA7_max)) %>%
        pull(keyword_en) %>%
        unique()
      
      if(input$select_search_category_country != "All"){
        keywords_vec <- keywords_df$keyword_en[keywords_df$category %in% input$select_search_category_country]
        keywords_in_country <- keywords_in_country[keywords_in_country %in% keywords_vec]
      }
      
      out <- selectInput(
        "select_keyword_country",
        label = strong("Search Term"),
        choices = sort(keywords_in_country),
        selected = keywords_in_country[1],
        multiple = F
      )
      
      out
      
    })
    
  })
  
  # ** Country List - Restrict based on category --------------------
  observe({
    
    output$select_country_ui <- renderUI({
      
      # !!!!!!!!!!!!!!!!!!!!!!!!!! EDIT HERE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      selected <- "United States"
      
      if(!is.null(input$select_country)){
        if(input$select_country != ""){
          selected <- input$select_country
        }
      }
      
      ## Default
      out <- selectInput(
        "select_country",
        label = strong("Search Country"),
        choices = sort(unique(cor_df$name)),
        selected = "United States",
        multiple = F
      )
      
      cor_country_df <- cor_df
      
      if(input$select_search_category_country != "All"){
        keywords_vec <- keywords_df$keyword_en[keywords_df$category %in% input$select_search_category_country]
        
        cor_country_df <- cor_df[cor_df$keyword_en %in% keywords_vec,]
        
        # If the currently selected country is not in the new list of countries,
        # change selected to United States (default)
        if(!(selected %in% sort(unique(cor_country_df$name)))){
          selected <- "United States"
        }
      }
      
      
      out <- selectInput(
        "select_country",
        label = strong("Search Country"),
        choices = sort(unique(cor_country_df$name)),
        selected = selected,
        multiple = F
      )
      
      out
      
    })
    
  })
  
  
  # * * * * * * * * * * * P U R G A T O R Y * * * * * *  ------------------------
  
  
  
  # ** Global Map ---------------------------------------------------------------
  
  #### Basemap
  output$global_map <- renderLeaflet({
    
    # https://epsg.io/54030
    epsg2163 <- leafletCRS(
      crsClass = "L.Proj.CRS",
      code = "EPSG:4087",
      proj4def = "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ",
      resolutions = 2^(25:15))
    
    # options = leafletOptions(crs = epsg2163)
    
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      setView(lat = 0, lng = 0, zoom = 1)
    
  })
  
  observe({
    
    req(input$nav == "Country Level") # This makes leaflet show up; before no defaults.
    
    world$name <- NULL
    
    cor_sum_df <- cor_df %>%
      filter(type %in% "Cases") %>% # input$select_covid_type_map
      group_by(geo, name) %>%
      summarise(keyword_en = keyword_en[which.max(cor)])
    
    cor_sum_df$keyword_en[cor_sum_df$keyword_en %in% "Coronavirus Symptoms"] <- "Coronavirus<br>Symptoms"
    cor_sum_df$keyword_en[cor_sum_df$keyword_en %in% "Corona Symptoms"] <- "Corona<br>Symptoms"
    
    world_data <- merge(world, cor_sum_df, by = "geo", all.x=T, all.y=F)
    
    world_data$contain_historic_data <- ifelse(!is.na(world_data$keyword_en),
                                               "Historic Data Available",
                                               NA)
    
    colors <- brewer.pal(n = length(unique(world_data$contain_historic_data)), 
                         name = "Spectral")
    
    pal <- colorFactor(colors, 
                       world_data$contain_historic_data[!is.na(world_data$contain_historic_data)])
    
    leafletProxy("global_map",
                 data = world_data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(label = ~name,
                  layerId = ~ name,
                  stroke = F,
                  fillOpacity = 1,
                  color = ~pal(contain_historic_data)) %>%
      addLegend("bottomright", 
                pal = pal, 
                values = world_data$contain_historic_data[!is.na(world_data$contain_historic_data)],
                title = "",
                opacity = 1
      )
    
  })
  
  # * Country Figures ----------------------------------------------------------
  
  # *** Country Name Reactive -----
  country_name_react <- reactive({
    
    if(is.null(input$global_map_shape_click$id)){
      country_name <- "United States"
    } else{
      country_name <- input$global_map_shape_click$id
    }
    
    country_name
    
  })
  
  output$ui_select_covid_cases <- renderUI({
    
    numericInput(
      "select_covid_cases",
      label = strong(paste("Restrict to Countries with X", input$select_covid_type)),
      value = 100,
      min = 0
    )
    
  })
  
  output$country_name <- renderText({
    
    input$select_country
    
  })
  
  output$country_page_description <- renderText({
    
    paste0("Considering all countries, search terms including
                    `Loss of Smell`, `I Can't Smell` and `Loss of Taste` 
                    correlate most strongly with COVID-19 ",
           tolower(input$select_covid_type),
           ". However, other search terms strongly correlate 
                    with COVID trends in select countries. This page shows
                    how well each search term correlates with COVID-19 ",
           tolower(input$select_covid_type),
           " in individual countries.")
    
  })
  
  
  
  output$cor_title_text <- renderText({
    
    paste0("Correlation between search interest in ",
           input$select_keyword, 
           " and COVID-19 ",
           input$select_covid_type)
    
  })
  
  output$trends_title <- renderText({
    
    paste0("Trends in ",
           input$select_keyword,
           " and COVID-19 ",
           input$select_covid_type)
    
  })
  
  output$trends_country_subtitle <- renderText({
    
    out <- paste0("The table compares trends in <span style='color:orange;'>COVID-19 ",
                  tolower(input$select_covid_type), 
                  "</span> and the <span style='color:green;'>search term interest",
                  "</span>.")
    
    if(input$select_cor_raw_MA_country %in% "7 Day Moving Average"){
      out <- paste0(out,
                    " To compute the correlation, 7 day moving averages of COVID-19 ", 
                    tolower(input$select_covid_type), 
                    " and search interest are used.")
    }
    
    if(input$select_cor_type_country %in% "Best Lead/Lag"){
      out <- paste0(out,
                    " The highest correlation when COVID-19 ", 
                    tolower(input$select_covid_type), 
                    " are shifted -21 to 21 days is shown.")
    }
    
    out <- paste0(out, 
                  " Data after ", 
                  input$select_begin_date_country, 
                  " are used to compute the correlation.")    
    out
    
    
  })
  
  output$trends_subtitle <- renderText({
    
    paste0("We compare trends in <span style='color:orange;'>COVID-19 ",
           tolower(input$select_covid_type), 
           "</span> and <span style='color:green;'>search interest in ",
           input$select_keyword, 
           ".</span> We show the correlation between the two and the number of
           days in the past when the search interest is most strongly 
           correlated with COVID-19 ", tolower(input$select_covid_type), 
           ".")
    
    
  })
  
  output$which_keyword_title <- renderText({
    
    paste0("Which search terms are most correlated with COVID-19 ",
           input$select_covid_type, "?")
    
  })
  
  output$text_best_time_lag <- renderText({
    
    if(input$select_continent != "All"){
      cor_df <- cor_df %>%
        dplyr::filter(continent %in% input$select_continent) 
    }
    
    df <- cor_df %>%
      
      dplyr::filter(type %in% input$select_covid_type) %>%
      filter(keyword_en %in% input$select_keyword) 
    
    txt <- paste0("Across countries, the search term interest in ",
                  input$select_keyword,
                  " is most strongly correlated with COVID ",
                  input$select_covid_type, " ",
                  abs(round(mean(df$lag), 0)), " days ",
                  ifelse(round(mean(df$lag), 0) < 0, "into the past", "into the future"),
                  ".")
    
    txt
    
  })
  
  output$text_cor_countries <- renderText({
    
    if(input$select_continent != "All"){
      cor_df <- cor_df %>%
        dplyr::filter(continent %in% input$select_continent) 
    }
    
    df <- cor_df %>%
      
      dplyr::filter(type %in% input$select_covid_type) %>%
      filter(keyword_en %in% input$select_keyword) 
    
    txt <- paste0("Across countries, the average correlation between the search interest in ",
                  input$select_keyword,
                  " and COVID ",
                  input$select_covid_type, 
                  " is ",
                  round(mean(df$cor), 2),
                  ". ",
                  df$name[which.max(df$cor)],
                  " has the strongest correlation (",
                  max(df$cor) %>% round(2),
                  ").")
    
    txt
    
  })
  
  
  output$ui_line_graph <- renderUI({
    
    n_states <- readRDS(file.path("precomputed_figures", 
                                  paste0("stat_line_cor_N_countries",
                                         "_keyword", input$select_keyword,
                                         "_cases_deaths", input$select_covid_type,
                                         "_continent", input$select_continent,
                                         ".Rds")))
    
    n_states_div <- ceiling(n_states/5)
    
    plotOutput("line_graph",
               height = paste0(n_states_div*180,"px"))
    
  })
  
  output$ui_select_sort_by <- renderUI({
    
    selectInput(
      "select_sort_by",
      label = strong("Sort By"),
      choices = c("Name", input$select_covid_type, "Correlation"),
      selected = "Correlation",
      multiple = F
    )
    
  })
  
})

# RUN THE APP ==================================================================
# app <- shinyApp(ui, server)
# 
# profvis({
#   runApp(app, display.mode="normal")
# })

shinyApp(ui, server)



