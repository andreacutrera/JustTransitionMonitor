################################################################################
########             Master Degree Easy Visualization tool          ############
########                                                            ############
########                   Just Transition Monitor                  ############
########                                                            ############
########                  Andrea Pio Cutrera: 965591                ############
################################################################################

#   package "pacman" to load (and install) all the requirements for the correct 
#   execution of the app  


library(shiny) 
library(shinythemes)
library(highcharter)
library(dplyr)
library(stringr)

##   add data
pca <- read.csv('pca.csv')
pca_country <- pca%>% 
  group_by(country)%>%
  summarise(environment = mean(environment),
            society = mean(society),
            economy = mean(economy),
            group = "Means")

environment <- read.csv('environment.csv')
society <- read.csv('society.csv')
economy <- read.csv('economy.csv')

environment['country'] <- substr(environment$Region.NUTS.id, 1, 2)
society['country'] <- substr(society$Region.NUTS.id, 1, 2)
economy['country'] <- substr(economy$Region.NUTS.id, 1, 2)

normalize <- function(column) {
  column <- column / max(column)
}

for (i in 3:(length(colnames(environment))-1)) {
  environment[,i] <- normalize(environment[,i])
}
for (i in 3:(length(colnames(society))-1)) {
  society[,i] <- normalize(society[,i])
}
for (i in 3:(length(colnames(economy))-1)) {
  economy[,i] <- normalize(economy[,i])
}

#   UI - user interface definition
ui <- navbarPage(
  paste('Just Transition Monitor'),
  theme = shinytheme("flatly"),
  
  tabPanel(
    "Unidimensional Analysis",
    sidebarLayout(
      sidebarPanel(
        selectInput("variable", label = h5("Selected Variable"), choices = list("Environment" = "environment", "Society" = "society", "Economy" = "economy"), selected = "environment"),
        selectInput("granularity", label = h5("Select granularity level"), choices = list("Country", "Region"), selected="Region"),
        helpText(h6("Regions in EU (290) and Countries (26)")),
        br(),
        radioButtons("method", label = h5("Select aggregation method"), choices = list("PCA" = "pca", "Weighted-Sum" = "ws"), selected = "pca"),
        
        conditionalPanel(
          condition = "input.method == 'ws' && input.variable == 'environment'",
          selectInput("p_env", h6("Compensation parameter"), choices = list("p = 1" = 1, "p = 3" = 2), selected = 1),
          textOutput("weights_environment"),
          br(),
          br(),
          sliderInput("w1", h6("Renewables for cooling and heating in buildings"), min=0, max=0.5, step=0.05, value=0.15
          ),
          sliderInput("w2", h6("Public transport vehicles"), min=0, max=0.5, step=0.05, value=0.15
          ),
          sliderInput("w3", h6("Municipal waste"), min=0, max=0.5, step=0.05, value=0.15
          ),
          sliderInput("w4", h6("Energy consumption per capita"), min=0, max=0.5, step=0.05, value=0.1
          ),
          sliderInput("w5", h6("Ozone"), min=0, max=0.5, step=0.05, value=0.25
          ),
          sliderInput("w6", h6("Marine sites area protection"), min=0, max=0.5, step=0.05, value=0.05
          ),
          sliderInput("w7", h6("Forest area protection"), min=0, max=0.5, step=0.05, value=0.05
          ),
          sliderInput("w8", h6("Nature area protection"), min=0, max=0.5, step=0.05, value=0.05
          ),
          sliderInput("w9", h6("Land coverage by artificial surfaces"), min=0, max=0.5, step=0.05, value=0.05
          )
        ),
        
        conditionalPanel(
          condition = "input.method == 'ws' && input.variable == 'society'",
          selectInput("p_soc", h6("Compensation parameter"), choices = list("p = 1" = 1, "p = 3" = 2), selected = 1),
          textOutput("weights_society"),
          br(),
          br(),
          sliderInput("w10", h6("People at risk of poverty or social exclusion"), min=0, max=0.5, step=0.05, value=0.35
          ),
          sliderInput("w11", h6("Overcrowded living conditions"), min=0, max=0.5, step=0.05, value=0.25
          ),
          sliderInput("w12", h6("Life expectancy at birth"), min=0, max=0.5, step=0.05, value=0.1
          ),
          sliderInput("w13", h6("Early leavers from education and training"), min=0, max=0.5, step=0.05, value=0.2
          ),
          sliderInput("w14", h6("Female students in tertiary education"), min=0, max=0.5, step=0.05, value=0.05
          ),
          sliderInput("w15", h6("EU quality of government"), min=0, max=0.5, step=0.05, value=0.05
          )
        ),
        conditionalPanel(
          condition = "input.method == 'ws' && input.variable == 'economy'",
          selectInput("p_eco", h6("Compensation parameter"), choices = list("p = 1" = 1, "p = 3" = 2), selected = 1),
          textOutput("weights_economy"),
          br(),
          br(),
          sliderInput("w16", h6("Intramural R&D expenditure"), min=0, max=0.5, step=0.05, value=0.15
          ),
          sliderInput("w17", h6("Long term unemployment"), min=0, max=0.5, step=0.05, value=0.40
          ),
          sliderInput("w18", h6("Disposable income of private households"), min=0, max=0.5, step=0.05, value=0.45
          )
        ),
        conditionalPanel(
          condition = "input.method == 'pca'"
        ),
      ),
      mainPanel(
        conditionalPanel(
          condition = "input.method == 'pca'",
          titlePanel(h4('Plot of the distributions between and within countries')),
          highchartOutput('boxplot_pca'),
          br(),
          helpText("Note: use the cursor to interact with the graph")
        ),
        conditionalPanel(
          condition = "input.method == 'ws'",
          titlePanel(h4('Plot of the distributions between and within countries')),
          highchartOutput('boxplot_composite'),
          br(),
          helpText("Note: use the cursor to interact with the graph")
        )
      )
    )
  ),
  
  tabPanel(
    "Multidimensional Analysis",
    sidebarLayout(
      sidebarPanel(
        selectInput("granularity_multi", label = h5("Select granularity level"), choices = list("Country", "Region"), selected="Region"),
        helpText(h6("Regions in EU (290) and Countries (26)")),
        br(),
        radioButtons("method_multi", label = h5("Select aggregation method"), choices = list("PCA" = "pca", "Weighted-Sum" = "ws"), selected = "pca"),
        conditionalPanel(
          condition = "input.method_multi == 'ws' && input.variable_multi == 'environment'",
          selectInput("p_env_multi", h6("Compensation parameter"), choices = list("p = 1" = 1, "p = 3" = 2), selected = 1),
          textOutput("weights_environment_multi"),
          br(),
          br(),
          sliderInput("w1_multi", h6("Renewables for cooling and heating in buildings"), min=0, max=0.5, step=0.05, value=0.15
          ),
          sliderInput("w2_multi", h6("Public transport vehicles"), min=0, max=0.5, step=0.05, value=0.15
          ),
          sliderInput("w3_multi", h6("Municipal waste"), min=0, max=0.5, step=0.05, value=0.15
          ),
          sliderInput("w4_multi", h6("Energy consumption per capita"), min=0, max=0.5, step=0.05, value=0.1
          ),
          sliderInput("w5_multi", h6("Ozone"), min=0, max=0.5, step=0.05, value=0.25
          ),
          sliderInput("w6_multi", h6("Marine sites area protection"), min=0, max=0.5, step=0.05, value=0.05
          ),
          sliderInput("w7_multi", h6("Forest area protection"), min=0, max=0.5, step=0.05, value=0.05
          ),
          sliderInput("w8_multi", h6("Nature area protection"), min=0, max=0.5, step=0.05, value=0.05
          ),
          sliderInput("w9_multi", h6("Land coverage by artificial surfaces"), min=0, max=0.5, step=0.05, value=0.05
          )
        ),
        
        conditionalPanel(
          condition = "input.method_multi == 'ws' && input.variable_multi == 'society'",
          selectInput("p_soc_multi", h6("Compensation parameter"), choices = list("p = 1" = 1, "p = 3" = 2), selected = 1),
          textOutput("weights_society_multi"),
          br(),
          br(),
          sliderInput("w10_multi", h6("People at risk of poverty or social exclusion"), min=0, max=0.5, step=0.05, value=0.35
          ),
          sliderInput("w11_multi", h6("Overcrowded living conditions"), min=0, max=0.5, step=0.05, value=0.25
          ),
          sliderInput("w12_multi", h6("Life expectancy at birth"), min=0, max=0.5, step=0.05, value=0.1
          ),
          sliderInput("w13_multi", h6("Early leavers from education and training"), min=0, max=0.5, step=0.05, value=0.2
          ),
          sliderInput("w14_multi", h6("Female students in tertiary education"), min=0, max=0.5, step=0.05, value=0.05
          ),
          sliderInput("w15_multi", h6("EU quality of government"), min=0, max=0.5, step=0.05, value=0.05
          )
        ),
        conditionalPanel(
          condition = "input.method_multi == 'ws' && input.variable_multi == 'economy'",
          selectInput("p_eco_multi", h6("Compensation parameter"), choices = list("p = 1" = 1, "p = 3" = 2), selected = 1),
          textOutput("weights_economy_multi"),
          br(),
          br(),
          sliderInput("w16_multi", h6("Intramural R&D expenditure"), min=0, max=0.5, step=0.05, value=0.15
          ),
          sliderInput("w17_multi", h6("Long term unemployment"), min=0, max=0.5, step=0.05, value=0.40
          ),
          sliderInput("w18_multi", h6("Disposable income of private households"), min=0, max=0.5, step=0.05, value=0.45
          )
        ),
        conditionalPanel(
          condition = "input.method_multi == 'pca'"
        ),
      ),
      mainPanel(
        titlePanel(h4('Projection of the observations into multi-dimensional space')),
        highchartOutput('scatterplot'),
        br(),
        helpText("Note: use the cursor to interact with the graph")
      )
    )
  ),
  
  tabPanel(
    "Time Series Analysis",
    sidebarPanel(
      titlePanel(h4('Coming next...'))
    ),
    mainPanel(
      br(),
      helpText("Note: when data would be available a time lapse with evolution trends is going to be posted here")
    )
  )
)
server <- function(input, output) {
  
  # weights computation
  w_environment <- reactive({
    return(input$w1 + input$w2 + input$w3 + input$w4 + input$w5 + input$w6 + input$w7 + input$w8 + input$w9)
  })
  w_society <- reactive({
    return(input$w10 + input$w11 + input$w12 + input$w13 + input$w14 + input$w15)
  })
  w_economy <- reactive({
    return(input$w16 + input$w17 + input$w18)
  })
  
  w_environment_multi <- reactive({
    return(input$w1_multi + input$w2_multi + input$w3_multi + input$w4_multi + input$w5_multi + input$w6_multi + input$w7_multi + input$w8_multi + input$w9_multi)
  })
  w_society_multi <- reactive({
    return(input$w10_multi + input$w11_multi + input$w12_multi + input$w13_multi + input$w14_multi + input$w15_multi)
  })
  w_economy_multi <- reactive({
    return(input$w16_multi + input$w17_multi + input$w18_multi)
  })
  
  output$weights_environment <- renderText({ 
    paste("Sum of the selected weights: ", w_environment())
  })
  output$weights_society <- renderText({ 
    paste("Sum of the selected weights: ", w_society())
  })
  output$weights_economy <- renderText({ 
    paste("Sum of the selected weights: ", w_economy())
  })
  
  output$weights_environment_multi <- renderText({ 
    paste("Sum of the selected weights: ", w_environment_multi())
  })
  output$weights_society_multi <- renderText({ 
    paste("Sum of the selected weights: ", w_society_multi())
  })
  output$weights_economy_multi <- renderText({ 
    paste("Sum of the selected weights: ", w_economy_multi())
  })
  
  # data preparation
  data_boxplot_pca <- reactive(
  if (input$granularity == 'Region') {
    data_to_boxplot(pca,
                    eval(as.symbol(input$variable)),
                    country,
                    group_var     = country,
                    add_outliers  = FALSE, 
                    pointWidth=14)
  }
  else {
    data_to_boxplot(pca_country,
                    eval(as.symbol(input$variable)),
                    group_var = group,
                    add_outliers  = FALSE, 
                    pointWidth=14)
  })
  
  weights <- reactive(
    c(input$w1, input$w2, input$w3, input$w4, input$w5, input$w6, input$w7, input$w8, input$w9, input$w10, input$w11, input$w12, input$w13, input$w14, input$w15, input$w16, input$w17, input$w18)
  )
  
  weights_multi <- reactive(
    c(input$w1_multi, input$w2_multi, input$w3_multi, input$w4_multi, input$w5_multi, input$w6_multi, input$w7_multi, input$w8_multi, input$w9_multi, input$w10_multi, input$w11_multi, input$w12_multi, input$w13_multi, input$w14_multi, input$w15_multi, input$w16_multi, input$w17_multi, input$w18_multi)
  )
  
  compensation_parameter <- reactive(
    c(input$p_env, input$p_soc, input$p_eco)
  )
  
  compensation_parameter_multi <- reactive(
    c(input$p_env_multi, input$p_soc_multi, input$p_eco_multi)
  )
  
  composite_indicators <- reactive(
    if (input$method == 'ws') {
    merge(
      merge(
        # environmental data
        data.frame("Region.NUTS.id" = environment$Region.NUTS.id, 
                   "Region.name" = environment$Region.name, 
                   "country" = environment$country,
                   "composite_environment" = (weights()[1]*environment$X7_renewables_in_heating_cooling_buildings + 
                     weights()[2]*environment$X9_public_transport_vehicles - 
                     weights()[3]*environment$X12_municipal_waste - 
                     weights()[4]*environment$X12_energy_consumption_per_capita - 
                     weights()[5]*environment$X13_ozone + 
                     weights()[6]*environment$X14_marine_sites_protection + 
                     weights()[7]*environment$X15_forest_area_protection + 
                     weights()[8]*environment$X15_nature_area_protection - 
                     weights()[9]*environment$X15_land_covered_by_artificial_surfaces)^as.numeric(compensation_parameter()[1])),
        # societal data
        data.frame("Region.NUTS.id" = society$Region.NUTS.id, 
                   "Region.name" = society$Region.name, 
                   "country" = society$country,
                   "composite_society" = (-weights()[10]*society$X1_poverty_social_exclusion_risk - 
                     weights()[11]*society$X1_overcrowded_living + 
                     weights()[12]*society$X3_life_expectancy - 
                     weights()[13]*society$X4_early_leavers_edu_train + 
                     weights()[14]*society$X5_female_enrolment_tertiary_edu + 
                     weights()[15]*society$X16_eu_quality_of_govnt)^as.numeric(compensation_parameter()[2])),
        by=c("Region.NUTS.id", "Region.name", "country")),
      # economical data
      data.frame("Region.NUTS.id" = economy$Region.NUTS.id, 
                 "Region.name" = economy$Region.name, 
                 "country" = economy$country,
                 "composite_economy" =  (weights()[16]*economy$X9_intramural_r.d_expenditure -
                   weights()[17]*economy$X8_long_term_unemployment + 
                   weights()[18]*economy$X10_disposable_income_private_households)^as.numeric(compensation_parameter()[3])),
      
      by=c("Region.NUTS.id", "Region.name", "country"))
  })
  
  composite_indicators_multi <- reactive(
    if (input$method_multi == 'ws') {
      merge(
        merge(
          # environmental data
          data.frame("Region.NUTS.id" = environment$Region.NUTS.id, 
                     "Region.name" = environment$Region.name, 
                     "country" = environment$country,
                     "environment" = (weights_multi()[1]*environment$X7_renewables_in_heating_cooling_buildings + 
                                                  weights_multi()[2]*environment$X9_public_transport_vehicles - 
                                                  weights_multi()[3]*environment$X12_municipal_waste - 
                                                  weights_multi()[4]*environment$X12_energy_consumption_per_capita - 
                                                  weights_multi()[5]*environment$X13_ozone + 
                                                  weights_multi()[6]*environment$X14_marine_sites_protection + 
                                                  weights_multi()[7]*environment$X15_forest_area_protection + 
                                                  weights_multi()[8]*environment$X15_nature_area_protection - 
                                                  weights_multi()[9]*environment$X15_land_covered_by_artificial_surfaces)^as.numeric(compensation_parameter_multi()[1])),
          # societal data
          data.frame("Region.NUTS.id" = society$Region.NUTS.id, 
                     "Region.name" = society$Region.name, 
                     "country" = society$country,
                     "society" = (-weights_multi()[10]*society$X1_poverty_social_exclusion_risk - 
                                              weights_multi()[11]*society$X1_overcrowded_living + 
                                              weights_multi()[12]*society$X3_life_expectancy - 
                                              weights_multi()[13]*society$X4_early_leavers_edu_train + 
                                              weights_multi()[14]*society$X5_female_enrolment_tertiary_edu + 
                                              weights_multi()[15]*society$X16_eu_quality_of_govnt)^as.numeric(compensation_parameter_multi()[2])),
          by=c("Region.NUTS.id", "Region.name", "country")),
        # economical data
        data.frame("Region.NUTS.id" = economy$Region.NUTS.id, 
                   "Region.name" = economy$Region.name, 
                   "country" = economy$country,
                   "economy" =  (weights_multi()[16]*economy$X9_intramural_r.d_expenditure -
                                             weights_multi()[17]*economy$X8_long_term_unemployment + 
                                             weights_multi()[18]*economy$X10_disposable_income_private_households)^as.numeric(compensation_parameter_multi()[3])),
        
        by=c("Region.NUTS.id", "Region.name", "country"))
    })
  
  composite_indicators_country_multi <- reactive(
  if (input$method_multi == 'ws' & input$granularity_multi == 'Country') {
    composite_indicators_multi()%>% 
      group_by(country)%>%
      summarise(environment = mean(environment),
                society = mean(society),
                economy = mean(economy),
                group = "Means")
  }
  else if (input$method_multi == 'ws' & input$granularity_multi == 'Region') {
    composite_indicators_multi()
  })
  
  
  data_boxplot_composite <- reactive(
    if (input$granularity == 'Region') {
      data_to_boxplot(composite_indicators(),
                      eval(as.symbol(paste0("composite_",input$variable))),
                      country,
                      group_var     = country,
                      add_outliers  = FALSE, 
                      pointWidth=14)
    }
    else {
      data_to_boxplot(composite_indicators_country(),
                      eval(as.symbol(paste0("composite_",input$variable))),
                      group_var = group,
                      add_outliers  = FALSE, 
                      pointWidth=14)
    })
  
  data_scatter <- reactive(
    if (input$method_multi == "pca" & input$granularity_multi == "Region") {
      pca
    }
    else if (input$method_multi == "pca" & input$granularity_multi == "Country") {
      pca_country
    }
    else if (input$method_multi == "ws" & input$granularity_multi == "Region") {
      eval(composite_indicators_multi())
    }
    else if (input$method_multi == "ws" & input$granularity_multi == "Country") {
      eval(composite_indicators_country_multi())
    }
  )
  
  # plots
  output$boxplot_pca <- renderHighchart2(
  highchart()%>%
    hc_xAxis(type ="category")%>%
    hc_add_series_list(data_boxplot_pca())%>%
    hc_xAxis(title = list(text = input$granularity))%>%
    hc_yAxis(title = list(text = input$variable))%>%
    hc_title(text = str_to_title(input$variable)) %>% 
    hc_subtitle(text= "Composite Indicator") %>% 
    hc_caption(text= "")%>% 
    hc_legend(enabled= FALSE)%>%
    hc_add_theme(hc_theme_elementary())
  )
  
  output$boxplot_composite <- renderHighchart2(
    highchart()%>%
      hc_xAxis(type="category")%>%
      hc_add_series_list(data_boxplot_composite())%>%
      hc_xAxis(title = list(text = input$granularity))%>%
      hc_yAxis(title = list(text = input$variable))%>%
      hc_title(text = str_to_title(input$variable)) %>% 
      hc_subtitle(text= "Composite Indicator") %>% 
      hc_caption(text= "")%>% 
      hc_legend(enabled= FALSE)%>%
      hc_add_theme(hc_theme_elementary())
  )
  
  output$scatterplot <- renderHighchart2(
    data_scatter() %>% 
      hchart('scatter', hcaes(x = environment, 
                              y = society, 
                              size = economy,
                              group = country),
             maxSize = "5%")%>%
      hc_exporting (enabled = TRUE) %>%
      hc_legend(enabled= FALSE)%>%
      hc_add_theme(hc_theme_flat())
  )
  
}

#shiny APP
shinyApp(ui = ui, server = server)



