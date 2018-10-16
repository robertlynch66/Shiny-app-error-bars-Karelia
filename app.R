# To do add 
#  aafb  - age at first birth and
#  fcyob - first child year of birth  to app using format of birth cohorts (byear)

library(Hmisc)
library("tidyverse")
library(ggplot2)
library(shiny)
library(dplyr)
# load the data
# make a change
# get data
source("getdata.R")
# filter only those born in karelia
#s <- s  %>% filter (birthregion=="karelia")
#setwd("C:/Users/Robert/Dropbox/My shiny apps/Error_bars")


colors_1<- c("blue","darkgreen","black","darkred","darkgoldenrod4","purple","orange","yellow","lightblue","grey")
x_axis_choices <- c("Sex" = "sex", 
                    "Population of birthplace (logged)"="birthpopulation",
                    "Population of Return Destination Karelia (logged)"="rdk_population",
                    "Population of First Destination Finland (logged)"="fdf_population",
                    "Agriculture" = "agriculture",
                    "Social class "="social_class",
                    "Manual labor"="man_labor",
                    "Education"="education",
                    "Served in Lotta Svard"="lotta",  
                    "Served during the war"="servedduringwar",   
                    "Injured during the war"="injuredinwar",
                    "Farm total hectares (logged)"="farmtotalarea",
                    "Statistics Finland occupational categories"="statistics_finland",
                    "Census 1950 occupational categories"="census_1950",
                    "Wedding Year" = "wedyear",
                    "Birth cohort"="byear",
                    "Age at first birth"="aafb",
                    "First childs YOB"= "fcyob")
y_axis_choices <- c("Probability of returning to Karelia (95% CI)"="returnedkarelia",
                    "Pre war moves (95% CI)"="movesbefore1940",
                    "Post war moves (95% CI)" = "movesafter1945",
                    "Probability of marrying a non Karelian (95% CI)"="outbred",
                    "Peacetime moves (95% CI)"="peacetime_migrations",
                    "Number of children (95% CI)"="kids",
                    "Population of birthplace (95% CI)"="birthpopulation",
                    "Age at first birth"="aafb_n")

legend_choices <- c("Sex" = "sex", 
                    "Agriculture" = "agriculture",
                    "Social class"="social_class",
                    "Manual labor"="man_labor",
                    "Education"="education",
                    "Served in Lotta svard"="lotta", 
                    "Served during the war"="servedduringwar",   
                    "Injured during the war"="injuredinwar",
                    "Statistics Finland occupational categories"="statistics_finland",
                    "Census 1950 occupational categories"="census_1950",
                    "Wedding Year" = "wedyear",
                    "Birth cohort"="byear",
                    "Age at first birth"="aafb",
                    "First childs YOB"= "fcyob")
#"Returned to Karelia between the wars"="returnedkarelia_factor")

ui<-fluidPage(
  
  title = "The effect of occupation on migrations",
  
  plotOutput('plot'),
  
  
  hr(),
  
  fluidRow(
    column(3,
           h4("The forced migration from Karelia during World war II"),
           #sliderInput('sampleSize', 'Sample', 
           #min=1, max=nrow(dataset),
           #value= min((nrow(dataset)), nrow(dataset)), 
           #step=500, round=0),
           br(),
           checkboxInput('jitter', 'Show data', value=FALSE), #,
           checkboxInput('refugee', 'Evacuees only', value=TRUE),
           checkboxInput('primary', 'Primary only', value=TRUE)
    ),
    
    
    ### add a button for primary only
    column(4, offset = 1,
           selectInput('x', label='X-axis', choices=list("Sex"="sex","Returned to Karelia between the wars"="returnedkarelia_factor",
                                                         "Agriculture"="agriculture","Manual labor"="man_labor",
                                                         "Educated"="education","Married In"="outbreed","Statistics Finland occupational categories"="statistics_finland","Population of birthplace"="birthpopulation",
                                                         "Population of First Destination Finland"="fdf_population","Population of Return Destination Karelia"="rdk_population","Social class"="social_class",
                                                         "Census 1950 occupation categories"='census_1950',"Served in Lotta svard"="lotta", 
                                                         "Served during the war"="servedduringwar",   
                                                         "Injured during the war"="injuredinwar",
                                                         "Farm total hectares (logged)"="farmtotalarea","Wedding Year" = "wedyear","Birth cohort"="byear",
                                                         "Age at first birth"="aafb",
                                                         "First childs YOB"= "fcyob"),selected="birthpopulation"),      
           
           selectInput('y', label='Y-axis', choices=list("Probability of returning to Karelia"="returnedkarelia","Married a non Karelian"="outbred","Peactime movements"="peacetime_migrations",
                                                         "Pre war moves"="movesbefore1940","Post war moves"="movesafter1945","Number of kids"="kids","Age at first birth"="aafb_n"),selected="returnedkarelia")#,
           
           
           
           #selectInput('color', 'Color', choices=list("Sex"="sex",
           #                                           "Agriculture"="agriculture","Manual labor"="man_labor",
           #                                          "Educated"="education","Statistics Finland occupational categories"="statistics_Finland",
           #                                        "Social class (1=highest, 7 = lowest)"="social_class","Census 1950 occupation categories"='census_1950'),selected="social_class")
    ),
    
    
    column(4,
           selectInput('facet_row', 'Rows',
                       c(None='.',"Sex"="sex","Birth cohort"="byear","Age at first birth"="aafb",
                         "First childs YOB"= "fcyob","Wedding Year" = "wedyear","Agriculture"="agriculture","Manual labor"=
                           "man_labor","Returned to Karelia between the wars"="returnedkarelia_factor", "Educated"="education","Married In"="outbreed","Statistics Finland occupational categories"="statistics_finland",
                         "Social class"="social_class","Census 1950 occupation categories"="census_1950","Served in Lotta svard"="lotta", 
                         "Served during the war"="servedduringwar",   
                         "Injured during the war"="injuredinwar"
                          )),
           selectInput('facet_col', 'Columns',c(None='.', "Sex"="sex","Birth cohort"="byear","Age at first birth"="aafb",
                                                "First childs YOB"= "fcyob","Wedding Year" = "wedyear","Agriculture"="agriculture","Manual labor"=
                                                  "man_labor", "Educated"="education","Returned to Karelia between the wars"="returnedkarelia_factor","Married In"="outbreed","Statistics Finland occupational categories"="statistics_finland",
                                                "Social class"="social_class","Census 1950 occupation categories"="census_1950",
                                                "Served in Lotta svard"="lotta",
                                                "Served during the war"="servedduringwar",   
                                                "Injured during the war"="injuredinwar")))
  )
)






server<- function(input, output) {
  # if else loop subsetting dataframe to get rid on NA's for the variables being plotted
  
  
  
  
  #dataset <- reactive({
  
  
  #  s[sample(nrow(s), input$sampleSize),]
  #})
  
  
  
  
  
  output$plot <- renderPlot({
    if (input$y == "peactime_migrations" ) {s<- s[!is.na(s$peactime_migrations), ]}
    else if (input$y == "movesbefore1940" ) {s<- s[!is.na(s$movesbefore1940), ]}
    else if (input$y == "movesafter1945" ) {s<- s[!is.na(s$movesafter1945), ]}
    else if (input$y == "aafb_n") {s<- s[!is.na(s$aafb_n), ]}
    else s <- s
    
    if (input$refugee)
    {s <- s  %>% filter (birthregion=="karelia")}
    else s<- s
    
    if (input$primary)
    {s <- s  %>% filter (primaryperson=="TRUE")}
    else s<- s
    
    if (input$x == "social_class" ) {s<- s[!is.na(s$social_class), ]}
    else if (input$x == "sex" ) {s<- s[!is.na(s$sex), ]}
    else if (input$x == "byear" ) {s<- s[!is.na(s$byear), ]}
    else if (input$x == "aafb" ) {s<- s[!is.na(s$aafb), ]}
    else if (input$x == "fcyob" ) {s<- s[!is.na(s$fcyob), ]}
    else if (input$x == "wedyear" ) {s<- s[!is.na(s$wedyear), ]}
    else if (input$x == "agriculture" ) {s<- s[!is.na(s$agriculture), ]}
    else if (input$x == "outbreed") {s<- s[!is.na(s$outbreed), ]}
    else if (input$x == "man_labor" ) {s<- s[!is.na(s$man_labor), ]}
    else if (input$x == "education") {s<- s[!is.na(s$education), ]}
    else if (input$x == "statistics_finland") {s<- s[!is.na(s$statistics_finland), ]}
    else if (input$x == "census_1950") {s<- s[!is.na(s$census_1950), ]}
    else if (input$x == "returnedkarelia_factor"){s<- s[!is.na(s$returnedkarelia_factor),]}
    else if (input$x == "birthpopulation"){s <- s[!is.na(s$birthpopulation),]}
    else if (input$x == "fdf_population"){s <- s[!is.na(s$fdf_population),]}
    else if (input$x == "rdk_population"){s <- s[!is.na(s$rdk_population)& s$returnedkarelia_factor=="Returned",]}
    else if (input$x == "farmtotalarea"){s <- s[!is.na(s$farmtotalarea)& s$agriculture=="Agricultural",]}
    else if (input$x == "lotta"){s <- s[!is.na(s$lotta)& s$sex=="Female",]}
    else if (input$x == "servedduringwar"){s <- s[!is.na(s$servedduringwar),]}
    else if (input$x == "injuredinwar"){s <- s[!is.na(s$injuredinwar),]}
    else s<- s
    
    if (input$facet_row == "social_class" ) {s<- s[!is.na(s$social_class), ]}
    else if (input$facet_row == "sex" ) {s<- s[!is.na(s$sex), ]}
    else if (input$facet_row == "byear" ) {s<- s[!is.na(s$byear), ]}
    else if (input$facet_row == "fcyob" ) {s<- s[!is.na(s$fcyob), ]}
    else if (input$facet_row == "aafb" ) {s<- s[!is.na(s$aafb), ]}
    else if (input$facet_row == "wedyear" ) {s<- s[!is.na(s$wedyear), ]}
    else if (input$facet_row == "agriculture" ) {s<- s[!is.na(s$agriculture), ]}
    else if (input$facet_row == "outbreed") {s<- s[!is.na(s$outbreed), ]}
    else if (input$facet_row == "man_labor" ) {s<- s[!is.na(s$man_labor), ]}
    else if (input$facet_row == "education") {s<- s[!is.na(s$education), ]}
    else if (input$facet_row == "statistics_finland") {s<- s[!is.na(s$statistics_finland), ]}
    else if (input$facet_row == "census_1950") {s<- s[!is.na(s$census_1950), ]}
    else if (input$facet_row == "returnedkarelia_factor"){s<- s[!is.na(s$returnedkarelia_factor),]}
    else if (input$facet_row == "lotta"){s <- s[!is.na(s$lotta)& s$sex=="Female",]}
    else if (input$facet_row == "servedduringwar"){s <- s[!is.na(s$servedduringwar),]}
    else if (input$facet_row == "injuredinwar"){s <- s[!is.na(s$injuredinwar),]}
    else s<- s
    
    if (input$facet_col == "social_class" ) {s<- s[!is.na(s$social_class), ]}
    else if (input$facet_col == "sex" ) {s<- s[!is.na(s$sex), ]}
    else if (input$facet_col == "wedyear" ) {s<- s[!is.na(s$wedyear), ]}
    else if (input$facet_col == "byear" ) {s<- s[!is.na(s$byear), ]}
    else if (input$facet_col == "fcyob" ) {s<- s[!is.na(s$fcyob), ]}
    else if (input$facet_col == "aafb" ) {s<- s[!is.na(s$aafb), ]}
    else if (input$facet_col == "agriculture" ) {s<- s[!is.na(s$agriculture), ]}
    else if (input$facet_col == "outbreed") {s<- s[!is.na(s$outbreed), ]}
    else if (input$facet_col == "man_labor" ) {s<- s[!is.na(s$man_labor), ]}
    else if (input$facet_col == "education") {s<- s[!is.na(s$education), ]}
    else if (input$facet_col == "statistics_finland") {s<- s[!is.na(s$statistics_finland), ]}
    else if (input$facet_col == "census_1950") {s<- s[!is.na(s$census_1950), ]}
    else if (input$facet_col == "lotta"){s <- s[!is.na(s$lotta)& s$sex=="Female",]}
    else if (input$facet_col == "servedduringwar"){s <- s[!is.na(s$servedduringwar),]}
    else if (input$facet_col == "injuredinwar"){s <- s[!is.na(s$injuredinwar),]}
    else if (input$facet_col == "returnedkarelia_factor"){s<- s[!is.na(s$returnedkarelia_factor),]}
    else s<- s
    
    if (input$x == "birthpopulation" ) {p<- ggplot (data=s, aes_string (x=input$x, y=input$y)) + 
      geom_smooth(method = "lm", se = TRUE, color="green")+
      scale_x_continuous(name="Population (logged)", limits=NA)+
      guides(color=guide_legend(paste("\n",names(legend_choices[legend_choices == input$x]))),nrow=3) +
      #guides(color = guide_legend(nrow = 2))
      theme(legend.title= element_text(color="darkblue",face="bold",size=16))+
      theme(axis.text.x = element_text(face="bold", color="darkgreen",size=13),
            axis.title.x=element_text(face="bold", color="black", size=16),
            axis.title.y=element_text(face="bold", color="black", size=16),
            axis.text.y = element_text(face="bold", color="black",size=14),
            # this changes the facet labels font, size and color etc...
            strip.text = element_text(face="bold", size=9),
            legend.key = element_rect(size = 3),
            legend.key.size = unit(2.0, 'lines'),
            strip.background = element_rect(fill="lightblue", colour="black",
                                            size=1))+
      
      
      scale_y_continuous(paste("\n",names(y_axis_choices[y_axis_choices== input$y])))+
      
      guides(colour = guide_legend(override.aes = list(alpha = 1)))
    }
    else if (input$x == "fdf_population" ) {p<- ggplot (data=s, aes_string (x=input$x, y=input$y)) + 
      geom_smooth(method = "lm", se = TRUE, color="green")+
      scale_x_continuous(name="Population (logged)", limits=NA)+
      guides(color=guide_legend(paste("\n",names(legend_choices[legend_choices == input$x]))),nrow=3) +
      #guides(color = guide_legend(nrow = 2))
      theme(legend.title= element_text(color="darkblue",face="bold",size=16))+
      theme(axis.text.x = element_text(face="bold", color="darkgreen",size=13),
            axis.title.x=element_text(face="bold", color="black", size=16),
            axis.title.y=element_text(face="bold", color="black", size=16),
            axis.text.y = element_text(face="bold", color="black",size=14),
            # this changes the facet labels font, size and color etc...
            strip.text = element_text(face="bold", size=9),
            legend.key = element_rect(size = 3),
            legend.key.size = unit(2.0, 'lines'),
            strip.background = element_rect(fill="lightblue", colour="black",
                                            size=1))+
      
      
      scale_y_continuous(paste("\n",names(y_axis_choices[y_axis_choices== input$y])))+
      
      guides(colour = guide_legend(override.aes = list(alpha = 1)))
    }
    
    else if (input$x == "rdk_population" ) {p<- ggplot (data=s, aes_string (x=input$x, y=input$y)) + 
      geom_smooth(method = "lm", se = TRUE, color="green")+
      scale_x_continuous(name="Population (logged)", limits=NA)+
      guides(color=guide_legend(paste("\n",names(legend_choices[legend_choices == input$x]))),nrow=3) +
      #guides(color = guide_legend(nrow = 2))
      theme(legend.title= element_text(color="darkblue",face="bold",size=16))+
      theme(axis.text.x = element_text(face="bold", color="darkgreen",size=13),
            axis.title.x=element_text(face="bold", color="black", size=16),
            axis.title.y=element_text(face="bold", color="black", size=16),
            axis.text.y = element_text(face="bold", color="black",size=14),
            # this changes the facet labels font, size and color etc...
            strip.text = element_text(face="bold", size=9),
            legend.key = element_rect(size = 3),
            legend.key.size = unit(2.0, 'lines'),
            strip.background = element_rect(fill="lightblue", colour="black",
                                            size=1))+
      
      
      scale_y_continuous(paste("\n",names(y_axis_choices[y_axis_choices== input$y])))+
      
      guides(colour = guide_legend(override.aes = list(alpha = 1)))
    }
    else if (input$x == "farmtotalarea" ) {p<- ggplot (data=s, aes_string (x=input$x, y=input$y)) + 
      geom_smooth(method = "lm", se = TRUE, color="green")+
      scale_x_continuous(name="Population (logged)", limits=NA)+
      guides(color=guide_legend(paste("\n",names(legend_choices[legend_choices == input$x]))),nrow=3) +
      #guides(color = guide_legend(nrow = 2))
      theme(legend.title= element_text(color="darkblue",face="bold",size=16))+
      theme(axis.text.x = element_text(face="bold", color="darkgreen",size=13),
            axis.title.x=element_text(face="bold", color="black", size=16),
            axis.title.y=element_text(face="bold", color="black", size=16),
            axis.text.y = element_text(face="bold", color="black",size=14),
            # this changes the facet labels font, size and color etc...
            strip.text = element_text(face="bold", size=9),
            legend.key = element_rect(size = 3),
            legend.key.size = unit(2.0, 'lines'),
            strip.background = element_rect(fill="lightblue", colour="black",
                                            size=1))+
      
      
      scale_y_continuous(paste("\n",names(y_axis_choices[y_axis_choices== input$y])))+
      
      guides(colour = guide_legend(override.aes = list(alpha = 1)))
    }
    else p <- ggplot(data=s, aes_string(x=input$x, y=input$y, col=input$x )) + 
        
        
        
        
        
        
        
        stat_summary(fun.y = mean, geom = "point", shape=19, size=5, col="black")+
        
        
        
        stat_summary(fun.data = mean_se, geom = "errorbar", width=0.7, size=1.3)+
        #stat_summary(fun.data = mean_cl_normal, geom = "errorbar", fun.args = list(mult = 1))+
        
        #scale_x_discrete(paste("\n",names(x_axis_choices[x_axis_choices == input$x])))+
        #theme(legend.position="none")+
        
        
        guides(color=guide_legend(paste("\n",names(legend_choices[legend_choices == input$x]))),nrow=3) +
        #guides(color = guide_legend(nrow = 2))
        theme(legend.title= element_text(color="darkblue",face="bold",size=16))+
        theme(axis.text.x = element_text(face="bold", color="darkgreen",size=13),
              axis.title.x=element_text(face="bold", color="black", size=16),
              axis.title.y=element_text(face="bold", color="black", size=16),
              axis.text.y = element_text(face="bold", color="black",size=14),
              # this changes the facet labels font, size and color etc...
              strip.text = element_text(face="bold", size=9),
              legend.key = element_rect(size = 3),
              legend.key.size = unit(2.0, 'lines'),
              strip.background = element_rect(fill="lightblue", colour="black",
                                              size=1))+
        
        
        scale_y_continuous(paste("\n",names(y_axis_choices[y_axis_choices== input$y])))+
        
        guides(colour = guide_legend(override.aes = list(alpha = 1)))
    
    #to change x axis tick mark labels using nested if else
    p<- p +  
      if (input$x == "social_class") {
        scale_x_discrete("Social Class", labels = c("1"="Wealthy","2"="Rich",
                                                    "3"="Upper Middle","4"="Middle","5"="Lower Middle", "6"="Lower",
                                                    "7"="Poor"))} 
    #else if (input$x == "sex") {
    # scale_x_discrete("Sex", labels=c("0"="Females", "1"="Males"))
    #} 
    else if (input$x == "agriculture") {
      scale_x_discrete("Agricultural",labels=c("0"="Non-agricultural workers", "1"="Agricultural workers"))
    }
    else if (input$x == "man_labor") {
      scale_x_discrete("Manual labor", 
                       labels=c("0"="Non-manual labor", "1"="Heavy manual labor"))
    }
    else if (input$x == "birthpopulation") {
      scale_x_continuous("Population of Birthplace (logged)",limits=c(6,11))
    }
    else if (input$x == "fdf_population") {
      scale_x_continuous("Population of First Destination in Finland (logged)",limits=c(6,11))
    }
    else if (input$x == "rdk_population") {
      scale_x_continuous("Population of Return Destination in Karelia (logged)",limits=c(6,11))
    }
    else if (input$x == "farmtotalarea") {
      scale_x_continuous("Total hectares of land (logged)",limits=c(0,7))
    }
    else if (input$x == "byear") {
      scale_x_discrete("Birth Cohort", 
                       labels=c("0"="Before 1900","1"="1900-1905","2"="1906-1910",
                                "3"="1911-1915","4"="1916-1920","5"="1921-1925",
                                "6"="1926-1930","7"="1931-1935","8"="1936-1940",
                                "9"="After 1940"))
    }
    
    else if (input$x == "fcyob") {
      scale_x_discrete("First child year year of birth", 
                       labels=c("0"="Before 1920","1"="1920-1925","2"="1926-1930",
                                         "3"="1931-1935","4"="1936-1940","5"="1941-1945",
                                         "6"="1946-1950","7"="1951-1955","8"="1956-1960",
                                         "9"="After 1960"))
    }
    
    else if (input$x == "aafb") {
      scale_x_discrete("Age at first birth", 
                       labels=c("0"="Under 19","1"="19-22","2"="23-25",
                                               "3"="26-28","4"="29-31","5"="32-34",
                                               "6"="35-37","7"="38-40","8"="41-43",
                                               "9"="Over 43"))
    }
    else if (input$x == "wedyear") {
      scale_x_discrete("Wedding Year", 
                       labels=c("0"="Before the war", "1"="During the war", "2"="After the war"))
    }
    else if (input$x == "education") {
      scale_x_discrete("Education", 
                       labels=c("0"="Uneducated", "1"="Educated"))}
    else if (input$x == "lotta") {
      scale_x_discrete("Served in Lotta Svard", 
                       labels=c("0"="Did not serve", "1"="Served"))}
   
    else if (input$x == "servedduringwar") {
      scale_x_discrete("Served During the War", 
                       labels=c("0"="Did not serve", "1"="Served"))}
    else if (input$x == "injuredinwar") {
      scale_x_discrete("Injured During the War", 
                       labels=c("0"="Not injured", "1"="Injured"))}
    else if (input$x == "outbreed") {
      scale_x_discrete("Married Out", 
                       labels=c("0"="Married In", "1"="Married Out","2"="Never married"))
    }
    else if (input$x == "statistics_finland") {
      scale_x_discrete("Statistics Finland occupational categories", 
                       labels=c("1"="Self-employed /nnand employer farmers",
                                "2"="Self-employed /nor employers", 
                                "3"= "Upper level employees", 
                                "4"= "Lower level employees", 
                                "5"= "Manual workers"))
    }
    else if (input$x == "census_1950") {
      scale_x_discrete("Census (1950) occupational categories", 
                       labels=c("0"="Technical /nprofessionals,/n teachers",
                                "1"="Directors, /noffice workers/n typers",
                                "2"="Business /nand selling", "3"= "Agriculture and /nforestry related",
                                "4"="Mining /n and industry", "5"="Transportation", "6"= "Factory and craftsmen",
                                "7"="Handicraft /nworkers", "8"="Service"))
    }
    
    else if (input$x == "returnedkarelia_factor") {
      scale_x_discrete("Returned to Karelia between the wars", 
                       labels=c("0"="Did not return",
                                "1"="Returned"))
    }
    else scale_x_discrete("")
    
    
    
    # next change the legend labels - key is to put in p+ first
    p<- p +      
      if (input$x == "social_class") {
        scale_color_manual (name="Social class", values=colors_1, labels = c("1"="Wealthy","2"="Rich",
                                                                             "3"="Upper Middle","4"="Middle","5"="Lower Middle", "6"="Lower",
                                                                             "7"="Poor"))} 
    else if (input$x == "sex") {
      scale_color_manual (name="Sex",values=colors_1,  labels=c("0"="Females", "1"="Males"))
    } 
    else if (input$x == "wedyear") {
      scale_color_manual (name="Wedding Year",values=colors_1,  labels=c("0"="Before the war", "1"="During the war", "2"="After the war"))
    } 
    else if (input$x == "birthpopulation") {
      scale_color_manual (name="Population (logged)",values=colors_1  )
    } 
    else if (input$x == "fdf_population") {
      scale_color_manual (name="Population (logged)",values=colors_1  )
    }
    else if (input$x == "rdk_population") {
      scale_color_manual (name="Population (logged)",values=colors_1  )
    } 
    else if (input$x == "farmtotalarea") {
      scale_color_manual (name="Hectares (logged)",values=colors_1  )
    } 
    else if (input$x == "byear") {
      scale_color_manual (name="Birth cohort",values=colors_1,  labels=c("0"="Before 1900","1"="1900-1905","2"="1906-1910",
                                                                         "3"="1911-1915","4"="1916-1920","5"="1921-1925",
                                                                         "6"="1926-1930","7"="1931-1935","8"="1936-1940",
                                                                         "9"="After 1940"))
    } 
    
    else if (input$x == "agriculture") {
      scale_color_manual (name="Agriculture",values=colors_1, labels=c("0"="Non-agricultural workers", "1"="Agricultural workers"))
    }
    else if (input$x == "man_labor") {
      scale_color_manual (name="Manual labor", values=colors_1, 
                          labels=c("0"="Non-manual labor", "1"="Heavy manual labor"))
    }
    else if (input$x == "education") {
      scale_color_manual (name="Education",values=colors_1, 
                          labels=c("0"="Uneducated", "1"="Educated"))
    }
    
    else if (input$x == "outbreed") {
      scale_color_manual (name="Married Out",values=colors_1,  labels=c("0"="Married In", "1"="Married Out","2"="Never married"))
    } 
    else if (input$x == "statistics_finland") {
      scale_color_manual (name="Statistics Finland \noccupation categories",values=colors_1, 
                          labels=c("1"="Self-employed and employer farmers",
                                   "2"="Self-employed or employers", 
                                   "3"= "Upper level employees", 
                                   "4"= "Lower level employees", 
                                   "5"= "Manual workers"))
    }
    else if (input$x == "census_1950") {
      scale_color_manual (name="Census 1950 \noccupation categories",values=colors_1, 
                          labels=c("0"="Technical professionals and teachers",
                                   "1"="Directors, office workers and typers",
                                   "2"="Business and selling", "3"= "Agriculture and forestry related",
                                   "4"="Mining and industry", "5"="Transportation", "6"= "Factory and craftsmen",
                                   "7"="Handicraft workers", "8"="Service"))
    }
    else if (input$x == "returnedkarelia_factor") {
      scale_color_manual (name="Returned to Karelia between the wars",values=colors_1,  labels=c("0"="Did not return", "1"="Returned"))
    }
    else if (input$x == "lotta") {
      scale_color_manual (name="Served in Lotta svard",values=colors_1,  labels=c("0"="Did not serve", "1"="Served"))
    }
    
    else if (input$x == "servedduringwar") {
      scale_color_manual (name="Served during the war",values=colors_1,  labels=c("0"="Did not serve", "1"="Served"))
    }
    else if (input$x == "injuredinwar") {
      scale_color_manual (name="Injured during the war",values=colors_1,  labels=c("0"="Not Injured", "1"="Injured"))
    }
    
    else if (input$x == "fcyob") {
      scale_color_manual (name="First child YOB",values=colors_1)
    }
    else if (input$x == "aafb") {
      scale_color_manual (name="Age at first birth",values=colors_1)
    }
    else
      scale_color_manual (labels =("") )
    
    
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets, labeller=label_value)
    
    
    
    
    if (input$jitter)
      p <- p + geom_jitter(width=0.15, alpha=0.1)
    
    
    print(p)
    
  })
  
  
}

shinyApp(ui=ui, server=server)








