#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
# Read in data

aam <- read.csv(file = 'clean_survey.csv') 

# Set factor ordering

aam$age_scale <- factor(aam$age_scale, levels = c("Young", "Middle", "Old"))
aam$edu_scale <- factor(aam$edu_scale, levels = c("Low", "Medium", "High"))
aam$gender <- factor(aam$gender, levels = c("Woman", "Other", "Man"))
aam$wom_notwom <- factor(aam$wom_notwom, levels = c("Woman", "Not Woman"))

# The following line of code is BROKE in R
# It drops Asian
#aam$race <- factor(aam$race, levels = c("White", "Black", "Asian", "Hispanic", "Other"))
aam$race <- factor(aam$race)

aam$wht_notwht <- factor(aam$wht_notwht, levels = c("White", "Not White"))
aam$ind_dom <- factor(aam$ind_dom, levels = c("Woman", "Not Woman"))

rad_dist_sb_lab = c("Gender","Race","Age","Education","Experience")
rad_dist_sb_val = c("wom_notwom","wht_notwht","age_scale", "edu_scale", "prof_exp")

rad_rel_sb_lab = c("Age","Education","Profssional Experience","Field Experience")
rad_rel_sb_val = c("age_mid","edu_num","prof_exp_mid","field_exp_mid")

rad_rib_sb_lab = c("Gender-2","Gender-3","Race-2","Race-5")
rad_rib_sb_val = c("wom_notwom","gender","wht_notwht","race")

rad_coun_lab = c("All","USA","Not USA","Canada", "UK")
rad_coun_val = c("all","usa","not_usa","canada", "uk")

sel_ind_list = c("All" = "all","Woman-Dominated" = "wom", 
                 "Not Woman-Dominated" = "not_wom", "Health Care" = "hc", 
                 "Law" = "law", "Technology" = "tech", "Engineering" = "eng")

# Define UI for AAM application that draws density and line plots

ui <- fluidPage(

    # Application title
    titlePanel(
        fluidRow(
            tags$style(
                "a, p {font-size: 18px;}"
            ),
            column(8,tags$h3("Ask a Manager: Compensation Explorations",
                             style = "color: #854505; font: bold;"),
                     tags$p("The survey can be found at ",
                         tags$a("Ask a Manager", 
                             href = "https://www.askamanager.org/2021/04/how-much-money-do-you-make-4.html"))),
            column(4,img(height = 100, width = 300, src = "People.jpg"))
        )
    ),
    tags$hr(),
    
    # Outer tabset
    
    tabsetPanel(
        id = "outer",
        tabPanel(
            "Density Plots",
            fluidRow(
                column(1),
                column(11, 
                    br(),
                    radioButtons(
                        "dist_sb",
                        "Choose Distribution Comparison",
                        inline = TRUE, 
                        choiceNames = rad_dist_sb_lab,
                        choiceValues = rad_dist_sb_val
                    )
                )        
            ),
            tags$hr(),
            fluidRow(
                # Inner tabset
                tabsetPanel(
                    id = "inner_c1",
                    tabPanel(
                        "Country",
                        fluidRow(
                            column(1),
                            column(5, 
                                br(),
                                radioButtons(
                                    "dist_coun_rb1",
                                    "Choose Country Type",
                                    inline = TRUE,
                                    choiceNames = rad_coun_lab,
                                    choiceValues = rad_coun_val)
                            ),
                            
                            column(1),
                            column(5, 
                                br(),
                                radioButtons(
                                    "dist_coun_rb2",
                                    "Choose Country Type",
                                    inline = TRUE,
                                    choiceNames = rad_coun_lab,
                                    choiceValues = rad_coun_val)
                            ),
                            tags$hr()
                        ),
                        fluidRow(
                            column(6, plotOutput("dist_coun_pt1")),
                            column(6, plotOutput("dist_coun_pt2"))
                        ),
                        fluidRow(
                            column(6, tableOutput("dist_coun_tab1")),
                            column(6, tableOutput("dist_coun_tab2"))
                        )
                    ),
                    tabPanel(
                        "Industry",
                        fluidRow(
                            column(1),
                            column(5, 
                                br(),
                                selectInput(
                                    "dist_ind_sel1",
                                    "Choose Industry Type",
                                    choices = sel_ind_list)
                            ),
                            column(1),
                            column(5, 
                                br(),
                                selectInput(
                                    "dist_ind_sel2",
                                    "Choose Industry Type",
                                    choices = sel_ind_list)
                            ),
                            tags$hr()
                        ),
                        fluidRow(
                            column(6, plotOutput("dist_ind_pt1")),
                            column(6, plotOutput("dist_ind_pt2"))
                        ),
                        fluidRow(
                            column(6, tableOutput("dist_ind_tab1")),
                            column(6, tableOutput("dist_ind_tab2"))
                        )
                    )
                )
            )
        ),
        tabPanel(
            "Ribbon Plots",
            fluidRow(
                column(1),
                column(4,
                    br(),
                    radioButtons(
                        "rel_sb",
                        "Choose Relationship Variable",
                        inline = TRUE, 
                        choiceNames = rad_rel_sb_lab,
                        choiceValues = rad_rel_sb_val)
                ),
                column(3,
                    br(),
                    radioButtons(
                        "rib_sb",
                        "Choose Feature Comparison",
                        inline = TRUE, 
                        choiceNames = rad_rib_sb_lab,
                        choiceValues = rad_rib_sb_val)
                ),
                column(4,
                    br(),
                    sliderInput(
                        "slide_sb",
                        "Choose Maximum Salary",
                        value = 370, 
                        min = 100,
                        max = 1000)
                ),
                tags$hr()
            ),
            fluidRow(
                # Inner tabset
                tabsetPanel(
                    id = "inner_c2",
                    tabPanel(
                        "Country",
                        fluidRow(
                            column(1),
                            column(5,
                                br(),
                                radioButtons(
                                    "rel_coun_rb1",
                                    "Choose Country Type",
                                    inline = TRUE,
                                    choiceNames = rad_coun_lab,
                                    choiceValues = rad_coun_val)
                            ),
                            column(1),
                            column(5,
                                br(),
                                radioButtons(
                                    "rel_coun_rb2",
                                    "Choose Country Type",
                                    inline = TRUE,
                                    choiceNames = rad_coun_lab,
                                    choiceValues = rad_coun_val)
                            ),
                            tags$hr()
                        ),
                        fluidRow(
                            column(6, plotOutput("rel_coun_pt1")),
                            column(6, plotOutput("rel_coun_pt2"))
                        )
                    ),
                    tabPanel(
                        "Industry",
                        fluidRow(
                            column(1),
                            column(5,
                                br(),
                                selectInput(
                                    "rel_ind_sel1",
                                    "Choose Industry Type",
                                    choices = sel_ind_list)
                            ),
                            column(1),
                            column(5,
                                br(),
                                selectInput(
                                    "rel_ind_sel2",
                                    "Choose Industry Type",
                                    choices = sel_ind_list)

                            ),
                            tags$hr()
                        ),
                        fluidRow(
                            column(6, plotOutput("rel_ind_pt1")),
                            column(6, plotOutput("rel_ind_pt2"))
                        )
                    )
                )
            )
        )
    )
)

# Define server logic 
server <- function(input, output) 
{ 
    # Colors
    
    # Woman - Not Woman
    color2a = c("#660033", "#000066")
    
    # Gender: Woman, Other, Man
    color3a = c("#660033", "#997300", "#000066")
    
    # White - Not White
    color2b = c("#997300", "#3D0066")
    
    # Race: Asian, Black, Hispanic, Other, White
    color5 = c("#000066", "#333333","#660033", "#336600", "#997300")
    
    # Age Scale: Young, Middle, Old
    # Edu Scale: Low, Medium,  High
    color3b = c("#997300", "#660033", "#336600")
    
    # Edu Scale: Low, Medium,  High
    color3c = c("#336600", "#660033", "#997300")
    
    # Experience
    color4 = c("#997300","#336600","#660033","#000066") 

    #################################
    ##                             ##
    ##    Density Country Plots    ##
    ##                             ##
    ################################# 
    
    # Distribution Density Plots

    # Returns data.frame used in ggplot based on
    # Distribution Radio Buttons for Country Density Plots
    # "dist_coun_rb": rad_coun = c("all","usa","not_usa","canada", "uk")
    
    df_den_coun_lt <- reactive(
    {
        switch(input$dist_coun_rb1, 
               "all" = aam %>% filter(usd_total_comp < 370), 
               "usa" = aam %>% filter(usd_total_comp < 370 & usa == "USA"), 
               "not_usa" = aam %>% filter(usd_total_comp < 370 & usa == "Not USA"),
               "canada" = aam %>% filter(usd_total_comp < 370 & country == "canada"),
               "uk" = aam %>% filter(usd_total_comp < 370 & country == "uk"))
    })

    df_den_coun_rt <- reactive(
    {
        switch(input$dist_coun_rb2, 
               "all" = aam %>% filter(usd_total_comp < 370), 
               "usa" = aam %>% filter(usd_total_comp < 370 & usa == "USA"), 
               "not_usa" = aam %>% filter(usd_total_comp < 370 & usa == "Not USA"),
               "canada" = aam %>% filter(usd_total_comp < 370 & country == "canada"),
               "uk" = aam %>% filter(usd_total_comp < 370 & country == "uk"))
    })
    
    # Returns title used in ggplot based on
    # Distribution Radio Buttons for Country Density Plots
    # "dist_coun_rb": rad_coun = c("all","usa","not_usa","canada", "uk")

    tit_den_coun_lt <- reactive(
    {
        switch(input$dist_coun_rb1, 
               "all" = "Density Plot for Total Compensation\nFor All Respondents", 
               "usa" = "Density Plot for Total Compensation\nFor United States Respondents", 
               "not_usa" = "Density Plot for Total Compensation\nFor Non US Respondents",
               "canada" = "Density Plot for Total Compensation\nFor Canadian Respondents",
               "uk" = "Density Plot for Total Compensation\nFor United Kingdom Respondents")
    })
    
    tit_den_coun_rt <- reactive(
    {
        switch(input$dist_coun_rb2, 
               "all" = "Density Plot for Total Compensation\nFor All Respondents", 
               "usa" = "Density Plot for Total Compensation\nFor United States Respondents", 
               "not_usa" = "Density Plot for Total Compensation\nFor Non US Respondents",
               "canada" = "Density Plot for Total Compensation\nFor Canadian Respondents",
               "uk" = "Density Plot for Total Compensation\nFor United Kingdom Respondents")
    })

    # Returns legend name in ggplot based on 
    # Distribution Radio Buttons for Density Plots
    # "dist_sb": rad_dist_sb = c("wom_notwom","wht_notwht","age_scale", "edu_scale", "prof_exp")
    
    den_name <- reactive( 
    { 
        switch(input$dist_sb,
               "wom_notwom" = "Gender",
               "wht_notwht" = "Race",
               "age_scale" = "Age",
               "edu_scale" = "Education",
               "prof_exp" = "Experience")
    })  
        
    # Returns color in ggplot based on 
    # Distribution Radio Buttons for Density Plots
    # "dist_sb": rad_dist_sb = c("wom_notwom","wht_notwht","age_scale", "edu_scale", "prof_exp")

    den_color <- reactive( 
    { 
        switch(input$dist_sb,
               "wom_notwom" = color2a,
               "wht_notwht" = color2b,
               "age_scale" = color3b,
               "edu_scale" = color3c,
               "prof_exp" = color4)
    })
    
    # Returns mapping in ggplot based on 
    # Distribution Radio Buttons for Density Plots
    # "dist_sb": rad_dist_sb = c("wom_notwom","wht_notwht","age_scale", "edu_scale", "prof_exp")
    
    den_map <- reactive( 
    { 
        switch(input$dist_sb,
               "wom_notwom" = aes(x = usd_total_comp, color = wom_notwom,
                                                      fill = wom_notwom),
               "wht_notwht" = aes(x = usd_total_comp, color = wht_notwht,
                                                      fill = wht_notwht),
               "age_scale" = aes(x = usd_total_comp, color = age_scale,
                                                      fill = age_scale),
               "edu_scale" = aes(x = usd_total_comp, color = edu_scale,
                                                      fill = edu_scale),
               "prof_exp" = aes(x = usd_total_comp, color = prof_exp))
    })
    
    # Summary Data Frame for Table and Vertical Line on Left
    
    sum_coun_den_df_lt <- reactive(
    {
        if (input$dist_coun_rb1 == "all")
        {
             df <- aam %>% select (.data[[input$dist_sb]], usd_total_comp) %>%
                       group_by(.data[[input$dist_sb]]) %>% 
                       summarize (Mean = mean(usd_total_comp),
                                  Std = sd(usd_total_comp),
                                  Median = median(usd_total_comp)) 
             
        }
        else if (input$dist_coun_rb1 == "usa")
        {
            df <- aam %>% filter(usa == "USA") %>% 
                    select (.data[[input$dist_sb]], usd_total_comp) %>%
                    group_by(.data[[input$dist_sb]]) %>% 
                    summarize (Mean = mean(usd_total_comp),
                               Std = sd(usd_total_comp),
                               Median = median(usd_total_comp)) 
        }
        else if (input$dist_coun_rb1 == "not_usa")
        {
            df <- aam %>% filter(usa == "Not USA") %>% 
                    select (.data[[input$dist_sb]], usd_total_comp) %>%
                    group_by(.data[[input$dist_sb]]) %>% 
                    summarize (Mean = mean(usd_total_comp),
                               Std = sd(usd_total_comp),
                               Median = median(usd_total_comp)) 
        }
        else if (input$dist_coun_rb1 == "canada")
        {
            df <- aam %>% filter(country == "canada") %>% 
                    select (.data[[input$dist_sb]], usd_total_comp) %>%
                    group_by(.data[[input$dist_sb]]) %>% 
                    summarize (Mean = mean(usd_total_comp),
                               Std = sd(usd_total_comp),
                               Median = median(usd_total_comp)) 
        }
        else if (input$dist_coun_rb1 == "uk")
        {
            df <- aam %>% filter(country == "uk") %>% 
                    select (.data[[input$dist_sb]], usd_total_comp) %>%
                    group_by(.data[[input$dist_sb]]) %>% 
                    summarize (Mean = mean(usd_total_comp),
                               Std = sd(usd_total_comp),
                               Median = median(usd_total_comp)) 
        }
        
        df
    })   

    # Summary Data Frame for Table and Vertical Line on Right
    
    sum_coun_den_df_rt <- reactive(
    {
        if (input$dist_coun_rb2 == "all")
        {
            df <- aam %>% select (.data[[input$dist_sb]], usd_total_comp) %>%
                group_by(.data[[input$dist_sb]]) %>% 
                summarize (Mean = mean(usd_total_comp),
                           Std = sd(usd_total_comp),
                           Median = median(usd_total_comp)) 
            
        }
        else if (input$dist_coun_rb2 == "usa")
        {
            df <- aam %>% filter(usa == "USA") %>% 
                select (.data[[input$dist_sb]], usd_total_comp) %>%
                group_by(.data[[input$dist_sb]]) %>% 
                summarize (Mean = mean(usd_total_comp),
                           Std = sd(usd_total_comp),
                           Median = median(usd_total_comp)) 
        }
        else if (input$dist_coun_rb2 == "not_usa")
        {
            df <- aam %>% filter(usa == "Not USA") %>% 
                select (.data[[input$dist_sb]], usd_total_comp) %>%
                group_by(.data[[input$dist_sb]]) %>% 
                summarize (Mean = mean(usd_total_comp),
                           Std = sd(usd_total_comp),
                           Median = median(usd_total_comp)) 
        }
        else if (input$dist_coun_rb2 == "canada")
        {
            df <- aam %>% filter(country == "canada") %>% 
                select (.data[[input$dist_sb]], usd_total_comp) %>%
                group_by(.data[[input$dist_sb]]) %>% 
                summarize (Mean = mean(usd_total_comp),
                           Std = sd(usd_total_comp),
                           Median = median(usd_total_comp)) 
        }
        else if (input$dist_coun_rb2 == "uk")
        {
            df <- aam %>% filter(country == "uk") %>% 
                select (.data[[input$dist_sb]], usd_total_comp) %>%
                group_by(.data[[input$dist_sb]]) %>% 
                summarize (Mean = mean(usd_total_comp),
                           Std = sd(usd_total_comp),
                           Median = median(usd_total_comp)) 
        }
        
        df
    })   

    # Country Density Distribution Left Side Plot
    
    output$dist_coun_pt1 <- renderPlot(
    {
        g <- ggplot(df_den_coun_lt(), mapping = den_map()) +
        geom_density(alpha=0.3, mapping = aes(y =..density..), size = 1.2) +
    
        scale_color_manual(name = den_name(),
                           values = den_color()) +
        scale_fill_manual(name = den_name(),
                          values = den_color()) +
        scale_x_continuous(limits = c(0, 370)) +
    
        geom_vline(data = sum_coun_den_df_lt(), aes(xintercept = Median, 
                              color = .data[[input$dist_sb]]), size=1) + 
            
        labs(title = tit_den_coun_lt(),
            x = "Compensation (in 1000s of US Dollars)",
            y = "Density") +
    
        theme_classic() +
        theme(plot.title = element_text(family="sans", size=14),
            axis.title.x = element_text(family="sans", size=12),
            axis.title.y = element_text(family="sans", size=12),
            axis.text.x = element_text(family="sans", size=11),
            axis.text.y = element_blank())
        g
        
    }, res = 96)
    
    # Country Density Distribution Right Side Plot
    
    output$dist_coun_pt2 <- renderPlot(
    {
        g <- ggplot(df_den_coun_rt(), mapping = den_map()) +
        geom_density(alpha=0.3, mapping = aes(y =..density..), size = 1.2) +
    
        scale_color_manual(name = den_name(),
                           values = den_color()) +
        scale_fill_manual(name = den_name(),
                          values = den_color()) +
        scale_x_continuous(limits = c(0, 370)) +
    
        geom_vline(data = sum_coun_den_df_rt(), aes(xintercept = Median, 
                              color = .data[[input$dist_sb]]), size=1) +
                                       
        labs(title = tit_den_coun_rt(),
            x = "Compensation (in 1000s of US Dollars)",
            y = "Density") +
    
        theme_classic() +
        theme(plot.title = element_text(family="sans", size=14),
            axis.title.x = element_text(family="sans", size=12),
            axis.title.y = element_text(family="sans", size=12),
            axis.text.x = element_text(family="sans", size=11),
            axis.text.y = element_blank())
        g
        
    }, res = 96)
    
    
    tab_col_name <- reactive(
    {
        switch(input$dist_sb, 
               "wom_notwom" = "Gender", 
               "wht_notwht" = "Race", 
               "age_scale" = "Age",
               "edu_scale" = "Education",
               "prof_exp" = "Experience")  
    }) 
    
    # Country Density Distribution Left Side Table
    # "dist_sb": rad_dist_sb = c("wom_notwom","wht_notwht","age_scale", "edu_scale", "prof_exp")
    # "dist_coun_rb": rad_coun = c("all","usa","not_usa","canada", "uk")
    
    output$dist_coun_tab1 <- renderTable(
    {
        df <- sum_coun_den_df_lt()
        colnames(df)[1] <- tab_col_name()
        df
    })   
    
    # Country Density Distribution Right Side Table
    # "dist_sb": rad_dist_sb = c("wom_notwom","wht_notwht","age_scale", "edu_scale", "prof_exp")
    # "dist_coun_rb": rad_coun = c("all","usa","not_usa","canada", "uk")
    
    output$dist_coun_tab2 <- renderTable(
    {
        df <- sum_coun_den_df_rt()
        colnames(df)[1] <- tab_col_name()
        df
        
    })   
    
    #################################
    ##                             ##
    ##   Density Industry Plots    ##
    ##                             ##
    ################################# 

    # Returns data.frame used in ggplot based on
    # Distribution Select Input for Industry Density Plots
    # "dist_ind_rb": rad_ind = c("all","wom","hc","law","not_wom", "tech","eng")
    
    df_den_ind_lt <- reactive(
    {
        switch(input$dist_ind_sel1, 
               "all" = aam %>% filter(usd_total_comp < 370), 
               "wom" = aam %>% filter(usd_total_comp < 370 & ind_dom == "Woman"), 
               "not_wom" = aam %>% filter(usd_total_comp < 370 & ind_dom == "Not Woman"),
               "hc" = aam %>% filter(usd_total_comp < 370 & industry == "Health care"),
               "law" = aam %>% filter(usd_total_comp < 370 & industry == "Law"),
               "tech" = aam %>% filter(usd_total_comp < 370 & industry == "Computing or Tech"),
               "eng" = aam %>% filter(usd_total_comp < 370 & industry == "Engineering or Manufacturing"))
    })

    df_den_ind_rt <- reactive(
    {
        switch(input$dist_ind_sel2, 
               "all" = aam %>% filter(usd_total_comp < 370), 
               "wom" = aam %>% filter(usd_total_comp < 370 & ind_dom == "Woman"), 
               "not_wom" = aam %>% filter(usd_total_comp < 370 & ind_dom == "Not Woman"),
               "hc" = aam %>% filter(usd_total_comp < 370 & industry == "Health care"),
               "law" = aam %>% filter(usd_total_comp < 370 & industry == "Law"),
               "tech" = aam %>% filter(usd_total_comp < 370 & industry == "Computing or Tech"),
               "eng" = aam %>% filter(usd_total_comp < 370 & industry == "Engineering or Manufacturing"))
    })

    # Returns title used in ggplot based on
    # Distribution Select Input for Industry Density Plots
    # "dist_ind_rb": rad_ind = c("all","wom","hc","law","not_wom", "tech","eng")
    
    tit_den_ind_lt <- reactive(
    {
        switch(input$dist_ind_sel1, 
               "all" = "Density Plot for Total Compensation\nFor All Industries", 
               "wom" = "Density Plot for Total Compensation\nFor Woman-Dominated Industries", 
               "not_wom" = "Density Plot for Total Compensation\nFor Non Woman-Dominated Industries",
               "hc" = "Density Plot for Total Compensation\nFor Health Care Industries",
               "law" = "Density Plot for Total Compensation\nFor Law Industries",
               "tech" = "Density Plot for Total Compensation\nFor Technology Industries",
               "eng" = "Density Plot for Total Compensation\nFor Engineering Industries")
    })
    
     
    tit_den_ind_rt <- reactive(
    {
        switch(input$dist_ind_sel2, 
               "all" = "Density Plot for Total Compensation\nFor All Industries", 
               "wom" = "Density Plot for Total Compensation\nFor Woman-Dominated Industries", 
               "not_wom" = "Density Plot for Total Compensation\nFor Non Woman-Dominated Industries",
               "hc" = "Density Plot for Total Compensation\nFor Health Care Industries",
               "law" = "Density Plot for Total Compensation\nFor Law Industries",
               "tech" = "Density Plot for Total Compensation\nFor Technology Industries",
               "eng" = "Density Plot for Total Compensation\nFor Engineering Industries")
    })
    
    sum_ind_den_df_lt <- reactive(
    {
        if (input$dist_ind_sel1 == "all")
        {
            df <- aam %>% select (.data[[input$dist_sb]], usd_total_comp) %>%
                group_by(.data[[input$dist_sb]]) %>% 
                summarize (Mean = mean(usd_total_comp),
                           Std = sd(usd_total_comp),
                           Median = median(usd_total_comp))
        }
        else if (input$dist_ind_sel1 == "wom")
        {
            df <- aam %>% filter(ind_dom == "Woman") %>% 
                select (.data[[input$dist_sb]], usd_total_comp) %>%
                group_by(.data[[input$dist_sb]]) %>% 
                summarize (Mean = mean(usd_total_comp),
                           Std = sd(usd_total_comp),
                           Median = median(usd_total_comp))
        }
        else if (input$dist_ind_sel1 == "not_wom")
        {
            df <- aam %>% filter(ind_dom == "Not Woman") %>% 
                select (.data[[input$dist_sb]], usd_total_comp) %>%
                group_by(.data[[input$dist_sb]]) %>% 
                summarize (Mean = mean(usd_total_comp),
                           Std = sd(usd_total_comp),
                           Median = median(usd_total_comp))
        }
        else if (input$dist_ind_sel1 == "hc")
        {
            df <- aam %>% filter(industry == "Health care") %>% 
                select (.data[[input$dist_sb]], usd_total_comp) %>%
                group_by(.data[[input$dist_sb]]) %>% 
                summarize (Mean = mean(usd_total_comp),
                           Std = sd(usd_total_comp),
                           Median = median(usd_total_comp))
        }
        else if (input$dist_ind_sel1 == "law")
        {
            df <- aam %>% filter(industry == "Law") %>% 
                select (.data[[input$dist_sb]], usd_total_comp) %>%
                group_by(.data[[input$dist_sb]]) %>% 
                summarize (Mean = mean(usd_total_comp),
                           Std = sd(usd_total_comp),
                           Median = median(usd_total_comp))
        }
        else if (input$dist_ind_sel1 == "tech")
        {
            df <- aam %>% filter(industry == "Computing or Tech") %>% 
                select (.data[[input$dist_sb]], usd_total_comp) %>%
                group_by(.data[[input$dist_sb]]) %>% 
                summarize (Mean = mean(usd_total_comp),
                           Std = sd(usd_total_comp),
                           Median = median(usd_total_comp))
        }
        else if (input$dist_ind_sel1 == "eng")
        {
            df <- aam %>% filter(industry == "Engineering or Manufacturing") %>% 
                select (.data[[input$dist_sb]], usd_total_comp) %>%
                group_by(.data[[input$dist_sb]]) %>% 
                summarize (Mean = mean(usd_total_comp),
                           Std = sd(usd_total_comp),
                           Median = median(usd_total_comp))
        }
        
        df
    })
    
    sum_ind_den_df_rt <- reactive(
    {
        if (input$dist_ind_sel2 == "all")
        {
            df <- aam %>% 
                select (.data[[input$dist_sb]], usd_total_comp) %>%
                group_by(.data[[input$dist_sb]]) %>% 
                summarize (Mean = mean(usd_total_comp),
                           Std = sd(usd_total_comp),
                           Median = median(usd_total_comp))
        }
        else if (input$dist_ind_sel2 == "wom")
        {
            df <- aam %>% filter(ind_dom == "Woman") %>% 
                select (.data[[input$dist_sb]], usd_total_comp) %>%
                group_by(.data[[input$dist_sb]]) %>% 
                summarize (Mean = mean(usd_total_comp),
                           Std = sd(usd_total_comp),
                           Median = median(usd_total_comp))
        }
        else if (input$dist_ind_sel2 == "not_wom")
        {
            df <- aam %>% filter(ind_dom == "Not Woman") %>% 
                select (.data[[input$dist_sb]], usd_total_comp) %>%
                group_by(.data[[input$dist_sb]]) %>% 
                summarize (Mean = mean(usd_total_comp),
                           Std = sd(usd_total_comp),
                           Median = median(usd_total_comp))
        }
        else if (input$dist_ind_sel2 == "hc")
        {
            df <- aam %>% filter(industry == "Health care") %>% 
                select (.data[[input$dist_sb]], usd_total_comp) %>%
                group_by(.data[[input$dist_sb]]) %>% 
                summarize (Mean = mean(usd_total_comp),
                           Std = sd(usd_total_comp),
                           Median = median(usd_total_comp))
        }
        else if (input$dist_ind_sel2 == "law")
        {
            df <- aam %>% filter(industry == "Law") %>% 
                select (.data[[input$dist_sb]], usd_total_comp) %>%
                group_by(.data[[input$dist_sb]]) %>% 
                summarize (Mean = mean(usd_total_comp),
                           Std = sd(usd_total_comp),
                           Median = median(usd_total_comp))
        }
        else if (input$dist_ind_sel2 == "tech")
        {
            df <- aam %>% filter(industry == "Computing or Tech") %>% 
                select (.data[[input$dist_sb]], usd_total_comp) %>%
                group_by(.data[[input$dist_sb]]) %>% 
                summarize (Mean = mean(usd_total_comp),
                           Std = sd(usd_total_comp),
                           Median = median(usd_total_comp))
        }
        else if (input$dist_ind_sel2 == "eng")
        {
            df <- aam %>% filter(industry == "Engineering or Manufacturing") %>% 
                select (.data[[input$dist_sb]], usd_total_comp) %>%
                group_by(.data[[input$dist_sb]]) %>% 
                summarize (Mean = mean(usd_total_comp),
                           Std = sd(usd_total_comp),
                           Median = median(usd_total_comp))
        }
        
        df
    }) 
    
    # Industry Density Distribution Plot
     
    # Left Side Plot
    
    output$dist_ind_pt1 <- renderPlot(
    {
        ggplot(df_den_ind_lt(), mapping = den_map()) +
        geom_density(alpha=0.3, mapping = aes(y =..density..), size = 1.2) +
    
        scale_color_manual(name = den_name(),
                           values = den_color()) +
        scale_fill_manual(name = den_name(),
                          values = den_color()) +
        scale_x_continuous(limits = c(0, 370)) +
    
        geom_vline(data = sum_ind_den_df_lt(), aes(xintercept = Median, 
                              color = .data[[input$dist_sb]]), size = 0.9) +
            
        labs(title = tit_den_ind_lt(),
            x = "Compensation (in 1000s of US Dollars)",
            y = "Density") +
    
        theme_classic() +
        theme(plot.title = element_text(family="sans", size=14),
            axis.title.x = element_text(family="sans", size=12),
            axis.title.y = element_text(family="sans", size=12),
            axis.text.x = element_text(family="sans", size=11),
            axis.text.y = element_blank())
            
    }, res = 96)
    
    # Right Side Plot
    
    output$dist_ind_pt2 <- renderPlot(
    {
        ggplot(df_den_ind_rt(), mapping = den_map()) +
        geom_density(alpha=0.3, mapping = aes(y =..density..), size = 1.2) +
    
        scale_color_manual(name = den_name(),
                           values = den_color()) +
        scale_fill_manual(name = den_name(),
                          values = den_color()) +
        scale_x_continuous(limits = c(0, 370)) +
            
        geom_vline(data = sum_ind_den_df_rt(), aes(xintercept = Median, 
                              color = .data[[input$dist_sb]]), size = 0.9) +
                              
        labs(title = tit_den_ind_rt(),
            x = "Compensation (in 1000s of US Dollars)",
            y = "Density") +
    
        theme_classic() +
        theme(plot.title = element_text(family="sans", size=14),
            axis.title.x = element_text(family="sans", size=12),
            axis.title.y = element_text(family="sans", size=12),
            axis.text.x = element_text(family="sans", size=11),
            axis.text.y = element_blank())
            
    }, res = 96) 
    

    # Industry Density Distribution Left Side Table
    # "dist_sb": rad_dist_sb = c("wom_notwom","wht_notwht","age_scale", "edu_scale", "prof_exp")
    # "dist_ind_rb": rad_ind = c("all","wom","hc","law","not_wom", "tech","eng")
    
    output$dist_ind_tab1 <- renderTable(
    {
        df <- sum_ind_den_df_lt()
        colnames(df)[1] <- tab_col_name()
        df
    }) 
    
    # Industry Density Distribution Right Side Table
    # "dist_sb": rad_dist_sb = c("wom_notwom","wht_notwht","age_scale", "edu_scale", "prof_exp")
    # "dist_ind_rb": rad_ind = c("all","wom","hc","law","not_wom", "tech","eng")
    
    output$dist_ind_tab2 <- renderTable(
    {
        df <- sum_ind_den_df_rt()
        colnames(df)[1] <- tab_col_name()
        df
    }) 
    
    #################################
    #################################
    ##                             ##
    ##      Ribbon Line Plots      ##
    ##                             ##
    ################################# 
    #################################  

    #################################
    ##                             ##
    ##    Ribbon Country Plots     ##
    ##                             ##
    ################################# 
    
    # Returns data.frame used in ggplot based on
    # Relationshiup Radio Buttons for Country Line Plots
    # "rel_coun_rb": rad_coun = c("all","usa","not_usa","canada", "uk")
    
    df_rel_coun_lt <- reactive(
    {
        switch(input$rel_coun_rb1, 
               "all" = aam %>% filter(usd_total_comp < input$slide_sb), 
               "usa" = aam %>% filter(usd_total_comp < input$slide_sb & usa == "USA"), 
               "not_usa" = aam %>% filter(usd_total_comp < input$slide_sb & usa == "Not USA"),
               "canada" = aam %>% filter(usd_total_comp < input$slide_sb & country == "canada"),
               "uk" = aam %>% filter(usd_total_comp < input$slide_sb & country == "uk"))
    })

    df_rel_coun_rt <- reactive(
    {
        switch(input$rel_coun_rb2, 
               "all" = aam %>% filter(usd_total_comp < input$slide_sb), 
               "usa" = aam %>% filter(usd_total_comp < input$slide_sb & usa == "USA"), 
               "not_usa" = aam %>% filter(usd_total_comp < input$slide_sb & usa == "Not USA"),
               "canada" = aam %>% filter(usd_total_comp < input$slide_sb & country == "canada"),
               "uk" = aam %>% filter(usd_total_comp < input$slide_sb & country == "uk"))
    })

    # Returns title used in ggplot based on
    # Relationshiup Radio Buttons for Country Line Plots
    # "rel_coun_rb": rad_coun = c("all","usa","not_usa","canada", "uk")
    # "rel_sb": rad_rel_sb = c("age_mid","edu_num","prof_exp_mid","field_exp_mid")

    tit_rel_coun_lt <- reactive(
    {
        if (input$rel_sb == "age_mid")
        {
            switch(input$dist_coun_rb1, 
                   "all" = "Total Compensation by Age\nFor All Respondents", 
                   "usa" = "Total Compensation by Age\nFor United States Respondents", 
                   "not_usa" = "Total Compensation by Age\nFor Non US Respondents",
                   "canada" = "Total Compensation by Age\nFor Canadian Respondents",
                   "uk" = "Total Compensation by Age\nFor UK Respondents")
        }
        else if (input$rel_sb == "edu_num")
        {
            switch(input$dist_coun_rb1, 
                   "all" = "Total Compensation by Education\nFor All Respondents", 
                   "usa" = "Total Compensation by Education\nFor United States Respondents", 
                   "not_usa" = "Total Compensation by Education\nFor Non US Respondents",
                   "canada" = "Total Compensation by Education\nFor Canadian Respondents",
                   "uk" = "Total Compensation by Education\nFor UK Respondents")
        }
        else if (input$rel_sb == "prof_exp_mid")
        {
            switch(input$dist_coun_rb1, 
                   "all" = "Total Compensation by Professional Experience\nFor All Respondents", 
                   "usa" = "Total Compensation by Professional Experience\nFor United States Respondents", 
                   "not_usa" = "Total Compensation by Professional Experience\nFor Non US Respondents",
                   "canada" = "Total Compensation by Professional Experience\nFor Canadian Respondents",
                   "uk" = "Total Compensation by Professional Experience\nFor UK Respondents")
        }
        else if (input$rel_sb == "field_exp_mid")
        {
            switch(input$dist_coun_rb1, 
                   "all" = "Total Compensation by Field Experience\nFor All Respondents", 
                   "usa" = "Total Compensation by Field Experience\nFor United States Respondents", 
                   "not_usa" = "Total Compensation by Field Experience\nFor Non US Respondents",
                   "canada" = "Total Compensation by Field Experience\nFor Canadian Respondents",
                   "uk" = "Total Compensation by Field Experience\nFor UK Respondents")
        }
    })
    
    tit_rel_coun_rt <- reactive(
    {
        if (input$rel_sb == "age_mid")
        {
            switch(input$dist_coun_rb2, 
                   "all" = "Total Compensation by Age\nFor All Respondents", 
                   "usa" = "Total Compensation by Age\nFor United States Respondents", 
                   "not_usa" = "Total Compensation by Age\nFor Non US Respondents",
                   "canada" = "Total Compensation by Age\nFor Canadian Respondents",
                   "uk" = "Total Compensation by Age\nFor UK Respondents")
        }
        else if (input$rel_sb == "edu_num")
        {
            switch(input$dist_coun_rb2, 
                   "all" = "Total Compensation by Education\nFor All Respondents", 
                   "usa" = "Total Compensation by Education\nFor United States Respondents", 
                   "not_usa" = "Total Compensation by Education\nFor Non US Respondents",
                   "canada" = "Total Compensation by Education\nFor Canadian Respondents",
                   "uk" = "Total Compensation by Education\nFor UK Respondents")
        }
        else if (input$rel_sb == "prof_exp_mid")
        {
            switch(input$dist_coun_rb2, 
                   "all" = "Total Compensation by Professional Experience\nFor All Respondents", 
                   "usa" = "Total Compensation by Professional Experience\nFor United States Respondents", 
                   "not_usa" = "Total Compensation by Professional Experience\nFor Non US Respondents",
                   "canada" = "Total Compensation by Professional Experience\nFor Canadian Respondents",
                   "uk" = "Total Compensation by Professional Experience\nFor UK Respondents")
        }
        else if (input$rel_sb == "field_exp_mid")
        {
            switch(input$dist_coun_rb2, 
                   "all" = "Total Compensation by Field Experience\nFor All Respondents", 
                   "usa" = "Total Compensation by Field Experience\nFor United States Respondents", 
                   "not_usa" = "Total Compensation by Field Experience\nFor Non US Respondents",
                   "canada" = "Total Compensation by Field Experience\nFor Canadian Respondents",
                   "uk" = "Total Compensation by Field Experience\nFor UK Respondents")
        }
    })
    
    # Returns legend name in ggplot based on 
    # Relationship Radio Buttons for Line Plots
    # "rib_sb": rad_rib_sb= c("wom_notwom","gender","wht_notwht","race")
    
    rib_name <- reactive( 
    { 
        switch(input$rib_sb,
               "wom_notwom" = "Gender-2",
               "gender" = "Gender-3",
               "wht_notwht" = "Race-2",
               "race" = "Race-5")
    })
    
    # Returns color in ggplot based on 
    # Relationship Radio Buttons for Line Plots
    # "rib_sb": rad_rib_sb= c("wom_notwom","gender","wht_notwht","race")
    
    rib_color <- reactive( 
    { 
        switch(input$rib_sb,
               "wom_notwom" = color2a,
               "gender" = color3a,
               "wht_notwht" = color2b,
               "race" = color5)
    })
    
    
    # Create country summary function for left plot
     
    sum_coun_df_lt <- reactive(
    {
        df <- df_rel_coun_lt()
        df$.data[[input$rib_sb]] <- factor(df$.data[[input$rib_sb]])
         
        df %>%
            group_by(.data[[input$rib_sb]], .data[[input$rel_sb]]) %>%
            summarize(n = n(),
                med = quantile(usd_total_comp)[3],
                mad = mad(usd_total_comp),
                lowerMad = med - (sqrt(mad)/2),
                upperMad = med + (sqrt(mad)/2))
    })

    # Create country summary function for right plot
     
    sum_coun_df_rt <- reactive(
    {
        df <- df_rel_coun_rt()
        df$.data[[input$rib_sb]] <- factor(df$.data[[input$rib_sb]])
         
        df %>%
            group_by(.data[[input$rib_sb]], .data[[input$rel_sb]]) %>%
            summarize(n = n(),
                med = quantile(usd_total_comp)[3],
                mad = mad(usd_total_comp),
                lowerMad = med - (sqrt(mad)/2),
                upperMad = med + (sqrt(mad)/2))
    })
    
    # Left Side Country Relationship Line Plot
    # "rel_sb": rad_rel_sb = c("age_mid","edu_num","prof_exp_mid","field_exp_mid")
    
    output$rel_coun_pt1 <- renderPlot(
    { 
        # Relationship SideBar Radio Buttons
        
        if (input$rel_sb == "age_mid")
        {
            x_brks = c(20,30,40,50,60)
            x_tks = c("24-","25-34","35-44","45-54","55+")
            x_lab = "Age"
        } 
        else if (input$rel_sb == "edu_num")
        {
            x_brks = c(1,2,3,4,5,6)
            x_tks = c("HS","Some Col","Col Deg","MS","PhD","Prof")
            x_lab = "Education"
        } 
        else if (input$rel_sb == "prof_exp_mid")
        {
            x_brks = c(5,15,25,35)
            x_tks = c("0-10","11-20","21-30","31+")
            x_lab = "Professional Experience"
        } 
        else if (input$rel_sb == "field_exp_mid")
        {
            x_brks = c(5,15,25,35)
            x_tks = c("0-10","11-20","21-30","31+")
            x_lab = "Field Experience"
        }
        
        ggplot(sum_coun_df_lt(), aes(x = .data[[input$rel_sb]], 
                                  color = .data[[input$rib_sb]])) +
        geom_line(aes(y = sum_coun_df_lt()$med, 
                      group = .data[[input$rib_sb]]), size = 1.2) +
        geom_ribbon(aes(ymin = sum_coun_df_lt()$lowerMad, 
                        ymax = sum_coun_df_lt()$upperMad, 
                        group = .data[[input$rib_sb]], 
                        fill = .data[[input$rib_sb]]), 
                        alpha = 0.2, size = 0.3)+
     
        scale_color_manual(name = rib_name(),
                           values = rib_color())+
        scale_fill_manual(name = rib_name(),
                          values = rib_color()) +
        scale_y_continuous(breaks = seq(20,350,20)) +
        scale_x_continuous(breaks = x_brks, labels = x_tks) +
    
        labs(title = tit_rel_coun_lt(),
             x = x_lab,
             y = "Median Compensation (in 1000s of US Dollars)") +
    
        theme_minimal() +
        theme(legend.position = "bottom",
              plot.title = element_text(family="sans", size=16),
              axis.title.x = element_text(family="sans", size=14),
              axis.title.y = element_text(family="sans", size=14),
              axis.text.x = element_text(family="sans", size=13),
              axis.text.y = element_text(family="sans", size=13))
    })
    

    # Right Side Country Relationship Line Plot
    
    output$rel_coun_pt2 <- renderPlot(
    { 
        # Relationship SideBar Radio Buttons
        
        if (input$rel_sb == "age_mid")
        {
            x_brks = c(20,30,40,50,60)
            x_tks = c("24-","25-34","35-44","45-54","55+")
            x_lab = "Age"
        } 
        else if (input$rel_sb == "edu_num")
        {
            x_brks = c(1,2,3,4,5,6)
            x_tks = c("HS","Some Col","Col Deg","MS","PhD","Prof")
            x_lab = "Education"
        } 
        else if (input$rel_sb == "prof_exp_mid")
        {
            x_brks = c(5,15,25,35)
            x_tks = c("0-10","11-20","21-30","31+")
            x_lab = "Professional Experience"
        } 
        else if (input$rel_sb == "field_exp_mid")
        {
            x_brks = c(5,15,25,35)
            x_tks = c("0-10","11-20","21-30","31+")
            x_lab = "Field Experience"
        }
        
        ggplot(sum_coun_df_rt(), aes(x = .data[[input$rel_sb]], 
                                  color = .data[[input$rib_sb]])) +
        geom_line(aes(y = sum_coun_df_rt()$med, 
                      group = .data[[input$rib_sb]]), size = 1.2) +
        geom_ribbon(aes(ymin = sum_coun_df_rt()$lowerMad, 
                        ymax = sum_coun_df_rt()$upperMad, 
                        group = .data[[input$rib_sb]], 
                        fill = .data[[input$rib_sb]]), 
                        alpha = 0.2, size = 0.3)+
     
        scale_color_manual(name = rib_name(),
                           values = rib_color())+
        scale_fill_manual(name = rib_name(),
                          values = rib_color()) +
        scale_y_continuous(breaks = seq(20,350,20)) +
        scale_x_continuous(breaks = x_brks, labels = x_tks) +
    
        labs(title = tit_rel_coun_rt(),
             x = x_lab,
             y = "Median Compensation (in 1000s of US Dollars)") +
    
        theme_minimal() +
        theme(legend.position = "bottom",
              plot.title = element_text(family="sans", size=16),
              axis.title.x = element_text(family="sans", size=14),
              axis.title.y = element_text(family="sans", size=14),
              axis.text.x = element_text(family="sans", size=13),
              axis.text.y = element_text(family="sans", size=13))
    })

    
    #################################
    ##                             ##
    ##   Ribbon Industry Plots     ##
    ##                             ##
    ################################# 
    
    # Returns data.frame used in ggplot based on
    # Relationshiup Radio Buttons for Industry Line Plots 
    # "rel_ind_rb": rad_ind = c("all","wom","hc","law","not_wom", "tech","eng")
    
    df_rel_ind_lt <- reactive(
    {
        switch(input$rel_ind_sel1, 
               "all" = aam %>% filter(usd_total_comp < input$slide_sb), 
               "wom" = aam %>% filter(usd_total_comp < input$slide_sb & ind_dom == "Woman"), 
               "not_wom" = aam %>% filter(usd_total_comp < input$slide_sb & ind_dom != "Woman"),
               "hc" = aam %>% filter(usd_total_comp < input$slide_sb & industry == "Health care"),
               "law" = aam %>% filter(usd_total_comp < input$slide_sb & industry == "Law"),
               "tech" = aam %>% filter(usd_total_comp < input$slide_sb & industry == "Computing or Tech"),
               "eng" = aam %>% filter(usd_total_comp < input$slide_sb & industry == "Engineering or Manufacturing"))
    })

    df_rel_ind_rt <- reactive(
    {
        switch(input$rel_ind_sel2, 
               "all" = aam %>% filter(usd_total_comp < input$slide_sb), 
               "wom" = aam %>% filter(usd_total_comp < input$slide_sb & ind_dom == "Woman"), 
               "not_wom" = aam %>% filter(usd_total_comp < input$slide_sb & ind_dom != "Woman"),
               "hc" = aam %>% filter(usd_total_comp < input$slide_sb & industry == "Health care"),
               "law" = aam %>% filter(usd_total_comp < input$slide_sb & industry == "Law"),
               "tech" = aam %>% filter(usd_total_comp < input$slide_sb & industry == "Computing or Tech"),
               "eng" = aam %>% filter(usd_total_comp < input$slide_sb & industry == "Engineering or Manufacturing"))
    })

    # Returns title used in ggplot based on
    # Relationshiup Radio Buttons for Industry Line Plots
    # "rel_ind_rb": rad_ind = c("all","wom","hc","law","not_wom", "tech","eng")
    # "rel_sb": rad_rel_sb = c("age_mid","edu_num","prof_exp_mid","field_exp_mid")

    tit_rel_ind_lt <- reactive(
    {
        if (input$rel_sb == "age_mid")
        {
            switch(input$rel_ind_sel1, 
                   "all" = "Total Compensation by Age\nFor All Industries", 
                   "wom" = "Total Compensation by Age\nFor Woman Dominating Industries", 
                   "not_wom" = "Total Compensation by Age\nFor Non Woman Dominating Industries",
                   "hc" = "Total Compensation by Age\nFor Health Care Industries",
                   "law" = "Total Compensation by Age\nFor Law Industries",
                   "tech" = "Total Compensation by Age\nFor Technology Industries",
                   "eng" = "Total Compensation by Age\nFor Engineering Industries")
        }
        else if (input$rel_sb == "edu_num")
        {
            switch(input$rel_ind_sel1, 
                   "all" = "Total Compensation by Education\nFor All Industries", 
                   "wom" = "Total Compensation by Education\nFor Woman Dominating Industries", 
                   "not_wom" = "Total Compensation by Education\nFor Non Woman Dominating Industries",
                   "hc" = "Total Compensation by Education\nFor Health Care Industries",
                   "law" = "Total Compensation by Education\nFor Law Industries",
                   "tech" = "Total Compensation by Education\nFor Technology Industries",
                   "eng" = "Total Compensation by Education\nFor Engineering Industries")
        }
        else if (input$rel_sb == "prof_exp_mid")
        {
            switch(input$rel_ind_sel1, 
                   "all" = "Total Compensation by Professional Experience\nFor All Industries", 
                   "wom" = "Total Compensation by Professional Experience\nFor Woman Dominating Industries", 
                   "not_wom" = "Total Compensation by Professional Experience\nFor Non Woman Dominating Industries",
                   "hc" = "Total Compensation by Professional Experience\nFor Health Care Industries",
                   "law" = "Total Compensation by Professional Experience\nFor Law Industries",
                   "tech" = "Total Compensation by Professional Experience\nFor Technology Industries",
                   "eng" = "Total Compensation by Professional Experience\nFor Engineering Industries")
        }
        else if (input$rel_sb == "field_exp_mid")
        {
            switch(input$rel_ind_sel1, 
                   "all" = "Total Compensation by Field Experience\nFor All Industries", 
                   "wom" = "Total Compensation by Field Experience\nFor Woman Dominating Industries", 
                   "not_wom" = "Total Compensation by Field Experience\nFor Non Woman Dominating Industries",
                   "hc" = "Total Compensation by Field Experience\nFor Health Care Industries",
                   "law" = "Total Compensation by Field Experience\nFor Law Industries",
                   "tech" = "Total Compensation by Field Experience\nFor Technology Industries",
                   "eng" = "Total Compensation by Field Experience\nFor Engineering Industries")
        }
    })

    tit_rel_ind_rt <- reactive(
    {
        if (input$rel_sb == "age_mid")
        {
            switch(input$rel_ind_sel2, 
                   "all" = "Total Compensation by Age\nFor All Industries", 
                   "wom" = "Total Compensation by Age\nFor Woman Dominating Industries", 
                   "not_wom" = "Total Compensation by Age\nFor Non Woman Dominating Industries",
                   "hc" = "Total Compensation by Age\nFor Health Care Industries",
                   "law" = "Total Compensation by Age\nFor Law Industries",
                   "tech" = "Total Compensation by Age\nFor Technology Industries",
                   "eng" = "Total Compensation by Age\nFor Engineering Industries")
        }
        else if (input$rel_sb == "edu_num")
        {
            switch(input$rel_ind_sel2, 
                   "all" = "Total Compensation by Education\nFor All Industries", 
                   "wom" = "Total Compensation by Education\nFor Woman Dominating Industries", 
                   "not_wom" = "Total Compensation by Education\nFor Non Woman Dominating Industries",
                   "hc" = "Total Compensation by Education\nFor Health Care Industries",
                   "law" = "Total Compensation by Education\nFor Law Industries",
                   "tech" = "Total Compensation by Education\nFor Technology Industries",
                   "eng" = "Total Compensation by Education\nFor Engineering Industries")
        }
        else if (input$rel_sb == "prof_exp_mid")
        {
            switch(input$rel_ind_sel2, 
                   "all" = "Total Compensation by Professional Experience\nFor All Industries", 
                   "wom" = "Total Compensation by Professional Experience\nFor Woman Dominating Industries", 
                   "not_wom" = "Total Compensation by Professional Experience\nFor Non Woman Dominating Industries",
                   "hc" = "Total Compensation by Professional Experience\nFor Health Care Industries",
                   "law" = "Total Compensation by Professional Experience\nFor Law Industries",
                   "tech" = "Total Compensation by Professional Experience\nFor Technology Industries",
                   "eng" = "Total Compensation by Professional Experience\nFor Engineering Industries")
        }
        else if (input$rel_sb == "field_exp_mid")
        {
            switch(input$rel_ind_sel2, 
                   "all" = "Total Compensation by Field Experience\nFor All Industries", 
                   "wom" = "Total Compensation by Field Experience\nFor Woman Dominating Industries", 
                   "not_wom" = "Total Compensation by Field Experience\nFor Non Woman Dominating Industries",
                   "hc" = "Total Compensation by Field Experience\nFor Health Care Industries",
                   "law" = "Total Compensation by Field Experience\nFor Law Industries",
                   "tech" = "Total Compensation by Field Experience\nFor Technology Industries",
                   "eng" = "Total Compensation by Field Experience\nFor Engineering Industries")
        }
    })


    # Create industry summary function for left plot
     
    sum_ind_df_lt <- reactive(
    {
        df <- df_rel_ind_lt()
        df$.data[[input$rib_sb]] <- factor(df$.data[[input$rib_sb]])
         
        df %>%
            group_by(.data[[input$rib_sb]], .data[[input$rel_sb]]) %>%
            summarize(n = n(),
                med = quantile(usd_total_comp)[3],
                mad = mad(usd_total_comp),
                lowerMad = med - (sqrt(mad)/2),
                upperMad = med + (sqrt(mad)/2))
    })

    # Create industry summary function for right plot
     
    sum_ind_df_rt <- reactive(
    {
        df <- df_rel_ind_rt()
        df$.data[[input$rib_sb]] <- factor(df$.data[[input$rib_sb]])
         
        df %>%
            group_by(.data[[input$rib_sb]], .data[[input$rel_sb]]) %>%
            summarize(n = n(),
                med = quantile(usd_total_comp)[3],
                mad = mad(usd_total_comp),
                lowerMad = med - (sqrt(mad)/2),
                upperMad = med + (sqrt(mad)/2))
    })

    
    # Left Side Industry Relationship Line Plot
    
    output$rel_ind_pt1 <- renderPlot(
    { 
        # Relationship SideBar Radio Buttons
        
        if (input$rel_sb == "age_mid")
        {
            x_brks = c(20,30,40,50,60)
            x_tks = c("24-","25-34","35-44","45-54","55+")
            x_lab = "Age"
        } 
        else if (input$rel_sb == "edu_num")
        {
            x_brks = c(1,2,3,4,5,6)
            x_tks = c("HS","Some Col","Col Deg","MS","PhD","Prof")
            x_lab = "Education"
        } 
        else if (input$rel_sb == "prof_exp_mid")
        {
            x_brks = c(5,15,25,35)
            x_tks = c("0-10","11-20","21-30","31+")
            x_lab = "Professional Experience"
        } 
        else if (input$rel_sb == "field_exp_mid")
        {
            x_brks = c(5,15,25,35)
            x_tks = c("0-10","11-20","21-30","31+")
            x_lab = "Field Experience"
        }
        
        ggplot(sum_ind_df_lt(), aes(x = .data[[input$rel_sb]], 
                                 color = .data[[input$rib_sb]])) +
        geom_line(aes(y = sum_ind_df_lt()$med, 
                  group = .data[[input$rib_sb]]), size = 1.2) +
        geom_ribbon(aes(ymin = sum_ind_df_lt()$lowerMad, 
                        ymax = sum_ind_df_lt()$upperMad, 
                        group = .data[[input$rib_sb]], 
                        fill = .data[[input$rib_sb]]), 
                        alpha = 0.2, size = 0.3)+
     
        scale_color_manual(name = rib_name(),
                           values = rib_color())+
        scale_fill_manual(name = rib_name(),
                          values = rib_color()) +
        scale_y_continuous(breaks = seq(20,350,20)) +
        scale_x_continuous(breaks = x_brks, labels = x_tks) +
    
        labs(title = tit_rel_ind_lt(),
             x = x_lab,
             y = "Median Compensation (in 1000s of US Dollars)") +
    
        theme_minimal() +
        theme(legend.position = "bottom",
              plot.title = element_text(family="sans", size=16),
              axis.title.x = element_text(family="sans", size=14),
              axis.title.y = element_text(family="sans", size=14),
              axis.text.x = element_text(family="sans", size=13),
              axis.text.y = element_text(family="sans", size=13))
    })

    # Right Side Industry Relationship Line Plot
    
    output$rel_ind_pt2 <- renderPlot(
    { 
        # Relationship SideBar Radio Buttons
        
        if (input$rel_sb == "age_mid")
        {
            x_brks = c(20,30,40,50,60)
            x_tks = c("24-","25-34","35-44","45-54","55+")
            x_lab = "Age"
        } 
        else if (input$rel_sb == "edu_num")
        {
            x_brks = c(1,2,3,4,5,6)
            x_tks = c("HS","Some Col","Col Deg","MS","PhD","Prof")
            x_lab = "Education"
        } 
        else if (input$rel_sb == "prof_exp_mid")
        {
            x_brks = c(5,15,25,35)
            x_tks = c("0-10","11-20","21-30","31+")
            x_lab = "Professional Experience"
        } 
        else if (input$rel_sb == "field_exp_mid")
        {
            x_brks = c(5,15,25,35)
            x_tks = c("0-10","11-20","21-30","31+")
            x_lab = "Field Experience"
        }
        
        ggplot(sum_ind_df_rt(), aes(x = .data[[input$rel_sb]], 
                                 color = .data[[input$rib_sb]])) +
        geom_line(aes(y = sum_ind_df_rt()$med, 
                      group = .data[[input$rib_sb]]), size = 1.2) +
        geom_ribbon(aes(ymin = sum_ind_df_rt()$lowerMad, 
                        ymax = sum_ind_df_rt()$upperMad, 
                        group = .data[[input$rib_sb]], 
                        fill = .data[[input$rib_sb]]), 
                        alpha = 0.2, size = 0.3)+
     
        scale_color_manual(name = rib_name(),
                           values = rib_color())+
        scale_fill_manual(name = rib_name(),
                          values = rib_color()) +
        scale_y_continuous(breaks = seq(20,350,20)) +
        scale_x_continuous(breaks = x_brks, labels = x_tks) +
    
        labs(title = tit_rel_ind_rt(),
             x = x_lab,
             y = "Median Compensation (in 1000s of US Dollars)") +
    
        theme_minimal() +
        theme(legend.position = "bottom",
              plot.title = element_text(family="sans", size=16),
              axis.title.x = element_text(family="sans", size=14),
              axis.title.y = element_text(family="sans", size=14),
              axis.text.x = element_text(family="sans", size=13),
              axis.text.y = element_text(family="sans", size=13))
    })
}

# Run the application 

shinyApp(ui = ui, server = server)
