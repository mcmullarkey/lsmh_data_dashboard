#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Loading packages

library(shiny) 
library(tidyverse)
library(glue)
library(janitor)

# Importing the data

init_data <- read_rds("lsmh_dash_data.rds")

# Notes as of right now: All parents in Empower are given age of 18 since the age variable seems to be missing. 
# For original YES data collection all participants were given the minimum age of the age range they selected (Ex. "11-13" would be 11)

# Here's a screening function necessary for the both main functions to work

make_screen <- function(screen_var){
    
    vec_var <- enframe(screen_var) %>% 
        mutate(row = row_number()) %>% 
        mutate(weight = case_when(
            max(row) == 1 & row == 1 ~ 2,
            max(row) == 2 & row == 1 ~ 1,
            max(row) == 2 & row == 2 ~ 1)) %>%
        uncount() %>% 
        dplyr::select(value) %>% 
        deframe()
    
}

create_scatter <- function(.data, min_age, max_age, race_var, sgm_var, sitb_var, qual_var, rand_var, non_rand_var, rec_ssi_var, self_var, parent_var){
    
    screening <- list(race_pipe = {{race_var}}, sgm_pipe = {{sgm_var}}, sitb_pipe = {{sitb_var}}, qual_pipe = {{qual_var}}, rand_pipe = {{rand_var}},
                      non_rand_pipe = {{non_rand_var}}, rec_ssi_pipe = {{rec_ssi_var}}, self_pipe = {{self_var}}, parent_pipe = {{parent_var}})
    
    multi_ex <- map_dfc(screening, make_screen)
    
    data_highlight <- .data %>% 
        filter(age >= {{min_age}} & age <= {{max_age}},
               sgm == deframe(multi_ex[1,"sgm_pipe"]) | sgm == deframe(multi_ex[2,"sgm_pipe"]),
               race_minoritized == deframe(multi_ex[1,"race_pipe"]) | race_minoritized == deframe(multi_ex[2,"race_pipe"]),
               sitb_assess == deframe(multi_ex[1,"sitb_pipe"]) | sitb_assess == deframe(multi_ex[2,"sitb_pipe"]),
               qual_feed == deframe(multi_ex[1,"qual_pipe"]) | qual_feed == deframe(multi_ex[2,"qual_pipe"]),
               randomized == deframe(multi_ex[1,"rand_pipe"]) | randomized == deframe(multi_ex[2,"rand_pipe"]),
               non_rand_treat == deframe(multi_ex[1,"non_rand_pipe"]) | non_rand_treat == deframe(multi_ex[2,"non_rand_pipe"]),
               rec_ssi == deframe(multi_ex[1,"rec_ssi_pipe"]) | rec_ssi == deframe(multi_ex[2,"rec_ssi_pipe"]),
               self_report == deframe(multi_ex[1,"self_pipe"]) | self_report == deframe(multi_ex[2,"self_pipe"]),
               parent_report == deframe(multi_ex[1,"parent_pipe"]) | parent_report == deframe(multi_ex[2,"parent_pipe"]))
    
    .data %>% 
        ggplot(aes(x=id,y=rand_id)) + 
        geom_jitter(alpha=0.3) +
        geom_point(data=data_highlight, 
                   aes(x=id,y=rand_id), 
                   color='blue', alpha = 0.6) +
        labs(x = "Participant ID", y = "Random Number (For Plotting)")
    
}

## And now sample size

get_n <- function(.data, min_age, max_age, race_var, sgm_var, sitb_var, qual_var, rand_var, non_rand_var, rec_ssi_var, self_var, parent_var){
    
    screening <- list(race_pipe = {{race_var}}, sgm_pipe = {{sgm_var}}, sitb_pipe = {{sitb_var}}, qual_pipe = {{qual_var}}, rand_pipe = {{rand_var}},
                      non_rand_pipe = {{non_rand_var}}, rec_ssi_pipe = {{rec_ssi_var}}, self_pipe = {{self_var}}, parent_pipe = {{parent_var}})
    
    multi_ex <- map_dfc(screening, make_screen)
    
    data_n <- .data %>% 
        filter(age >= {{min_age}} & age <= {{max_age}},
               sgm == deframe(multi_ex[1,"sgm_pipe"]) | sgm == deframe(multi_ex[2,"sgm_pipe"]),
               race_minoritized == deframe(multi_ex[1,"race_pipe"]) | race_minoritized == deframe(multi_ex[2,"race_pipe"]),
               sitb_assess == deframe(multi_ex[1,"sitb_pipe"]) | sitb_assess == deframe(multi_ex[2,"sitb_pipe"]),
               qual_feed == deframe(multi_ex[1,"qual_pipe"]) | qual_feed == deframe(multi_ex[2,"qual_pipe"]),
               randomized == deframe(multi_ex[1,"rand_pipe"]) | randomized == deframe(multi_ex[2,"rand_pipe"]),
               non_rand_treat == deframe(multi_ex[1,"non_rand_pipe"]) | non_rand_treat == deframe(multi_ex[2,"non_rand_pipe"]),
               rec_ssi == deframe(multi_ex[1,"rec_ssi_pipe"]) | rec_ssi == deframe(multi_ex[2,"rec_ssi_pipe"]),
               self_report == deframe(multi_ex[1,"self_pipe"]) | self_report == deframe(multi_ex[2,"self_pipe"]),
               parent_report == deframe(multi_ex[1,"parent_pipe"]) | parent_report == deframe(multi_ex[2,"parent_pipe"]))
    
    n_from_df <- data_n %>% 
        tally() %>% 
        deframe()
    
    glue_string <- glue("There are {n_from_df} participants across all datasets who meet these inclusion criteria.")
    
    glue_string
}

# ui <- fluidPage(
#     fluidRow(
#         column(4,
#                "Frequency polygon",
#                sliderInput("minimum_age", label = "Minimum Age", value = c(13, 16), min = 13, max = 16),
#                sliderInput("maximum_age", label = "Maximum Age", value = c(13, 16), min = 13, max = 16)
#         )
#     ),
#     fluidRow(
#         column(1, plotOutput("scatter")),
#         column(3, verbatimTextOutput("n"))
#     )
# )

ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "darkly"),
    titlePanel("LSMH Data (Blue Represents Included Participants)"),
    sidebarLayout(
        sidebarPanel(
            numericInput("minimum_age", "Minimum Age", 13, min = 9, max = 85),
            numericInput("maximum_age", "Maximum Age", 14, min = 9, max = 85),
            checkboxGroupInput("race_included", 
                               h3("Race"), 
                               choices = c("BIPOC Participants" = "Yes", 
                                              "White Participants" = "No"),
                               selected = c("Yes","No")),
            checkboxGroupInput("sgm_included", 
                               h3("Sexual and Gender Identity"), 
                               choices = c("SGM Participants" = "Yes", 
                                              "Non-SGM Participants" = "No"),
                               selected = c("Yes","No")),
            checkboxGroupInput("sitbs_assessed", 
                                h3("SITBs Assessed"), 
                                choices = c("SITBs Assessed" = "Yes", 
                                               "No SITBs Assessed" = "No"),
                                                selected = c("Yes","No")),
            checkboxGroupInput("rand_to_treat", 
                               h3("Randomized to Treatment?"), 
                               choices = c("Yes" = "Yes", 
                                              "No" = "No"),
                               selected = c("Yes","No")),
            checkboxGroupInput("rec_treat", 
                               h3("Received Treatment (Non-Randomized)?"), 
                               choices = c("Yes" = "Yes", 
                                              "No" = "No"),
                               selected = c("Yes","No")),
            checkboxGroupInput("rec_any_ssi", 
                               h3("Received Treatment (Randomized and Non-Randomized)?"), 
                               choices = c("Yes" = "Yes", 
                                              "No" = "No"),
                               selected = c("Yes","No")),
            checkboxGroupInput("has_self_report", 
                               h3("Has Self-Report Data?"), 
                               choices = c("Yes" = "Yes", 
                                              "No" = "No"),
                               selected = c(1,2)),
            checkboxGroupInput("has_parent_report", 
                               h3("Has Parent-Report Data?"), 
                               choices = c("Yes" = "Yes", 
                                              "No" = "No"),
                               selected = c("Yes","No")),
        ),
        mainPanel(
            plotOutput("scatter"),
            textOutput("n")
        )
    )
)

server <- function(input, output, session) {
    
    thematic::thematic_shiny()
    
    output$scatter <- renderPlot({
        
        create_scatter(init_data, min_age = input$minimum_age, max_age = input$maximum_age)
    }, res = 96)
    
    output$n <- renderText({
        
        get_n(init_data, min_age = input$minimum_age, max_age = input$maximum_age)
    })
}

shinyApp(ui, server)
