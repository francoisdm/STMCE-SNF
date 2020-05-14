if(!exists("alternatives")) source("intro.R")

js <- '
$(document).on("keyup", function(e) {
  if(e.keyCode == 13){
    Shiny.onInputChange("keyPressed", Math.random());
  }
});
'

shinyUI(fluidPage(
  
    useShinyjs(),
    
    tags$script(js),

    titlePanel("SMCE Tool for the SONGS Case Study"),

    tabsetPanel(
        tabPanel(
            parameters$tabname[1],
            br(),
            p("Below are the criterion measures used for our analysis:"),
            # textOutput("criteriaText"),
            br(),
            DT::dataTableOutput("criteria1"),
            br(),
            # textOutput("stdResText"),
            p(paste("Using the mean values for all the criterion measures,",
                    "we obtain the following alternative ranking:")),
            br(),
            tableOutput("standardResults1"),
            br(),
            # textOutput("mcResText"),
            p(paste("To measure the robustness of the standard result,",
                  "we sampled the values of our impact matrix", 
                  parameters$n_sample[1],
                  "different times.")),
            downloadButton(outputId = "downloadPlot1", label = "Save plot"),
            plotOutput("mcBoxplot1")
        ),
        
        tabPanel(
            parameters$tabname[2],
            br(),
            p("Below are the criterion measures used for our analysis:"),
            # textOutput("criteriaText"),
            br(),
            DT::dataTableOutput("criteria2"),
            br(),
            # textOutput("stdResText"),
            p(paste("Using the mean values for all the criterion measures,",
                  "we obtain the following alternative ranking:")),
            br(),
            tableOutput("standardResults2"),
            br(),
            # textOutput("mcResText"),
            p(paste("To measure the robustness of the standard result,",
                    "we sampled the values of our impact matrix", 
                    parameters$n_sample[2],
                    "different times.")),            
            downloadButton(outputId = "downloadPlot2", label = "Save plot"),
            plotOutput("mcBoxplot2")
        ),

         tabPanel(
            parameters$tabname[3],
            br(),
            p("Below are the criterion measures used for our analysis:"),
            # textOutput("criteriaText"),
            br(),
            DT::dataTableOutput("criteria3"),
            br(),
            # textOutput("stdResText"),
            p(paste("Using the mean values for all the criterion measures,",
                  "we obtain the following alternative ranking:")),
            br(),
            tableOutput("standardResults3"),
            br(),
            # textOutput("mcResText"),
            p(paste("To measure the robustness of the standard result,",
                    "we sampled the values of our impact matrix", 
                    parameters$n_sample[3],
                    "different times.")),            
            downloadButton(outputId = "downloadPlot3", label = "Save plot"),
            plotOutput("mcBoxplot3")
        ),
               
        tabPanel(
            "Stakeholders",
            br(),
            DT::dataTableOutput("stakeholderTable"),
            br(),
            textOutput("actorText"),
            br(),
            actionButton("updateSocialRank", "Run Analysis"),
            br(),
            DT::dataTableOutput("actorTable", width = "75%"),
            plotOutput("actorDendrogram"),
            br(),
            # textOutput("stakeholderText"),
            # br(),
            textOutput("socialRankText"),
            tableOutput("socialRank"),
            textOutput("socialRank_noWeightText"),
            tableOutput("socialRank_noWeight"),
            shinyjs::hidden(
              textInput("dendrogramGroups",
                        label="Enter number of groups in dendrogram to analyze:")
            ),
            htmlOutput("groupText"),
            tableOutput("socialRank_groups")
            # plotlyOutput("stakeholderMap"),
        )
    )
))
