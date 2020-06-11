if(!exists("eval_mat_soc")) source("intro.R")

# Necessary script to keep track of when the return button
# is pressed.
js <- '
$(document).on("keyup", function(e) {
  if(e.keyCode == 13){
    Shiny.onInputChange("keyPressed", Math.random());
  }
});
'

# Text that are repeatedly used in the multi-criteria analysis tabs.
criteriaText <- "Below are the criterion measures used for our analysis:"
stdResText <- paste("Using the mean values for all the criterion measures",
                    "we obtain the following ranking:")
mcResText <- paste("To measure the robustness of the results above,",
                   "we sampled the values of our impact matrix",
                   params$n_sample, "times:")
# Text used in the stakeholder tab.
analysisText <- paste("Pressing the button below will generate a dendrogram of stakeholders",
                      "according to their alternative preferences. It will also generate",
                      "3 different tables of rankings: one where all stakeholders are considered",
                      "equal, one where the stakeholders' interest is taken into account, and",
                      "one that examines preferences by coalition (as determined by the dendrogram).")
shRankText_w <- "Ranking with stakeholder interest/stake as weights:"
shRankText_ew <- "Ranking with equal weights:"

shinyUI(fluidPage(
  
    useShinyjs(),
    tags$script(js),

    titlePanel("SMCE Tool for the SONGS Case Study"),

    tabsetPanel(
      tabPanel(
        "Background",
        includeMarkdown("background.md")
      ),
      
      tabPanel(
        "Method",
        includeMarkdown("method.md")
      ),
      
      tabPanel(
        ifelse(is.na(params$tabname[1]), "MCE1", params$tabname[1]),
        br(),
        p(criteriaText),
        br(),
        DT::dataTableOutput("criteria1"),
        br(),
        p(stdResText),
        br(),
        tableOutput("stdRes1"),
        br(),
        p(mcResText[1]),
        downloadButton(outputId = "savePlot1", label = "Save plot"),
        plotOutput("mcBoxplot1")
      ),
        
      tabPanel(
        ifelse(is.na(params$tabname[2]), "MCE2", params$tabname[2]),
        br(),
        p(criteriaText),
        br(),
        DT::dataTableOutput("criteria2"),
        br(),
        p(stdResText),
        br(),
        tableOutput("stdRes2"),
        br(),
        p(mcResText[2]),            
        downloadButton(outputId = "savePlot2", label = "Save plot"),
        plotOutput("mcBoxplot2")
      ),
      
      tabPanel(
        ifelse(is.na(params$tabname[3]), "MCE3", params$tabname[3]),
        br(),
        p(criteriaText),
        br(),
        DT::dataTableOutput("criteria3"),
        br(),
        p(stdResText),
        br(),
        tableOutput("stdRes3"),
        br(),
        p(mcResText[3]),            
        downloadButton(outputId = "savePlot3", label = "Save plot"),
        plotOutput("mcBoxplot3")
      ),
               
      tabPanel(
        params_sia$tabname,
        br(),
        DT::dataTableOutput("shTable"),
        br(),
        p(analysisText),
        br(),
        actionButton("runShAnalysis", "Run Analysis"),
        br(),
        plotOutput("shDendrogram"),
        br(),
        hidden(p(id="shRankText_w", shRankText_w)),
        tableOutput("shRank_w"),
        hidden(p(id="shRankText_ew", shRankText_ew)),
        tableOutput("shRank_ew"),
        hidden(
          textInput("dendrogramGroups",
                    label="Enter number of groups in dendrogram to analyze:")
        ),
        htmlOutput("groupText"),
        DT::dataTableOutput("socialRank_groups")
        # tableOutput("socialRank_groups")
      )
    )
))
