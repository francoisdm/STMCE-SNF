library(shiny)

shinyServer(function(input, output) {
    
    # ==== Reactive values ====
    r <- reactiveValues()
    r$nActors <- reactive({nrow(r$actorTable)})
    r$actorTable <- actorTable
    r$stakeholders <- stakeholders %>%
        rename(`Influence/Power` = y,
               `Interest/Stake` = x)
    
    # ==== Display criteria 1 ====
    output$criteriaText <- renderText({
        paste("Below are the criterion measures used for our analysis:")
    })
    
    output$criteria1 <- DT::renderDataTable({
        DT::datatable(criteria[[1]] %>% select(Category, Criterion),
                      rownames=F,
                      options= list(pageLength=10))
    })    
    
    output$criteria2 <- DT::renderDataTable({
        DT::datatable(criteria[[2]] %>% select(Category, Criterion),
                      rownames=F,
                      options= list(pageLength=10))
    })        
    
    output$criteria3 <- DT::renderDataTable({
        DT::datatable(criteria[[3]] %>% select(Category, Criterion),
                      rownames=F,
                      options= list(pageLength=10))
    })        

    # ==== Technical standard results ====
    output$stdResText <- renderText({
        paste("Using the mean values for all the criterion measures,",
              "we obtain the following alternative ranking:")
    })
    
    output$standardResults1 <- renderTable({
        simulateMC(1, impactMatrix[[1]], criteria[[1]], mean = T)$ranking[1,]
    })
    
    output$standardResults2 <- renderTable({
        simulateMC(2, impactMatrix[[2]], criteria[[2]], mean = T)$ranking[1,]
    })    

    output$standardResults3 <- renderTable({
        simulateMC(3, impactMatrix[[3]], criteria[[3]], mean = T)$ranking[1,]
    })    
        
    # ==== MC simulation on technical impact matrix ====
    output$mcResText <- renderText({
        paste("To measure the robustness of the standard result,",
              "we sampled the values of our impact matrix 300 different times.")
    })
    
    mcBoxplot <- function(i){
        ytick<-seq(1, nrow(alternatives), by=1)
        boxplot(simulations[[i]]$ranking, yaxt='n')
        # title("Alternative Ranking Boxplot (300 Simulations)")
        axis(side=2, at=ytick, labels = 1:nrow(alternatives))
    }    
    
    output$downloadPlot1 <- downloadHandler(
        filename = function() {
            paste('MC_boxplot.png')
        },
        content = function(file) {
            png(file, width=1920)
            mcBoxplot(1)
            dev.off()
        }
    )
    
    output$mcBoxplot1 <- renderPlot({
        mcBoxplot(1)
    })
    
    output$downloadPlot2 <- downloadHandler(
        filename = function() {
            paste('MC_boxplot.png')
        },
        content = function(file) {
            png(file, width=1920)
            mcBoxplot(2)
            dev.off()
        }
    )
    
    output$mcBoxplot2 <- renderPlot({
        mcBoxplot(2)
    })    
    
    output$downloadPlot3 <- downloadHandler(
        filename = function() {
            paste('MC_boxplot.png')
        },
        content = function(file) {
            png(file, width=1920)
            mcBoxplot(3)
            dev.off()
        }
    )
    
    output$mcBoxplot3 <- renderPlot({
        mcBoxplot(3)
    })  

    # ==== Actor dendogram ====
    output$actorText <- renderText({
        paste("Using actor preferences on the alternatives, we group actors together by",
              "computing the semantic distance between pairwise preferences.",
              "Note that actor preferences for now have been randomly generated.")
    })
    
    output$actorTable <- DT::renderDataTable({
        DT::datatable(actorTable,
                      options = list(pageLength = 40,
                                     dom = 't'),
                      rownames = FALSE,
                      editable = F)
    })
    
    output$actorDendrogram <- renderPlot({
        # rownames(impactSocial) <- actorTable$Stakeholder
        hc <- hclust(as.dist(impactSocial), method="complete")
        plot(hc, xlab="Actor ID")
    })    
    
    observeEvent(input$addSocialActor, {
        showModal(modalDialog(
            textInput(inputId = "actorNameInput", label = "Actor Name", placeholder = "Name of social actor"),
            selectInput(inputId = "bauInput", label = "BAU Rank", choices = rankOptions),
            selectInput(inputId = "interimInput", label = "INTERIM Rank", choices = rankOptions),
            selectInput(inputId = "directInput", label = "DIRECT Rank", choices = rankOptions),
            selectInput(inputId = "indirectInput", label = "INDIRECT Rank", choices = rankOptions),
            title = "Add Social Actor",
            footer = tagList(
                actionButton("saveSocialActor", "Save"),
                modalButton("Cancel")
            ),
            easyClose = FALSE
        ))
    })
    
    observeEvent(input$saveSocialActor, {
        req(input$actorNameInput)
        
        actor <- input$actorNameInput
        bau <- input$bauInput
        interim <- input$interimInput
        direct <- input$directInput
        indirect <- input$indirectInput
        
        r$actorTable <- r$actorTable %>%
            rbind(data.frame(
                Actor = actor, 
                X1 = bau, 
                X2 = interim, 
                X3 = direct, 
                X4 = indirect))
        
        removeModal()
    })    
    
    # ====== Stakeholder mapping ======   
    output$stakeholderText <- renderText({
        paste("Given the default stakes of the actors, we would obtain the result below.",
              "Edit the stakeholder map below to change the weight distribution and possibly the rankings.")
    })
    
    # Get ranking
    getActorImpact <- function(i) {
        prefs <- r$actorTable[i,] %>%
            select(-ID, -Stakeholder)
        
        prefs <- sapply(prefs, function(i) linguisticOrder[[i]])
        
        d <- nrow(alternatives)
        E <- data.frame(matrix(nrow = d, 
                               ncol = d,
                               dimnames = list(alternatives$Option, alternatives$Option)))
        for (i in 1:length(prefs)) {
            curPref <- prefs[[names(prefs)[i]]]
            p <- ifelse(curPref > prefs, 1,
                        ifelse(curPref == prefs, 0.5, 0))
            E[i,] <- p
            E[i,i] <- 0
        }
        E*r$stakeholders$weight[i]
    }        
    
    E <- eventReactive(input$updateSocialRank,{
        add(lapply(1:nrow(r$actorTable), getActorImpact))
    })
    
    output$socialRank <- renderTable({
        E <- E()
        -E %>%
            t %>%
            colSums %>%
            rank %>%
            as.factor %>%
            data.frame %>%
            t
    })
    
    # Interactive stakeholder map
    output$stakeholderMap <- renderPlotly({
        
        xa <- list(title="Influence/Power",
                   range=c(0,100),
                   fixedrange=T,
                   showticklabels=F)        
        ya <- list(title="Interest/Stake",
                   range=c(0, 100),
                   fixedrange=T,
                   showticklabels=F)
        
        circles <- map2(
            r$stakeholders$`Influence/Power`, 
            r$stakeholders$`Interest/Stake`, 
            ~list(
                type = "circle",
                # anchor circles at (x, y)
                xanchor = .x,
                yanchor = .y,
                # give each circle a 2 pixel diameter
                x0 = -3, x1 = 3,
                y0 = -3, y1 = 3,
                xsizemode = "pixel", 
                ysizemode = "pixel",
                # other visual properties
                fillcolor = "blue",
                line = list(color = "transparent")
            )
        )
        
        plot_ly() %>%
            add_trace(
                type="scatter",
                x=~r$stakeholders$`Influence/Power`,
                y=~r$stakeholders$`Interest/Stake`,
                mode="text",
                text=~r$stakeholders$ID,
                textposition="middle right"
            ) %>%
            layout(title="Stakeholder Map",
                   xaxis=xa,
                   yaxis=ya,
                   shapes = circles) %>%
            config(edits = list(shapePosition = TRUE),
                   displayModeBar=F)
    })
    
    # Update actor weights after making edits to stakeholder map
    observeEvent(event_data("plotly_relayout"), {
        event <- event_data("plotly_relayout")
        if (!is.null(event) & all(grepl("anchor", names(event)))) {
            rownum <- parse_number(names(event)[1]) + 1
            newCoord <- sapply(5*round(as.numeric(event)/5), max, 0)
            r$stakeholders$`Influence/Power`[rownum] <- newCoord[1]
            r$stakeholders$`Interest/Stake`[rownum] <- newCoord[2]
        }
    })
    
    # Show table of actors
    getWeight <- function(a) {
        aStake <- r$stakeholders %>%
            filter(ID == a) %>%
            .$`Interest/Stake`
        r$stakeholders %>%
            filter(ID != a) %>%
            mutate(`Interest/Stake` = aStake - `Interest/Stake`) %>%
            summarise(weight = mean(`Interest/Stake`)) %>%
            mutate(weight = weight/100 + 1) %>%
            .$weight
    }    
    
    output$stakeholderTable <- DT::renderDataTable({
        r$stakeholders$weight <- sapply(r$stakeholders$ID, getWeight)
        DT::datatable(r$stakeholders %>%
                         mutate(weight = round(weight, 2)),
                      rownames=F,
                      editable="cell",
                      # editable=list(
                      #     target="row",
                      #     disable=list(columns=c(1,4))
                      # ),
                      options=list(
                          pageLength=20,
                          # dom='t',
                          columnDefs=list(list(visible=F,targets=c(0)))
                      ))
    })
    # observe(str(input$stakeholderTable_cell_edit))
    
    observeEvent(input$stakeholderTable_cell_edit, {
        info <- input$stakeholderTable_cell_edit
        r$stakeholders[info$row, (info$col+1)] <- as.numeric(info$value)
        print(r$stakeholders)
        # str(info)
    })

})
