shinyServer(function(input, output) {
    
    # ==== Criterion tables ====
    output$criteria1 <- DT::renderDataTable({
        DT::datatable(
            criteria[[1]] %>% 
                dplyr::select(Category, Criterion),
            rownames=F,
            options= list(pageLength=10)
        )
    })    
    
    output$criteria2 <- DT::renderDataTable({
        DT::datatable(
            criteria[[2]] %>% 
                dplyr::select(Category, Criterion),
            rownames=F,
            options= list(pageLength=10)            
        )
    })        
    
    output$criteria3 <- DT::renderDataTable({
        DT::datatable(
            criteria[[3]] %>% 
                dplyr::select(Category, Criterion),
            rownames=F,
            options= list(pageLength=10)            
        )
    })        

    # ==== Standard results ====
    output$stdRes1 <- renderTable({
        simulateMC(1, impactMatrix[[1]], criteria[[1]], mean = T)$ranking[1,]
    })
    
    output$stdRes2 <- renderTable({
        simulateMC(2, impactMatrix[[2]], criteria[[2]], mean = T)$ranking[1,]
    })    

    output$stdRes3 <- renderTable({
        simulateMC(3, impactMatrix[[3]], criteria[[3]], mean = T)$ranking[1,]
    })    
        
    # ==== Monte Carlo simulation results ====
    output$savePlot1 <- downloadHandler(
        filename = function() {
            paste('MC_boxplot1.png')
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
    
    output$savePlot2 <- downloadHandler(
        filename = function() {
            paste('MC_boxplot2.png')
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
    
    output$savePlot3 <- downloadHandler(
        filename = function() {
            paste('MC_boxplot3.png')
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

    # ==== Stakeholder dendogram ====
    r <- reactiveValues()
    r$nActors <- reactive({nrow(r$equityImpact)})
    r$equityImpact <- equityImpact
    r$stakeholders <- stakeholders %>%
        rename(`Influence/Power` = y,
               `Interest/Stake` = x)
    
    socImpact <- eventReactive(input$runShAnalysis, {
        if (is.null(socialImpact)) {
            socImpact <- getSocialImpact()
            write_csv(data.frame(socImpact), "social-impact.csv", col_names=F)
        } else {
            socImpact <- socialImpact
        }
        socImpact
    })
    
    dendrogram <- reactive({
        hclust(as.dist(1 - socImpact()), method="complete")
    })
    
    output$shDendrogram <- renderPlot({
        plot(dendrogram(), xlab="Actor ID")
    }) 
    
    # Get ranking
    getActorImpact <- function(i, weight=T) {
        prefs <- r$equityImpact[i,] %>%
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
        if (weight) {
            E*r$stakeholders$weight[i]
        } else {
            E
        }
    }        
    
    E <- eventReactive(input$runShAnalysis,{
        add(lapply(1:nrow(r$stakeholders), getActorImpact))
    })
    
    E2 <- eventReactive(input$runShAnalysis,{
        add(lapply(1:nrow(r$stakeholders), getActorImpact, weight=FALSE))
    })
    
    observeEvent(input$runShAnalysis, {
        shinyjs::show("dendrogramGroups")
        shinyjs::show("shRankText_w")
        shinyjs::show("shRankText_ew")
    })
    
    output$shRank_w <- renderTable({
        E <- E()
        -E %>%
            t %>%
            colSums %>%
            rank %>%
            as.factor %>%
            data.frame %>%
            t
    })
    
    output$shRank_ew <- renderTable({
        E2 <- E2()
        -E2 %>%
            t %>%
            colSums %>%
            rank %>%
            as.factor %>%
            data.frame %>%
            t
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
    
    output$shTable <- DT::renderDataTable({
        r$stakeholders$weight <- sapply(r$stakeholders$ID, getWeight)
        DT::datatable(r$stakeholders %>%
                         mutate(weight = round(weight, 2)),
                      rownames=F,
                      editable="cell",
                      options=list(
                          pageLength=20,
                          columnDefs=list(list(visible=F,targets=c(0, 4)))
                      ))
    })
    
    observeEvent(input$shTable_cell_edit, {
        info <- input$shTable_cell_edit
        r$stakeholders[info$row, (info$col+1)] <- as.numeric(info$value)
    })
    
    g <- reactiveVal()
    groups <- reactiveVal()
    groupText <- reactiveVal()
    
    observeEvent(input[["keyPressed"]], {
        g(input$dendrogramGroups)
        groups(cutree(dendrogram(), k = input$dendrogramGroups))
    })
    
    output$groupText <- renderUI({
        groups <- groups()
        n <- g()
        if (length(n) > 0) {
            groupings <- c()
            for (g in 1:n) {
                curGroup <- paste(which(groups == g), collapse=', ')
                curGroup <- paste0("Group ", g, ": ", curGroup)
                groupings <- c(groupings, curGroup)
            }
            HTML(paste(groupings, collapse='<br/>'))
        }
    })
    
    output$socialRank_groups <- renderTable({
        n <- g()
        groups <- groups()
        
        if (length(n) > 0) {
            groupTable <- data.frame()
    
            for (g in 1:n) {
                curActors <- which(groups == g)
                if (length(curActors) == 1) {
                    groupTable <- groupTable %>%
                        rbind(r$equityImpact[curActors,] %>%
                                  select(-ID, -Stakeholder))
                } else {
                    curGroup <- add(lapply(curActors, getActorImpact))
                    groupTable <- groupTable %>%
                        rbind(-curGroup %>%
                                  t %>%
                                  colSums %>%
                                  rank %>%
                                  as.factor %>%
                                  data.frame %>%
                                  t)
                }
            }
    
            data.frame(Group = 1:n) %>%
                cbind(groupTable)
        }
    })
    
    # ====== Stakeholder mapping ======   
    # output$stakeholderMap <- renderPlotly({
    #     
    #     xa <- list(title="Influence/Power",
    #                range=c(0,100),
    #                fixedrange=T,
    #                showticklabels=F)        
    #     ya <- list(title="Interest/Stake",
    #                range=c(0, 100),
    #                fixedrange=T,
    #                showticklabels=F)
    #     
    #     circles <- map2(
    #         r$stakeholders$`Influence/Power`, 
    #         r$stakeholders$`Interest/Stake`, 
    #         ~list(
    #             type = "circle",
    #             # anchor circles at (x, y)
    #             xanchor = .x,
    #             yanchor = .y,
    #             # give each circle a 2 pixel diameter
    #             x0 = -3, x1 = 3,
    #             y0 = -3, y1 = 3,
    #             xsizemode = "pixel", 
    #             ysizemode = "pixel",
    #             # other visual properties
    #             fillcolor = "blue",
    #             line = list(color = "transparent")
    #         )
    #     )
    #     
    #     plot_ly() %>%
    #         add_trace(
    #             type="scatter",
    #             x=~r$stakeholders$`Influence/Power`,
    #             y=~r$stakeholders$`Interest/Stake`,
    #             mode="text",
    #             text=~r$stakeholders$ID,
    #             textposition="middle right"
    #         ) %>%
    #         layout(title="Stakeholder Map",
    #                xaxis=xa,
    #                yaxis=ya,
    #                shapes = circles) %>%
    #         config(edits = list(shapePosition = TRUE),
    #                displayModeBar=F)
    # })
    
    # Update actor weights after making edits to stakeholder map
    # observeEvent(event_data("plotly_relayout"), {
    #     event <- event_data("plotly_relayout")
    #     if (!is.null(event) & all(grepl("anchor", names(event)))) {
    #         rownum <- parse_number(names(event)[1]) + 1
    #         newCoord <- sapply(5*round(as.numeric(event)/5), max, 0)
    #         r$stakeholders$`Influence/Power`[rownum] <- newCoord[1]
    #         r$stakeholders$`Interest/Stake`[rownum] <- newCoord[2]
    #     }
    # })    

})
