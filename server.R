shinyServer(function(input, output) {
    
    # ==== Criterion tables ====
    output$criteria1 <- DT::renderDataTable({
        DT::datatable(
            criteria[[1]] %>% 
                dplyr::select(Dimension, Criterion),
            rownames=F,
            options= list(pageLength=10)
        )
    })    
    
    output$criteria2 <- DT::renderDataTable({
        DT::datatable(
            criteria[[2]] %>% 
                dplyr::select(Dimension, Criterion),
            rownames=F,
            options= list(pageLength=10)            
        )
    })        
    
    output$criteria3 <- DT::renderDataTable({
        DT::datatable(
            criteria[[3]] %>% 
                dplyr::select(Dimension, Criterion),
            rownames=F,
            options= list(pageLength=10)            
        )
    })        

    # ==== Standard results ====
    output$stdRes1 <- renderTable({
        simulateMC(1, impact_mat[[1]], criteria[[1]], mean = T)$ranking[1,]
    })
    
    output$stdRes2 <- renderTable({
        simulateMC(2, impact_mat[[2]], criteria[[2]], mean = T)$ranking[1,]
    })    

    output$stdRes3 <- renderTable({
        simulateMC(3, impact_mat[[3]], criteria[[3]], mean = T)$ranking[1,]
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
    r$stakeholders <- stakeholders %>%
        dplyr::rename(`Influence/Power` = y,
                      `Interest/Stake` = x)
    
    # Evaluation matrix for stakeholders
    ems <- eventReactive(input$runShAnalysis, {
        if (is.null(eval_mat_soc)) {
            ems <- getEvalMatSoc()
            eval_mat_soc <- ems
            readr::write_csv(data.frame(ems), "eval-mat-soc.csv", col_names=F)
        } else {
            ems <- eval_mat_soc
        }
        ems
    })    
    
    # Dendrogram
    dendrogram <- reactive({
        hclust(as.dist(1 - ems()), method="complete")
    })
    
    output$shDendrogram <- renderPlot({
        plot(dendrogram(), xlab="Actor ID")
    }) 
    
    # Get rankings based on stakeholder preferences
    getActorImpact <- function(i, weight=T) {
        prefs <- socialImpact[i,] %>%
            select(-ID, -Stakeholder)
        
        prefs <- sapply(prefs, function(j) lv_order[[j]])
        
        d <- nrow(alternatives)
        E <- data.frame(matrix(nrow = d, 
                               ncol = d,
                               dimnames = list(alternatives$Option, alternatives$Option)))
        for (j in 1:length(prefs)) {
            curPref <- prefs[[names(prefs)[j]]]
            p <- ifelse(curPref > prefs, 1,
                        ifelse(curPref == prefs, 0.5, 0))
            E[j,] <- p
            E[j,j] <- 0
        }
        if (weight) {
            E*r$stakeholders$weight[i]
        } else {
            E
        }
    }        
    
    # Outranking matrices for stakeholders
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
    
    # Show stakeholder rankings
    output$shRank_w <- renderTable({
        E <- E()
        -E %>%
            t %>%
            colSums %>%
            rank(ties.method = c("min")) %>%
            as.factor %>%
            data.frame %>%
            t
    })
    
    output$shRank_ew <- renderTable({
        E2 <- E2()
        -E2 %>%
            t %>%
            colSums %>%
            rank(ties.method = c("min")) %>%
            as.factor %>%
            data.frame %>%
            t
    })    
    
    # Show table of actors
    getWeight <- function(a) {
        aStake <- r$stakeholders %>%
            dplyr::filter(ID == a) %>%
            .$`Interest/Stake`
        r$stakeholders %>%
            dplyr::filter(ID != a) %>%
            dplyr::mutate(`Interest/Stake` = aStake - `Interest/Stake`) %>%
            dplyr::summarise(weight = mean(`Interest/Stake`)) %>%
            dplyr::mutate(weight = weight/100 + 1) %>%
            .$weight
    }    
    
    output$shTable <- DT::renderDataTable({
        r$stakeholders$weight <- sapply(r$stakeholders$ID, getWeight)
        DT::datatable(r$stakeholders %>%
                         dplyr::mutate(weight = round(weight, 2)),
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
    
    # Rankings per stakeholder group
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
            groupings <- c(groupings, "Columns highlighted grey can be vetoed by some coalitions.")
            HTML(paste(groupings, collapse='<br/>'))
        }
    })
    
    output$socialRank_groups <- DT::renderDataTable({
        n <- g()
        groups <- groups()
        veto <- c()
        
        if (length(n) > 0) {
            groupTable <- data.frame()
    
            for (g in 1:n) {
                curActors <- which(groups == g)
                curGroup <- add(lapply(curActors, getActorImpact))
                groupTable <- groupTable %>%
                    rbind(-curGroup %>%
                              t %>%
                              colSums %>%
                              rank(ties.method = c("min")) %>%
                              as.factor %>%
                              data.frame %>%
                              t)
                vg <- floor(nrow(alternatives) * length(curActors) / length(groups)) - 1
                if (vg > 0) {
                    rawRank <- -curGroup %>%
                        t %>%
                        colSums %>%
                        rank(ties.method = c("min")) %>%
                        unname
                    toVeto <- which(rawRank %in% sort(rawRank, decreasing=T)[1:vg])
                    veto <- c(veto, toVeto)
                }
            }
            
            DT::datatable(
                data.frame(Group = 1:n) %>%
                    cbind(groupTable),
                rownames=F,
                options= list(pageLength=10,
                              dom='t')
            ) %>%
                DT::formatStyle(veto + 1,  backgroundColor = '#A9A9A9')
        }
    })

})
