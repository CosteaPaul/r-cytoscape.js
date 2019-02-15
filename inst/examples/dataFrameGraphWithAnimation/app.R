library(cyjShiny)
library(later)

dataFramesToJSON = function (tbl.edges, tbl.nodes = NULL) 
{
    stopifnot(!grepl("factor", as.character(lapply(tbl.edges, 
        class))))
    stopifnot(all(c("source", "target") %in% colnames(tbl.edges)))
    stopifnot("interaction" %in% colnames(tbl.edges))
    stopifnot(all(rownames(tbl.nodes) == tbl.nodes$id))

    hasPos = FALSE
    #Did the user provide positions for points? Then, add those properly
    if ("x" %in% colnames(tbl.nodes) && "y" %in% colnames(tbl.nodes)) {
	tbl.positions = tbl.nodes[,c("x","y")]
	tbl.positions$x = tbl.positions$x * 5 
      #Remove them from the points def, otherwise they end up there too
      tbl.nodes = tbl.nodes[,-match(c("x","y"),colnames(tbl.nodes))]
      hasPos = TRUE
    }

    nodes.implied.by.edgeData <- sort(unique(c(tbl.edges$source, 
        tbl.edges$target)))
    if (is.null(tbl.nodes)) {
        node.count <- length(nodes.implied.by.edgeData)
        tbl.nodes <- data.frame(id = nodes.implied.by.edgeData, 
            type = rep("unspecified", node.count), stringsAsFactors = FALSE)
    }
    nodes <- sort(unique(c(tbl.edges$source, tbl.edges$target, 
        tbl.nodes$id)))
    edgeCount <- nrow(tbl.edges)
    vector.count <- 10 * (edgeCount + length(nodes))
    vec <- vector(mode = "character", length = vector.count)
    i <- 1
    vec[i] <- "{\"elements\": {\"nodes\": ["
    i <- i + 1
    noa.names <- colnames(tbl.nodes)[-1]
    eda.names <- colnames(tbl.edges)[-(1:2)]
    nodeCount <- length(nodes)
    for (n in 1:nodeCount) {
        node <- nodes[n]
        vec[i] <- "{\"data\": "
        i <- i + 1
        nodeList <- list(id = node)
        if (ncol(tbl.nodes) > 1) 
            nodeList <- c(nodeList, as.list(tbl.nodes[node, -1, 
                drop = FALSE]))
        nodeList.json <- toJSON(nodeList, auto_unbox = TRUE)
        vec[i] <- nodeList.json
	  i <- i + 1
        if (hasPos) {
	  	vec[i] <- ", \"position\": "
	  	i <- i + 1
	  	vec[i] <- toJSON(as.list(tbl.positions[node,, drop = FALSE]),auto_unbox=T)
        	i <- i + 1
	  }
        if (n != nodeCount) {
            vec[i] <- "},"
            i <- i + 1
        }
    }
    vec[i] <- "}]"
    i <- i + 1
    if (edgeCount > 0) {
        vec[i] <- ", \"edges\": ["
        i <- i + 1
        for (e in seq_len(edgeCount)) {
            vec[i] <- "{\"data\": "
            i <- i + 1
            sourceNode <- tbl.edges[e, "source"]
            targetNode <- tbl.edges[e, "target"]
            interaction <- tbl.edges[e, "interaction"]
            edgeName <- sprintf("%s-(%s)-%s", sourceNode, interaction, 
                targetNode)
            edgeList <- list(id = edgeName, source = sourceNode, 
                target = targetNode, interaction = interaction)
            if (ncol(tbl.edges) > 3) 
                edgeList <- c(edgeList, as.list(tbl.edges[e, 
                  -(1:3), drop = FALSE]))
            edgeList.json <- toJSON(edgeList, auto_unbox = TRUE)
            vec[i] <- edgeList.json
            i <- i + 1
            if (e != edgeCount) {
                vec[i] <- "},"
                i <- i + 1
            }
        }
        vec[i] <- "}]"
        i <- i + 1
    }
    vec[i] <- "}"
    i <- i + 1
    vec[i] <- "}"
    vec.trimmed <- vec[which(vec != "")]
    paste0(vec.trimmed, collapse = " ")
}

#----------------------------------------------------------------------------------------------------
styles <- c("",
            "generic style"="basicStyle.js",
            "style 01" = "style01.js")
#----------------------------------------------------------------------------------------------------
# create  data.frames for nodes, edges, and two simulated experimental variables, in 3 conditions
#----------------------------------------------------------------------------------------------------

tbl.nodes <- read.table('map00010.nodes',header=T,sep='\t',stringsAsFactors=F)
rownames(tbl.nodes) = tbl.nodes$id
tbl.nodes$selectionDiv = '<table frame="box"><tr><td>Mass</td></tr><tr><td><img src="https://upload.wikimedia.org/wikipedia/commons/5/50/Green_Arrow_Up.svg" height="20" width="20"></td></tr></table>'


tbl.edges<- read.table('map00010.edges',header=T,sep='\t',stringsAsFactors=F)
tbl.edges$interaction = 'Unknown'
tbl.edges$selectionDiv = '<table frame="box"><tr><td>Gene1</td><td>Gene2</td></tr><tr><td><img src="https://upload.wikimedia.org/wikipedia/commons/5/50/Green_Arrow_Up.svg" height="20" width="20"></td><td><img src="https://upload.wikimedia.org/wikipedia/commons/0/04/Red_Arrow_Down.svg" height="20" width="20"></td></tr></table>'

graph.json <- dataFramesToJSON(tbl.edges, tbl.nodes)

tbl.lfc <- data.frame(A=c(0,  1,   1,  -3),
                      B=c(0,  3,   2,   3),
                      C=c(0, -3,  -2,  -1),
                      stringsAsFactors=FALSE)

rownames(tbl.lfc) <- c("baseline", "cond1", "cond2", "cond3")

tbl.count <- data.frame(A=c(1, 10,  100, 150),
                        B=c(1, 5,   80,  3),
                        C=c(1, 100, 50,  300),
                        stringsAsFactors=FALSE)

rownames(tbl.count) <- c("baseline", "cond1", "cond2", "cond3")

#----------------------------------------------------------------------------------------------------
ui = shinyUI(fluidPage(

  sidebarLayout(
      sidebarPanel(
          selectInput("loadStyleFile", "Select Style: ", choices=styles),
          selectInput("doLayout", "Select Layout:",
                      choices=c("",
                                "cose",
                                "cola",
                                "circle",
                                "concentric",
                                "breadthfirst",
                                "grid",
                                "random",
                                "dagre",
                                "cose-bilkent")),


          selectInput("showCondition", "Select Condition:", choices=rownames(tbl.lfc)),
          selectInput("selectName", "Select Node by ID:", choices = c("", sort(tbl.nodes$id))),
          actionButton("sfn", "Select First Neighbor"),
          actionButton("fit", "Fit Graph"),
          actionButton("fitSelected", "Fit Selected"),
          actionButton("clearSelection", "Clear Selection"), HTML("<br>"),
          #actionButton("loopConditions", "Loop Conditions"), HTML("<br>"),
          actionButton("removeGraphButton", "Remove Graph"), HTML("<br>"),
          actionButton("addRandomGraphFromDataFramesButton", "Add Random Graph"), HTML("<br>"),
          actionButton("getSelectedNodes", "Get Selected Nodes"), HTML("<br><br>"),
          htmlOutput("selectedNodesDisplay"),
          width=2
      ),
      mainPanel(cyjShinyOutput('cyjShiny', height=400),width=10)
  ) # sidebarLayout
))
#----------------------------------------------------------------------------------------------------
server = function(input, output, session)
{
    observeEvent(input$fit, ignoreInit=TRUE, {
       fit(session, 80)
       })

    observeEvent(input$showCondition, ignoreInit=TRUE, {
       condition.name <- isolate(input$showCondition)
       #printf(" condition.name: %s", condition.name)
       values <- as.numeric(tbl.lfc[condition.name,])
       node.names <- colnames(tbl.lfc)
       #printf("sending lfc values for %s: %s", paste(node.names, collapse=", "), paste(values, collapse=", "))
       setNodeAttributes(session, attributeName="lfc", nodes=node.names, values)
       values <- as.numeric(tbl.count[condition.name,])
       node.names <- colnames(tbl.count)
       #printf("sending count values for %s: %s", paste(node.names, collapse=", "), paste(values, collapse=", "))
       setNodeAttributes(session, attributeName="count", nodes=colnames(tbl.count), values)
       })

    observeEvent(input$loadStyleFile,  ignoreInit=TRUE, {
       if(input$loadStyleFile != ""){
          tryCatch({
             loadStyleFile(input$loadStyleFile)
             }, error=function(e) {
                msg <- sprintf("ERROR in stylesheet file '%s': %s", input$loadStyleFile, e$message)
                showNotification(msg, duration=NULL, type="error")
                })
           later(function() {updateSelectInput(session, "loadStyleFile", selected=character(0))}, 0.5)
          }
       })

    observeEvent(input$doLayout,  ignoreInit=TRUE,{
       if(input$doLayout != ""){
          strategy <- input$doLayout
          doLayout(session, strategy)
          later(function() {updateSelectInput(session, "doLayout", selected=character(0))}, 1)
          }
       })

    observeEvent(input$selectName,  ignoreInit=TRUE,{
       selectNodes(session, input$selectName)
       })

    observeEvent(input$sfn,  ignoreInit=TRUE,{
       selectFirstNeighbors(session)
       })

    observeEvent(input$fitSelected,  ignoreInit=TRUE,{
       fitSelected(session, 100)
       })

    observeEvent(input$getSelectedNodes, ignoreInit=TRUE, {
       output$selectedNodesDisplay <- renderText({" "})
       getSelectedNodes(session)
       })

    observeEvent(input$clearSelection,  ignoreInit=TRUE, {
       clearSelection(session)
       })

    observeEvent(input$loopConditions, ignoreInit=TRUE, {
        condition.names <- rownames(tbl.lfc)
        for(condition.name in condition.names[-1]){
           #browser()
           lfc.vector <- as.numeric(tbl.lfc[condition.name,])
           node.names <- rownames(tbl.lfc)
           setNodeAttributes(session, attributeName="lfc", nodes=node.names, values=lfc.vector)
           #updateSelectInput(session, "setNodeAttributes", selected=condition.name)
           Sys.sleep(1)
           } # for condition.name
        updateSelectInput(session, "setNodeAttributes", selected="baseline")
        })

    observeEvent(input$removeGraphButton, ignoreInit=TRUE, {
        removeGraph(session)
        })

    observeEvent(input$addRandomGraphFromDataFramesButton, ignoreInit=TRUE, {
        source.nodes <-  LETTERS[sample(1:5, 5)]
        target.nodes <-  LETTERS[sample(1:5, 5)]
        tbl.edges <- data.frame(source=source.nodes,
                                target=target.nodes,
                                interaction=rep("generic", length(source.nodes)),
                                stringsAsFactors=FALSE)
        all.nodes <- sort(unique(c(source.nodes, target.nodes, "orphan")))
        tbl.nodes <- data.frame(id=all.nodes,
                                type=rep("unspecified", length(all.nodes)),
                                stringsAsFactors=FALSE)
        addGraphFromDataFrame(session, tbl.edges, tbl.nodes)
        })

    observeEvent(input$selectedNodes, {
          #  communicated here via assignement in cyjShiny.js
          #     Shiny.setInputValue("selectedNodes", value, {priority: "event"});
        newNodes <- input$selectedNodes;
        output$selectedNodesDisplay <- renderText({
           paste(newNodes)
           })
        })

    output$value <- renderPrint({ input$action })
    output$cyjShiny <- renderCyjShiny({
       cyjShiny(graph=graph.json, layoutName="preset",style_file="basicStyle.js")
       })

} # server
#----------------------------------------------------------------------------------------------------
app <- shinyApp(ui = ui, server = server)
