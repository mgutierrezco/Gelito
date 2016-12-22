#' GelitoShiny
#' Shiny version from Gelito

library(shiny)
library(Gelito)
library(memoise)
library(DT)

shinyServer(function(input, output, session) {
  
  #' Manager
  glt <- Gelito()
  glt$creator$id <- 1  # ID is into column number 1
  glt$creator$ni <- 1  # By default, one file is used
  
  #' Convert string separated by commas to list
  #' @param n string
  to_list <- function(n) {
    # Test if n
    if (is.null(n) || glt$creator$trim(n) == "") return()
    
    # Test if input is ok
    return(as.list(strsplit(gsub("[[:space:]]", "", n), ",")[[1]]))
  }

  #' Element shows id
  output$o_id <- renderPrint({
    
    # Configure or set buttons clicked
    if (input$do_configure > 0)
      isolate({
        cat(paste0("ID Column number: ", glt$creator$id))
      })
  })
  
  #' Element shows nf
  output$o_nf <- renderPrint({
    
    # Configure or set buttons clicked
    if (input$do_configure > 0)
      isolate({
        glt$creator$nf <- to_list(input$nf)
        if (length(glt$creator$nf) != length(unique(glt$creator$nf)))
          cat("All elements must be different")
        else cat(paste0("Filter number columns: ", 
                       paste0(glt$creator$nf, collapse = ",")))
      })
  })
  
  #' Element shows cc
  output$o_cc <- renderPrint({
    
    # Set button clicked
    if (input$do_configure > 0)
      isolate({
        glt$creator$cc = list()
        t <- ""
        for (i in 1:length(glt$creator$nf)) {
          e <- paste0("glt$creator$cc[", i, "] <- input$c", i)
          eval(parse(text = e))
          t <- paste0(t, "Column ", glt$creator$nf[i], ": ",
                      glt$creator$cc[i])
        }
        cat(paste0("Criterions: ", t))
      })
  })

  #' Element shows cn
  output$o_cn <- renderPrint({
    
    # Configure or set buttons clicked
    if (input$do_configure > 0)
      isolate({
        glt$creator$cn <- to_list(input$cn)
        if (length(glt$creator$cn) != length(unique(glt$creator$cn)))
          cat("All elements must be different")
        else cat(paste0("Column numbers or output: ", 
                        paste0(glt$creator$cn, collapse = ",")))
      })
  })
 
  #' Setting up environment
  setEnvironment <- memoise(function() {
    
    # All files
    e <- ""
    
    if (length(input$cn) == "") {
      e <- c("Out column numbers is missing")
    } else {
      for (i in 1:input$ni) {
        # Dynamic input file
        f <- eval(parse(text = paste0("input$f", i)))
        if (is.null(f)) return(NULL)
      
        # Read file into Gelito
        t <- paste0("glt$readFile(filename = (input$f", i, ")$datapath)")
        eval(parse(text = t))
      
        # Update real file name
        t <- paste0("glt$df[length(glt$df)] <- basename((input$f", i, ")$name)")
        eval(parse(text = t))
      
        if (i > 1) e <- paste0(e, ", ")
        e <- paste0(e, glt$df[i])
      }
      e <- paste0("File(s) uploaded: ", e)
    }
    cat(e)
    #return(e)
  })
  
  #' Element shows ll
  output$o_ll <- renderPrint(
    isolate({
      if (input$cn == "")
        eval(parse(text = ""))
      else {
        withProgress({
          setProgress(message = "Processing data ...")
          setEnvironment()
        }, value = 0.5)
      }
    })
  ) 
  
  #' Element shows operation results
  output$set_operation <- renderPrint({# renderTable
    if (input$do_operation > 0)
      isolate({
        if (input$operation == "Union") {
          unlist(glt$union(glt$ll, glt$creator$id)[1:100])
        } else if (input$operation == "Intersect") {
          unlist(glt$intersect(glt$ll, glt$creator$id)[1:100])
        } else if (input$operation == "Common") {
          l_common <- glt$common(glt$ll, glt$creator$id)
          t <- c("Only showing first 100 elements by common lists.\n\n")
          for (i in 1:length(l_common)) {
            d <- paste(unlist(l_common[[i]][[1]]), collapse = " and ")
            u <- paste(unlist(l_common[[i]][[2]][1:100]), collapse = ",")
            if (d == "0 and 0") t <- paste0(t, "[Common between each one")
            else t <- paste0(t, "[Common between samples ", d)
            t <- paste0(t, "]\n", u, "\n\n")
          }
          l_common <- NA
          cat(t)
        }
      })
  })
  
  #' Element configures criterions
  output$set_criterions <- renderUI({
    
    # Configure button clicked
    if (input$do_configure > 0)
      isolate({
        if (is.null(input$nf) || glt$creator$trim(input$nf) == "")
          paste0("")
        else {
          r <- to_list(input$nf)
          if (length(r) != length(unique(r))) 
            paste0("All elements must be different")
          else {
            t <- ""
            # Build dynamic elements
            for (i in 1:length(r)) {
              if (t != "") t <- paste0(t, ",")
              # Element
              t <- paste0(t, "column(4, textInput('c", i, 
                          "', 'Criterion for column ", r[i], ":', '",
                          eval(parse(text = paste0("glt$cc[", i, "]"))), 
                          "', placeholder = 'condition')")
              # Add CSS for length
              t <- paste0(t, ", tags$head(tags$style(type='text/css', '#c",
                          i, " {width: 150px}')))")
            }
            
            # Show it
            t <- paste0("fluidRow(", t, ")")
            eval(parse(text = t))
          }
        }
      })
  })
  
  #' Button for setting criterios
  output$set_button <- renderUI(
    if (input$do_configure > 0)
      isolate({
        r <- to_list(input$nf)
        t <- paste0("p(\"Criterion examples: >6 || <5, <5 && >1, ==1.05, '[^lo]'\")")
        eval(parse(text = t))
      })
  ) 
  
  #' Element configure input files
  output$set_inputfiles <- renderUI(

    # Configure button clicked
    if (input$do_configure > 0)
      isolate({
        if (is.null(input$ni) || glt$creator$trim(input$ni) == "")
          paste0("")
        else {
          t <- ""
          # Build dynamic elements
          for (i in 1:input$ni) {
            if (t != "") t <- paste0(t, ",")
            t <- paste0(t, "column(4, fileInput('f", i, "', 'Choose input file ", i,
                        ":', accept = c('text/txt', 'text/plain', '.txt'), width = '100%')")
            
            # Add CSS for length
            t <- paste0(t, ", tags$head(tags$style(type='text/css', '#f", i, " {width: 150px}')))")
          }
          
          # Show it
          t <- paste0("fluidRow(", t, ")")
          eval(parse(text = t))
        }
      })  
  )
  
  #' Element for outputs venn diagram
  output$set_diagram <- renderPlot(
    
    # Action button clicked
    isolate({
      # If filenames
      if (length(glt$df) > 0)
        glt$intersectVenn(glt$ll, glt$creator$id, unlist(glt$df))
    })
  , height = "auto", width = "auto")
  
  #' Element for output subset
  output$set_subset <- renderUI({
    
    t <- c()
    
    # Basic operations
    t[1] <- c("Intersect")
    t[2] <- c("Union")

    # All instersect between lists
    for (i in 1:(length(glt$ll)-1)) {
      for (j in (i+1):length(glt$ll)) {
        t[i + 2] <- paste("Common:", i, " <-> ",j)
      }
    }
    selectInput("list", "Choose available list for subsetting:", choices = t)
  })

  #' Element shows em
  output$o_em <- renderPrint(
    isolate({
      # Read file into Gelito
      if (input$em > 0) {
        t <- paste0("glt$setMatrixFromFile((input$em)$datapath)")
        eval(parse(text = t))
      }
      return()
    })
  ) 
  
  #' Element for output results
  #output$results <- renderDataTable({
  output$results <- renderTable({
      
    # Click do subset
    if (input$do_subset > 0)
      isolate({
        # Check which list is selected
        if (input$list == "Intersect") {
          glt$subset(unlist(glt$intersect(glt$ll, glt$creator$id)))
        } else if (input$list == "Union") {
          glt$subset(unlist(glt$union(glt$ll, glt$creator$id)))
        } else { # Common
        }
      })
  })
  
  #' Download operation
  output$downloadOperation <- downloadHandler(
    filename = function() {
      paste("gelito-", Sys.Date(), ".csv", sep = "")
    },
    content = function(con) {

      data <- ""  
      if (input$operation == "Union") {
        data <- as.data.frame(glt$union(glt$ll, glt$creator$id))
      } else if (input$operation == "Intersect") {
        data <- as.data.frame(glt$intersect(glt$ll, glt$creator$id))
      } else {
        data <- as.data.frame(glt$common(glt$ll, glt$creator$id))
      }

      if (input$format == "csv") write.csv(data, con)
      else if (input$format == "tab") write.table(data, sep = "\t", con)
      else WriteXLS::WriteXLS(data, con)
    }
  )
  
})
