library(ggplot2)
source("helpers.R")
library(quantmod)
library(plotly)
library(DT)
library(D3TableFilter)
source('packages_sources.R')
packages_sources()
library(minpack.lm)

edits <- data.frame(Row = c("", ""), Column = (c("", "")), Value = (c("", "")), stringsAsFactors = FALSE);
rownames(edits) <- c("Fail", "Success");

filtering <- data.frame(Rows = c(nrow(mtcars), nrow(mtcars)), Indices = c(paste(1:nrow(mtcars), collapse = ', '), paste(1:nrow(mtcars), collapse = ', ')), stringsAsFactors = FALSE);
rownames(filtering) <- c("Before", "After")

shinyServer(function(input, output,session) {
  revals <- reactiveValues();
  
  revals$mtcars <- mtcars;
  revals$edits <- edits;
  revals$filtering <- filtering;
  revals$filters <- NULL;
  revals$rowIndex <- 1:nrow(mtcars);
  revals$filters <- data.frame(Column = character(), Filter = character(), stringsAsFactors = FALSE);
  
  output$mtcars <- renderD3tf({

    # Define table properties. See http://tablefilter.free.fr/doc.php
    # for a complete reference
    tableProps <- list(
      btn_reset = TRUE,
      sort = TRUE,
      # enableEdit(session, "mtcars", c("col_1", "col_2")),
      sort_config = list(
        # alphabetic sorting for the row names column, numeric for all other columns
        sort_types = c("String", rep("Number", ncol(mtcars)))
      )
    );
    
    observe({
      if(is.null(input$mtcars_edit)) return(NULL);
      edit <- input$mtcars_edit;
      
      isolate({
        # need isolate, otherwise this observer would run twice
        # for each edit
        id <- edit$id;
        row <- as.integer(edit$row);
        col <- as.integer(edit$col);
        val <- edit$val;
        
        # validate input 
        if(col == 0) {
          # rownames
          oldval <- rownames(revals$mtcars)[row];
          if(grepl('^\\d', val)) {
            rejectEdit(session, tbl = "mtcars", row = row, col = col,  id = id, value = oldval);
            revals$edits["Fail", "Row"] <- row;
            revals$edits["Fail", "Column"] <- col;
            revals$edits["Fail", "Value"] <- val;
            return(NULL);
          }
        } else if (col %in% c(1, 2, 3)){
          # numeric columns
          if(is.na(suppressWarnings(as.numeric(val)))) {
            oldval <- revals$mtcars[row, col];
            # reset to the old value
            # input will turn red briefly, than fade to previous color while
            # text returns to previous value
            rejectEdit(session, tbl = "mtcars", row = row, col = col, id = id, value = oldval);
            revals$edits["Fail", "Row"] <- row;
            revals$edits["Fail", "Column"] <- col;
            revals$edits["Fail", "Value"] <- val;
            return(NULL);
          } 
        } else if (col %in% c(4, 5)) {
          ; #nothing to validate for logical columns
        }
        # accept edits
        if(col == 0) {
          rownames(revals$mtcars)[row] <- val;
        } else if (col %in% c(1, 2, 3)) {
          revals$mtcars[row, col] <- as.numeric(val);
          val = round(as.numeric(val), 1)
        } else if (col == 4) {
          revals$mtcars[row, col] <- val;
        } else if (col == 5) {
          # radio buttons. There is no uncheck event
          # so we need to set the whole column to FALSE here
          revals$mtcars[, "favorite"] <- FALSE;
          revals$mtcars[row, col] <- val;
        }
        # confirm edits
        confirmEdit(session, tbl = "mtcars", row = row, col = col, id = id, value = val);
        revals$edits["Success", "Row"] <- row;
        revals$edits["Success", "Column"] <- col;
        revals$edits["Success", "Value"] <- val;
      })
      
    })
    
    d3tf(mtcars,
      tableProps = tableProps,
      showRowNames = F,
      edit=TRUE,
      
      tableStyle = "table table-bordered");
    # enable editing of the first two columns of the "mtcars" table
    # enableEdit(session, "mtcars", c("col_1", "col_2"))


  })
 

  v <- reactiveValues(p = NULL)
  
  observeEvent(input$action, {
    # if(is.null(revals$rowIndex)) return(invisible());    
    # if(is.null(revals$mtcars)) v$p <- autorun(autorun_data, finaloutput,input,mtcars) 
    # else v$p <- autorun(autorun_data, finaloutput,input,revals$mtcars) 
    v$p <- autorun(autorun_data, finaloutput,input,revals$mtcars) 
  })
# } 
#       
#   })
  ll=c(autorun_data$Experiments,'Mean spectrum', 'Median spectrum')
  spectra=cbind(ll,rep(c(1,2),length(ll)/2))
  # rownames(spectra)=ll
  colnames(spectra)=c('spectrum','Group')
  
  output$x1 = DT::renderDataTable(
    
   spectra , selection = list(mode = 'multiple', selected = 11),server = FALSE)
    # spectra ,server = FALSE)
  
  output$plot <- renderPlotly({
    if (is.null(v$p)) {
      # return()
    dataset=rbind(autorun_data$dataset,colMeans(autorun_data$dataset),apply(autorun_data$dataset,2,median))
    rownames(dataset)[(dim(autorun_data$dataset)[1]+1):dim(dataset)[1]]=c('Mean spectrum', 'Median spectrum')
    lol=which(round(autorun_data$ppm,6)==round(input$num1,6))
    lol2=which(round(autorun_data$ppm,6)==round(input$num2,6))

    plotdata = data.frame(Xdata=autorun_data$ppm[lol:lol2], t(dataset[input$x1_rows_selected,lol:lol2,drop=F]))
    # 
    # plot_ly(data=plotdata,x=~Xdata,y=~Ydata)
    plotdata3 <- melt(plotdata, id = "Xdata")
    plot_ly(data=plotdata3,x=~Xdata,y=~value,color=~variable,type='scatter',mode='lines') %>% layout(xaxis = list(autorange = "reversed"))
    } else {
      ggplotly(v$p)
    }
    
  })
})
