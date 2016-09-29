library(ggplot2)
source("helpers.R")
library(quantmod)
library(plotly)
library(DT)
library(D3TableFilter)
source('packages_sources.R')
packages_sources()
library(minpack.lm)
source('sign_par.R')
source('signals_int.R')

# 
# 
# edits <- data.frame(Row = c("", ""), Column = (c("", "")), Value = (c("", "")), stringsAsFactors = FALSE);
# rownames(edits) <- c("Fail", "Success");



shinyServer(function(input, output,session) {
  revals <- reactiveValues();
  
  revals$mtcars <- mtcars;
  # revals$edits <- edits;
  revals$rowIndex <- 1:nrow(mtcars);
  v <- reactiveValues(meh=NULL, blah = NULL)
  
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
            # rejectEdit(session, tbl = "mtcars", row = row, col = col,  id = id, value = oldval);
            # revals$edits["Fail", "Row"] <- row;
            # revals$edits["Fail", "Column"] <- col;
            # revals$edits["Fail", "Value"] <- val;
            # return(NULL);
          }
        } else if (col %in% c(2, 3,4,5,6,7,8)){
          # numeric columns
          if(is.na(suppressWarnings(as.numeric(val)))) {
            oldval <- revals$mtcars[row, col];
            # reset to the old value
            # input will turn red briefly, than fade to previous color while
            # text returns to previous value
          #   rejectEdit(session, tbl = "mtcars", row = row, col = col, id = id, value = oldval);
          #   revals$edits["Fail", "Row"] <- row;
          #   revals$edits["Fail", "Column"] <- col;
          #   revals$edits["Fail", "Value"] <- val;
          #   return(NULL);
           } 
        } 
        # accept edits
        if(col == 0) {
          rownames(revals$mtcars)[row] <- val;
        } else if (col %in% c(2, 3,4,5,6,7,8)) {
          revals$mtcars[row, col] <- as.numeric(val);
          val = round(as.numeric(val), 3)

        }
        # confirm edits
        confirmEdit(session, tbl = "mtcars", row = row, col = col, id = id, value = val);
        # revals$edits["Success", "Row"] <- row;
        # revals$edits["Success", "Column"] <- col;
        # revals$edits["Success", "Value"] <- val;
      })
      
    })
    
    d3tf(mtcars,
      tableProps = tableProps,
      showRowNames = F,
      edit=TRUE,
      
      tableStyle = "table table-bordered");
    
  })
  
  # mtcars3=input$mtcars2_edit
  revals2 <- reactiveValues();
  # mtcars2=ifelse(exists(v$meh),v$meh,rbind(c(1,0,0,0,0,1,0),rbind(c(1,0,0,0,0,1,0))))
  revals2$mtcars <- mtcars2;
  # revals2$edits <- edits;
  revals2$rowIndex <- 1:nrow(mtcars2);
  
  output$mtcars2 <- renderD3tf({
    
    # Define table properties. See http://tablefilter.free.fr/doc.php
    # for a complete reference
    tableProps <- list(
      btn_reset = TRUE,
      sort = TRUE,
      # enableEdit(session, "mtcars", c("col_1", "col_2")),
      sort_config = list(
        # alphabetic sorting for the row names column, numeric for all other columns
        sort_types = c("String", rep("Number", ncol(v$meh)))
      )
    );
    # print(input$mtcars2)
    # print(input$mtcars2_edit)
    # print(v$meh)
    observe({
      if(is.null(input$mtcars2_edit)) return(NULL);
      print(revals2$mtcars)
#       if(is.null(v$meh)) {
#         edit <- input$mtcars2_edit}
#       else {edit <- v$meh;
# }
      edit <- input$mtcars2_edit
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
          oldval <- rownames(revals2$mtcars)[row];
          # if(grepl('^\\d', val)) {
          #   rejectEdit(session, tbl = "mtcars2", row = row, col = col,  id = id, value = oldval);
          #   revals2$edits["Fail", "Row"] <- row;
          #   revals2$edits["Fail", "Column"] <- col;
          #   revals2$edits["Fail", "Value"] <- val;
          #   return(NULL);
          # }
        } else if (col %in% c(1,2, 3,4,5,6,7)){
          # numeric columns
          if(is.na(suppressWarnings(as.numeric(val)))) {
            # oldval <- revals2$mtcars[row, col];
            # # reset to the old value
            # # input will turn red briefly, than fade to previous color while
            # # text returns to previous value
            # rejectEdit(session, tbl = "mtcars2", row = row, col = col, id = id, value = oldval);
            # revals2$edits["Fail", "Row"] <- row;
            # revals2$edits["Fail", "Column"] <- col;
            # revals2$edits["Fail", "Value"] <- val;
            # return(NULL);
          } 
        } 
        # accept edits
        if(col == 0) {
          rownames(revals2$mtcars)[row] <- val;
        } else if (col %in% c(1,2, 3,4,5,6,7)) {
          revals2$mtcars[row, col] <- as.numeric(val);
          val = round(as.numeric(val), 3)
          
        }
        # confirm edits
        confirmEdit(session, tbl = "mtcars2", row = row, col = col, id = id, value = val);
      #   revals2$edits["Success", "Row"] <- row;
      #   revals2$edits["Success", "Column"] <- col;
      #   revals2$edits["Success", "Value"] <- val;
      })
     # if (exists('val')) {
       # v$sol <- signals_int(autorun_data, finaloutput,input,revals2$mtcars) 
      a=signals_int(autorun_data, finaloutput,input,revals2$mtcars) 
      print(a$p)
      output$plot=renderPlotly({ggplotly(a$p)})
      
       # v$sol2=v$sol$signals_parameters
       # revals2 <- reactiveValues();
       # revals2$mtcars <- v$meh;
       # # revals2$edits <- edits;
       # revals2$rowIndex <- 1:nrow(v$meh);
     # }

    })
    
    d3tf(revals2$mtcars,
      tableProps = tableProps,
      showRowNames = F,
      edit=TRUE,
      
      tableStyle = "table table-bordered");
    # if (!is.null(val))
    
  })

  
  observeEvent(input$action, {
    # if(is.null(revals$rowIndex)) return(invisible());    
    # if(is.null(revals$mtcars)) v$p <- autorun(autorun_data, finaloutput,input,mtcars) 
    # else v$p <- autorun(autorun_data, finaloutput,input,revals$mtcars) 
    v$blah <- autorun(autorun_data, finaloutput,input,revals$mtcars) 
    v$meh=v$blah$signals_parameters
    # revals2 <- reactiveValues();
    revals2$mtcars <- v$meh;
    # revals2$edits <- edits;
    revals2$rowIndex <- 1:nrow(v$meh);
     # v$mtcars3=v$meh
    
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
    if (is.null(v$blah)) {
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
      ggplotly(v$blah$p)
    }
    
  })
  
  # observe({
  #   if(is.null(v$meh_edit)) return(NULL);
  #   print('Whatver')})
  # 
  # t <- reactiveValues(signals_parameters = NULL)
  
  # observeEvent(input$action, {
  #   if (is.null(v$signals_parameters)) {
  #     # return()
  #     t$signals_parameters=sign_par(autorun_data, finaloutput,input,ROI_profile)
  #     
  #   } else {
  #     ggplotly(v$blah$p)
  #   }
  #   
  # })
  
  
})
