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
library(shinyjs)
# 
# 
# edits <- data.frame(Row = c("", ""), Column = (c("", "")), Value = (c("", "")), stringsAsFactors = FALSE);
# rownames(edits) <- c("Fail", "Success");



shinyServer(function(input, output,session) {
  revals <- reactiveValues();
  revals2 <- reactiveValues();
  v <- reactiveValues(meh=NULL, blah = NULL,stop3=0)
  
  sell <- reactiveValues(mtcars=NULL);
  observeEvent(input$select, {
   sell$mtcars=ROI_data[ROI_separator[, 1][as.numeric(input$select)]:(ROI_separator[, 1][as.numeric(input$select)+1]-1),]
  reset("mtcars_edit")
  print(input$mtcars_edit)
  # print(input$select)
  # print(sell$mtcars)
  v$blah=NULL
  sell$change=1
  sell$stop=1
  sell$change2=1
  sell$stop2=0
  sell$roi=0
  
  # sell$roi=1

  
  # attr(input, "readonly") <- FALSE
  # input$mtcars_edit <- NULL
  revals$mtcars <- sell$mtcars;
  revals2$mtcars <- rbind(rep(NA,7),rep(NA,7));
  
  # revals$edits <- edits;
  revals$rowIndex <- 1:nrow(sell$mtcars);
  
  output$mtcars <- renderD3tf({
  # print(sell$stop)
  #   print(sell$change)
    # Define table properties. See http://tablefilter.free.fr/doc.php
    # for a complete reference
    tableProps <- list(
      btn_reset = TRUE,
      sort = TRUE,
      # enableEdit(session, "mtcars", c("col_1", "col_2")),
      sort_config = list(
        # alphabetic sorting for the row names column, numeric for all other columns
        sort_types = c("String", rep("Number", ncol(sell$mtcars)))
      )
    );
    
    observe({
      print(sell$roi)
      print(sell$stop)
      print(sell$change)
      if(sell$stop==1) {
        # if(sell$roi==0) {
        sell$change=0
        print('step1')
        return(NULL);
      # } 
      }
      edit <- input$mtcars_edit;
      # print(sell$roi)
      # if (sell$roi==1) {
      #   edit=NULL
      #   sell$roi=0
      # }
      isolate({
        
        id <- edit$id;
        row <- as.integer(edit$row);
        col <- as.integer(edit$col);
        print(col)
        val <- edit$val;
        
        # validate input 
        # if(col == 0) {
        #   # rownames
        #   oldval <- rownames(revals$mtcars)[row];
        #   if(grepl('^\\d', val)) {
        #     # rejectEdit(session, tbl = "mtcars", row = row, col = col,  id = id, value = oldval);
        #     # revals$edits["Fail", "Row"] <- row;
        #     # revals$edits["Fail", "Column"] <- col;
        #     # revals$edits["Fail", "Value"] <- val;
        #     # return(NULL);
        #   }
        # } else if (col %in% c(5:11)){
        #   # numeric columns
        #   if(is.na(suppressWarnings(as.numeric(val)))) {
        #     oldval <- revals$mtcars[row, col];
        #     # reset to the old value
        #     # input will turn red briefly, than fade to previous color while
        #     # text returns to previous value
        #   #   rejectEdit(session, tbl = "mtcars", row = row, col = col, id = id, value = oldval);
        #   #   revals$edits["Fail", "Row"] <- row;
        #   #   revals$edits["Fail", "Column"] <- col;
        #   #   revals$edits["Fail", "Value"] <- val;
        #   #   return(NULL);
        #    } 
        # } 
        if (sell$change==1){
          # rownames(revals$mtcars)[row] <- val;
          
          # sell$change=0
          
          sell$change2=0
          sell$stop2=1          # sell$stop=1
          # sell$roi=0
          
          print('step2')
          
          # return(NULL);
          
        } else{
          print('step3')
          # sell$change=1
          if (length(col)==0) col=0
          print(col)
          
# accept edits
        if(col == 0) {
          # revals$mtcars=revals$mtcars
          } else if (col %in% c(1:2,5:11)) {
          # print(val)
          revals$mtcars[row, col] <- as.numeric(val);
          # val = round(as.numeric(val), 3)

        } else if (col %in% c(3)) {
          revals$mtcars[row, col] <- val;
          # val = round(as.numeric(val), 3)
          
        }
        # confirm edits
        # print(id)
          confirmEdit(session, tbl = "mtcars", row = row, col = col, id = id, value = val);
        # revals$edits["Success", "Row"] <- row;
        # revals$edits["Success", "Column"] <- col;
        # revals$edits["Success", "Value"] <- val;
        }
      })
      
    })
    
    d3tf(sell$mtcars,
      tableProps = tableProps,
      showRowNames = F,
      edit=TRUE,
      
      tableStyle = "table table-bordered");
    
  })
  })
  # mtcars3=input$mtcars2_edit
  
  
  output$mtcars2 <- renderD3tf({
    
    # Define table properties. See http://tablefilter.free.fr/doc.php
    # for a complete reference
    tableProps <- list(
      btn_reset = TRUE,
      sort = TRUE,
      # enableEdit(session, "mtcars", c("col_1", "col_2")),
      sort_config = list(
        # alphabetic sorting for the row names column, numeric for all other columns
        sort_types = c("String", rep("Number", ncol(revals2$mtcars)))
      )
    );
    # print(input$mtcars2)
    # print(input$mtcars2_edit)
    # print(v$meh)
    observe({
      if(is.null(input$mtcars2_edit)|(sell$stop2==1)) {
        sell$change2=0
        return(NULL);
      }       # print(revals2$mtcars)
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
        # print(sell$change2)
        if (sell$change2==1){
          # rownames(revals$mtcars)[row] <- val;
          
          sell$change2=0
          sell$stop2=1
          # print('hey')
          v$blah=NULL
          # return(NULL);
          
        } else {

          revals2$mtcars[row, col] <- as.numeric(val);
          val = round(as.numeric(val), 3)
          confirmEdit(session, tbl = "mtcars2", row = row, col = col, id = id, value = val);
          v$meh=signals_int(autorun_data, finaloutput,input,revals2$mtcars,revals$mtcars) 
          v$stop3=1
          
          v$blah$signals_parameters=v$meh$signals_parameters
          v$blah$p=v$meh$p
          v$blah$finaloutput=v$meh$finaloutput
        }
        # confirm edits
      #   revals2$edits["Success", "Row"] <- row;
      #   revals2$edits["Success", "Column"] <- col;
      #   revals2$edits["Success", "Value"] <- val;
      })
     # if (exists('val')) {
       # v$sol <- signals_int(autorun_data, finaloutput,input,revals2$mtcars) 
      

      # print(a$p)
      # output$plot=renderPlotly({ggplotly(a$p)})
      
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
    # print(revals$mtcars)
    v$blah <- autorun(autorun_data, finaloutput, input,revals$mtcars) 
    # v$stop3=1
    
    # print(v$blah$plot_path)
    # revals2 <- reactiveValues();
    if (!is.null(v$blah$signals_parameters))
    revals2$mtcars <- v$blah$signals_parameters;
    revals2$rowIndex <- 1:nrow(revals2$mtcars);
    sell$stop=0

  })
# } 
  observeEvent(input$save_results, {
    if (!is.null(v$blah))
    save_roi_testing(v$blah,autorun_data, finaloutput) 
    

  })
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
    # print(v$stop3)
    if (is.null(v$blah)) {
      # return()
    dataset=rbind(autorun_data$dataset,colMeans(autorun_data$dataset),apply(autorun_data$dataset,2,median))
    rownames(dataset)[(dim(autorun_data$dataset)[1]+1):dim(dataset)[1]]=c('Mean spectrum', 'Median spectrum')
    lol=which(round(autorun_data$ppm,6)==round(revals$mtcars[1,1],6))
    lol2=which(round(autorun_data$ppm,6)==round(revals$mtcars[1,2],6))

    plotdata = data.frame(Xdata=autorun_data$ppm[lol:lol2], t(dataset[input$x1_rows_selected,lol:lol2,drop=F]))
    # 
    # plot_ly(data=plotdata,x=~Xdata,y=~Ydata)
    plotdata3 <- melt(plotdata, id = "Xdata")
    plot_ly(data=plotdata3,x=~Xdata,y=~value,color=~variable,type='scatter',mode='lines') %>% layout(xaxis = list(autorange = "reversed"))
    } else {
      # print('Hey')
      ggplotly(v$blah$p)
      # v$stop3=1
      
    }
  })
  
  
  
  
})
