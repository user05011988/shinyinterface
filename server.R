library(ggplot2)
library(plotly)
library(DT)
library(D3TableFilter)

library(minpack.lm)
source('sign_par.R')
source('signals_int.R')
source("autorun.R")
source("save_roi_testing.R")

shinyServer(function(input, output,session) {
  revals <- reactiveValues();
  revals2 <- reactiveValues();
  revals3 <- reactiveValues();
  
  v <- reactiveValues(meh=NULL, blah = NULL,stop3=0)
  
  sell <- reactiveValues(mtcars=NULL);
  observeEvent(input$select, {
    sell$mtcars=ROI_data[ROI_separator[, 1][as.numeric(input$select)]:(ROI_separator[, 1][as.numeric(input$select)+1]-1),]
    
    v$blah=NULL
    sell$change=1
    sell$stop=0
    sell$change2=1
    sell$stop2=0
    sell$roi=0
    print(sell$roi)
    # shinyjs::reset("mtcars")
    # shinyjs::reset("mtcars_edit")
    # session$sendCustomMessage(type = "resetValue", message = "mtcars_edit")
    # attr(input, "readonly") <- FALSE
    
    
    revals$mtcars <- sell$mtcars;
    revals2$mtcars <- rbind(rep(NA,7),rep(NA,7));
    colnames(revals2$mtcars)=c("intensity",	"shift",	"width",	"gaussian",	"J_coupling",	"multiplicities",	"roof_effect")
    revals3$mtcars <- rbind(rep(NA,3),rep(NA,3));
    colnames(revals3$mtcars)=c('Quantification','fitting error','signal/total area ratio')
    revals$rowIndex <- 1:nrow(sell$mtcars);
    
    output$mtcars <- renderD3tf({
      
      tableProps <- list(
        btn_reset = F,
        sort = TRUE,
        sort_config = list(
          sort_types = c("String", rep("Number", ncol(sell$mtcars)))
        )
      );
      
      observe({
        if(is.null(input$mtcars_edit)|(sell$stop==1)) {
          # if(sell$roi==0) {
          sell$change=0
          print('step1')
          return(NULL);
          # } 
        }
        # if(is.null(input$mtcars_edit)) {
        #   # sell$change=0
        #   print('step1')
        #   return(NULL);
        #   # } 
        # }
        edit <- input$mtcars_edit;
        #
        isolate({
          id <- edit$id;
          row <- as.integer(edit$row);
          col <- as.integer(edit$col);
          # print(col)
          val <- edit$val;
         
          if(col == 0) {
            # rownames
            oldval <- rownames(revals$mtcars)[row];
            # rownames can not start with a digit
            if(grepl('^\\d', val)) {
              rejectEdit(session, tbl = "mtcars", row = row, col = col,  id = id, value = oldval);
              sell$roi=0
              
              return(NULL);
            }
          } else if (col %in% c(1:2,5:11)){
            # numeric columns
            if(is.na(suppressWarnings(as.numeric(val)))) {
              oldval <- revals$mtcars[row, col];
              
              rejectEdit(session, tbl = "mtcars", row = row, col = col, id = id, value = oldval);
              
              
              sell$roi=0
              return(NULL);
            }
            } else if (col %in% c(3)) {
              if(is.na(suppressWarnings(val))) {
                oldval <- revals$mtcars[row, col];
               
                rejectEdit(session, tbl = "mtcars", row = row, col = col, id = id, value = oldval);
                return(NULL);
            }
          }
          
          if (sell$change==1){
            # rownames(revals$mtcars)[row] <- val;
            
            # sell$change=0
            
            sell$change=0
            sell$stop=1          # sell$stop=1
            disableEdit(session, "mtcars", c(1:11))
            
            # sell$roi=0
            
            print('step2')
            
            # return(NULL);
            
          } else{ 
            if(col == 0) {
              
            } else if (col %in% c(1:2,5:11)) {
              # print(val)
              revals$mtcars[row, col] <- as.numeric(val);
              sell$roi=1
              print('step')
              
              # val = round(as.numeric(val), 3)
              
            } else if (col %in% c(3)) {
              revals$mtcars[row, col] <- val;
             
              
            }
            
            confirmEdit(session, tbl = "mtcars", row = row, col = col, id = id, value = val);
          }
          
        })
        
      })
      
      d3tf(sell$mtcars,
        tableProps = tableProps,
        enableTf = F,
        edit=TRUE,
        
        tableStyle = "table table-bordered");
      
    })
  })
  
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
          
          
          revals3$mtcars=cbind(v$meh$results_to_save$Area,v$meh$results_to_save$fitting_error,v$meh$results_to_save$signal_area_ratio)
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
      
      
      
    })
    
    d3tf(revals2$mtcars,
      tableProps = tableProps,
      enableTf = F,
      edit=TRUE,
      
      tableStyle = "table table-bordered");
    # if (!is.null(val))
    
  })
  
  
  observeEvent(input$action, {
    is_autorun='N'
    if (length(input$x1_rows_selected)>1|input$x1_rows_selected>dim(autorun_data$dataset)[1]) {
      print('Select one valid spectrum')
      return(NULL)
    }
    # if(is.null(revals$rowIndex)) return(invisible());    
    # if(is.null(revals$mtcars)) v$p <- autorun(autorun_data, finaloutput,input,mtcars) 
    # else v$p <- autorun(autorun_data, finaloutput,input,revals$mtcars) 
    # print(sell$roi)
    # print(sell$mtcars)
    # print(revals$mtcars)
    if (sell$roi==0) {
      v$blah <- autorun(autorun_data, finaloutput, input,sell$mtcars,is_autorun) 
      
    }
    else {    
      v$blah <- autorun(autorun_data, finaloutput, input,revals$mtcars,is_autorun) 

    }
    revals3$mtcars=cbind(v$blah$results_to_save$Area,v$blah$results_to_save$fitting_error,v$blah$results_to_save$signal_area_ratio)
    finaloutput=v$blah$finaloutput
    # print(v$blah)
    # v$stop3=1
    
    # print(v$blah$plot_path)
    # revals2 <- reactiveValues();
    if (!is.null(v$blah$signals_parameters))
      revals2$mtcars <- v$blah$signals_parameters;
    revals2$rowIndex <- 1:nrow(revals2$mtcars);
    sell$stop=0
    
  })
  
  observeEvent(input$autorun, {
    
    # if(is.null(revals$rowIndex)) return(invisible());    
    # if(is.null(revals$mtcars)) v$p <- autorun(autorun_data, finaloutput,input,mtcars) 
    # else v$p <- autorun(autorun_data, finaloutput,input,revals$mtcars) 
    is_autorun='Y'
      v$chor <- autorun(autorun_data, finaloutput, input,revals$mtcars,is_autorun) 
    
    
    finaloutput=v$chor$finaloutput
    
    
  })
  is_autorun='Y'
  # } 
  observeEvent(input$save_results, {
    if (!is.null(v$blah))
      save_roi_testing(v$blah,autorun_data, finaloutput) 
    
    
  })
  
  observeEvent(input$save_profile, {
    
    ROI_data[ROI_separator[, 1][as.numeric(input$select)]:(ROI_separator[, 1][as.numeric(input$select)+1]-1),]=revals$mtcars
    write.csv(ROI_data,autorun_data$profile_folder_path)
    
    
  })
  #       
  #   })
  
  output$mtcars3 <- renderD3tf({
    tableProps <- list(
      btn_reset = TRUE,
      sort = TRUE,
      # enableEdit(session, "mtcars", c("col_1", "col_2")),
      sort_config = list(
        # alphabetic sorting for the row names column, numeric for all other columns
        sort_types = c("String", rep("Number", ncol(revals3$mtcars)))
      )
    );
    # observe({
    
    d3tf(revals3$mtcars,
      tableProps = tableProps,
      enableTf = F,
      edit=F,
      
      tableStyle = "table table-bordered");
    
    # })
  })
  
  output$fit_error <- renderD3tf({
    tableProps <- list(
      btn_reset = TRUE,
      sort = TRUE,
      # enableEdit(session, "mtcars", c("col_1", "col_2")),
      sort_config = list(
        # alphabetic sorting for the row names column, numeric for all other columns
        sort_types = c("String", rep("Number", ncol(revals3$mtcars)))
      )
    );
    # observe({
    bgColScales <- list()
      for(i in 1:dim(finaloutput$fitting_error)[2]) {
        bgColScales[[i]] = JS('function colorScale(tbl, i){
        var color = d3.scale.linear()
        .domain([0, 7, 14])
        .range(["#2ee524", "white", "#f20707"]);
        return color(i);
      }')
      }
    names(bgColScales)=paste0("col_", 0:(dim(finaloutput$fitting_error)[2]-1))
    
    d3tf(round(finaloutput$fitting_error,2),
      tableProps = tableProps,
      edit=F,
      showRowNames = T,
      bgColScales = bgColScales,
      tableStyle = "table table-bordered");
    
    # })
  })
  dataset=rbind(autorun_data$dataset,colMeans(autorun_data$dataset),apply(autorun_data$dataset,2,median))
  rownames(dataset)[(dim(autorun_data$dataset)[1]+1):dim(dataset)[1]]=c('Mean spectrum', 'Median spectrum')
  mm=matrix(NA,2,dim(autorun_data$Metadata)[2])
  colnames(mm)=colnames(autorun_data$Metadata)
  spectra=cbind(as.matrix(rownames(dataset)),rbind(autorun_data$Metadata,mm))
  # rownames(spectra)=ll
  colnames(spectra)[1]='spectrum'
  
  output$x1 = DT::renderDataTable(

    spectra , selection = list(mode = 'multiple', selected = 1),server = T)
  
  output$p_value_final = DT::renderDataTable(round(p_value_final,3),selection = list(mode = 'multiple', selected = 1),server = T)
  
  output$quant_selection = DT::renderDataTable(round(finaloutput$Area,2),selection = list(target = 'cell'),server = T)
  
  
  
  # output$x1 <- renderD3tf({
  #   
  #   # define table properties. See http://tablefilter.free.fr/doc.php
  #   # for a complete reference
  #   tableProps <- list(
  #     btn_reset = TRUE,
  #     rows_counter = TRUE,  
  #     rows_counter_text = "Rows: ",
  #     # sort = TRUE,
  #     on_keyup = TRUE,  
  #     on_keyup_delay = 800,
  #     # sort_config = list(
  #     #   sort_types = c("Number", "Number")
  #     # ),
  #     filters_row_index = 1
  #     # adding a summary row, showing the column means
  #     # rows_always_visible = list(nrow(mtcars) + 2),
  #     # col_operation = list( 
  #     #   id = list("frow_0_fcol_1_tbl_x1","frow_0_fcol_2_tbl_x1"),    
  #     #   col = list(1,2),    
  #     #   operation = list("mean","mean"),
  #     #   write_method = list("innerhtml",'innerhtml'),  
  #       # exclude_row = list(nrow(mtcars) + 2),  
  #       # decimal_precision = list(1, 1)
  #     # )
  #   );
  #   
  #   # add a summary row. Can be used to set values statically, but also to 
  #   # make use of TableFilters "col_operation"
  #   # footData <- data.frame(Rownames = "Mean", mpg = 0, cyl = 0);
  #   
  #   d3tf(spectra,
  #     enableTf = F,
  #     tableProps = tableProps,
  #     showRowNames = F, 
  #     selectableRows = "multi",
  #     selectableRowsClass = "info",
  #     tableStyle = "table table-bordered table-condensed",
  #     # rowStyles = c(rep("", 7), rep("info", 7)),
  #     # filterInput = TRUE,
  #     # footData = footData,
  #     height = 500);
  # })
  
  # spectra ,server = FALSE)
  
  output$plot <- renderPlotly({
    # print(is.null(v$blah))
    # if(input$x1_select)
    # print(input$x1_select)
    if (is.null(v$blah)|length(input$x1_rows_selected)>1) {
      # return()
      # 
      lol=which(round(autorun_data$ppm,6)==round(sell$mtcars[1,1],6))
      lol2=which(round(autorun_data$ppm,6)==round(sell$mtcars[1,2],6))

    
      # plotdata = data.frame(Xdata=autorun_data$ppm[lol:lol2], t(dataset[input$x1_rows_selected,lol:lol2,drop=F]))
      plotdata = data.frame(Xdata=autorun_data$ppm, t(dataset[input$x1_rows_selected,,drop=F]))
      # 
      # plot_ly(data=plotdata,x=~Xdata,y=~Ydata)
      plotdata3 <- melt(plotdata, id = "Xdata")
      plot_ly(data=plotdata3,x=~Xdata,y=~value,color=~variable,type='scatter',mode='lines') %>% layout(xaxis = list(range = c(round(sell$mtcars[1,1],6), round(sell$mtcars[1,2],6))),yaxis = list(range = c(0, max(dataset[input$x1_rows_selected,lol:lol2]))))
    } else {
      # print('Hey')
      ggplotly(v$blah$p) 
      
      # v$stop3=1
      
    }
  })
  
  output$plot_p_value <- renderPlotly({
    plot_ly(data=bucketing,x=~Xdata,y=~mediani,color=~value,type='scatter',mode='lines') %>% layout(xaxis = list(autorange = "reversed"),yaxis = list(range = c(0, max(mediani))))
    
  })
  
  output$plot_p_value_2 <- renderPlotly({
    plot_ly(ab, x = ~Signal, y = ~Value, color = ~Metadata, type = "box") %>%
      layout(boxmode = "group")
  })
  
  
})