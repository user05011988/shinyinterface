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
  revals2 <- reactiveValues();
  # mtcars2=ifelse(exists(v$meh),v$meh,rbind(c(1,0,0,0,0,1,0),rbind(c(1,0,0,0,0,1,0))))
  revals2$mtcars <- rbind(rep(NA,7),rep(NA,7))
  # revals2$edits <- edits;
  revals2$rowIndex <- 1:nrow(mtcars2);
  v <- reactiveValues(meh=NULL, blah = NULL)
  
  sell <- reactiveValues(mtcars=NULL);
  observeEvent(input$select, {
    # input$mtcars_edit=NULL    
   sell$mtcars=ROI_data[ROI_separator[, 1][as.numeric(input$select)]:(ROI_separator[, 1][as.numeric(input$select)+1]-1),1:11]
   # input$mtcars_edit=NULL    
   
  # if(!is.null(input$mtcars_edit)) input$mtcars_edit=NULL
  v$blah=NULL
  sell$change=1
  sell$stop=0
  sell$change2=1
  sell$stop2=0
  revals$mtcars <- sell$mtcars;
  revals2$mtcars <-  rbind(rep(NA,7),rep(NA,7))

  
  # print(sell$mtcars)
  # print(revals$mtcars)
  revals$rowIndex <- 1:nrow(sell$mtcars);
  output$mtcars <- renderD3tf({

    
    tableProps <- list( btn_reset = TRUE,sort = TRUE,
      sort_config = list(
        sort_types = c("String", rep("Number", ncol(sell$mtcars)))));
    
    observe({
      
      if(is.null(input$mtcars_edit)|(sell$stop==1)) {
        sell$change=0
        return(NULL);
      }   
      
      
      edit <- input$mtcars_edit
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
          }
        } else if (col %in% c(1,2,5,6,7,8,9,10,11)){
          # numeric columns
          if(is.na(suppressWarnings(as.numeric(val)))) {
            oldval <- revals$mtcars[row, col];
        
           } 
        } 
        # accept edits
        if (sell$change==1){
          # rownames(revals$mtcars)[row] <- val;
          
          sell$change=0
          sell$stop=1
          print('hey')
          # return(NULL);
          
        } else{
        if(col == 0) {
          oldval <- revals$mtcars[row, col];
          
          rownames(revals$mtcars)[row] <- val;
        } else if (col %in% c(1,2,5,6,7,8,9,10,11)) {
          revals$mtcars[row, col] <- as.numeric(val);
          val = round(as.numeric(val), 3)
          # sell$change=0

        # confirm edits
        }
          confirmEdit(session, tbl = "mtcars", row = row, col = col, id = id, value = val);
          print('shit')
          
        
        }
     
      })
        
    })
    
    
    d3tf(revals$mtcars,
      tableProps = tableProps,
      showRowNames = F,
      edit=TRUE,
      tableStyle = "table table-bordered");
    
  })
  
  })
  # mtcars3=input$mtcars2_edit
  
  
  output$mtcars2 <- renderD3tf({
    
    
    tableProps <- list(btn_reset = TRUE,sort = TRUE,sort_config = list(sort_types = c("String", rep("Number", ncol(v$meh)))))
    # print(input$mtcars2)
    # print(input$mtcars2_edit)
    observe({
      # print(is.null(input$mtcars2_edit))
      # print(sell$stop)
      # print(sell$change)
      if(is.null(input$mtcars2_edit)|(sell$stop2==1)) {
        sell$change2=0
        return(NULL);
      }    
      edit <- input$mtcars2_edit
      isolate({
        
        id <- edit$id;
        row <- as.integer(edit$row);
        col <- as.integer(edit$col);
        val <- edit$val;
        
        # validate input 
        if(col == 0) {
          # rownames
          oldval <- rownames(revals2$mtcars)[row];
          
        } else if (col %in% c(1,2, 3,4,5,6,7)){
          # numeric columns
          if(is.na(suppressWarnings(as.numeric(val)))) {
           
          } 
        } 
        # accept edits
        
        print(sell$change2)
        if (sell$change2==1){
          # rownames(revals$mtcars)[row] <- val;
          
          sell$change2=0
          sell$stop2=1
          print('hey')
          v$blah=NULL
          # return(NULL);
          
        } else{
          
            # rownames(revals2$mtcars)[row] <- val;
            revals2$mtcars[row, col] <- as.numeric(val);
            confirmEdit(session, tbl = "mtcars2", row = row, col = col, id = id, value = val);
            a=signals_int(autorun_data, finaloutput,revals$mtcars,input,revals2$mtcars) 
            # print(a$p)
            v$blah$p=a$p
            # val = round(as.numeric(val), 3)
            
          }
        # confirm edits
      #   revals2$edits["Success", "Row"] <- row;
      #   revals2$edits["Success", "Column"] <- col;
      #   revals2$edits["Success", "Value"] <- val;
      })
     # if (exists('val')) {
       # v$sol <- signals_int(autorun_data, finaloutput,input,revals2$mtcars) 
      
      
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
    # print(sell$mtcars)
    # print("---")
    # print(revals$mtcars)
    
    v$blah <- autorun(autorun_data, finaloutput,revals$mtcars,input) 
    v$meh=v$blah$signals_parameters
    revals2$mtcars <- v$meh;
    revals2$rowIndex <- 1:nrow(v$meh);

  })
# } 
#       
#   })
  
  
  
  ll=c(autorun_data$Experiments,'Mean spectrum', 'Median spectrum')
  spectra=cbind(ll,rep(c(1,2),length(ll)/2))
  # rownames(spectra)=ll
  colnames(spectra)=c('spectrum','Group')
  
  output$x1 = DT::renderDataTable(spectra , selection = list(mode = 'multiple', selected = 11),server = FALSE)
    # spectra ,server = FALSE)
  
  output$plot <- renderPlotly({
    print(is.null(v$blah))
    if (is.null(v$blah)) {
      # return()
    dataset=rbind(autorun_data$dataset,colMeans(autorun_data$dataset),apply(autorun_data$dataset,2,median))
    rownames(dataset)[(dim(autorun_data$dataset)[1]+1):dim(dataset)[1]]=c('Mean spectrum', 'Median spectrum')
    lol=which(round(autorun_data$ppm,6)==round(sell$mtcars[1,1],6))
    lol2=which(round(autorun_data$ppm,6)==round(sell$mtcars[1,2],6))

    plotdata = data.frame(Xdata=autorun_data$ppm[lol:lol2], t(dataset[input$x1_rows_selected,lol:lol2,drop=F]))
    # 
    # plot_ly(data=plotdata,x=~Xdata,y=~Ydata)
    plotdata3 <- melt(plotdata, id = "Xdata")
    plot_ly(data=plotdata3,x=~Xdata,y=~value,color=~variable,type='scatter',mode='lines') %>% layout(xaxis = list(autorange = "reversed"))
    } else {
      ggplotly(v$blah$p)
    }
    
  })
  
  
  
  
})
