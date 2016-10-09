autorun = function(autorun_data, finaloutput,ROI_profile,input) {
  
  
    #Preparation of necessary variables and folders to store figures and information of the fitting
  if(is.null(input$x1_rows_selected)) {spectrum_index=1
  } else {spectrum_index=input$x1_rows_selected}
  
  ROI_buckets=which(round(autorun_data$ppm,6)==round(ROI_profile[1,1],6)):which(round(autorun_data$ppm,6)==round(ROI_profile[1,2],6))
  # print(ROI_profile)
  Xdata= as.numeric(autorun_data$ppm[ROI_buckets])
    Ydata = as.numeric(autorun_data$dataset[spectrum_index, ROI_buckets])
    
    other_fit_parameters = fitting_variables()
    other_fit_parameters$freq = autorun_data$freq
    other_fit_parameters$ROI_buckets = ROI_buckets
    other_fit_parameters$buck_step = autorun_data$buck_step
    # experiment_name = autorun_data$Experiments[[spectrum_index]]
    # plot_path = file.path(autorun_data$export_path,
    #                       experiment_name,
    #                       signals_names)
    # for (i in seq_along(plot_path))
    #   if (!dir.exists(plot_path[i]))
    #     dir.create(plot_path[i])

    #If the quantification is through integration with or without baseline
    # if (fitting_type == "Clean Sum" ||
    #     fitting_type == "Baseline Sum") {
    #   is_roi_testing = "N"
    #   clean_fit = ifelse(fitting_type == "Clean Sum", "Y", "N")
    #   integration_parameters = data.frame(plot_path, is_roi_testing,
    #                                       clean_fit)
    #   results_to_save = integration(integration_parameters, Xdata,
    # 
    #                                 Ydata)
    #   #Generation of output variables specific of every quantification
    # 
    #   write.csv(
    #     integration_parameters,
    #     file.path(plot_path[i],
    #               "integration_parameters.csv"),
    #     row.names = F
    #   )

      #If the quantification is through fitting with or without baseline
    # } else if (fitting_type == "Clean Fitting" || fitting_type ==
    #            "Baseline Fitting") {
      is_roi_testing = "N"
      clean_fit='N'
      signals_names=autorun_data$signals_names[1:2]
      signals_codes=autorun_data$signals_codes[1:2]
      
      # clean_fit = ifelse(fitting_type == "Clean Fitting", "Y",
      #                    "N")
    # print(ROI_profile)
      #Parameters of every signal necessary for the fitting
      initial_fit_parameters = ROI_profile[, 5:11,drop=F]
      # initial_fit_parameters=as.data.frame(apply(initial_fit_parameters,2,as.numeric))

      # initial_fit_parameters = initial_fit_parameters[complete.cases(initial_fit_parameters),]
      colnames(initial_fit_parameters) = c(
        "positions",
        "widths",
        "quantification_or_not",
        "multiplicities",
        "Jcoupling",
        "roof_effect",
        "shift_tolerance"
      )

      #Ydata is scaled to improve the quality of the fitting
      scaledYdata = as.vector(Ydata / (max(Ydata)))

      #Other parameters necessary for the fitting independent of the type of signal

      other_fit_parameters$clean_fit = clean_fit
      
      
      #Adaptation of the info of the parameters into a single matrix and preparation (if necessary) of the background signals that will conform the baseline
      FeaturesMatrix = fitting_prep(Xdata,
                                    scaledYdata,
                                    initial_fit_parameters,
                                    other_fit_parameters)


      #Calculation of the parameters that will achieve the best fitting
      signals_parameters = fittingloop(FeaturesMatrix,
                                       Xdata,
                                       scaledYdata,
                                       other_fit_parameters)

      #Fitting of the signals
      multiplicities=FeaturesMatrix[,11]
      roof_effect=FeaturesMatrix[,12]
      fitted_signals = fitting_optimization(signals_parameters,
                                         Xdata,multiplicities,roof_effect)
      # signals_parameters=as.matrix(signals_parameters)
      dim(signals_parameters) = c(5, length(signals_parameters)/5)
      rownames(signals_parameters) = c(
        'intensity',
        'shift',
        'width',
        'gaussian',
        'J_coupling'
         )     
      signals_to_quantify=c(1,2)
      other_fit_parameters$signals_to_quantify=signals_to_quantify


      #Generation of output data about the fitting and of the necessary variables for the generation ofa figure
      output_data = output_generator(
        signals_to_quantify,
        fitted_signals,
        scaledYdata,
        Xdata,
        signals_parameters,multiplicities
      )

      output_data$intensity=signals_parameters[1, signals_to_quantify] * max(Ydata)
      output_data$width=signals_parameters[3, signals_to_quantify]

      #Generation of the dataframe with the final output variables
      results_to_save = data.frame(
        shift = output_data$shift,
        Area = output_data$Area * max(Ydata),
        signal_area_ratio = output_data$signal_area_ratio,
        fitting_error = output_data$fitting_error,
        intensity = output_data$intensity,
        width = output_data$width
      )

      #Adaptation of the quantification to de-scaled Ydata
      # results_to_save$Area = results_to_save$Area * max(Ydata)

      #Generation of the figure when the conditions specified in the Parameters file are accomplished
      plot_data = rbind(
        output_data$signals_sum,
        output_data$baseline_sum,
        output_data$fitted_sum,
        output_data$signals
      )
      rownames(plot_data) = c("signals_sum",
        "baseline_sum",
        "fitted_sum",
        as.character(ROI_profile[,4]),rep('additional signal',dim(plot_data)[1]-length(ROI_profile[,4])-3))
      r=1
      # plotdata = data.frame(Xdata=autorun_data$ppm[ROI_buckets], t(dataset[input$x1_rows_selected,ROI_buckets,drop=F]))
      plotdata = data.frame(Xdata, signals = plot_data[3 + other_fit_parameters$signals_to_quantify[r], ] * max(Ydata))
      plotdata2 = data.frame(Xdata,
        Ydata,
        plot_data[3, ] * max(Ydata),
        plot_data[2, ] * max(Ydata))
      plotdata3 <- melt(plotdata2, id = "Xdata")
      plotdata3$variable = c(
        rep('Original Spectrum', length(Ydata)),
        rep('Generated Spectrum', length(Ydata)),
        rep('Generated Background', length(Ydata))
      )
      plotdata4 = data.frame(Xdata, (t(plot_data[-c(1, 2, 3), , drop = F]) *
          max(Ydata)))
      plotdata5 = melt(plotdata4, id = "Xdata")
      # p=plot_ly(data=plotdata3,x=~Xdata,y=~value,color=~variable,type='scatter',mode='lines') %>% layout(xaxis = list(autorange = "reversed"))
      # p <- add_trace(p,data=plotdata5,x = ~Xdata,
      #   y = ~value,
      #   colour = 'Surrounding signals',
      #   group = ~variable)
      # p <- add_trace(p,data=plotdata5,x = ~Xdata,
      #   y = ~value,
      #   colour = 'Surrounding signals',
      #   group = ~variable)
      # plot_ly(data=plotdata3,x=~Xdata,y=~value,color=~variable,type='scatter',mode='lines') %>% layout(xaxis = list(autorange = "reversed"))
      p=ggplot() +
        geom_line(data = plotdata3,
          aes(
            x = Xdata,
            y = value,
            colour = variable,
            group = variable
          )) +
        geom_line(data = plotdata5,
          aes(
            x = Xdata,
            y = value,
            colour = 'Surrounding signals',
            group = variable
          )) +
        geom_area(
          data = plotdata,
          aes(
            x = Xdata,
            y = signals,
            position = 'fill',
            fill = 'Quantified Signal'
          )
        ) +
        scale_x_reverse() + labs(x='ppm',y='Intensity')
      # renderPlotly({
      #   ggplotly(p)
      # })
      # ggsave(paste(plot_path[other_fit_parameters$signals_to_quantify[r]],"Fit.jpeg",sep='/'),width = 10, height = 5)
      # plotgenerator(
      #   results_to_save,
      #   plot_data,
      #   Xdata,
      #   Ydata,
      #   fitted_signals,
      #   other_fit_parameters,
      #   signals_names,
      #   experiment_name,
      #   is_roi_testing,
      #   plot_path
      # )

      #Generation of output variables specific of every quantification
      # for (i in seq_along(plot_path)) {
      #   write.csv(
      #     import_excel_profile,
      #     file.path(plot_path[i],
      #               "import_excel_profile.csv")
      #     # row.names = F
      #   )
      #   write.table(Ydata,
      #               file.path(plot_path[i], "Ydata.csv"),
      #               # row.names = F,
      #               col.names = F)
      # 
      #   other_fit_parameters$signals_to_quantify=NULL
      # 
      #   write.csv(
      #     other_fit_parameters,
      #     file.path(plot_path[i],
      #               "other_fit_parameters.csv"),
      #     # row.names = F
      #   )
      #   write.table(fitted_signals,
      #               file.path(plot_path[i], "fitted_signals.csv"))
      #   # row.names = F,
      #   # col.names = F))
      #   write.table(plot_data,
      #               file.path(plot_path[i], "plot_data.csv"))
      #   # col.names = F)
      #   write.csv(FeaturesMatrix,
      #             file.path(plot_path[i], "FeaturesMatrix.csv"))
      #   # row.names = F)
      #   write.table(signals_parameters,
      #               file.path(plot_path[i],
      #                         "signals_parameters.csv"))
      #   # col.names = F
      #   write.table(Xdata,
      #               file.path(plot_path[i], "Xdata.csv"))
      #   # row.names = F,
      #   # col.names = F))
      #   write.table(Ydata,
      #               file.path(plot_path[i], "Ydata.csv"))
      #   write.csv(results_to_save,
      #             file.path(plot_path[i], "results_to_save.csv"),
      #             row.names = F)
      # 
      # }

    # }

    #Generation of output variables specific of every ROI

    finaloutput = save_output(
      spectrum_index,
      signals_codes,
      results_to_save,
      autorun_data$buck_step,
      finaloutput)
  #   )
  #   write.csv(finaloutput$Area,
  #             file.path(autorun_data$export_path,
  #                       "Area.csv"))
  #   write.csv(finaloutput$shift,
  #             file.path(autorun_data$export_path,
  #                       "shift.csv"))
  #   write.csv(finaloutput$width,
  #             file.path(autorun_data$export_path,
  #                       "width.csv"))
  #   write.csv(
  #     finaloutput$signal_area_ratio,
  #     file.path(autorun_data$export_path,
  #               "signal_area_ratio.csv")
  #   )
  #   write.csv(
  #     finaloutput$fitting_error,
  #     file.path(autorun_data$export_path,
  #               "fitting_error.csv")
  #   )
  #   write.csv(
  #     finaloutput$intensity,
  #     file.path(autorun_data$export_path,
  #               "intensity.csv")
  #   )
  # 
  #   # row.names = F,
  #   # col.names = F))
  # 
  # 
  # }
  # 
  # }
  # 
  # #Validation post-quantification system
  # # alarmmatrix=validation(finaloutput, other_fit_parameters)
  # # write.csv(alarmmatrix,
  # #           file.path(autorun_data$export_path, "alarmmatrix.csv"),
  # #           )
   
    signals_parameters=t(rbind(signals_parameters,multiplicities,roof_effect))
    blah=list()
    blah$signals_parameters=signals_parameters
    blah$p=p
    blah$finaloutput=finaloutput
  return(blah)
}
