ROI_data = read.csv(autorun_data$profile_folder_path, sep = ";",stringsAsFactors = F)
dummy = which(!is.na(ROI_data[, 1]))
ROI_separator = cbind(dummy, c(dummy[-1] - 1, dim(ROI_data)[1]))
ROI_index=1  
  
  #Loading of every ROI parameters
  pre_import_excel_profile = ROI_data[ROI_separator[ROI_index, 1]:ROI_separator[ROI_index, 2],]
  ROI_limits = round(as.numeric(pre_import_excel_profile[1, 1:2]),3)
  if (ROI_limits[1] < ROI_limits[2])
    rev(ROI_limits)
  print(paste(ROI_limits[1], ROI_limits[2], sep = '-'))
  ROI_buckets = which(autorun_data$ppm <= ROI_limits[1] &
      autorun_data$ppm >=
      ROI_limits[2])
  preXdata = autorun_data$ppm[ROI_buckets]
  
  #Preparation of necessary parameters
  other_fit_parameters = fitting_variables()
  other_fit_parameters$freq = autorun_data$freq
  other_fit_parameters$ROI_buckets = ROI_buckets
  other_fit_parameters$buck_step = autorun_data$buck_step
  
  
  fitting_type = as.character(pre_import_excel_profile[1, 3])
  signals_to_quantify = which(pre_import_excel_profile[, 7] == 1)
  
  quartile_spectrum = as.numeric(apply(autorun_data$dataset[, other_fit_parameters$ROI_buckets,drop=F], 2, function(x)
    quantile(x, 0.75)))
  reference_spectrum_ind = which.min(apply(autorun_data$dataset[, other_fit_parameters$ROI_buckets,drop=F], 1, function(x)
    sqrt(mean((x - quartile_spectrum) ^ 2
    ))))
  
  Ydata = as.numeric(autorun_data$dataset[reference_spectrum_ind, ROI_buckets])
  if (fitting_type == "Clean Fitting" || fitting_type ==
      "Baseline Fitting") {
    is_roi_testing = "N"
    
    clean_fit = ifelse(fitting_type == "Clean Fitting", "Y",
      "N")
    
    #Parameters of every signal necessary for the fitting
    initial_fit_parameters = pre_import_excel_profile[, 5:11,drop=F]
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
    FeaturesMatrix = fitting_prep(preXdata,
      scaledYdata,
      initial_fit_parameters,
      other_fit_parameters)
    
    
    #Calculation of the parameters that will achieve the best fitting
    signals_parameters = fittingloop(FeaturesMatrix,
      preXdata,
      scaledYdata,
      other_fit_parameters)
    dim(signals_parameters) = c(5, (length(signals_parameters)/5))
    
    # pre_import_excel_profile[signals_to_quantify,6]=signals_parameters[3,signals_to_quantify]*autorun_data$freq
    cla=pre_import_excel_profile[signals_to_quantify,5]
    pre_import_excel_profile[signals_to_quantify,5]=signals_parameters[2,signals_to_quantify]
    ROI_limits = round(as.numeric(pre_import_excel_profile[1, 1:2])-cla-pre_import_excel_profile[signals_to_quantify,5],3)
    ROI_limits = round(as.numeric(pre_import_excel_profile[1, 1:2]),3)
    if (ROI_limits[1] < ROI_limits[2])
      rev(ROI_limits)
    print(paste(ROI_limits[1], ROI_limits[2], sep = '-'))
    ROI_buckets = which(autorun_data$ppm <= ROI_limits[1] &
        autorun_data$ppm >=
        ROI_limits[2])
    
  }
  
  
  #
  
  # automatic roi edition, if specified
  tot_import_excel_profile=list()
  tot_Xdata=list()
  
  if (other_fit_parameters$automatic_roi_edition=='Y') {
    
    for (i in 1:length(bd)) {
      dummy = automatic_roi_edition(autorun_data$dataset[bd[[i]],,drop=F],
        pre_import_excel_profile,
        preXdata,
        other_fit_parameters,ROI_limits,autorun_data$ppm)
      
      tot_import_excel_profile[[i]]=dummy$import_excel_profile
      # ROI_limits=dummy$ROI_limits
      # if (ROI_limits[1] < ROI_limits[2])
      #   rev(ROI_limits)
      # other_fit_parameters$ROI_buckets = which(autorun_data$ppm < ROI_limits[1] &
      #                                            autorun_data$ppm >
      #                                            ROI_limits[2])
      # Xdata[[i]] = autorun_data$ppm[other_fit_parameters$ROI_buckets]
      tot_Xdata[[i]] = autorun_data$ppm[which(autorun_data$ppm <= dummy$ROI_limits[1] &autorun_data$ppm >= dummy$ROI_limits[2])]
      
    }
    tot2_import_excel_profile=as.data.frame(matrix(NA,dim(pre_import_excel_profile)[1]*dim(autorun_data$dataset)[1],dim(pre_import_excel_profile)[2]),stringsAsFactors = F)
    tot2_Xdata=matrix(NA,dim(autorun_data$dataset)[1],length(preXdata))
    for (kkk in 1:length(bd)) {
      for (kkl in 1:length(bd[[kkk]])) {
        tot2_import_excel_profile[(dim(pre_import_excel_profile)[1]*bd[[kkk]][kkl]-dim(pre_import_excel_profile)[1]+1):(dim(pre_import_excel_profile)[1]*bd[[kkk]][kkl]),]=tot_import_excel_profile[[kkk]]
        tot2_Xdata[bd[[kkk]][kkl],]=tot_Xdata[[kkk]]
      }}
    
  } else {
    tot2_import_excel_profile=do.call(rbind, replicate(dim(autorun_data$dataset)[1], pre_import_excel_profile, simplify=FALSE))
    tot2_Xdata=matrix(rep(preXdata,each=dim(autorun_data$dataset)[1]),nrow=dim(autorun_data$dataset)[1])
    
  }
  
  
  
  # bf=apcluster(negDistMat(r=2),dd[be,])
  fitting_type = as.character(pre_import_excel_profile[1, 3])
  signals_to_quantify = which(pre_import_excel_profile[, 7] == 1)
  signals_codes = replicate(length(signals_to_quantify), NA)
  signals_names = replicate(length(signals_to_quantify), NA)
  j = 1
  for (i in signals_to_quantify) {
    k = which(autorun_data$signals_names == pre_import_excel_profile[i,
      4])
    signals_codes[j] = autorun_data$signals_codes[k]
    signals_names[j] = as.character(autorun_data$signals_names[k])
    j = j + 1
  }
  
  #Quantification for every experiment
  
  spectrum_index=1
  
    print(spectrum_index)
    
    Xdata=round(as.numeric(tot2_Xdata[spectrum_index,]),3)
    import_excel_profile=tot2_import_excel_profile[(dim(pre_import_excel_profile)[1]*spectrum_index-dim(pre_import_excel_profile)[1]+1):(dim(pre_import_excel_profile)[1]*spectrum_index),]
    
    #Preparation of necessary variables and folders to store figures and information of the fitting
    Ydata = as.numeric(autorun_data$dataset[spectrum_index, ROI_buckets])
    
    experiment_name = autorun_data$Experiments[[spectrum_index]]
    plot_path = file.path(autorun_data$export_path,
      experiment_name,
      signals_names)
    for (i in seq_along(plot_path))
      if (!dir.exists(plot_path[i]))
        dir.create(plot_path[i])
    
    #If the quantification is through integration with or without baseline
    
      is_roi_testing = "N"
      
      clean_fit = ifelse(fitting_type == "Clean Fitting", "Y",
        "N")
      
      #Parameters of every signal necessary for the fitting
      initial_fit_parameters = import_excel_profile[, 5:11,drop=F]
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
      # scaledYdata = as.vector(Ydata / (max(Ydata)))
      scaledYdata = as.vector(Ydata)
      
      #Other parameters necessary for the fitting independent of the type of signal
      
      other_fit_parameters$clean_fit = clean_fit
      
      #Adaptation of the info of the parameters into a single matrix and preparation (if necessary) of the background signals that will conform the baseline
      FeaturesMatrix = fitting_prep(Xdata,
        scaledYdata,
        initial_fit_parameters,
        other_fit_parameters)
      