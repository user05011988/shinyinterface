fittingloop = function(FeaturesMatrix,
  Xdata,
  Ydata,
  other_fit_parameters) {
  
  
  #Created by Daniel Ca?ueto 30/08/2016
  #Calculation through least squares algorithm of parameters that achieve the best fitting.
  
  # There are several optimization iterations and the one with less error is chosen.
  # The number of iter depends on the complexity of the ROI.
  
  # After the first fitting there can be further ones, depending on the need to add additional signals
  # to adapt the signals and ROI information provided by the user to the concrete
  # characteristics of the spectrum
  
  #TO DO: ideally, multiplicity and roof effect should not be incorporated into the optimization. Maybe this is ithe cause of the improper input parameters message. However, it would be necessary to load it from a txt file in every iteration, or with a global variable. I guess this procedures would make it much more slower. But it can be checked.
  #TO DO: another revision of algorithm alternatives
  
  
  #Depending on the complexity of the ROI, more or less iterations are performed
  if (is.numeric(other_fit_parameters$fitting_maxiter)) {
    fitting_maxiter = other_fit_parameters$fitting_maxiter
  } else {
    if (dim(FeaturesMatrix)[1] > 8 |
        any(FeaturesMatrix[, 4] - FeaturesMatrix[, 3] > 0.01)) {
      fitting_maxiter = 10
    } else if ((dim(FeaturesMatrix)[1] > 5 &&
        dim(FeaturesMatrix)[1] < 9)) {
      fitting_maxiter = 7
    } else {
      fitting_maxiter = 4
    }
  }
  
  #Necessary information to incorporate additional singals if necessary
  signals_to_quantify = which(FeaturesMatrix[, 11] != 0)
  range_ind = round(
    other_fit_parameters$additional_signal_ppm_distance / other_fit_parameters$buck_step
  )
  signals_to_add = other_fit_parameters$signals_to_add
  
  #Variables to initialize the loop
  # try_error=000
  error2 = 3000
  iterrep = 0
  fitting_maxiterrep = other_fit_parameters$fitting_maxiterrep
  
  #Function where to find a minimum
  residFun <-
    function(par, observed, xx,multiplicities,roof_effect,freq)
      observed - colSums(fitting_optimization(par, xx,multiplicities,roof_effect,observed,freq))
  
  
  # Loop to control if additional signals are incorporated, until a maximum of iterations specified bt fitting_maxiterrep.
  # If at the last fitting the improvement was lesser than 25% respective to the previous fitting,
  # iterrep becomes equal to fitting_maxiterrep and the loop is stooped
  while (iterrep < fitting_maxiterrep) {
    # print(iterrep)
    iter = 0
    errorprov = 3000
    error1 = 3000
    worsterror = 0
    if (iterrep>0) print(FeaturesMatrix[,2])

    
    # bounds = list(ub = matrix(0, dim(FeaturesMatrix)[1], (dim(FeaturesMatrix)[2] /
    #                                                         2) - 2), lb = matrix(0, dim(FeaturesMatrix)[1], (dim(FeaturesMatrix)[2] /
    #                                                                                                            2) - 2))
    lb = as.vector(t(FeaturesMatrix[, seq(1, 9, 2), drop = F]))
    ub = as.vector(t(FeaturesMatrix[, seq(2, 10, 2), drop = F]))
    multiplicities=FeaturesMatrix[,11]
    roof_effect=FeaturesMatrix[,12]
   
    
    while (error1 > other_fit_parameters$errorprov &
        error1 > (1 / 3 * worsterror) & iter < fitting_maxiter) {
      #Algorithm options that can be changed
      #Preparation of lower and upper bounds
      # bounds=list(ub=matrix(0,dim(FeaturesMatrix)[1],dim(FeaturesMatrix)[2]/2),lb=matrix(0,dim(FeaturesMatrix)[1],dim(FeaturesMatrix)[2]/2))
      
      #Initialization of parameters to optimize. In every iteration the initialization will be different
      s0 = lb + (ub - lb) * runif(length(ub))
      # if (iterrep %in% seq(1,16,3)) s0[2]=lb[2] + (ub[2] - lb[2]) * runif(1,min=0,max=1/3)
      # if (iterrep %in% seq(2,17,3)) s0[2]=lb[2] + (ub[2] - lb[2]) * runif(1,min=1/3,max=2/3)
      # if (iterrep %in% seq(3,18,3)) s0[2]=lb[2] + (ub[2] - lb[2]) * runif(1,min=2/3,max=1)
      # # s0[which(seq_along(s0)%%5==3)]=s0[which(seq_along(s0)%%5==3)]/1.5
      # s0[which(seq_along(s0)%%5==5)]=s0[which(seq_along(s0)%%5==5)]/2
      s0[which(seq_along(s0)%%5!=2)]=lb[which(seq_along(s0)%%5!=2)] + (ub[which(seq_along(s0)%%5!=2)] - lb[which(seq_along(s0)%%5!=2)]) * runif(1,min=0,max=(iterrep%%3)/3)
      #
      # if (exists('nls.out')) {
      #   s0=paramprov+ (ub-lb)*0.2*matrix(runif(dim(lb)[1] * dim(lb)[2],min=-1,max=1), dim(lb)[1], dim(lb)[2])
      #   s0[(s0-lb)<0]=lb[(s0-lb)<0]
      #   s0[(ub-s0)<0]=ub[(ub-s0)<0]
      #
      #   }
      #
      # print(ple)
      nls.out <-
        nls.lm(
          par = s0,
          fn = residFun,
          observed = Ydata,
          xx = Xdata,
          multiplicities=multiplicities,
          roof_effect=roof_effect,
        freq=other_fit_parameters$freq,
          lower = lb,
          upper = ub,
          control = nls.lm.control(
            factor = other_fit_parameters$factor,
            maxiter = other_fit_parameters$nls_lm_maxiter,
            ftol = other_fit_parameters$ftol,
            ptol = other_fit_parameters$ptol
          )
          
        )
      
      iter = iter + 1
      
      # #Procedure to calculate the fititng error in all the ROI
      #An adapted MSE error is calculated, and the parameters of the optimization with less MSE are stored
      errorprov = (sqrt(nls.out$deviance / length(Ydata))) * 100 / (max(Ydata) -
          min(Ydata))
      if (is.nan(errorprov) || is.na(errorprov))
        errorprov = error1
      
      if (errorprov < error1) {
        error1 = errorprov
        paramprov=coef(nls.out)

      } else if (errorprov > worsterror) {
        worsterror = errorprov
      }
      # if (dim(FeaturesMatrix)[1]>8) try_error=0
    }
    
    if (iterrep>0) print(paramprov)
    
    
    iter = 0
    errorprov = 3000
    error1 = 3000
    lb[which(seq_along(lb)%%5!=3)]=paramprov[which(seq_along(lb)%%5!=3)]
    ub[which(seq_along(lb)%%5!=3)]=paramprov[which(seq_along(lb)%%5!=3)]

    
    while (iter < 1) {
    
      s0 = lb + (ub - lb) * runif(length(ub))

      
      nls.out <-
        nls.lm(
          par = s0,
          fn = residFun,
          observed = Ydata,
          xx = Xdata,
          multiplicities=multiplicities,
          roof_effect=roof_effect,
          freq=other_fit_parameters$freq,
          lower = lb,
          upper = ub,
          control = nls.lm.control(
            factor = other_fit_parameters$factor,
            maxiter = other_fit_parameters$nls_lm_maxiter,
            ftol = other_fit_parameters$ftol,
            ptol = other_fit_parameters$ptol
          )
          
        )
      
      iter = iter + 1
      
      # #Procedure to calculate the fititng error in all the ROI
      #An adapted MSE error is calculated, and the parameters of the optimization with less MSE are stored
      errorprov = (sqrt(nls.out$deviance / length(Ydata))) * 100 / (max(Ydata) -
          min(Ydata))
      if (is.nan(errorprov) || is.na(errorprov))
        errorprov = error1
      
      if (errorprov < error1) {
        error1 = errorprov
        paramprov=coef(nls.out)
        
      } else if (errorprov > worsterror) {
        worsterror = errorprov
      }
      # if (dim(FeaturesMatrix)[1]>8) try_error=0
    }
    
    
    
    dummy = error2
    
    
    #If the incorporation of additional signals has improved the fitting the parameters are updated
    if (error1 < error2) {
      error2 = error1
      # error = errorprov

      signals_parameters = paramprov
      
      
      # print(errorprov)
      # print(signals_parameters[4,])
      
    }
    # print(error2)
    #If the fitting seems to be still clearly improvable through the addition of signals
    if (error2 < (other_fit_parameters$additional_signal_improvement * dummy) &
        (error1 > other_fit_parameters$additional_signal_percentage_limit)) {
      # print(iterrep)
      #Finding of signals in the vector of the difference between the fitted and the original ROI
      lol = peakdet(nls.out$fvec, other_fit_parameters$peakdet_minimum)
      if (is.null(lol$maxtab) == F) {
        #Preparation of information of where signals of interest are located
        lolll = matrix(
          paramprov,
          nrow = length(FeaturesMatrix[, 11]),
          ncol = 5,
          byrow = TRUE
        )
        points_to_avoid = rbind(abs(
          matrix(
            Xdata,
            nrow = length(signals_to_quantify),
            ncol = length(Xdata),
            byrow = TRUE
          ) - matrix(
            lolll[signals_to_quantify, 2] - lolll[signals_to_quantify, 5],
            nrow = length(signals_to_quantify),
            ncol = length(Xdata)
          )
        ), abs(
          matrix(
            Xdata,
            nrow = length(signals_to_quantify),
            ncol = length(Xdata),
            byrow = TRUE
          ) - matrix(
            lolll[signals_to_quantify, 2] + lolll[signals_to_quantify, 5],
            nrow = length(signals_to_quantify),
            ncol = length(Xdata)
          )
        ))
        points_to_avoid = apply(points_to_avoid, 1, which.min)
        seq_range = c()
        for (i in-range_ind:range_ind)
          seq_range = append(seq_range, points_to_avoid - i)
        
        #Finding of posible additional signals to incorporate if there are not in zones where the signals o interest are located
        lol2 = cbind(lol$maxtab$pos, lol$maxtab$val)
        lol3 = matrix(NA, 0, 2)
        for (i in 1:dim(lol2)[1]) {
          if (any(abs(points_to_avoid - lol2[i, 1]) < range_ind) == F)
            lol3 = rbind(lol3, lol2[i, ])
        }
        
        if (dim(lol3)[1] > signals_to_add) {
          #Selection of more intense additional signals
          ad = sort(lol3[, 2],
            decreasing = T,
            index.return = T)$ix
          lol3 = lol3[ad[1:min(signals_to_add, length(ad))], , drop = F]
        }
        #Creation of rows to incorporate to FeaturesMatrix
        dummy = matrix(
          FeaturesMatrix[1, ],
          nrow = dim(lol3)[1],
          ncol = length(FeaturesMatrix[1, ]),
          byrow = TRUE
        )
        # dummy[, 2] = lol3[, 2]
        dummy[, 3] = Xdata[lol3[, 1]] - 0.001
        dummy[, 4] = Xdata[lol3[, 1]] + 0.001
        dummy[, 9] = rep(0, dim(lol3)[1])
        dummy[, 10] = rep(0, dim(lol3)[1])
        
        FeaturesMatrix = rbind(FeaturesMatrix, dummy)
        multiplicities=c(FeaturesMatrix[,11],rep(1,dim(lol3)[1]))
        roof_effect=c(FeaturesMatrix[,12],rep(0,dim(lol3)[1]))
        iterrep = iterrep + 1
      }
    } else {
      iterrep = fitting_maxiterrep
    }
    
  }
  
  return(signals_parameters)
}