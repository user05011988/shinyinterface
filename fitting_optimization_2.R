#Aquesta ?s la funci? adaptada i R.
fitting_optimization_2 = function(parS, Xdata,multipli,roof_eff,ROI_indicator) {
  # lol=which(!is.na(dummy[,1]))
  # parS[lol*5-4]=dummy[lol,2]*parS[dummy[lol,1]*5-4]
  i = as.numeric(parS[seq(1, length(parS) - 4, 5)])
  p = as.numeric(parS[seq(2, length(parS) - 3, 5)])
  w = as.numeric(parS[seq(3, length(parS) - 2, 5)])
  g = as.numeric(parS[seq(4, length(parS) - 1, 5)])
  j = as.numeric(parS[seq(5, length(parS) - 0, 5)])
  signals_parameters=fitted_signals=multiplicities=roof_effect=list()
  for (ind in 1:max(ROI_indicator)) {
  signals_parameters[[ind]]=rbind(i,p,w,g,j)[,ROI_indicator==ind]
  fitted_signals[[ind]] = matrix(NaN, dim(signals_parameters[[ind]])[2], length(Xdata[[ind]]))
  multiplicities[[ind]]=multipli[ROI_indicator==ind]
    roof_effect[[ind]]=roof_eff[ROI_indicator==ind]
  }

  # multiplicities = as.numeric(parS[seq(6, length(parS) - 1, 7)])
  # roof_effect = as.numeric(parS[seq(7, length(parS) - 0, 7)])

for (ind in 1:length(roof_effect)) {
  for (s in seq_along(multiplicities[[ind]])) {
    if (roof_effect[[ind]][s] > 0) {
      if (multiplicities[[ind]][s] == 1)   {
        fitted_signals[[ind]][s, ] = peakpvoigt(
          c(
            signals_parameters[[ind]][1, s],
            signals_parameters[[ind]][2, s],
            signals_parameters[[ind]][3, s],
            signals_parameters[[ind]][4, s]
          ),
          Xdata[[ind]]
        )
      } else if (multiplicities[[ind]][s] == 2) {
        fitted_signals[[ind]][s, ] = peakpvoigt(c(
          signals_parameters[[ind]][1, s]/(1 - roof_effect[[ind]][s]),
          (signals_parameters[[ind]][2, s] - signals_parameters[[ind]][5, s]),
          signals_parameters[[ind]][3, s],
          signals_parameters[[ind]][4, s]
        ),
        Xdata[[ind]]) + peakpvoigt(
          c(
            signals_parameters[[ind]][1, s],
            (signals_parameters[[ind]][2, s] + signals_parameters[[ind]][5, s]),
            signals_parameters[[ind]][3, s],
            signals_parameters[[ind]][4, s]
          ),
          Xdata[[ind]]
        )
      } else if (multiplicities[[ind]][s] == 3) {
        y=1/(2 + roof_effect[[ind]][s])
        x= 1-y
        fitted_signals[[ind]][s, ] = peakpvoigt(
          c(
            signals_parameters[[ind]][1, s] *x,
            (signals_parameters[[ind]][2, s] - signals_parameters[[ind]][5, s]),
            signals_parameters[[ind]][3, s],
            signals_parameters[[ind]][4, s]
          ),
          Xdata[[ind]]
        ) + peakpvoigt(
          c(
            signals_parameters[[ind]][1, s],
            signals_parameters[[ind]][2, s],
            signals_parameters[[ind]][3, s],
            signals_parameters[[ind]][4, s]
          ),
          Xdata[[ind]]
        ) + peakpvoigt(
          c(
            signals_parameters[[ind]][1, s] *y,
            (signals_parameters[[ind]][2, s] + signals_parameters[[ind]][5, s]),
            signals_parameters[[ind]][3, s],
            signals_parameters[[ind]][4, s]
          ),
          Xdata[[ind]]
        )
      } else if (multiplicities[[ind]][s] == 4) {
        #     fitted_signals[[ind]][s, ] = peakpvoigt(
        #       c(
        #         signals_parameters[[ind]][1, s] / 3,
        #         (signals_parameters[[ind]][2, s] - 3 * signals_parameters[[ind]][5, s]),
        #         signals_parameters[[ind]][3, s],
        #         signals_parameters[[ind]][4, s]
        #       ),
        #       Xdata[[ind]]
        #     ) + peakpvoigt(c(
        #       signals_parameters[[ind]][1, s],
        #       (signals_parameters[[ind]][2, s] - signals_parameters[[ind]][5, s]),
        #       signals_parameters[[ind]][3, s],
        #       signals_parameters[[ind]][4, s]
        #     ),
        #     Xdata[[ind]]) + peakpvoigt(c(
        #       signals_parameters[[ind]][1, s],
        #       (signals_parameters[[ind]][2, s] + signals_parameters[[ind]][5, s]),
        #       signals_parameters[[ind]][3, s],
        #       signals_parameters[[ind]][4, s]
        #     ),
        #     Xdata[[ind]]) + peakpvoigt(
        #       c(
        #         signals_parameters[[ind]][1, s] / 3,
        #         (signals_parameters[[ind]][2, s] + 3 * signals_parameters[[ind]][5, s]),
        #         signals_parameters[[ind]][3, s],
        #         signals_parameters[[ind]][4, s]
        #       ),
        #       Xdata[[ind]]
        #     )
      }
    } else if (roof_effect[[ind]][s] == 0) {
      if (multiplicities[[ind]][s] == 0) {
        fitted_signals[[ind]][s, ] = peakpvoigt(
          c(
            signals_parameters[[ind]][1, s],
            signals_parameters[[ind]][2, s],
            signals_parameters[[ind]][3, s],
            signals_parameters[[ind]][4, s]
          ),
          Xdata[[ind]]
        )
      } else if (multiplicities[[ind]][s] == 1) {
        fitted_signals[[ind]][s, ] = peakpvoigt(
          c(
            signals_parameters[[ind]][1, s],
            signals_parameters[[ind]][2, s],
            signals_parameters[[ind]][3, s],
            signals_parameters[[ind]][4, s]
          ),
          Xdata[[ind]]
        )
      } else if (multiplicities[[ind]][s] == 2) {
        fitted_signals[[ind]][s, ] = peakpvoigt(c(
          signals_parameters[[ind]][1, s],
          (signals_parameters[[ind]][2, s] - signals_parameters[[ind]][5, s]),
          signals_parameters[[ind]][3, s],
          signals_parameters[[ind]][4, s]
        ),
        Xdata[[ind]]) + peakpvoigt(c(
          signals_parameters[[ind]][1, s],
          (signals_parameters[[ind]][2, s] + signals_parameters[[ind]][5, s]),
          signals_parameters[[ind]][3, s],
          signals_parameters[[ind]][4, s]
        ),
        Xdata[[ind]])
      } else if (multiplicities[[ind]][s] == 3) {
        fitted_signals[[ind]][s, ] = peakpvoigt(
          c(
            signals_parameters[[ind]][1, s] / 2,
            (signals_parameters[[ind]][2, s] - signals_parameters[[ind]][5, s]),
            signals_parameters[[ind]][3, s],
            signals_parameters[[ind]][4, s]
          ),
          Xdata[[ind]]
        ) + peakpvoigt(
          c(
            signals_parameters[[ind]][1, s],
            signals_parameters[[ind]][2, s],
            signals_parameters[[ind]][3, s],
            signals_parameters[[ind]][4, s]
          ),
          Xdata[[ind]]
        ) + peakpvoigt(
          c(
            signals_parameters[[ind]][1, s] / 2,
            (signals_parameters[[ind]][2, s] + signals_parameters[[ind]][5, s]),
            signals_parameters[[ind]][3, s],
            signals_parameters[[ind]][4, s]
          ),
          Xdata[[ind]]
        )
      } else if (multiplicities[[ind]][s] == 4) {
        # fitted_signals[[ind]][s, ] = peakpvoigt(
        #   c(
        #     signals_parameters[[ind]][1, s] / 3,
        #     (signals_parameters[[ind]][2, s] - 3 * signals_parameters[[ind]][5, s]),
        #     signals_parameters[[ind]][3, s],
        #     signals_parameters[[ind]][4, s]
        #   ),
        #   Xdata[[ind]]
        # ) + peakpvoigt(c(
        #   signals_parameters[[ind]][1, s] ,
        #   (signals_parameters[[ind]][2, s] - signals_parameters[[ind]][5, s]) ,
        #   signals_parameters[[ind]][3, s],
        #   signals_parameters[[ind]][4, s]
        # ),
        # Xdata[[ind]]) + peakpvoigt(c(
        #   signals_parameters[[ind]][1, s],
        #   (signals_parameters[[ind]][2, s] + signals_parameters[[ind]][5, s]),
        #   signals_parameters[[ind]][3, s],
        #   signals_parameters[[ind]][4, s]
        # ),
        # Xdata[[ind]]) + peakpvoigt(
        #   c(
        #     signals_parameters[[ind]][1, s] / 3,
        #     (signals_parameters[[ind]][2, s] + 3 * signals_parameters[[ind]][5, s]) ,
        #     signals_parameters[[ind]][3, s],
        #     signals_parameters[[ind]][4, s]
        #   ),
        #   Xdata[[ind]]
        # )
      }
    } else if (roof_effect[[ind]][s] < 0) {
      if (multiplicities[[ind]][s] == 1) {
        fitted_signals[[ind]][s, ] = peakpvoigt(
          c(
            signals_parameters[[ind]][1, s],
            signals_parameters[[ind]][2, s],
            signals_parameters[[ind]][3, s],
            signals_parameters[[ind]][4, s]
          ),
          Xdata[[ind]]
        )
      } else if (multiplicities[[ind]][s] == 2) {
        fitted_signals[[ind]][s, ] = peakpvoigt(
          c(
            signals_parameters[[ind]][1, s],
            (signals_parameters[[ind]][2, s] - signals_parameters[[ind]][5, s]),
            signals_parameters[[ind]][3, s],
            signals_parameters[[ind]][4, s]
          ),
          Xdata[[ind]]
        ) + peakpvoigt(c(
          signals_parameters[[ind]][1, s] *
            (1 - roof_effect[[ind]][s]) ,
          (signals_parameters[[ind]][2, s] + signals_parameters[[ind]][5, s]),
          signals_parameters[[ind]][3, s],
          signals_parameters[[ind]][4, s]
        ),
        Xdata[[ind]])
      } else if (multiplicities[[ind]][s] == 3) {
        y=1/(1 + roof_effect[[ind]][s])
        x= 1-y
        fitted_signals[[ind]][s, ] = peakpvoigt(
          c(
            signals_parameters[[ind]][1, s]*x,
            (signals_parameters[[ind]][2, s] - signals_parameters[[ind]][5, s]),
            signals_parameters[[ind]][3, s]*y,
            signals_parameters[[ind]][4, s]
          ),
          Xdata[[ind]]
        ) + peakpvoigt(
          c(
            signals_parameters[[ind]][1, s],
            signals_parameters[[ind]][2, s],
            signals_parameters[[ind]][3, s],
            signals_parameters[[ind]][4, s]
          ),
          Xdata[[ind]]
        ) + peakpvoigt(
          c(
            signals_parameters[[ind]][1, s] / 2 * (1 - roof_effect[[ind]][s]),
            (signals_parameters[[ind]][2, s] + signals_parameters[[ind]][5, s]) ,
            signals_parameters[[ind]][3, s],
            signals_parameters[[ind]][4, s]
          ),
          Xdata[[ind]]
        )
      } else if (multiplicities[[ind]][s] == 4) {
        # fitted_signals[[ind]][s, ] = peakpvoigt(
        #   c(
        #     signals_parameters[[ind]][1, s] / 3 ,
        #     (signals_parameters[[ind]][2, s] - 3 * signals_parameters[[ind]][5, s]),
        #     signals_parameters[[ind]][3, s],
        #     signals_parameters[[ind]][4, s]
        #   ),
        #   Xdata[[ind]]
        # ) + peakpvoigt(c(
        #   signals_parameters[[ind]][1, s],
        #   (signals_parameters[[ind]][2, s] - signals_parameters[[ind]][5, s]),
        #   signals_parameters[[ind]][3, s],
        #   signals_parameters[[ind]][4, s]
        # ),
        # Xdata[[ind]]) + peakpvoigt(c(
        #   signals_parameters[[ind]][1, s],
        #   (signals_parameters[[ind]][2, s] + signals_parameters[[ind]][5, s]),
        #   signals_parameters[[ind]][3, s],
        #   signals_parameters[[ind]][4, s]
        # ),
        # Xdata[[ind]]) + peakpvoigt(
        #   c(
        #     signals_parameters[[ind]][1, s] / 3,
        #     (signals_parameters[[ind]][2, s] + 3 * signals_parameters[[ind]][5, s]),
        #     signals_parameters[[ind]][3, s],
        #     signals_parameters[[ind]][4, s]
        #   ),
        #   Xdata[[ind]]
        # )
      }
    }
  }
}
  # mam=c()
  # for (ind in 1:length(fitted_signals)) mam=c(mam,colSums(fitted_signals[[ind]]))
  # return(mam)
return(fitted_signals)

}
