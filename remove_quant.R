remove_quant=function(input,autorun_data,final_output) {
  ind1=input$quant_selection_cell_clicked$row
  ind2=input$quant_selection_cell_clicked$col
  

finaloutput$Area[ind1,ind2]=finaloutput$shift[ind1,ind2]=finaloutput$Area[ind1,ind2]=finaloutput$width[ind1,ind2]=finaloutput$signal_area_ratio[ind1,ind2]=finaloutput$fitting_error[ind1,ind2]=finaloutput$intensity[ind1,ind2]=NA
write.csv(finaloutput$Area,
  file.path(autorun_data$export_path,
    "Area.csv"))
write.csv(finaloutput$shift,
  file.path(autorun_data$export_path,
    "shift.csv"))
write.csv(finaloutput$width,
  file.path(autorun_data$export_path,
    "width.csv"))
write.csv(
  finaloutput$signal_area_ratio,
  file.path(autorun_data$export_path,
    "signal_area_ratio.csv")
)
write.csv(
  finaloutput$fitting_error,
  file.path(autorun_data$export_path,
    "fitting_error.csv")
)
write.csv(
  finaloutput$intensity,
  file.path(autorun_data$export_path,
    "intensity.csv")
)

return(finaloutput)
}