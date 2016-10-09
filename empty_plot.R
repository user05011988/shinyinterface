empty_plot = function(autorun_data,input,sell) {

  if(is.null(input$x1_rows_selected)) {spectrum_index=1
  } else {spectrum_index=input$x1_rows_selected}
  
  
dataset=rbind(autorun_data$dataset,colMeans(autorun_data$dataset),apply(autorun_data$dataset,2,median))
rownames(dataset)[(dim(autorun_data$dataset)[1]+1):dim(dataset)[1]]=c('Mean spectrum', 'Median spectrum')
lol=which(round(autorun_data$ppm,6)==round(sell$mtcars[1,1],6))
lol2=which(round(autorun_data$ppm,6)==round(sell$mtcars[1,2],6))
# print(input$x1_rows_selected)
plotdata = data.frame(Xdata=autorun_data$ppm[lol:lol2], t(dataset[spectrum_index,lol:lol2,drop=F]))
# 
# plot_ly(data=plotdata,x=~Xdata,y=~Ydata)
plotdata3 <- melt(plotdata, id = "Xdata")

# print(plotdata3)
p=ggplot() +
  geom_line(data = plotdata3,
    aes(
      x = Xdata,
      y = value,
      colour = variable,
      group = variable
    ))+
  scale_x_reverse() + labs(x='ppm',y='Intensity')
return(p)
}