prepare <- read.csv("/home/lemess/Documents/data/winner.csv")
enquete <- factor(prepare$HomeTeam)
enquete <- split(x=prepare["Winner"],f=enquete)
enquete
taille <- enquete$`Paris SG`$Winner
length(enquete$`PSG(champion2016)`$Winner)

enquete
moyenne_point <-matrix(nrow=4,ncol=11)
rownames(moyenne_point) <- c("Paris","Monaco","Nice","Champion2016")
moyenne_point
# i create matrix with 4 teams , for know the average of point win per game
# after each game
for(i in 1:11){
  
  if(enquete$`Paris SG`$Winner[i] =="Paris SG")
  {
    if(i==1)
      moyenne_point["Paris",i] = 3
    
    else if(i!=1)
      moyenne_point["Paris",i] = ((moyenne_point["Paris",i-1]*(i-1))+3)/i
  }
  
  else if(enquete$`Paris SG`$Winner[i] == "N")
  {
    
    if(i==1)
      moyenne_point["Paris",i] = 1
    
    else if(i!=1)
      moyenne_point["Paris",i] = ((moyenne_point["Paris",i-1]*(i-1))+1)/i
    
  }
  
  else if(enquete$`Paris SG`$Winner[i] != "N" && enquete$`Paris SG`$Winner[i] !="Paris SG" )
  {
    
    if(i==1)
      moyenne_point["Paris",i] = 0
    
    else if(i!=1)
      moyenne_point["Paris",i] = ((moyenne_point["Paris",i-1]*(i-1))+0)/i
    
  }
  
  
  
} 
#for Paris:
#for each game, i compute the mean of point win 
# i add the result into the "moyenne_point result" matrix
#the computation is note very easy
# i will not explain that
#but trust me it's true.





for(i in 1:11){
  
  if(enquete$`PSG(champion2016)`$Winner[i] =="PSG(champion2016)")
  {
    
    if(i==1)
      moyenne_point["Champion2016",i] = 3
    
    else if(i!=1)
      moyenne_point["Champion2016",i] = ((moyenne_point["Champion2016",i-1]*(i-1))+3)/i
  }
  
  
  
  else if(enquete$`PSG(champion2016)`$Winner[i] == "N")
  {
    
    if(i==1)
      moyenne_point["PSG(champion2016",i] = 1
    
    else if(i!=1)
      moyenne_point["Champion2016",i] = ((moyenne_point["Champion2016",i-1]*(i-1))+1)/i
    
  }
  
  else if(enquete$`PSG(champion2016)`$Winner[i] != "N" && enquete$`PSG(champion2016)`$Winner[i] !="Champion2016" )
  {
    
    if(i==1)
      moyenne_point["Champion2016",i] = 0
    
    else if(i!=1)
      moyenne_point["Champion2016",i] = ((moyenne_point["Champion2016",i-1]*(i-1))+0)/i
    
  }
  
}

#for Champion:
#for each game, i compute the mean of point win 
# i add the result into the "moyenne_point result" matrix



for(i in 1:11){
  
  if(enquete$Monaco$Winner[i] =="Monaco")
  {
    
    if(i==1)
      moyenne_point["Monaco",i] = 3
    
    else if(i!=1)
      moyenne_point["Monaco",i] = ((moyenne_point["Monaco",i-1]*(i-1))+3)/i
  
    }
  
  
  
  else if(enquete$Monaco$Winner[i] == "N")
  {
   
    if(i==1)
      moyenne_point["Monaco",i] = 1
    
    else if(i!=1)
      moyenne_point["Monaco",i] = ((moyenne_point["Monaco",i-1]*(i-1))+1)/i
    
  }
  
  else if(enquete$Monaco$Winner[i] != "N" && enquete$Monaco$Winner[i] !="Monaco" )
  {
    
    if(i==1)
      moyenne_point["Monaco",i] = 0
    
    else if(i!=1)
      moyenne_point["Monaco",i] = ((moyenne_point["Monaco",i-1]*(i-1))+0)/i
    
  }
  
} 
#for Monaco:
#for each game, i compute the mean of point win 
# i add the result into the "moyenne_point result" matrix


for(i in 1:11){
  
  if(enquete$Nice$Winner[i] =="Nice")
  {
    
    if(i==1)
      moyenne_point["Nice",i] = 3
    
    else if(i!=1)
      moyenne_point["Nice",i] = ((moyenne_point["Nice",i-1]*(i-1))+3)/i  
  }
  
  else if(enquete$Nice$Winner[i] == "N")
  {
    
    if(i==1)
      moyenne_point["Nice",i] = 1
    
    else if(i!=1)
      moyenne_point["Nice",i] = ((moyenne_point["Nice",i-1]*(i-1))+1)/i
    
    
  }
  
  else if(enquete$Nice$Winner[i] != "N" && enquete$Nice$Winner[i] !="Nice" )
  {
    defaite <- defaite+ 1;
    if(i==1)
      moyenne_point["Nice",i] = 0
    
    else if(i!=1)
      moyenne_point["Nice",i] = ((moyenne_point["Nice",i-1]*(i-1))+0)/i
    
  }
  
}

#for Nice: 
#for each game, i compute the mean of point win 
# i add the result into the "moyenne_point result" matrix
#the computation is note very easy
# i will not explain that
#but trust me it's true.


plot_colors <- c("blue","red","forestgreen","black")


#each team -> different color for the plot

plot(moyenne_point["Paris",], type="o", col=plot_colors[1],
     ylim=c(2,3), axes=FALSE, ann=FALSE)

axis(2,las=3, at=0.5*4:1000)


axis(1, at=1:11, colnames(moyenne_point))
box()
lines(moyenne_point["Monaco",], type="o", pch=22, lty=2, 
      col=plot_colors[2])
lines(moyenne_point["Nice",], type="o", pch=23, lty=3, 
      col=plot_colors[3])
lines(moyenne_point["Champion2016",], type="o", pch=23, lty=3, 
      col=plot_colors[4])
title(main="Average of points win per match", col.main="red", font.main=4)
legend(2, 3, rownames(moyenne_point), cex=0.66, col=plot_colors, 
       pch=21:23, lty=1:3)


# finally i plot the result, each line correspond to one team 

