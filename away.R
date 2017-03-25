prepare <- read.csv("/home/lemess/Documents/data/winner.csv")
enquete <- factor(prepare$AwayTeam) #I sort the data by the awayteam
enquete
enquete <- split(x=prepare["Winner"],f=enquete) # for each team who win the match
enquete
taille <- enquete$`Paris SG`$Winner
length(enquete$`PSG(champion2016)`$Winner)
victoire <- 0
NUL <- 0
defaite <- 0
enquete

mat <-matrix(nrow=4,ncol=3)
colnames(mat) <- c("Victory", "Draw", "Defeat")
rownames(mat) <- c("Paris","Monaco","Nice","PSG(champion2016)")

#I create a matrix for know, for each team the numbers of victories, draw and defeat


for(i in 1:11)
{
  
  if(enquete$`Paris SG`$Winner[i] =="Paris SG")
    victoire <- victoire + 1
    
  else if(enquete$`Paris SG`$Winner[i] == "N")
    NUL <- NUL + 1
    
  else if(enquete$`Paris SG`$Winner[i] != "N" && enquete$`Paris SG`$Winner[i] !="Paris SG" )
    defaite <- defaite+ 1;
  
} 

# 1:11, in my dataset i have 11 aways games per team
# i do a loop for Paris SG team:
#for each game, if Paris SG win, I increment the victoire variable
# the same for when Paris draw or lose the game
# the "N" letter mean draw



mat["Paris","Victory"] <- victoire
mat["Paris","Draw"] <- NUL
mat["Paris","Defeat"] <- defaite

#i add the result of the previous loop in the matrix
mat 
victoire <- 0
NUL <- 0
defaite <- 0

for(i in 1:11){
  
  if(enquete$`PSG(champion2016)`$Winner[i] =="PSG(champion2016)")
    victoire <- victoire + 1
    
  else if(enquete$`PSG(champion2016)`$Winner[i] == "N")
    NUL <- NUL + 1
   
  else if(enquete$`PSG(champion2016)`$Winner[i] != "N" && enquete$`PSG(champion2016)`$Winner[i] !="Champion2016" )
    defaite <- defaite+ 1;

}  

# 1:11, in my dataset i have 11 aways games per team
# i do a loop for Champion 2016 team:
#for each game, if Champion 2016, I increment the victoire variable
# the same for when Champion 2016 draw or lose the game
# the "N" letter mean draw






mat["PSG(champion2016)","Victory"] <- victoire
mat["PSG(champion2016)","Draw"] <- NUL
mat["PSG(champion2016)","Defeat"] <- defaite
mat
victoire <- 0
NUL <- 0
defaite <- 0


for(i in 1:11){
  
  if(enquete$Monaco$Winner[i] =="Monaco")
    victoire <- victoire + 1
    
  else if(enquete$Monaco$Winner[i] == "N")
    NUL <- NUL + 1
   
  else if(enquete$Monaco$Winner[i] != "N" && enquete$Monaco$Winner[i] !="Monaco" )
    defaite <- defaite+ 1;
} 


# 1:11, in my dataset i have 11 aways games per team
# i do a loop for Monaco team:
#for each game, if Monaco win, I increment the victoire variable
# the same for when Monaco draw or lose the game
# the "N" letter mean draw

mat["Monaco","Victory"] <- victoire
mat["Monaco","Draw"] <- NUL
mat["Monaco","Defeat"] <- defaite

#i add the result of the previous loop in the matrix for the Monaco team

victoire<- 0
NUL <- 0
defaite <- 0
for(i in 1:11){

  if(enquete$Nice$Winner[i] =="Nice")
    victoire <- victoire + 1
   
  else if(enquete$Nice$Winner[i] == "N")
    NUL <- NUL + 1
   
  
  else if(enquete$Nice$Winner[i] != "N" && enquete$Nice$Winner[i] !="Nice" )
    defaite <- defaite+ 1;
} 


# i do a loop for Nice team:
#for each game, if Nice win, I increment the victoire variable
# the same for when Nice draw or lose the game
# the "N" letter mean draw

mat["Nice","Victory"] <- victoire
mat["Nice","Draw"] <- NUL
mat["Nice","Defeat"] <- defaite

#i add the result of the previous loop in the matrix for the Nice team






test <- barplot(mat,beside=TRUE,legend.text = TRUE,col=c("blue","red","green","yellow"),ylim=c(0,11),main="Away results"
                ,xlab="",ylab="Numbers of matches",args.legend = list(x ='topright', bty='n', inset=c(-0.,-0.1)))
text(test, 0, round(mat, 2),cex=1,pos=3)  



# finally i plot the result thanks to my matrix  













