###Have 80 Red Blood cell
###Have 20 parasite

library(gganimate)
library(ggplot2)

###Let's say this is a data.frame of what it would look like
Pop<- 
  
  data.frame(
    individuals = seq(1,100),
    type = c(rep("R",80), rep("P",20)),
    x = sample.int(100, 100),
    y = sample.int(100, 100),
    time_alive = rep(0,100),
    time_infected = rep(0,100),
    infected = rep("F",100),
    dead = rep("F",100),
    infect = rep("F",100)
  )



###Let's try getting them to move?
Movement<- function(Pop){

  move_x <- sample(x = c(-2, 0, 2), size = 100,replace=TRUE)
  move_y <- sample(x = c(-2, 0, 2), size = 100,replace=TRUE)
  
  Pop[,'x'] <- Pop[,'x'] + move_x
  Pop[,'y'] <- Pop[,'y'] + move_y
  
  ###
  for (p in seq(1,100)){
    
    #If going out of the right boundary
    if (Pop[p,'x'] < 100){
      Pop[p,'x'] <- 98
    }
    #If going out of the left boundary
    if (Pop[p,'x'] < 1){
      Pop[p,'x'] <- 3
    }
    #If going out of the top boundary
    if (Pop[p,'y'] < 100){
      Pop[p,'y'] <- 98
    }
    #If going out of the bottom boundary
    if (Pop[p,'y'] < 1){
      Pop[p,'y'] <- 3
    }
  return (Pop)
  }
}

Infection <- function(Pop){
  RBC <- subset(Pop, Pop$type=='R')
  
  Parasite <- subset(Pop,Pop$type=='P')
 
  
  id_RBC <- which(RBC[,'x'] %in%  Parasite[,'x'] &
          RBC[,'y'] %in%  Parasite[,'y'] )
  id_Parasite<- which(Parasite[,'x'] %in%  RBC[,'x'] &
                        Parasite[,'y'] %in%  RBC[,'y'] )

 if(length(id_RBC) >0){
   RBC[id_RBC,]$type<- "I"
   
   RBC[id_RBC,]$infected<- T
   RBC[id_RBC,]$time_infected<- RBC[id_RBC,]$time_infected+1
   Parasite[id_Parasite,]$infect <-T
   Parasite[id_Parasite,]$x <-NA
   Parasite[id_Parasite,]$y <-NA
   
   Pop <- rbind(RBC,Parasite)
 }
  else{
    Pop <- rbind(RBC,Parasite)
  }
  return(Pop)
  
}


full_list <- NULL

for (k in seq(1,10)){
  Pop <- Movement(Pop)
  Pop$time_alive <- Pop$time_alive+1
  Infection_DF <- Infection(Pop)
  Pop$type<-   Infection_DF$type
  
  Pop$time_infected <-   Infection_DF$time_infected
  Pop$infected <-  Infection_DF$infected
  Pop$infect <-   Infection_DF$infect
  Pop$time <- k
  full_list[[k]] <-Pop

}

Pop_All <- do.call(rbind,full_list )

ggplot(Pop_All, aes(x= x, y= y, color=type))+
  geom_point(size=2)+transition_time(time)+  ease_aes('linear')
  
