###Have 80 Red Blood cell
###Have 20 parasite

library(gganimate)
library(ggplot2)

###Let's say this is a data.frame of what it would look like

###Movement of all agents 
Movement<- function(Pop){

  move_x <- sample(x = c(-1, 0, 1), size = nrow(Pop),replace=TRUE)
  move_y <- sample(x = c(-1, 0, 1), size = nrow(Pop),replace=TRUE)
  
  Pop[,'x'] <- Pop[,'x'] + move_x
  Pop[,'y'] <- Pop[,'y'] + move_y
  
  ###
  for (p in seq(1,nrow(Pop))){
    
    if (is.na(Pop[p,'x']) != TRUE){
     
    
    #If going out of the right boundary
    if (Pop[p,'x'] > 100){
      Pop[p,'x'] <- 99
    }
    #If going out of the left boundary
    if (Pop[p,'x'] < 1){
      Pop[p,'x'] <- 2
    }
    #If going out of the top boundary
    if (Pop[p,'y'] > 100){
      Pop[p,'y'] <- 99
    }
    #If going out of the bottom boundary
    if (Pop[p,'y'] < 1){
      Pop[p,'y'] <- 2
    }
    }else{
    Pop[p,'x'] <- NA
    Pop[p,'y'] <- NA
    }
    }
  return (Pop)
  
}

###Infection of the RBC by a parasite 
Infection <- function(Pop){
  ###RBC
  RBC <-  subset(Pop, Pop$type=='R')
  ###Parasite
  Parasite <- subset(Pop,Pop$type=='P')
  ###Infected
  Infected_RBC <- subset(Pop,Pop$type=='I')
  
  Same_spots <- merge(RBC, Parasite, by =c('x','y'))
  
  if(nrow(Same_spots) > 0){
   RBC_ind <- Same_spots$individuals.x
   Parasite_ind <- Same_spots$individuals.y
    
   RBC[RBC$individuals %in% RBC_ind ,]$type<- "I"
   RBC[RBC$individuals%in% RBC_ind,]$infected<- "T"
   RBC[RBC$individuals%in%RBC_ind,]$time_infected <- 1
  
   Parasite[Parasite$individuals%in%Parasite_ind ,]$infect <-"T"
   Parasite[Parasite$individuals%in%Parasite_ind ,]$x <- NA
   Parasite[Parasite$individuals%in% Parasite_ind ,]$y <- NA
   
   Pop <- rbind(RBC,Parasite,Infected_RBC)
   Pop  <- Pop[order( Pop $individuals),]
   
 }
  else{
    Pop <- rbind(RBC,Parasite,Infected_RBC)
    Pop  <- Pop[order(  Pop $individuals),]
    
  }
  return(Pop)
  
}

###The natural death of a parasite if failing to infect
Death <- function(Pop){

  RBC <-     subset(Pop, Pop$type=='R')
  ###Parasite
  Parasite <- subset(Pop,Pop$type=='P')
  ###Infected
  Infected_RBC <- subset(Pop,Pop$type=='I')
  
  #Bye Bye
  Dead_Parasite<- subset(Parasite, Parasite$time_alive >10 & Parasite$infect == "F")
  
  if(nrow(Dead_Parasite) > 0){
  Dead_Parasite_Inds <-    Dead_Parasite$individuals
  
  Parasite[Parasite$individuals %in% Dead_Parasite_Inds,]$dead = 'T'
  Parasite[Parasite$individuals%in% Dead_Parasite_Inds,]$x = NA
  Parasite[Parasite$individuals%in% Dead_Parasite_Inds,]$y = NA
  
  Pop <- rbind(RBC,Parasite,Infected_RBC)
  Pop  <- Pop[order(Pop $individuals),]
  
}else{
  Pop <- rbind(RBC,Parasite,Infected_RBC)
  Pop  <- Pop[order(  Pop $individuals),]
  
}
return(Pop)
  
}

###Bursting of infected RBC  

Burst <- function(Pop){
  ###RBC
  RBC <-     subset(Pop, Pop$type=='R')
  ###Parasite
  Parasite <- subset(Pop,Pop$type=='P')
  ###Infected
  Infected_RBC <- subset(Pop,Pop$type=='I')
  
  #Bye Bye
Burst <- subset(Infected_RBC,  Infected_RBC$time_infected >5 & Infected_RBC$dead == "F")
  
  if(nrow(Burst) > 0){
    Burst_Ind <-  Burst$individuals
  
    Infected_RBC[Infected_RBC$individuals %in% Burst_Ind,]$dead = 'T'
  
    
    New_Parasites <- NULL
    for (ind in seq(1,length(Burst_Ind))){
      ind_no <- Burst_Ind[ind]
      x_pos <-  Infected_RBC[Infected_RBC$individual%in%ind_no,]$x
      y_pos <-  Infected_RBC[Infected_RBC$individual %in%ind_no,]$y
      
      
      move_x <- sample(x = c(-1, 0, 1), size = 5,replace=TRUE)
      move_y <- sample(x = c(-1, 0, 1), size = 5,replace=TRUE)
      
      x_pos_new<- x_pos + move_x
      y_pos_new <- y_pos + move_y
      
      for (p in seq(1,length(x_pos_new))){
        
        #If going out of the right boundary
        if (x_pos_new[p] > 100){
          x_pos_new[p] <- 99
        }
        #If going out of the left boundary
        if (x_pos_new[p] < 1){
          x_pos_new[p] <- 2
        }
        #If going out of the top boundary
        if ( y_pos_new[p] > 100){
          y_pos_new[p] <- 99
        }
        #If going out of the bottom boundary
        if ( y_pos_new[p] < 1){
          y_pos_new[p] <- 2
        }
      }
   
       New_Parasites[[ind]]<- 
        
        data.frame(
         type = rep("P",5),
          x =  x_pos_new,
          y =   y_pos_new ,
          time_alive = rep(0,5),
          time_infected = rep(0,5),
          infected = rep("F",5),
          dead = rep("F",5),
          infect = rep("F",5),
          time = unique(Pop$time)
        )
      
    }
    New_Parasites_F <- do.call(rbind,  New_Parasites )
    New_Parasites_F <-cbind(individuals=seq(1,nrow(New_Parasites_F)),
          New_Parasites_F)
      
    previous_last_entry<- tail(Pop,n=1)$individuals
    
    
    New_Parasites_F$individuals <-New_Parasites_F$individuals+ previous_last_entry
    
    Parasite_F <- rbind(Parasite,New_Parasites_F)
    
    Infected_RBC[Infected_RBC$individuals %in% Burst_Ind,]$x=NA
    Infected_RBC[Infected_RBC$individuals %in% Burst_Ind,]$y=NA
    
    
    
    Pop <- rbind(RBC,Parasite_F,Infected_RBC)
    Pop  <- Pop[order(Pop $individuals),]
    
  }else{
    Pop <- rbind(RBC,Parasite,Infected_RBC)
    Pop  <- Pop[order(Pop $individuals),]
  }
return(Pop)

}

  


Pop<- 
  data.frame(
    individuals = seq(1,200),
    type = c(rep("R",180), rep("P",20)),
    x = sample.int(100, 200,replace=TRUE),
    y = sample.int(100, 200,replace=TRUE),
    time_alive = rep(0,200),
    time_infected = rep(0,200),
    infected = rep("F",200),
    dead = rep("F",200),
    infect = rep("F",200),
    time = 0
  )



full_list <- NULL
for (k in seq(1,100)){

  Pop$time <- k
  Pop <- Burst(Pop)
  Pop <- Death(Pop)
  Pop <- Infection(Pop)
  Pop <- Movement(Pop)
  Pop[Pop$dead=="F",]$time_alive<- Pop[Pop$dead=="F",]$time_alive +1
  Pop[Pop$time_infected!=0,]$time_infected <-  Pop[Pop$time_infected!=0,]$time_infected+1
  

  
  full_list[[k]] <- Pop

}

Pop_All <- do.call(rbind,full_list )

animation<- ggplot(Pop_All, aes(x= x, y= y,fill=type))+
  geom_point(size=4,shape=21)+
  scale_fill_manual(name="",label=c("I"='Infected RBC', "R"='RBC', "P"= 'Merozoite'),
                     values=c("I"='#74f76d','R'='#d95981', 'P'='#97c7f7'))+
  transition_time(time)+ theme_void()+ labs(title = 'Minutes: {frame_time}')

#anim_save('animation.gif', animation = last_animation())
