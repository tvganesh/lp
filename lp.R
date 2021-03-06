# 1.
library("lpSolveAPI")
lprec <- make.lp(0, 2)
lp.control(lprec, sense="min")
set.objfn(lprec, c(4, 3))
add.constraint(lprec, c(1, 1), "=",4)
add.constraint(lprec, c(1, 0), ">",1)
add.constraint(lprec, c(0, 1), ">",1)
lprec
solve(lprec)
get.objective(lprec) # 13
get.variables(lprec) # 1    3 

# 2. 
# 5 0 7 0 
library("lpSolveAPI")
lprec <- make.lp(0, 4)
lp.control(lprec, sense="min")
set.objfn(lprec, c(3, 4,2,5))
add.constraint(lprec, c(1, 1,0,0), "<=",8)
add.constraint(lprec, c(0, 0,1,1), "<=",7)
add.constraint(lprec, c(1, 1,1,1), "=",12)
lprec
solve(lprec)
get.objective(lprec) # 29
get.variables(lprec) # 5 0 7 0

# 3.
library("lpSolveAPI")
lprec <- make.lp(0, 4)
lp.control(lprec, sense="min")
set.objfn(lprec, c(4, 2,2,5))
add.constraint(lprec, c(1, 1,0,0), "<=",8)
add.constraint(lprec, c(0, 0,1,1), "<=",7)
add.constraint(lprec, c(1, 1,1,1), "=",5)
add.constraint(lprec, c(1, 0,0,0), ">",1)
add.constraint(lprec, c(0, 1,0,0), ">",1)
add.constraint(lprec, c(0, 0,1,0), ">",1)
add.constraint(lprec, c(0, 0,0,1), ">",1)
lprec
solve(lprec)
get.objective(lprec) # 35
get.variables(lprec) # 1 6 2 3


library("lpSolveAPI")
lprec <- make.lp(0, 4)
lp.control(lprec, sense="min")
set.objfn(lprec, c(4, 2,2,5))
add.constraint(lprec, c(1, 1,0,0), "<=",3)
add.constraint(lprec, c(0, 0,1,1), "<=",3)
add.constraint(lprec, c(1, 1,1,1), "=",5)
add.constraint(lprec, c(1, 0,0,0), ">",1)
add.constraint(lprec, c(0, 1,0,0), ">",1)
add.constraint(lprec, c(0, 0,1,0), ">",1)
add.constraint(lprec, c(0, 0,0,1), ">",1)
lprec
solve(lprec)
get.objective(lprec) #   15
get.variables(lprec) # 1 2 2 1

#B1	B2	B1	B2	Runs
#1	1	1	2	18
#1	2	1	1	15
#2	1	1	1	17



lprec <- make.lp(0, 2)
lp.control(lprec, sense="max")
set.objfn(lprec, c(143, 60))
add.constraint(lprec, c(120, 210), "<=", 15000)
add.constraint(lprec, c(110, 30), "<=", 4000)
add.constraint(lprec, c(1, 1), "<=", 75)
solve(lprec)
get.objective(lprec)

library(lpSolve)
f.obj <- c(3, 4)
f.con <- matrix (c(1, 1,1,0,0,1), nrow=3, byrow=TRUE)
f.dir <- c("<=")
f.rhs <- c(5)
f.rhs <- c(2)
f.rhs <- c(3)
lp ("max", f.obj, f.con, f.dir, f.rhs)
load("Chennai Super Kings-Mumbai Indians-allMatches.RData")

library(dplyr)

a <- matches %>% filter(batsman=="G Gambhir" & bowler=="SL Malinga") 
gambhirMalinga <- a %>% summarize(totalRuns=sum(runs),count=n()) %>% mutate(SR=(totalRuns/count)*100)
a <- matches %>% filter(batsman=="G Gambhir" & bowler=="Harbhajan Singh") 
gambhirHarbhajan <- a %>% summarize(totalRuns=sum(runs),count=n()) %>% mutate(SR=(totalRuns/count)*100)


computeSR <- function(batsman1,bowler1){
    a <- matches %>% filter(batsman==batsman1 & bowler==bowler1) 
    a1 <- a %>% summarize(totalRuns=sum(runs),count=n()) %>% mutate(SR=(totalRuns/count)*6)
    a1
}

# Gambhir
gambhirMalinga <- computeSR("G Gambhir","SL Malinga")
gambhirHarbhajan <- computeSR("G Gambhir","Harbhajan Singh")
gambhirPollard <- computeSR("G Gambhir","KA Pollard")




#Yusuf Pathan
yusufMalinga <- computeSR("YK Pathan","SL Malinga")
yusufHarbhajan <- computeSR("YK Pathan","Harbhajan Singh")
yusufPollard <- computeSR("YK Pathan","KA Pollard")

#JH Kallis
kallisMalinga <- computeSR("JH Kallis","SL Malinga")
kallisHarbhajan <- computeSR("JH Kallis","Harbhajan Singh")
kallisPollard <- computeSR("JH Kallis","KA Pollard")



#RV Uthappa
uthappaMalinga <- computeSR("RV Uthappa","SL Malinga")
uthappaHarbhajan <- computeSR("RV Uthappa","Harbhajan Singh")
uthappaPollard <- computeSR("RV Uthappa","KA Pollard")



#Minimize Z
 Z=  gambhirMalinga$SR*o11+yusufMalinga$SR*o12+kallisMalinga$SR*o13 + uthappaMalinga$SR*o14+
    gambhirHarbhajan$SR*o21 + yusufHarbhajan$SR*o22+kallisHarbhajan*o23 +uthappaHarbhajan$SR*o24 +
    gambhirPollard$SR*o31+yusufPollard$SR*o32 + kallisPollard$SR*o33 +uthappaPollard$SR*o34

 
# where o11,o12,o13,o14 <- Overs of Malinga to 4 KKR batsmen
#    o21,o22,o23,o24 <- oVers of Harbhajan
#    o31,o32,o33,o34 <- over of Pollard
 
#Lets assume that there are 12 overs remaining
#1 <= oij <= 4
 
#Writing the equations
 library("lpSolveAPI")
 lprec <- make.lp(0, 4)
 lp.control(lprec, sense="min")
 set.objfn(lprec, c(4, 2,2,5))
 add.constraint(lprec, c(1, 1,0,0), "<=",3)
 add.constraint(lprec, c(0, 0,1,1), "<=",3)
 add.constraint(lprec, c(1, 1,1,1), "=",5)
 add.constraint(lprec, c(1, 0,0,0), ">",1)
 add.constraint(lprec, c(0, 1,0,0), ">",1)
 add.constraint(lprec, c(0, 0,1,0), ">",1)
 add.constraint(lprec, c(0, 0,0,1), ">",1)
 lprec
 solve(lprec)
 get.objective(lprec) #   15
 get.variables(lprec) # 1 2 2 1
 

 library("lpSolveAPI")
 lprec <- make.lp(0, 12)
 lp.control(lprec, sense="min")


 set.objfn(lprec, c(gambhirMalinga$SR, yusufMalinga$SR,kallisMalinga$SR,uthappaMalinga$SR,
                    gambhirHarbhajan$SR,yusufHarbhajan$SR,kallisHarbhajan$SR,uthappaHarbhajan$SR,
                    gambhirPollard$SR,yusufPollard$SR,kallisPollard$SR,uthappaPollard$SR))
 add.constraint(lprec, c(1, 1,1,1,0,0,0,0,0,0,0,0), "<=",4)
 add.constraint(lprec, c(0,0,0,0,1,1,1,1,0,0,0,0), "<=",4)
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,1,1,1,1), "<=",4)
 
 add.constraint(lprec, c(1,1,1,1,1,1,1,1,1,1,1,1), "<=",10)
 
 add.constraint(lprec, c(1,0,0,0,0,0,0,0,0,0,0,0), ">=",0)
 #add.constraint(lprec, c(1,0,0,0,0,0,0,0,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,1,0,0,0,0,0,0,0,0,0,0), ">=",0)
 #add.constraint(lprec, c(0,1,0,0,0,0,0,0,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,1,0,0,0,0,0,0,0,0,0), ">=",0)
# add.constraint(lprec, c(0,0,1,0,0,0,0,0,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,1,0,0,0,0,0,0,0,0), ">=",0)
 #add.constraint(lprec, c(0,0,0,1,0,0,0,0,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,1,0,0,0,0,0,0,0), ">=",0)
 #add.constraint(lprec, c(0,0,0,0,1,0,0,0,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,0,1,0,0,0,0,0,0), ">=",0)
 #add.constraint(lprec, c(0,0,0,0,0,1,0,0,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,0,0,1,0,0,0,0,0), ">=",0)
 #add.constraint(lprec, c(0,0,0,0,0,0,1,0,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,0,0,0,1,0,0,0,0), ">=",0)
 #add.constraint(lprec, c(0,0,0,0,0,0,0,1,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,1,0,0,0), ">=",0)
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,1,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,0,1,0,0), ">=",0)
 #add.constraint(lprec, c(0,0,0,0,0,0,0,0,0,1,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,0,0,1,0), ">=",0)
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,0,0,1,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,0,0,0,1), ">=",0)
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,0,0,0,1), "<=",4)
 
 ###########################################
 lprec
 solve(lprec)
 get.objective(lprec) #   
 get.variables(lprec) # 
 
 
 library("lpSolveAPI")
 lprec <- make.lp(0, 4)
 lp.control(lprec, sense="min")
 
 set.objfn(lprec, c(gambhirMalinga$SR, yusufMalinga$SR,
                    gambhirHarbhajan$SR,yusufHarbhajan$SR
                    ))
 add.constraint(lprec, c(1, 1,0,0), "<=",4)
 add.constraint(lprec, c(0,0,1,1), "<=",4)
 add.constraint(lprec, c(1,1,1,1), "=",6)
 
 add.constraint(lprec, c(1, 0,0,0), ">",1)
 #add.constraint(lprec, c(1, 0,0,0), "<=",4)
 add.constraint(lprec, c(0, 1,0,0), ">",1)
 #add.constraint(lprec, c(0, 1,0,0), "<=",4)
 add.constraint(lprec, c(0, 0,1,0), ">",1)
 #add.constraint(lprec, c(0, 0,1,0), "<=",4)
 add.constraint(lprec, c(0, 0,0,1), ">",1)
 #add.constraint(lprec, c(0, 0,0,1), "<=",4)
 lprec
 solve(lprec)
 get.objective(lprec) #   
 get.variables(lprec) # 
 
 ########################################################6######################## # 3 batsman
 lprec
 solve(lprec)
 get.objective(lprec) #   
 get.variables(lprec) # 
 
 
 library("lpSolveAPI")
 lprec <- make.lp(0, 6)
 lp.control(lprec, sense="min")
 
 set.objfn(lprec, c(gambhirMalinga$SR, yusufMalinga$SR,kallisMalinga$SR,
                    gambhirHarbhajan$SR,yusufHarbhajan$SR,kallisHarbhajan$SR))
 
 add.constraint(lprec, c(1, 1,0,0,0,0), "<=",4)
 add.constraint(lprec, c(0,0,1,1,0,0), "<=",4)
 add.constraint(lprec, c(0,0,0,0,1,1,), "<=",4)
 add.constraint(lprec, c(1,1,1,1,1,1), "=",8)
 
 add.constraint(lprec, c(1, 0,0,0,0,0), ">",1)
 #add.constraint(lprec, c(1, 0,0,0), "<=",4)
 add.constraint(lprec, c(0, 1,0,0,0,0), ">",1)
 #add.constraint(lprec, c(0, 1,0,0), "<=",4)
 add.constraint(lprec, c(0, 0,1,0,0,0), ">",2)
 #add.constraint(lprec, c(0, 0,1,0), "<=",4)
 add.constraint(lprec, c(0, 0,0,1,0,0), ">",1)
 #add.constraint(lprec, c(0, 0,0,1), "<=",4)
 
 add.constraint(lprec, c(0,0, 0,0,1,0), ">",1)
 add.constraint(lprec, c(0,0, 0,0,0,1), ">",1)
 lprec
 solve(lprec)
 get.objective(lprec) #  
 get.variables(lprec) # 
 
 
 
 ########################################################6######################## # 4 batsman
 lprec
 solve(lprec)
 get.objective(lprec) #   
 get.variables(lprec) # 
 
 
 library("lpSolveAPI")
 lprec <- make.lp(0, 12)
 lp.control(lprec, sense="min")
 
 set.objfn(lprec, c(gambhirMalinga$SR, yusufMalinga$SR,kallisMalinga$SR,uthappaMalinga$SR,
                    gambhirHarbhajan$SR,yusufHarbhajan$SR,kallisHarbhajan$SR,uthappaHarbhajan$SR,
                    gambhirPollard$SR,yusufPollard$SR,kallisPollard$SR,uthappaPollard$SR))
 
 add.constraint(lprec, c(1, 1,1,1, 0,0,0,0, 0,0,0,0), "<=",4)
 add.constraint(lprec, c(0,0,0,0,1,1,1,1,0,0,0,0), "<=",4)
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,0,1,1,1,1), "<=",4)
 add.constraint(lprec, c(1,1,1,1,1,1,1,1,1,1,1,1), "=",10)

 
 add.constraint(lprec, c(1,0,0,0,0,0,0,0,0,0,0,0), ">=",1)
 #add.constraint(lprec, c(1,0,0,0,0,0,0,0,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,1,0,0,0,0,0,0,0,0,0,0), ">=",0)
 #add.constraint(lprec, c(0,1,0,0,0,0,0,0,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,1,0,0,0,0,0,0,0,0,0), ">=",2)
 # add.constraint(lprec, c(0,0,1,0,0,0,0,0,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,1,0,0,0,0,0,0,0,0), ">=",0)
 #add.constraint(lprec, c(0,0,0,1,0,0,0,0,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,1,0,0,0,0,0,0,0), ">=",1)
 #add.constraint(lprec, c(0,0,0,0,1,0,0,0,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,0,1,0,0,0,0,0,0), ">=",0)
 #add.constraint(lprec, c(0,0,0,0,0,1,0,0,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,0,0,1,0,0,0,0,0), ">=",1)
 #add.constraint(lprec, c(0,0,0,0,0,0,1,0,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,0,0,0,1,0,0,0,0), ">=",0)
 #add.constraint(lprec, c(0,0,0,0,0,0,0,1,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,1,0,0,0), ">=",1)
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,1,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,0,1,0,0), ">=",0)
 #add.constraint(lprec, c(0,0,0,0,0,0,0,0,0,1,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,0,0,1,0), ">=",0)
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,0,0,1,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,0,0,0,1), ">=",0)
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,0,0,0,1), "<=",4)
 
 lprec
 solve(lprec)
 get.objective(lprec) #  
 get.variables(lprec) # 
 
 
 
 ######################################### Maximizing
 lprec
 solve(lprec)
 get.objective(lprec) #   
 get.variables(lprec) # 
 
 
 library("lpSolveAPI")
 lprec <- make.lp(0, 12)
 lp.control(lprec, sense="max")
 
 set.objfn(lprec, c(gambhirMalinga$SR, yusufMalinga$SR,kallisMalinga$SR,uthappaMalinga$SR,
                    gambhirHarbhajan$SR,yusufHarbhajan$SR,kallisHarbhajan$SR,uthappaHarbhajan$SR,
                    gambhirPollard$SR,yusufPollard$SR,kallisPollard$SR,uthappaPollard$SR))
 
 add.constraint(lprec, c(1, 1,1,1, 0,0,0,0, 0,0,0,0), "<=",4)
 add.constraint(lprec, c(0,0,0,0,1,1,1,1,0,0,0,0), "<=",4)
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,0,1,1,1,1), "<=",4)
 add.constraint(lprec, c(1,1,1,1,1,1,1,1,1,1,1,1), "=",10)
 
 
 add.constraint(lprec, c(1,0,0,0,0,0,0,0,0,0,0,0), ">=",1)
 #add.constraint(lprec, c(1,0,0,0,0,0,0,0,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,1,0,0,0,0,0,0,0,0,0,0), ">=",0)
 #add.constraint(lprec, c(0,1,0,0,0,0,0,0,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,1,0,0,0,0,0,0,0,0,0), ">=",2)
 # add.constraint(lprec, c(0,0,1,0,0,0,0,0,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,1,0,0,0,0,0,0,0,0), ">=",0)
 #add.constraint(lprec, c(0,0,0,1,0,0,0,0,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,1,0,0,0,0,0,0,0), ">=",1)
 #add.constraint(lprec, c(0,0,0,0,1,0,0,0,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,0,1,0,0,0,0,0,0), ">=",0)
 #add.constraint(lprec, c(0,0,0,0,0,1,0,0,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,0,0,1,0,0,0,0,0), ">=",1)
 #add.constraint(lprec, c(0,0,0,0,0,0,1,0,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,0,0,0,1,0,0,0,0), ">=",0)
 #add.constraint(lprec, c(0,0,0,0,0,0,0,1,0,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,1,0,0,0), ">=",1)
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,1,0,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,0,1,0,0), ">=",0)
 #add.constraint(lprec, c(0,0,0,0,0,0,0,0,0,1,0,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,0,0,1,0), ">=",0)
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,0,0,1,0), "<=",4)
 
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,0,0,0,1), ">=",0)
 add.constraint(lprec, c(0,0,0,0,0,0,0,0,0,0,0,1), "<=",4)
 
 lprec
 solve(lprec)
 get.objective(lprec) #  
 get.variables(lprec) # 
 
 
 
 
 
 
 ##############################T20
 load("Australia-India-allMatches.RData")
 
 # Kohli
kohliWatson<- computeSR("V Kohli","SR Watson")
kohliWatson

kohliLee <- computeSR("V Kohli","B Lee")
kohliLee

kohliStarc <- computeSR("V Kohli","MA Starc")
kohliStarc
 

# Yuvraj
yuvrajWatson<- computeSR("Yuvraj Singh","SR Watson")
yuvrajWatson

yuvrajLee <- computeSR("Yuvraj Singh","B Lee")
yuvrajLee

yuvrajStarc <- computeSR("Yuvraj Singh","MA Starc")
yuvrajStarc
 
# MS Dhoni
dhoniWatson<- computeSR("MS Dhoni","SR Watson")
dhoniWatson

dhoniLee <- computeSR("MS Dhoni","B Lee")
dhoniLee

dhoniStarc <- computeSR("MS Dhoni","MA Starc")
dhoniStarc



library("lpSolveAPI")
lprec <- make.lp(0, 9)
lp.control(lprec, sense="max")

set.objfn(lprec, c(kohliWatson$SR, kohliLee$SR,kohliStarc$SR,
                   yuvrajWatson$SR,yuvrajLee$SR,yuvrajStarc$SR,
                   dhoniWatson$SR,dhoniLee$SR,dhoniStarc$SR))

add.constraint(lprec, c(1, 1,1,0,0,0, 0,0,0), "<=",3)
add.constraint(lprec, c(0,0,0,1,1,1,0,0,0), "<=",3)
add.constraint(lprec, c(0,0,0,0,0,0,1,1,1), "<=",3)
add.constraint(lprec, c(1,1,1,1,1,1,1,1,1), "=",7)


add.constraint(lprec, c(1,0,0,0,0,0,0,0,0), ">=",0)
#add.constraint(lprec, c(1,0,0,0,0,0,0,0,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,1,0,0,0,0,0,0,0), ">=",0)
#add.constraint(lprec, c(0,1,0,0,0,0,0,0,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,0,1,0,0,0,0,0,0), ">=",0)
# add.constraint(lprec, c(0,0,1,0,0,0,0,0,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,0,0,1,0,0,0,0,0), ">=",0)
#add.constraint(lprec, c(0,0,0,1,0,0,0,0,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,0,0,0,1,0,0,0,0), ">=",0)
#add.constraint(lprec, c(0,0,0,0,1,0,0,0,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,0,0,0,0,1,0,0,0), ">=",0)
#add.constraint(lprec, c(0,0,0,0,0,1,0,0,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,0,0,0,0,0,1,0,0), ">=",0)
#add.constraint(lprec, c(0,0,0,0,0,0,1,0,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,0,0,0,0,0,0,1,0), ">=",0)
#add.constraint(lprec, c(0,0,0,0,0,0,0,1,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,0,0,0,0,0,0,0,1), ">=",0)

lprec
solve(lprec)
get.objective(lprec) #  
get.variables(lprec) # 


################################################################################### Bowlingg
# RA Jadeja
jadejaWatson<- computeSR("SR Watson","RA Jadeja")
jadejaWatson

jadejaFinch <- computeSR("AJ Finch","RA Jadeja")
jadejaFinch

jadejaWarner <- computeSR("DA Warner","RA Jadeja")
jadejaWarner


# Ashwin
ashwinWatson<- computeSR("SR Watson","R Ashwin")
ashwinWatson

ashwinFinch <- computeSR("AJ Finch","R Ashwin")
ashwinFinch

ashwinWarner <- computeSR("DA Warner","R Ashwin")
ashwinWarner

# JJ Bunrah
bumrahWatson<- computeSR("SR Watson","JJ Bumrah")
bumrahWatson

bumrahFinch <- computeSR("AJ Finch","JJ Bumrah")
bumrahFinch

bumrahWarner <- computeSR("DA Warner","JJ Bumrah")
bumrahWarner



lprec <- make.lp(0, 9)
lp.control(lprec, sense="min")

set.objfn(lprec, c(jadejaWatson$SR, jadejaFinch$SR,jadejaWarner$SR,
                   ashwinWatson$SR,ashwinFinch$SR,ashwinWarner$SR,
                   bumrahWatson$SR,bumrahFinch$SR,bumrahWarner$SR))

add.constraint(lprec, c(1, 1,1,0,0,0, 0,0,0), "<=",4)
add.constraint(lprec, c(0,0,0,1,1,1,0,0,0), "<=",3)
add.constraint(lprec, c(0,0,0,0,0,0,1,1,1), "<=",4)
add.constraint(lprec, c(1,1,1,1,1,1,1,1,1), "=",8)


add.constraint(lprec, c(1,0,0,0,0,0,0,0,0), ">=",0)
#add.constraint(lprec, c(1,0,0,0,0,0,0,0,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,1,0,0,0,0,0,0,0), ">=",0)
#add.constraint(lprec, c(0,1,0,0,0,0,0,0,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,0,1,0,0,0,0,0,0), ">=",0)
# add.constraint(lprec, c(0,0,1,0,0,0,0,0,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,0,0,1,0,0,0,0,0), ">=",0)
#add.constraint(lprec, c(0,0,0,1,0,0,0,0,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,0,0,0,1,0,0,0,0), ">=",0)
#add.constraint(lprec, c(0,0,0,0,1,0,0,0,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,0,0,0,0,1,0,0,0), ">=",0)
#add.constraint(lprec, c(0,0,0,0,0,1,0,0,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,0,0,0,0,0,1,0,0), ">=",0)
#add.constraint(lprec, c(0,0,0,0,0,0,1,0,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,0,0,0,0,0,0,1,0), ">=",0)
#add.constraint(lprec, c(0,0,0,0,0,0,0,1,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,0,0,0,0,0,0,0,1), ">=",0)

lprec
solve(lprec)
get.objective(lprec) #  
get.variables(lprec) # 
 

###################################
lprec <- make.lp(0, 9)
lp.control(lprec, sense="min")

set.objfn(lprec, c(jadejaWatson$SR, jadejaFinch$SR,jadejaWarner$SR,
                   ashwinWatson$SR,ashwinFinch$SR,ashwinWarner$SR,
                   bumrahWatson$SR,bumrahFinch$SR,bumrahWarner$SR))

add.constraint(lprec, c(1, 1,1,0,0,0, 0,0,0), "<=",4)
add.constraint(lprec, c(0,0,0,1,1,1,0,0,0), "<=",3)
add.constraint(lprec, c(0,0,0,0,0,0,1,1,1), "<=",4)
add.constraint(lprec, c(1,1,1,1,1,1,1,1,1), "=",10)


add.constraint(lprec, c(1,0,0,0,0,0,0,0,0), ">=",1)
#add.constraint(lprec, c(1,0,0,0,0,0,0,0,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,1,0,0,0,0,0,0,0), ">=",0)
#add.constraint(lprec, c(0,1,0,0,0,0,0,0,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,0,1,0,0,0,0,0,0), ">=",1)
# add.constraint(lprec, c(0,0,1,0,0,0,0,0,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,0,0,1,0,0,0,0,0), ">=",0)
#add.constraint(lprec, c(0,0,0,1,0,0,0,0,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,0,0,0,1,0,0,0,0), ">=",1)
#add.constraint(lprec, c(0,0,0,0,1,0,0,0,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,0,0,0,0,1,0,0,0), ">=",0)
#add.constraint(lprec, c(0,0,0,0,0,1,0,0,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,0,0,0,0,0,1,0,0), ">=",0)
#add.constraint(lprec, c(0,0,0,0,0,0,1,0,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,0,0,0,0,0,0,1,0), ">=",1)
#add.constraint(lprec, c(0,0,0,0,0,0,0,1,0,0,0,0), "<=",4)

add.constraint(lprec, c(0,0,0,0,0,0,0,0,1), ">=",0)

lprec
solve(lprec)
get.objective(lprec) #  
get.variables(lprec) # 