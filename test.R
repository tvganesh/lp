

library("lpSolveAPI")

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

lprec <- make.lp(0, 12)
lp.control(lprec, sense="min")

set.objfn(lprec, c(gambhirMalinga$SR, yusufMalinga$SR,kallisMalinga$SR,uthappaMalinga$SR,
                   gambhirHarbhajan$SR,yusufHarbhajan$SR,kallisHarbhajan$SR,uthappaHarbhajan$SR,
                   gambhirPollard$SR,yusufPollard$SR,kallisPollard$SR,uthappaPollard$SR))


add.constraint(lprec, c(1, 1,1,1, 0,0,0,0, 0,0,0,0), "<=",4)
add.constraint(lprec, c(0,0,0,0,1,1,1,1,0,0,0,0), "<=",4)
add.constraint(lprec, c(0,0,0,0,0,0,0,0,1,1,1,1), "<=",4)
add.constraint(lprec, c(1,1,1,1, 1,1,1,1,1,1,1,1), "=",10)


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