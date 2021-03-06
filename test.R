

B1	B2	Obj Fun(=4*B1+3*B2)
1	3	13
2	2	14
3	1	15

a <- c(1,3,13)
b <- c(2,2,14)
c <- c(3,1,15)

d <- as.data.frame(rbind(a,b,c))
names(d) <- c("B1","B2","Obj func=4*B1+3*B2")
rownames(d) <- c("opt1","opt2","opt3")

e <- as.data.frame(rbind(c(1,3,13),c(2,2,14),c(3,1,15)))
names(e) <- c("B1","B2","Obj func=4*B1+3*B2")
rownames(e) <- c("opt1","opt2","opt3")


e <- as.data.frame(rbind(c(1,1,1),c(0,3,0),c(2,0,0),c(3,4,1)))
names(e) <- c("S Watson","B Lee","MA Starc")
rownames(e) <- c("Kohli","Yuvraj","Dhoni","overs")
e


e <- as.data.frame(rbind(c(2,0,2),c(0, 3, 0),c(1, 0, 2),c(0,0,0),c(3,3,4)))
names(e) <- c("Malinga","Harbhajan","Pollard")
rownames(e) <- c("Gambhir","Yusuf","Kallis","Uthappa","Overs")
e 
                   
                   
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