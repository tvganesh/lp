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
add.constraint(lprec, c(1, 1,1,1), "=",12)
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
