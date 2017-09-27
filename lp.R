library("lpSolveAPI")
lprec <- make.lp(0, 2)
lp.control(lprec, sense="min")
set.objfn(lprec, c(4, 3))
add.constraint(lprec, c(1, 1), "=",4)
add.constraint(lprec, c(1, 0), ">",1)
add.constraint(lprec, c(0, 1), ">",1)
lprec
solve(lprec)
get.objective(lprec)
get.variables(lprec)



library("lpSolveAPI")
lprec <- make.lp(0, 4)
lp.control(lprec, sense="min")
set.objfn(lprec, c(3, 4,2,5))
add.constraint(lprec, c(1, 1,0,0), "<=",8)
add.constraint(lprec, c(0, 0,1,1), "<=",7)
add.constraint(lprec, c(1, 1,1,1), "=",12)
lprec
solve(lprec)
get.objective(lprec)
get.variables(lprec)


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
