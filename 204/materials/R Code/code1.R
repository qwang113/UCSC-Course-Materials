S=matrix(c(1,0.5,1.3,0.4,-1,-0.2,0.5,-1,0.6),3,3)
S[1:2,c(1,3)]
apply(S,2,sum)

####
S=matrix(c(1,0.4,0.5,-1),2,2)
x=c(0,1)
S%*%x
eigen(S)
t(S)
solve(S)
solve(S,x)

########## Assignments in R #################
# Operators <- and = assign into the environment in which they are evaluated
# <- can be used anywhere, whereas = is only allowed at the top level (expression types at the command prompt) or as one of the subexpressions in a braced list of expressions
# <<- is normally used in functions and causes a search to be made through parent environments for an existing definition of the variable being assigned. If variable is found (and not locked) its value is redifined, o.w. assignment takes place in the global environment
### Some examples:

x=10
median(x=1:10)
x
median(x <- 1:10)
x
assign_example=function(x){x<-10; print(x)}
assign_example(x)
x

assign_example=function(x){x<<-10}
assign_example(x)
x
