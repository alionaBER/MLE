library(shiny)

S <- 20 #total number of applicants to a summer school
C <- 50 #total number of calls to hotline

#########  applicants level ######### 
applicantID <- 1:S

### binomial
set.seed(698)
admitted.to.summerschool <- rbinom(S, size=1, 0.6)

### poisson
set.seed(48) #get 50 calls
times.contacted.hotline <- rpois(S, 2.5)

### normal 
set.seed(21)
applicants.height <- rnorm(S, mean = 170, sd = 10)

######### calls level ######### 
### discrete uniform 
set.seed(15)
employee <- sample(1:5, size = C, replace = TRUE)

#exponential 
set.seed(42)
waiting.time.for.call <- rexp(C, 1) #waiting time for a call for all employees


#creating dataframes of applicants and calls
applicants <- as.data.frame(cbind(applicantID, admitted.to.summerschool, applicants.height, times.contacted.hotline))
applicants$applicantID <- as.integer(applicants$applicantID)
applicants$times.contacted.hotline <- as.integer(applicants$times.contacted.hotline)
applicants$admitted.to.summerschool <- as.integer(applicants$admitted.to.summerschool)

index <- rep(1:nrow(applicants), applicants$times.contacted.hotline)
calls <- applicants[index,]
calls$employee <- as.integer(employee)
calls$waiting.time.for.call <- waiting.time.for.call
rownames(calls) <- NULL
