setwd("D:/hacxy/Downloads/Final Exam/Profiling Linear Regression")
source("final_q1.R")

set.seed(1234567890)
output = matrix(NA,5,20)

r_squared_fun_i = function(i,x,y){
  if (i == 1){
    r_squared_fun_1(x,y)
  }
  if (i ==2){
    r_squared_fun_2(x,y)
  }
  if (i ==3){
    r_squared_fun_3(x,y)
  }
  if (i ==4){
    r_squared_fun_4(x,y)
  }
  if (i ==5){
    r_squared_fun_5(x,y)
  }
}

totaltime = function(i,x,y){
  Rprof(filename='q.out',filter.callframes = TRUE,interval=0.01)
  r_squared_fun_i(i,x,y)
  Rprof(NULL)
  summaryRprof('q.out')$by.total[1,1]
}

for (j in 1:20){
  x = matrix(rnorm(1000*500),1000,500)
  y = rnorm(1000)
  for (i in 1:5){
    output[i,j] = totaltime(i,x,y)
  }
}
output


profiling_matrix <- output
f1 = summary(profiling_matrix[1,])
f2 = summary(profiling_matrix[2,])
f3 = summary(profiling_matrix[3,])
f4 = summary(profiling_matrix[4,])
f5 = summary(profiling_matrix[5,])
f = rbind(f1,f2,f3,f4,f5)
summary_matrix = as.matrix(f)
summary_matrix
# apperantly, the function2, "solve linear system" is the best.
# Why? Because SVD function cost lots of caluctions, 
# then calculate eigen is not necessary, could directly use formulation to calculate.(the formulation is given by calculated and proved, it is easy and fast)
# And also, in 1 and 2, use solve function in base R is better than use the formulation (inv%*%xty) in the textbook.