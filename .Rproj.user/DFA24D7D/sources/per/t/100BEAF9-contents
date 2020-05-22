# 16-1 test your R might
 Jacks.Equation <- function (a,b,c){
   output <- (a*b) - c*324 + log(a)
   return(output)
 }
 
 # 16-2
 standardize.me <- function(x){
   output <- (x - mean(x))/sd(x)
   return(output)
 }
 
 # 16-3
 recode.numeric <- function(x, lb, ub){
 x[x < lb | x >ub] <- NA

   return(x)
 }
 
 # 16-4
 plot.advanced <- function(x = rnomrm(100),
                           y = rnorm(100),
                           add.regression = FALSE,
                           add.means = FALSE,
                           add.test = FALSE){
   plot(x,y)
   
   if(add.regression == TRUE){
   model <- lm(y ~ x)
   abline(model, lwd = 2)
   }
   
   if (add.means == TRUE){
     abline(h = mean(y), lty = 2)
     abline(v = mean(x), lty = 2)
   }
   if (add.test == TRUE){
     test <- cor.test(x = x,
                      y = y)
     apa.test <- apa(test)
     mtext(text = apa.test,
           side = 3)
   }
 }
 
 # 17-1
 par(mfrow = c(2,2))
 loop.vec <- c(0, 2, 4, 6)
 for(i in loop.vec){
  x = ChickWeight$weight [ChickWeight$Time == i ]
  hist(x,
       main = paste("Time", i))}
  
#17-2
survey.clean <- survey
 for(i in 1:5){
   x <- survey[i]
   x[x>5| x<1] = NA
   survey.clean[i] <- x
 }

#17-3
survey.clean$invalid.answer <- rep(0, 6)
for(i in 1:6){
  Nah <- is.na(survey.clean[i,])
  CountNah <- sum(Nah)
  survey.clean$invalid.answer[i] <-CountNah
}

#17-4
survey.B.z <- survey.B
for(i in 1:5){
x = survey.B.z[,i]
  output <- (x - mean(x))/sd(x)
  survey.B.z[i] <- output

}
