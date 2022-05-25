## ??????????????????:?????????high/low value???...method????????????????????????steps,??????????????????????????????????????????
# ??????????????????conditions???????????????,?????????accuracy
getwd()

# phase 1

convert.N.to.C <- function(U,UH,UL){
  x <- (U - (UH+UL)/2) / ((UH-UL)/2)
  return(x)
}

# Function for converting from coded units to natural units
convert.C.to.N <- function(x,UH,UL){
  U <- x*((UH-UL)/2) + (UH+UL)/2
  return(U)
}

# 2-level
# important: Prev.Length, Match.Score, Prev.Length:Match.Score
# ????????????????????????preview length and match score

data <- read.csv("RESULTS_20708813_2021-08-13.csv",header=T)
data$Prev.Length = convert.N.to.C(data$Prev.Length, 120, 100)
data$Match.Score = convert.N.to.C(data$Match.Score, 100, 80)
data$Tile.Size = convert.N.to.C(data$Tile.Size, 0.3,0.1)
model.full <- lm(Browse.Time~Prev.Length * Match.Score * Tile.Size , data=data)
summary(model.full)

#fit the largest model here
model.fullish <- lm(Browse.Time~Prev.Length + Match.Score + Prev.Length*Match.Score, data=data)
summary(model.fullish)

library(gplots)
plot(formula=Browse.Time~Match.Score, ylab="Browsing Time", xlab="Preview Length", data=data)
# -------------------------------------------

# phase 2 - 2.1 curvature test

blue_palette <- colorRampPalette(c(rgb(247,251,255,maxColorValue = 255), rgb(8,48,107,maxColorValue = 255)))
data1=read.csv("RESULTS_20708813_2021-08-13-phase2.csv",header=T)

#try
data2=read.csv("RESULTS_20708813_2021-08-13-phase2.csv",header=T)
table(data2$Prev.Length, data2$Match.Score)
# determine whether we're close to the optimum to begin with
ph1 <- data.frame(y = data2$Browse.Time,
                  x1 = convert.N.to.C(U = data2$Prev.Length, UH = 120, UL = 100),
                  x2 = convert.N.to.C(U = data2$Match.Score, UH = 100, UL = 80))
ph1$xPQ <- (ph1$x1^2 + ph1$x2^2)/2
# check average browsing time in each condition:????????????
aggregate(ph1$y, by = list(x1 = ph1$x1, x2 = ph1$x2), FUN = mean)
# the difference in average browsing time in factorial conditions vs the center
# point condition???????????????
mean(ph1$y[ph1$xPQ != 0]) - mean(ph1$y[ph1$xPQ == 0])
# check to see if significant
# ??????2 mean effects and interaction effect and second order effect xPQ
m = lm(y~x1+x2+x1*x2+xPQ, data=ph1)
summary(m)
# ??????significant, x1 mean effect is significant, x2 mean effect significant, ??????second order effect / curvature significant,
# ??????x0????????????????????????optimal point area???
# ???????????????steepest descent method???
ph1 <- data.frame(y = data1$Browse.Time,
                  x1 = convert.N.to.C(U = data1$Prev.Length, UH = 120, UL = 100),
                  x2 = convert.N.to.C(U = data1$Match.Score, UH = 100, UL = 80))

#### -----------------------------------------------------------------------------------
# steepest descent method 
library("plot3D")
m.fo <- lm(y~x1+x2, data = ph1)
summary(m.fo)
beta0 <- coef(m.fo)[1]
beta1 <- coef(m.fo)[2]
beta2 <- coef(m.fo)[3]
grd <- mesh(x = seq(convert.N.to.C(U = 30, UH = 120, UL = 100), 
                    convert.N.to.C(U = 120, UH = 120, UL = 100), 
                    length.out = 100), 
            y = seq(convert.N.to.C(U = 0, UH = 100, UL = 80), 
                    convert.N.to.C(U = 100, UH = 100, UL = 80), 
                    length.out = 100))
x1 <- grd$x
x2 <- grd$y
eta.fo <- beta0 + beta1*x1 + beta2*x2
# 2D contour plot
contour(x = seq(convert.N.to.C(U = 30, UH = 120, UL =100), 
                convert.N.to.C(U = 120, UH = 120, UL = 100), 
                length.out = 100),
        y = seq(convert.N.to.C(U = 0, UH = 100, UL = 80), 
                convert.N.to.C(U = 100, UH = 100, UL = 80), 
                length.out = 100), 
        z = eta.fo, xlab = "x1 (Preview Length)", ylab = "x2 (Match Score)", main="plot of x path for TT",
        nlevels = 15, col = blue_palette(15), labcex = 0.9, asp=0.25)
abline(a = 0, b = beta2/beta1, lty = 2)
points(x = 0, y = 0, col = "red", pch = 16)

# The gradient vector
g <- matrix(c(beta1, beta2), nrow = 1)
PL.step <- convert.N.to.C(U = 110 + 5, UH = 120, UL = 100)
lamda <- PL.step/abs(beta1)

## Step 0: The center point we've already observed
x.old <- matrix(0, nrow=1, ncol=2)
text(x = 0, y = 0+0.25, labels = "0")
step0 <- data.frame(Prev.Length = convert.C.to.N(x = 0, UH = 120, UL = 100), 
                    Match.Score = convert.C.to.N(x = 0, UH = 100, UL = 80)) 
## Step 1: 
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "1")
step1 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80))

## Step 2: 
x.old <- x.new
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "2")
step2 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80))

## Step 3: 
x.old <- x.new
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "3")
step3 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80))

## Step 4: 
x.old <- x.new
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "4")
step4 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80))

## Step 5: 
x.old <- x.new
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "5")
step5 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80))

## Step 6: 
x.old <- x.new
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "6")
step6 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80))

## Step 7: 
x.old <- x.new
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "7")
step7 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80))

## Step 8: 
x.old <- x.new
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "8")
step8 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80))

## The following is a list of the conditions along the path of steepest descent
pstd.cond <- data.frame(Step = 0:8, rbind(step0, step1, step2, step3, step4, step5, step6, step7, step8))
pstd.cond
# ?????????9???conditions =??? 900 ?????????,??????
# ?????????1???1????????????,control conditions
# x0??????????????????
# ?????????MOI?????????

##########Find MOI for each
netflix.ph2 <- read.csv("RESULTS_20708813_2021-08-13-phase2(2).csv",header=T)

## Calculate the average browsing time in each of these conditions and find the 
## condition that minimizes it
pstd.means <- aggregate(netflix.ph2$Browse.Time, 
                        by = list(Prev.Length = netflix.ph2$Prev.Length, 
                                  Match.Score = netflix.ph2$Match.Score), 
                        FUN = mean)
pstd.means
plot(x = 0:8, y = pstd.means$x[9:1],
     type = "l", xlab = "Step Number", ylab = "Average Browsing Time")
points(x = 0:8, y = pstd.means$x[9:1],
       col = "red", pch = 16)
# ?????????????????????9??????
# ????????????X0?????????Average browsing time????????????
# as step number increases, average browsing time also increases
# ??????????????????X0?????????browsing time?????????improve??????optimal point???X0??????
# ???????????????x4,x5????????????????????????,??????????????????:
# ???step5??????????????????,???85,65???=???prev.length [75,95], score [55,75]
## (i.e, check whether the pure quadratic effect is significant)
data3=read.csv("RESULTS_20708813_2021-08-13-phase2(3).csv",header=T)
ph2 <- data.frame(y = data3$Browse.Time,
                  x1 = convert.N.to.C(U = data3$Prev.Length, UH = 95, UL = 75),
                  x2 = convert.N.to.C(U = data3$Match.Score, UH = 77, UL = 57))
ph2$xPQ <- (ph2$x1^2 + ph2$x2^2)/2
## Check the average browsing time in each condition:
aggregate(ph2$y, by = list(x1 = ph2$x1, x2 = ph2$x2), FUN = mean)
## The difference in average browsing time in factorial conditions vs. the center 
## point condition
mean(ph2$y[ph2$xPQ != 0]) - mean(ph2$y[ph2$xPQ == 0])
## Check to see if that's significant
m <- lm(y~x1+x2+x1*x2+xPQ, data = ph2)
summary(m)

# phase2 ??????


# --------------------------------------------------------------------

# phase 3??????2 STEP5##############################################
# CCD method
# ????????????????????????
convert.C.to.N(1,75,95)
convert.C.to.N(-1,75,95)
convert.C.to.N(1,55,75)
convert.C.to.N(-1,55,75)
# fit second order model?
netflix <- read.csv("RESULTS_20708813_2021-08-14-phase3(3).csv",header=T)
table(netflix$Prev.Length,netflix$Match.Score)

#condition <- data.frame(x1 = convert.C.to.N(x = c(-1,-1,1,1,0,1.4,-1.4,0,0), UH = 105, UL = 75), 
#x2 = convert.C.to.N(x = c(-1,1,-1,1,0,0,0,1.4,-1.4), UH = 0.6, UL = 0.4))
netflix$Prev.Length <- convert.N.to.C(netflix$Prev.Length,95, 75)
netflix$Match.Score <- convert.N.to.C(netflix$Match.Score,75, 55)
## We then fit the full 2nd-order response surface
model <- lm(Browse.Time ~ Prev.Length + Match.Score + Prev.Length*Match.Score + I(Prev.Length^2) + I(Match.Score^2), data = netflix)
summary(model)
# should be all significant
anova(model)

## Let's visualize this surface: (second order counter plot)
beta0 <- coef(model)[1]
beta1 <- coef(model)[2]
beta2 <- coef(model)[3]
beta12 <- coef(model)[6]
beta11 <- coef(model)[4]
beta22 <- coef(model)[5]
grd <- mesh(x = seq(convert.N.to.C(U = 75, UH = 95, UL = 75), 
                    convert.N.to.C(U = 95, UH = 95, UL = 75), 
                    length.out = 100), 
            y = seq(convert.N.to.C(U = 80, UH = 75, UL = 55), 
                    convert.N.to.C(U = 75, UH = 75, UL = 55), 
                    length.out = 100))
x1 <- grd$x
x2 <- grd$y
eta.so <- beta0 + beta1*x1 + beta2*x2 + beta12*x1*x2 + beta11*x1^2 + beta22*x2^2

# 2D contour plot (coded units) #??????????????????U????????????grid
contour(x = seq(convert.N.to.C(U = 75, UH = 95, UL = 75), 
                convert.N.to.C(U = 95, UH = 95, UL = 75), 
                length.out = 100), 
        y = seq(convert.N.to.C(U = 55, UH = 75, UL = 55), 
                convert.N.to.C(U = 75, UH = 75, UL = 55), 
                length.out = 100), 
        z = eta.so, xlab = "x1", ylab = "x2",
        nlevels = 20, col = blue_palette(20), labcex = 0.9)

# ?????????counter plot,????????????


## Let's find the maximum of this surface and the corresponding factor levels 
## at which this is achieved
b <- matrix(c(beta1,beta2), ncol = 1)
B <- matrix(c(beta11, 0.5*beta12, 0.5*beta12, beta22), nrow = 2, ncol = 2)
x.s <- -0.5*solve(B) %*% b 
points(x = x.s[1], y = x.s[2], col = "red", pch = 16)
#points(x = 1.36225, y = 0.5747397, col = "red", pch = 16)


# The predicted book rate at this configuration is:
# coded value -> natural value
eta.so.opt=beta0+beta1*x.s[1]+beta2*x.s[2]+beta12*x.s[1]*x.s[2]+beta11*x.s[1]^2+beta22*x.s[2]^2
eta.so.opt

# In natural units this optimum is located at
convert.C.to.N(x = x.s[1,1], UH = 95, UL = 75)
convert.C.to.N(x = x.s[2,1], UH = 75, UL = 55)


# Remake the contour plot but in natural units
# ??????????????????????????????,????????????,??????optimal point
contour(x = seq(75, 95, length.out = 100), 
        y = seq(55, 75, length.out = 100), 
        z = eta.so, xlab = "Preview Length", ylab = "Match Score",
        nlevels = 20, col = blue_palette(20), labcex = 0.9)

points(x = convert.C.to.N(x = x.s[1,1], UH = 95, UL = 75),
       y = convert.C.to.N(x = x.s[2,1], UH = 75, UL = 55), 
       col = "red", pch = 16)

## 95% prediction interval at this optimum:
pred <- predict(model, newdata = data.frame(Prev.Length=x.s[1,1], Match.Score=x.s[2,1]), type = "response", se.fit = TRUE)
pred 
print(paste("Prediction: ", pred$fit, sep = ""))
print(paste("95% Prediction interval: (", pred$fit-qnorm(0.975)*pred$se.fit, ",", pred$fit+qnorm(0.975)*pred$se.fit, ")", sep = ""))












##----PHASE3??????1????????????!!?????????phase2(1)------##
  
  
  # phase 3
  # CCD method
  
  # ????????????????????????
convert.C.to.N(1,100,120)
convert.C.to.N(-1,100,120)
convert.C.to.N(1,80,100)
convert.C.to.N(-1,80,100)

# fit second order model?
netflix <- read.csv("RESULTS_20708813_2021-08-13-phase3(2).csv",header=T)
table(netflix$Prev.Length,netflix$Match.Score)

#condition <- data.frame(x1 = convert.C.to.N(x = c(-1,-1,1,1,0,1.4,-1.4,0,0), UH = 105, UL = 75), 
#x2 = convert.C.to.N(x = c(-1,1,-1,1,0,0,0,1.4,-1.4), UH = 0.6, UL = 0.4))
netflix$Prev.Length <- convert.N.to.C(netflix$Prev.Length,120, 100)
netflix$Match.Score <- convert.N.to.C(netflix$Match.Score,100, 80)
## We then fit the full 2nd-order response surface
model <- lm(Browse.Time ~ Prev.Length + Match.Score + Prev.Length*Match.Score + I(Prev.Length^2) + I(Match.Score^2), data = netflix)
summary(model)
# should be all significant

## Let's visualize this surface: (second order counter plot)
beta0 <- coef(model)[1]
beta1 <- coef(model)[2]
beta2 <- coef(model)[3]
beta12 <- coef(model)[6]
beta11 <- coef(model)[4]
beta22 <- coef(model)[5]
grd <- mesh(x = seq(convert.N.to.C(U = 100, UH = 120, UL = 100), 
                    convert.N.to.C(U = 120, UH = 120, UL = 100), 
                    length.out = 100), 
            y = seq(convert.N.to.C(U = 80, UH = 100, UL = 80), 
                    convert.N.to.C(U = 100, UH = 100, UL = 80), 
                    length.out = 100))
x1 <- grd$x
x2 <- grd$y
eta.so <- beta0 + beta1*x1 + beta2*x2 + beta12*x1*x2 + beta11*x1^2 + beta22*x2^2

# 2D contour plot (coded units) #??????????????????U????????????grid
contour(x = seq(convert.N.to.C(U = 100, UH = 120, UL = 100), 
                convert.N.to.C(U = 120, UH = 120, UL = 100), 
                length.out = 100), 
        y = seq(convert.N.to.C(U = 80, UH = 100, UL = 80), 
                convert.N.to.C(U = 100, UH = 100, UL = 80), 
                length.out = 100), 
        z = eta.so, xlab = "x1", ylab = "x2",
        nlevels = 20, col = blue_palette(20), labcex = 0.9)

# ?????????counter plot,????????????


## Let's find the maximum of this surface and the corresponding factor levels 
## at which this is achieved
b <- matrix(c(beta1,beta2), ncol = 1)
B <- matrix(c(beta11, 0.5*beta12, 0.5*beta12, beta22), nrow = 2, ncol = 2)
x.s <- -0.5*solve(B) %*% b 
points(x = x.s[1], y = x.s[2], col = "red", pch = 16)
#points(x = 1.36225, y = 0.5747397, col = "red", pch = 16)


# The predicted book rate at this configuration is:
# coded value -> natural value
eta.so.opt=beta0+beta1*x.s[1]+beta2*x.s[2]+beta12*x.s[1]*x.s[2]+beta11*x.s[1]^2+beta22*x.s[2]^2
eta.so.opt

# In natural units this optimum is located at
convert.C.to.N(x = x.s[1,1], UH = 120, UL = 100)
convert.C.to.N(x = x.s[2,1], UH = 100, UL = 80)


# Remake the contour plot but in natural units
# ??????????????????????????????,????????????,??????optimal point
contour(x = seq(100, 120, length.out = 100), 
        y = seq(80, 100, length.out = 100), 
        z = eta.so, xlab = "Preview Length", ylab = "Match Score",
        nlevels = 20, col = blue_palette(20), labcex = 0.9)

points(x = convert.C.to.N(x = x.s[1,1], UH = 120, UL = 100),
       y = convert.C.to.N(x = x.s[2,1], UH = 100, UL = 80), 
       col = "red", pch = 16)

## 95% prediction interval at this optimum:
pred <- predict(model, newdata = data.frame(Prev.Length=x.s[1,1], Match.Score=x.s[2,1]), type = "response", se.fit = TRUE)
pred 
print(paste("Prediction: ", pred$fit, sep = ""))
print(paste("95% Prediction interval: (", pred$fit-qnorm(0.975)*pred$se.fit, ",", pred$fit+qnorm(0.975)*pred$se.fit, ")", sep = ""))
