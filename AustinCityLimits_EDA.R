#A histogram to visualize the age 
hist(AustinCityLimits$Age)

#A Contingency table displays the frequency distribution of the variables according to age and Grammy award.
table(AustinCityLimits$Age.Group, AustinCityLimits$Grammy)

#calculation of probabilities (%age)
t <- prop.table(table(AustinCityLimits$Grammy, AustinCityLimits$Age.Group))*100
t

#ploting the data
x <- barplot(t, main="Age/Grammy Bar plot",
             xlab="Age", ylab = "Grammy %age", col=c(rgb(0.3,0.1,0.4,0.6) ,rgb(0.3,0.5,0.4,0.6)), 
             beside = TRUE)

#adding legend
legend("topleft", legend = c("Yes","No" ) , 
       col = c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6)), 
       bty = "n", pch=20 , pt.cex = 2, cex = 0.8, inset = c(0.06, 0.05))

#adding %age values
text(x, 0, round(t, 1),cex=1,pos=3) 

#Young - age = twenties
#old - twenties plus

#According to the above analysis young artist claim right that that judges were 
#inclined to give Grammy to old artists.

#*******************************
