# NBA Salary Analysis

#### Authors: Benjamin Fishman, Hamza Farooq, Juan Nicolas Serrano


**ABSTRACT**

NBA salaries range from $50,000 (practice players) to over $40,000,000 for star players. This report will look at statistics from the Basketball Reference database for the 2020-2021 NBA season. The data analyzed includes NBA salary, conference, position, player’s age, games played this season, minutes played per game, field goals made per game, 3-pointers made per game, rebounds per game, assists per game, steals per game, turnovers per game, and points per game. Our goal for this analysis is to predict the salary and determine which factors are significant. After conducting a regression analysis, we found that the primary variables for predicting NBA Salary were **Age, Games, Mins, FG, Rebound, Assist, Steal, and Point**. In our analysis, we found no statistical significance between different positions and salary.




**INTRODUCTION**

The NBA is an extremely profitable enterprise which is why athletes are paid, on average, millions of dollars per season. The more a player contributes to his team’s success, the higher his salary is expected to be. By looking at an extensive list of statistics, a general manager can expect to pay their respective athlete an equitable amount for their contributions to the team. Our motivation for choosing this topic stems from the amount of money NBA players make compared to a regular job. There are many recorded statistics that contribute to the amount of money a player makes, and we were determined to find out which were the most significant with the hopes of being able to correctly predict the salary a player should be making.

By conducting a data analysis, we will be able to determine which predictor variables are the most significant towards influencing a player’s NBA salary.
This data set is composed of many variables, but we will be analyzing the following predictors:

  Salary – per season in $ (response variable)

  Conf – Conference (East or West)

  Pos – Position (PG, SG, SF, PF, C)

  Age – NBA player’s current age (years)

  Games – total games played this season
  
  Mins – average number of minutes played per game

  FG – average number of field goals made per game 

  Three – average number of three-pointers made per game

  Rebound – average number of rebounds per game

  Assist – average number of assists per game

  Steal – average number of steals per game

  TOV – average number of turnovers per game

  Point – average number of points scored per game





```{r warning=FALSE, message=FALSE, echo=FALSE}
#upload data
df = read.csv("nba.csv")
#here I made 5 positions - before there were 10 - so we can use the POS variable in our analysis for grouping
df$Pos[df$Pos=="C-F"] = "C"
df$Pos[df$Pos=="F-C"] = "PF"
df$Pos[df$Pos=="F"] = "PF"
df$Pos[df$Pos=="G"] = "PG"
df$Pos[df$Pos=="G-F"] = "SG"
NBA = subset(df, select = -c(Rk, Player, Tm, GS, X3P., FG., FGA, X2P., X3PA, X2P, X2PA, eFG., FT, FTA, FT., ORB, DRB, BLK, PF))
names(NBA) = c("Salary", "Conf", "Pos", "Age", "Games", "Mins", "FG", "Three", "Rebound", "Assist", "Steal", "TOV", "Point")
#summary(factor(NBA$Pos))
```


```{r echo=F}
#ran a quick MLR to see how variables fit the model
reg = lm(Salary ~., data = NBA)
summary(reg)
```


We began our analysis of variables by creating a correlation plot. This plot consists of the 12 predictor variables that our dataset provided. Our interest was to see how these variables correlated with one another both numerically and by the color scale on the right side.


```{r warning=FALSE, message=FALSE, echo=FALSE}
#correlation plot with the variables to see how they relate - high correlation may hint at 
#multicolinearity but professor said it is not a problem and expected. More minutes you play = more chance for higher stats
library(corrplot)       
cplot = subset(NBA, select = -c(Conf, Pos))
c = cor(cplot)
par(cex = 0.6)          
corrplot(c)             
corrplot(c, add = TRUE, method = "number", type="lower", diag=FALSE, tl.pos="n", cl.pos="n")    
```
The figure above gives us a visual representation of the correlation between each variable pair. Our response variable, Salary, has a very weak correlation with Games, while every other variable has at least a semi-strong correlation with our response variable (greater than 0.40). High correlation between the independent variables hints at multicollinearity. Additionally, the correlation between Age and Games is very weak, which would suggest that they do not rely on one another. We want to explore these relationships more in future figures, but it supports our theory that more game time (minutes played) relates to higher player statistics.

We inserted a correlation matrix to help our initial analysis of the database. Coloring the variables by Position proved to be the most useful.


```{r warning=FALSE, message=FALSE, echo=FALSE}
#the plot professor used in the example colored by position - conference wasn't showing as
#solid of differences. we need to analyze this more
library(GGally)
ggpairs(NBA, columns=c(4,5,6,13),ggplot2::aes(colour=Pos))
```

In the figure above, red, gold, green, blue, and purple represent the center, power forward, point guard, small forward, and shooting guard positions respectively. We expected each position to have different values for age, games played, minutes per game, and average points scored per game. By looking at the minutes against minutes plot, we see that shooting guards have a higher peak in average minutes played per game compared to other positions. In the point-by-point model, shooting guards have a median points per game around 10 while small forward and center are closer to 5. The correlation between minutes and games overall is 0.519. If we look at the independent correlations of positions, shooting guard is the lowest with a 0.312 while center is the highest at 0.571. Overall, the scatterplot of minutes and points shows us that all positions follow a similar pathway with a slight exponential curve. 


We decided to create two scatterplot models to further investigate our earlier hypotheses that both Position and Age are related to Salary.


```{r warning=FALSE, message=FALSE, echo=FALSE}
#par(mfrow=c(1,2))
a = ggplot(NBA, aes(Point, Salary, colour=Pos))+geom_point()+geom_smooth(method="lm",se=F)
a + ggtitle("Points and Salary Scatterplot")
  
b = ggplot(NBA, aes(Age, Salary, colour=Pos))+geom_point()+geom_smooth(method="lm",se=F)
b + ggtitle("Age and Salary Scatterplot")
```

From these scatterplots above, we can visualize the relationship between the number of points scored per game and Salary on one figure, and the relation between Age and Salary on the other. For 30-point scorers, the shooting guard position gets paid the least (at about $25,000,000) while the center and point guard are getting paid over $30,000,000. In the Age graph, the older the point guard, the higher they are paid while the oldest centers have a salary more than $10,000,000 less than point guards.




**ANALYSIS**

Once we conducted an initial analysis of the dataset, we noticed that the model’s intercept was -16,440,628 which was very interesting to us as it was a massive negative number. To make sense of this intercept, we performed mean-centering on both the Age and Games variables while also inserting a squared Mins term. This process then led to an interpretable intercept of 3,073,394, which can be understood as the base salary for an NBA player who is of an average age (26.1 years old) and games played this season (31), while other variables are held at 0.



```{r echo=FALSE}
#the intercept above was very negative, so we mean centered age and games to make 
#it easier to analyze the intercept along with the other variables - averages across the board for variables now
NBA$MAge = NBA$Age- mean(NBA$Age)
NBA$MGames = NBA$Games - mean(NBA$Games)
reg2 = lm(Salary ~ Conf + Pos + MAge + MGames + Mins + I(Mins^2) + FG + Three + Rebound + Assist + Steal + TOV + Point, data = NBA)
summary(reg2)
```

At this point, our current regression model has 13 variables (not including 4 dummy variables for position), and our goal is to make a parsimonious model. We ran a backwards elimination regression model which resulted in 9 predictor variables, all of which are significant except for FG. This process yielded an adjusted R-squared of 0.6537. Because this is a heuristics method, it does not guarantee the optimal model, so we ran a best subsets regression on this data, which produced an adjusted R-squared of 0.652, which is almost identical to the backwards elimination model. Nevertheless, we will use the backwards elimination model for our future analysis because of the higher adjusted R-squared.


```{r results = "hide", echo = FALSE}
#backwards elimination - want this to not be shown
BE = step(reg2, direction = "backward")
```



We created an adjusted R square plot below to verify our best subsets regression significant variable conclusion.



```{r warning=FALSE, message=FALSE, echo=FALSE}
#best subsets regression
library(leaps)
#best subsets regression to find the optimal amount of variables for the model, better than backwards elimination
BSR=regsubsets(Salary~ Conf + Pos + MAge + MGames + Mins + I(Mins^2) + FG + Three + Rebound + Assist + Steal + TOV + Point, data = NBA)
plot(BSR, scale="adjr2")                 #this means we would use Age, Games, Mins, FG, Rebound, Assist, Steal, Point as predictors                                                                          8 variables yields highest adjusted R^2 = .64
```

```{r results = "hide", echo = FALSE}
reg.summary=summary(BSR)            
which.max(reg.summary$adjr2)        
reg.summary$adjr2  
coef(BSR,8)                             #the variables that we want
```



**MODELING**


```{r echo=FALSE}
#we want this to be shown in the final pdf output - this is our parsimonious model
summary(BE)
```

The interpretation of the NBA parsimonious model above, found through a backwards elimination regression method (and checked with a best subset regression), is the following:

  The salary of an NBA player who is of mean age (26.1 years old) is increased by $741,278, given all other variables are constant.

  The salary of an NBA player who has played the mean number of games (31) this season is decreased by $76,459, given all other variables are constant.

  For every minute played, on average, the salary of an NBA player decreases by $756,403, given all other variables are constant.

  For every minute squared, on average, the salary of an NBA player increases by $17,586, given all other variables are constant.

  For every unit increase in FG made per game, salary will decrease by $1,427,128, given all other variables are constant. (this is the only non-significant        variable in the model)

  For every unit increase in rebounds per game, salary will increase by $515,223, given all other variables are constant.

  For every unit increase in assists per game, salary will increase by $526,895, given all other variables are constant.

  For every unit increase in steals per game, salary will increase by $3,008,377, given all other variables are constant.

  For every unit increase in points per game, salary will increase by $1,213,258, given all other variables are constant.


The plots below were designed to verify the assumptions of multiple linear regression:
	
  
  The relationship between X and Y is linear
	
	Each error is independent
	
	The error is random and normally distributed
	
	The variable of the error is constant





```{r echo=FALSE}
#residual plot - shows some non-constant variance need to fix this thru log/x^2/something
#histogram - normal curve is good
par(mfrow=c(1,2))                             #makes a side by side graph
par(mfrow=c(1,2))                             #makes a side by side graph
plot(BE, which = 1)
#plot(BE$fitted.values, BE$residuals, main="Residual Plot", xlab="Fitted Values", ylab="Residuals")
hist(BE$residuals, main = "Histogram of Residuals", xlab="Residuals")
```

The scatterplot on the left indicates there is slight heteroscedasticity as many data points are clustered on the left side. We attempted to log and square root the response variable (Salary) after running a boxCox diagnostic (which suggested a log transformation, but a square root transformation was also a possibility), but it did not improve the dataset, in fact it decreased our adjusted R square value significantly. We applied a log transformation to Mins, as well as Rebounds, but once again, there was no improvement to this scatterplot. There is clearly a linear relation between X and Y, as well as each error being independent. The histogram suggests normality and randomness for the residuals, which suggests our dataset is valid.

Even though our scatterplot does not exhibit perfect homoscedasticity, our histogram plot indicates normality, which is an assumption that allows us to utilize this dataset for our conclusions.

The scatterplot matrix below shows the relations between all variables. We want to make sure that there is a linear correlation between Salary and each predictor variable.



```{r echo=FALSE}
#shows the relations between all variables - we want to make sure they are all linear
#any of the graphs in the Salary row that are not linear we may want to change
#(which is why we did mean centering for Age and Games)
sub = subset(NBA, select = c(Salary, Mins, FG, Rebound, Assist, Steal, Point))
plot(sub)
```

The graphs in the Salary row that are not perfectly linear are Mins and Rebounds, which is why we applied transformations to them, but they did not improve the data set. This scatterplot matrix lead us to create a squared Mins term in our parsimonious model.

**QUESTION 1**

Done above in the Introduction and Modeling sections of this report.


**QUESTION 2**

**Your teacher’s favorite basketball player is Lonzo Ball and his stats (on the New Orleans Pelicans) are found through this link: https://www.basketball-reference.com/players/r/rosede01.html.  Is Lonzo Ball’s salary reasonable?**

We were curious to see if our parsimonious model could accurately predict any NBA player’s salary using a 95% prediction interval. We imported Lonzo Ball’s 2020-2021 NBA season stats and predicted that his salary should be $29,614,878. This result is significant because it was predicted using our regression model. 




```{r echo=FALSE, results = "hide"}
#predicting Lonzo Ball salary based on our model
p = predict(BE, data.frame(MAge = 23, MGames = 46, Mins = 31.1, FG = 5.1, Rebound = 4.1, Assist = 5.6, Steal = 1.4, Point = 14.1))
p
#From that you can calculate a prediction interval and see if Derrick Rose's actual salary is in the 95% prediction interval.
predict(BE, NBA[28,], interval = "prediction")
```

We turned this question into a hypothesis test, where the null is the salary should be $29,614,878 while the alternative hypothesis is that it should not be $29,614,878. 


$$H_0: \beta_1 = \$ 29,614,878$$


$$H_1: \beta_1 \neq \$ 29,614,878$$

Our 95% prediction interval yielded a range from $2,590,330 to $24,753,414. Because our predicted value is outside of this prediction interval range, we reject the null hypothesis based on 95% significance, however, the value we found is not far from being within the range.





**Question 3**

**The NBA Commissioner believes that all 5 positions have the same mean salary. Can you refute his claim?**



```{r echo=FALSE}
#salaries split by position
ggplot(NBA, aes(x=Pos, y=Salary, fill=Pos))+
  geom_boxplot() +
  ggtitle("Boxplot of Posiitons Distribution") +
  labs(y="Salary $", x = "Position")
```

The above figure is a boxplot distribution split on the 5 positions in the NBA. An initial analysis shows that the PG position has the highest mean salary by a large margin, while also having the highest outliers of all positions. We are going to create a hypothesis test to analyze the commissioner’s claim. 

Our null hypothesis will be this: salaries across positions are the same and therefore are not statistically significant





*Null hypothesis*
$$H_0: \mu_1 = \mu_2 = \mu_3 = \mu_4$$


Our alternative hypothesis will be this: salaries across positions are NOT the same and therefore is statistically significant.


*Alternative Hypothesis*
$$H_1: \mu_1\neq \mu_2 \neq \mu_3 \neq \mu_4$$
To conclude whether or not position is statistically significant, we ran the new regression model below that shows the relation between Salary and Position.


```{r echo=FALSE}
#simple linear regression
q3= lm(Salary ~ Pos, data=NBA)
summary(q3)
```

After running the linear regression model where we predicted Salary based on Position (dummy variables were automatically made within R), we found that C (center) was our baseline reference. This summary concludes that Position is NOT a statistically significant predictor of Salary. The p-value is very high, 0.1961, while our adjusted R square is very low, 0.005341, so compared to the baseline reference of C, the other positions are not statistically different. There is a very large amount of variability within this data set as our Residual Standard Error is 9,444,000.



**Conclusion**

After completing our analysis for this project, we created two different regression models to help us find the significant variables for our designated questions. These models allowed us to understand the relationship between response variable (Salary) and predictor variables.  Our model to predict Salary has an adjusted R square of 0.6537 which is a relatively strong linear relationship between our response and predictor variables. 

We wanted to put our parsimonious model to the test by predicting an NBA player’s salary based upon their current 2020-2021 stats. We chose one of the most hyped up players of our generation, Lonzo Ball. After conducting a 95% prediction interval, we found that his predicted salary fell outside of this interval which had an upper range of about $25,000,000.

Our regression model to explore the relationship between Salary and Position yielded a large p-value, 0.1961, which is larger than our alpha of 0.05, leading us to believe that there is no statistical significance between Position and Salary – which was a surprise for us because we believed that Position would influence Salary as depicted in the boxplot above. This make sense because players are paid before a season starts (sometimes many years in advance) which allows them to make more money than they deserve based upon their actual statistical performance. 

