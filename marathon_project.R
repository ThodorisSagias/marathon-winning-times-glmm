#install.packages("readxl")
library("readxl")
data <- read_excel("SAW2026.xlsx", sheet = 1)
str(data)
data <- as.data.frame(data)
str(data)
head(data)

# Fixing the issue with the dates from EXCEL
data$Date <- as.Date(as.numeric(data$Date), origin = "1899-12-30")
data$Date

# EXCEL saving dates issue
data[284:286,]

str(data)

# Managing properly our variables

data$Country <- factor(data$Country)
data$Event <- factor(data$Event)
data$Gender <- factor(data$Gender)
data$Shoes <- factor(data$Shoes)
data$Humidity <- factor(data$Humidity)
data$Weather <- factor(data$Weather)
data$Route_Feature <- factor(data$Route_Feature)


str(data)

########################### Country ###########################################


table(data$Country)
sort(table(data$Country), decreasing = TRUE)

# We are having so much runners from USA because back in the past in events like New York Marathon,
# Chicago and Boston only natives were participating

# Also around 1970, we had a running boom back in the USA that this boosted American success
# even further

########################### Event ###########################################


table(data$Event)

# No information from here because we took all the years for the major marathons in the world
# and to be more specific we took the winner for males and females each year


########################### Year of Event ###########################################


table(data$YearofEvent)
sort(table(data$YearofEvent), decreasing = TRUE)


########################### Year of Birth ###########################################

sort(table(data$YearofBirth), decreasing = TRUE)


########################### Age ###########################################

summary(data$Age)

hist(data$Age, main = "Marathon Winners and their Age", prob = TRUE, col = "lightblue3")
lines(density(data$Age))


########################### Gender ###########################################

table(data$Gender)

# That difference between Female and Male winners happened because back in the past,
# we had not races for women only

########################### Time ###########################################

summary(data$Time)

hist(data$Time, main = "Marathon Winners and their Time", prob = TRUE, col = "purple1")
lines(density(data$Time))

########################### Shoes ###########################################


sort(table(data$Shoes), decreasing = TRUE)



########################### Humidity ###########################################


sort(table(data$Humidity), decreasing = TRUE)

# Most races are having moderate humidity because it is fixed every year the date of each
# event in order to try to have the best possible weather conditions for the athletes for each
# event

########################### Weather ###########################################


sort(table(data$Weather), decreasing = TRUE)

# Exactly what we said about the Humidity is also true in the weather

########################### Route Feature ###########################################


sort(table(data$Route_Feature), decreasing = TRUE)


#############################################################################################
#############################################################################################


# Time Distribution by Gender

str(data)
Women <- data[data$Gender == "Female",]
str(Women)
Men <- data[data$Gender == "Male",]
str(Men)


summary(Men$Time)
summary(Women$Time)

par(mfrow=c(1,2))

hist(Men$Time, main = "Men Winners and their Time", prob = TRUE, col = "skyblue")
lines(density(Men$Time))

hist(Women$Time, main = "Women Winners and their Time", prob = TRUE, col = "purple1")
lines(density(Women$Time))

# Comments about the plots #

# It was expected that the men would have better times than women

# Most men are behind the mean time 2.23 so the rest men who are having worse time than 2.23
# are guiding the mean time to 2.23

# Most women are around 2.4 as time and exactly because there are also women who are having worse
# times than the mean time, for women the final mean time is around 2.5

# Worth mentioning is that because before 1965 we had only men running marathons, we are
# observing that more men that women are having worse times than the mean time for each category


###################################################################################################
###################################################################################################

# New Era Idea #

# We continue our study for only these data from now on 

str(data)
table(data$YearofEvent)

k <- which(data$YearofEvent < 1980)
k


NewEra <- data[-c(k),]
length(k)
dim(data)
str(NewEra)

table(NewEra$YearofEvent)

# We will do exactly what I did before for the whole data in order to identify differences

par(mfrow=c(1,2))

# All Data

hist(data$Time, main = "Marathon Winners and their Time", prob = TRUE, col = "purple1")
lines(density(data$Time))

# New Era


hist(NewEra$Time, main = "Marathon Winners and their Time (New Era)", prob = TRUE, col = "blue1")
lines(density(NewEra$Time))




#### Pro Plots


x_limits <- range(data$Time) 

par(mfrow = c(1, 2)) # Split screen


hist(data$Time, 
     main = "All Winners (1897-2018)", 
     xlab = "Time (Hours)", 
     prob = TRUE, 
     col = "midnightblue",    
     border = "white",        
     xlim = x_limits, 
     ylim = c(0, 3), 
     las = 1)


hist(NewEra$Time, 
     main = "New Era Only (1980-2018)", 
     xlab = "Time (Hours)", 
     prob = TRUE, 
     col = "steelblue", 
     border = "white", 
     xlim = x_limits, 
     ylim = c(0, 3), 
     las = 1)


par(mfrow = c(1, 1))

# 1 SLIDE IN PRESENTATION #


######################################################

d_all <- density(data$Time)
d_new <- density(NewEra$Time)

# 1. Define the logical cutoff
cutoff <- 2.3

# 2. Extract exactly TWO peaks (using the New Era curve as the modern reference)
# Find the x coordinates
peak1_x <- d_new$x[d_new$x < cutoff][which.max(d_new$y[d_new$x < cutoff])]
peak2_x <- d_new$x[d_new$x > cutoff][which.max(d_new$y[d_new$x > cutoff])]

# Find the corresponding y coordinates (the height of the curve)
peak1_y <- max(d_new$y[d_new$x < cutoff])
peak2_y <- max(d_new$y[d_new$x > cutoff])

# 3. Base Plot (Notice the ylim is increased to 1.2 for extra text headroom)
plot(d_all, 
     main = "The Shift: Amateur vs New Era", 
     xlab = "Winning Time (Hours)", 
     ylab = "Density",
     col = "midnightblue",    
     lwd = 3,                 
     ylim = c(0, max(d_new$y) * 1.2), 
     las = 1)

# Fill for New Era
polygon(d_new, col = rgb(0.27, 0.51, 0.71, 0.5), border = "steelblue")

# 4. Add the "Blue Markers" (Solid points right at the peak)
# pch = 19 creates a solid circle, cex = 1.5 makes it large enough for a projector
points(x = c(peak1_x, peak2_x), 
       y = c(peak1_y, peak2_y), 
       col = "steelblue", 
       pch = 19, 
       cex = 1.5)

# 5. Add Text Labels right above the markers
# We add a slight offset to the y-coordinate so the text floats cleanly above the dot
offset <- max(d_new$y) * 0.05

text(x = peak1_x, y = peak1_y + offset, 
     labels = paste(round(peak1_x, 2), "h"), 
     col = "steelblue", font = 2, cex = 1.1)

text(x = peak2_x, y = peak2_y + offset, 
     labels = paste(round(peak2_x, 2), "h"), 
     col = "steelblue", font = 2, cex = 1.1)

# Legend
legend("topright", 
       legend = c("All History", "New Era (1980+)"), 
       fill = c(NA, rgb(0.27, 0.51, 0.71, 0.5)), 
       col = c("midnightblue", "steelblue"), 
       lwd = c(3, 2), 
       bty = "n")


# NEXT SLIDE AFTER THE 1 SLIDE IN PRESENTATION I WROTE ABOVE

# Very interesting and expected result that they key winning times that most athletes are doing
# have stayed the same among these 2 periods (Old Era vs New Era)

########################### Country ###########################################


table(NewEra$Country)
sort(table(NewEra$Country), decreasing = TRUE)

# We are having so much runners from Kenya because the sport of marathon is very famous to the country
# because they are the best on it and because of the way of living there the running is almost 
# necessary for their lives


########################### Event ###########################################


table(NewEra$Event)

# No information from here because we took all the years for the major marathons in the world


########################### Year of Event ###########################################


table(NewEra$YearofEvent)
sort(table(NewEra$YearofEvent), decreasing = TRUE)


########################### Year of Birth ###########################################

sort(table(NewEra$YearofBirth), decreasing = TRUE)


########################### Age ###########################################

summary(NewEra$Age)

hist(NewEra$Age, main = "Marathon Winners and their Age (New Era)", prob = TRUE, col = "lightblue3")
lines(density(NewEra$Age))

par(mfrow=c(1,2))


hist(data$Age, main = "Marathon Winners and their Age", prob = TRUE, col = "midnightblue")
lines(density(data$Age))

hist(NewEra$Age, main = "Marathon Winners and their Age (New Era)", prob = TRUE, col = "steelblue")
lines(density(NewEra$Age))


# Very Interesting that the 2 plots are almost identical

# We can see a noticeable spike in winners aged 20-24 
# This demographic shift is driven by the professionalization of the sport 
# The introduction of high-stakes prize money and targeted East African talent pipelines 
# allows athletes to bypass a traditional track career and specialize directly in the marathon during 
# their early physiological peak


# SLIDE IN PRESENTATION AS WELL

# Explanation.txt some interpretation from AI Research


########################### Gender ###########################################

table(NewEra$Gender)

# 1 male more --> Double Winner 453-454 row London 1981

data[453:455,]

NewEra

# We went to the new era so we have no differences between the number of men and women winners

########################### Time ###########################################

summary(NewEra$Time)

hist(NewEra$Time, main = "Marathon Winners and their Time", prob = TRUE, col = "purple1")
lines(density(NewEra$Time))

########################### Shoes ###########################################


sort(table(NewEra$Shoes), decreasing = TRUE)

sort(table(data$Shoes), decreasing = TRUE)


# Nike seems to be more famous in the New Era while in all the data Adidas is the more famous
# among our runners

# A significant driver of this shift is the "Super Shoe" technology, initiated by the release 
# of the Nike Vaporfly in 2017

########################### Humidity ###########################################


sort(table(NewEra$Humidity), decreasing = TRUE)

# Most races are having moderate humidity because it is fixed every year the date of each
# event in order to try to have the best possible weather conditions for the athletes

########################### Weather ###########################################


sort(table(NewEra$Weather), decreasing = TRUE)

# Exactly what we said about the Humidity is also true in the weather

########################### Route Feature ###########################################


sort(table(NewEra$Route_Feature), decreasing = TRUE)


#############################################################################################
#############################################################################################


# Time Distribution by Gender

str(NewEra)
Women <- NewEra[NewEra$Gender == "Female",]
str(Women)
Men <- NewEra[NewEra$Gender == "Male",]
str(Men)


summary(Men$Time)
summary(Women$Time)

par(mfrow=c(1,2))

hist(Men$Time, main = "Men Winners and their Time", prob = TRUE, col = "skyblue")
lines(density(Men$Time))

hist(Women$Time, main = "Women Winners and their Time", prob = TRUE, col = "purple1")
lines(density(Women$Time))

# Comments about the plots #


# In the New Era, the variance in winning times has become incredibly narrow. The data shows we are 
# looking at a highly optimized, hyper-competitive environment where winning times are consistently 
# clustered around a tight mean, leaving almost zero margin for a 'slow' winning performance



# When we isolate the New Era, the distributions for Men and Women are strictly bimodal with virtually 
# no overlap. This confirms that Gender is the primary underlying driver of baseline speed, meaning any 
# predictive model we build must account for this fixed physiological difference before evaluating other 
# variables.



# Both distributions exhibit a slight rightward skew. Because we are looking at the absolute limits of 
# human physiology, there is a 'hard floor' on how fast someone can run, but a 'soft ceiling' on how slow 
# a winning time can be due to tactical races or severe weather conditions pulling times to the right


# SLIDE IN PRESENTATION SINCE WE ARE STAYING IN NEW ERA FROM NOW ON FOR THE ANALYSIS


###################################################################################################
###################################################################################################

############ NEW VARIABLE : Region

table(data$Country)

dim(data)[1]


n <- dim(data)[1]

data$Region <- c()

for(i in 1:n){
	if(data$Country[i]== "Kenya" | data$Country[i]== "Ethiopia" | data$Country[i]== "Tanzania" | 
data$Country[i]== "Eritrea" | data$Country[i]== "Uganda"){
		data$Region[i] <- "East Africa"
} else if(data$Country[i]== "United States"){
		data$Region[i] <- "USA"
} else if(data$Country[i]== "Japan" | data$Country[i]== "South Korea" | data$Country[i]== "China" | 
data$Country[i]== "North Korea"){
		data$Region[i] <- "Asia"
} else { 
		data$Region[i] <- "Europe"
}
}


table(data$Region)


################ SHOES HANDLING 


table(data$Shoes)



# Directly re-assign the specific levels to "Other"

levels(data$Shoes)[levels(data$Shoes) == "Not specified"] <- "Other"
levels(data$Shoes)[levels(data$Shoes) == "Brooks"] <- "Other"
levels(data$Shoes)[levels(data$Shoes) == "Skechers"] <- "Other"


table(data$Shoes)


####################################### MODELS ##########################################

## Underdog Meeting

# From now on we are only studying the New Era (1980-2018)


str(data)

# Adding New Era Variable in the data

data$NewEra <- ifelse(data$YearofEvent >= 1980,1,0)

table(data$NewEra)
str(NewEra)


head(NewEra,30)

#################### Solve the issues with singularities in the shoes & country

table(NewEra$Shoes)

# 1. Reset the variable to character
NewEra$Shoes <- as.character(NewEra$Shoes)

# 2. Lock in ONLY the three Titans (N > 50)
is_titan_shoe <- NewEra$Shoes == "Adidas" | 
                 NewEra$Shoes == "Asics" | 
                 NewEra$Shoes == "Nike"

# 3. Push Diadora, New Balance, and everything else into "Other"
NewEra$Shoes[!is_titan_shoe] <- "Other"

# 4. Lock it back as a factor
NewEra$Shoes <- as.factor(NewEra$Shoes)

# 5. Set the baseline to Adidas
NewEra$Shoes <- relevel(NewEra$Shoes, ref = "Adidas")

# 6. Verify the clean, unbreakable matrix
table(NewEra$Shoes)


#################################################

sort(table(NewEra$Country))

# ==========================================
# PRE-SPLIT COUNTRY QUARANTINE
# ==========================================

# 1. Reset the variable to character for safe manipulation
NewEra$Country <- as.character(NewEra$Country)

# 2. Lock in ONLY the major marathon powerhouse nations
is_titan_country <- NewEra$Country == "Kenya" | 
                    NewEra$Country == "Ethiopia" | 
                    NewEra$Country == "United States" | 
                    NewEra$Country == "Germany" | 
                    NewEra$Country == "United Kingdom" |
                    NewEra$Country == "Japan"

# 3. Push all other nations (with low variance) into "Other"
NewEra$Country[!is_titan_country] <- "Other"

# 4. Lock it back as a factor
NewEra$Country <- as.factor(NewEra$Country)

# 5. Set the baseline to the United States
# This forces the model to explicitly calculate the "East African Speed Premium" 
# compared to the historical American standard.
NewEra$Country <- relevel(NewEra$Country, ref = "United States")

# 6. Verify the clean, unbreakable matrix
table(NewEra$Country)


#################################################

# PREPARATION & DATA SPLIT

#install.packages("glmnet")
library(glmnet)


# Dropping unique variables and Region because we will keep the Country in order
# to have more accurate and interesting conclusions


str(NewEra)

str(data)

NewEra_clean <- NewEra[,-c(3,6)]

str(NewEra_clean)


set.seed(888) 

# Create an 80/20 split

sample_size <- floor(0.80 * nrow(NewEra_clean))
train_indices <- sample(seq_len(nrow(NewEra_clean)), size = sample_size)

train_data <- NewEra_clean[train_indices, ]
test_data  <- NewEra_clean[-train_indices, ]



####################################

# THE UPGRADED BLIND STEPWISE SELECTION

str(data)
cols_to_hide <- names(train_data) == "Event" | names(train_data) == "Name" | names(train_data) == "YearofEvent"

# 2. Create the blind dataset by keeping everything EXCEPT those three columns
train_data_blind <- train_data[, !cols_to_hide]

# (Proceed with the Stepwise selection exactly as you did before using train_data_blind)



null_model <- lm(Time ~ 1, data = train_data_blind)


# The Full Model (all variables EXCEPT Event)
full_model <- lm(Time ~ ., data = train_data_blind)


# Stepwise algorithm 


step_model <- step(null_model, 
                   scope = list(lower = null_model, upper = full_model), 
                   direction = "both",
                   trace = 0) 

summary(step_model)


#################################################

# THE OLS BASELINE

final_ols <- lm(Time ~ Gender + Country + Route_Feature + Shoes + Humidity, data = train_data)

summary(final_ols)

predictions <- predict(final_ols, newdata = test_data)
rmse <- sqrt(mean((test_data$Time - predictions)^2))
cat("\nOut-of-Sample RMSE:", round(rmse, 4), "hours\n")

# THE DIAGNOSTIC PROOF

library(lmtest)
library(car)

shapiro.test(residuals(final_ols)) 
bptest(final_ols)                  
dwtest(final_ols)                  
vif(final_ols)                     



# THE GLM PIVOT

glm_log <- glm(Time ~ Gender + Country + Route_Feature + Shoes + Humidity, 
               family = Gamma(link = "log"), data = train_data)

glm_inverse <- glm(Time ~ Gender + Country + Route_Feature + Shoes + Humidity, 
                   family = Gamma(link = "inverse"), data = train_data)

glm_identity <- glm(Time ~ Gender + Country + Route_Feature + Shoes + Humidity, 
                    family = Gamma(link = "identity"), data = train_data)

cat("\n--- LINK FUNCTION AIC SHOWDOWN ---\n")
cat("Log Link AIC:     ", AIC(glm_log), "\n")
cat("Inverse Link AIC: ", AIC(glm_inverse), "\n")
cat("Identity Link AIC:", AIC(glm_identity), "\n")


##################################################


# THE BASELINE GLMM (Main Effects Only)


library(lme4)


# Event is strictly quarantined inside (1 | Event)

final_glmm <- glmer(Time ~ Gender + Country + Route_Feature + Shoes + Humidity + (1 | Event), 
                    family = Gamma(link = "inverse"), 
                    data = train_data,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))


summary(final_glmm)



#############################################

# RESEARCH FOR INTERACTIONS


# The Base Model 
base_model <- lm(Time ~ Gender + Country + Route_Feature + Shoes + Humidity, 
                 data = train_data_blind)

interaction_scope <- lm(Time ~ Gender + Country + Route_Feature + Shoes + Humidity + 
                        Gender:Shoes +           
                        Gender:Country + 
                        Gender:Route_Feature + 
                        Gender:Humidity + 
                        Country:Humidity + 
                        Country:Shoes +          
                        Route_Feature:Shoes + 
                        Shoes:Humidity, 
                        data = train_data_blind)


# Forward Stepwise Selection 

# The algorithm starts with the base variables and tests every single interaction 
# listed above to see if it significantly lowers the AIC


step_interactions_massive <- step(base_model, 
                                  scope = list(lower = base_model, upper = interaction_scope), 
                                  direction = "forward",
                                  trace = 0) 

summary(step_interactions_massive)


######################################


# FINAL MODEL 

library(lme4)

final_glmm <- glmer(Time ~ Gender + Country + Route_Feature + Shoes + Humidity + 
                           Gender:Shoes + 
                           Gender:Route_Feature + 
                           Route_Feature:Shoes + 
                           (1 | Event), 
                    family = Gamma(link = "inverse"), 
                    data = train_data,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))


summary(final_glmm)

# We neutralize the two massive vulnerabilities: Time Confounding and Pseudoreplication


final_glmm_stress <- glmer(Time ~ Gender + Country + Route_Feature + Shoes + Humidity + 
                                  Gender:Shoes + 
                                  Gender:Route_Feature + 
                                  Route_Feature:Shoes + 
                                  scale(YearofEvent) +             
                                  (1 | Event) + 
                                  (1 | Name),       
                           family = Gamma(link = "inverse"), 
                           data = train_data,
                           control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))


summary(final_glmm_stress)

cat("\n--- THE FINAL LIKELIHOOD RATIO TEST ---\n")
anova(final_glmm, final_glmm_stress)


# RE-TESTING BIOLOGICAL AGING IN THE NEW MATRIX


final_glmm_stress_age <- glmer(Time ~ Gender + Country + Route_Feature + Shoes + Humidity + 
                                      Gender:Shoes + 
                                      Gender:Route_Feature + 
                                      Route_Feature:Shoes + 
                                      scale(YearofEvent) + 
                                      Age + I(Age^2) +           
                                      (1 | Event) + 
                                      (1 | Name),               
                               family = Gamma(link = "inverse"), 
                               data = train_data,
                               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))


summary(final_glmm_stress_age)

cat("\n--- LIKELIHOOD RATIO TEST (Stress Model vs Stress Model + Age) ---\n")
anova(final_glmm_stress, final_glmm_stress_age)

# Age is not a significant factor once again


############################


### My Final Model



final_glmm_stress <- glmer(Time ~ Gender + Country + Route_Feature + Shoes + Humidity + 
                                  Gender:Shoes + 
                                  Gender:Route_Feature + 
                                  Route_Feature:Shoes + 
                                  scale(YearofEvent) +             
                                  (1 | Event) + 
                                  (1 | Name),       
                           family = Gamma(link = "inverse"), 
                           data = train_data,
                           control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))


summary(final_glmm_stress)

#################################

# install.packages("splines")
library(splines)

cat("\n--- FITTING THE GLMM WITH SCALED AGE SPLINES ---\n")

# We scale Age inside the spline to help the optimizer calculate the complex curves,
# and we double the maximum function evaluations to 200,000.
final_glmm_spline_fixed <- glmer(Time ~ Gender + Country + Route_Feature + Shoes + Humidity + 
                                        Gender:Shoes + 
                                        Gender:Route_Feature + 
                                        Route_Feature:Shoes + 
                                        scale(YearofEvent) + 
                                        ns(scale(Age), df = 3) +   # The Optimizer Fix
                                        (1 | Event) + 
                                        (1 | Name),               
                                 family = Gamma(link = "inverse"), 
                                 data = train_data,
                                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

cat("\n--- VERIFIED LIKELIHOOD RATIO TEST ---\n")
anova(final_glmm_stress, final_glmm_spline_fixed)

summary(final_glmm_spline_fixed)

#################################


#### PREDICTIONS ####

# ==========================================
# PHASE 4: FINAL PREDICTIVE SIMULATION (SPLINE MODEL)
# ==========================================

cat("\n--- GENERATING PREDICTIONS ON TEST DATA ---\n")

# ONLY CHANGE: Swapped final_glmm_stress for final_glmm_spline_fixed
test_data$Predicted_Time <- predict(final_glmm_spline_fixed, 
                                    newdata = test_data, 
                                    type = "response",          
                                    allow.new.levels = TRUE)    

# Calculating Error Metrics
rmse_value <- sqrt(mean((test_data$Time - test_data$Predicted_Time)^2))
mae_value <- mean(abs(test_data$Time - test_data$Predicted_Time))

cat("RMSE (Root Mean Square Error):", round(rmse_value, 4), "\n")
cat("MAE (Mean Absolute Error):    ", round(mae_value, 4), "\n")

# Visualizing the Predictive Accuracy
plot(test_data$Predicted_Time, test_data$Time,
     main = "GLMM Predictions vs Actual Finish Times (Spline Model)",
     xlab = "Predicted Time (Hours)",
     ylab = "Actual Time (Hours)",
     pch = 16, col = "darkblue", cex = 0.8)
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)



#################################

