##  Install package if required 
#install.packages("MASS")
#install.packages("car")


#Load the data to variable "carprice"
carprice <- read.csv("CarPrice_Assignment.csv")

View(carprice)
str(carprice)

# New derived column companyname, separating companyname
carprice$companyname <-   gsub( " .*$", "", carprice$CarName )

# filtering out data which are significant, carprice_filtered is new data frame 
# Column carid and carname removed as they will not have significance on prediction 
carprice_filtered <- subset(carprice, select = -c(1,3) )

View(carprice_filtered)

##Data Cleaning
# performing EDA on companyname.
# Replacing redundant and wrong data with correct one
carprice_filtered$companyname <- as.character(carprice_filtered$companyname)
carprice_filtered$companyname[carprice_filtered$companyname == "vw"] <- "volkswagen"
carprice_filtered$companyname[carprice_filtered$companyname == "toyouta"] <- "toyota"
carprice_filtered$companyname[carprice_filtered$companyname == "vokswagen"] <- "volkswagen"
carprice_filtered$companyname[carprice_filtered$companyname == "maxda"] <- "mazda"
carprice_filtered$companyname[carprice_filtered$companyname == "Nissan"] <- "nissan"
carprice_filtered$companyname[carprice_filtered$companyname == "porcshce"] <- "porsche"

table(carprice_filtered$companyname)

#****************Dummy variable creation******************#

# Creating multiple dummy variables with labels and model.matrix 

##1## fueltype
str(carprice_filtered$fueltype)
summary(carprice_filtered$fueltype)
#convert fueltype variable to numeric is to replace the levels- diesel and gas with 1 and 0 is:
levels(carprice_filtered$fueltype)<-c(1,0)
carprice_filtered$fueltype<- as.numeric(levels(carprice_filtered$fueltype))[carprice_filtered$fueltype]

##2## aspiration
str(carprice_filtered$aspiration)
summary(carprice_filtered$aspiration)
#convert aspiration variable to numeric is to replace the levels- std and turbo with 1 and 0 is:
levels(carprice_filtered$aspiration)<-c(1,0)
carprice_filtered$aspiration<- as.numeric(levels(carprice_filtered$aspiration))[carprice_filtered$aspiration]

##3## doornumber
str(carprice_filtered$doornumber)
summary(carprice_filtered$doornumber)
levels(carprice_filtered$doornumber)<-c(1,0)
carprice_filtered$doornumber<- as.numeric(levels(carprice_filtered$doornumber))[carprice_filtered$doornumber]


##4## carbody 
str(carprice_filtered$carbody)
summary(carprice_filtered$carbody)

# creating dummy variable dummy_carbody using model.matrix 
dummy_carbody <- data.frame(model.matrix( ~carbody, data = carprice_filtered))
View(dummy_carbody)

#remove dummy
dummy_carbody <- dummy_carbody[,-1]
View(dummy_drivewheel)

#Remove and combine
carprice_filtered <- subset(carprice_filtered, select = -c(5) )
carprice_filtered <- cbind(carprice_filtered[,-25], dummy_carbody)

##5## Drivewheel
summary(carprice_filtered$drivewheel)

# creating dummy variable dummy_drivewheel using model.matrix
dummy_drivewheel <- data.frame(model.matrix( ~drivewheel, data = carprice_filtered))
View(dummy_drivewheel)

#remove dummy
dummy_drivewheel <- dummy_drivewheel[,-1]
View(dummy_drivewheel)

#Remove and combine
carprice_filtered <- subset(carprice_filtered, select = -c(5) )
carprice_filtered <- cbind(carprice_filtered[,-28], dummy_drivewheel)

##6## enginelocation
str(carprice_filtered$enginelocation)
summary(carprice_filtered$enginelocation)
levels(carprice_filtered$enginelocation)<-c(1,0)
carprice_filtered$enginelocation<- as.numeric(levels(carprice_filtered$enginelocation))[carprice_filtered$enginelocation]

##7## Enginetype
summary(carprice_filtered$enginetype)

# creating dummy variable dummy_enginetype using model.matrix
dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = carprice_filtered))
View(dummy_enginetype)

#remove dummy
dummy_enginetype <- dummy_enginetype[,-1]
View(dummy_enginetype)

#Remove and combine
carprice_filtered <- subset(carprice_filtered, select = -c(11) )
carprice_filtered <- cbind(carprice_filtered[,-30], dummy_enginetype)



##8## cylindernumber

summary(carprice_filtered$cylindernumber)
levels(carprice_filtered$cylindernumber) <- c("8", "5", "4", "6", "3", "12", "2")
summary(carprice_filtered$cylindernumber)
View(carprice_filtered)

##9## fuelsystem
summary(carprice_filtered$fuelsystem)

# creating dummy variable dummy_fuelsystem using model.matrix
dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = carprice_filtered))
View(dummy_fuelsystem)

#remove dummy
dummy_fuelsystem <- dummy_fuelsystem[,-1]
View(dummy_fuelsystem)

#Remove and combine
carprice_filtered <- subset(carprice_filtered, select = -c(13) )
carprice_filtered <- cbind(carprice_filtered[,-34], dummy_fuelsystem)


##10 Companyname

#summary of the companyname
table(carprice_filtered$companyname)

# creating dummy variable dummy_companyname using model.matrix
dummy_companyname <- data.frame(model.matrix( ~companyname, data = carprice_filtered))
View(dummy_companyname)

#remove dummy
dummy_companyname <- dummy_companyname[,-1]
View(dummy_companyname)

#Remove and combine
carprice_filtered <- subset(carprice_filtered, select = -c(21) )
carprice_filtered <- cbind(carprice_filtered[,-41], dummy_companyname)


# separate training and testing data
# separating 70% data for training and rest for testing
set.seed(100)
trainindices_cp= sample(1:nrow(carprice_filtered), 0.7*nrow(carprice_filtered))
train_cp = carprice_filtered[trainindices_cp,]
test_cp = carprice_filtered[-trainindices_cp,]

#*************************************************
#                 Model creation
#*************************************************

# Build model 1 containing all variables
modelcp_1 <-lm(price~.,data=train_cp)
summary(modelcp_1)

# Load library for stepAIC
library(MASS)

step_cp <- stepAIC(modelcp_1, direction="both")

step_cp

#Model 2 ,  step model executed  here,
modelcp_2 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                  carlength + carwidth + carheight + curbweight + cylindernumber + 
                  enginesize + boreratio + stroke + peakrpm + carbodyhardtop + 
                  carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                  enginetypeohcv + fuelsystemspdi + companynameaudi + companynamebmw + 
                  companynamebuick + companynamedodge + companynamehonda + 
                  companynamemazda + companynamemitsubishi + companynamenissan + 
                  companynameplymouth + companynameporsche + companynamesaab + 
                  companynamevolkswagen + fuelsystem2bbl, data = train_cp)



summary(modelcp_2)



# load the package for VIF calculation
library("car")

## Let us check for multicollinearity 
# If the VIF is above 2 or 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
vif(modelcp_2)

##***modelcp_2 Observation
#Multiple R-squared:  0.9785,	Adjusted R-squared:  0.9709 
# cylindernumber has a high VIF of 6163.162531 and has a high p-value (cylindernumber5=0.516864,cylindernumber6= 0.742881 )
#Hence we can remove cylindernumber

#Model 3

modelcp_3 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                  carlength + carwidth + carheight + curbweight  + 
                  enginesize + boreratio + stroke + peakrpm + carbodyhardtop + 
                  carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                  enginetypeohcv + fuelsystemspdi + companynameaudi + companynamebmw + 
                  companynamebuick + companynamedodge + companynamehonda + 
                  companynamemazda + companynamemitsubishi + companynamenissan + 
                  companynameplymouth + companynameporsche + companynamesaab + 
                  companynamevolkswagen + fuelsystem2bbl, data = train_cp)

summary(modelcp_3)
vif(modelcp_3)

## Observation on model 3
#Multiple R-squared:  0.9583,	Adjusted R-squared:  0.9467
# curbweight has VIF of 24.868160 and has p-value of 0.00341 which is low, and hence it is significant. Can not be removed.
# carlength  has VIF of 17.970348 and has p-value of 0.12431 which is not that much low.


# MOdel 4
# removing carlength 
modelcp_4 <- lm(formula = price ~ symboling + aspiration + enginelocation 
                   + carwidth + carheight + curbweight  + 
                  enginesize + boreratio + stroke + peakrpm + carbodyhardtop + 
                  carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                  enginetypeohcv + fuelsystemspdi + companynameaudi + companynamebmw + 
                  companynamebuick + companynamedodge + companynamehonda + 
                  companynamemazda + companynamemitsubishi + companynamenissan + 
                  companynameplymouth + companynameporsche + companynamesaab + 
                  companynamevolkswagen + fuelsystem2bbl, data = train_cp)

summary(modelcp_4)
vif(modelcp_4)

##observation on model 4
#Multiple R-squared:  0.9574,	Adjusted R-squared:  0.946 
# enginesize has VIF of 14.810500 and p-value of 9.89e-12, which is low, and hence it is significant. Can not be removed.
# carbodysedan has a VIF of 15.632469 and p-value is 0.00597 which is low, and hence it is significant. Can not be removed.
# carbodyhatchback has a VIF of 13.095539 and p-value of 0.00597 which is low, and hence it is significant. Can not be removed.
# carwidth has a VIF of 8.844470 and a p-value of 0.01262 which is comparatively low, so we will not remocve this.
# enginelocation has VIF of 5.321290 and a p-value of 0.02610 which is comparatively low, so we will not remocve this.
# boreratio ahs a VIF of 3.593998 and a p-vlaue of 0.08960

#** Model 5
# Removing boreratio
modelcp_5 <- lm(formula = price ~ symboling + aspiration + enginelocation 
                + carwidth + carheight + curbweight  + 
                  enginesize  + stroke + peakrpm + carbodyhardtop + 
                  carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                  enginetypeohcv + fuelsystemspdi + companynameaudi + companynamebmw + 
                  companynamebuick + companynamedodge + companynamehonda + 
                  companynamemazda + companynamemitsubishi + companynamenissan + 
                  companynameplymouth + companynameporsche + companynamesaab + 
                  companynamevolkswagen + fuelsystem2bbl, data = train_cp)

summary(modelcp_5)
vif(modelcp_5)

#Observation of model 5
# Multiple R-squared:  0.9546,	Adjusted R-squared:  0.9429 
# No much change in Adjusted R-squared value
# enginetypeohcv has a VIF of 4.040995 and p-value of 0.034762,which is comparatively low, so we will not remocve this.
# companynameporsche has a VIF of 4.620018 and p-value of 0.038430, which is comparatively low, so we will not remocve this.
# symboling has a VIF of 3.464907 and p-value of 0.286670, hence can be removed

# Model 6 
# Removing symboling

modelcp_6 <- lm(formula = price ~  aspiration + enginelocation 
                + carwidth + carheight + curbweight  + 
                  enginesize  + stroke + peakrpm + carbodyhardtop + 
                  carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                  enginetypeohcv + fuelsystemspdi + companynameaudi + companynamebmw + 
                  companynamebuick + companynamedodge + companynamehonda + 
                  companynamemazda + companynamemitsubishi + companynamenissan + 
                  companynameplymouth + companynameporsche + companynamesaab + 
                  companynamevolkswagen + fuelsystem2bbl, data = train_cp)

summary(modelcp_6)
vif(modelcp_6)

## observation of Model 6
# Multiple R-squared:  0.9541,	Adjusted R-squared:  0.9429
# No much change in Adjusted R-squared value
# carheight has VIF of 3.145625 and p-value of 0.127093, comparitively higher hence can be removed


# Model 7 
# Removing carheight

modelcp_7 <- lm(formula = price ~  aspiration + enginelocation 
                + carwidth  + curbweight  + 
                  enginesize  + stroke + peakrpm + carbodyhardtop + 
                  carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                  enginetypeohcv + fuelsystemspdi + companynameaudi + companynamebmw + 
                  companynamebuick + companynamedodge + companynamehonda + 
                  companynamemazda + companynamemitsubishi + companynamenissan + 
                  companynameplymouth + companynameporsche + companynamesaab + 
                  companynamevolkswagen + fuelsystem2bbl, data = train_cp)

summary(modelcp_7)
vif(modelcp_7)

#Observation on model 7
# Multiple R-squared:  0.9532,	Adjusted R-squared:  0.9422 
# No much change in Adjusted R-squared value
# carbodyhardtop has a VIF of 2.648105 and p-value = 0.020320, significant, so can not be removed
# fuelsystemspdi has a VIF of 2.238519 and p-value = 0.350990, higher p-value , so can be removed

# Model 8 
# Removing fuelsystemspdi

modelcp_8 <- lm(formula = price ~  aspiration + enginelocation 
                + carwidth  + curbweight  + 
                  enginesize  + stroke + peakrpm + carbodyhardtop + 
                  carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                  enginetypeohcv  + companynameaudi + companynamebmw + 
                  companynamebuick + companynamedodge + companynamehonda + 
                  companynamemazda + companynamemitsubishi + companynamenissan + 
                  companynameplymouth + companynameporsche + companynamesaab + 
                  companynamevolkswagen + fuelsystem2bbl, data = train_cp)

summary(modelcp_8)
vif(modelcp_8)

#Observation on model 8
# Multiple R-squared:  0.9528,	Adjusted R-squared:  0.9422 
# No much change in Adjusted R-squared value
# fuelsystem2bbl VIF = 2.729891 , p-value = 0.048599, cannot be removed as it is significant.
# companynamenissan VIF = 2.247860, p-value = 0.291240, comparitively higher so can be removed.



# Model 9 
# Removing companynamenissan

modelcp_9 <- lm(formula = price ~  aspiration + enginelocation 
                + carwidth  + curbweight  + 
                  enginesize  + stroke + peakrpm + carbodyhardtop + 
                  carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                  enginetypeohcv  + companynameaudi + companynamebmw + 
                  companynamebuick + companynamedodge + companynamehonda + 
                  companynamemazda + companynamemitsubishi + 
                  companynameplymouth + companynameporsche + companynamesaab + 
                  companynamevolkswagen + fuelsystem2bbl, data = train_cp)

summary(modelcp_9)
vif(modelcp_9)

#Observation on model 9
# Multiple R-squared:  0.9524,	Adjusted R-squared:  0.9422 
# No much change in Adjusted R-squared value
# Now we have analysed all variable which VIF > 2 and kept/removed them based on their p-value


#*************Filtering based on p-value now  ****************

# Model 10
# Removing companynamevolkswagen

modelcp_10 <- lm(formula = price ~  aspiration + enginelocation 
                + carwidth  + curbweight  + 
                  enginesize  + stroke + peakrpm + carbodyhardtop + 
                  carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                  enginetypeohcv  + companynameaudi + companynamebmw + 
                  companynamebuick + companynamedodge + companynamehonda + 
                  companynamemazda + companynamemitsubishi + 
                  companynameplymouth + companynameporsche + companynamesaab + 
                   fuelsystem2bbl, data = train_cp)

summary(modelcp_10)

## Observation on Model 10
# Multiple R-squared:  0.9522,	Adjusted R-squared:  0.9424 
# companynamesaab p-value 0.159484, can be removed in next model.


# Model 11
# Removing companynamesaab

modelcp_11 <- lm(formula = price ~  aspiration + enginelocation 
                 + carwidth  + curbweight  + 
                   enginesize  + stroke + peakrpm + carbodyhardtop + 
                   carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                   enginetypeohcv  + companynameaudi + companynamebmw + 
                   companynamebuick + companynamedodge + companynamehonda + 
                   companynamemazda + companynamemitsubishi + 
                   companynameplymouth + companynameporsche  + 
                   fuelsystem2bbl, data = train_cp)

summary(modelcp_11)

## Observation on Model 11
# Multiple R-squared:  0.9513,	Adjusted R-squared:  0.9419 
# enginetypel p-value 0.609007 can be removed in next model



# Model 12
# Removing enginetypel

modelcp_12 <- lm(formula = price ~  aspiration + enginelocation 
                 + carwidth  + curbweight  + 
                   enginesize  + stroke + peakrpm + carbodyhardtop + 
                   carbodyhatchback + carbodysedan + carbodywagon  + 
                   enginetypeohcv  + companynameaudi + companynamebmw + 
                   companynamebuick + companynamedodge + companynamehonda + 
                   companynamemazda + companynamemitsubishi + 
                   companynameplymouth + companynameporsche  + 
                   fuelsystem2bbl, data = train_cp)

summary(modelcp_12)


## Observation on Model 12
# Multiple R-squared:  0.9512,	Adjusted R-squared:  0.9423 
# companynamehonda p-value = 0.620935, can be removed in next model


# Model 13
# Removing companynamehonda

modelcp_13 <- lm(formula = price ~  aspiration + enginelocation 
                 + carwidth  + curbweight  + 
                   enginesize  + stroke + peakrpm + carbodyhardtop + 
                   carbodyhatchback + carbodysedan + carbodywagon  + 
                   enginetypeohcv  + companynameaudi + companynamebmw + 
                   companynamebuick + companynamedodge  + 
                   companynamemazda + companynamemitsubishi + 
                   companynameplymouth + companynameporsche  + 
                   fuelsystem2bbl, data = train_cp)

summary(modelcp_13)

## Observation on Model 13
#Multiple R-squared:  0.9511,	Adjusted R-squared:  0.9427 
# companynamedodge p-value = 0.226915 can be remoced in next model


# Model 14
# Removing companynamedodge

modelcp_14 <- lm(formula = price ~  aspiration + enginelocation 
                 + carwidth  + curbweight  + 
                   enginesize  + stroke + peakrpm + carbodyhardtop + 
                   carbodyhatchback + carbodysedan + carbodywagon  + 
                   enginetypeohcv  + companynameaudi + companynamebmw + 
                   companynamebuick   + 
                   companynamemazda + companynamemitsubishi + 
                   companynameplymouth + companynameporsche  + 
                   fuelsystem2bbl, data = train_cp)

summary(modelcp_14)

## Observation on Model 14
# Multiple R-squared:  0.9505,	Adjusted R-squared:  0.9424 
# companynameplymouth p-value = 0.133069, can be removed in next model


# Model 15
# Removing companynameplymouth

modelcp_15 <- lm(formula = price ~  aspiration + enginelocation 
                 + carwidth  + curbweight  + 
                   enginesize  + stroke + peakrpm + carbodyhardtop + 
                   carbodyhatchback + carbodysedan + carbodywagon  + 
                   enginetypeohcv  + companynameaudi + companynamebmw + 
                   companynamebuick   + 
                   companynamemazda + companynamemitsubishi + 
                    companynameporsche  + 
                   fuelsystem2bbl, data = train_cp)

summary(modelcp_15)


## Observation on Model 15
# Multiple R-squared:  0.9496,	Adjusted R-squared:  0.9418 
# fuelsystem2bbl p-value = 0.178521, can be removed in next model



# Model 16
# Removing fuelsystem2bbl

modelcp_16 <- lm(formula = price ~  aspiration + enginelocation 
                 + carwidth  + curbweight  + 
                   enginesize  + stroke + peakrpm + carbodyhardtop + 
                   carbodyhatchback + carbodysedan + carbodywagon  + 
                   enginetypeohcv  + companynameaudi + companynamebmw + 
                   companynamebuick   + 
                   companynamemazda + companynamemitsubishi + 
                   companynameporsche  , data = train_cp)

summary(modelcp_16)


## Observation on Model 16
# Multiple R-squared:  0.9489,	Adjusted R-squared:  0.9414 
# companynamemitsubishi p-value = 0.058659, can be removed in next model


# Model 17
# Removing companynamemitsubishi

modelcp_17 <- lm(formula = price ~  aspiration + enginelocation 
                 + carwidth  + curbweight  + 
                   enginesize  + stroke + peakrpm + carbodyhardtop + 
                   carbodyhatchback + carbodysedan + carbodywagon  + 
                   enginetypeohcv  + companynameaudi + companynamebmw + 
                   companynamebuick   + 
                   companynamemazda  + 
                   companynameporsche  , data = train_cp)

summary(modelcp_17)


## Observation on Model 17
# Multiple R-squared:  0.9474,	Adjusted R-squared:  0.9402 
# Now will remove variable based on star(*) value
# companynameporsche will remove in next model


# Model 18
# Removing companynameporsche

modelcp_18 <- lm(formula = price ~  aspiration + enginelocation 
                 + carwidth  + curbweight  + 
                   enginesize  + stroke + peakrpm + carbodyhardtop + 
                   carbodyhatchback + carbodysedan + carbodywagon  + 
                   enginetypeohcv  + companynameaudi + companynamebmw + 
                   companynamebuick   + 
                   companynamemazda  , data = train_cp)

summary(modelcp_18)


## Observation on Model 18
# Multiple R-squared:  0.9456,	Adjusted R-squared:  0.9387 
# companynameaudi p-value = 0.062871, can be removed in next model


# Model 19
# Removing companynameaudi

modelcp_19 <- lm(formula = price ~  aspiration + enginelocation 
                 + carwidth  + curbweight  + 
                   enginesize  + stroke + peakrpm + carbodyhardtop + 
                   carbodyhatchback + carbodysedan + carbodywagon  + 
                   enginetypeohcv   + companynamebmw + 
                   companynamebuick   + 
                   companynamemazda  , data = train_cp)

summary(modelcp_19)


## Observation on Model 19
# Multiple R-squared:  0.9456,	Adjusted R-squared:  0.9387 
# carbodyhardtop p-value = 0.039554, can be removed in next model


# Model 20
# Removing carbodyhardtop

modelcp_20 <- lm(formula = price ~  aspiration + enginelocation 
                 + carwidth  + curbweight  + 
                   enginesize  + stroke + peakrpm +  
                   carbodyhatchback + carbodysedan + carbodywagon  + 
                   enginetypeohcv   + companynamebmw + 
                   companynamebuick   + 
                   companynamemazda  , data = train_cp)

summary(modelcp_20)


## Observation on Model 20
# Multiple R-squared:  0.9422,	Adjusted R-squared:  0.9359 
# carbodysedan p-value = 0.067648, can be removed in next model


# Model 21
# Removing carbodysedan

modelcp_21 <- lm(formula = price ~  aspiration + enginelocation 
                 + carwidth  + curbweight  + 
                   enginesize  + stroke + peakrpm +  
                   carbodyhatchback  + carbodywagon  + 
                   enginetypeohcv   + companynamebmw + 
                   companynamebuick   + 
                   companynamemazda  , data = train_cp)

summary(modelcp_21)


## Observation on Model 21
# Multiple R-squared:  0.9407,	Adjusted R-squared:  0.9347 
# carbodyhatchback p-value = 0.301631, can be removed in next model


# Model 22
# Removing carbodyhatchback

modelcp_22 <- lm(formula = price ~  aspiration + enginelocation 
                 + carwidth  + curbweight  + 
                   enginesize  + stroke + peakrpm + carbodywagon  + 
                   enginetypeohcv   + companynamebmw + 
                   companynamebuick   + 
                   companynamemazda  , data = train_cp)

summary(modelcp_22)


## Observation on Model 22
# Multiple R-squared:  0.9402,	Adjusted R-squared:  0.9347 
# aspiration p-value = 0.098966, can be removed in next model


# Model 23
# Removing aspiration

modelcp_23 <- lm(formula = price ~  enginelocation 
                 + carwidth  + curbweight  + 
                   enginesize  + stroke + peakrpm + carbodywagon  + 
                   enginetypeohcv   + companynamebmw + 
                   companynamebuick   + 
                   companynamemazda  , data = train_cp)

summary(modelcp_23)


## Observation on Model 23
# Multiple R-squared:  0.9389,	Adjusted R-squared:  0.9338 
# carbodywagon p-value = 0.049871, can be removed in next model


# Model 24
# Removing carbodywagon

modelcp_24 <- lm(formula = price ~  enginelocation 
                 + carwidth  + curbweight  + 
                   enginesize  + stroke + peakrpm +  
                   enginetypeohcv   + companynamebmw + 
                   companynamebuick   + 
                   companynamemazda  , data = train_cp)

summary(modelcp_24)


## Observation on Model 24
# Multiple R-squared:  0.9389,	Adjusted R-squared:  0.9338
# All 1 star variable removed, removing 2 star variable now 
# companynamemazda p-value = 0.004278, can be removed in next model


# Model 25
# Removing companynamemazda

modelcp_25 <- lm(formula = price ~  enginelocation 
                 + carwidth  + curbweight  + 
                   enginesize  + stroke + peakrpm +  
                   enginetypeohcv   + companynamebmw + 
                   companynamebuick  , data = train_cp)

summary(modelcp_25)


## Observation on Model 25
# Multiple R-squared:  0.9331,	Adjusted R-squared:  0.9285
# stroke p-value = 0.007754, can be removed in next model

# Model 26
# Removing stroke

modelcp_26 <- lm(formula = price ~  enginelocation 
                 + carwidth  + curbweight  + 
                   enginesize   + peakrpm +  
                   enginetypeohcv   + companynamebmw + 
                   companynamebuick  , data = train_cp)

summary(modelcp_26)


## Observation on Model 27
# Multiple R-squared:  0.9294,	Adjusted R-squared:  0.9252 
# enginetypeohcv p-value = 0.022713, can be removed in next model

# Model 27
# Removing enginetypeohcv

modelcp_27 <- lm(formula = price ~  enginelocation 
                 + carwidth  + curbweight  + 
                   enginesize   + peakrpm   + companynamebmw + 
                   companynamebuick  , data = train_cp)

summary(modelcp_27)

## Observation on Model 27
# Multiple R-squared:  0.9266,	Adjusted R-squared:  0.9228  
# We have got our model with all p-value much lesser than 0.001 

##*****Important variables in the final model *******
# enginelocation - Location of car engine, has very  low p-value hence plays an imp  role in price determination 
# carwidth- width of car p-value is very less than 0.001, hence this is important variable
# curbweight- The weight of a car without occupants or baggage. p-value is 0.000134, important factor
# enginesize - Size of car, p-value =1.59e-11, very very low, so it is significant variable
# peakrpm- car peak rpm (Numeric)		 p-value is  1.52e-06 significantly low so it is an important factor.
# companynamebmw    p-value is  4.40e-09 significantly low so it is an important factor
# companynamebuick  p-value is 3.29e-10 significantly low so it is an important factor



#Predict with test data
Predict_cp <- predict(modelcp_27,test_cp[,-1])
test_cp$test_price <- Predict_cp

# Now, we need to test the r square between actual and predicted price 
r_cp <- cor(test_cp$price,test_cp$test_price)
rsquared_cp <- cor(test_cp$price,test_cp$test_price)^2
r_cp
rsquared_cp

##***************************************************************##
#rsquared_cp has high value means test data price is highly correlated with the model price
# means model is able to predict the price
#R-squared is a statistical measure of how close the data are to the fitted regression line. 
