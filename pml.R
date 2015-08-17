library(caret)
library(ggbiplot)
library(dplyr)
library(magrittr)
library(GGally)
#library(scatterplot3d)
library(rgl)

library(kernlab)
library(pls)

# --- CONSTANTS ----
set.seed(1000)
training_filename <- "~/Dropbox/Coursera/Predmachlearn_CP/pml-training.csv"
testing_filename <- "~/Dropbox/Coursera/Predmachlearn_CP/pml-testing.csv"

# --- EXPLORATORY FUNCTIONS ----

# Plot 3 variables to see if PCA would reduce dimensionality
# df[data.frame], c[string] -> plot
pcaplot <-function(df, c) {
        tr_pca <- prcomp(select(df, contains(c)), 
                         center=TRUE, 
                         scale.=TRUE)
        ggbiplot(tr_pca, 
                 obs.scale = 1, 
                 var.scale = 1,
                 groups = df$classe, 
                 ellipse = TRUE, 
                 circle = TRUE) +
                scale_color_discrete(name = '') +
                theme(legend.direction = 'horizontal', 
                      legend.position = 'top')
        
}

# Find and plot a heatmap of correlations between all numeric variables
# df[data.frame] -> list(matrix, heatmap)
corplot <- function(df) {
        tr_nofactor <- dplyr::select(df, -c(classe, user_name))
        cm <- cor(tr_nofactor)
        hm <- heatmap(cm, Rowv=NA, Colv=NA, col=heat.colors(256), scale="column", margins=c(5,10))
        list(cm, hm)
}

# Create a rotating 3d plot of the first 3 columns containing c
# df[data.frame], c[string] -> plot
xyzplot <- function(df, c) {
        n <- names(df)
        xyz <- n[grepl(c, n)]
        x <- df[[xyz[1]]]
        y <- df[[xyz[2]]]
        z <- df[[xyz[3]]]
        plot3d(x, y, z,
               type="s",
               col=as.numeric(df$classe))
}

# Create a pairs plot showing more than just scatterplots
# df[data.frame], c[string] -> plot
fancypairs <- function(df, c) {
        ggpairs(dplyr::select(df, classe, user_name, contains(c)),
                diag=list(continuous="density", discrete="bar"), 
                upper=list(continuous="density", discrete="bar", mixed="box"),
                color="classe")
}

# Create a series of boxplots of variables, c, against classe
# df[data.frame], c[string] -> plot
boxplots <- function(df, c) {
        x <- dplyr::select(df, classe, contains(c))
        xtall <- gather(x, "meas","val",-classe)
        qplot(x=classe, y=val, data=xtall) + geom_boxplot() + facet_wrap(~meas, scales="free")
}


# EXPLORE THE DATA
training <- read.csv(training_filename)
testing <- read.csv(testing_filename)

# Select covariates with full range of data -------
trdf <- training %>%
        dplyr::select(classe,
                      user_name,
                      gyros_belt_x,     accel_belt_x,     magnet_belt_x, 
                      gyros_belt_y,     accel_belt_y,     magnet_belt_y, 
                      gyros_belt_z,     accel_belt_z,     magnet_belt_z, 
                      gyros_arm_x,      accel_arm_x,      magnet_arm_x, 
                      gyros_arm_y,      accel_arm_y,      magnet_arm_y, 
                      gyros_arm_z,      accel_arm_z,      magnet_arm_z, 
                      gyros_dumbbell_x, accel_dumbbell_x, magnet_dumbbell_x, 
                      gyros_dumbbell_y, accel_dumbbell_y, magnet_dumbbell_y, 
                      gyros_dumbbell_z, accel_dumbbell_z, magnet_dumbbell_z, 
                      gyros_forearm_x,  accel_forearm_x,  magnet_forearm_x,
                      gyros_forearm_y,  accel_forearm_y,  magnet_forearm_y,
                      gyros_forearm_z,  accel_forearm_z,  magnet_forearm_z,
                      total_accel_belt,
                      total_accel_arm,
                      total_accel_dumbbell,
                      total_accel_forearm,
                      roll_dumbbell,  roll_forearm,  roll_arm,
                      pitch_dumbbell, pitch_forearm, pitch_arm,
                      yaw_dumbbell,   yaw_forearm,   yaw_arm)

# Remove outliers
trdf %<>% filter(accel_belt_x > -100 & 
                         gyros_dumbbell_x > -200 & 
                         gyros_forearm_x > -20 &
                         gyros_dumbbell_y < 40 &
                         magnet_dumbbell_y > -1000 &
                         gyros_forearm_y < 300 &
                         accel_forearm_y < 750 &
                         gyros_dumbbell_z < 300 &
                         gyros_forearm_z < 200 &
                         total_accel_dumbbell < 50 &
                         total_accel_forearm < 90)

# Plot covariates
corplot(trdf)[[1]]

pcaplot(trdf, "accel_belt")
pcaplot(trdf, "magnet_belt")
pcaplot(trdf, "magnet_arm")
pcaplot(trdf, "magnet_dumbbell")

xyzplot(trdf, "accel_belt")
xyzplot(trdf, "magnet_belt")
xyzplot(trdf, "magnet_arm")
xyzplot(trdf, "magnet_dumbbell")

boxplots(trdf, "_x")
boxplots(trdf, "_y")
boxplots(trdf, "_z")
boxplots(trdf, "total_accel")

# variant on selection for fancypairs
fancypairs(trdf, "_x")
fancypairs(trdf, "_y")
fancypairs(trdf, "_z")
fancypairs(dplyr::select(trdf, 
                         classe, 
                         user_name, 
                         contains("roll_"), contains("pitch_"), contains("yaw_")), 
           "_")

# --- PRE-PROCESSING ----

### ---- NEED TO DO SOME TRANSFORMS ON THE DATA, SUCH AS TRAJECTORY, VELOCITY, ACCELERATION

nzv <- nearZeroVar(training19619)
names(training)[nzv]
# this was null.


#dv <- dummyVars(~user_name, data=training)
#dv <- data.frame(predict(dv, newdata=training), training19622)

# -- TRAIN ----

tc0 <- trainControl(method="repeatedcv", 
                   number=10, 
                   repeats=10,
                   classProbs=TRUE,
                   allowParallel=TRUE)
trfit0 <- train(classe ~ .,
               data=trdf,
               trControl=tc0,
               method="pls")
cm <- confusionMatrix(trfit0)
heatmap(cm$table, Rowv=NA, Colv=NA)

# Simple possible model
ptm <- proc.time()
tc <- trainControl(method="repeatedcv", 
                   number=10, 
                   repeats=10,
                   classProbs=TRUE,
                   allowParallel=TRUE)
trfit <- train(classe ~ roll_dumbbell + 
                       magnet_arm_y + 
                       magnet_dumbbell_y + 
                       magnet_forearm_y +
                       accel_belt_z +
                       magnet_belt_x +
                       magnet_belt_y +
                       magnet_belt_z,
               data=trdf,
               trControl=tc,
               method="rf")
proc.time() - ptm
cm <- confusionMatrix(trfit)
cm
heatmap(cm$table, Rowv=NA, Colv=NA)

plot(trfit)

prd <- predict(trfit, newdata=testing)

# Train ------------------------

