# read in libraries
library(nlme)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(psych)

# reading in the mins (nationals) datasets:
set02_min <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set02_min.rds")
set05_min <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set05_min.rds")
set08_min <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set08_min.rds")
set10_min <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set10_min.rds")
set12_min <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set12_min.rds")
set14_min <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set14_min.rds")

# reading in the mins (nationals) based on 05 (ie including Daily Sun)... not developed yet...think about it
set05_min2 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set05_min2.rds")
set08_min2 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set08_min2.rds")
set10_min2 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set10_min2.rds")
set12_min2 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set12_min2.rds")
set14_min2 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set14_min2.rds")

# reading in the models by period
fact_02 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/fact_02.rds")
fact_05 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/fact_05.rds")
fact_08 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2008/fact_08.rds")
fact_10 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/fact_10.rds")
fact_12 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2012/fact_12.rds")
fact_14 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2014/fact_14.rds")

# reading in the loadings by period
fact_02_loadings <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/fact_02_loadings.rds")
fact_05_loadings <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/fact_05_loadings.rds")
fact_08_loadings <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2008/fact_08_loadings.rds")
fact_10_loadings <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/fact_10_loadings.rds")
fact_12_loadings <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2012/fact_12_loadings.rds")
fact_14_loadings <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2014/fact_14_loadings.rds")

# reading in the scores by period
fact_02_scores <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/fact_02_scores.rds")
fact_05_scores <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/fact_05_scores.rds")
fact_08_scores <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2008/fact_08_scores.rds")
fact_10_scores <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/fact_10_scores.rds")
fact_12_scores <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2012/fact_12_scores.rds")
fact_14_scores <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2014/fact_14_scores.rds")

# scores with _12 as factor model
fact_02_scores_model_12 <- predict(fact_12, data = set02_min[,(which(names(set02_min) == "all") + 1):ncol(set02_min)])
fact_05_scores_model_12 <- predict(fact_12, data = set05_min[,(which(names(set05_min) == "all") + 1):ncol(set05_min)])
fact_08_scores_model_12 <- predict(fact_12, data = set08_min[,(which(names(set08_min) == "all") + 1):ncol(set08_min)])
fact_10_scores_model_12 <- predict(fact_12, data = set10_min[,(which(names(set10_min) == "all") + 1):ncol(set10_min)])
fact_12_scores_model_12 <- predict(fact_12, data = set12_min[,(which(names(set12_min) == "all") + 1):ncol(set12_min)])
fact_14_scores_model_12 <- predict(fact_12, data = set14_min[,(which(names(set14_min) == "all") + 1):ncol(set14_min)])

# next create dataset with scaled factors/dimensions instead of media vehicles
set02_factors <- data.frame(cbind(set02_min[1:19], scale(fact_02_scores_model_12)))
set05_factors <- data.frame(cbind(set05_min[1:19], scale(fact_05_scores_model_12)))
set08_factors <- data.frame(cbind(set08_min[1:21], scale(fact_08_scores_model_12)))
set10_factors <- data.frame(cbind(set10_min[1:21], scale(fact_10_scores_model_12)))
set12_factors <- data.frame(cbind(set12_min[1:21], scale(fact_12_scores_model_12)))
set14_factors <- data.frame(cbind(set14_min[1:21], scale(fact_14_scores_model_12)))

# function to create frames
frames_factors <- function(set, category) {
        require(dplyr)
        
        # set$cluster <- factor(set$cluster, labels = c("cluster1", "cluster2", "cluster3", "cluster4"))
        set$age <- factor(set$age, labels = c("15-24","25-44", "45-54","55+"), ordered = TRUE)
        set$race <- factor(set$race,labels = c("black", "coloured", "indian", "white"), ordered = TRUE)
        set$edu <- factor(set$edu, labels = c("<matric", "matric",">matric" ) ,ordered = TRUE)
        set$lsm <- factor(set$lsm, labels = c("LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10"), ordered = TRUE) #"LSM1-2", 
        set$sex <- factor(set$sex, labels = c("male", "female"), ordered = TRUE)
        set$hh_inc <- factor(set$hh_inc, labels = c("<R2500","R2500-R6999","R7000-R11999",">=R12000"), ordered = TRUE) # NB 2012 levels
        
        
        set %>%
                group_by_(category = category) %>%
                summarise(F1 = mean(PA1),
                          F2 = mean(PA2),
                          F3 = mean(PA3),
                          F4 = mean(PA4),
                          F5 = mean(PA5),
                          F6 = mean(PA6),
                          up_f1 = mean(PA1) + (2 * sd(PA1)/sqrt(length(PA1))),
                          low_f1 = mean(PA1) - (2 * sd(PA1)/sqrt(length(PA1))),
                          up_f2 = mean(PA2) + (2 * sd(PA2)/sqrt(length(PA2))),
                          low_f2 = mean(PA2) - (2 * sd(PA2)/sqrt(length(PA2))),
                          up_f3 = mean(PA3) + (2 * sd(PA3)/sqrt(length(PA3))),
                          low_f3 = mean(PA3) - (2 * sd(PA3)/sqrt(length(PA3))),
                          up_f4 = mean(PA4) + (2 * sd(PA4)/sqrt(length(PA4))),
                          low_f4 = mean(PA4) - (2 * sd(PA4)/sqrt(length(PA4))),
                          up_f5 = mean(PA5) + (2 * sd(PA5)/sqrt(length(PA5))),
                          low_f5 = mean(PA5) - (2 * sd(PA5)/sqrt(length(PA5))),
                          up_f6 = mean(PA6) + (2 * sd(PA6)/sqrt(length(PA6))),
                          low_f6 = mean(PA6) - (2 * sd(PA6)/sqrt(length(PA6))))
        
}

# function to bind the frames by year
frame_bind_factor <- function(set, year) {
        rbind.data.frame(#frames_factors(set,"cluster"),
                frames_factors(set,"sex"),
                frames_factors(set,"age"),
                frames_factors(set,"edu"),
                frames_factors(set,"race"),
                frames_factors(set, "hh_inc"),
                frames_factors(set,"lsm")) %>% 
                mutate(year = year) %>%
                select(category, year, everything())
        
}

factor_fr_02 <- frame_bind_factor(set02_factors, 2002)
factor_fr_05 <- frame_bind_factor(set05_factors, 2005)
factor_fr_08 <- frame_bind_factor(set08_factors, 2008)
factor_fr_10 <- frame_bind_factor(set10_factors, 2010)
factor_fr_12 <- frame_bind_factor(set12_factors, 2012)
factor_fr_14 <- frame_bind_factor(set14_factors, 2014)


# putting them togther:
frame_factors <- rbind.data.frame(factor_fr_02,
                                  factor_fr_05,
                                  factor_fr_08,
                                  factor_fr_10,
                                  factor_fr_12,
                                  factor_fr_14)


# EXPLORING
# considering plots of all factors on demographic categories
# defining a function
all_plots_factors <- function(data, title = "All Factors") {
        ggplot(data = data, title = title) +
                geom_line(aes(year, F1, group = category, colour = "F1")) +
                geom_line(aes(year, F2, group = category, colour = "F2")) +
                geom_line(aes(year, F3, group = category, colour = "F3")) +
                geom_line(aes(year, F4, group = category, colour = "F4")) +
                geom_line(aes(year, F5, group = category, colour = "F5")) +
                geom_line(aes(year, F6, group = category, colour = "F6")) +
                scale_colour_discrete(name="Factors") +
                facet_grid(. ~ category) +
                theme(axis.text.x = element_text(size = 6)) +
                labs(y = "aggregate scores", title = title)
        
}

vector_row1 <- c("male", "female","15-24","25-44", "45-54","55+","black", "coloured", "indian", "white")
vector_row2 <- c("<matric", "matric",">matric", "<R2500","R2500-R6999","R7000-R11999",">=R12000", "LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10")
p_up <- all_plots_factors(frame_factors[which(frame_factors$category %in% vector_row1),])
p_down <- all_plots_factors(frame_factors[which(frame_factors$category %in% vector_row2),])

jpeg("all_plots_factors_12.jpeg", quality = 100)
grid.arrange(p_up, p_down, nrow = 2)
dev.off()

# function to plot details eith error bars: factor per category:
plot_factor_by_category <- function(data, factor, category) {# category: one of age, race, income, sex, education, lsm, cluster
        # factor: one of: F1, F2, F3, F4, F5, F6
        age_levels <- c("15-24","25-44", "45-54","55+" )
        race_levels <- c("black", "coloured", "indian", "white")
        inc_levels <- c("<R2500","R2500-R6999","R7000-R11999",">=R12000")
        sex_levels <- c("male", "female")
        edu_levels <- c("<matric", "matric",">matric")
        lsm_levels <- c("LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10")
        # cluster_levels <- c("c1", "c2", "c3", "c4")
        
        if(category == "age") {
                temp_levels <- age_levels
        }
        if(category == "race") {
                temp_levels <- race_levels
        }
        if(category == "income") {
                temp_levels <- inc_levels
        }
        if(category == "sex") {
                temp_levels <- sex_levels
        }
        if(category == "education") {
                temp_levels <- edu_levels
        }
        if(category == "lsm") {
                temp_levels <- lsm_levels
        }
        if(category == "cluster") {
                temp_levels <- cluster_levels
        }
        
        temp_frame <- data %>%
                filter(category %in% temp_levels)
        
        if(factor == "F1") {
                a <- "F1"
                b <- "low_f1"
                c <- "up_f1"
                d <- "factor 1"
                e <- paste("Factor 1 and ", category)
        }
        if(factor == "F2") {
                a <- "F2"
                b <- "low_f2"
                c <- "up_f2"
                d <- "factor 2"
                e <- paste("Factor 2 and ", category)
        }
        if(factor == "F3") {
                a <- "F3"
                b <- "low_f3"
                c <- "up_f3"
                d <- "factor 3"
                e <- paste("Factor 3 and ", category)
        }
        if(factor == "F4") {
                a <- "F4"
                b <- "low_f4"
                c <- "up_f4"
                d <- "factor 4"
                e <- paste("Factor 4 and ", category)
        }
        if(factor == "F5") {
                a <- "F5"
                b <- "low_f5"
                c <- "up_f5"
                d <- "factor 5"
                e <- paste("Factor 5 and ", category)
        }
        if(factor == "F6") {
                a <- "F6"
                b <- "low_f6"
                c <- "up_f6"
                d <- "factor 6"
                e <- paste("Factor 6 and ", category)
        }
        
        
        ggplot(temp_frame, aes_string("year", a, group = "category")) +
                geom_point( color = "blue", size = 1, fill = "white", alpha = 0.5) +
                geom_line(size = 0.2) +
                facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6)) +
                geom_errorbar(aes_string(ymin = b, ymax = c),size = 0.3, width = 0.4, alpha = 0.5) +
                labs(y = d, title = e)
}

p_f1_age <- plot_factor_by_category(frame_factors, "F1", "age") # etc..any combination...
p_f2_income <- plot_factor_by_category(frame_factors, "F2", "income") # etc..any combination...
p_f3_race <- plot_factor_by_category(frame_factors, "F3", "race")
p_f5_lsm <- plot_factor_by_category(frame_factors, "F5", "lsm")
p_f6_age <- plot_factor_by_category(frame_factors, "F6", "age")
p_f1_edu <- plot_factor_by_category(frame_factors, "F1", "education")

jpeg("factors_12_category.jpeg", quality = 100)
grid.arrange(p_f1_age,
             p_f2_income,
             p_f3_race,
             p_f5_lsm,
             p_f6_age,
             p_f1_edu, nrow = 3)
dev.off()

# MODELING

## FACTOR 1 (free tv)
f1_grouped <- groupedData(F1 ~ year | category, data = frame_factors)
# plot(f1_grouped) # check
f1_list <- lmList(F1 ~ I(year - mean(year)) | category, data = f1_grouped)
# plot(intervals(f1_list))
f1_lme <- lme(f1_list)
# summary(f1_lme)

## FACTOR 2 (multi media main stream)
f2_grouped <- groupedData(F2 ~ year | category, data = frame_factors)
# plot(f2_grouped) # check
f2_list <- lmList(F2 ~ I(year - mean(year)) | category, data = f2_grouped)
# plot(intervals(f2_list))
f2_lme <- lme(f2_list)
# summary(f2_lme)

## FACTOR 3 (afrikaans)
f3_grouped <- groupedData(F3 ~ year | category, data = frame_factors)
# plot(f3_grouped) # check
f3_list <- lmList(F3 ~ I(year - mean(year)) | category, data = f3_grouped)
# plot(intervals(f3_list))
f3_lme <- lme(f3_list)
# summary(f3_lme)

## FACTOR 4 (african)
f4_grouped <- groupedData(F4 ~ year | category, data = frame_factors)
# plot(f4_grouped) # check
f4_list <- lmList(F4 ~ I(year - mean(year)) | category, data = f4_grouped)
# plot(intervals(f4_list))
f4_lme <- lme(f4_list)
# summary(f4_lme)

## FACTOR 5 (african)
f5_grouped <- groupedData(F5 ~ year | category, data = frame_factors)
# plot(f5_grouped) # check
f5_list <- lmList(F5 ~ I(year - mean(year)) | category, data = f5_grouped)
# plot(intervals(f5_list))
f5_lme <- lme(f5_list)
# summary(f5_lme)

## FACTOR 6 (popular)
f6_grouped <- groupedData(F6 ~ year | category, data = frame_factors)
# plot(f6_grouped) # check
f6_list <- lmList(F6 ~ I(year - mean(year)) | category, data = f6_grouped)
# plot(intervals(f6_list))
f6_lme <- lme(f6_list)
# summary(f6_lme)

# Own plots of Factors and Categories with Fitted Values
# add model predicted values to data frame
frame_factor_preds <- frame_factors %>%
        mutate(preds_f1 = as.vector(fitted(f1_lme))) %>%
        mutate(preds_f2 = as.vector(fitted(f2_lme))) %>%
        mutate(preds_f3 = as.vector(fitted(f3_lme))) %>%
        mutate(preds_f4 = as.vector(fitted(f4_lme))) %>%
        mutate(preds_f5 = as.vector(fitted(f5_lme))) %>%
        mutate(preds_f6 = as.vector(fitted(f6_lme)))

# function for plotting fitted models
plot_fitted_factors <- function(data = frame_factor_preds, factor) { # factor:one of: F1, F2, F3, F4, F5, F6
        
        if(factor == "F1") {
                a <- "F1"
                b <- "preds_f1"
                c <- "up_f1"
                d <- "low_f1"
                e <- "factor 1: free tv"
                f <- "Factor 1 (free tv) with Fitted Values"
        }
        if(factor == "F2") {
                a <- "F2"
                b <- "preds_f2"
                c <- "up_f2"
                d <- "low_f2"
                e <- "factor 2: multi media main stream"
                f <- "Factor 2 (multi media main stream) with Fitted Values"
        }
        if(factor == "F3") {
                a <- "F3"
                b <- "preds_f3"
                c <- "up_f3"
                d <- "low_f3"
                e <- "factor 3: afrikaans"
                f <- "Factor 3 (afrikaans) with Fitted Values"
        }
        if(factor == "F4") {
                a <- "F4"
                b <- "preds_f4"
                c <- "up_f4"
                d <- "low_f4"
                e <- "factor 4: soccer"
                f <- "Factor 4 (soccer) with Fitted Values"
        }
        if(factor == "F5") {
                a <- "F5"
                b <- "preds_f5"
                c <- "up_f5"
                d <- "low_f5"
                e <- "factor 5: african"
                f <- "Factor 5 (african) with Fitted Values"
        }
        if(factor == "F6") {
                a <- "F6"
                b <- "preds_f6"
                c <- "up_f6"
                d <- "low_f6"
                e <- "factor 6: popular"
                f <- "Factor 6 (popular) with Fitted Values"
        }
        
        #plot
        ggplot(data, aes_string("year", a, group = "category")) +
                geom_point(color = "blue", size = 1, fill = "white", alpha = 0.5) +
                geom_line(size = 0.2) +
                geom_line(aes_string("year", b, group = "category"), colour = "red", size = 0.3, linetype = 2 ) +
                facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6)) +
                geom_errorbar(aes_string(ymax = c, ymin = d), size = 0.3, width = 0.4, alpha = 0.5) +
                labs(y = e, title = f)
        
}

## F1
pf1_up <- plot_fitted_factors(data = frame_factor_preds[which(frame_factor_preds$category %in% vector_row1),],
                              factor = "F1")
pf1_down <- plot_fitted_factors(data = frame_factor_preds[which(frame_factor_preds$category %in% vector_row2),],
                                factor = "F1")
jpeg("f1_fitted_12.jpeg", quality = 100)
grid.arrange(pf1_up, pf1_down, nrow = 2)
dev.off()

## F2
pf2_up <- plot_fitted_factors(data = frame_factor_preds[which(frame_factor_preds$category %in% vector_row1),],
                              factor = "F2")
pf2_down <- plot_fitted_factors(data = frame_factor_preds[which(frame_factor_preds$category %in% vector_row2),],
                                factor = "F2")
jpeg("f2_fitted_12.jpeg", quality = 100)
grid.arrange(pf2_up, pf2_down, nrow = 2)
dev.off()

## F3
pf3_up <- plot_fitted_factors(data = frame_factor_preds[which(frame_factor_preds$category %in% vector_row1),],
                              factor = "F3")
pf3_down <- plot_fitted_factors(data = frame_factor_preds[which(frame_factor_preds$category %in% vector_row2),],
                                factor = "F3")
jpeg("f3_fitted_12.jpeg", quality = 100)
grid.arrange(pf3_up, pf3_down, nrow = 2)
dev.off()

## F4
pf4_up <- plot_fitted_factors(data = frame_factor_preds[which(frame_factor_preds$category %in% vector_row1),],
                              factor = "F4")
pf4_down <- plot_fitted_factors(data = frame_factor_preds[which(frame_factor_preds$category %in% vector_row2),],
                                factor = "F4")
jpeg("f4_fitted_12.jpeg", quality = 100)
grid.arrange(pf4_up, pf4_down, nrow = 2)
dev.off()

## F5
pf5_up <- plot_fitted_factors(data = frame_factor_preds[which(frame_factor_preds$category %in% vector_row1),],
                              factor = "F5")
pf5_down <- plot_fitted_factors(data = frame_factor_preds[which(frame_factor_preds$category %in% vector_row2),],
                                factor = "F5")
jpeg("f5_fitted_12.jpeg", quality = 100)
grid.arrange(pf5_up, pf5_down, nrow = 2)
dev.off()

## F6
pf6_up <- plot_fitted_factors(data = frame_factor_preds[which(frame_factor_preds$category %in% vector_row1),],
                              factor = "F6")
pf6_down <- plot_fitted_factors(data = frame_factor_preds[which(frame_factor_preds$category %in% vector_row2),],
                                factor = "F6")
jpeg("f6_fitted_12.jpeg", quality = 100)
grid.arrange(pf6_up, pf6_down, nrow = 2)
dev.off()
