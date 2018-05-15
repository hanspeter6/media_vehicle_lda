# read in libraries
library(nlme)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(psych)

# reading in the mins (nationals) datasets:
set02_min_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set02_min_simple.rds")
set05_min_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set05_min_simple.rds")
set08_min_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set08_min_simple.rds")
set10_min_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set10_min_simple.rds")
set12_min_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set12_min_simple.rds")
set14_min_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set14_min_simple.rds")

# # reading in the mins (nationals) based on 05 (ie including Daily Sun)... not developed yet...think about it
# set05_min2 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set05_min2.rds")
# set08_min2 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set08_min2.rds")
# set10_min2 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set10_min2.rds")
# set12_min2 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set12_min2.rds")
# set14_min2 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set14_min2.rds")

# reading in the models by period
fact_02_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/fact_02_simple.rds")
fact_05_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/fact_05_simple.rds")
fact_08_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2008/fact_08_simple.rds")
fact_10_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/fact_10_simple.rds")
fact_12_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2012/fact_12_simple.rds")
fact_14_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2014/fact_14_simple.rds")

# reading in the loadings by period
fact_02_loadings_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/fact_02_loadings_simple.rds")
fact_05_loadings_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/fact_05_loadings_simple.rds")
fact_08_loadings_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2008/fact_08_loadings_simple.rds")
fact_10_loadings_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/fact_10_loadings_simple.rds")
fact_12_loadings_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2012/fact_12_loadings_simple.rds")
fact_14_loadings_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2014/fact_14_loadings_simple.rds")

# reading in the scores by period
fact_02_scores_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/fact_02_scores_simple.rds")
fact_05_scores_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/fact_05_scores_simple.rds")
fact_08_scores_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2008/fact_08_scores_simple.rds")
fact_10_scores_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/fact_10_scores_simple.rds")
fact_12_scores_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2012/fact_12_scores_simple.rds")
fact_14_scores_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2014/fact_14_scores_simple.rds")

# scores with _02 as factor model
fact_02_scores_model_02_simple <- predict(fact_02_simple, data = set02_min_simple[,(which(names(set02_min_simple) == "all") + 1):ncol(set02_min_simple)])
fact_05_scores_model_02_simple <- predict(fact_02_simple, data = set05_min_simple[,(which(names(set05_min_simple) == "all") + 1):ncol(set05_min_simple)])
fact_08_scores_model_02_simple <- predict(fact_02_simple, data = set08_min_simple[,(which(names(set08_min_simple) == "all") + 1):ncol(set08_min_simple)])
fact_10_scores_model_02_simple <- predict(fact_02_simple, data = set10_min_simple[,(which(names(set10_min_simple) == "all") + 1):ncol(set10_min_simple)])
fact_12_scores_model_02_simple <- predict(fact_02_simple, data = set12_min_simple[,(which(names(set12_min_simple) == "all") + 1):ncol(set12_min_simple)])
fact_14_scores_model_02_simple <- predict(fact_02_simple, data = set14_min_simple[,(which(names(set14_min_simple) == "all") + 1):ncol(set14_min_simple)])

# next create dataset with scaled factors/dimensions instead of media vehicles
set02_factors_model_02_simple <- data.frame(cbind(set02_min_simple[1:19], scale(fact_02_scores_model_02_simple)))
set05_factors_model_02_simple <- data.frame(cbind(set05_min_simple[1:19], scale(fact_05_scores_model_02_simple)))
set08_factors_model_02_simple <- data.frame(cbind(set08_min_simple[1:21], scale(fact_08_scores_model_02_simple)))
set10_factors_model_02_simple <- data.frame(cbind(set10_min_simple[1:21], scale(fact_10_scores_model_02_simple)))
set12_factors_model_02_simple <- data.frame(cbind(set12_min_simple[1:21], scale(fact_12_scores_model_02_simple)))
set14_factors_model_02_simple <- data.frame(cbind(set14_min_simple[1:21], scale(fact_14_scores_model_02_simple)))

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

factor_fr_02_model_02_simple <- frame_bind_factor(set02_factors_model_02_simple, 2002)
factor_fr_05_model_02_simple<- frame_bind_factor(set05_factors_model_02_simple, 2005)
factor_fr_08_model_02_simple<- frame_bind_factor(set08_factors_model_02_simple, 2008)
factor_fr_10_model_02_simple<- frame_bind_factor(set10_factors_model_02_simple, 2010)
factor_fr_12_model_02_simple <- frame_bind_factor(set12_factors_model_02_simple, 2012)
factor_fr_14_model_02_simple <- frame_bind_factor(set14_factors_model_02_simple, 2014)


# putting them togther:
frame_factors_model_02_simple <- rbind.data.frame(factor_fr_02_model_02_simple,
                                  factor_fr_05_model_02_simple,
                                  factor_fr_08_model_02_simple,
                                  factor_fr_10_model_02_simple,
                                  factor_fr_12_model_02_simple,
                                  factor_fr_14_model_02_simple)


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

vector_row1_simple <- c("male", "female","15-24","25-44", "45-54","55+","black", "coloured", "indian", "white")
vector_row2_simple <- c("<matric", "matric",">matric", "<R2500","R2500-R6999","R7000-R11999",">=R12000", "LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10")
p_up_model_02_simple <- all_plots_factors(frame_factors_model_02_simple[which(frame_factors_model_02_simple$category %in% vector_row1_simple),])
p_down_model_02_simple <- all_plots_factors(frame_factors_model_02_simple[which(frame_factors_model_02_simple$category %in% vector_row2_simple),])

jpeg("all_plots_factors_model_02_simple.jpeg", quality = 100)
grid.arrange(p_up_model_02_simple, p_down_model_02_simple, nrow = 2)
dev.off()

# function to plot details with error bars: factor per category:
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

p_f1_age_model_02_simple <- plot_factor_by_category(frame_factors_model_02_simple, "F1", "age") # etc..any combination...
p_f2_income_model_02_simple <- plot_factor_by_category(frame_factors_model_02_simple, "F2", "income") # etc..any combination...
p_f3_race_model_02_simple <- plot_factor_by_category(frame_factors_model_02_simple, "F3", "race")
p_f5_lsm_model_02_simple <- plot_factor_by_category(frame_factors_model_02_simple, "F5", "lsm")
p_f6_age_model_02_simple <- plot_factor_by_category(frame_factors_model_02_simple, "F6", "age")
p_f1_edu_model_02_simple <- plot_factor_by_category(frame_factors_model_02_simple, "F1", "education")

jpeg("factors_category_model_02_simple.jpeg", quality = 100)
grid.arrange(p_f1_age_model_02_simple,
             p_f2_income_model_02_simple,
             p_f3_race_model_02_simple,
             p_f5_lsm_model_02_simple,
             p_f6_age_model_02_simple,
             p_f1_edu_model_02_simple, nrow = 3)
dev.off()

# MODELING

## FACTOR 1 (free tv)
f1_grouped_model_02_simple <- groupedData(F1 ~ year | category, data = frame_factors_model_02_simple)
# plot(f1_grouped_model_02_simple) # check
f1_list_model_02_simple <- lmList(F1 ~ I(year - mean(year)) | category, data = f1_grouped_model_02_simple)
# plot(intervals(f1_list_model_02_simple))
f1_lme_model_02_simple <- lme(f1_list_model_02_simple)
# summary(f1_lme_model_02_simple)

## FACTOR 2 (multi media main stream)
f2_grouped_model_02_simple <- groupedData(F2 ~ year | category, data = frame_factors_model_02_simple)
# plot(f2_grouped_model_02_simple) # check
f2_list_model_02_simple <- lmList(F2 ~ I(year - mean(year)) | category, data = f2_grouped_model_02_simple)
# plot(intervals(f2_list_model_02_simple))
f2_lme_model_02_simple <- lme(f2_list_model_02_simple)
# summary(f2_lme_model_02_simple)

## FACTOR 3 (afrikaans)
f3_grouped_model_02_simple <- groupedData(F3 ~ year | category, data = frame_factors_model_02_simple)
# plot(f3_grouped_model_02_simple) # check
f3_list_model_02_simple <- lmList(F3 ~ I(year - mean(year)) | category, data = f3_grouped_model_02_simple)
# plot(intervals(f3_list_model_02_simple))
f3_lme_model_02_simple <- lme(f3_list_model_02_simple)
# summary(f3_lme_model_02_simple)

## FACTOR 4 (african)
f4_grouped_model_02_simple <- groupedData(F4 ~ year | category, data = frame_factors_model_02_simple)
# plot(f4_grouped_model_02_simple) # check
f4_list_model_02_simple <- lmList(F4 ~ I(year - mean(year)) | category, data = f4_grouped_model_02_simple)
# plot(intervals(f4_list_model_02_simple))
f4_lme_model_02_simple <- lme(f4_list_model_02_simple)
# summary(f4_lme_model_02_simple)

## FACTOR 5 (african)
f5_grouped_model_02_simple <- groupedData(F5 ~ year | category, data = frame_factors_model_02_simple)
# plot(f5_grouped_model_02_simple) # check
f5_list_model_02_simple <- lmList(F5 ~ I(year - mean(year)) | category, data = f5_grouped_model_02_simple)
# plot(intervals(f5_list_model_02_simple))
f5_lme_model_02_simple <- lme(f5_list_model_02_simple)
# summary(f5_lme_model_02_simple)

## FACTOR 6 (popular)
f6_grouped_model_02_simple <- groupedData(F6 ~ year | category, data = frame_factors_model_02_simple)
# plot(f6_grouped_model_02_simple) # check
f6_list_model_02_simple <- lmList(F6 ~ I(year - mean(year)) | category, data = f6_grouped_model_02_simple)
# plot(intervals(f6_list_model_02_simple))
f6_lme_model_02_simple <- lme(f6_list_model_02_simple)
# summary(f6_lme_model_02_simple)

# Own plots of Factors and Categories with Fitted Values
# add model predicted values to data frame
frame_factor_preds_model_02_simple <- frame_factors_model_02_simple %>%
        mutate(preds_f1 = as.vector(fitted(f1_lme_model_02_simple))) %>%
        mutate(preds_f2 = as.vector(fitted(f2_lme_model_02_simple))) %>%
        mutate(preds_f3 = as.vector(fitted(f3_lme_model_02_simple))) %>%
        mutate(preds_f4 = as.vector(fitted(f4_lme_model_02_simple))) %>%
        mutate(preds_f5 = as.vector(fitted(f5_lme_model_02_simple))) %>%
        mutate(preds_f6 = as.vector(fitted(f6_lme_model_02_simple)))

# function for plotting fitted models
plot_fitted_factors <- function(data, factor) { # factor:one of: F1, F2, F3, F4, F5, F6
        
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
pf1_up_model_02_simple <- plot_fitted_factors(data = frame_factor_preds_model_02_simple[which(frame_factor_preds_model_02_simple$category %in% vector_row1_simple),],
                              factor = "F1")
pf1_down_model_02_simple <- plot_fitted_factors(data = frame_factor_preds_model_02_simple[which(frame_factor_preds_model_02_simple$category %in% vector_row2_simple),],
                                factor = "F1")
jpeg("f1_fitted_model_02_simple.jpeg", quality = 100)
grid.arrange(pf1_up_model_02_simple, pf1_down_model_02_simple, nrow = 2)
dev.off()

## F2
pf2_up_model_02_simple <- plot_fitted_factors(data = frame_factor_preds_model_02_simple[which(frame_factor_preds_model_02_simple$category %in% vector_row1_simple),],
                                              factor = "F2")
pf2_down_model_02_simple <- plot_fitted_factors(data = frame_factor_preds_model_02_simple[which(frame_factor_preds_model_02_simple$category %in% vector_row2_simple),],
                                                factor = "F2")
jpeg("f2_fitted_model_02_simple.jpeg", quality = 100)
grid.arrange(pf2_up_model_02_simple, pf2_down_model_02_simple, nrow = 2)
dev.off()

## F3
pf3_up_model_02_simple <- plot_fitted_factors(data = frame_factor_preds_model_02_simple[which(frame_factor_preds_model_02_simple$category %in% vector_row1_simple),],
                                              factor = "F3")
pf3_down_model_02_simple <- plot_fitted_factors(data = frame_factor_preds_model_02_simple[which(frame_factor_preds_model_02_simple$category %in% vector_row2_simple),],
                                                factor = "F3")
jpeg("f3_fitted_model_02_simple.jpeg", quality = 100)
grid.arrange(pf3_up_model_02_simple, pf3_down_model_02_simple, nrow = 2)
dev.off()

## F4
pf4_up_model_02_simple <- plot_fitted_factors(data = frame_factor_preds_model_02_simple[which(frame_factor_preds_model_02_simple$category %in% vector_row1_simple),],
                                              factor = "F4")
pf4_down_model_02_simple <- plot_fitted_factors(data = frame_factor_preds_model_02_simple[which(frame_factor_preds_model_02_simple$category %in% vector_row2_simple),],
                                                factor = "F4")
jpeg("f4_fitted_model_02_simple.jpeg", quality = 100)
grid.arrange(pf4_up_model_02_simple, pf4_down_model_02_simple, nrow = 2)
dev.off()

## F5
pf5_up_model_02_simple <- plot_fitted_factors(data = frame_factor_preds_model_02_simple[which(frame_factor_preds_model_02_simple$category %in% vector_row1_simple),],
                                              factor = "F5")
pf5_down_model_02_simple <- plot_fitted_factors(data = frame_factor_preds_model_02_simple[which(frame_factor_preds_model_02_simple$category %in% vector_row2_simple),],
                                                factor = "F5")
jpeg("f5_fitted_model_02_simple.jpeg", quality = 100)
grid.arrange(pf5_up_model_02_simple, pf5_down_model_02_simple, nrow = 2)
dev.off()

## F6
pf6_up_model_02_simple <- plot_fitted_factors(data = frame_factor_preds_model_02_simple[which(frame_factor_preds_model_02_simple$category %in% vector_row1_simple),],
                                              factor = "F6")
pf6_down_model_02_simple <- plot_fitted_factors(data = frame_factor_preds_model_02_simple[which(frame_factor_preds_model_02_simple$category %in% vector_row2_simple),],
                                                factor = "F6")
jpeg("f6_fitted_model_02_simple.jpeg", quality = 100)
grid.arrange(pf6_up_model_02_simple, pf6_down_model_02_simple, nrow = 2)
dev.off()
