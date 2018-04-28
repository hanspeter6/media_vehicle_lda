library(plm)
library(nlme)
library(tidyr)
library(dplyr)
library(ggplot2)
library(FactoMineR)

# reading in the mins (nationals) datasets:
set02_min <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set02_min.rds")
set05_min <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set05_min.rds")
set08_min <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set08_min.rds")
set10_min <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set10_min.rds")
set12_min <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set12_min.rds")
set14_min <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set14_min.rds")

# reading in the scores predicted on the _08 model:

fact_02_scores_model_08 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/fact_02_scores_model_08.rds")
fact_05_scores_model_08 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/fact_05_scores_model_08.rds")
fact_08_scores_model_08 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2008/fact_08_scores.rds")
fact_10_scores_model_08 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/fact_10_scores_model_08.rds")
fact_12_scores_model_08 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2012/fact_12_scores_model_08.rds")
fact_14_scores_model_08 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2014/fact_14_scores_model_08.rds")

# next create dataset with factors/dimensions instead of media vehicles
set02_factors <- data.frame(cbind(set02_min[1:19], fact_02_scores_model_08))
set05_factors <- data.frame(cbind(set05_min[1:19], fact_05_scores_model_08))
set08_factors <- data.frame(cbind(set08_min[1:21], fact_08_scores_model_08))
set10_factors <- data.frame(cbind(set10_min[1:21], fact_10_scores_model_08))
set12_factors <- data.frame(cbind(set12_min[1:21], fact_12_scores_model_08))
set14_factors <- data.frame(cbind(set14_min[1:21], fact_14_scores_model_08))

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
                summarise(PA1 = mean(PA1),
                          PA2 = mean(PA2),
                          PA3 = mean(PA3),
                          PA4 = mean(PA4),
                          PA5 = mean(PA5),
                          PA6 = mean(PA6))
        
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
type_frame_factors <- rbind.data.frame(factor_fr_02,
                                       factor_fr_05,
                                       factor_fr_08,
                                       factor_fr_10,
                                       factor_fr_12,
                                       factor_fr_14)


# try grouped object:
pa1_grouped <- groupedData(PA1 ~ as.numeric(as.character(year)) | category, data = type_frame_factors)
plot(pa1_grouped)

pa

pa1_test_list <- lmList(pa1_grouped)
plot(intervals(pa1_test_list))



# putting it into plm data frame type
type_frame <- pdata.frame(type_frame_factors)

# try a grouped object;
# per grouping

type_frame_age <- type_frame %>%
        filter(category %in% c("15-24","25-44", "45-54","55+" ))

type_frame_race <- type_frame %>%
        filter(category %in% c("black", "coloured", "indian", "white"))

type_frame_inc <- type_frame %>%
        filter(category %in% c("<R2500","R2500-R6999","R7000-R11999",">=R12000"))

type_frame_sex <- type_frame %>%
        filter(category %in% c("male", "female"))

type_frame_edu <- type_frame %>%
        filter(category %in% c("<matric", "matric",">matric"))

type_frame_lsm <- type_frame %>%
        filter(category %in% c("LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10"))

# type_frame_cluster <- type_frame %>%
#         filter(category %in% c("cluster1", "cluster2", "cluster3", "cluster4"))

#considering plots of scaled dimension means on demographic categories
# defining a function
all_plots <- function(data, title) {
        ggplot(data = data, title = "title") +
                geom_line(aes(year, PA1, group = category, colour = "Dim.1")) +
                geom_line(aes(year, PA2, group = category, colour = "Dim.2")) +
                geom_line(aes(year, PA3, group = category, colour = "Dim.3")) +
                geom_line(aes(year, PA4, group = category, colour = "Dim.4")) +
                geom_line(aes(year, PA5, group = category, colour = "Dim.5")) +
                geom_line(aes(year, PA6, group = category, colour = "Dim.6")) +
                scale_colour_discrete(name="Dims") +
                facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6)) +
                labs(y = "standardised mean scores", title = title)
        
}


all_plots(type_frame,"test")

jpeg("plot_dims_combined_age.jpeg")
all_plots(type_frame_age, "Age")
dev.off()

jpeg("plot_dims_combined_race.jpeg")
all_plots(type_frame_race, "Population Group")
dev.off()

jpeg("plot_dims_combined_inc.jpeg")
all_plots(type_frame_inc, "Household Income")
dev.off()

jpeg("plot_dims_combined_sex.jpeg")
all_plots(type_frame_sex, "Gender")
dev.off()

jpeg("plot_dims_combined_edu.jpeg")
all_plots(type_frame_edu, "Education Level")
dev.off()

jpeg("plot_dims_combined_lsm.jpeg")
all_plots(type_frame_lsm, "Living Standards Measure LSM")
dev.off()

# jpeg("plot_dims_combined_cluster.jpeg")
# all_plots(type_frame_cluster, "Clusters")
# dev.off()
