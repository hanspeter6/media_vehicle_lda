library(plm)
library(nlme)
library(tidyr)
library(dplyr)
library(ggplot2)
library(FactoMineR)

# reading in the national datasets:
set02_nat <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set02_nat.rds")
set05_nat <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set05_nat.rds")
set08_nat <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set08_nat.rds")
set10_nat <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set10_nat.rds")
set12_nat <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set12_nat.rds")
set14_nat <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set14_nat.rds")

# reading in the factominer objects
pca_02_nat <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/pca_02_nat.rds")
pca_05_nat <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/pca_05_nat.rds")
pca_08_nat <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2008/pca_08_nat.rds")
pca_10_nat <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/pca_10_nat.rds")
pca_12_nat <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2012/pca_12_nat.rds")
pca_14_nat <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2014/pca_14_nat.rds")

# extract scores
scores_02 <- scale(pca_02_nat$ind$coord)
scores_05 <- scale(pca_05_nat$ind$coord)
scores_08 <- scale(pca_08_nat$ind$coord)
scores_10 <- scale(pca_10_nat$ind$coord)
scores_12 <- scale(pca_12_nat$ind$coord)
scores_14 <- scale(pca_14_nat$ind$coord)

# next create dataset with factors/dimensions instead of media vehicles
set02_factors <- data.frame(cbind(set02_nat[1:19], scores_02))
set05_factors <- data.frame(cbind(set05_nat[1:19], scores_05))
set08_factors <- data.frame(cbind(set08_nat[1:21], scores_08))
set10_factors <- data.frame(cbind(set10_nat[1:21], scores_10))
set12_factors <- data.frame(cbind(set12_nat[1:21], scores_12))
set14_factors <- data.frame(cbind(set14_nat[1:21], scores_14))

# # since lsm 1-2 in set14-ct has no observations for level 1 and that causes kak, will force singe observation of 2 to 1 and see if that works
# set14_factors_ct[which(set14_factors_ct$lsm == 2)[1],] <-1

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
                summarise(Dim.1 = mean(Dim.1),
                          Dim.2 = mean(Dim.2),
                          Dim.3 = mean(Dim.3),
                          Dim.4 = mean(Dim.4),
                          Dim.5 = mean(Dim.5),
                          Dim.6 = mean(Dim.6))

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
                geom_line(aes(year, Dim.1, group = category, colour = "Dim.1")) +
                geom_line(aes(year, Dim.2, group = category, colour = "Dim.2")) +
                geom_line(aes(year, Dim.3, group = category, colour = "Dim.3")) +
                geom_line(aes(year, Dim.4, group = category, colour = "Dim.4")) +
                geom_line(aes(year, Dim.5, group = category, colour = "Dim.5")) +
                geom_line(aes(year, Dim.6, group = category, colour = "Dim.6")) +
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

