# Analysis of corvid vigilance around carcasses
# patbragato@gmail.com
# 14th April 2021

# Libraries ----
library(ggplot2)
library(nlme)
library(MuMIn)

# Loading and Preparing the Data ----
vigilance_corvids <- read.csv("Data/corvid_vigilance2.csv")

summary(vigilance_corvids)
str(vigilance_corvids) # Checking factor classification

names(vigilance_corvids)[1] <-  "Habitat" #changing first column name, always glitches

## Reclassifying the classes into correct classification

vigilance_corvids$Habitat <- as.factor(vigilance_corvids$Habitat)
vigilance_corvids$Habitat <- factor(vigilance_corvids$Habitat, c("Open", "Closed"))
vigilance_corvids$Site_R <- as.factor(vigilance_corvids$Site_R) # May need to rename Site_Rs for each drop
vigilance_corvids$Group_Size <- as.numeric(vigilance_corvids$Group_Size)

# Exploring the data ----
## Making a histogram
(vigilance_hist <- ggplot(vigilance_corvids, aes(x = Prop_vig)) + 
   geom_histogram(binwidth = 0.1, colour = "#000000", fill = "#8497B0") + # Changing colour and bin width
   geom_vline(aes(xintercept = mean(Prop_vig)), colour = "red", linetype = "dashed", size = 1) +
   theme_hist() +
   xlab("\nProportion Vigilant") +
   ylab("Count\n"))


options(na.action = "na.omit")

# Modelling the data ----
rvig_mod1 <- lme(Prop_vig ~ time_pred + Group_Size + Habitat, random = ~1|Site_R, data = vigilance_corvids)

rvig_mod2 <- lme(Prop_vig ~ time_pred*Habitat + Group_Size, random = ~1|Site_R, data = vigilance_corvids)

rvig_mod3 <- lme(Prop_vig ~ time_pred*Habitat, random = ~1|Site_R, data = vigilance_corvids)

rvig_mod4 <- lme(Prop_vig ~ time_pred + Habitat, random = ~1|Site_R, data = vigilance_corvids)

rvig_mod5 <- lme(Prop_vig ~ time_pred:Habitat, random = ~1|Site_R, data = vigilance_corvids)

rvig_mod6 <- lme(Prop_vig ~ time_pred:Habitat + Group_Size, random = ~1|Site_R, data = vigilance_corvids)

rvig_mod7 <- lme(Prop_vig ~ time_pred + Group_Size, random = ~1|Site_R, data = vigilance_corvids)

rvig_mod8 <- lme(Prop_vig ~ time_pred, random = ~1|Site_R, data = vigilance_corvids)

rvig_mod9 <- lme(Prop_vig ~ Group_Size + Habitat, random = ~1|Site_R, data = vigilance_corvids)

rvig_mod10 <- lme(Prop_vig ~ Group_Size, random = ~1|Site_R, data = vigilance_corvids)

rvig_mod11 <- lme(Prop_vig ~ Habitat, random = ~1|Site_R, data = vigilance_corvids)

rvig_mod12 <- lme(Prop_vig ~ time_pred + Species + Habitat + Group_Size, random = ~1|Site_R, data = vigilance_corvids)

rvig_mod13 <- lme(Prop_vig ~ time_pred*Species + Habitat + Group_Size, random = ~1|Site_R, data = vigilance_corvids)

rvig_mod14 <- lme(Prop_vig ~ time_pred*Species + Habitat, random = ~1|Site_R, data = vigilance_corvids)

rvig_mod15 <- lme(Prop_vig ~ time_pred*Species, random = ~1|Site_R, data = vigilance_corvids)

rvig_mod16 <- lme(Prop_vig ~ time_pred:Species, random = ~1|Site_R, data = vigilance_corvids)

rvig_mod17 <- lme(Prop_vig ~ Habitat*Species + time_pred, random = ~1|Site_R, data = vigilance_corvids)

rvig_mod18 <- lme(Prop_vig ~ Habitat*Species, random = ~1|Site_R, data = vigilance_corvids)

rvig_mod19 <- lme(Prop_vig ~ Habitat*Species + Group_Size, random = ~1|Site_R, data = vigilance_corvids)

rvig_mod20 <- lme(Prop_vig ~ Species + Habitat, random = ~1|Site_R, data = vigilance_corvids)

rvig_mod21 <- lme(Prop_vig ~ Species + time_pred, random = ~1|Site_R, data = vigilance_corvids)


# Model Selection and averaging ----
vig_mod_sel <- model.sel(rvig_mod1, rvig_mod2, rvig_mod3, rvig_mod4, rvig_mod7, rvig_mod8, rvig_mod9,
                         rvig_mod10, rvig_mod11, rvig_mod12, rvig_mod13, rvig_mod14,
                         rvig_mod15, rvig_mod17, rvig_mod19,
                         rvig_mod20, rvig_mod21, rvig_mod5, rvig_mod6, rvig_mod16, rvig_mod18, rank = AICc) #  Ranking models by AIC

as.data.frame(vig_mod_sel) 
write.csv(vig_mod_sel, file = "Vigilance/model_sel_table.csv", row.names = TRUE)

summary(rvig_mod11)

# Plotting the data ----
# Box plot
(vigilance_box <- ggplot(vigilance_corvids, aes(x = Habitat, y = Prop_vig, fill = Habitat)) + 
   geom_boxplot() +
   theme_hist()  +
   scale_fill_manual(values = c("#8497B0", "#8497B0")) + # Adding custom colours
   theme(legend.position = "none") + 
   stat_summary(fun = mean, shape = 7, size = 1, colour = "red",
                aes(group = Habitat)) +
   ylab("Proportion of Corvids Vigilant\n") +                             
   xlab("\nHabitat"))

# Time since predator
(vig_plot_pred <- ggplot(vigilance_corvids, aes(x = time_pred, y = Prop_vig)) +
    theme_hist() +
    geom_point(alpha = 0.5) + 
    scale_x_continuous(limits = c(0, 1500)) +
    scale_fill_manual(name = 'Habitat', values = c("#8497B0", "#FF4040")) +
    ylab("Proportion of Corvids Vigilant\n") +                             
    xlab("\nTime since predator (minutes)") +
    guides(alpha = FALSE)
)

# Group size
(vig_predict_plot_group <- ggplot(vigilance_corvids, aes(x = Group_Size, y = Prop_vig)) +
    theme_hist() +
    theme(legend.position = "none") +
    scale_x_continuous(limits = c(0, 15)) +
    scale_fill_manual(name = 'Habitat', values = c("#8497B0", "#FF4040")) +
    geom_point(alpha = 0.5) + 
    ylab("Proportion of Corvids Vigilant\n") +                             
    xlab("\nGroup Size")
)

