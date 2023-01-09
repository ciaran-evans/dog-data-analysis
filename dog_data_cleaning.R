library(tidyverse)
library(patchwork)

dog_data <- read_csv("2020-05-05_canine_contact_data_raw.csv", 
                     na=c("N/A", "", "x", "n/a", "Doesn't apply", "X", "9999"))

# there are a few values in PN, L2, and SC which are outside the allowed range
# replace these values with NAs 
# Also reverse the scores for selected items (Loneliness, SCS, and Integration)

dog_data <- dog_data %>%
  mutate(across(starts_with("PN"), ~ replace(.x, .x > 5, NA)),
         across(starts_with("L2"), ~ replace(.x, .x > 4, NA)),
         across(starts_with("SC"), ~ replace(.x, .x > 6, NA))) %>%
  mutate_at(paste("L1_", c(1, 5, 6, 9, 10, 15, 16, 19, 20), sep=""),
            function(x){5-x}) %>%
  mutate_at(paste("L2_", c(1, 5, 6, 9, 10, 15, 16, 19, 20), sep=""),
            function(x){5-x}) %>%
  mutate_at(paste("SC1_", c(3, 6, 7, 9, 11, 13, 15, 17, 18, 20), sep=""),
            function(x){7-x}) %>%
  mutate_at(paste("SC2_", c(3, 6, 7, 9, 11, 13, 15, 17, 18, 20), sep=""),
            function(x){7-x}) %>%
  mutate(integration1 = 6 - HO1_2,
         integration2 = 6 - HO2_2)


# now average across mulitple items to get scores

dog_data <- dog_data %>%
  mutate(posaff1 = rowMeans(select(dog_data,
                                   num_range("PN1_", range = c(3,5,7,8,10))),
                            na.rm = TRUE),
         posaff2 = rowMeans(select(dog_data,
                                   num_range("PN2_", range = c(3,5,7,8,10))),
                            na.rm = TRUE), 
         happy1 = rowMeans(select(dog_data, num_range("HA1_", range = c(1,2,3))), na.rm = TRUE),
         happy2 = rowMeans(select(dog_data, num_range("HA2_", range = c(1,2,3))), na.rm = TRUE),
         social1 = rowMeans(select(dog_data, starts_with("SC1_")), na.rm = TRUE),
         social2 = rowMeans(select(dog_data, starts_with("SC2_")), na.rm = TRUE),
         flourish1 = rowMeans(select(dog_data, starts_with("F1_")), na.rm = TRUE),
         flourish2 = rowMeans(select(dog_data, starts_with("F2_")), na.rm = TRUE),
         stress1 = S1_1,
         stress2 = S2_1,
         homesick1 = HO1_1,
         homesick2 = HO2_1,
         lonely1 = rowMeans(select(dog_data, starts_with("L1_")), na.rm = T),
         lonely2 = rowMeans(select(dog_data, starts_with("L2_")), na.rm = T),
         negaff1 = rowMeans(select(dog_data,
                                   num_range("PN1_", range = c(1,2,4,6,9))),
                            na.rm = TRUE),
         negaff2 = rowMeans(select(dog_data,
                                   num_range("PN2_", range = c(1,2,4,6,9))),
                            na.rm = TRUE)) %>%
  dplyr::select(RID, GroupAssignment, Gender, Age_Yrs, Age_Mo,
                Ethnicity, Ethnicity_Specify, Hometown_city, Hometown_country,
                Local, Year_of_Study, Live_with, Live_Pets, Describe_pets, Consumer_BARK,
                Helpful, Connection_feel, Eye_contact, Physical_proximity, Physically_touched,
                posaff1, posaff2, happy1, happy2, social1, social2, integration1, integration2,
                flourish1, flourish2, stress1, stress2, homesick1, homesick2, lonely1, 
                lonely2, negaff1, negaff2)


# finally, let's make the data long not wide, by making a new column 
# for stage (pre/post)

dog_data <- dog_data %>% 
  pivot_longer( 
    cols = -c(RID, GroupAssignment, Gender, Age_Yrs, Age_Mo,
              Ethnicity, Ethnicity_Specify, Hometown_city, Hometown_country,
              Local, Year_of_Study, Live_with, Live_Pets, Describe_pets, 
              Consumer_BARK, Helpful, Connection_feel, Eye_contact, Physical_proximity,
              Physically_touched),
    names_to = c(".value", "Stage"),
    names_pattern = "([A-Za-z]+)(\\d+)"
    ) %>%
  mutate(Stage = ifelse(Stage == 1, "pre", "post"))  



# now check that we get the same results as Table 2

dog_data %>%
  mutate(GroupAssignment = factor(GroupAssignment, 
                                  levels = c("Control", "Indirect", "Direct")),
         Stage = factor(Stage, levels = c("pre", "post"))) %>%
  group_by(GroupAssignment, Stage) %>%
  summarize(FS = round(mean(flourish, na.rm=T), digits=2),
            PANAS_PA = round(mean(posaff, na.rm=T), digits=2),
            SCS = round(mean(social, na.rm=T), digits=2),
            SHS = round(mean(happy, na.rm=T), digits=2),
            Engagement = round(mean(integration, na.rm=T), digits=2),
            Stress = round(mean(stress, na.rm=T), digits=2),
            Homesick = round(mean(homesick, na.rm=T), digits=2),
            Lonely = round(mean(lonely, na.rm=T), digits=2),
            PANAS_NA = round(mean(negaff, na.rm=T), digits=2)) %>%
  arrange(GroupAssignment, Stage) %>%
  t() %>%
  View()



# first, make some plots to visualize how students feel at the beginning of the
# study. Are they generally happy, or stressed, etc.?

p1 <- dog_data %>%
  filter(Stage == "pre") %>%
  ggplot(aes(x = posaff)) +
  geom_histogram(bins=10) +
  geom_hline(yintercept=0)+
  theme_bw() +
  labs(x = "Positive affect scale, pre-test", y="Frequency")+
  ylim(0,130)

p2 <- dog_data %>%
  filter(Stage == "pre") %>%
  ggplot(aes(x = flourish)) +
  geom_histogram(bins=10) +
  geom_hline(yintercept=0)+
  theme_bw() +
  labs(x = "Flourishing scale, pre-test", y="Frequency")+
  ylim(0,130)

p3 <- dog_data %>%
  filter(Stage == "pre") %>%
  ggplot(aes(x = social)) +
  geom_histogram(bins=10) +
  geom_hline(yintercept=0)+
  theme_bw() +
  labs(x = "Social connect. scale, pre-test", y="Frequency")+
  ylim(0,130)

p4 <- dog_data %>%
  filter(Stage == "pre") %>%
  ggplot(aes(x = negaff)) +
  geom_histogram(bins=10) +
  geom_hline(yintercept=0)+
  theme_bw() +
  labs(x = "Negative affect scale, pre-test", y="Frequency")+
  ylim(0,130)

p5 <- dog_data %>%
  filter(Stage == "pre") %>%
  ggplot(aes(x = stress)) +
  geom_bar() +
  geom_hline(yintercept=0)+
  theme_bw() +
  labs(x = "Stress scale, pre-test", y="Frequency")+
  ylim(0,130)

p6 <- dog_data %>%
  filter(Stage == "pre") %>%
  ggplot(aes(x = lonely)) +
  geom_histogram(bins=10) +
  geom_hline(yintercept=0)+
  theme_bw() +
  labs(x = "Loneliness scale, pre-test", y="Frequency")+
  ylim(0,130)

p7 <- dog_data %>%
  filter(Stage == "pre") %>%
  ggplot(aes(x = happy)) +
  geom_histogram(bins=10) +
  geom_hline(yintercept=0)+
  theme_bw() +
  labs(x = "Happiness scale, pre-test", y="Frequency")+
  ylim(0,130)

p8 <- dog_data %>%
  filter(Stage == "pre") %>%
  ggplot(aes(x = integration)) +
  geom_bar() +
  geom_hline(yintercept=0)+
  theme_bw() +
  labs(x = "Integration scale, pre-test", y="Frequency")+
  ylim(0,130)

p9 <- dog_data %>%
  filter(Stage == "pre") %>%
  ggplot(aes(x = homesick)) +
  geom_bar() +
  geom_hline(yintercept=0)+
  theme_bw() +
  labs(x = "Homesick scale, pre-test", y="Frequency")+
  ylim(0,130)

     
            

pdf(file = "pre_test_scales.pdf", width = 7.5, height = 6)
(p1+p2+p3)/(p4+p5+p6)/(p7+p8+p9)
dev.off()



# now look at differences pre- and post-test

p1 <- dog_data %>%
  filter(Stage == "post") %>%
  mutate(diff = posaff - (dog_data %>% 
                            filter(Stage == "pre") %>%
                            pull(posaff))) %>%
  mutate(Group = factor(GroupAssignment, 
                        levels = c("Control", "Indirect", "Direct"))) %>%
  ggplot(aes(x = Group,
             y = diff)) +
  geom_boxplot() +
  theme_bw() +
  labs(y = "Positive affect change")+
  ylim(-4,4)

p2 <- dog_data %>%
  filter(Stage == "post") %>%
  mutate(diff = flourish - (dog_data %>% 
                            filter(Stage == "pre") %>%
                            pull(flourish))) %>%
  mutate(Group = factor(GroupAssignment, 
                        levels = c("Control", "Indirect", "Direct"))) %>%
  ggplot(aes(x = Group,
             y = diff)) +
  geom_boxplot() +
  theme_bw() +
  labs(y = "Flourishing change")+
  ylim(-4,4)

p3 <- dog_data %>%
  filter(Stage == "post") %>%
  mutate(diff = social - (dog_data %>% 
                            filter(Stage == "pre") %>%
                            pull(social))) %>%
  mutate(Group = factor(GroupAssignment, 
                        levels = c("Control", "Indirect", "Direct"))) %>%
  ggplot(aes(x = Group,
             y = diff)) +
  geom_boxplot() +
  theme_bw() +
  labs(y = "Social connect. change")+
  ylim(-4,4)


p4 <- dog_data %>%
  filter(Stage == "post") %>%
  mutate(diff = negaff - (dog_data %>% 
                            filter(Stage == "pre") %>%
                            pull(negaff))) %>%
  mutate(Group = factor(GroupAssignment, 
                        levels = c("Control", "Indirect", "Direct"))) %>%
  ggplot(aes(x = Group,
             y = diff)) +
  geom_boxplot() +
  theme_bw() +
  labs(y = "Negative affect change")+
  ylim(-4,4)

p5 <- dog_data %>%
  filter(Stage == "post") %>%
  mutate(diff = stress - (dog_data %>% 
                            filter(Stage == "pre") %>%
                            pull(stress))) %>%
  mutate(Group = factor(GroupAssignment, 
                        levels = c("Control", "Indirect", "Direct"))) %>%
  ggplot(aes(x = Group,
             y = diff)) +
  geom_boxplot() +
  theme_bw() +
  labs(y = "Stress change")+
  ylim(-4,4)

p6 <- dog_data %>%
  filter(Stage == "post") %>%
  mutate(diff = lonely - (dog_data %>% 
                            filter(Stage == "pre") %>%
                            pull(lonely))) %>%
  mutate(Group = factor(GroupAssignment, 
                        levels = c("Control", "Indirect", "Direct"))) %>%
  ggplot(aes(x = Group,
             y = diff)) +
  geom_boxplot() +
  theme_bw() +
  labs(y = "Loneliness change")+
  ylim(-4,4)

p7 <- dog_data %>%
  filter(Stage == "post") %>%
  mutate(diff = happy - (dog_data %>% 
                           filter(Stage == "pre") %>%
                           pull(happy))) %>%
  mutate(Group = factor(GroupAssignment, 
                        levels = c("Control", "Indirect", "Direct"))) %>%
  ggplot(aes(x = Group,
             y = diff)) +
  geom_boxplot() +
  theme_bw() +
  labs(y = "Happiness change")+
  ylim(-4,4)

p8 <- dog_data %>%
  filter(Stage == "post") %>%
  mutate(diff = integration - (dog_data %>% 
                           filter(Stage == "pre") %>%
                           pull(integration))) %>%
  mutate(Group = factor(GroupAssignment, 
                        levels = c("Control", "Indirect", "Direct"))) %>%
  ggplot(aes(x = Group,
             y = diff)) +
  geom_boxplot() +
  theme_bw() +
  labs(y = "Integration change")+
  ylim(-4,4)


p9 <- dog_data %>%
  filter(Stage == "post") %>%
  mutate(diff = homesick - (dog_data %>% 
                                 filter(Stage == "pre") %>%
                                 pull(homesick))) %>%
  mutate(Group = factor(GroupAssignment, 
                        levels = c("Control", "Indirect", "Direct"))) %>%
  ggplot(aes(x = Group,
             y = diff)) +
  geom_boxplot() +
  theme_bw() +
  labs(y = "Homesick change")+
  ylim(-4,4)

pdf(file = "pre_post_diff.pdf", width = 7.5, height = 6)
(p1+p2+p3)/(p4+p5+p6)/(p7+p8+p9)
dev.off()

p1 <- dog_data %>%
  mutate(Group = factor(GroupAssignment, 
                        levels = c("Control", "Indirect", "Direct"))) %>%
  ggplot(aes(x = posaff, y = negaff, color = Group, shape = Group)) +
  geom_point() +
  geom_jitter(width = 0.1, height = 0.1) +
  theme_bw() +
  labs(x = "Positive affect score", y = "Negative affect score")


p2 <- dog_data %>%
  mutate(Group = factor(GroupAssignment, 
                        levels = c("Control", "Indirect", "Direct"))) %>%
  ggplot(aes(x = social, y = flourish, color = Group, shape = Group)) +
  geom_point() +
  geom_jitter(width = 0.1, height = 0.1) +
  theme_bw() +
  labs(x = "Social connectedness", y = "Flourishing")


p3 <- dog_data %>%
  mutate(Group = factor(GroupAssignment, 
                        levels = c("Control", "Indirect", "Direct"))) %>%
  ggplot(aes(x = social, y = lonely, color = Group, shape = Group)) +
  geom_point() +
  geom_jitter(width = 0.1, height = 0.1) +
  theme_bw() +
  labs(x = "Social connectedness", y = "Loneliness")

library(patchwork)
pdf(file = "pairwise_relationships.pdf", width = 7.5, height = 4)
comb.plot<-p1+p2+p3 & theme(legend.position = "bottom")
comb.plot+plot_layout(guides = "collect")
dev.off()

#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
# Table 3
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
library(effectsize)
dog_data %>%
  mutate(GroupAssignment = factor(GroupAssignment, 
                                  levels = c("Control", "Indirect", "Direct")))%>%
  pivot_longer(cols = starts_with(c("flourish", "posaff", "social", "happy", "integration", "stress",
                                    "homesick", "lonely", "negaff")),
               names_to = "variable",
               values_to = "value")%>%
  mutate(variable = factor(variable,
                           levels = c("flourish", "posaff", "social", "happy", "integration", "stress",
                                      "homesick", "lonely", "negaff"),
                           labels = c("FS", "PANAS_PA", "SCS", "SHS", "Engagement", "Stress",
                                      "Homesick", "Lonely", "PANAS_NA"))) %>%
  mutate(alt = case_when(variable %in% c("FS", "PANAS_PA", "SCS", "SHS", "Engagement") ~ "greater",
                         variable %in% c("Stress","Homesick", "Lonely", "PANAS_NA") ~ "less"))%>%
  pivot_wider(values_from = value, names_from = Stage) %>%
  group_by(variable, GroupAssignment)%>%
  filter(complete.cases(pre,post)) %>%
  summarize(
    t = round(t.test(post, pre, paired=T, alternative=unique(alt))$statistic, 2),
    df = round(t.test(post, pre, paired=T, alternative=unique(alt))$parameter,2),
    p = round(t.test(post, pre, paired=T, alternative=unique(alt))$p.value,3),
    d = round(effectsize::cohens_d(post, pre, paired=T, alternative=unique(alt))$Cohens_d,2),
    LL = round(effectsize::cohens_d(post, pre, paired=T, conf.level=0.95)$CI_low,2),
    UL = round(effectsize::cohens_d(post, pre, paired=T, conf.level=0.95)$CI_high,2)
  ) %>%
  arrange(GroupAssignment) %>%
  view()

#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
# Table 4 (Hypothesis 2 -- Binfet et al contrast with a different approach -- Scheffe Adjusted )
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
library(emmeans)
dog_data %>%
  mutate(GroupAssignment = factor(GroupAssignment, 
                                  levels = c("Control", "Indirect", "Direct")))%>%
  pivot_longer(cols = starts_with(c("flourish", "posaff", "social", "happy", "integration", "stress",
                                    "homesick", "lonely", "negaff")),
               names_to = "variable",
               values_to = "value")%>%
  mutate(variable = factor(variable,
                           levels = c("flourish", "posaff", "social", "happy", "integration", "stress",
                                      "homesick", "lonely", "negaff"),
                           labels = c("FS", "PANAS_PA", "SCS", "SHS", "Engagement", "Stress",
                                      "Homesick", "Lonely", "PANAS_NA"))) %>%
  mutate(alt = case_when(variable %in% c("FS", "PANAS_PA", "SCS", "SHS", "Engagement") ~ "greater",
                         variable %in% c("Stress","Homesick", "Lonely", "PANAS_NA") ~ "less"))%>%
  pivot_wider(values_from = value, names_from = Stage) %>%
  filter(complete.cases(variable,GroupAssignment)) %>%
  group_by(variable)%>%
  summarize(
    beta = data.frame(contrast(emmeans(aov(post~pre+GroupAssignment), ~GroupAssignment),
                               list("(Direct/Indirect)-Control" = c(-1, 0.5, 0.5),
                                    "Direct - Indirect" = c(0, -1, 1)), adjust="bonferroni")),
    effs = data.frame(eff_size(emmeans(aov(post~pre+GroupAssignment), ~GroupAssignment),
                               method=list("(Direct/Indirect)-Control" = c(-1, 0.5, 0.5),
                                           "Direct - Indirect" = c(0, -1, 1)),
                               sigma=sigma(aov(post~pre+GroupAssignment)),
                               edf=aov(post~pre+GroupAssignment)$df.residual))
  ) %>%
  view()

#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
# Table 4 (Hypothesis 2 -- 'fixed' contrasts a statistician would run -- Scheffe Adjusted )
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
library(emmeans)
tab1<-dog_data %>%
  mutate(GroupAssignment = factor(GroupAssignment, 
                                  levels = c("Control", "Indirect", "Direct")))%>%
  pivot_longer(cols = starts_with(c("flourish", "posaff", "social", "happy", "integration", "stress",
                                    "homesick", "lonely", "negaff")),
               names_to = "variable",
               values_to = "value")%>%
  mutate(variable = factor(variable,
                           levels = c("flourish", "posaff", "social", "happy", "integration", "stress",
                                      "homesick", "lonely", "negaff"),
                           labels = c("FS", "PANAS_PA", "SCS", "SHS", "Engagement", "Stress",
                                      "Homesick", "Lonely", "PANAS_NA"))) %>%
  mutate(alt = case_when(variable %in% c("FS", "PANAS_PA", "SCS", "SHS", "Engagement") ~ "greater",
                         variable %in% c("Stress","Homesick", "Lonely", "PANAS_NA") ~ "less"))%>%
  pivot_wider(values_from = value, names_from = Stage) %>%
  filter(complete.cases(variable,GroupAssignment)) %>%
  group_by(variable)%>%
  summarize(
    beta = data.frame(contrast(emmeans(aov(post~pre+GroupAssignment), ~GroupAssignment),
                               list("Direct-Control" = c(-1, 0, 1),
                                    "Indirect-Control" = c(-1, 1, 0)), adjust="bonferroni")),
    effs = data.frame(eff_size(emmeans(aov(post~pre+GroupAssignment), ~GroupAssignment),
                               method=list("Direct-Control" = c(-1, 0, 1),
                                           "Indirect-Control" = c(-1, 1, 0)),
                               sigma=sigma(aov(post~pre+GroupAssignment)),
                               edf=aov(post~pre+GroupAssignment)$df.residual))
  ) %>%
  view()

#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
# Table 4 (Hypothesis 4 -- Binfet et al contrast with a different approach -- Not Adjusted b/c it's one contrast per response)
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
tab2<-dog_data %>%
  mutate(GroupAssignment = factor(GroupAssignment, 
                                  levels = c("Control", "Indirect", "Direct")))%>%
  pivot_longer(cols = starts_with(c("flourish", "posaff", "social", "happy", "integration", "stress",
                                    "homesick", "lonely", "negaff")),
               names_to = "variable",
               values_to = "value")%>%
  mutate(variable = factor(variable,
                           levels = c("flourish", "posaff", "social", "happy", "integration", "stress",
                                      "homesick", "lonely", "negaff"),
                           labels = c("FS", "PANAS_PA", "SCS", "SHS", "Engagement", "Stress",
                                      "Homesick", "Lonely", "PANAS_NA"))) %>%
  mutate(alt = case_when(variable %in% c("FS", "PANAS_PA", "SCS", "SHS", "Engagement") ~ "greater",
                         variable %in% c("Stress","Homesick", "Lonely", "PANAS_NA") ~ "less"))%>%
  pivot_wider(values_from = value, names_from = Stage) %>%
  filter(complete.cases(variable,GroupAssignment)) %>%
  group_by(variable)%>%
  summarize(
    beta = data.frame(contrast(emmeans(aov(post~pre+GroupAssignment), ~GroupAssignment),
                               list("Direct - Indirect" = c(0, -1, 1)), adjust="bonferroni")),
    effs = data.frame(eff_size(emmeans(aov(post~pre+GroupAssignment), ~GroupAssignment),
                               method=list("Direct - Indirect" = c(0, -1, 1)),
                               sigma=sigma(aov(post~pre+GroupAssignment)),
                               edf=aov(post~pre+GroupAssignment)$df.residual))
  ) %>%
  view()

unnest_dataframes <- function(x) {
  
  y <- do.call(data.frame, x)
  
  if("data.frame" %in% sapply(y, class)) unnest_dataframes(y)
  
  y
  
}

tab12<-rbind(data.frame(tab1),data.frame(tab2))
new_data <- unnest_dataframes(tab12)
new_data<- new_data %>% 
  select(variable,
         beta.contrast,beta.estimate, beta.SE, beta.t.ratio, beta.p.value,
         effs.effect.size, effs.lower.CL, effs.upper.CL) %>%
  mutate(beta.contrast=factor(beta.contrast, 
                              levels=c("Indirect-Control","Direct-Control", "Direct - Indirect"))) %>%
  arrange(beta.contrast) %>%
  select(-beta.contrast)

colnames(new_data)<-c("Measure", "$beta$", "$SE$", "$t$", "$p$",
                      "$d$", "lower", "upper")
library(xtable)
print(xtable(new_data), include.rownames=FALSE)
view(new_data)
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
# Table 4 (Hypothesis 2-3 simultaneously -- Tukey adjusted all pairwise comparisons)
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
library(emmeans)
tab3<-dog_data %>%
  mutate(GroupAssignment = factor(GroupAssignment, 
                                  levels = c("Direct","Indirect","Control")))%>%
  pivot_longer(cols = starts_with(c("flourish", "posaff", "social", "happy", "integration", "stress",
                                    "homesick", "lonely", "negaff")),
               names_to = "variable",
               values_to = "value")%>%
  mutate(variable = factor(variable,
                           levels = c("flourish", "posaff", "social", "happy", "integration", "stress",
                                      "homesick", "lonely", "negaff"),
                           labels = c("FS", "PANAS_PA", "SCS", "SHS", "Engagement", "Stress",
                                      "Homesick", "Lonely", "PANAS_NA"))) %>%
  mutate(alt = case_when(variable %in% c("FS", "PANAS_PA", "SCS", "SHS", "Engagement") ~ "greater",
                         variable %in% c("Stress","Homesick", "Lonely", "PANAS_NA") ~ "less"))%>%
  pivot_wider(values_from = value, names_from = Stage) %>%
  filter(complete.cases(variable,GroupAssignment)) %>%
  group_by(variable)%>%
  summarize(
    beta = data.frame(pairs(emmeans(aov(post~pre+GroupAssignment), ~GroupAssignment))),
    effs = data.frame(eff_size(emmeans(aov(post~pre+GroupAssignment), ~GroupAssignment),
                              sigma=sigma(aov(post~pre+GroupAssignment)),
                              edf=aov(post~pre+GroupAssignment)$df.residual))
  ) %>%
  view()


new_data <- unnest_dataframes(tab3)
new_data<- new_data %>% 
  select(variable,
         beta.contrast,beta.estimate, beta.SE, beta.t.ratio, beta.p.value,
         effs.effect.size, effs.lower.CL, effs.upper.CL) %>%
  mutate(beta.contrast=factor(beta.contrast, 
                              levels=c("Indirect - Control","Direct - Control", "Direct - Indirect"))) %>%
  arrange(beta.contrast) %>%
  select(-beta.contrast)

colnames(new_data)<-c("Measure", "$beta$", "$SE$", "$t$", "$p$",
                      "$d$", "lower", "upper")
library(xtable)
print(xtable(new_data), include.rownames=FALSE)

#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
# Test on what they do?
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
test.data <- dog_data %>%
  mutate(GroupAssignment = factor(GroupAssignment, 
                                  levels = c("Control", "Indirect", "Direct")))%>%
  pivot_longer(cols = starts_with(c("flourish", "posaff", "social", "happy", "integration", "stress",
                                    "homesick", "lonely", "negaff")),
               names_to = "variable",
               values_to = "value")%>%
  mutate(variable = factor(variable,
                           levels = c("flourish", "posaff", "social", "happy", "integration", "stress",
                                      "homesick", "lonely", "negaff"),
                           labels = c("FS", "PANAS_PA", "SCS", "SHS", "Engagement", "Stress",
                                      "Homesick", "Lonely", "PANAS_NA"))) %>%
  mutate(alt = case_when(variable %in% c("FS", "PANAS_PA", "SCS", "SHS", "Engagement") ~ "greater",
                         variable %in% c("Stress","Homesick", "Lonely", "PANAS_NA") ~ "less"))%>%
  pivot_wider(values_from = value, names_from = Stage) %>%
  filter(complete.cases(variable,GroupAssignment))


aov.obj <- aov(post~pre+GroupAssignment, data=test.data)
data.frame(lmtest::coeftest(aov.obj, vcov. = sandwich::vcovHC(aov.obj))[,])

