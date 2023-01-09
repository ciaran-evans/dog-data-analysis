library(tidyverse)
library(patchwork)

dog_data <- read_csv("dog_data_cleaned.csv")


# First, check that we get the same results as Table 2
# in the original paper

dog_data %>%
  mutate(GroupAssignment = factor(GroupAssignment, 
                                  levels = c("Control", "Indirect", "Direct")),
         Stage = factor(Stage, levels = c("pre", "post"))) %>%
  group_by(GroupAssignment, Stage) %>%
  summarize(FS = round(mean(FS, na.rm=T), digits=2),
            PANAS_PA = round(mean(PANAS_PA, na.rm=T), digits=2),
            SCS = round(mean(SCS, na.rm=T), digits=2),
            SHS = round(mean(SHS, na.rm=T), digits=2),
            Engagement = round(mean(Engagement, na.rm=T), digits=2),
            Stress = round(mean(Stress, na.rm=T), digits=2),
            Homesick = round(mean(Homesick, na.rm=T), digits=2),
            Lonely = round(mean(Lonely, na.rm=T), digits=2),
            PANAS_NA = round(mean(PANAS_NA, na.rm=T), digits=2)) %>%
  arrange(GroupAssignment, Stage) %>%
  t() %>%
  View()



# Next, make some plots to visualize how students feel at the beginning of the
# study. Are they generally happy, or stressed, etc.?

p1 <- dog_data %>%
  filter(Stage == "pre") %>%
  ggplot(aes(x = PANAS_PA)) +
  geom_histogram(bins=10) +
  geom_hline(yintercept=0)+
  theme_bw() +
  labs(x = "Positive affect scale, pre-test", y="Frequency")+
  ylim(0,130)

p2 <- dog_data %>%
  filter(Stage == "pre") %>%
  ggplot(aes(x = FS)) +
  geom_histogram(bins=10) +
  geom_hline(yintercept=0)+
  theme_bw() +
  labs(x = "Flourishing scale, pre-test", y="Frequency")+
  ylim(0,130)

p3 <- dog_data %>%
  filter(Stage == "pre") %>%
  ggplot(aes(x = SCS)) +
  geom_histogram(bins=10) +
  geom_hline(yintercept=0)+
  theme_bw() +
  labs(x = "Social connect. scale, pre-test", y="Frequency")+
  ylim(0,130)

p4 <- dog_data %>%
  filter(Stage == "pre") %>%
  ggplot(aes(x = PANAS_NA)) +
  geom_histogram(bins=10) +
  geom_hline(yintercept=0)+
  theme_bw() +
  labs(x = "Negative affect scale, pre-test", y="Frequency")+
  ylim(0,130)

p5 <- dog_data %>%
  filter(Stage == "pre") %>%
  ggplot(aes(x = Stress)) +
  geom_bar() +
  geom_hline(yintercept=0)+
  theme_bw() +
  labs(x = "Stress scale, pre-test", y="Frequency")+
  ylim(0,130)

p6 <- dog_data %>%
  filter(Stage == "pre") %>%
  ggplot(aes(x = Lonely)) +
  geom_histogram(bins=10) +
  geom_hline(yintercept=0)+
  theme_bw() +
  labs(x = "Loneliness scale, pre-test", y="Frequency")+
  ylim(0,130)

p7 <- dog_data %>%
  filter(Stage == "pre") %>%
  ggplot(aes(x = SHS)) +
  geom_histogram(bins=10) +
  geom_hline(yintercept=0)+
  theme_bw() +
  labs(x = "Happiness scale, pre-test", y="Frequency")+
  ylim(0,130)

p8 <- dog_data %>%
  filter(Stage == "pre") %>%
  ggplot(aes(x = Engagement)) +
  geom_bar() +
  geom_hline(yintercept=0)+
  theme_bw() +
  labs(x = "Integration scale, pre-test", y="Frequency")+
  ylim(0,130)

p9 <- dog_data %>%
  filter(Stage == "pre") %>%
  ggplot(aes(x = Homesick)) +
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
  mutate(diff = PANAS_PA - (dog_data %>% 
                            filter(Stage == "pre") %>%
                            pull(PANAS_PA))) %>%
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
  mutate(diff = FS - (dog_data %>% 
                            filter(Stage == "pre") %>%
                            pull(FS))) %>%
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
  mutate(diff = SCS - (dog_data %>% 
                            filter(Stage == "pre") %>%
                            pull(SCS))) %>%
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
  mutate(diff = PANAS_NA - (dog_data %>% 
                            filter(Stage == "pre") %>%
                            pull(PANAS_NA))) %>%
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
  mutate(diff = Stress - (dog_data %>% 
                            filter(Stage == "pre") %>%
                            pull(Stress))) %>%
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
  mutate(diff = Lonely - (dog_data %>% 
                            filter(Stage == "pre") %>%
                            pull(Lonely))) %>%
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
  mutate(diff = SHS - (dog_data %>% 
                           filter(Stage == "pre") %>%
                           pull(SHS))) %>%
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
  mutate(diff = Engagement - (dog_data %>% 
                           filter(Stage == "pre") %>%
                           pull(Engagement))) %>%
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
  mutate(diff = Homesick - (dog_data %>% 
                                 filter(Stage == "pre") %>%
                                 pull(Homesick))) %>%
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
  ggplot(aes(x = PANAS_PA, y = PANAS_NA, color = Group, shape = Group)) +
  geom_point() +
  geom_jitter(width = 0.1, height = 0.1) +
  theme_bw() +
  labs(x = "Positive affect score", y = "Negative affect score")


p2 <- dog_data %>%
  mutate(Group = factor(GroupAssignment, 
                        levels = c("Control", "Indirect", "Direct"))) %>%
  ggplot(aes(x = SCS, y = FS, color = Group, shape = Group)) +
  geom_point() +
  geom_jitter(width = 0.1, height = 0.1) +
  theme_bw() +
  labs(x = "Social connectedness", y = "Flourishing")


p3 <- dog_data %>%
  mutate(Group = factor(GroupAssignment, 
                        levels = c("Control", "Indirect", "Direct"))) %>%
  ggplot(aes(x = SCS, y = Lonely, color = Group, shape = Group)) +
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
  pivot_longer(cols = starts_with(c("FS", "PANAS_PA", "SCS", "SHS", "Engagement", "Stress",
                                    "Homesick", "Lonely", "PANAS_NA")),
               names_to = "variable",
               values_to = "value")%>%
  mutate(variable = factor(variable,
                           levels = c("FS", "PANAS_PA", "SCS", "SHS", "Engagement", "Stress",
                                      "Homesick", "Lonely", "PANAS_NA"),
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
# Table 4 (Hypothesis 2 -- Binfet et al contrast with a different approach -- Bonferonni Adjusted )
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
library(emmeans)

dog_data %>%
  mutate(GroupAssignment = factor(GroupAssignment, 
                                  levels = c("Control", "Indirect", "Direct")))%>%
  pivot_longer(cols = starts_with(c("FS", "PANAS_PA", "SCS", "SHS", "Engagement", "Stress",
                                    "Homesick", "Lonely", "PANAS_NA")),
               names_to = "variable",
               values_to = "value")%>%
  mutate(variable = factor(variable,
                           levels = c("FS", "PANAS_PA", "SCS", "SHS", "Engagement", "Stress",
                                      "Homesick", "Lonely", "PANAS_NA"),
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
# Table 4 (Hypothesis 2 -- 'fixed' contrasts a statistician would run -- Bonferroni Adjusted )
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
library(emmeans)
tab1<-dog_data %>%
  mutate(GroupAssignment = factor(GroupAssignment, 
                                  levels = c("Control", "Indirect", "Direct")))%>%
  pivot_longer(cols = starts_with(c("FS", "PANAS_PA", "SCS", "SHS", "Engagement", "Stress",
                                    "Homesick", "Lonely", "PANAS_NA")),
               names_to = "variable",
               values_to = "value")%>%
  mutate(variable = factor(variable,
                           levels = c("FS", "PANAS_PA", "SCS", "SHS", "Engagement", "Stress",
                                      "Homesick", "Lonely", "PANAS_NA"),
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
  pivot_longer(cols = starts_with(c("FS", "PANAS_PA", "SCS", "SHS", "Engagement", "Stress",
                                    "Homesick", "Lonely", "PANAS_NA")),
               names_to = "variable",
               values_to = "value")%>%
  mutate(variable = factor(variable,
                           levels = c("FS", "PANAS_PA", "SCS", "SHS", "Engagement", "Stress",
                                      "Homesick", "Lonely", "PANAS_NA"),
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
  pivot_longer(cols = starts_with(c("FS", "PANAS_PA", "SCS", "SHS", "Engagement", "Stress",
                                    "Homesick", "Lonely", "PANAS_NA")),
               names_to = "variable",
               values_to = "value")%>%
  mutate(variable = factor(variable,
                           levels = c("FS", "PANAS_PA", "SCS", "SHS", "Engagement", "Stress",
                                      "Homesick", "Lonely", "PANAS_NA"),
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
  pivot_longer(cols = starts_with(c("FS", "PANAS_PA", "SCS", "SHS", "Engagement", "Stress",
                                    "Homesick", "Lonely", "PANAS_NA")),
               names_to = "variable",
               values_to = "value")%>%
  mutate(variable = factor(variable,
                           levels = c("FS", "PANAS_PA", "SCS", "SHS", "Engagement", "Stress",
                                      "Homesick", "Lonely", "PANAS_NA"),
                           labels = c("FS", "PANAS_PA", "SCS", "SHS", "Engagement", "Stress",
                                      "Homesick", "Lonely", "PANAS_NA"))) %>%
  mutate(alt = case_when(variable %in% c("FS", "PANAS_PA", "SCS", "SHS", "Engagement") ~ "greater",
                         variable %in% c("Stress","Homesick", "Lonely", "PANAS_NA") ~ "less"))%>%
  pivot_wider(values_from = value, names_from = Stage) %>%
  filter(complete.cases(variable,GroupAssignment))


aov.obj <- aov(post~pre+GroupAssignment, data=test.data)
data.frame(lmtest::coeftest(aov.obj, vcov. = sandwich::vcovHC(aov.obj))[,])

