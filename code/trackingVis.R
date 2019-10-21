# Viz
# run after rmdSetup.R
# library(networkD3)

sankeyData <- LIculvertDataStatus_location %>% st_drop_geometry() %>% 
  replace_na(list(FieldAssessmentComplete = "N", DesktopAssessmentComplete = "N", FullAssessmentComplete = "N")) %>% 
  mutate(Priority = ifelse(ToBeAssessed_2019, "Priority Crossings", "Tier 2 Priorities")) %>% 
  mutate(FieldAssessmentComplete = recode(FieldAssessmentComplete, 
                                          Y = "Field \nAssessments \nComplete", 
                                          N = "Field \nAssessments \nIncomplete",
                                          `In Progress` = "Field \nAssessments \nIn Progress")) %>% 
  mutate(DesktopAssessmentComplete = recode(DesktopAssessmentComplete, 
                                            Y = "Desktop \nAssessment \nComplete", 
                                            `In Progress` = "Desktop \nAssessment \nIn Progress",
                                            N = "Desktop \nAssessment \nScheduled")) %>% 
  mutate(Located =  recode(Located, Y = "Ground-truthed")) %>% 
  mutate(Scheduled = ifelse(ToBeAssessed_2019, "Field Assessment \n scheduled", "Unscheduled")) %>% 
  select(crossingID, Located, Priority, Scheduled, FieldAssessmentComplete:FullAssessmentComplete) %>% 
  group_by(Located, Priority, Scheduled, FieldAssessmentComplete, DesktopAssessmentComplete) %>% 
  tally()
  
library(ggalluvial)
sankeyData %>% ggplot(aes(axis1 = Located, 
                          axis2 = Priority,
                          #axis4 = FieldAssessmentComplete,
                          axis3 = DesktopAssessmentComplete,
                          axis5 = Scheduled,
                          y = n)) +
  geom_alluvium(aes(fill = FieldAssessmentComplete), alpha = .9) +
  scale_x_discrete(limits = c("", "Priority", "Desktop Assessment", "Field Assessment", "Scheduled")) +
  geom_stratum(width = .7, knot.pos = .7, alpha = .2, fill = "#009ecb") +
  geom_text(stat = "stratum", label.strata = TRUE, nudge_y = 5, hjust = "center") +
  geom_text(stat = 'stratum', aes(label = n), nudge_y = -15, fontface = "bold") +
  theme_ipsum(base_size = 12) +  
  # scale_fill_manual(values = c("#0096d6", "#dac792")) +
  labs(title = paste0("As of ", Sys.Date() %>% format("%B %d %Y"))) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), rect = element_blank()) +
  NULL


ggsave(filename = paste0(tidalCulvert_outputs, "/ProgressDiagram_", Sys.Date(), ".png"), device = png(), width = 10, height = 7, units = "in")

# Pivot Table approach


SummaryData <- LIculvertDataStatus_location %>% st_drop_geometry() %>% 
  replace_na(list(FieldAssessmentComplete = "N", DesktopAssessmentComplete = "N", FullAssessmentComplete = "N")) %>% 
  # mutate(Priority = ifelse(ToBeAssessed_2019, "Tier 1 Priorities", "Tier 2 Priorities")) %>% 
  # mutate(FieldAssessmentComplete = recode(FieldAssessmentComplete, 
  #                                         Y = "Field \nAssessments \nIn Progress", 
  #                                         N = "Field \nAssessments \nIncomplete",
  #                                         `In Progress` = "Field \nAssessments \nIn Progress")) %>% 
  # mutate(DesktopAssessmentComplete = recode(DesktopAssessmentComplete, 
  #                                           Y = "Desktop \nAssessment \nIn Progress", 
  #                                           `In Progress` = "Desktop \nAssessment \nIn Progress",
  #                                           N = "Desktop \nAssessment \nScheduled")) %>% 
  # mutate(Located =  recode(Located, Y = "Ground-truthed")) %>% 
  # mutate(Scheduled = ifelse(ToBeAssessed_2019, "Field Assessment \n scheduled", "Unscheduled")) %>% 
  select(crossingID, Located, FieldAssessmentComplete:FullAssessmentComplete) %>% 
  group_by(Located, FieldAssessmentComplete, DesktopAssessmentComplete) %>% 
  tally()# No R code for this pivotTable setting


