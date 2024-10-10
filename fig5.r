read_rds('expt-03-clean-data.rds') %>%
  filter(!is.na(mismatch)) %>%
  mutate(mismatch=ifelse(mismatch=='mismatch','Mismatch','Match'),
         ellipsis=ifelse(ellipsis=='elided','ellipsis','no ellipsis')) %>%
  group_by(mismatch,`wh.word`,ellipsis) %>%
  summarize(mean.acc=mean(response),
            se=sd(response)/sqrt(length(response))) %>%
  ggplot(aes(x=mismatch,y=mean.acc,ymin=mean.acc-se,ymax=mean.acc+se,color=`wh.word`,shape=`wh.word`)) +
  facet_grid(~ellipsis) +
  geom_line(size=1,aes(group=interaction(ellipsis,`wh.word`)))  +
  geom_errorbar(width=0.1) +
  geom_point(size=3) +
  scale_color_manual(values=c('black','orange','grey')) +
  scale_y_continuous(name='Mean acceptability') +
  scale_x_discrete(name=element_blank()) +
  theme_bw(base_size=12) +
  theme(legend.background=element_rect(color='black'),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank())