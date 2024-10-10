d <- read_rds('expt-01-clean-data.rds')

d %>%
  filter(itemType=="filler") %>%
  filter(str_detect(filler.type," elliptical")) %>%
  group_by(filler.type) %>%
  summarize(mean.acc=mean(acc)) ->
  filler.means

d %>%
  filter(itemType=="experimental") %>%
  group_by(mismatch,ellipsis,wh.word) %>%
  summarize(mean.acc=mean(acc),
            se=sd(acc)/sqrt(length(acc))) %>%
  ggplot(aes(x=mismatch,y=mean.acc,group = wh.word)) +
  facet_wrap(~ellipsis,ncol=2) +
  geom_errorbar(aes(ymin=mean.acc-se,
                    ymax=mean.acc+se),
                width=0.3,
                position = position_dodge(0.2)) +
  geom_point(aes(color=wh.word,shape=wh.word),
             size=4,
             position = position_dodge(0.2)) +
  geom_line(aes(group=wh.word,color=wh.word),
            size=1) +
  theme_bw() +
  scale_color_manual(name = 'wh-word:',
                     values = c('black','orange','grey')) +
  scale_shape_discrete(name = 'wh-word:') +
  scale_y_continuous(name="Mean acceptability",
                     limits=c(1,5)) +
  scale_x_discrete(name=element_blank()) +
  theme(legend.background=element_rect(color="black"))

