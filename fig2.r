d <- read_rds('expt-01-clean-data.rds')

d %>%
  filter(itemType == 'experimental') %>%
  mutate(ellipsis = ifelse(ellipsis == 'ellipsis',1,0),
         mismatch = ifelse(mismatch == 'Mismatch',0.5,-0.5)) %>%
  brm(formula = acc ~ ellipsis*mismatch*wh.word +
        (1 + ellipsis*mismatch*wh.word | sid) +
        (1 + ellipsis*mismatch*wh.word | itemNo),
      family = cumulative(probit),
      inits = 0,
      prior = prior(normal(0,4), class = 'b'),
      warmup = 1000,
      iter = 2000,
      cores = 4,
      seed = 1234) ->
  m

hyps = c('overall when effect' = 'wh.wordwhen < 0',
         'overall where effect' = 'wh.wordwhere < 0',
         'ellipsis-specific when effect' = 'ellipsis:wh.wordwhen < 0',
         'ellipsis-specific where effect' = 'ellipsis:wh.wordwhere < 0')

hypothesis(m,
           hyps,
           alpha = 0.025)$hypothesis %>%
  select(-Est.Error,-Evid.Ratio) %>%
  mutate_at(vars(-Hypothesis,-Star),
            round,
            digits = 2)

hyps = c('when_noellipsis' = 'wh.wordwhen < 0',
         'when_ellipsis' = 'wh.wordwhen + ellipsis:wh.wordwhen < 0',
         'where_noellipsis' = 'wh.wordwhere < 0',
         'where_ellipsis' = 'wh.wordwhere + ellipsis:wh.wordwhere < 0')

hypothesis(m,
           hyps,
           group='itemNo',
           scope='coef',
           alpha = 0.025)$hypothesis %>%
  select(-Est.Error,-Evid.Ratio) %>%
  mutate_at(vars(-Group,-Hypothesis,-Star),
            round,
            digits = 2) ->
  h

h %>%
  select(Group,Hypothesis,Estimate,CI.Lower,CI.Upper,Post.Prob) %>%
  separate(Hypothesis,into=c('wh.word','ellipsis')) %>%
  mutate(Item = factor(Group,levels=1:24),
         ellipsis = ifelse(ellipsis == 'noellipsis','unelided','elided')) %>%
  ggplot(aes(x = Item,
             y = Estimate,
             group = ellipsis)) +
  geom_hline(yintercept = 0,
             linetype = 'dashed') +
  facet_wrap(~wh.word,ncol=1) +
  geom_errorbar(aes(ymin = CI.Lower,
                    ymax = CI.Upper),
                width = 0,
                position = position_dodge(width=0.5)) +
  geom_point(shape = 21,
             position = position_dodge(width=0.5),
             aes(fill = ellipsis)) +
  scale_fill_manual(name = element_blank(),
                    values = c('black','white')) +
  scale_y_continuous(name = 'Model coefficient') +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position=c(0.5,0.95),
        legend.direction='horizontal',
        legend.margin=margin(c(0,0,0,0)))