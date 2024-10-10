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
  select(Group,Hypothesis,Estimate,CI.Lower,CI.Upper) %>%
  separate(Hypothesis,into=c('wh.word','ellipsis')) %>%
  unite('combined',Estimate,CI.Lower,CI.Upper) %>%
  spread(key=ellipsis,value=combined) %>%
  separate(ellipsis,sep='_',into=c('ellipsis_Estimate','ellipsis_CI.Lower','ellipsis_CI.Upper')) %>%
  separate(noellipsis,sep='_',into=c('noellipsis_Estimate','noellipsis_CI.Lower','noellipsis_CI.Upper')) %>%
  mutate_at(vars(-Group,-wh.word),as.numeric) %>%
  ggplot(aes(x = noellipsis_Estimate,
             y = ellipsis_Estimate)) +
  facet_wrap(~wh.word) +
  geom_errorbar(aes(ymin = ellipsis_CI.Lower, ymax = ellipsis_CI.Upper),
                alpha = 0.2) +
  geom_errorbarh(aes(xmin = noellipsis_CI.Lower, xmax = noellipsis_CI.Upper),
                 alpha = 0.2) +
  geom_point() +
  scale_x_continuous(name = "unelided variant") +
  scale_y_continuous(name = "elided variant") +
  theme_bw()