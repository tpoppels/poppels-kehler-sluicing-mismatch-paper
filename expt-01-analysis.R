d <- read_rds('expt-01-clean-data.rds')

## Expt 1: Regression 1
d %>%
  filter(itemType == 'experimental') %>%
  mutate(wh.word = factor(wh.word, levels = c('when','where','how')),
         ellipsis = ifelse(ellipsis == 'no ellipsis',1,0),
         mismatch = ifelse(mismatch == 'Mismatch',1,0)) ->
  tmp

contrasts(tmp$wh.word) <- contr.sum(3)/3

tmp %>%
  brm(formula = acc ~ ellipsis*mismatch*wh.word +
        (1 + ellipsis*mismatch*wh.word | sid) +
        (1 + ellipsis*mismatch*wh.word | itemNo),
      family = cumulative(probit),
      inits = 0,
      prior = prior(normal(0,4), class = 'b'),
      warmup = 1000,
      iter = 3000,
      cores = 4) ->
  m

hyps = c('mismatch (ellipsis)' = 'mismatch < 0',
         'mismatch (no ellipsis)' = 'mismatch + ellipsis:mismatch < 0',
         'ellipsis:mismatch int' = 'ellipsis:mismatch < 0')

hypothesis(m,
           hyps,
           alpha = 0.025)$hypothesis %>%
  select(-Est.Error,-Evid.Ratio) %>%
  mutate_at(vars(-Hypothesis,-Star),
            round,
            digits = 2)

## Expt 1: Regression 2
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

## Expt 1: Correlation
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
  group_by(wh.word) %>%
  summarize(r = cor(ellipsis_Estimate,noellipsis_Estimate)) %>%
  mutate(r = round(r,3))
