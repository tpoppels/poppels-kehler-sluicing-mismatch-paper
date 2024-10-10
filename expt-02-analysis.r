
## Expt 2: Norming experiment
read_rds('norming-expt-clean-data.rds') %>%
  group_by(mismatch,paraphrase.type) %>%
  count() %>%
  group_by(mismatch) %>%
  mutate(n_total = sum(n)) %>%
  group_by(mismatch,paraphrase.type) %>%
  mutate(prop = n / n_total,
         ci.lower = exactci(n,n_total,0.95)$conf.int[[1]],
         ci.upper = exactci(n,n_total,0.95)$conf.int[[2]]) %>%
  ungroup() %>%
  mutate_at(vars(-mismatch,-paraphrase.type),
            round,
            digits = 3)

read_rds('norming-expt-clean-data.rds') %>%
  mutate(total_n = n()) %>%
  group_by(paraphrase.type,total_n) %>%
  summarize(n = length(response)) %>%
  ungroup() %>%
  mutate(prop = n / total_n) %>%
  mutate_at(vars(prop), round, digits = 3)

## Expt 2: Regression
read_rds('expt-02-clean-data.rds') %>%
  filter(!is.na(mismatch)) %>%
  ungroup() %>%
  mutate(wh.word = factor(wh.word), ## will be entered as grouping factor
         ellipsis = ifelse(ellipsis=='unelided', -0.5, 0.5),
         mismatch = ifelse(mismatch == 'match', -0.5, 0.5)) %>%
  brm(formula = response ~ ellipsis*mismatch +
        (1 + ellipsis*mismatch | sid) +
        (1 + ellipsis*mismatch | item) +
        (1 + ellipsis*mismatch | wh.word),
      family = cumulative(probit),
      inits = 0,
      warmup = 1000,
      iter = 3000,
      control = list(adapt_delta = 0.99),
      prior = prior(normal(0,4), class = 'b'),
      cores = 4,
      seed = 1234) ->
  m

## Testing population-level hypotheses
hyps = c('ellipsis penalty (overall)' = 'ellipsis < 0',
         'mismatch penalty (overall)' = 'mismatch < 0',
         'ellipsis:mismatch interaction' = 'ellipsis:mismatch < 0')

hypothesis(m,
           hyps,
           alpha = 0.025)$hypothesis %>%
  select(-Est.Error,-Evid.Ratio) %>%
  mutate_at(vars(-Hypothesis,-Star),
            round,
            digits = 2)

## Testing hypotheses for each wh-word
hyps = c('ellipsis penalty (overall)' = 'ellipsis < 0',
         'mismatch penalty (overall)' = 'mismatch < 0',
         'ellipsis:mismatch interaction' = 'ellipsis:mismatch < 0')

hypothesis(m,
           hyps,
           group = 'wh.word',
           scope = 'coef',
           alpha = 0.025)$hypothesis %>%
  select(-Est.Error,-Evid.Ratio) %>%
  mutate_at(vars(-Group,-Hypothesis,-Star),
            round,
            digits = 2)