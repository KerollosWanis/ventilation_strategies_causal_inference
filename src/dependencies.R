requiredPackages = c(
  'tidyverse',
  'data.table',
  'haven',
  'splines',
  'fastDummies',
  'gdata',
  'ranger',
  'caret',
  'ipred',
  'caret',
  'gbm',
  'pbapply',
  'glmnet',
  'wavethresh',
  'wavelets',
  'parallel',
  'SMUT',
  'fda',
  'nlshrink',
  'ggpubr'
)
for (p in requiredPackages) {
  if (!require(p, character.only = TRUE))
    install.packages(p)
  library(p, character.only = TRUE)
}

