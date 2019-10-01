campaigns = list(
  data.frame(campaign = 'Summer 2019', start = '2019-06-01', stop = '2019-09-24',
    stringsAsFactors = FALSE),
  data.frame(campaign = 'Fall 2019', start = '2019-09-24', stop = '2019-12-31',
    stringsAsFactors = FALSE)
  )
campaigns = dplyr::bind_rows(campaigns)
campaigns = dplyr::mutate(campaigns, start = as.Date(start), stop = as.Date(stop))

default_current_campaign = dplyr::filter(campaigns, campaign == 'Fall 2019')
