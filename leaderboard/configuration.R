campaigns = list(
  data.frame(campaign = 'Summer 2019', start = '2019-06-01', stop = '2019-09-24',
    stringsAsFactors = FALSE),
  data.frame(campaign = 'Fall 2019', start = '2019-09-24', stop = '2020-03-15',
    stringsAsFactors = FALSE),
  data.frame(campaign = 'Social distancing 2020', start = '2020-03-16',
    stop = '2020-04-30',
    stringsAsFactors = FALSE)
  )
campaigns = dplyr::bind_rows(campaigns)
campaigns = dplyr::mutate(campaigns, start = as.Date(start), stop = as.Date(stop))

default_current_campaign = dplyr::filter(campaigns, campaign == 'Social distancing 2020')
