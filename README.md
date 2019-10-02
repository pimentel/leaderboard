# paper reading club leaderboard

This is a shiny app for paper reading club

Actual app is in the `leaderboard` directory.

## setup

It should just work, but you need to configure `url.R` with the following:

```r
SHEET_URL = 'https://...'
```

Make sure that you have published the Google Sheet to public (`File -> Published to the web`).
Afterwards, the url is the url you get from clicking 'Share' in the top right hand corner.

```r
library('shiny')

# for local testing
runApp()
```
