name: Deploy NECBL Dashboard

on:
  push:
    branches:
      - main
  schedule:
    # Runs every night at 4am ET (8am UTC)
    - cron: '0 8 * * *'

concurrency:
  group: deploy-necbl
  cancel-in-progress: false

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: '4.4.1'

      - name: Install system dependencies
        run: |
          sudo apt-get update
          sudo apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev \
            texlive-xetex texlive-fonts-recommended texlive-latex-extra

      - name: Install R packages
        run: |
          install.packages(c(
            "shiny", "dplyr", "tidyr", "readr", "ggplot2", "plotly",
            "DT", "scales", "httr", "jsonlite", "gridExtra", "openxlsx", "rsconnect"
          ))
        shell: Rscript {0}

      - name: Write Google service account credentials
        env:
          GOOGLE_SERVICE_ACCOUNT_JSON: ${{ secrets.GOOGLE_SERVICE_ACCOUNT_JSON }}
        run: |
          echo "$GOOGLE_SERVICE_ACCOUNT_JSON" > service_account.json

      - name: Write admin credentials
        env:
          ADMIN_PASSWORD: ${{ secrets.ADMIN_PASSWORD }}
          APP_PASSWORD: ${{ secrets.APP_PASSWORD }}
          GH_PAT: ${{ secrets.GH_PAT }}
        run: |
          echo "$ADMIN_PASSWORD" > admin_password.txt
          echo "$APP_PASSWORD" > app_password.txt
          echo "$GH_PAT" > gh_pat.txt

      - name: Deploy to shinyapps.io
        env:
          SHINYAPPS_ACCOUNT: ${{ secrets.SHINYAPPS_ACCOUNT }}
          SHINYAPPS_TOKEN: ${{ secrets.SHINYAPPS_TOKEN }}
          SHINYAPPS_SECRET: ${{ secrets.SHINYAPPS_SECRET }}
          GOOGLE_SERVICE_ACCOUNT_JSON: ${{ secrets.GOOGLE_SERVICE_ACCOUNT_JSON }}
        run: |
          rsconnect::setAccountInfo(
            name   = Sys.getenv("SHINYAPPS_ACCOUNT"),
            token  = Sys.getenv("SHINYAPPS_TOKEN"),
            secret = Sys.getenv("SHINYAPPS_SECRET")
          )
          deploy_with_retry <- function(attempts=3, wait_secs=30) {
            for (i in seq_len(attempts)) {
              result <- tryCatch({
                rsconnect::deployApp(
                  appDir      = ".",
                  appName     = "NECBLDashboard",
                  account     = Sys.getenv("SHINYAPPS_ACCOUNT"),
                  server      = "shinyapps.io",
                  forceUpdate = TRUE,
                  lint        = FALSE
                )
                TRUE
              }, error = function(e) {
                message("Attempt ", i, " failed: ", e$message)
                if (i < attempts) {
                  message("Waiting ", wait_secs, "s before retry...")
                  Sys.sleep(wait_secs)
                }
                FALSE
              })
              if (result) break
            }
          }
          deploy_with_retry()
        shell: Rscript {0}
