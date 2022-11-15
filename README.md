# Org-Fit-HS

Extremely hacky scipt based on https://github.com/pbrisbin/google-oauth2 used to retrieve Google Fit data
and process it for org mode time stamps.

## Usage
Place your client id and your client secret in one line each in `~/.local/share/org-fit-hs.credentials`, then run `stack install` to generate the `org-fit` executable.
It retrieves all Google Fit activity measured by my specific phone and outputs the times as org mode clock timespans, between the first command line argument (the start time) in a format along the lines of `2022-11-15 12:20:00 UTC` and the current time.
