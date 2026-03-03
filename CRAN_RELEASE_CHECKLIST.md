# CRAN Release Checklist (optbinningR)

## 1) Final metadata updates
- Edit `DESCRIPTION`:
  - Replace placeholder maintainer email with real CRAN contact.
  - Confirm `URL` and `BugReports` are live.

## 2) Build source tarball
```bash
cd /Users/sudheerrani/Documents/Sahaj_coding_codex/optbinningR
R CMD build .
```

## 3) Run CRAN-like checks (internet-enabled machine)
```bash
cd /Users/sudheerrani/Documents/Sahaj_coding_codex/optbinningR
R CMD check --as-cran optbinningR_0.2.0.tar.gz
```

## 4) Submit to CRAN
In R:
```r
install.packages("devtools")
devtools::submit_cran("optbinningR_0.2.0.tar.gz")
```
Or upload via CRAN web submission form.

## 5) After CRAN acceptance
Users install with:
```r
install.packages("optbinningR")
```

## 6) Optional immediate distribution before CRAN
- Publish GitHub tag/release with `optbinningR_0.2.0.tar.gz` attached.
- Users install with:
```r
remotes::install_github("sahaj-coding/optbinningR")
# or
install.packages("https://github.com/sahaj-coding/optbinningR/releases/download/v0.2.0/optbinningR_0.2.0.tar.gz", repos = NULL, type = "source")
```
