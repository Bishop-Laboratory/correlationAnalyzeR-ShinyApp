# correlationAnalyzeR-ShinyApp

A web application interface for the correlationAnalyzeR R package written in Shiny.

## Local installation instructions

To set up a local shiny server do the following:

1. Clone the repo

```shell
git clone https://github.com/Bishop-Laboratory/correlationAnalyzeR-ShinyApp.git
cd correlationAnalyzeR-ShinyApp/
```

2. Install R v4.2.0 session and get the `renv` package

```R
R -e 'if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")'
```

3. Install `getSysReqs`

```shell
R -e 'if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")'
R -e 'remotes::install_github("mdneuzerling/getsysreqs", force=TRUE)'
```

4. Install system reqs (requires sudo)

```shell
REQS=$(Rscript -e 'options(warn = -1); cat(getsysreqs::get_sysreqs("renv.lock"))' | sed s/"WARNING: ignoring environment value of R_HOME"//) \
  && echo $REQS \
  && sudo apt-get install -y $REQS
```

5. Optional: You may need `cmake > v3.15` to build `nlopt`. If so, follow these instructions to get the latest `cmake`: [link](https://askubuntu.com/a/865294/952008)

6. Restore the R environment to build all dependencies

```shell
R -e "renv::restore()"
```

7. Run the server

```shell
Rscript app.R
```
