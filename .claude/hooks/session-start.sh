#!/bin/bash
# SessionStart hook: make R and the gRs dependencies available in Claude Code
# on the web, so devtools::test() and R CMD check can run in-session.
set -euo pipefail

# Local sessions already have R installed; only the cloud container needs this.
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

export DEBIAN_FRONTEND=noninteractive
SUDO=""
[ "$(id -u)" -ne 0 ] && SUDO="sudo"

# Dependencies that Ubuntu packages directly. Installing from apt rather than
# CRAN keeps this working under a restricted egress policy, and the container
# image is cached once the hook has run.
APT_PACKAGES=(
  r-base-core
  r-cran-dplyr r-cran-dt r-cran-ggplot2 r-cran-ggtext r-cran-glue
  r-cran-lubridate r-cran-magrittr r-cran-openxlsx r-cran-purrr
  r-cran-readxl r-cran-rlang r-cran-stringr r-cran-tidyr r-cran-withr
  r-cran-writexl
  r-cran-rcolorbrewer
  r-cran-testthat r-cran-devtools r-cran-pkgload
)

if ! command -v Rscript >/dev/null 2>&1; then
  $SUDO apt-get update -qq
fi
$SUDO apt-get install -y --no-install-recommends "${APT_PACKAGES[@]}" >/dev/null

# janitor, openair, trend and gt have no Ubuntu package, so they can only come
# from CRAN. CRAN is blocked under the default network policy; if the
# environment is later allowed to reach it, pick them up here.
MISSING=$(Rscript -e 'cat(setdiff(c("janitor","openair","trend","gt"), rownames(installed.packages())), sep=" ")')
if [ -n "$MISSING" ]; then
  if curl -sS -o /dev/null --max-time 15 https://cloud.r-project.org 2>/dev/null; then
    # shellcheck disable=SC2086
    Rscript -e "install.packages(commandArgs(TRUE), repos='https://cloud.r-project.org')" $MISSING || true
    MISSING=$(Rscript -e 'cat(setdiff(c("janitor","openair","trend","gt"), rownames(installed.packages())), sep=" ")')
  fi
fi

if [ -n "$MISSING" ]; then
  echo "gRs setup: R is installed, but these packages are unavailable: $MISSING" >&2
  echo "  janitor, openair and trend are Imports. pkgload checks every Import" >&2
  echo "  before it loads the package, so no test runs at all until they are" >&2
  echo "  present - not even a test file that does not touch them. To fix," >&2
  echo "  set the cloud environment's network access to Custom and allow" >&2
  echo "  cloud.r-project.org, keeping the default package-manager list." >&2
fi
