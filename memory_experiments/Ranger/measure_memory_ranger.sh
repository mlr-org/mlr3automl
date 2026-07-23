#!/usr/bin/env bash
# ------------------------------------------------------------
# measure_memory_ranger.sh  —  sweep parameters, capture peak RAM
# needs GNU time: gtime on macOS (brew install gnu-time),
# /usr/bin/time on Linux (apt install time)
# ------------------------------------------------------------
set -euo pipefail

SUFFIX="$1"
SCRIPT="learner_ranger.R"
INCSV="parameters_ranger_${SUFFIX}.csv"
OUTCSV="memory_result_ranger_${SUFFIX}.csv"

if command -v gtime >/dev/null 2>&1; then
  GTIME="gtime"
else
  GTIME="/usr/bin/time"
fi

# ---------------------------------------------------------------------------
# how many data rows already done?
if [ -f "$OUTCSV" ]; then
  # total lines minus 1 header → done rows
  DONE=$(( $(wc -l <"$OUTCSV") - 1 ))
else
  # no file yet -> write header
  DONE=0
  echo "nrow,nfeatures,num_trees,mtry_ratio,sample_fraction,replace,rss_kb,rss_mib" >"$OUTCSV"
fi

# set the starting line in the input (header + DONE rows)
START=$(( DONE + 2 ))

# Skip header and feed the rows to the loop
tail -n +"$START" "$INCSV" | while IFS=',' read -r _ nrow nfeat ntrees mtry sfrac replace; do

  rss_kb=$(
    { "$GTIME" -v Rscript "$SCRIPT" \
    "$nrow" "$nfeat" "$ntrees" "$mtry" "$sfrac" "$replace"; } 2>&1 \
    | awk -F: '/Maximum resident set size/{ gsub(/[^0-9]/, "", $2); print $2 }'
  )

  # Convert kbytes to MiB
  rss_mib=$(bc -l <<<"$rss_kb/1024")

  printf '%s,%s,%s,%s,%s,%s,%s,%s\n' \
    "$nrow" "$nfeat" "$ntrees" "$mtry" "$sfrac" "$replace" \
    "$rss_kb" "$rss_mib" >> "$OUTCSV"

  printf '✔  %-30s → %8.2f MiB\n' \
    "$nrow-$nfeat-$ntrees-$sfrac" "$rss_mib"
done || true
