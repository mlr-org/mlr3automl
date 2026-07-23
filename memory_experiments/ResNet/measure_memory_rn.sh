#!/usr/bin/env bash
# ------------------------------------------------------------
# measure_rss.sh  —  sweep parameters, capture peak RAM (macOS)
# ------------------------------------------------------------
set -euo pipefail

SUFFIX="$1"
SCRIPT="learner_rn.R"
INCSV="parameters_rn_${SUFFIX}.csv"
OUTCSV="memory_result_rn_${SUFFIX}.csv"

# ---------------------------------------------------------------------------
# how many data rows already done?
if [ -f "$OUTCSV" ]; then
  # total lines minus 1 header → done rows
  DONE=$(( $(wc -l <"$OUTCSV") - 1 ))
else
  # no file yet -> write header
  DONE=0
  echo "nrow,nfeatures,n_blocks,d_block,d_hidden_multiplier,dropout1,dropout2,lr,weight_decay,epochs,rss_kb,rss_mib" >"$OUTCSV"
fi

# set the starting line in the input (header + DONE rows)
START=$(( DONE + 2 ))

# Skip header and feed the rows to the loop
tail -n +"$START" "$INCSV" | while IFS=',' read -r _ nrow nfeatures n_blocks d_block d_hidden_multiplier dropout1 dropout2 lr wd epochs; do

  rss_kb=$(
    { gtime -v Rscript "$SCRIPT" \
    "$nrow" "$nfeatures" "$n_blocks" "$d_block" "$d_hidden_multiplier" "$dropout1" "$dropout2" "$lr" "$wd" "$epochs"; } 2>&1 \
    | awk -F: '/Maximum resident set size/{ gsub(/[^0-9]/, "", $2); print $2 }'
  )

  # Convert kbytes to MiB
  rss_mib=$(bc -l <<<"$rss_kb/1024")

  printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
    "$nrow" "$nfeatures" "$n_blocks" "$d_block" "$d_hidden_multiplier" "$dropout1" "$dropout2" "$lr" "$wd" "$epochs" \
    "$rss_kb" "$rss_mib" >> "$OUTCSV"

  printf '✔  %-30s → %8.2f MiB\n' \
    "$nrow-$nfeatures-$n_blocks-$d_block-$d_hidden_multiplier-$dropout1-$dropout2-$lr-$wd-$epochs" "$rss_mib"
done || true
