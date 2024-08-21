#!/usr/bin/env bash

set -x

# Required bc

# Wait time: Real 180-300s (scaled by factor 4: 45-75s)

OWN_IP=$(ip a | grep "inet 10.128." | sed 's/.*\(10\.128\..*\)\/.*/\1/g')
DAS_APIDOWN="10.128.0.6:5000/down?ip=$OWN_IP"
DAS_APIUP="10.128.0.6:5000/up?ip=$OWN_IP"
DAS_APIHOLD="10.128.0.6:5000/hold?ip=$OWN_IP"
DAS_TFROM=45
DAS_TTO=75
DAS_LOGFILE="../das/das.log"

calc_up_probability() {
  current_value=$(echo "$1 / 600000000.0" | bc -l)
  lower_threshold="0.7"
  cap="1"
  upper_threshold="0.95"
  k=$(echo "1 / ($lower_threshold - $upper_threshold) * l(1.0 / (1 + $cap))" | bc -l)
  A=$(echo "(1 + $cap) * e(-1 * $k * $upper_threshold)" | bc -l)
  if (( $(echo "$current_value < $lower_threshold" |bc -l) )); then
    echo "0"
  elif (( $(echo "$current_value > $upper_threshold" |bc -l) )); then
    echo "$cap"
  else
    value=$(echo "$A * e($k * $current_value) - 1" | bc -l)
    if (( $(echo "$value < $cap" |bc -l) )); then
      echo "$value"
    else
      echo "$cap"
    fi
  fi
}

calc_down_probability() {
  current_value=$(echo "$1 / 600000000.0" | bc -l)
  lower_threshold="0"
  upper_threshold="0.3"
  cap="0.5"
  k=$(echo "1 / ($lower_threshold - $upper_threshold) * l(1 + $cap)" | bc -l)
  A=$(echo "e(-1 * $k * $upper_threshold)" | bc -l)
  if (( $(echo "$current_value > $upper_threshold" |bc -l) )); then
    echo "0"
  else
    value=$(echo "$A * e($k * $current_value) - 1" | bc -l)
    if (( $(echo "$value < $cap" |bc -l) )); then
      echo "$value"
    else
      echo "$cap"
    fi
  fi
}

#Variables are coming as env vars not command-line args
#while [[ $# -gt 0 ]]; do
#  case $1 in
#    -u|--apiup)
#      DAS_APIUP="$2"
#      shift # past argument
#      shift # past value
#      ;;
#    -d|--apidown)
#      DAS_APIDOWN="$2"
#      shift # past argument
#      shift # past value
#      ;;
#    -H|--apihold)
#      DAS_APIHOLD="$2"
#      shift # past argument
#      shift # past value
#      ;;
#    -f|--tfrom)
#      DAS_TFROM="$2"
#      shift # past argument
#      shift # past value
#      ;;
#    -t|--tto)
#      DAS_TTO="$2"
#      shift # past argument
#      shift # past value
#      ;;
#    -l|--log)
#      DAS_LOGFILE="$2"
#      shift # past argument
#      shift # past value
#      ;;
#    -*|--*)
#      echo "Unknown option $1"
#      exit 1
#      ;;
#    *)
#      echo "Unknown positional arg $1"
#      exit 1
#      ;;
#  esac
#done

TRANSMIT=0
RECEIVE=0
OLD_TRANSMIT=0
OLD_RECEIVE=0

while true; do
  WAIT_TIME=$(shuf -i ${DAS_TFROM}-${DAS_TTO} -n 1)
  echo "Wait for $WAIT_TIME seconds"
  sleep $WAIT_TIME
  NET_STATS=$(cat /proc/net/dev | grep ens4 | xargs)
  IFS=' ' read -r -a STATS_ARRAY <<< "$NET_STATS"
  RECEIVE="${STATS_ARRAY[1]}"
  TRANSMIT="${STATS_ARRAY[9]}"
  TRANSMIT_BANDWIDTH=$(bc -l <<< "8 * ($TRANSMIT - $OLD_TRANSMIT) / $WAIT_TIME")
  RECEIVE_BANDWIDTH=$(bc -l <<< "8 * ($RECEIVE - $OLD_RECEIVE) / $WAIT_TIME")
  UP_PROB=$(calc_up_probability "$TRANSMIT_BANDWIDTH")
  DOWN_PROB=$(calc_down_probability "$TRANSMIT_BANDWIDTH")
  CURRENT_TIME=$(date -u)
  echo "$CURRENT_TIME : Transmit Bandwidth: $TRANSMIT_BANDWIDTH" >> "$DAS_LOGFILE"
  echo "$CURRENT_TIME : Up Probability: $UP_PROB" >> "$DAS_LOGFILE"
  echo "$CURRENT_TIME : Down Probability: $DOWN_PROB" >> "$DAS_LOGFILE"
  DECISION=$(awk -v n=1 -v seed="$RANDOM" 'BEGIN { srand(seed); for (i=0; i<n; ++i) printf("%.6f\n", rand()) }')
  TARGET_URL=""
  if (( $(echo "$DOWN_PROB > $DECISION" | bc -l) )); then
    TARGET_URL=$DAS_APIDOWN
  elif (( $(echo "$UP_PROB > $DECISION" | bc -l) )); then
    TARGET_URL=$DAS_APIUP
  else
    TARGET_URL=$DAS_APIHOLD
  fi
  curl -H "Content-Type: application/json" -d "{\"up\":\"$UP_PROB\",\"down\":\"$DOWN_PROB\"}" $TARGET_URL
  OLD_TRANSMIT=$TRANSMIT
  OLD_RECEIVE=$RECEIVE
done
