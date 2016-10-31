#!/bin/bash
rm out.*
rm log.*
rm err.*
echo "log files deleted"
echo "submitting to condor..."
condor_submit submit
echo "submission to condor complete"
