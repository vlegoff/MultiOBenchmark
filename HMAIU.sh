for q in normal xlarge xxlarge; do

echo "========================================================================"
echo "$q queue"
echo "========================================================================"

cputot=$(ccc_mpinfo \
    | grep "^$q" \
    | awk '{print $3-$4}' \
    | paste -s -d+ - \
    | bc)

EXP=$(ccc_mpp -u $USER -r -q $q \
    | awk '{print $4}' \
    | tail -n +2 \
    | head -n -1 \
    | paste -s -d+ -)
if [ -z $EXP ]; then
    EXP=0
fi
ncpus=$(echo $EXP | bc)
percent=$(echo "scale=1; 100 * $ncpus / $cputot" | bc -l)
echo "User $USER, you are using $ncpus cpus on $q queue." \
    "That is $percent % of $cputot total cpus"

EXP=$(ccc_mpp -u $USER -p -q $q \
    | awk '{print $4}' \
    | tail -n +2 \
    | head -n -1 \
    | paste -s -d+ -)
if [ -z $EXP ]; then
    EXP=0
fi
ncpus=$(echo $EXP | bc)
echo "User $USER, you have $ncpus cpus pending on $q"

EXP=$(ccc_mpp -r -q $q \
    | awk '{print $4}' \
    | tail -n +2 \
    | head -n -1 \
    | paste -s -d+ -)
if [ -z $EXP ]; then
    EXP=0
fi
ncpus=$(echo $EXP | bc)
percent=$(echo "scale=1; 100 * $ncpus / $cputot" | bc -l)
echo "$ncpus cpus are used on $q queue." \
    "That is $percent % of $cputot total cpus"

EXP=$(ccc_mpp -p -q $q \
    | awk '{print $4}' \
    | tail -n +2 \
    | head -n -1 \
    | paste -s -d+ -)
if [ -z $EXP ]; then
    EXP=0
fi
ncpus=$(echo $EXP | bc)
percent=$(echo "scale=1; 100 * $ncpus / $cputot" | bc -l)
echo "$ncpus cpus are pending on normal $q."

echo "  "

done
