NODES=8

if [ "$PBS_ENVIRONMENT" != "" ] ; then
    echo Ack!  Launched via a qsub -- not continuing
    exit
fi

rm -rf output-*
rm star-*
rm job-*
for (( i = 1; i <= $NODES; i++ )); do
	PBS_FILE="job-$i.pbs"
	`touch $PBS_FILE`
	echo "#!/bin/sh" >  $PBS_FILE
	echo "#PBS -l nodes=1:ppn=1" >>  $PBS_FILE
	echo "#PBS -l walltime=12:00:00" >>  $PBS_FILE
	echo "#PBS -o output.txt" >>  $PBS_FILE
	echo "#PBS -j oe" >>  $PBS_FILE
	echo "#PBS -m ea" >>  $PBS_FILE
	echo "#PBS -M am2qa@virginia.edu" >>  $PBS_FILE
	echo "cd \$PBS_O_WORKDIR" >>  $PBS_FILE
	echo "cp /usr/bin/blender \$TMPDIR" >> $PBS_FILE
	echo "cp Star-collapse-ntsc.blend \$TMPDIR" >> $PBS_FILE
done
X=1
Y=0
for (( i = 1; i <= 250; i++ )); do
	if ((($i%2) == 0));then 
		PBS_FILE="job-$X.pbs";
		X=$((($X+1)%$NODES));
	else 
		PBS_FILE="job-$(($NODES-$Y)).pbs";
		Y=$((($Y+1)%$NODES));
	fi
	echo $PBS_FILE
	echo "\$TMPDIR/blender -b \$TMPDIR/Star-collapse-ntsc.blend -s $i -e $i" >> $PBS_FILE
done

for (( i = 1; i <= $NODES; i++ )); do
	PBS_FILE="job-$i.pbs"
	qsub $PBS_FILE
done

FRAME_COUNT=250

START=$(date +%s)
	while true; do
		DONE_COUNT=`ls star-collapse-* 2> NUL |wc -l`
		if (($DONE_COUNT >= $FRAME_COUNT)); then echo "DONE!!"; break; fi
	done
END=$(date +%s)
DIFF=$(( $END - $START ))
echo "It took $DIFF seconds wall time"

