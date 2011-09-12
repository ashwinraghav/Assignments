NODES=8

if [ "$PBS_ENVIRONMENT" != "" ] ; then
    echo Ack!  Launched via a qsub -- not continuing
    exit
fi

rm -rf output-*

for (( i = 1; i <= $NODES; i++ )); do
	#echo "Creating job for node $i..."
	PBS_DIR="doutput-$i"
	PBS_FILE="$PBS_DIR/job-$i.pbs"
	#PBS_FILE="job-$i.pbs"
	`mkdir $PBS_DIR`
	`touch $PBS_FILE`
	echo "#!/bin/sh" >  $PBS_FILE
	echo "#PBS -l nodes=1:ppn=1" >>  $PBS_FILE
	echo "#PBS -l walltime=12:00:00" >>  $PBS_FILE
	echo "#PBS -o output.txt" >>  $PBS_FILE
	echo "#PBS -j oe" >>  $PBS_FILE
	echo "#PBS -m ea" >>  $PBS_FILE
	echo "#PBS -M am2qa@virginia.edu" >>  $PBS_FILE
	echo "cd \$PBS_O_WORKDIR" >>  $PBS_FILE
done

for (( i = 1; i <= 250; i++ )); do
	PBS_DIR="doutput-$(($i % $NODES + 1))"
	PBS_FILE="$PBS_DIR/job-$(($i % $NODES + 1)).pbs"
	echo "/usr/bin/blender -b ../Star-collapse-ntsc.blend -s $i -e $i -a" >> $PBS_FILE
done

for (( i = 1; i <= $NODES; i++ )); do
	PBS_DIR="doutput-$i"
	PBS_FILE="job-$i.pbs"
	#PBS_FILE="job-$i.pbs"
	cd $PBS_DIR
	qsub $PBS_FILE
	cd ..
done

FRAME_COUNT=$((250/$NODES))

START=$(date +%s)
for (( i = 1; i <= $NODES; i++ )); do
	while true; do
		DONE_COUNT=`ls doutput-$i/star-collapse-* 2> NUL |wc -l`
		if (($DONE_COUNT >= $FRAME_COUNT)); then echo "DONE!!"; break; fi
		#if diff -b -w test_result test_answer > NUL ; then echo "*** done" ; break; fi 
	done
done
END=$(date +%s)
DIFF=$(( $END - $START ))
echo "It took $DIFF seconds wall time"
# note that there is another format for for loops in bash:
#
# for i in cheese apple crackers
# do
#   echo $i
# done

