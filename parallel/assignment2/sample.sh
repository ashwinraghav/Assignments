#created by ASHWIN RAGHAV MOHAN GANESH
#am2qa

if [ "$PBS_ENVIRONMENT" != "" ] ; then
  echo Ack!  Launched via a qsub -- not continuing
  exit
fi

#clean up previously created files
NODES=8

#Start the timer
START=$(date +%s)

FRAME_COUNT=250
rm -rf output-*
rm star-*
rm job-*

for (( i = 1; i <= $NODES; i++ )); do
   PBS_FILE="job-$i.pbs"
   `touch $PBS_FILE`
   echo "#!/bin/sh" >  $PBS_FILE
   echo "#PBS -l nodes=1:ppn=1" >>  $PBS_FILE

   #The walltime is inaccurately estimated
   echo "#PBS -l walltime=12:00:00" >>  $PBS_FILE
	
   echo "#PBS -o output.txt" >>  $PBS_FILE
   echo "#PBS -j oe" >>  $PBS_FILE
   echo "#PBS -m ea" >>  $PBS_FILE
   echo "#PBS -M am2qa@virginia.edu" >>  $PBS_FILE
	
   echo "cd \$PBS_O_WORKDIR" >>  $PBS_FILE
 
   #Copy the blender binaries and the .blend file into the node to remove chat acros the network
   echo "cp /usr/bin/blender \$TMPDIR" >> $PBS_FILE
   echo "cp Star-collapse-ntsc.blend \$TMPDIR" >> $PBS_FILE
done

X=0 # X is the front index
Y=0 # Y is the rear index

for (( i = 1; i <= $FRAME_COUNT; i++ )); do
   #alternating put the frames at the beginning and end of the shrinking window
   if ((($i%2) == 0));then
     #put the frame in the beginning 
     PBS_FILE="job-$(($X+1)).pbs";
     X=$((($X+1)%$NODES));
   else
     #put the frame to the end
     PBS_FILE="job-$(($NODES-$Y)).pbs";
     Y=$((($Y+1)%$NODES));
   fi
  
   #one frame generated per blender invocation
   #node that the invovation is not from /usr/bin but from the $TMPDIR directory
   echo "\$TMPDIR/blender -b \$TMPDIR/Star-collapse-ntsc.blend -s $i -e $i -a" >> $PBS_FILE
done

#queue up all files in order
for (( i = 1; i <= $NODES; i++ )); do
   PBS_FILE="job-$i.pbs"
   qsub $PBS_FILE
done


#wait till all files are on the local file-system
while true; do
   DONE_COUNT=`ls star-collapse-* 2> NUL |wc -l`
   if (($DONE_COUNT = $FRAME_COUNT)); then echo "DONE!!"; break; fi
done


#Measure the total time and print
END=$(date +%s)
echo "It took $(($END - $START)) seconds wall time"
