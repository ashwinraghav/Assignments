#!/bin/bash
#
# This is a sample script that will create 8 other jobs (that really
# don't do much), and send them through qsub

NODES=8

if [ "$PBS_ENVIRONMENT" != "" ] ; then
    echo Ack!  Launched via a qsub -- not continuing
    exit
fi

for (( i = 1; i <= $NODES; i++ )); do
    echo "Creating job for node $i..."
    echo "#!/bin/bash" > job-$i.pbs
    echo "#PBS -l nodes=1" >> job-$i.pbs
    echo "#PBS -l walltime=0:05:00" >> job-$i.pbs
    echo "hostname" >> job-$i.pbs
    qsub job-$i.pbs
    /bin/rm -f job-$i.pbs
done


#
# note that there is another format for for loops in bash:
#
# for i in cheese apple crackers
# do
#   echo $i
# done

