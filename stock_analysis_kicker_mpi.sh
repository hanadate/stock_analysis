nohup mpirun -hostfile /home/shuhei/mpi/stock_analysis/src/hosts_96 -n 96 R --slave -f /home/shuhei/mpi/stock_analysis/src/run_by_mpi.R > /home/shuhei/mpi/stock_analysis/doc/log.txt 2>&1 &
