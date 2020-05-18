#!/bin/bash -l

cd /home/salute/seasonal/ens_procedure

pwd
day=86400
diff=$(($(date +%s) - $(date +%s -r /home/salute/seasonal/PESI-san9.csv)))
diff_1=$(($(date +%s) - $(date +%s -r /home/salute/seasonal/PESI-pct9.csv)))

if [ $diff -gt $day ]
then
	echo "file input gianni vecchio"
	exit
fi
if [ $diff_1 -gt $day ]
then
	echo "file input gianni vecchio"
	exit
fi

/usr/bin/Rscript create_forecast_temp_bayes.r
/usr/bin/Rscript create_forecast_rain_bayes.r
/usr/bin/Rscript create_forecast_temp_bayes_v20_clim.r
/usr/bin/Rscript create_forecast_rain_bayes_v20_clim.r
/usr/bin/Rscript create_forecast_temp_bayes_v20_9818.r
/usr/bin/Rscript create_forecast_rain_bayes_v20_9818.r

echo "Procedura per il calcolo degli ensemble che produce netcdf"





