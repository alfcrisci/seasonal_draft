#!/bin/bash -l
############################################################################
#
# Project : WRF forecast system
# Author  : Gianni Messeri (LaMMA Regione Toscana)
# Date    : Thu Jun 20 07:03:07 GMT 2015
# RCS     : $Id$
#
############################################################################
############################################################################
export LANGUAGE="it_IT:en_GB:en"


echo "procedura x previsioni stagional con WT osservati" 

cd /home/salute/seasonal/ens_procedure


giorno=`date +%d --date='1 month ago'`
mese=`date +%-m --date='1 month ago'`
MESE=`date +%B --date='1 month ago'`
anno=`date +%Y --date='1 month ago'`

giorno=`date +%d`
mese=`date +%-m`
MESE=`date +%B`
anno=`date +%Y`

echo $giorno $mese $anno $MESE

NOME_PCT="OBS"$MESE"_"$anno"PCT9.txt"
NOME_SAN="OBS"$MESE"_"$anno"SAN9.txt"
NOMEb_PCT="/home/salute/seasonal/PESIOBS"$MESE"_"$anno"PCT9.txt"
NOMEb_SAN="/home/salute/seasonal/PESIOBS"$MESE"_"$anno"SAN9.txt"


cp /home/salute/seasonal/$NOME_PCT /home/salute/seasonal/tests/
cp /home/salute/seasonal/$NOME_SAN /home/salute/seasonal/tests/

echo $NOME_PCT $NOME_SAN


echo "Rscript create_verification.r $NOME_PCT $NOME_SAN $anno $mese"
# Rscript create_verification.r $NOME_PCT $NOME_SAN $anno $mese

echo "ECCO ANCHE LA PROCEDURA BAYESIANA"

pwd

Rscript create_obs_temp_bayes.r $NOMEb_SAN $anno $mese
Rscript create_obs_rain_bayes.r $NOMEb_PCT $anno $mese


exit







