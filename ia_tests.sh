#!/bin/bash
# --------------------------------------------------
#
#       Title: ia_tests.sh
# Description: Script Testes Projecto IA
#      Author: Ricardo Sequeira
#
#        Name: ia_tests
#        File: ia_tests.sh
#     Created: November 06, 2013
#       Usage: ./ia_tests.sh
#
# --------------------------------------------------

# Directorio onde se encontram os testes
TESTSDIR='/Users/Beatriz/Downloads/testes-entrega1'

# Directorio base do projecto
PROJDIR='/Users/Beatriz/Downloads'

PROJFILE='1Entrega.lisp'

TEMPDIR='/tmp'

SUCCESS=0
FAIL=0


cd $PROJDIR

for file in $TESTSDIR/*
do
    filename=$(basename $file)
    filename=${filename%%.*}
    

    echo "====================$filename===================="
    clisp -i $PROJFILE --quiet --silent < $TESTSDIR/$filename/input > $TEMPDIR/output_tmp
    if diff -w $TESTSDIR/$filename/output $TEMPDIR/output_tmp; then
      echo "TEST PASSED!"
      SUCCESS=$[SUCCESS+1]
      STRRESULTS="$STRRESULTS$filename - OK\n"
    else
      echo "output differs from expected"
      FAIL=$[FAIL+1]
      STRRESULTS="$STRRESULTS$filename - FAIL\n"
    fi
done
echo ""
echo "=====================TEST RESULTS====================="
echo -e $STRRESULTS
echo "SUCCESS: $SUCCESS"
echo "FAIL: $FAIL"