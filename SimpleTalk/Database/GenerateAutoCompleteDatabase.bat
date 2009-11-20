@ECHO OFF
osql -S. -dLOGIFIT -E -iGenerateAutoCompleteDatabase.sql
ECHO Het bestand is uitgevoerd. 
ECHO -
PAUSE