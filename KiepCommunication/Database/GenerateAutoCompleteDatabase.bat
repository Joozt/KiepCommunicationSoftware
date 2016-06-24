@ECHO OFF
osql -S S071241\SQLEXPRESS -d AUTOCOMPLETE -E -i C:\spraakprogramma\SimpleTalk\Database\GenerateAutoCompleteDatabase.sql
ECHO Het bestand is uitgevoerd. 
ECHO -
PAUSE