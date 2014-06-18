rm xsql-image*
ml-build sources.cm Xsql.main xsql-image > log.txt
sml @SMLload xsql-image* $1
