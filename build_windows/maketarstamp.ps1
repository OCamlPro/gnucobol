$date=get-date
$date=$date.ToUniversalTime()
'#define COB_TAR_DATE    "' + '{0:MMM dd yyyy HH:mm:ss UTC}' -f $date + '"'
'#define COB_NUM_TAR_DATE ' + '{0:yyyyMMdd}' -f $date
'#define COB_NUM_TAR_TIME ' + '{0:HHmmss}' -f $date
