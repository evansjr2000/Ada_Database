#include <mysql/mysql.h>
#include <stdio.h>

int main() 
{

MYSQL mysql;

MYSQL *w;

w = mysql_init( &mysql );
mysql_options(&mysql,MYSQL_READ_DEFAULT_GROUP,"test-mysql");
if (!mysql_real_connect(&mysql,"localhost","root","go.navy","CLASS2019",0,NULL,0))
  {
    fprintf(stderr, "Failed to connect to database: Error: %s\n",
	    mysql_error(&mysql));
  }
}
