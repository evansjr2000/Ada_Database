#!/bin/bash -x
#gnatmake -g add_user
#gnatlink -g add_user.ali ./c_mysql.o
gcc -L/usr/lib64/mysql -o test-mysql test-mysql.c  /usr/lib64/mysql/libmysqlclient.so
