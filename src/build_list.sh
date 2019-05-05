#!/bin/bash -x
gnatmake list_users
gnatlink list_users.ali ./c_mysql.o `mariadb_config --libs`
