#!/bin/bash -x
gnatmake -g add_user
gnatlink -g add_user.ali ./c_mysql.o
