#!/bin/bash -x
gnatmake add_user
gnatlink add_user.ali ./c_mysql.o
