------------------------------------------------------------------------------
--                                                                          --
--                          APQ DATABASE BINDINGS                           --
--                                                                          --
--                              A P Q - MYSQL  				    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2007, Warren W. Gay VE3WWG                    --
--         Copyright (C) 2007-2011, KOW Framework Project                   --
--                                                                          --
--                                                                          --
-- APQ is free software;  you can  redistribute it  and/or modify it under  --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  APQ is distributed in the hope that it will be useful, but WITH-  --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with APQ;  see file COPYING.  If not, write  --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
------------------------------------------------------------------------------


-- **********************************************************
-- WARNING: The APQ.MySQL package is generated. Apply changes
--          to input file apq-mysql.ads-in instead!!!!!!!!!!!
-- **********************************************************

with Ada.Unchecked_Conversion;
with Interfaces.c.Strings;

package APQ.MySQL is

	-- %mysql_linker_options%
	-- NOTE :: since 3.2 the linker options are in the gpr file



	pragma Linker_Options( "-L/usr/lib64/mysql" );
	pragma Linker_Options( "-lmysqlclient" );
	pragma Linker_Options( "-lpthread" );
	pragma Linker_Options( "-lz" );
	pragma Linker_Options( "-lm" );
	pragma Linker_Options( "-ldl" );
	pragma Linker_Options( "-lssl" );
	pragma Linker_Options( "-lcrypto" ); 

	----------------
	-- Field Type --
	----------------

	type Field_Type is (
		MYSQL_TYPE_DECIMAL,
		MYSQL_TYPE_TINY,
		MYSQL_TYPE_SHORT,
		MYSQL_TYPE_LONG,
		MYSQL_TYPE_FLOAT,
		MYSQL_TYPE_DOUBLE,
		MYSQL_TYPE_NULL,
		MYSQL_TYPE_TIMESTAMP,
		MYSQL_TYPE_LONGLONG,
		MYSQL_TYPE_INT24,
		MYSQL_TYPE_DATE,
		MYSQL_TYPE_TIME,
		MYSQL_TYPE_DATETIME,
		MYSQL_TYPE_YEAR,
		MYSQL_TYPE_NEWDATE,
		MYSQL_TYPE_VARCHAR,
		MYSQL_TYPE_BIT,
		MYSQL_TYPE_NEWDECIMAL,
		MYSQL_TYPE_ENUM,
		MYSQL_TYPE_SET,
		MYSQL_TYPE_TINY_BLOB,
		MYSQL_TYPE_MEDIUM_BLOB,
		MYSQL_TYPE_LONG_BLOB,
		MYSQL_TYPE_BLOB,
		MYSQL_TYPE_VAR_STRING,
		MYSQL_TYPE_STRING,
		MYSQL_TYPE_GEOMETRY 
		);
	for Field_Type use (
		MYSQL_TYPE_DECIMAL	=> 0,
		MYSQL_TYPE_TINY	=> 1,
		MYSQL_TYPE_SHORT	=> 2,
		MYSQL_TYPE_LONG	=> 3,
		MYSQL_TYPE_FLOAT	=> 4,
		MYSQL_TYPE_DOUBLE	=> 5,
		MYSQL_TYPE_NULL	=> 6,
		MYSQL_TYPE_TIMESTAMP	=> 7,
		MYSQL_TYPE_LONGLONG	=> 8,
		MYSQL_TYPE_INT24	=> 9,
		MYSQL_TYPE_DATE	=> 10,
		MYSQL_TYPE_TIME	=> 11,
		MYSQL_TYPE_DATETIME	=> 12,
		MYSQL_TYPE_YEAR	=> 13,
		MYSQL_TYPE_NEWDATE	=> 14,
		MYSQL_TYPE_VARCHAR	=> 15,
		MYSQL_TYPE_BIT	=> 16,
		MYSQL_TYPE_NEWDECIMAL	=> 246,
		MYSQL_TYPE_ENUM	=> 247,
		MYSQL_TYPE_SET	=> 248,
		MYSQL_TYPE_TINY_BLOB	=> 249,
		MYSQL_TYPE_MEDIUM_BLOB	=> 250,
		MYSQL_TYPE_LONG_BLOB	=> 251,
		MYSQL_TYPE_BLOB	=> 252,
		MYSQL_TYPE_VAR_STRING	=> 253,
		MYSQL_TYPE_STRING	=> 254,
		MYSQL_TYPE_GEOMETRY	=> 255 
		);
	for Field_Type'Size use 32;


	-----------------
	-- Result Type --
	-----------------

	type Result_Type is (
		CR_NO_ERROR 
		);

	for Result_Type use (
		CR_NO_ERROR	=> 0 
		);
	for Result_Type'Size use 32;



	-----------------------------
	-- MySQL Connection Option --
	-----------------------------
	type MySQL_Option_Type is (
		none 
		);

	for MySQL_Option_Type use (
		none	=> 3105 
		);

	for  MySQL_Option_Type'Size use 32;

	type MySQL_Enum_Option is mod 2**32;


	-- FIXME: Several names in here are not how they should be!

	function toUnsigned is new ada.Unchecked_Conversion(Source => MySQL_Option_Type , Target => MySQL_Enum_Option  );
	function To_Mysql_Enum_option is new ada.Unchecked_Conversion(Source => MySQL_Option_Type , Target => MySQL_Enum_Option  );

	type nature_enum_type is (
			nat_ptr_char,
			nat_ptr_ui,
			nat_uint,
			nat_not_used,
			nat_ptr_my_bool,
			none
		);
	type root_option_enum is ( common, ssl, none );
	type ssl_enum is ( key, cert, ca, capath, cipher , none );
	type root_option_record is private;

private

	type root_option_record is tagged record
		especie      : root_option_enum := none;
		is_valid     : boolean := false;
		value_nature : nature_enum_type := none;
		--
		key_ssl : ssl_enum := none;
		key_common : MySQL_Option_Type := none;
		--
		value_s : ada.Strings.Unbounded.Unbounded_String := ada.Strings.Unbounded.To_Unbounded_String("");
		value_u : Unsigned_Integer := 0 ;
		value_b : boolean := false;
	end record;

	type Return_Status is range -2 ** 31 .. 2 ** 31 - 1;
	for Return_Status'Size use 32;

	type MySQL_Oid_Type is mod 2 ** 64;
	for MySQL_Oid_Type'Size use 64;

	type u_long is mod 2 ** 32;
	for u_long'size use 32;

	Null_Row_ID : constant Row_ID_Type := 0;

	type MYSQL       is new System.Address;   -- Connection
	type MYSQL_RES   is new System.Address;   -- Result
	type MYSQL_ROW   is new System.Address;   -- Row
	type MYSQL_FIELD is new System.Address;   -- Field

	Null_Connection : constant MYSQL := MYSQL(System.Null_Address);
	Null_Result :     constant MYSQL_RES := MYSQL_RES(System.Null_Address);
	Null_Row :        constant MYSQL_ROW := MYSQL_ROW(System.Null_Address);
	Null_Field :      constant MYSQL_FIELD := MYSQL_FIELD(System.Null_Address);

	type MYSQL_ROW_NO is mod 2 ** 64; -- find out if it's necessary to query this information from the installed mysql

end APQ.MySQL;

