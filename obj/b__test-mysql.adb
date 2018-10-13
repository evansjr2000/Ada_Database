pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__test-mysql.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__test-mysql.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E084 : Short_Integer; pragma Import (Ada, E084, "system__os_lib_E");
   E022 : Short_Integer; pragma Import (Ada, E022, "system__soft_links_E");
   E034 : Short_Integer; pragma Import (Ada, E034, "system__exception_table_E");
   E079 : Short_Integer; pragma Import (Ada, E079, "ada__io_exceptions_E");
   E064 : Short_Integer; pragma Import (Ada, E064, "ada__strings_E");
   E049 : Short_Integer; pragma Import (Ada, E049, "ada__containers_E");
   E036 : Short_Integer; pragma Import (Ada, E036, "system__exceptions_E");
   E066 : Short_Integer; pragma Import (Ada, E066, "ada__strings__maps_E");
   E070 : Short_Integer; pragma Import (Ada, E070, "ada__strings__maps__constants_E");
   E054 : Short_Integer; pragma Import (Ada, E054, "interfaces__c_E");
   E030 : Short_Integer; pragma Import (Ada, E030, "system__soft_links__initialize_E");
   E090 : Short_Integer; pragma Import (Ada, E090, "system__object_reader_E");
   E059 : Short_Integer; pragma Import (Ada, E059, "system__dwarf_lines_E");
   E048 : Short_Integer; pragma Import (Ada, E048, "system__traceback__symbolic_E");
   E110 : Short_Integer; pragma Import (Ada, E110, "ada__tags_E");
   E131 : Short_Integer; pragma Import (Ada, E131, "ada__streams_E");
   E152 : Short_Integer; pragma Import (Ada, E152, "interfaces__c__strings_E");
   E150 : Short_Integer; pragma Import (Ada, E150, "system__file_control_block_E");
   E133 : Short_Integer; pragma Import (Ada, E133, "system__finalization_root_E");
   E129 : Short_Integer; pragma Import (Ada, E129, "ada__finalization_E");
   E149 : Short_Integer; pragma Import (Ada, E149, "system__file_io_E");
   E164 : Short_Integer; pragma Import (Ada, E164, "ada__streams__stream_io_E");
   E135 : Short_Integer; pragma Import (Ada, E135, "system__storage_pools_E");
   E126 : Short_Integer; pragma Import (Ada, E126, "system__finalization_masters_E");
   E124 : Short_Integer; pragma Import (Ada, E124, "system__storage_pools__subpools_E");
   E116 : Short_Integer; pragma Import (Ada, E116, "ada__strings__unbounded_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "ada__calendar_E");
   E184 : Short_Integer; pragma Import (Ada, E184, "ada__calendar__time_zones_E");
   E145 : Short_Integer; pragma Import (Ada, E145, "ada__text_io_E");
   E176 : Short_Integer; pragma Import (Ada, E176, "system__pool_global_E");
   E010 : Short_Integer; pragma Import (Ada, E010, "apq_helper_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "apq_E");
   E004 : Short_Integer; pragma Import (Ada, E004, "apq__mysql_E");
   E003 : Short_Integer; pragma Import (Ada, E003, "apq__mysql__client_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "apq__samples_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E003 := E003 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "apq__mysql__client__finalize_spec");
      begin
         if E003 = 0 then
            F1;
         end if;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "apq__mysql__finalize_spec");
      begin
         E004 := E004 - 1;
         if E004 = 0 then
            F2;
         end if;
      end;
      E008 := E008 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "apq__finalize_spec");
      begin
         if E008 = 0 then
            F3;
         end if;
      end;
      E176 := E176 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "system__pool_global__finalize_spec");
      begin
         if E176 = 0 then
            F4;
         end if;
      end;
      E145 := E145 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "ada__text_io__finalize_spec");
      begin
         if E145 = 0 then
            F5;
         end if;
      end;
      E116 := E116 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "ada__strings__unbounded__finalize_spec");
      begin
         if E116 = 0 then
            F6;
         end if;
      end;
      E124 := E124 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "system__storage_pools__subpools__finalize_spec");
      begin
         if E124 = 0 then
            F7;
         end if;
      end;
      E126 := E126 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "system__finalization_masters__finalize_spec");
      begin
         if E126 = 0 then
            F8;
         end if;
      end;
      E164 := E164 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "ada__streams__stream_io__finalize_spec");
      begin
         if E164 = 0 then
            F9;
         end if;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "system__file_io__finalize_body");
      begin
         E149 := E149 - 1;
         if E149 = 0 then
            F10;
         end if;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      if E022 = 0 then
         System.Soft_Links'Elab_Spec;
      end if;
      if E034 = 0 then
         System.Exception_Table'Elab_Body;
      end if;
      E034 := E034 + 1;
      if E079 = 0 then
         Ada.Io_Exceptions'Elab_Spec;
      end if;
      E079 := E079 + 1;
      if E064 = 0 then
         Ada.Strings'Elab_Spec;
      end if;
      E064 := E064 + 1;
      if E049 = 0 then
         Ada.Containers'Elab_Spec;
      end if;
      E049 := E049 + 1;
      if E036 = 0 then
         System.Exceptions'Elab_Spec;
      end if;
      E036 := E036 + 1;
      if E084 = 0 then
         System.Os_Lib'Elab_Body;
      end if;
      E084 := E084 + 1;
      if E066 = 0 then
         Ada.Strings.Maps'Elab_Spec;
      end if;
      if E070 = 0 then
         Ada.Strings.Maps.Constants'Elab_Spec;
      end if;
      E070 := E070 + 1;
      if E054 = 0 then
         Interfaces.C'Elab_Spec;
      end if;
      if E030 = 0 then
         System.Soft_Links.Initialize'Elab_Body;
      end if;
      E030 := E030 + 1;
      E022 := E022 + 1;
      E066 := E066 + 1;
      E054 := E054 + 1;
      if E090 = 0 then
         System.Object_Reader'Elab_Spec;
      end if;
      if E059 = 0 then
         System.Dwarf_Lines'Elab_Spec;
      end if;
      E059 := E059 + 1;
      if E048 = 0 then
         System.Traceback.Symbolic'Elab_Body;
      end if;
      E048 := E048 + 1;
      E090 := E090 + 1;
      if E110 = 0 then
         Ada.Tags'Elab_Spec;
      end if;
      if E110 = 0 then
         Ada.Tags'Elab_Body;
      end if;
      E110 := E110 + 1;
      if E131 = 0 then
         Ada.Streams'Elab_Spec;
      end if;
      E131 := E131 + 1;
      if E152 = 0 then
         Interfaces.C.Strings'Elab_Spec;
      end if;
      E152 := E152 + 1;
      if E150 = 0 then
         System.File_Control_Block'Elab_Spec;
      end if;
      E150 := E150 + 1;
      if E133 = 0 then
         System.Finalization_Root'Elab_Spec;
      end if;
      E133 := E133 + 1;
      if E129 = 0 then
         Ada.Finalization'Elab_Spec;
      end if;
      E129 := E129 + 1;
      if E149 = 0 then
         System.File_Io'Elab_Body;
      end if;
      E149 := E149 + 1;
      if E164 = 0 then
         Ada.Streams.Stream_Io'Elab_Spec;
      end if;
      E164 := E164 + 1;
      if E135 = 0 then
         System.Storage_Pools'Elab_Spec;
      end if;
      E135 := E135 + 1;
      if E126 = 0 then
         System.Finalization_Masters'Elab_Spec;
      end if;
      if E126 = 0 then
         System.Finalization_Masters'Elab_Body;
      end if;
      E126 := E126 + 1;
      if E124 = 0 then
         System.Storage_Pools.Subpools'Elab_Spec;
      end if;
      E124 := E124 + 1;
      if E116 = 0 then
         Ada.Strings.Unbounded'Elab_Spec;
      end if;
      E116 := E116 + 1;
      if E015 = 0 then
         Ada.Calendar'Elab_Spec;
      end if;
      if E015 = 0 then
         Ada.Calendar'Elab_Body;
      end if;
      E015 := E015 + 1;
      if E184 = 0 then
         Ada.Calendar.Time_Zones'Elab_Spec;
      end if;
      E184 := E184 + 1;
      if E145 = 0 then
         Ada.Text_Io'Elab_Spec;
      end if;
      if E145 = 0 then
         Ada.Text_Io'Elab_Body;
      end if;
      E145 := E145 + 1;
      if E176 = 0 then
         System.Pool_Global'Elab_Spec;
      end if;
      E176 := E176 + 1;
      E010 := E010 + 1;
      if E008 = 0 then
         APQ'ELAB_SPEC;
      end if;
      if E008 = 0 then
         APQ'ELAB_BODY;
      end if;
      E008 := E008 + 1;
      if E004 = 0 then
         APQ.MYSQL'ELAB_SPEC;
      end if;
      E004 := E004 + 1;
      if E003 = 0 then
         APQ.MYSQL.CLIENT'ELAB_SPEC;
      end if;
      if E003 = 0 then
         APQ.MYSQL.CLIENT'ELAB_BODY;
      end if;
      E003 := E003 + 1;
      E006 := E006 + 1;
   end adainit;

--  BEGIN Object file/option list
   --   /home/evansjr/notes/code/database/obj/apq_helper.o
   --   /home/evansjr/notes/code/database/obj/apq.o
   --   /home/evansjr/notes/code/database/obj/apq-mysql.o
   --   /home/evansjr/notes/code/database/obj/apq-mysql-client.o
   --   /home/evansjr/notes/code/database/obj/apq-samples.o
   --   /home/evansjr/notes/code/database/obj/add_user.o
   --   -L/home/evansjr/notes/code/database/obj/
   --   -L/home/evansjr/notes/code/database/obj/
   --   -L/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/adalib/
   --   -static
   --   -L/usr/lib64/mysql
   --   -lmysqlclient
   --   -lpthread
   --   -lz
   --   -lm
   --   -ldl
   --   -lssl
   --   -lcrypto
   --   -lgnat
   --   -ldl
--  END Object file/option list   

end ada_main;
