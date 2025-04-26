CLASS zcl_llena_catalogos DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  interfaces if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_llena_catalogos IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

  data: lt_priority type standard table of zpriority_jcp.
  data: lt_status   type standard table of ztstatus_jcp .

  lt_priority = VALUE #(
           ( priority_code = 'A'  priority_description = 'High' )
           ( priority_code = 'B'  priority_description = 'Low' )
       ) .

lt_status = VALUE #(
           ( status_code = 'PE'  status_description = 'Pending' )
           ( status_code = 'CO'  status_description = 'Completed' )
       ) .

*  Modify zpriority_jcp from table @lt_priority.
*  Modify ztstatus_jcp from table @lt_status.

  ENDMETHOD.

ENDCLASS.
