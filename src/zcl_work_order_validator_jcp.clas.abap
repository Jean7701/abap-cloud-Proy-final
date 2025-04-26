CLASS zcl_work_order_validator_jcp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
*    INTERFACES if_oo_adt_classrun.

    METHODS:
     validate_create_order  IMPORTING iv_customer_id    TYPE string
                                      iv_technician_id  TYPE string
                                      iv_priority       TYPE string
                            RETURNING VALUE(rv_valid)   TYPE abap_bool,

      validate_update_order IMPORTING iv_work_order_id TYPE string
                                      iv_status        TYPE string
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_delete_order IMPORTING iv_work_order_id TYPE string
                                      iv_status        TYPE string
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_status_and_priority IMPORTING iv_status       TYPE string
                                             iv_priority     TYPE string
                                   RETURNING VALUE(rv_valid) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: c_valid_status   TYPE RANGE OF ztstatus_jcp-status_code.
    DATA: c_valid_priority TYPE RANGE OF ztpriority_jcp-priority_code.

    METHODS check_customer_exists   IMPORTING iv_customer_id   TYPE string
                                    RETURNING VALUE(rv_exists) TYPE abap_bool.
    METHODS check_technician_exists IMPORTING iv_technician_id TYPE string
                                    RETURNING VALUE(r_result)  TYPE abap_bool.
    METHODS check_order_exists      IMPORTING iv_work_order_id TYPE string
                                    RETURNING VALUE(r_result)  TYPE abap_bool.
    METHODS check_order_history     IMPORTING iv_work_order_id TYPE string
                                    RETURNING VALUE(r_result)  TYPE abap_bool.
ENDCLASS.

CLASS zcl_work_order_validator_jcp IMPLEMENTATION.

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  METHOD validate_create_order.

    " Check if customer exists
    DATA(lv_customer_exists) = check_customer_exists( iv_customer_id ).
    IF lv_customer_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if technician exists
    DATA(lv_technician_exists) = check_technician_exists( iv_technician_id ).
    IF lv_technician_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    IF iv_priority NOT IN c_valid_priority.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.


  METHOD check_customer_exists.
    DATA: lv_customer_id TYPE n LENGTH 8.

    SELECT SINGLE customer_id
    FROM ztcustomer_jcp
    WHERE customer_id EQ @iv_customer_id
       INTO @lv_customer_id .
  ENDMETHOD.


  METHOD check_technician_exists.
    DATA: lv_technician_id TYPE c LENGTH 8.

    SELECT SINGLE technician_id
     FROM zttechnician_jcp
     WHERE technician_id EQ @iv_technician_id
     INTO @lv_technician_id .
  ENDMETHOD.

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  METHOD validate_update_order.
    " Check if the work order exists
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order status is editable (e.g., Pending)
    IF iv_status NOT IN c_valid_status.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.


  METHOD check_order_exists.
    DATA: lv_work_order_id  TYPE n LENGTH 10.

    SELECT SINGLE work_order_id
     FROM ztwork_order_jcp
     WHERE work_order_id EQ @iv_work_order_id
     INTO @lv_work_order_id .

  ENDMETHOD.

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  METHOD validate_delete_order.
    " Check if the order exists
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order status is "PE" (Pending)
    IF iv_status NE 'PE'.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order has a history (i.e., if it has been modified before)
    DATA(lv_has_history) = check_order_history( iv_work_order_id ).
    IF lv_has_history IS NOT INITIAL.
      .       rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.

  ENDMETHOD.

  METHOD check_order_history.
    DATA: lv_work_order_id TYPE n LENGTH 12.

    SELECT SINGLE work_order_id
     FROM ztwrkordhist_jcp
     WHERE work_order_id EQ @iv_work_order_id
     INTO @lv_work_order_id .

  ENDMETHOD.

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  METHOD validate_status_and_priority.

  data: lt_priority type standard table of zpriority_jcp.
  data: lt_status   type standard table of ztstatus_jcp.

  select priority_code, priority_description
  from zpriority_jcp
  into table @lt_priority.

  loop at lt_priority assigning FIELD-SYMBOL(<fs_priority>).
       c_valid_priority = VALUE #(
        ( sign = 'I' option = 'EQ' low = <fs_priority>-priority_code )
      ) .
  endloop.

 select status_code, status_description
  from ztstatus_jcp
  into table @lt_status.

Loop at lt_status assigning FIELD-SYMBOL(<fs_status>).
    c_valid_status = VALUE #(
         ( sign = 'I' option = 'EQ' low = <fs_status>-status_code )
       ) .
endloop.

    " Validate the status value
    IF iv_status NOT IN c_valid_status.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Validate the priority value
    IF iv_priority NOT IN c_valid_priority.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.

  ENDMETHOD.

ENDCLASS.
