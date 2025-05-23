CLASS zcl_work_order_validator_jcp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      validate_create_order  IMPORTING iv_customer_id   TYPE zecustomer_id
                                       iv_technician_id TYPE zetechn_id
                                       iv_status        TYPE zestatus_jcp
                                       iv_priority      TYPE zepriority_jcp
                             RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_update_order IMPORTING is_work_order   TYPE ztwork_order_jcp
                            EXPORTING ev_status       TYPE zestatus_jcp
                            RETURNING VALUE(rv_valid) TYPE abap_bool,

      validate_exist_hist_order IMPORTING is_histOrd        TYPE ztwrkordhist_jcp
                                RETURNING VALUE(rv_validoh) TYPE abap_bool,

      validate_delete_order IMPORTING is_work_order   TYPE ztwork_order_jcp
                                      is_histOrd      TYPE ztwrkordhist_jcp
                            EXPORTING ev_status       TYPE zestatus_jcp
                                      ev_priority     TYPE zepriority_jcp
                            RETURNING VALUE(rv_valid) TYPE abap_bool,

      validate_status_and_priority IMPORTING iv_status       TYPE zestatus_jcp
                                             iv_priority     TYPE zepriority_jcp
                                   RETURNING VALUE(rv_valid) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: r_valid_status      TYPE RANGE OF ztstatus_jcp-status_code.
    DATA: r_valid_priority    TYPE RANGE OF ztpriority_jcp-priority_code.
    DATA: r_valid_status_nu   TYPE RANGE OF ztstatus_jcp-status_code.
    DATA: r_valid_priority_nu TYPE RANGE OF ztpriority_jcp-priority_code.

    METHODS check_customer_exists   IMPORTING iv_customer_id   TYPE zecustomer_id  "string
                                    RETURNING VALUE(rv_valid)  TYPE abap_bool.

    METHODS check_order_exists      IMPORTING iv_work_order_id TYPE zewrkord_id
                                    RETURNING VALUE(r_result)  TYPE abap_bool.

    METHODS check_technician_exists IMPORTING iv_technician_id TYPE zetechn_id
                                    RETURNING VALUE(r_result)  TYPE abap_bool.

    METHODS check_order_history     IMPORTING is_histOrd       TYPE ztwrkordhist_jcp
                                    RETURNING VALUE(r_result)  TYPE abap_bool.
ENDCLASS.



CLASS zcl_work_order_validator_jcp IMPLEMENTATION.


  METHOD check_customer_exists.
    DATA: lv_customer_id TYPE n LENGTH 8.

    SELECT SINGLE customer_id
    FROM ztcustomer_jcp
    WHERE customer_id EQ @iv_customer_id
       INTO @lv_customer_id .
    IF sy-subrc EQ 0.
      rv_valid = abap_true.
    ELSE.
      rv_valid = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD check_order_exists.
    DATA: lv_work_order_id  TYPE n LENGTH 10.

    SELECT SINGLE work_order_id
     FROM ztwork_order_jcp
     WHERE work_order_id EQ @iv_work_order_id
     INTO @lv_work_order_id .
    IF sy-subrc EQ 0.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD check_order_history.
    DATA: lv_work_order_id TYPE n LENGTH 12.

    SELECT SINGLE work_order_id
     FROM ztwrkordhist_jcp
     WHERE history_id    EQ @is_histOrd-history_id
       AND work_order_id EQ @is_histOrd-work_order_id
     INTO @lv_work_order_id .
    IF sy-subrc EQ 0.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD check_technician_exists.
    DATA: lv_technician_id TYPE c LENGTH 8.

    SELECT SINGLE technician_id
     FROM zttechnician_jcp
     WHERE technician_id EQ @iv_technician_id
     INTO @lv_technician_id .
    IF sy-subrc EQ 0.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD validate_create_order.

    " Check if customer exists
    DATA(lv_customer_exists) = check_customer_exists( iv_customer_id ).
    rv_valid = lv_customer_exists.

    " Check if technician exists
    DATA(lv_technician_exists) = check_technician_exists( iv_technician_id ).
    rv_valid  = lv_technician_exists.

    DATA(rv_valido) = validate_status_and_priority( EXPORTING iv_status = iv_status
                                                              iv_priority  = iv_priority ).
    RETURN.

  ENDMETHOD.

  METHOD validate_exist_hist_order.
    DATA(lv_order_ohexist) = check_order_history( is_histOrd ).
    IF lv_order_ohexist = abap_true.
      rv_validoh = abap_true.
    ELSE.
      rv_validoh = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD validate_status_and_priority.

    DATA: lt_priority TYPE STANDARD TABLE OF zpriority_jcp.
    DATA: lt_status   TYPE STANDARD TABLE OF ztstatus_jcp.

    SELECT priority_code, priority_description
    FROM zpriority_jcp
    INTO TABLE @lt_priority.

    LOOP AT lt_priority ASSIGNING FIELD-SYMBOL(<fs_priority>).
      r_valid_priority = VALUE #(
       ( sign = 'I' option = 'EQ' low = <fs_priority>-priority_code )
     ) .
    ENDLOOP.

    SELECT status_code, status_description
     FROM ztstatus_jcp
     INTO TABLE @lt_status.

    LOOP AT lt_status ASSIGNING FIELD-SYMBOL(<fs_status>).
      r_valid_status = VALUE #(
           ( sign = 'I' option = 'EQ' low = <fs_status>-status_code )
         ) .
    ENDLOOP.

    " Validate the status value
    IF iv_status NOT IN r_valid_status.
      rv_valid = abap_false.
    ELSE.
      rv_valid = abap_true.
    ENDIF.

    " Validate the priority value
    IF iv_priority NOT IN r_valid_priority.
      rv_valid = abap_false.
    ELSE.
      rv_valid = abap_true.
    ENDIF.

    RETURN.
  ENDMETHOD.

  METHOD validate_update_order.
    " Check if the work order exists
    DATA(lv_order_exists) = check_order_exists( is_work_order-work_order_id ).
    IF lv_order_exists IS INITIAL.
      rv_valid = abap_false.
    ENDIF.

    SELECT SINGLE status
      FROM ztwork_order_jcp
      WHERE work_order_id = @is_work_order-work_order_id
        AND customer_id  = @is_work_order-customer_id
        AND technician_id = @is_work_order-technician_id
        INTO @ev_status .

    " Check if the order status is editable (e.g., Pending)
    IF is_work_order-status EQ 'PE'.
      rv_valid = abap_true.
    ELSE.
      rv_valid = abap_false.
    ENDIF.

    RETURN.

  ENDMETHOD.


  METHOD validate_delete_order.

    SELECT SINGLE FROM ztwork_order_jcp
       FIELDS status, priority
       WHERE work_order_id = @is_work_order-work_order_id
         AND customer_id   = @is_work_order-customer_id
*        and technician_id = @is_work_order-technician_id
         INTO ( @ev_status, @ev_priority ).
    IF sy-subrc eq 0.
      rv_valid = abap_true.
    ELSE.
      rv_valid = abap_false.
    ENDIF.
      RETURN.
  ENDMETHOD.

ENDCLASS.
