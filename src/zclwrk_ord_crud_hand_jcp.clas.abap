CLASS zclwrk_ord_crud_hand_jcp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_work_order,
             work_order_id TYPE ztwork_order-work_order_id,
             customer_id   TYPE ztwork_order-customer_id,
             technician_id TYPE ztwork_order-technician_id,
             priority      TYPE ztwork_order-priority,
             description   TYPE ztwork_order-description,
             created_at    TYPE d,
           END OF ty_work_order.

    METHODS: create_work_order IMPORTING is_workorder      TYPE ztwork_order_jcp
                               RETURNING VALUE(rv_success) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mo_validator TYPE REF TO zcl_work_order_validator_jcp.
ENDCLASS.



CLASS zclwrk_ord_crud_hand_jcp IMPLEMENTATION.

  METHOD create_work_order.
    DATA: lv_text TYPE string.
    TRY.
*    delete ztwork_order_jcp FROM @is_workorder.
        INSERT ztwork_order_jcp FROM @is_workorder.
      CATCH cx_sy_open_sql_db INTO DATA(lx_sql_db).
        lv_text =  lx_sql_db->get_text( ).
    ENDTRY.
    IF sy-subrc EQ 0.
      rv_success = abap_true.
    ELSE.
      rv_success = abap_false.
    ENDIF.
    RETURN.

  ENDMETHOD.



ENDCLASS.
