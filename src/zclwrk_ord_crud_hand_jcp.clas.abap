CLASS zclwrk_ord_crud_hand_jcp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS: create_work_order IMPORTING is_workorder          TYPE ztwork_order_jcp
                               RETURNING VALUE(rv_success)     TYPE abap_bool,

              modificar_work_order IMPORTING is_workorder      TYPE ztwork_order_jcp
                                   RETURNING VALUE(rv_success) TYPE abap_bool,

              crear_Hist_order IMPORTING is_wrkordhist         TYPE ztwrkordhist_jcp
                               RETURNING VALUE(rv_success)     TYPE abap_bool,

              update_hist_order IMPORTING is_histOrd           TYPE ztwrkordhist_jcp
                                RETURNING VALUE(rv_upd)        TYPE abap_bool,

              delete_work_order IMPORTING is_workorder      TYPE ztwork_order_jcp
*                                          is_wrkordhist     TYPE ztwrkordhist_jcp
                                RETURNING VALUE(rv_success) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mo_validator TYPE REF TO zcl_work_order_validator_jcp.
ENDCLASS.


CLASS zclwrk_ord_crud_hand_jcp IMPLEMENTATION.
  METHOD crear_hist_order.
    DATA: lv_text TYPE string.

    TRY.
*        INSERT ztwrkordhist_jcp FROM @is_wrkordhist .
        MODIFY ztwrkordhist_jcp FROM @is_wrkordhist .
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


  METHOD modificar_work_order.
    DATA: lv_text TYPE string.

    TRY.
        UPDATE ztwork_order_jcp
         SET status   = @is_workorder-status,
             priority = @is_workorder-priority
         WHERE work_order_id  = @is_workorder-work_order_id
          AND  customer_id    = @is_workorder-customer_id
          AND  technician_id  = @is_workorder-technician_id.
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

  METHOD update_hist_order.
     DATA: lv_text TYPE string.
    TRY.
        UPDATE ztwrkordhist_jcp
           SET  modification_date = @is_histOrd-modification_date,
                change_description = @is_histOrd-change_description
           WHERE  history_id = @is_histOrd-history_id
              AND work_order_id  = @is_histOrd-work_order_id.
      CATCH cx_sy_open_sql_db INTO DATA(lx_sql_db).
        lv_text =  lx_sql_db->get_text( ).
    ENDTRY.
    IF sy-subrc EQ 0.
      rv_upd = abap_true.
    ELSE.
      rv_upd = abap_false.
    ENDIF.
    RETURN.
  ENDMETHOD.

  METHOD delete_work_order.
  DATA: lv_text TYPE string.
    TRY.
    DELETE ztwork_order_jcp FROM @is_workorder.
    CATCH cx_sy_open_sql_db INTO DATA(lx_sql_db).
        lv_text =  lx_sql_db->get_text( ).
    ENDTRY.
    IF sy-subrc EQ 0.
      rv_success = abap_true.
    ELSE.
      rv_success = abap_false.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
