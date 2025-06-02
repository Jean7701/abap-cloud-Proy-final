CLASS zclwrk_ord_crud_hand_jcp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA lt_wo     TYPE STANDARD TABLE OF zewo_consolid. "ty_wo.

    DATA lt_cte    TYPE STANDARD TABLE OF ztcustomer_jcp.
    DATA lt_tech   TYPE STANDARD TABLE OF zttechnician_jcp.
    DATA: lr_wo    TYPE RANGE OF ztwork_order_jcp-work_order_id.
    DATA lr_cust   TYPE RANGE OF ztcustomer_jcp-customer_id.
    DATA lr_tech   TYPE RANGE OF zttechnician_jcp-technician_id.

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
                                RETURNING VALUE(rv_success) TYPE abap_bool,

              lectura_BD      importing  lv_di   type d
                                         lv_df   type d
                                         lr_wo   type any
                                         lr_cust type any
                                         lr_tech type any
                              exporting  lt_wo   type ZTT_CONSOLIDADO
                                         lt_cte  type ZTT_CUSTOMER_JCP
                                         lt_tech type ZTT_TECHNICIAN_JCP.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mo_validator TYPE REF TO zcl_work_order_validator_jcp.
ENDCLASS.

CLASS zclwrk_ord_crud_hand_jcp IMPLEMENTATION.
  METHOD crear_hist_order.
    DATA: lv_text TYPE string.

    TRY.
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

  METHOD lectura_bd.
      DATA: database_name TYPE string,
          campos        TYPE string,
          cond          TYPE string,
          table         TYPE string.

    database_name = 'ztwork_order_jcp as a left join ztwrkordhist_jcp as b ON a~work_order_id = b~work_order_id'.
    campos =  'a~work_order_id, a~customer_id, a~technician_id, a~creation_date, a~status, a~priority, a~description, b~modification_date, b~change_description'.
    cond = 'a~work_order_id in @lr_wo and a~creation_date between @lv_di and @lv_df'.

    SELECT FROM (database_name)
     FIELDS (campos)
     WHERE (cond)
    INTO CORRESPONDING FIELDS OF TABLE @lt_wo.
    SORT lt_wo BY work_order_id.
*
** -----------------Lee clientes -------------------------------
    CLEAR: database_name, campos, cond.

    database_name = 'ztcustomer_jcp'.
    campos        = ' client, customer_id, name, address, phone'.
    cond          = 'customer_id in @lr_cust'.

    SELECT FROM (database_name)
     FIELDS (campos)
     WHERE (cond)
     INTO TABLE @lt_cte.
** ---------------Lee técnicos ---------------------------------
    CLEAR: database_name, campos, cond.
    database_name = 'zttechnician_jcp'.
    campos =  'client, technician_id, name, speciality'.
    cond = 'technician_id in @lr_tech'.

    SELECT FROM (database_name)
     FIELDS (campos)
    WHERE (cond)
    INTO TABLE @lt_tech.
  ENDMETHOD.

ENDCLASS.
