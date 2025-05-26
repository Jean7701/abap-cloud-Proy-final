CLASS zcl_work_order_crud_test_jcp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    DATA: mv_timestamp TYPE utclong,
          mv_time      TYPE t.

    DATA: ls_workorder TYPE ztwork_order_jcp,
          ls_histOrd   TYPE ztwrkordhist_jcp,
          lv_valid     TYPE abap_bool.
    DATA: lv_status   TYPE zestatus_jcp,
          lv_priority TYPE zepriority_jcp.

    METHODS: enqueue_ot IMPORTING iv_campo        TYPE string
                                   iv_name         TYPE if_abap_lock_object=>tv_name
                                   iv_value        TYPE zewrkord_id
                         EXPORTING lt_parameter    TYPE if_abap_lock_object=>tt_parameter
                         RETURNING VALUE(rv_valid) TYPE abap_bool RAISING cx_abap_lock_failure,

            dequeue_OT IMPORTING iv_name         TYPE if_abap_lock_object=>tv_name
                                 lt_parameter    TYPE if_abap_lock_object=>tt_parameter
                               RETURNING VALUE(rv_valid) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_work_order_crud_test_jcp IMPLEMENTATION.


  METHOD dequeue_ot.

    TRY.
        DATA(lo_locked_object) = cl_abap_lock_object_factory=>get_instance(
            EXPORTING iv_name = iv_name ). "'EZ_WRKORD_JCP'
*Bloqueo de objetos instancia no creada
      CATCH cx_abap_lock_failure.
        rv_valid = abap_false.
        RETURN.
    ENDTRY.


    TRY.
        lo_locked_object->dequeue( it_parameter = lt_parameter ).
      CATCH cx_abap_lock_failure.
*        out->write( |El objeto de negocio No fue actualizado en la Base de datos| ).
    ENDTRY.
    IF sy-subrc EQ 0.
      rv_valid = abap_true.
    ELSE.
      rv_valid = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD enqueue_ot.

*    DATA lt_parameter TYPE if_abap_lock_object=>tt_parameter.

    TRY.
        DATA(lo_locked_object) = cl_abap_lock_object_factory=>get_instance(
        EXPORTING iv_name = iv_name ). "'EZ_WRKORD_JCP'
*Bloqueo de objetos instancia no creada
      CATCH cx_abap_lock_failure.
        rv_valid = abap_false.
        RETURN.
    ENDTRY.


    lt_parameter = VALUE #(  ( name = iv_campo  "'WORK_ORDER_ID'
                               value = REF #( iv_value ) "ls_workorder-work_order_id )
                             )
                          ).

    TRY.
        lo_locked_object->enqueue( it_parameter = lt_parameter ).
      CATCH cx_abap_foreign_lock.
        rv_valid = abap_false.
        RETURN.
        RETURN.
    ENDTRY.

    IF sy-subrc EQ 0.
      rv_valid = abap_true.
    ELSE.
      rv_valid = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    DATA lt_parameter TYPE if_abap_lock_object=>tt_parameter.
    DATA: opc TYPE string.
    DATA: lv_date TYPE d,
          lv_time TYPE t.


*Obtiene la fecha actual con la función “UTCLONG_CURRENT()”.
    me->mv_timestamp = utclong_current( ).

*Obtiene la fecha del sistema y la pasa a dos variables 1.fecha y 2.hora
    TRY.
        CONVERT UTCLONG me->mv_timestamp
        TIME ZONE cl_abap_context_info=>get_user_time_zone( )
        INTO DATE lv_date
        TIME lv_time.
      CATCH cx_abap_context_info_error.
        "handle exception.
    ENDTRY.
*Create,Read,Update, Delete
* - - - - - - - - - - - - - - -  - - - - -  - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - -
* 4.1 Validación para crear Ordenes de Trabajo
* - - - - - - - - - - - - - - -  - - - - -  - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - -

**Datos para crear la orden
    DATA(lo_crud)   = NEW zclwrk_ord_crud_hand_jcp( ).
    DATA(lo_valida) = NEW zcl_work_order_validator_jcp( ).

    ls_workorder = VALUE #(       client = sy-mandt
                                  work_order_id    = '0000000005'
                                  customer_id      = '00000001' "'70000001'
                                  technician_id    = 'T0000003' "'STDESLIB'
                                  creation_date    = lv_date "utclong_current( )    "CL_ABAP_CONTEXT_INFO=>get_system_date( )
                                  status           = 'PE'
                                  priority         = 'B'
                                  description      = 'Recup.datos móviles' "'Soporte de dspositivos moviles'
                               ).

*    ls_histOrd = VALUE #( history_id     = '100000000000'
*                          work_order_id   = '0000000002'
*                          modification_date =  lv_date
*                          change_description  =  'Diagnóstico inicial'
*
*   ).

*Swithch para opción operación.
    opc =
*    'CR_OT'.
*   'MOD_OT'.
   'DEL_OT'.


    CASE opc.
      WHEN 'CR_OT'.
        CLEAR lt_parameter.
*Valida Autorizacion para crear Ordenes
*- - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        AUTHORITY-CHECK OBJECT 'ZAOWO_ID'
                        ID 'ZAFWO_ID' FIELD ls_workorder-work_order_id
                        ID 'ACTVT'    FIELD '01'.
        DATA(lv_aut) = COND #( WHEN sy-subrc = 0
                               THEN abap_true
                               ELSE abap_false ).

        IF lv_aut = abap_true.
          out->write( TEXT-c07 ).
        ELSE.
          out->write( TEXT-c05 ).
          out->write( ls_workorder-work_order_id ).
          EXIT.
        ENDIF.

* Registro a crear
        out->write( ls_workorder-work_order_id ).
        out->write( ls_workorder-customer_id ).
        out->write( ls_workorder-technician_id ).
        out->write( ls_workorder-creation_date ).
        out->write( ls_workorder-status ).
        out->write( ls_workorder-priority ).
        out->write( ls_workorder-description ).

**Valida que tenga campos existan.
        CLEAR lv_valid.
        lv_valid  = lo_valida->validate_create_order( EXPORTING iv_customer_id   = ls_workorder-customer_id
                                                                iv_technician_id = ls_workorder-technician_id
                                                                iv_status        = ls_workorder-status
                                                                iv_priority      = ls_workorder-priority
                                         ).

        IF lv_valid EQ abap_true.
          out->write( TEXT-C03 ).
        ELSE.
          out->write( TEXT-C04 ).
          EXIT.
        ENDIF.

*Realiza Bloqueo de tabla para crear orden de trabajo
        CLEAR lv_valid.
        TRY.
            lv_valid = me->enqueue_ot( EXPORTING iv_campo = 'WORK_ORDER_ID'
                                                  iv_name  = 'EZ_WRKORD_JCP'
                                                  iv_value = ls_workorder-work_order_id
                                                  ).
          CATCH cx_abap_lock_failure.
            "handle exception
        ENDTRY.
        IF  lv_valid EQ abap_true.
          out->write( TEXT-B01 ).
        ELSE.
          out->write( TEXT-B02 ).
          EXIT.
        ENDIF.
*Crea OT en BD
        DATA(lv_success) = lo_crud->create_work_order( ls_workorder ).
        IF lv_success EQ abap_true.
          out->write( TEXT-C08 ).
             COMMIT WORK AND WAIT.
        ELSE.
          out->write( TEXT-C09 ).
                ROLLBACK WORK.
        ENDIF.
        out->write( ls_workorder-work_order_id ).

*  Desbloqueo de tabla para crear orden de trabajo
        lv_valid = me->dequeue_ot( EXPORTING  iv_name  = 'EZ_WRKORD_JCP'
                                              lt_parameter = lt_parameter
                                                    ).
        IF  lv_valid EQ abap_true.
          out->write( TEXT-B10 ).
        ELSE.
          out->write( TEXT-B11 ).
          EXIT.
        ENDIF.

      WHEN 'MOD_OT'.
* Validación de Actualización de Órdenes de Trabajo
*Valida Autorizacion para crear Ordenes
*- - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        AUTHORITY-CHECK OBJECT 'ZAOWO_ID'
                        ID 'ZAFWO_ID' FIELD ls_workorder-work_order_id
                        ID 'ACTVT'   FIELD '02'.
        DATA(lv_autm) = COND #( WHEN sy-subrc = 0
                               THEN abap_true
                               ELSE abap_false ).

        IF lv_autm = abap_true.
          out->write( TEXT-U06 ).
        ELSE.
          out->write( TEXT-U08 ).
          EXIT.
        ENDIF.

*Valida Autorizacion para crear Ordenes history_id
*- - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        AUTHORITY-CHECK OBJECT 'ZAOHIST_ID'
                        ID 'ZAFHIST_ID' FIELD ls_histOrd-history_id
                        ID 'ACTVT'   FIELD '02'.
        DATA(lv_auth) = COND #( WHEN sy-subrc = 0
                               THEN abap_true
                               ELSE abap_false ).

        IF lv_auth = abap_true.
          out->write( TEXT-U07 ).
        ELSE.
          out->write( TEXT-U08 ).
          EXIT.
        ENDIF.

*1. Verificar que la orden de trabajo existe en la base de datos antes de realizar cualquier modificación.
*   ademas, revisa que en la orden de trabajo el status pueda modificarse, es decir status = PE
        lv_valid = lo_valida->validate_update_order( EXPORTING is_work_order = ls_workorder
                                                     IMPORTING ev_status     = lv_status
                                               ).
        IF  lv_valid EQ abap_true AND lv_status EQ 'PE'.
          out->write( TEXT-U01 ).
          out->write( ls_workorder-work_order_id ).
        ELSEIF lv_valid EQ abap_true AND lv_status EQ 'CO'.
          out->write( TEXT-U02 ).
          EXIT.
        ELSE.
          out->write( TEXT-U03 ).
          EXIT.
        ENDIF.

        DATA(lv_validh) = lo_valida->validate_exist_hist_order( EXPORTING is_histOrd = ls_histOrd
                                               ).
        IF  lv_validh EQ abap_true.
          out->write( TEXT-U04 ).
          out->write( ls_workorder-work_order_id ).
        ELSE.
          out->write( TEXT-U05 ).
          out->write( ls_workorder-work_order_id ).
*          EXIT.
        ENDIF.

*Objeto de bloqueo orden trabajo
        TRY.
            lv_valid = me->enqueue_ot( EXPORTING iv_campo = 'WORK_ORDER_ID'
                                                  iv_name  = 'EZ_WRKORD_JCP'
                                                  iv_value = ls_workorder-work_order_id
                                                  ).
          CATCH cx_abap_lock_failure.
            "handle exception
        ENDTRY.
        IF  lv_valid EQ abap_true.
          out->write( TEXT-B01 ).
        ELSE.
          out->write( TEXT-B02 ).
          EXIT.
        ENDIF.

*Modificacion OT
        DATA(lv_rmo) = lo_crud->modificar_work_order( ls_workorder ).
        IF lv_rmo EQ abap_true.
          IF  lv_valid EQ abap_true.
            out->write( TEXT-U09 ).
          ELSE.
            out->write( TEXT-U10 ).
            EXIT.
          ENDIF.

          DATA(lv_val) = lo_valida->validate_exist_hist_order( ls_histOrd ).
          IF lv_val = abap_true.
            DATA(lv_crh) = lo_crud->update_hist_order( ls_histOrd ).
          ELSE.
            DATA(lv_cho) = lo_crud->crear_hist_order( ls_histOrd ).
          ENDIF.
        ENDIF.

        IF lv_rmo EQ abap_true OR lv_val = abap_true.
         COMMIT WORK AND WAIT.
         ELSE.
         ROLLBACK WORK.
         ENDIF.

        IF lv_crh EQ abap_true OR lv_cho EQ abap_true.
          out->write( TEXT-U09 ).
        ELSE.
          out->write( TEXT-U10 ).
        ENDIF.

*  Desbloqueo de tabla para crear orden de trabajo
        lv_valid = me->dequeue_ot( EXPORTING  iv_name  = 'EZ_WRKORD_JCP'
                                              lt_parameter = lt_parameter
                                                    ).
        IF  lv_valid EQ abap_true.
          out->write( TEXT-B10 ).
        ELSE.
          out->write( TEXT-B11 ).
          EXIT.
        ENDIF.

      WHEN 'DEL_OT'.
*Valida Autorizacion para crear Ordenes
*- - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        AUTHORITY-CHECK OBJECT 'ZAOWO_ID'
                        ID 'ZAFWO_ID' FIELD ls_workorder-work_order_id
                        ID 'ACTVT'   FIELD '06'.
        DATA(lv_autd) = COND #( WHEN sy-subrc = 0
                               THEN abap_true
                               ELSE abap_false ).

        IF lv_autd = abap_true.
          out->write( TEXT-D01 ).
        ELSE.
          out->write( TEXT-D02 ).
          EXIT.
        ENDIF.

*Borrar solo OT en estado pendinte
        DATA(lv_vdel)  =  lo_valida->validate_delete_order( EXPORTING is_work_order = ls_workorder
                                                                      is_histOrd    = ls_histOrd
                                                            IMPORTING ev_status     = lv_status
                                                                      ev_priority   = lv_priority
                                         ).
        IF lv_vdel EQ abap_true AND lv_STATUS EQ 'PE'.
*          out->write( TEXT-D03 ).
*Objeto de bloqueo orden trabajo
          TRY.
              lv_valid = me->enqueue_ot( EXPORTING iv_campo = 'WORK_ORDER_ID'
                                                    iv_name  = 'EZ_WRKORD_JCP'
                                                    iv_value = ls_workorder-work_order_id
                                                    ).
            CATCH cx_abap_lock_failure.
              "handle exception
          ENDTRY.
          IF  lv_valid EQ abap_true.
            out->write( TEXT-B01 ).
          ELSE.
            out->write( TEXT-B02 ).
            EXIT.
          ENDIF.

          DATA(lv_del) = lo_crud->delete_work_order( EXPORTING is_workorder =  ls_workorder ).
          IF lv_autd = abap_true.
            out->write( TEXT-D03 ).
          ELSE.
            out->write( TEXT-D04 ).
            EXIT.
          ENDIF.

*  Desbloqueo de tabla para crear orden de trabajo
          lv_valid = me->dequeue_ot( EXPORTING  iv_name  = 'EZ_WRKORD_JCP'
                                                lt_parameter = lt_parameter
                                                      ).
          IF  lv_valid EQ abap_true.
            out->write( TEXT-B10 ).
          ELSE.
            out->write( TEXT-B11 ).
            EXIT.
          ENDIF.
        ELSE.
          out->write( TEXT-D04 ).
        ENDIF.

      WHEN 'REPORTE'.

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
