CLASS zcl_work_order_crud_test_jcp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    TYPES: BEGIN OF ty_order,
             work_order_id     TYPE zewrkord_id,
             customer_id       TYPE zecustomer_id,
             creation_date     TYPE d,
             modification_date TYPE d,
             status            TYPE zestatus_jcp,
             priority          TYPE zepriority_jcp,
           END OF ty_order.

    TYPES: BEGIN OF ty_asig,
             technician_id     TYPE zetechn_id,
             work_order_id     TYPE zewrkord_id,
             customer_id       TYPE zecustomer_id,
             creation_date     TYPE d,
             modification_date TYPE d,
             status            TYPE zestatus_jcp,
             priority          TYPE zepriority_jcp,
           END OF ty_asig.

    TYPES: BEGIN OF ty_tareas,
             technician_id      TYPE zetechn_id,
             name               TYPE string,
             work_order_id      TYPE zewrkord_id,
             customer_id        TYPE zecustomer_id,
             status             TYPE zestatus_jcp,
             priority           TYPE zepriority_jcp,
             description        TYPE c LENGTH 50,
             modification_date  TYPE d,
             change_description TYPE c LENGTH 50,
           END OF ty_tareas.

    DATA lt_tareas TYPE STANDARD TABLE OF ty_tareas.
    DATA: mv_timestamp TYPE utclong,
          mv_time      TYPE t.

    DATA: ls_workorder TYPE ztwork_order_jcp,
          ls_histOrd   TYPE ztwrkordhist_jcp,
          lv_valid     TYPE abap_bool.

    DATA: lv_status   TYPE zestatus_jcp,
          lv_priority TYPE zepriority_jcp.

    DATA: lv_di TYPE d,
          lv_df TYPE d.

    DATA lr_wo     TYPE RANGE OF ztwork_order_jcp-work_order_id.
    data ls_wo     like line of lr_wo.
    DATA:lr_cust   TYPE RANGE OF ztcustomer_jcp-customer_id,
         ls_cust   like line of lr_cust.
    DATA lr_tech   TYPE RANGE OF zttechnician_jcp-technician_id.
    DATA lr_status TYPE RANGE OF ztwork_order_jcp-status.
    data lr_crdt type range of ztwork_order_jcp-creation_date.

    METHODS: enqueue_ot IMPORTING iv_campo         TYPE string
                                   iv_name         TYPE if_abap_lock_object=>tv_name
                                   iv_value        TYPE zewrkord_id
                         EXPORTING lt_parameter    TYPE if_abap_lock_object=>tt_parameter
                         RETURNING VALUE(rv_valid) TYPE abap_bool RAISING cx_abap_lock_failure,

            dequeue_OT IMPORTING iv_name         TYPE if_abap_lock_object=>tv_name
                                 lt_parameter    TYPE if_abap_lock_object=>tt_parameter
                               RETURNING VALUE(rv_valid) TYPE abap_bool.
  protected section.
  PRIVATE SECTION.

    METHODS: paramCRUD EXPORTING ework_order    TYPE ztwork_order_jcp
                                 ework_orf_hist TYPE ztwrkordhist_jcp,

             param_rep exporting iv_di   type d
                                 iv_df   type d
                                 lr_wo   type any table
                                 lr_cust type any table
                                 lr_tech type any table
                                 lr_status type any table.

ENDCLASS.

CLASS zcl_work_order_crud_test_jcp IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    DATA lt_parameter TYPE if_abap_lock_object=>tt_parameter.
    DATA: opc TYPE string.
*Create,Read,Update, Delete
* - - - - - - - - - - - - - - -  - - - - -  - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - -
* 4.1 Validación para crear Ordenes de Trabajo
* - - - - - - - - - - - - - - -  - - - - -  - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - -
    DATA(lo_crud)   = NEW zclwrk_ord_crud_hand_jcp( ).
    DATA(lo_valida) = NEW zcl_work_order_validator_jcp( ).

*Parámetros para Crear,Modificar, Borrar
   paramcrud( importing ework_order = ls_workorder
                        ework_orf_hist = ls_histOrd  ).

*Swithch para opción operación.
    opc =
*  'CR_OT'.
*  'MOD_OT'.
*  'DEL_OT'.
   'REPORTE'.
 CASE opc.
    WHEN 'CR_OT'. "crear Ordenes
*- - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        CLEAR lt_parameter.
*       Valida Autorizacion para crear Ordenes
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

*       Registro a crear
        out->write( ls_workorder-work_order_id ).
        out->write( ls_workorder-customer_id ).
        out->write( ls_workorder-technician_id ).
        out->write( ls_workorder-creation_date ).
        out->write( ls_workorder-status ).
        out->write( ls_workorder-priority ).
        out->write( ls_workorder-description ).

*       Valida que tenga campos existan.
        CLEAR lv_valid.
        lv_valid  = lo_valida->validate_create_order( EXPORTING iv_customer_id   = ls_workorder-customer_id
                                                                iv_technician_id = ls_workorder-technician_id
                                                                iv_status        = ls_workorder-status
                                                                iv_priority      = ls_workorder-priority
                                         ).

        IF lv_valid EQ abap_true.
          out->write( TEXT-c03 ).
        ELSE.
          out->write( TEXT-c04 ).
          EXIT.
        ENDIF.

*       Bloqueo de tabla para crear orden de trabajo
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
          out->write( TEXT-b01 ).
        ELSE.
          out->write( TEXT-b02 ).
          EXIT.
        ENDIF.

*       Crea OT en BD
        DATA(lv_success) = lo_crud->create_work_order( ls_workorder ).
        IF lv_success EQ abap_true.
          out->write( TEXT-c08 ).
          COMMIT WORK AND WAIT.
        ELSE.
          out->write( TEXT-c09 ).
          ROLLBACK WORK.
        ENDIF.
        out->write( ls_workorder-work_order_id ).

*       Desbloqueo de tabla para crear orden de trabajo
        lv_valid = me->dequeue_ot( EXPORTING  iv_name  = 'EZ_WRKORD_JCP'
                                              lt_parameter = lt_parameter
                                                    ).
        IF  lv_valid EQ abap_true.
          out->write( TEXT-b10 ).
        ELSE.
          out->write( TEXT-b11 ).
          EXIT.
        ENDIF.

    WHEN 'MOD_OT'." Actualización de Órdenes de Trabajo
*- - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*       Valida Autorizacion para crear Ordenes
        AUTHORITY-CHECK OBJECT 'ZAOWO_ID'
                        ID 'ZAFWO_ID' FIELD ls_workorder-work_order_id
                        ID 'ACTVT'   FIELD '02'.
        DATA(lv_autm) = COND #( WHEN sy-subrc = 0
                               THEN abap_true
                               ELSE abap_false ).

        IF lv_autm = abap_true.
          out->write( TEXT-u06 ).
        ELSE.
          out->write( TEXT-u08 ).
          EXIT.
        ENDIF.

*       Valida Autorizacion para crear Ordenes history_id
*- - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        AUTHORITY-CHECK OBJECT 'ZAOHIST_ID'
                        ID 'ZAFHIST_ID' FIELD ls_histOrd-history_id
                        ID 'ACTVT'   FIELD '02'.
        DATA(lv_auth) = COND #( WHEN sy-subrc = 0
                               THEN abap_true
                               ELSE abap_false ).

        IF lv_auth = abap_true.
          out->write( TEXT-u07 ).
        ELSE.
          out->write( TEXT-u08 ).
          EXIT.
        ENDIF.

*       1. Verificar que la orden de trabajo existe en la base de datos antes de realizar cualquier modificación.
*       ademas, revisa que en la orden de trabajo el status pueda modificarse, es decir status = PE
        lv_valid = lo_valida->validate_update_order( EXPORTING is_work_order = ls_workorder
                                                     IMPORTING ev_status     = lv_status
                                               ).
        IF  lv_valid EQ abap_true AND lv_status EQ 'PE'.
          out->write( TEXT-u01 ).
          out->write( ls_workorder-work_order_id ).
        ELSEIF lv_valid EQ abap_true AND lv_status EQ 'CO'.
          out->write( TEXT-u02 ).
          EXIT.
        ELSE.
          out->write( TEXT-u03 ).
          EXIT.
        ENDIF.

        DATA(lv_validh) = lo_valida->validate_exist_hist_order( EXPORTING is_histOrd = ls_histOrd
                                               ).
        IF  lv_validh EQ abap_true.
          out->write( TEXT-u04 ).
          out->write( ls_workorder-work_order_id ).
        ELSE.
          out->write( TEXT-u05 ).
          out->write( ls_workorder-work_order_id ).
*          EXIT.
        ENDIF.

*       Objeto de bloqueo orden trabajo
        TRY.
            lv_valid = me->enqueue_ot( EXPORTING iv_campo = 'WORK_ORDER_ID'
                                                  iv_name  = 'EZ_WRKORD_JCP'
                                                  iv_value = ls_workorder-work_order_id
                                                  ).
          CATCH cx_abap_lock_failure.
            "handle exception
        ENDTRY.
        IF  lv_valid EQ abap_true.
          out->write( TEXT-b01 ).
        ELSE.
          out->write( TEXT-b02 ).
          EXIT.
        ENDIF.

*       Modificacion OT
        DATA(lv_rmo) = lo_crud->modificar_work_order( ls_workorder ).
        IF lv_rmo EQ abap_true.
          IF  lv_valid EQ abap_true.
            out->write( TEXT-u09 ).
          ELSE.
            out->write( TEXT-u10 ).
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
          out->write( TEXT-u09 ).
        ELSE.
          out->write( TEXT-u10 ).
        ENDIF.

*       Desbloqueo de tabla para crear orden de trabajo
        lv_valid = me->dequeue_ot( EXPORTING  iv_name  = 'EZ_WRKORD_JCP'
                                              lt_parameter = lt_parameter
                                                    ).
        IF  lv_valid EQ abap_true.
          out->write( TEXT-b10 ).
        ELSE.
          out->write( TEXT-b11 ).
          EXIT.
        ENDIF.

    WHEN 'DEL_OT'."Borra Ordenes de trabajo
*- - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*       Valida Autorizacion para Borrar Ordenes de trabajo
        AUTHORITY-CHECK OBJECT 'ZAOWO_ID'
                        ID 'ZAFWO_ID' FIELD ls_workorder-work_order_id
                        ID 'ACTVT'   FIELD '06'.
        DATA(lv_autd) = COND #( WHEN sy-subrc = 0
                               THEN abap_true
                               ELSE abap_false ).

        IF lv_autd = abap_true.
          out->write( TEXT-d01 ).
        ELSE.
          out->write( TEXT-d02 ).
          EXIT.
        ENDIF.

*       Borrar solo OT en estado pendinte
        DATA(lv_vdel)  =  lo_valida->validate_delete_order( EXPORTING is_work_order = ls_workorder
                                                                      is_histOrd    = ls_histOrd
                                                            IMPORTING ev_status     = lv_status
                                                                      ev_priority   = lv_priority
                                         ).
        IF lv_vdel EQ abap_true AND lv_STATUS EQ 'PE'.
*          out->write( TEXT-D03 ).
*       Objeto de bloqueo orden trabajo
          TRY.
              lv_valid = me->enqueue_ot( EXPORTING iv_campo = 'WORK_ORDER_ID'
                                                    iv_name  = 'EZ_WRKORD_JCP'
                                                    iv_value = ls_workorder-work_order_id
                                                    ).
            CATCH cx_abap_lock_failure.
              "handle exception
          ENDTRY.
          IF  lv_valid EQ abap_true.
            out->write( TEXT-b01 ).
          ELSE.
            out->write( TEXT-b02 ).
            EXIT.
          ENDIF.

          DATA(lv_del) = lo_crud->delete_work_order( EXPORTING is_workorder =  ls_workorder ).
          IF lv_autd = abap_true.
            out->write( TEXT-d03 ).
          ELSE.
            out->write( TEXT-d04 ).
            EXIT.
          ENDIF.

*        Desbloqueo de tabla para crear orden de trabajo
          lv_valid = me->dequeue_ot( EXPORTING  iv_name  = 'EZ_WRKORD_JCP'
                                                lt_parameter = lt_parameter
                                                      ).
          IF  lv_valid EQ abap_true.
            out->write( TEXT-b10 ).
          ELSE.
            out->write( TEXT-b11 ).
            EXIT.
          ENDIF.
        ELSE.
          out->write( TEXT-d04 ).
        ENDIF.

    WHEN 'REPORTE'.
          data wa_t type ty_tareas.
          DATA lt_wo_st TYPE STANDARD TABLE OF zewo_consolid.
          DATA lt_wo    TYPE STANDARD TABLE OF zewo_consolid.
          DATA lt_cte   TYPE STANDARD TABLE OF ztcustomer_jcp.
          DATA lt_tech  TYPE STANDARD TABLE OF zttechnician_jcp.

*Obtiene parametros- simula parametros de entrada
       param_rep( importing iv_di = lv_di
                            iv_df = lv_df
                            lr_wo   = lr_wo
                            lr_cust = lr_cust
                            lr_tech = lr_cust
                            lr_status = lr_status
                                     ).

*       Valida Autorizacion para despliegue - lectura
        AUTHORITY-CHECK OBJECT 'ZAOWO_ID'
                        ID 'ZAFWO_ID' FIELD ls_workorder-work_order_id
                        ID 'ACTVT'    FIELD '03'.
        DATA(lv_desp) = COND #( WHEN sy-subrc = 0
                               THEN abap_true
                               ELSE abap_false ).

        IF lv_desp = abap_true.
          out->write( TEXT-r01 ).
        ELSE.
          out->write( TEXT-r02 ).
          EXIT.
        ENDIF.

*Extrae datos
        lo_crud->lectura_bd(  exporting  lv_di   = lv_di
                                         lv_df   = lv_df
                                         lr_wo   = lr_wo
                                         lr_cust = lr_cust
                                         lr_tech = lr_tech
                               importing lt_wo   = lt_wo
                                         lt_cte  = lt_cte
                                         lt_tech = lt_tech
                                         ) .
* Consultas.

*       Despliega las Ordenes de trabajo agrupados desplegados por estatus
        LOOP AT lt_wo ASSIGNING FIELD-SYMBOL(<fs>) GROUP BY <fs>-status.
          CLEAR lt_wo_st.
          LOOP AT GROUP <fs> INTO DATA(lw_wo).
            lt_wo_st = VALUE #( BASE lt_wo_st ( lw_wo ) ).
          ENDLOOP.
          out->write( data = lt_wo_st name = 'Ordenes agrupadas por STATUS' ).
        ENDLOOP.
        UNASSIGN <fs>.

*       Despliega tareas asignadas a los técnicos
        LOOP AT lt_wo into data(lw_wot).
           move-corresponding lw_wot to wa_t.
        data(lw_tech) = lt_tech[ ('TECHNICIAN_ID') = lw_wot-technician_id ].
        MOVE-CORRESPONDING lw_tech TO wa_t.
        APPEND wa_t TO LT_TAREAS.
        ENDLOOP.
        SORT LT_TAREAS BY TECHNICIAN_ID.
        out->write( data = lt_tareas name = 'Tareas asignadas a los técnicos' ).

        data cont type i.
        CLEAR: lt_wo_st , cont.

*       Despliega Ordenes concluidas
        LOOP AT lt_wo ASSIGNING FIELD-SYMBOL(<fsoc>) where status in lr_status.
            lt_wo_st = VALUE #( BASE lt_wo_st ( <fsoc> ) ).
            cont = cont + 1.
        ENDLOOP.
          out->write( cont ).
          out->write( data = lt_wo_st
                      name = 'Ordenes con STATUS' ).
        UNASSIGN <fs>.

        CLEAR: lt_wo_st.
*       Despliega Ordenes por fecha
        LOOP AT lt_wo ASSIGNING FIELD-SYMBOL(<fsf>) where creation_date in lr_crdt.
            lt_wo_st = VALUE #( BASE lt_wo_st ( <fsf> ) ).
        ENDLOOP.
          out->write( data = lt_wo_st
                      name = 'Ordenes por fecha' ).
        UNASSIGN <fs>.
    ENDCASE.

  ENDMETHOD.

  METHOD paramcrud.
    DATA: lw_workorder TYPE ztwork_order_jcp,
          lw_histOrd   TYPE ztwrkordhist_jcp.

    DATA: lv_date TYPE d,
          lv_time TYPE t.
*   Obtiene la fecha actual con la función “UTCLONG_CURRENT()”.
    me->mv_timestamp = utclong_current( ).

*   Obtiene la fecha del sistema y la pasa a dos variables 1.fecha y 2.hora
    TRY.
        CONVERT UTCLONG me->mv_timestamp
        TIME ZONE cl_abap_context_info=>get_user_time_zone( )
        INTO DATE lv_date
        TIME lv_time.
      CATCH cx_abap_context_info_error.
        "handle exception.
    ENDTRY.

*   Datos para crear la orden
    DATA lv_maxwo  TYPE ztwork_order_jcp-work_order_id.
    DATA lv_maxhwo TYPE ztwrkordhist_jcp-history_id.
    DATA lv_esptec TYPE zttechnician_jcp-speciality.

    DATA: lv_tec      TYPE  zttechnician_jcp-technician_id VALUE 'T0000003',
          lv_cust     TYPE ztcustomer_jcp-customer_id      VALUE '00000004',
          lv_status   TYPE ztstatus_jcp-status_code        VALUE 'CO',
          lv_priority TYPE ztpriority_jcp-priority_code  VALUE 'A'.


    SELECT MAX( DISTINCT work_order_id )
        FROM ztwork_order_jcp
        INTO  @lv_maxwo.

    SELECT MAX( DISTINCT history_id )
        FROM ztwrkordhist_jcp
        INTO  @lv_maxhwo.

    SELECT SINGLE
        FROM zttechnician_jcp
        FIELDS speciality
        WHERE technician_id EQ @lv_tec
        INTO  @lv_esptec.

    lw_workorder = VALUE #(       client = sy-mandt
                                  work_order_id    = ( lv_maxwo + 1 )
                                  customer_id      = lv_cust
                                  technician_id    = lv_tec
                                  creation_date    = lv_date
                                  status           = lv_status
                                  priority         = lv_priority
                                  description      = lv_esptec
                               ).

    lw_histOrd = VALUE #( history_id       = ( lv_maxhwo + 1 )
                         work_order_id     = ls_workorder-work_order_id
                         modification_date = lv_date
                         change_description =  'Servicio completado'

  ).

    ls_workorder =  lw_workorder.
    ls_histOrd =  lw_histOrd.

  ENDMETHOD.

  METHOD dequeue_ot.

    TRY.
        DATA(lo_locked_object) = cl_abap_lock_object_factory=>get_instance(
            EXPORTING iv_name = iv_name ). "'EZ_WRKORD_JCP'
*     Bloqueo de objetos instancia no creada
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
    TRY.
        DATA(lo_locked_object) = cl_abap_lock_object_factory=>get_instance(
        EXPORTING iv_name = iv_name ). "'EZ_WRKORD_JCP'
*     Bloqueo de objetos instancia no creada
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

  METHOD param_rep.

 DATA lr_wor     TYPE RANGE OF ztwork_order_jcp-work_order_id.
 DATA lr_custr   TYPE RANGE OF ztcustomer_jcp-customer_id.
 DATA lr_techr   TYPE RANGE OF zttechnician_jcp-technician_id.
 DATA lr_statusr TYPE RANGE OF ztwork_order_jcp-status.
 data lr_crdtr   type range of ztwork_order_jcp-creation_date.

    iv_di = '20250501'.
    iv_df = '20250531'.

*    lr_wor = VALUE #( ( sign   = 'I'
*                 option   = 'EQ'
*                 low      = '0000000003' )
*                ).

    lr_wo[] = lr_wor[].

*    lr_custr = VALUE #( ( sign   = 'I'
*                 option   = 'EQ'
*                 low      = '00000002' )
*                ).
    lr_cust[] = lr_custr[].

*    lr_techr = VALUE #( ( sign   = 'I'
*                 option   = 'EQ'
*                 low      = 'T0000004' )
*                ).

    lr_tech[] = lr_techr[].

    lr_statusr = VALUE #( ( sign   = 'I'
                 option   = 'EQ'
                 low      = 'CO' )
                ).
    lr_status[] = lr_statusr[].

    lr_crdtr = VALUE #( ( sign   = 'I'
                 option   = 'EQ'
                 low      = '20250529' )
                ).
 lr_crdt[] = lr_crdtr[].

  ENDMETHOD.

ENDCLASS.
