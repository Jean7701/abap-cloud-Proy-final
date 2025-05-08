CLASS zcl_work_order_crud_test_jcp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    DATA: mv_timestamp TYPE utclong,
          mv_time      TYPE t.

    DATA: ls_workorder TYPE ztwork_order_jcp,
          ls_histOrd   type ztwrkordhist_jcp,
          lv_valid     TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_work_order_crud_test_jcp IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
*    DATA: ls_workorder TYPE  ztwork_order_jcp.
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

    ls_workorder = VALUE #(    work_order_id    = '0000000001'
                                  customer_id   = '00000001' "'70000001'
                                  technician_id = 'T0000001' "'STDESLIB'
                                  creation_date = lv_date "utclong_current( )    "CL_ABAP_CONTEXT_INFO=>get_system_date( )
                                  status        = 'P'
                                  priority      = 'B'
                                  description   = 'Reparación de HW'

                               ).

   ls_histOrd = value #( history_id     = '100000000000'
                         work_order_id   = '0000000001'
                         modification_date =  lv_date
                         change_description  =  'Cambio lector huella'
  ).

**Valida que tenga campos existan.

    lv_valid  = lo_valida->validate_create_order( EXPORTING iv_customer_id   = ls_workorder-customer_id
                                                            iv_technician_id = ls_workorder-technician_id
                                                            iv_status        = ls_workorder-status
                                                            iv_priority      = ls_workorder-priority

                                     ).

    IF lv_valid EQ abap_true.
      out->write( | los datos de la orden  { ls_workorder-work_order_id } son validos | ).
    ELSE.
      out->write( | los datos de la orden  { ls_workorder-work_order_id } incorrectos | ).
      out->write( | El No.cliente { ls_workorder-customer_id } no existe | ).
      out->write( | El id_tecnico { ls_workorder-technician_id } no existe | ).
    ENDIF.

* - - - - - - - - - - - - - - -  - - - - -  - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - -
*Crear Ordenes de Trabajo
* - - - - - - - - - - - - - - -  - - - - -  - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - -
*Valida Autorizacion para crear Ordenes
*- - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    AUTHORITY-CHECK OBJECT 'ZAOWO_ID'
                    ID 'ZAFWO_ID' FIELD ls_workorder-work_order_id
                    ID 'ACTVT'   FIELD '01'.
    DATA(lv_aut) = COND #( WHEN sy-subrc = 0
                           THEN abap_true
                           ELSE abap_false ).

    IF lv_aut = abap_true.
      out->write( | Tienes permisos para crear la orden de trabajo { ls_workorder-work_order_id } | ).
    ELSE.
      out->write( | Tienes permisos para crear la orden de trabajo  { ls_workorder-work_order_id }  | ).
    ENDIF.

*Realiza Bloqueo de tabla para crear orden de trabajo
    out->write( |Userio inicia proceso de Bloqueo | ).
    TRY.
        DATA(lo_locked_object) = cl_abap_lock_object_factory=>get_instance(
        EXPORTING iv_name = 'EZ_WRKORD_JCP' ).

      CATCH cx_abap_lock_failure.
        out->write( |Bloqueo de objetos instancia no creada| ).
        RETURN.
    ENDTRY.


    DATA lt_parameter TYPE if_abap_lock_object=>tt_parameter.
    lt_parameter = VALUE #(  ( name = 'WORK_ORDER_ID'
                               value = REF #( ls_workorder-work_order_id )
                             )
                          ).
    TRY.
        lo_locked_object->enqueue( it_parameter = lt_parameter ).
      CATCH cx_abap_foreign_lock.
        out->write( |Falla en Objeto de bloqueo | ).
        RETURN.
    ENDTRY.

*Creación de Orden de trabajo
*- - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     out->write( |El objeto de bloqueo esta activo| ).
     out->write( | Objeto de bloqueado por: { sy-uname } | ).

    DATA(lv_success) = lo_crud->create_work_order( ls_workorder ).
    IF lv_success EQ abap_true.
      out->write( | La orden  { ls_workorder-work_order_id } fue creada correctamente | ).
    ELSE.
      out->write( | La orden  { ls_workorder-work_order_id } no fue creada | ).
    ENDIF.

    WAIT UP TO 3 SECONDS.

    TRY.
        lo_locked_object->dequeue( it_parameter = lt_parameter ).
      CATCH cx_abap_lock_failure.
        out->write( |El objeto de negocio No fue actualizado en la Base de datos| ).
    ENDTRY.
    out->write( |Termina boqueo | ).

* - - - - - - - - - - - - - - -  - - - - -  - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - -
*  4.2. Validación de Actualización de Órdenes de Trabajo
* - - - - - - - - - - - - - - -  - - - - -  - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - -

*Verificar que la orden de trabajo existe en la base de datos antes de realizar cualquier modificación.

*o Comprobar que solo se pueden actualizar las órdenes cuyo estado (STATUS) esté en un estado editable (por ejemplo, "PE"
*para pendiente).

 lv_valid  = lo_valida->validate_status_and_priority( EXPORTING iv_status  = ls_workorder-STATUS
                                                                iv_priority  =  ls_workorder-priority
                                                      ).


  ENDMETHOD.


ENDCLASS.
