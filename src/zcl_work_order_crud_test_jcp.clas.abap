CLASS zcl_work_order_crud_test_jcp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    DATA: mv_timestamp TYPE utclong,
          mv_time      TYPE t.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_work_order_crud_test_jcp IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    DATA: ls_workorder TYPE  ztwork_order_jcp.
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
* - - - - - - - - - - - - - - -  - - - - -  - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - -
* Creación de una Orden de servicio
* - - - - - - - - - - - - - - -  - - - - -  - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - -

    DATA(lo_handler) = NEW zclwrk_ord_crud_hand_jcp( ).
    ls_workorder = VALUE #(    work_order_id = '0000000001'
                                  customer_id   = '70000001'
                                  technician_id = 'STDESLIB'
                                  creation_date = lv_date "utclong_current( )    "CL_ABAP_CONTEXT_INFO=>get_system_date( )
                                  status        = 'P'
                                  priority      = 'B'
                                  description   = 'Desbloqueo y liberación'

                               ).

    DATA(lv_success) = lo_handler->create_work_order( ls_workorder ).
    IF lv_success EQ abap_true.
      out->write( | La orden  { ls_workorder-work_order_id } fue creada correctamente | ).
    ELSE.
      out->write( | La orden  { ls_workorder-work_order_id } no fue creada | ).
    ENDIF.


  ENDMETHOD.


ENDCLASS.
