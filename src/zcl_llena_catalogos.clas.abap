CLASS zcl_llena_catalogos DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_LLENA_CATALOGOS IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    DATA: lt_priority TYPE STANDARD TABLE OF zpriority_jcp.
    DATA: lt_status   TYPE STANDARD TABLE OF ztstatus_jcp .

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

* - - - - - - -  - - - - - - - - - -  - - - - - - - - - - - - - - - -

    DATA: lt_cliente TYPE TABLE OF ztcustomer_jcp.
    lt_cliente = VALUE #(  (  client =  sy-mandt
                            customer_id = '00000001'
                            name        = 'Jannet Cruz Pérez'
                            address     = 'Calle Miguel Hidalgo No. 44, Tlaxcala Tlax.'
                            phone       = '5555066488'
                            )

                            (  client =  sy-mandt
                            customer_id = '00000002'
                            name        = 'Yadira Jimenez Rosas'
                            address     = 'Calle del vecino No 33, Yauquemecan Tlax.'
                            phone       = '2415066477'
                            )

                           (  client =  sy-mandt
                            customer_id = '00000003'
                            name        = 'Israel Uri Hernandez Cruz'
                            address     = 'Av. Juarez No. 1234, Apizaco Tlax.'
                            phone       = '2414156893'
                            )

                               (  client =  sy-mandt
                            customer_id = '00000004'
                            name        = 'Cesar Daniel Hernandez Cruz '
                            address     = 'Av. Independencia No. 1000,Tlaxcala Tlax.'
                            phone       = '2464932652'
                            )
                               ).

*                               Modify ztcustomer_jcp from table @lt_cliente.


*  - - - - - - -  - - - - - - - - - -  - - - - - - - - - - - - - - - -

    DATA: lt_tech TYPE TABLE OF zttechnician_jcp.
    lt_tech = VALUE #(  (  client =  sy-mandt
                            technician_id = 'T0000001'
                            name          = 'Luis Angel Rodriguez Fernandez'
                            speciality     = 'Reparación de HW'
                            )

                             (  client =  sy-mandt
                            technician_id = 'T0000002'
                            name          = 'Efrain Hernandez Hernandez'
                            speciality     = 'Diag.reparación SW'
                            )

                                 (  client =  sy-mandt
                            technician_id = 'T0000003'
                            name          = 'Alicia Flores Gonzalez'
                            speciality     = 'Recup.datos móviles'
                            )

                                          (  client =  sy-mandt
                            technician_id = 'T0000004'
                            name          = 'Javier Balboa Cruz'
                            speciality    = 'Soporte disp.moviles'
                            )
                            ).

*                             Modify zttechnician_jcp from table @lt_tech.


delete from ztwork_order_jcp.


  endmethod.
ENDCLASS.
