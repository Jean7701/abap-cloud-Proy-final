CLASS zcl_llena_catalogos DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    TYPES: BEGIN OF ty_wo.
             INCLUDE TYPE ztwork_order_jcp.
    TYPES:   modification_date  TYPE d,
             change_description TYPE c LENGTH 50,
           END OF ty_wo.
    DATA lt_wo   TYPE STANDARD TABLE OF  ty_wo.
    DATA lt_wo_ST TYPE STANDARD TABLE OF  ty_wo.
    DATA lt_cte  TYPE STANDARD TABLE OF ztcustomer_jcp.
    DATA lt_Tech TYPE STANDARD TABLE OF zttechnician_jcp.
    DATA lr_wo   TYPE RANGE OF ztwork_order_jcp-work_order_id.
    DATA lr_cust TYPE RANGE OF ztcustomer_jcp-customer_id.
    DATA lr_tech TYPE RANGE OF zttechnician_jcp-technician_id.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_llena_catalogos IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA: lv_di TYPE d,
          lv_df TYPE d.

    DATA: database_name TYPE string,
          campos        TYPE string,
          cond          TYPE string,
          table         TYPE string.

* parámetros para selección de información
* - - - - - - - - - - -  - - - - - - - - - - -  -- - - - - -
    lv_di = '20250501'.
    lv_df = '20250531'.

*lr_wo = VALUE #( ( sign   = 'I'
*                 option   = 'EQ'
*                 low      = '0000000003' )
*                ).

*lr_cust = VALUE #( ( sign   = 'I'
*                 option   = 'EQ'
*                 low      = '00000002' )
*                ).

*lr_tech = VALUE #( ( sign   = 'I'
*                 option   = 'EQ'
*                 low      = 'T0000004' )
*                ).
* - - - - - - - - - - -  - - - - - - - - - - -  -- - - - - -
    database_name = 'ztwork_order_jcp as a left join ztwrkordhist_jcp as b ON a~work_order_id = b~work_order_id'.
    campos =  'a~work_order_id, a~customer_id, a~technician_id, a~creation_date, a~status, a~priority, a~description, b~modification_date, b~change_description'.
    cond = 'a~work_order_id in @lr_wo and a~creation_date between @lv_di and @lv_df'.

    SELECT FROM (database_name)
     FIELDS (campos)
     WHERE (cond)
    INTO CORRESPONDING FIELDS OF TABLE @lt_wo.

    SORT lt_wo BY work_order_id.

    out->write(
     EXPORTING
     data = lt_wo
     name = 'Orden Trabajo'
    ).
* -----------------Lee clientes -------------------------------
    CLEAR: database_name, campos, cond, table.
    database_name = 'ztcustomer_jcp'.
    campos        = ' client, customer_id, name, address, phone'.
    cond          = 'customer_id in @lr_cust'.

    SELECT FROM (database_name)
     FIELDS (campos)
     WHERE (cond)
     INTO TABLE @lt_cte.

    out->write(
     EXPORTING
     data = lt_cte
     name = 'Lista Clientes'
    ).
** ---------------Lee técnicos ---------------------------------
    CLEAR: database_name, campos, cond, table.
    database_name = 'zttechnician_jcp'.
    campos =  'client, technician_id, name, speciality'.
    cond = 'technician_id in @lr_tech'.

    SELECT FROM (database_name)
     FIELDS (campos)
    WHERE (cond)
    INTO TABLE @lt_tech.

    out->write(
     EXPORTING
     data = lt_tech
     name = 'Lista tecnicos'
    ).

*despliega las tareas de cada uno de los técnicos

    LOOP AT lt_wo ASSIGNING FIELD-SYMBOL(<fs>) GROUP BY <fs>-status.
      CLEAR lt_wo_st.
      LOOP AT GROUP <fs> INTO DATA(lw_wo).
        lt_wo_st = VALUE #( BASE lt_wo_st ( lw_wo ) ).
      ENDLOOP.
      out->write( data = lt_wo_st name = 'STATUS' ).
    ENDLOOP.
    UNASSIGN <fs>.















*    DATA: lt_priority TYPE STANDARD TABLE OF zpriority_jcp.
*    DATA: lt_status   TYPE STANDARD TABLE OF ztstatus_jcp .

*    lt_priority = VALUE #(
*             ( priority_code = 'A'  priority_description = 'High' )
*             ( priority_code = 'B'  priority_description = 'Low' )
*         ) .
*
*    lt_status = VALUE #(
*               ( status_code = 'PR'  status_description = 'Process' )
*               ( status_code = 'CO'  status_description = 'Completed' )
*           ) .
*
**  Modify zpriority_jcp from table @lt_priority.
*  Modify ztstatus_jcp from table @lt_status.
*
** - - - - - - -  - - - - - - - - - -  - - - - - - - - - - - - - - - -}

*data: lt_order type standard table of ztwork_order_jcp.
****data lv_max type ztwork_order_jcp-work_order_id.
****
****select MAX( DISTINCT work_order_id )
****from ztwork_order_jcp
****into  @lv_max .
****
****
****    DATA: lt_cliente TYPE TABLE OF ztcustomer_jcp.
****    lt_cliente = VALUE #(  (  client =  sy-mandt
****                            customer_id = '00000005'
****                            name        = 'Efrain Hernandez Hernandez'
****                            address     = 'Calle Miguel Hidalgo No. 44, Tlaxcala Tlax.'
****                            phone       = '2464665789'
****                            )
*
*                            (  client =  sy-mandt
*                            customer_id = '00000006'
*                            name        = 'Immer Hernandez Solis'
*                            address     = 'Av.Ferrocarril Mexicano No 33, Yauquemecan Tlax.'
*                            phone       = '2418235768'
*                            )
*
*                           (  client =  sy-mandt
*                            customer_id = '00000007'
*                            name        = 'Omar Xoca Garcia'
*                            address     = 'Av.Josefa Ortiz No.14, Amaxac de Gro.Tlax.'
*                            phone       = '2416789065'
*                            )
*
*                            (  client =  sy-mandt
*                            customer_id = '00000008'
*                            name        = 'Elia de la fuente'
*                            address     = 'Av. Independencia No.1,Sn.Bernardino Tlax.'
*                            phone       = '2468976434'
*                            )
*                               ).
*
*                            Modify ztcustomer_jcp from table @lt_cliente.
**
*
**  - - - - - - -  - - - - - - - - - -  - - - - - - - - - - - - - - - -
*
*    DATA: lt_tech TYPE TABLE OF zttechnician_jcp.
*    lt_tech = VALUE #(  (  client =  sy-mandt
*                            technician_id = 'T0000001'
*                            name          = 'Luis Angel Rodriguez Fernandez'
*                            speciality     = 'Reparación de HW'
*                            )
*
*                             (  client =  sy-mandt
*                            technician_id = 'T0000002'
*                            name          = 'Efrain Hernandez Hernandez'
*                            speciality     = 'Diag.reparación SW'
*                            )
*
*                                 (  client =  sy-mandt
*                            technician_id = 'T0000003'
*                            name          = 'Alicia Flores Gonzalez'
*                            speciality     = 'Recup.datos móviles'
*                            )
*
*                                          (  client =  sy-mandt
*                            technician_id = 'T0000004'
*                            name          = 'Javier Balboa Cruz'
*                            speciality    = 'Soporte disp.moviles'
*                            )
*                            ).
*
**                             Modify zttechnician_jcp from table @lt_tech.

*
**delete from ztwork_order_jcp.
*   DATA: r_valid_priority    TYPE RANGE OF ztpriority_jcp-priority_code,
*         LR_PR LIKE LINE OF r_valid_priority.
*
*    SELECT client, priority_code, priority_description
*    FROM zpriority_jcp
*    INTO TABLE @lt_priority.
*
**    LOOP AT lt_priority INTO DATA(LW_PRI).
***      r_valid_priority[] = VALUE #(
***       (
**        LR_PR-sign = 'I'. LR_PR-option = 'EQ'. LR_PR-Low = LW_PRI-priority_code.
**        APPEND LR_PR TO r_valid_priority.
***        )
***     ) .
**
**    ENDLOOP.
*
*    LOOP AT lt_priority INTO DATA(LW_PRI).
**      r_valid_priority[] = VALUE #(
**       (
*        LR_PR-sign = 'I'. LR_PR-option = 'EQ'. LR_PR-Low = LW_PRI-priority_code.
*        APPEND LR_PR TO r_valid_priority.
**        )
**     ) .
*
*    ENDLOOP.




*    AUTHORITY-CHECK OBJECT 'ZAOWO_ID'
*                        ID 'ZAFWO_ID' FIELD '0000000001'
*                        ID 'ACTVT'    FIELD '01'.
*        DATA(lv_aut) = COND #( WHEN sy-subrc = 0
*                               THEN abap_true
*                               ELSE abap_false ).
*
*        IF lv_aut = abap_true.
*          out->write( 'SI' ).
*        ELSE.
*          out->write( 'NO' ).
*          EXIT.
*        ENDIF.
* out->write( 'NO' ).



* UPDATE ztwork_order_jcp
*         SET STATUS    = 'PE'
*         WHERE work_order_id  = '0000000002'
*          AND  customer_id    = '00000003'
*          AND  technician_id  = 'T0000002'.
*          IF SY-SUBRC EQ 0.
*            out->write( 'SI' ).
*          ENDIF.

*DELETE FROM ztwrkordhist_jcp.

  ENDMETHOD.
ENDCLASS.
