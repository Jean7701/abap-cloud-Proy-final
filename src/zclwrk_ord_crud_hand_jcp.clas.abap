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
             created_at    TYPE timestampl,
           END OF ty_work_order.
    INTERFACES if_oo_adt_classrun.

    METHODS: create_work_order
      IMPORTING
                iv_customer_id          TYPE ztwork_order-customer_id
                iv_technician_id        TYPE ztwork_order-technician_id
                iv_priority             TYPE ztwork_order-priority
                iv_description          TYPE ztwork_order-description
      RETURNING VALUE(rv_work_order_id) TYPE ztwork_order-work_order_id.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zclwrk_ord_crud_hand_jcp IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    DATA: lv_country_code TYPE land1 VALUE 'ES'.

    AUTHORITY-CHECK OBJECT '/DMO/TRVL'
    ID '/DMO/CNTRY/'  FIELD lv_country_code
    ID 'ACTVT' FIELD '01'.

    DATA(lv_create_generated)  = COND #( WHEN sy-subrc = 0 THEN abap_true
                                                            ELSE abap_false ).
    IF lv_create_generated eq ABAP_true.
       out->write( | You have autherization to use company code { lv_country_code } on the create operations | ).
    else.
        out->write( | You DON'T have autherization to use company code { lv_country_code } on the create operations | ).
    endif.



    ENDMETHOD.

    METHOD create_work_order.

    ENDMETHOD.



ENDCLASS.
