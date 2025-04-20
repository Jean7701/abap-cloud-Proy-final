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
                  iv_customer_id   TYPE ztwork_order-customer_id
                  iv_technician_id TYPE ztwork_order-technician_id
                  iv_priority      TYPE ztwork_order-priority
                  iv_description   TYPE ztwork_order-description
        RETURNING VALUE(rv_work_order_id) TYPE ztwork_order-work_order_id.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zclwrk_ord_crud_hand_jcp IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
  ENDMETHOD.

  METHOD create_work_order.

  ENDMETHOD.



ENDCLASS.
