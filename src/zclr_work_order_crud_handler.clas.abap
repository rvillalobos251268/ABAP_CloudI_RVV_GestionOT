CLASS zclr_work_order_crud_handler DEFINITION
  " Clase de metodos CRUD
  " Creada por ronald Villalobos

  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    " Dimensionamiento de Metodos

    METHODS:
      " Creación de orden
      create_work_order IMPORTING
                          iv_work_order_id TYPE zrve_work_order_id
                          iv_customer_id   TYPE zrve_customer_id
                          iv_technician_id TYPE zrve_technician_id
                          iv_priority      TYPE zrve_priority_code
                          iv_description   TYPE string,

      " Lectura de orden
      read_work_order IMPORTING
                        iv_work_order_id TYPE zrve_work_order_id
                      EXPORTING
                        es_order         TYPE zrt_work_order,

      " Actualización de Orden
      update_work_order IMPORTING
                          iv_work_order_id TYPE zrve_work_order_id
                          iv_status        TYPE zrve_status_code,

      "Borrado de Orden
      delete_work_order IMPORTING
                          iv_work_order_id TYPE zrve_work_order_id
                          iv_status        TYPE zrve_status_code.

ENDCLASS.

CLASS zclr_work_order_crud_handler IMPLEMENTATION.


  METHOD create_work_order.

    DATA(lo_validator) = NEW zclr_work_order_validator( ).
    IF lo_validator->validate_create_order(
         iv_customer_id   = iv_customer_id
         iv_technician_id = iv_technician_id
         iv_priority      = iv_priority ) = abap_true.

      DATA(current_date) = cl_abap_context_info=>get_system_date( ).

      DATA(ls_work_order) = VALUE zrt_work_order(
        work_order_id   = iv_work_order_id
        customer_id     = iv_customer_id
        technician_id   = iv_technician_id
        creation_date   = current_date
        status          = 'PE'
        priority        = iv_priority
        description     = iv_description ).

      INSERT zrt_work_order FROM @ls_work_order.
      COMMIT WORK.

    ENDIF.
  ENDMETHOD.

  METHOD read_work_order.

    SELECT SINGLE FROM zrt_work_order
    FIELDS *
    WHERE work_order_id = @iv_work_order_id
    INTO @es_order.

  ENDMETHOD.

  METHOD update_work_order.

    DATA(lo_validator) = NEW zclr_work_order_validator( ).
    IF lo_validator->validate_update_order(
         iv_work_order_id = iv_work_order_id
         iv_status = iv_status ) = abap_true.

      UPDATE zrt_work_order SET status = @iv_status
        WHERE work_order_id = @iv_work_order_id.
      COMMIT WORK.

    ENDIF.

  ENDMETHOD.

  METHOD delete_work_order.

    DATA(lo_validator) = NEW zclr_work_order_validator( ).
    IF lo_validator->validate_delete_order(
         iv_work_order_id = iv_work_order_id
         iv_status = iv_status ) = abap_true.

      DELETE FROM zrt_work_order
        WHERE work_order_id = @iv_work_order_id.
      COMMIT WORK.

    ENDIF.

  ENDMETHOD.

ENDCLASS.


