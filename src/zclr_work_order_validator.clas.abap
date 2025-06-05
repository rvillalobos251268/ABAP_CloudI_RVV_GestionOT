CLASS zclr_work_order_validator DEFINITION
" Clase de validación creada por Ronald Villalobos

  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
  " Definición de Metodos

    METHODS:
      validate_create_order
        IMPORTING
                  iv_customer_id   TYPE string
                  iv_technician_id TYPE string
                  iv_priority      TYPE string
        RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_update_order
        IMPORTING
                  iv_work_order_id TYPE string
                  iv_status        TYPE string
        RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_delete_order
        IMPORTING
                  iv_work_order_id TYPE string
                  iv_status        TYPE string
        RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_status_and_priority
        IMPORTING
                  iv_status       TYPE string
                  iv_priority     TYPE string
        RETURNING VALUE(rv_valid) TYPE abap_bool.

  PRIVATE SECTION.
  " Definición de Constantes
    CONSTANTS:
      c_valid_status   TYPE string VALUE 'PE CO',  " Status
      c_valid_priority TYPE string VALUE 'A B'.    " Priorities

  " Definición de Metodos
    METHODS:
      check_customer_exists
        IMPORTING iv_customer_id   TYPE string
        RETURNING VALUE(rv_exists) TYPE abap_bool,
      check_technician_exists
        IMPORTING iv_technician_id TYPE string
        RETURNING VALUE(rv_exists) TYPE abap_bool,
      check_order_exists
        IMPORTING iv_work_order_id TYPE string
        RETURNING VALUE(rv_exists) TYPE abap_bool,
      check_order_history
        IMPORTING iv_work_order_id TYPE string
        RETURNING VALUE(rv_exists) TYPE abap_bool.
ENDCLASS.


CLASS zclr_work_order_validator IMPLEMENTATION.


  METHOD validate_create_order.
    DATA(lv_customer_exists) = check_customer_exists( iv_customer_id ).
    IF lv_customer_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    DATA(lv_technician_exists) = check_technician_exists( iv_technician_id ).
    IF lv_technician_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    IF iv_priority NE 'A' or iv_priority NE 'B'. " NOT IN c_valid_priority.
      rv_valid = abap_false.
    ELSE.
      rv_valid = abap_true.
      RETURN.
    ENDIF.


  ENDMETHOD.

  METHOD validate_update_order.
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists IS INITIAL OR iv_status NE 'PE'.
      rv_valid = abap_false.
    ELSE.
      rv_valid = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD validate_delete_order.
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists IS INITIAL OR iv_status NE 'PE'.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    DATA(lv_has_history) = check_order_history( iv_work_order_id ).
    IF lv_has_history = abap_true.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.

  METHOD validate_status_and_priority.
    IF iv_status NE 'A' OR iv_priority NE 'B'.
      rv_valid = abap_false.
    ELSE.
      rv_valid = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD check_customer_exists.
    SELECT SINGLE customer_id FROM zrt_customer
      WHERE customer_id = @iv_customer_id
      INTO @DATA(lv_id).
    rv_exists = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD check_technician_exists.
    SELECT SINGLE technician_id FROM zrt_technician
      WHERE technician_id = @iv_technician_id
      INTO @DATA(lv_id).
    rv_exists = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD check_order_exists.
    SELECT SINGLE work_order_id FROM zrt_work_order
      WHERE work_order_id = @iv_work_order_id
      INTO @DATA(lv_id).
    rv_exists = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD check_order_history.
    SELECT SINGLE history_id FROM zrt_work_order_h
      WHERE work_order_id = @iv_work_order_id
      INTO @DATA(lv_id).
    rv_exists = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

ENDCLASS.

