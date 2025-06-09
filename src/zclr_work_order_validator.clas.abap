CLASS zclr_work_order_validator DEFINITION
" Clase de validación creada por Ronald Villalobos

  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    " Definición de Metodos

    METHODS:
      validate_create_order IMPORTING
                                      iv_customer_id   TYPE zrve_customer_id
                                      iv_technician_id TYPE zrve_technician_id
                                      iv_priority      TYPE zrve_priority_code
                            EXPORTING ev_error         TYPE string
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_update_order IMPORTING
                                      iv_work_order_id TYPE zrve_work_order_id
                                      iv_status        TYPE zrve_status_code
                            EXPORTING ev_error         TYPE string
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_delete_order IMPORTING
                                      iv_work_order_id TYPE zrve_work_order_id
                                      iv_status        TYPE zrve_status_code
                            EXPORTING ev_error         TYPE string
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_status_and_priority IMPORTING
                                             iv_status       TYPE zrve_status_code
                                             iv_priority     TYPE zrve_priority_code
                                   EXPORTING ev_error        TYPE string
                                   RETURNING VALUE(rv_valid) TYPE abap_bool.



  PRIVATE SECTION.
    " Definición de Constantes
    CONSTANTS:
      c_valid_status   TYPE string VALUE 'PE CO',  " Status
      c_valid_priority TYPE string VALUE 'A B'.    " Priorities

    " Definición de Metodos
    METHODS:
      check_customer_exists IMPORTING iv_customer_id   TYPE zrve_customer_id
                            RETURNING VALUE(rv_exists) TYPE abap_bool,

      check_technician_exists IMPORTING iv_technician_id TYPE zrve_technician_id
                              RETURNING VALUE(rv_exists) TYPE abap_bool,

      check_order_exists IMPORTING iv_work_order_id TYPE zrve_work_order_id
                         RETURNING VALUE(rv_exists) TYPE abap_bool,

      check_order_history IMPORTING iv_work_order_id TYPE zrve_work_order_id
                          RETURNING VALUE(rv_exists) TYPE abap_bool.


ENDCLASS.


CLASS zclr_work_order_validator IMPLEMENTATION.


  METHOD validate_create_order.


    TYPES: tt_priority TYPE TABLE OF zrve_priority_code WITH EMPTY KEY.

    DATA lv_priorities TYPE tt_priority.
    lv_priorities = VALUE #( ( 'A' ) ( 'B' ) ).

    " Inicializo la variable de validación
    rv_valid = abap_true.
    ev_error = ''.

    " Valido customer
    IF check_customer_exists( iv_customer_id ) = abap_false.
      ev_error = |Customer { iv_customer_id } does not exist|.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Valido technician
    IF check_technician_exists( iv_technician_id ) = abap_false.
      ev_error = |Technician { iv_technician_id } does not exist|.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    "    Valido prioridad usando tabla tipada
    IF NOT line_exists( lv_priorities[ table_line = iv_priority ] ).
      ev_error = |Priority { iv_priority } is invalid|.
      rv_valid = abap_false.
      RETURN.
    ENDIF.


  ENDMETHOD.

  METHOD validate_status_and_priority.

    TYPES: tt_status   TYPE TABLE OF zrve_status_code WITH EMPTY KEY,
           tt_priority TYPE TABLE OF zrve_priority_code WITH EMPTY KEY.


    DATA lv_statuses   TYPE tt_status.
    lv_statuses =  VALUE #( ( 'PE' ) ( 'CO' ) ).
    DATA lv_priorities TYPE tt_priority.
    lv_priorities = VALUE #( ( 'A' ) ( 'B' ) ).


    rv_valid = abap_true.
    ev_error = ''.

    IF NOT line_exists( lv_statuses[ table_line = iv_status ] ).
      ev_error = |Estado { iv_status } no valido|.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    IF NOT line_exists( lv_priorities[ table_line = iv_priority ] ).
      ev_error = |Priority { iv_priority } is invalid|.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD validate_update_order.

    rv_valid = abap_true.
    ev_error = ''.

    IF check_order_exists( iv_work_order_id ) = abap_false.
      ev_error = |Order { iv_work_order_id } does not exist|.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    IF iv_status NE 'PE'.
      ev_error = |Orden no esta en Estado 'PE' no se puede actualizar|.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD validate_delete_order.

    rv_valid = abap_true.
    ev_error = ''.

    IF check_order_exists( iv_work_order_id ) = abap_false.
      ev_error = |Order { iv_work_order_id } no existe|.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    IF iv_status NE 'PE'.
      ev_error = |Orden no esta en estado 'PE' no se puede borrar|.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    IF check_order_history( iv_work_order_id ) = abap_true.
      ev_error = |Orden { iv_work_order_id } tiene datos historicos|.
      rv_valid = abap_false.
      RETURN.
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

