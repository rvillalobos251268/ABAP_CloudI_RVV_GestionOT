CLASS zclr_work_order_crud_test DEFINITION
" Clase de pruebas creada por Ronald Villalobos
  PUBLIC
  FINAL

  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES: if_oo_ADT_CLASSRUN.

  PRIVATE SECTION.
    METHODS:
      test_create_work_order,
      test_read_work_order,
      test_update_work_order,
      test_delete_work_order.
ENDCLASS.

CLASS zclr_work_order_crud_test IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

  ""  TRY.

        out->write( '--- PRUEBA: Crear Orden ---' ).
        test_create_work_order( ).
        out->write( 'Orden creada correctamente' ).

        out->write( '--- PRUEBA: Leer Orden ---' ).
        test_read_work_order( ).

        ""        out->write( '--- PRUEBA: Actualizar Orden ---' ).
        ""        test_update_work_order( ).
        ""        out->write( 'Orden actualizada correctamente' ).

        ""        out->write( '--- PRUEBA: Borrar Orden ---' ).
        ""        test_delete_work_order( ).
        ""         out->write( 'Orden borrada correctamente' ).

   ""   CATCH cx_root INTO DATA(lx).
   ""     out->write( 'Error durante la ejecución:' ).
   ""     out->write( lx->get_text( ) ).


    "" ENDTRY.

  ENDMETHOD.


  METHOD test_create_work_order.
    DATA(lo_handler) = NEW zclr_work_order_crud_handler( ).
    lo_handler->create_work_order(
      iv_work_order_id = '252525'
      iv_customer_id   = '2512681'
      iv_technician_id = 'TECH01'
      iv_priority      = 'A'
      iv_description   = 'Instalación inicial' ).
  ENDMETHOD.

  METHOD test_read_work_order.
    DATA: lo_handler TYPE REF TO zclr_work_order_crud_handler,
          ls_order   TYPE zrt_work_order.

    lo_handler = NEW zclr_work_order_crud_handler( ).
    lo_handler->read_work_order(
      EXPORTING iv_work_order_id = '252525'
      IMPORTING es_order = ls_order ).
  ENDMETHOD.

  METHOD test_update_work_order.
    DATA(lo_handler) = NEW zclr_work_order_crud_handler( ).
    lo_handler->update_work_order(
      iv_work_order_id = '252525'
      iv_status        = 'CO' ).
  ENDMETHOD.

  METHOD test_delete_work_order.
    DATA(lo_handler) = NEW zclr_work_order_crud_handler( ).
    lo_handler->delete_work_order(
      iv_work_order_id = '252525'
      iv_status        = 'PE' ).
  ENDMETHOD.


ENDCLASS.

