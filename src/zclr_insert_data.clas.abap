CLASS zclr_insert_data DEFINITION

" " Clase de insert de datos secundarios creada por Ronald Villalobos

  PUBLIC
  FINAL
  CREATE PUBLIC .


  PUBLIC SECTION.

    INTERFACES: if_oo_ADT_CLASSRUN.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zclr_insert_data IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

" Insert de datos de tecnico para pruebas
   DATA(ls_technician)  = VALUE zrt_technician(
          technician_id = 'TECH01'
          name     =  'Ronald Villalobos'
          specialty = 'Programing JDE'
          ).

   INSERT zrt_technician FROM @ls_technician.
   COMMIT WORK.

" Insert de datos de cliente para pruebas
    DATA(ls_customer)  = VALUE zrt_customer(
           customer_id = '2512681'
           name     =  'GFI Pharma Logistic'
           address = 'Sanb Jose, Costa Rica'
           phone = '506-8888-8888'
           ).

    INSERT zrt_customer FROM @ls_customer.
    COMMIT WORK.

" Insert de datos de prioridades para pruebas
 DATA(ls_priority)  = VALUE zrt_priority(
           priority_code = 'A'
           priority_description     =  'High'
           ).

    INSERT zrt_priority FROM @ls_priority.
    COMMIT WORK.

 DATA(ls_priority2)  = VALUE zrt_priority(
           priority_code = 'B'
           priority_description     =  'Low'
           ).

    INSERT zrt_priority FROM @ls_priority2.
    COMMIT WORK.

" Insert de datos de status pra pruebas
 DATA(ls_status)  = VALUE zrt_status(
           status_code = 'PE'
           status_description     =  'Peding'
           ).

    INSERT zrt_status FROM @ls_status.
    COMMIT WORK.

DATA(ls_status2)  = VALUE zrt_status(
           status_code = 'CO'
           status_description     =  'Completed'
           ).

    INSERT zrt_status FROM @ls_status2.
    COMMIT WORK.


  ENDMETHOD.

ENDCLASS.
