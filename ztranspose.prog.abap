*&---------------------------------------------------------------------*
*& Report ztranspose
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report ztranspose.

class lcl_appl definition create private.

  public section.

    class-methods create
      returning
        value(ro_appl) type ref to lcl_appl.

    methods start_of_selection.

  protected section.

  private section.

    class-data go_appl type ref to lcl_appl.

endclass.

start-of-selection.
  lcl_appl=>create( )->start_of_selection( ).

class lcl_appl implementation.

  method create.

    if go_appl is not bound.
      go_appl = new lcl_appl( ).
    endif.

    ro_appl  = go_appl.

  endmethod.

  method start_of_selection.

    data lt_data_table_transpose type ref to data.

    field-symbols <lv_data_table>           type any.
    field-symbols <lt_data_table_transpose> type table.
    field-symbols <lv_data_table_transpose> type any.

    select * from t000 into table @data(lt_data_table).

    cl_salv_table=>factory(
      importing
        r_salv_table   = data(lo_alv_table)    " Basis Class Simple ALV Tables
      changing
        t_table        = lt_data_table
    ).

    lo_alv_table->display( ).

    data(lt_components_transpose)
        = value cl_abap_structdescr=>component_table(
            (
                name = 'Attributes'
                type = cast #( cl_abap_datadescr=>describe_by_name( p_name = 'CHAR20' ) )
            )
        ).

    lt_components_transpose
        = value cl_abap_structdescr=>component_table(
            base lt_components_transpose
            for i = 1 then i + 1 while i <= lines( lt_data_table )
            (
                name = |Field{ lt_data_table[ i ]-mandt }|
                type = cast cl_abap_datadescr( cl_abap_datadescr=>describe_by_name( p_name = 'CHAR20' ) )
            )
        ).

    data(lo_tabledescr) = cl_abap_tabledescr=>create(
                          p_line_type = cast #( cl_abap_structdescr=>create( p_components = lt_components_transpose ) )
                      ).

    create data lt_data_table_transpose type handle lo_tabledescr.

    assign lt_data_table_transpose->* to <lt_data_table_transpose>.

    loop at cast cl_abap_structdescr( cast cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data( p_data = lt_data_table ) )->get_table_line_type( ) )->get_components( )
        assigning field-symbol(<ls_components>).

      append initial line to <lt_data_table_transpose> assigning field-symbol(<ls_data_table_transpose>).

      assign component 'Attributes' of structure <ls_data_table_transpose> to <lv_data_table_transpose>.

      <lv_data_table_transpose> = <ls_components>-name.

    endloop.

    loop at lt_data_table assigning field-symbol(<ls_data_table>).

      loop at cast cl_abap_structdescr( cast cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data( p_data = lt_data_table ) )->get_table_line_type( ) )->get_components( )
          assigning <ls_components>.

        loop at <lt_data_table_transpose> assigning <ls_data_table_transpose>.

          assign component 'Attributes' of structure <ls_data_table_transpose> to <lv_data_table_transpose>.

          if <lv_data_table_transpose> = <ls_components>-name.

            assign component <ls_components>-name of structure <ls_data_table> to <lv_data_table>.

            assign component |Field{ <ls_data_table>-mandt }| of structure <ls_data_table_transpose> to <lv_data_table_transpose>.

            <lv_data_table_transpose> = <lv_data_table>.

            exit.

          endif.

        endloop.

      endloop.

    endloop.

    cl_salv_table=>factory(
      importing
        r_salv_table   = data(lo_alv_table_transpose)    " Basis Class Simple ALV Tables
      changing
        t_table        = <lt_data_table_transpose>
    ).

    lo_alv_table_transpose->display( ).

  endmethod.

endclass.