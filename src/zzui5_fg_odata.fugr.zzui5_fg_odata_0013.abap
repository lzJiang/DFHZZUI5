FUNCTION zzui5_fg_odata_0013.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(REQUEST_PARAMETER) TYPE  STRING
*"  EXPORTING
*"     VALUE(RETURN_CODE) TYPE  STRING
*"     VALUE(RETURN_MESSAGE) TYPE  STRING
*"     VALUE(RETURN_RESULT) TYPE  STRING
*"----------------------------------------------------------------------
  TYPES:BEGIN OF ty_input,
          productionplant        TYPE zc_pp006-productionplant,
          purchaseorder          TYPE string,
          cpname                 TYPE string,
          purchaseorderdatestart TYPE zc_pp006-purchaseorderdate,
          purchaseorderdateend   TYPE zc_pp006-purchaseorderdate,
        END OF ty_input.

  DATA:ls_input TYPE ty_input.
  DATA:lv_year  TYPE string,
       lv_month TYPE string,
       lv_day   TYPE string.
  DATA:lv_where TYPE string,
       lv_in_purchaseorder TYPE string,
       lv_purchaseorder    TYPE zc_pp006-purchaseorder.

  lv_where = | purchaseorder ne ''|.


  "3. JSON报文转换为结构
  CALL METHOD /ui2/cl_json=>deserialize(
    EXPORTING
      json        = request_parameter
      pretty_name = /ui2/cl_json=>pretty_mode-none "格式化参数，NONE：字段名称大写
    CHANGING
      data        = ls_input ). "数据源CREATE OBJECT json_des.

  IF ls_input-purchaseorder IS NOT INITIAL.
    lv_where = | { lv_where } and productionplant = '{ ls_input-productionplant }'|.
  ENDIF.
  IF ls_input-purchaseorder IS NOT INITIAL.
    SPLIT ls_input-purchaseorder AT ';' INTO TABLE DATA(lt_purchaseorder).
    LOOP AT lt_purchaseorder INTO DATA(purchaseorder).
      lv_purchaseorder = |{ purchaseorder ALPHA = IN }|.
      IF lv_in_purchaseorder IS INITIAL.
        lv_in_purchaseorder = |'{ lv_purchaseorder }'|.
      ELSE.
        lv_in_purchaseorder = |{ lv_in_purchaseorder },'{ lv_purchaseorder }'|.
      ENDIF.
    ENDLOOP.
    IF lv_in_purchaseorder IS NOT INITIAL.
      lv_where = | { lv_where } and purchaseorder in ({ lv_in_purchaseorder })|.
    ENDIF.
  ENDIF.
  IF ls_input-cpname IS NOT INITIAL.
    lv_where = | { lv_where } and cpname like '%{ ls_input-cpname }%'|.
  ENDIF.
  IF ls_input-purchaseorderdatestart IS NOT INITIAL.
    lv_where = | { lv_where } and purchaseorderdate >= '{ ls_input-purchaseorderdatestart }'|.
  ENDIF.
  IF ls_input-purchaseorderdateend IS NOT INITIAL.
    lv_where = | { lv_where } and purchaseorderdate <= '{ ls_input-purchaseorderdateend }'|.
  ENDIF.

  SELECT *
    FROM zc_pp006
    WHERE (lv_where)
    INTO TABLE @DATA(lt_out).

  SORT lt_out BY purchaseorder purchaseorderitem reservation reservationitem.
  IF lt_out[] IS NOT INITIAL.
    LOOP AT lt_out ASSIGNING FIELD-SYMBOL(<fs_out>).
      <fs_out>-cp = |{ <fs_out>-cp ALPHA = OUT }|.
      <fs_out>-zj = |{ <fs_out>-zj ALPHA = OUT }|.
    ENDLOOP.

    DATA(lv_lines) = lines( lt_out ).
    return_code = 'S'.
    return_message = |共查询{ lv_lines }条数据！|.
  ELSE.
    return_code = 'E'.
    return_message = '未查询到有效数据！'.
    RETURN.
  ENDIF.
*----------------------------------------------------------------------*
* 生成传输JSON
*----------------------------------------------------------------------*
*&---序列化JSON SERIALIZE JSON
  /ui2/cl_json=>serialize(
            EXPORTING
                    data          = lt_out[]
                    compress      = abap_false
                    pretty_name   = /ui2/cl_json=>pretty_mode-low_case
            RECEIVING
                    r_json        = return_result ).


ENDFUNCTION.
