FUNCTION zzui5_fg_odata_0023.
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
          material         TYPE zztmm_0003-material,
          batch            TYPE zztmm_0003-batch,
          zwmsbatch        TYPE zztmm_0003-zwmsbatch,
          created_datestart TYPE zztmm_0003-created_date,
          created_dateend  TYPE zztmm_0003-created_date,
        END OF ty_input.

  DATA:ls_input TYPE ty_input.
  DATA:lv_where TYPE string.

  lv_where = | flag = 'S'|.

  "3. JSON报文转换为结构
  CALL METHOD /ui2/cl_json=>deserialize(
    EXPORTING
      json        = request_parameter
      pretty_name = /ui2/cl_json=>pretty_mode-low_case "格式化参数，NONE：字段名称大写
    CHANGING
      data        = ls_input ). "数据源CREATE OBJECT json_des.

  IF ls_input-material IS NOT INITIAL.
    lv_where = | { lv_where } and material like '%{ ls_input-material }%'|.
  ENDIF.
  IF ls_input-batch IS NOT INITIAL.
    lv_where = | { lv_where } and batch like '%{ ls_input-batch }%'|.
  ENDIF.
  IF ls_input-zwmsbatch IS NOT INITIAL.
    lv_where = | { lv_where } and zwmsbatch like '%{ ls_input-zwmsbatch }%'|.
  ENDIF.
  IF ls_input-created_datestart IS NOT INITIAL.
    lv_where = | { lv_where } and created_date >= '{ ls_input-created_datestart }'|.
  ENDIF.
  IF ls_input-created_dateend IS NOT INITIAL.
    lv_where = | { lv_where } and created_date <= '{ ls_input-created_dateend }'|.
  ENDIF.

  SELECT *
    FROM zztmm_0003
    WHERE (lv_where)
    INTO TABLE @DATA(lt_out).


  IF lt_out[] IS NOT INITIAL.
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
