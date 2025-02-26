FUNCTION zzui5_fg_odata_0025.
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
          outbillno                      TYPE zzt_pp_005-outbillno,
          productionplant                TYPE zzt_pp_005-productionplant,
          product                        TYPE zzt_pp_005-product,
          plndorderplannedstartdatestart TYPE zzt_pp_005-plndorderplannedstartdate,
          plndorderplannedstartdateend   TYPE zzt_pp_005-plndorderplannedstartdate,
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

  IF ls_input-outbillno IS NOT INITIAL.
    lv_where = | { lv_where } and outbillno like '%{ ls_input-outbillno }%'|.
  ENDIF.
  IF ls_input-productionplant IS NOT INITIAL.
    lv_where = | { lv_where } and productionplant like '%{ ls_input-productionplant }%'|.
  ENDIF.
  IF ls_input-product IS NOT INITIAL.
    lv_where = | { lv_where } and product like '%{ ls_input-product }%'|.
  ENDIF.
  IF ls_input-plndorderplannedstartdatestart IS NOT INITIAL.
    lv_where = | { lv_where } and plndorderplannedstartdate >= '{ ls_input-plndorderplannedstartdatestart }'|.
  ENDIF.
  IF ls_input-plndorderplannedstartdateend IS NOT INITIAL.
    lv_where = | { lv_where } and plndorderplannedstartdate <= '{ ls_input-plndorderplannedstartdateend }'|.
  ENDIF.

  SELECT *
    FROM zzt_pp_005
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
