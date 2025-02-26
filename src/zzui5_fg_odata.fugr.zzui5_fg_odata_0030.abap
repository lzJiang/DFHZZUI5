FUNCTION zzui5_fg_odata_0030.
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
          purchaseorderbycustomer TYPE zztsd_0003-purchaseorderbycustomer,
          salesorganization       TYPE zztsd_0003-salesorganization,
          productionplant         TYPE zztsd_0003-productionplant,
          material                TYPE zztsd_0003-material,
          batchwms                TYPE zztsd_0003-batchwms,
          salesordertype          TYPE zztsd_0003-salesordertype,
          soldtoparty             TYPE zztsd_0003-soldtoparty,
          created_datestart       TYPE zztsd_0003-created_date,
          created_dateend         TYPE zztsd_0003-created_date,
        END OF ty_input.


  DATA:ls_input TYPE ty_input,
       lt_out   TYPE zztsd_0003_tab.
  DATA:lv_where TYPE string.

  lv_where = | salesorder ne '' and flag = 'S'|.

  "3. JSON报文转换为结构
  CALL METHOD /ui2/cl_json=>deserialize(
    EXPORTING
      json        = request_parameter
      pretty_name = /ui2/cl_json=>pretty_mode-low_case "格式化参数，NONE：字段名称大写
    CHANGING
      data        = ls_input ). "数据源CREATE OBJECT json_des.

  IF ls_input-purchaseorderbycustomer IS NOT INITIAL.
    lv_where = | { lv_where } and purchaseorderbycustomer like '%{ ls_input-purchaseorderbycustomer }%'|.
  ENDIF.
  IF ls_input-salesorganization IS NOT INITIAL.
    lv_where = | { lv_where } and salesorganization like '%{ ls_input-salesorganization }%'|.
  ENDIF.
  IF ls_input-productionplant IS NOT INITIAL.
    lv_where = | { lv_where } and productionplant like '%{ ls_input-productionplant }%'|.
  ENDIF.
  IF ls_input-material IS NOT INITIAL.
    lv_where = | { lv_where } and material like '%{ ls_input-material }%'|.
  ENDIF.
  IF ls_input-batchwms IS NOT INITIAL.
    lv_where = | { lv_where } and batchwms like '%{ ls_input-batchwms }%'|.
  ENDIF.
  IF ls_input-salesordertype IS NOT INITIAL.
    lv_where = | { lv_where } and salesordertype like '%{ ls_input-salesordertype }%'|.
  ENDIF.
  IF ls_input-soldtoparty IS NOT INITIAL.
    lv_where = | { lv_where } and soldtoparty like '%{ ls_input-soldtoparty }%'|.
  ENDIF.
  IF ls_input-created_datestart IS NOT INITIAL.
    lv_where = | { lv_where } and created_date >= '{ ls_input-created_datestart }'|.
  ENDIF.
  IF ls_input-created_dateend IS NOT INITIAL.
    lv_where = | { lv_where } and created_date <= '{ ls_input-created_dateend }'|.
  ENDIF.

  SELECT *
    FROM zztsd_0003
    WHERE (lv_where)
    INTO TABLE @lt_out.

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
