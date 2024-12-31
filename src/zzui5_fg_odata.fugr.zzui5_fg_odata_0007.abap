FUNCTION zzui5_fg_odata_0007.
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
          productionplant           TYPE zc_pp003-productionplant,
          plannedorder              TYPE zc_pp003-plannedorder,
          material                  TYPE zc_pp003-material,
          productname               TYPE zc_pp003-productname,
          yy1_plannebatch_pla       TYPE zc_pp003-yy1_plannebatch_pla,
          plndorderplannedstartdate TYPE zc_pp003-plndorderplannedstartdate,
          plndorderplannedenddate   TYPE zc_pp003-plndorderplannedenddate,
          mrpcontroller             TYPE zc_pp003-mrpcontroller,
        END OF ty_input.

  DATA:ls_input TYPE ty_input.
  DATA:lv_where TYPE string.

  lv_where = | plannedorder ne ''|.

  "3. JSON报文转换为结构
  CALL METHOD /ui2/cl_json=>deserialize(
    EXPORTING
      json        = request_parameter
      pretty_name = /ui2/cl_json=>pretty_mode-none "格式化参数，NONE：字段名称大写
    CHANGING
      data        = ls_input ). "数据源CREATE OBJECT json_des.

  IF ls_input-productionplant IS NOT INITIAL.
    lv_where = | { lv_where } and productionplant like '%{ ls_input-productionplant }%'|.
  ENDIF.
  IF ls_input-plannedorder IS NOT INITIAL.
    lv_where = | { lv_where } and plannedorder like '%{ ls_input-plannedorder }%'|.
  ENDIF.
  IF ls_input-yy1_plannebatch_pla IS NOT INITIAL.
    lv_where = | { lv_where } and yy1_plannebatch_pla like '%{ ls_input-yy1_plannebatch_pla }%'|.
  ENDIF.
  IF ls_input-material IS NOT INITIAL.
    lv_where = | { lv_where } and material like '%{ ls_input-material }%'|.
  ENDIF.
  IF ls_input-productname IS NOT INITIAL.
    lv_where = | { lv_where } and productname like '%{ ls_input-productname }%'|.
  ENDIF.
  IF ls_input-mrpcontroller IS NOT INITIAL.
    lv_where = | { lv_where } and mrpcontroller like '%{ ls_input-mrpcontroller }%'|.
  ENDIF.
  IF ls_input-plndorderplannedstartdate IS NOT INITIAL.
    lv_where = | { lv_where } and plndorderplannedstartdate >= '{ ls_input-plndorderplannedstartdate }'|.
  ENDIF.
  IF ls_input-plndorderplannedenddate IS NOT INITIAL.
    lv_where = | { lv_where } and plndorderplannedstartdate <= '{ ls_input-plndorderplannedenddate }'|.
  ENDIF.

  SELECT *
    FROM zc_pp003
    WHERE (lv_where)
    INTO TABLE @DATA(lt_out).

  SORT lt_out BY plannedorder.
  IF lt_out[] IS NOT INITIAL.
    LOOP AT lt_out ASSIGNING FIELD-SYMBOL(<fs_out>).
      <fs_out>-plannedorder = |{ <fs_out>-plannedorder ALPHA = OUT }|.
      <fs_out>-material = |{ <fs_out>-material ALPHA = OUT }|.
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
