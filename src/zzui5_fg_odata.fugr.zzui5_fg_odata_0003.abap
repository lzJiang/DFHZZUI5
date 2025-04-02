FUNCTION zzui5_fg_odata_0003.
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
          productionplant          TYPE zc_pp002-productionplant,
          manufacturingorder       TYPE zc_pp002-manufacturingorder,
          manufacturingordertype   TYPE zc_pp002-manufacturingordertype,
          material                 TYPE zc_pp002-material,
          yy1_mfgbatch_ord         TYPE zc_pp002-yy1_mfgbatch_ord,
          mfgorderplannedstartdate TYPE zc_pp002-mfgorderplannedstartdate,
          mfgorderplannedenddate   TYPE zc_pp002-mfgorderplannedenddate,
          productname              TYPE zc_pp002-productname,
          status                   TYPE string,
        END OF ty_input.

  DATA:ls_input TYPE ty_input.
  DATA:lv_where TYPE string.

  lv_where = | manufacturingorder ne ''|.

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
  IF ls_input-manufacturingorder IS NOT INITIAL.
    lv_where = | { lv_where } and manufacturingorder like '%{ ls_input-manufacturingorder }%'|.
  ENDIF.
  IF ls_input-manufacturingordertype IS NOT INITIAL.
    lv_where = | { lv_where } and manufacturingordertypename like '%{ ls_input-manufacturingordertype }%'|.
  ENDIF.
  IF ls_input-material IS NOT INITIAL.
    lv_where = | { lv_where } and material like '%{ ls_input-material }%'|.
  ENDIF.
  IF ls_input-yy1_mfgbatch_ord IS NOT INITIAL.
    lv_where = | { lv_where } and yy1_mfgbatch_ord like '%{ ls_input-yy1_mfgbatch_ord }%'|.
  ENDIF.
  IF ls_input-mfgorderplannedstartdate IS NOT INITIAL.
    lv_where = | { lv_where } and mfgorderplannedstartdate >= '{ ls_input-mfgorderplannedstartdate }'|.
  ENDIF.
  IF ls_input-mfgorderplannedenddate IS NOT INITIAL.
    lv_where = | { lv_where } and mfgorderplannedstartdate <= '{ ls_input-mfgorderplannedenddate }'|.
  ENDIF.
  IF ls_input-productname IS NOT INITIAL.
    lv_where = | { lv_where } and productname like '%{ ls_input-productname }%'|.
  ENDIF.
  IF ls_input-status IS NOT INITIAL AND ls_input-status NE 'orderisll'.
    IF ls_input-status = 'yy1_approvestatus_n'.
      lv_where = | { lv_where } and yy1_approvestatus = 'N'|.
    ELSEIF ls_input-status = 'yy1_approvestatus_y'.
      lv_where = | { lv_where } and yy1_approvestatus = 'Y' and yy1_check is not initial|.
    ELSE.
      lv_where = | { lv_where } and { ls_input-status } = 'X'|.
    ENDIF.
  ENDIF.

  SELECT *
    FROM zc_pp002
    WHERE (lv_where)
    INTO TABLE @DATA(lt_out).

  SORT lt_out BY manufacturingorder.
  IF lt_out[] IS NOT INITIAL.

    LOOP AT lt_out ASSIGNING FIELD-SYMBOL(<fs_out>).
      DATA(lv_tabix) = sy-tabix.
      <fs_out>-manufacturingorder = |{ <fs_out>-manufacturingorder ALPHA = OUT }|.
      <fs_out>-material = |{ <fs_out>-material ALPHA = OUT }|.
      data(lv_aufnr) = |%{ <fs_out>-manufacturingorder }|.
      SELECT SINGLE a~*
        FROM zzt_pp_002_itemd WITH PRIVILEGED ACCESS AS a
       INNER JOIN zzt_pp_002_head WITH PRIVILEGED ACCESS AS b
          ON a~zllno = b~zllno
      WHERE a~manufacturingorder LIKE @lv_aufnr
        AND b~zllzt = 'RELEASE'
       INTO @DATA(ls_zlld).
      IF sy-subrc = 0.
        <fs_out>-orderisll = 'X'.
      ENDIF.
      IF ls_input-status = 'orderisll' AND <fs_out>-orderisll IS INITIAL.
        delete lt_out INDEX lv_tabix.
      ENDIF.
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
