FUNCTION zzui5_fg_odata_0011.
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
          manufacturingorder TYPE zc_pp004-manufacturingorder,
        END OF ty_input.
  TYPES:BEGIN OF ty_out,
          manufacturingorder TYPE zc_pp004-manufacturingorder,
          page               TYPE i,
          title              TYPE string,
          scpc               TYPE string,
          scjh               TYPE string,
          jhksrq             TYPE string,
          cpgg               TYPE string,
          yjwgsj             TYPE string,
          yltr               TYPE string,
          fltr               TYPE string,
          jhscsl             TYPE string,
          bctr               TYPE string,
          pzr                TYPE string,
          shr                TYPE string,
          zdr                TYPE string,
          flag               TYPE string,
          msg                TYPE string,
        END OF ty_out.

  DATA:lt_input TYPE TABLE OF ty_input,
       lt_out   TYPE TABLE OF ty_out.
  DATA:lv_year  TYPE string,
       lv_month TYPE string,
       lv_day   TYPE string.


  "3. JSON报文转换为结构
  CALL METHOD /ui2/cl_json=>deserialize(
    EXPORTING
      json        = request_parameter
      pretty_name = /ui2/cl_json=>pretty_mode-none "格式化参数，NONE：字段名称大写
    CHANGING
      data        = lt_input ). "数据源CREATE OBJECT json_des.

  IF lt_input[] IS INITIAL.
    return_code = 'E'.
    return_message = '传入订单号为空！'.
    RETURN.
  ENDIF.
  LOOP AT lt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
    <fs_input>-manufacturingorder = |{ <fs_input>-manufacturingorder ALPHA = IN }|.
  ENDLOOP.

  SELECT *
    FROM zc_pp004
    FOR ALL ENTRIES IN @lt_input
    WHERE manufacturingorder = @lt_input-manufacturingorder
    INTO TABLE @DATA(lt_pp004).


  DATA(lt_head) = lt_pp004.
  SORT lt_head BY manufacturingorder.
  DELETE ADJACENT DUPLICATES FROM lt_head COMPARING manufacturingorder.

  LOOP AT lt_head INTO DATA(ls_head).
    CLEAR:lv_year,lv_month,lv_day.
    APPEND INITIAL LINE TO lt_out ASSIGNING FIELD-SYMBOL(<fs_out>).
    <fs_out>-page = sy-tabix.
    <fs_out>-manufacturingorder = |{ ls_head-manufacturingorder ALPHA = OUT }|.
    <fs_out>-title = |{ ls_head-cpname }生产通知单|.
    <fs_out>-scpc = |{ ls_head-yy1_mfgbatch_ord }|.
    lv_year = ls_head-mfgordercreationdate+0(4).
    lv_month = ls_head-mfgordercreationdate+4(2).
    lv_month = |{ lv_month ALPHA = OUT }|.
    <fs_out>-scjh = |{ lv_year }年{ lv_month }月|.
    lv_year = ls_head-mfgorderplannedstartdate+0(4).
    lv_month = ls_head-mfgorderplannedstartdate+4(2).
    lv_month = |{ lv_month ALPHA = OUT }|.
    lv_day = ls_head-mfgorderplannedstartdate+6(2).
    lv_day = |{ lv_day ALPHA = OUT }|.
    <fs_out>-jhksrq = |{ lv_year }年{ lv_month }月{ lv_day }日|.
    <fs_out>-cpgg = |{ ls_head-sizeordimensiontext }|.
    lv_year = ls_head-mfgorderplannedenddate+0(4).
    lv_month = ls_head-mfgorderplannedenddate+4(2).
    lv_month = |{ lv_month ALPHA = OUT }|.
    lv_day = ls_head-mfgorderplannedenddate+6(2).
    lv_day = |{ lv_day ALPHA = OUT }|.
    <fs_out>-yjwgsj = |{ lv_year }年{ lv_month }月{ lv_day }日|.
    LOOP AT lt_pp004 INTO DATA(ls_pp004) WHERE manufacturingorder = ls_head-manufacturingorder.
      CASE ls_pp004-zjgroup.
        WHEN '3001'.
          IF <fs_out>-yltr IS INITIAL.
            <fs_out>-yltr = |{ ls_pp004-zjname }:{ ls_pp004-zjqty }{ ls_pp004-zjunit }|.
          ELSE.
            <fs_out>-yltr = |{ <fs_out>-yltr }<br>{ ls_pp004-zjname }:{ ls_pp004-zjqty }{ ls_pp004-zjunit }|.
          ENDIF.
        WHEN '3002'.
          IF <fs_out>-yltr IS INITIAL.
            <fs_out>-fltr = |{ ls_pp004-zjname }:{ ls_pp004-zjqty }{ ls_pp004-zjunit }|.
          ELSE.
            <fs_out>-fltr = |{ <fs_out>-fltr }<br>{ ls_pp004-zjname }:{ ls_pp004-zjqty }{ ls_pp004-zjunit }|.
          ENDIF.
        WHEN '4001'.
          IF <fs_out>-bctr IS INITIAL.
            <fs_out>-bctr = |{ ls_pp004-zjname }:{ ls_pp004-zjqty }{ ls_pp004-zjunit }|.
          ELSE.
            <fs_out>-bctr = |{ <fs_out>-bctr }<br>{ ls_pp004-zjname }:{ ls_pp004-zjqty }{ ls_pp004-zjunit }|.
          ENDIF.
      ENDCASE.
    ENDLOOP.
    <fs_out>-jhscsl = |{ ls_head-cpqty }{ ls_head-cpunit }|.
    <fs_out>-pzr = |{ ls_head-pzr }|.
    <fs_out>-shr = |{ ls_head-shr }|.
    <fs_out>-zdr = |{ ls_head-zdr }|.
    IF ls_head-orderisreleased IS INITIAL.
      <fs_out>-flag = 'E'.
      <fs_out>-msg = |生产任务单{ <fs_out>-manufacturingorder }尚未下达|.
    ELSE.
      <fs_out>-flag = 'S'.
      <fs_out>-msg = '打印成功'.
    ENDIF.
  ENDLOOP.

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
