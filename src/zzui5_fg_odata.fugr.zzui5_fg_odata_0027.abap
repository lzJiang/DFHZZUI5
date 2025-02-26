FUNCTION zzui5_fg_odata_0027.
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
          outbillno         TYPE zztmm_0004-outbillno,
          plant             TYPE zztmm_0004-plant,
          material          TYPE zztmm_0004-material,
          batchwms          TYPE zztmm_0004-batchwms,
          goodsmovementtype TYPE zztmm_0004-goodsmovementtype,
          created_datestart TYPE zztmm_0004-created_date,
          created_dateend   TYPE zztmm_0004-created_date,
        END OF ty_input.
  TYPES BEGIN OF ty_output.
  TYPES: outbillno         TYPE zztmm_0004-outbillno,
         materialdocument  TYPE zztmm_0004-materialdocument,
         documentdate      TYPE zztmm_0004-documentdate,
         postingdate       TYPE zztmm_0004-postingdate,
         plant             TYPE zztmm_0004-plant,
         goodsmovementtype TYPE zztmm_0004-goodsmovementtype,
         created_date      TYPE zztmm_0004-created_date,
         created_by        TYPE zztmm_0004-created_by,
         flag              TYPE zztmm_0004-flag,
         msg               TYPE zztmm_0004-msg,
         item              TYPE zztmm_0004_tab.
  TYPES END OF ty_output.

  DATA:ls_input TYPE ty_input,
       lt_out   TYPE TABLE OF ty_output.
  DATA:lv_where TYPE string.

  lv_where = | materialdocument ne '' and flag = 'S'|.

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
  IF ls_input-plant IS NOT INITIAL.
    lv_where = | { lv_where } and plant like '%{ ls_input-plant }%'|.
  ENDIF.
  IF ls_input-material IS NOT INITIAL.
    lv_where = | { lv_where } and material like '%{ ls_input-material }%'|.
  ENDIF.
  IF ls_input-batchwms IS NOT INITIAL.
    lv_where = | { lv_where } and batchwms like '%{ ls_input-batchwms }%'|.
  ENDIF.
  IF ls_input-goodsmovementtype IS NOT INITIAL.
    lv_where = | { lv_where } and goodsmovementtype like '%{ ls_input-goodsmovementtype }%'|.
  ENDIF.
  IF ls_input-created_datestart IS NOT INITIAL.
    lv_where = | { lv_where } and created_date >= '{ ls_input-created_datestart }'|.
  ENDIF.
  IF ls_input-created_dateend IS NOT INITIAL.
    lv_where = | { lv_where } and created_date <= '{ ls_input-created_dateend }'|.
  ENDIF.

  SELECT DISTINCT outbillno
    FROM zztmm_0004
    WHERE (lv_where)
    INTO TABLE @DATA(lt_outbillno).
  IF lt_outbillno[] IS NOT INITIAL.
    SELECT *
    FROM zztmm_0004
    FOR ALL ENTRIES IN @lt_outbillno[]
    WHERE outbillno = @lt_outbillno-outbillno
      AND flag = 'S'
    INTO TABLE @DATA(lt_item).
  ENDIF.

  IF lt_item[] IS NOT INITIAL.
    DATA(lt_head) = lt_item[].
    SORT lt_head BY outbillno.
    DELETE ADJACENT DUPLICATES FROM lt_head COMPARING outbillno.

    LOOP AT lt_head INTO DATA(ls_head).
      APPEND INITIAL LINE TO lt_out ASSIGNING FIELD-SYMBOL(<fs_out>).
      MOVE-CORRESPONDING ls_head TO <fs_out>.
      LOOP AT lt_item INTO DATA(ls_item) WHERE outbillno = ls_head-outbillno.
        APPEND INITIAL LINE TO <fs_out>-item ASSIGNING FIELD-SYMBOL(<fs_item>).
        MOVE-CORRESPONDING ls_item TO <fs_item>.
      ENDLOOP.
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
