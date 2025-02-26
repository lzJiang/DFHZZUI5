FUNCTION zzui5_fg_odata_0028.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(REQUEST_PARAMETER) TYPE  STRING
*"  EXPORTING
*"     VALUE(RETURN_CODE) TYPE  STRING
*"     VALUE(RETURN_MESSAGE) TYPE  STRING
*"     VALUE(RETURN_RESULT) TYPE  STRING
*"----------------------------------------------------------------------
  TYPES BEGIN OF ty_input.
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
  TYPES END OF ty_input.

  DATA:ls_req   TYPE ty_input,
       lt_input TYPE TABLE OF zztmm_0004.
  DATA:lv_where TYPE string,
       lv_flag  TYPE bapi_mtype,
       lv_msg   TYPE bapi_msg.

  "3. JSON报文转换为结构
  CALL METHOD /ui2/cl_json=>deserialize(
    EXPORTING
      json        = request_parameter
      pretty_name = /ui2/cl_json=>pretty_mode-none "格式化参数，NONE：字段名称大写
    CHANGING
      data        = ls_req ). "数据源CREATE OBJECT json_des.

  lt_input = ls_req-item[].
  IF lt_input[] IS NOT INITIAL.
    LOOP AT lt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
      <fs_input>-outbillno   = ls_req-outbillno.
      <fs_input>-documentdate = ls_req-documentdate.
      <fs_input>-postingdate = ls_req-postingdate.
    ENDLOOP.
    zclui5_odata_util_0007=>check( CHANGING lt_input = lt_input
                                           flag = lv_flag
                                           msg  = lv_msg ).

    IF lv_flag NE 'E'.
      zclui5_odata_util_0007=>post( CHANGING lt_input = lt_input
                                         flag = lv_flag
                                         msg  = lv_msg ).
    ELSE.
      LOOP AT lt_input ASSIGNING <fs_input>.
        <fs_input>-flag = lv_flag.
        <fs_input>-msg  = lv_msg.
      ENDLOOP.
    ENDIF.
  ELSE.
    return_code = 'E'.
    return_message = '传入值为空'.
    RETURN.
  ENDIF.
  return_code = 'S'.
  return_message = '处理完成'.
  DATA(ls_out) = ls_req.
  ls_out-item = lt_input[].
  READ TABLE lt_input INTO DATA(ls_input_1) INDEX 1.
  IF sy-subrc = 0.
    ls_out-materialdocument = ls_input_1-materialdocument.
    ls_out-flag = ls_input_1-flag.
    ls_out-msg = ls_input_1-msg.
  ENDIF.
*----------------------------------------------------------------------*
* 生成传输JSON
*----------------------------------------------------------------------*
*&---序列化JSON SERIALIZE JSON
  /ui2/cl_json=>serialize(
            EXPORTING
                    data          = ls_out
                    compress      = abap_false
                    pretty_name   = /ui2/cl_json=>pretty_mode-low_case
            RECEIVING
                    r_json        = return_result ).






ENDFUNCTION.
