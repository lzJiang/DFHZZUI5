FUNCTION zzui5_fg_odata_0016.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(REQUEST_PARAMETER) TYPE  STRING
*"  EXPORTING
*"     VALUE(RETURN_CODE) TYPE  STRING
*"     VALUE(RETURN_MESSAGE) TYPE  STRING
*"     VALUE(RETURN_RESULT) TYPE  STRING
*"----------------------------------------------------------------------
  DATA:ls_input TYPE zzs_ui50016_in.
  DATA:lv_where TYPE string.

  CLEAR:gv_flag,gv_msg.

  "3. JSON报文转换为结构
  CALL METHOD /ui2/cl_json=>deserialize(
    EXPORTING
      json        = request_parameter
      pretty_name = /ui2/cl_json=>pretty_mode-none "格式化参数，NONE：字段名称大写
    CHANGING
      data        = ls_input ). "数据源CREATE OBJECT json_des.


  IF ls_input IS NOT INITIAL.
    zclui5_odata_util_0003=>check( CHANGING ls_input = ls_input
                                         flag = ls_input-yy1_flag
                                         msg  = ls_input-yy1_msg ).
    IF ls_input-yy1_flag NE 'E'.
      zclui5_odata_util_0003=>set( CHANGING ls_input = ls_input
                                   flag = ls_input-yy1_flag
                                   msg  = ls_input-yy1_msg ).
    ENDIF.
  ELSE.
    return_code = 'E'.
    return_message = '传入值为空'.
    RETURN.
  ENDIF.
  return_code = 'S'.
  return_message = '处理完成'.
  data(ls_out) = ls_input.
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
