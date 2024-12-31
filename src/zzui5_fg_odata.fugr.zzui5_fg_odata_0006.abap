FUNCTION zzui5_fg_odata_0006.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(REQUEST_PARAMETER) TYPE  STRING
*"  EXPORTING
*"     VALUE(RETURN_CODE) TYPE  STRING
*"     VALUE(RETURN_MESSAGE) TYPE  STRING
*"     VALUE(RETURN_RESULT) TYPE  STRING
*"----------------------------------------------------------------------
  DATA:lt_input TYPE TABLE OF zc_pp002.
  DATA:lv_where TYPE string.

  CLEAR:gv_flag,gv_msg.
  "3. JSON报文转换为结构
  CALL METHOD /ui2/cl_json=>deserialize(
    EXPORTING
      json        = request_parameter
      pretty_name = /ui2/cl_json=>pretty_mode-none "格式化参数，NONE：字段名称大写
    CHANGING
      data        = lt_input ). "数据源CREATE OBJECT json_des.


  IF lt_input[] IS NOT INITIAL.
    LOOP AT lt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
      CLEAR:<fs_input>-yy1_flag,<fs_input>-yy1_msg.
      "推送WMS
      zclui5_odata_util=>senwms( CHANGING ls_input = <fs_input>
                                           flag = <fs_input>-yy1_flag
                                           msg  = <fs_input>-yy1_msg ).
      IF <fs_input>-yy1_flag = 'E'.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ELSE.
    return_code = 'E'.
    return_message = '传入值为空'.
    RETURN.
  ENDIF.
  return_code = 'S'.
  return_message = '处理完成'.
  DATA(lt_out) = lt_input[].
  LOOP AT lt_out ASSIGNING FIELD-SYMBOL(<fs_out>).
    <fs_out>-manufacturingorder = |{ <fs_out>-manufacturingorder ALPHA = OUT }|.
    <fs_out>-material = |{ <fs_out>-material ALPHA = OUT }|.
  ENDLOOP.
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
