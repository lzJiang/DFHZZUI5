FUNCTION zzui5_fg_odata_0001.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(REQUEST_CODE) TYPE  CHAR30
*"     VALUE(REQUEST_PARAMETER) TYPE  STRING
*"  EXPORTING
*"     REFERENCE(RETURN_CODE) TYPE  STRING
*"     REFERENCE(RETURN_MESSAGE) TYPE  STRING
*"     REFERENCE(RETURN_RESULT) TYPE  STRING
*"----------------------------------------------------------------------
*&---功能函数
  DATA:lv_function TYPE char30."函数名定义
*&---捕获异常变量定义
  DATA: lx_root    TYPE REF TO cx_root.
*----------------------------------------------------------------------*
* 后台函数获取
*----------------------------------------------------------------------*
*&---根据code码获取函数名
  SELECT SINGLE function
    FROM zzt_odata_config
   WHERE code = @request_code
     INTO @lv_function.
  CONDENSE request_parameter NO-GAPS.
  TRANSLATE request_parameter TO UPPER CASE.
  IF lv_function IS NOT INITIAL.
    TRY.
        CALL FUNCTION lv_function
          EXPORTING
            request_parameter = request_parameter
          IMPORTING
            return_result     = return_result
            return_code       = return_code
            return_message    = return_message.
*&---捕获异常
      CATCH cx_root INTO lx_root.
        DATA(lv_exc_msg) = lx_root->if_message~get_text( ).

        IF lv_exc_msg IS NOT INITIAL.
          return_code = 'E'.
*&---拼接消息
          return_message = 'SAP调用配置的函数出现异常'.
        ENDIF.
    ENDTRY.
  ELSE.
    return_code = 'E'.
*&---拼接消息
    return_message = '未配置当前CODE码，请检查CODE码是否正确'.
  ENDIF.

ENDFUNCTION.
