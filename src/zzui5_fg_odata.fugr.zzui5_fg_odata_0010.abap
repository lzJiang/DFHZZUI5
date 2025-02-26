FUNCTION zzui5_fg_odata_0010.
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
  TYPES:pk_no                  TYPE zztfi_0003-pk_no,
        accountingdocument     TYPE string,
        companycode            TYPE zztfi_0003-companycode,
        accountingdocumenttype TYPE zztfi_0003-accountingdocumenttype,
        documentdate           TYPE zztfi_0003-documentdate,
        postingdate            TYPE zztfi_0003-postingdate,
        documentheadertext     TYPE zztfi_0003-documentheadertext,
        transactioncurrency    TYPE zztfi_0003-transactioncurrency,
        jxje                   TYPE zztfi_0003-amountingroupcurrency,
        dxje                   TYPE zztfi_0003-amountingroupcurrency,
        customer               TYPE zztfi_0003-customer,
        flag                   TYPE zztfi_0003-flag,
        msg                    TYPE zztfi_0003-msg,
        item                   TYPE zzt_zztfi_0003.
  TYPES END OF ty_input.

  DATA:ls_req       TYPE ty_input,
       lt_input     TYPE TABLE OF zztfi_0003,
       lt_input_one TYPE TABLE OF zztfi_0003.
  DATA:lv_where TYPE string,
       lv_flag  TYPE bapi_mtype,
       lv_msg   TYPE bapi_msg.
  DATA: lv_count_line_no          TYPE i,
        lv_s_str                  TYPE string,
        lv_msg_str                TYPE string,
        lv_accountingdocument_str TYPE string,
        p_num                     TYPE i VALUE 998,
        lv_count                  TYPE i,
        lv_count_numc             TYPE n LENGTH 5,
        lv_total                  TYPE i, "发送条数
        lv_div                    TYPE i, "发送条数/分包数的整数部分
        lv_mod                    TYPE i, "发送条数/分包数的余数部分
        lv_do_time                TYPE i, "发送循环次数
        lv_start_idnex            TYPE i, "每次发送起始位置
        lv_end_idnex              TYPE i. "每次发送终止位置

  "3. JSON报文转换为结构
  CALL METHOD /ui2/cl_json=>deserialize(
    EXPORTING
      json        = request_parameter
      pretty_name = /ui2/cl_json=>pretty_mode-none "格式化参数，NONE：字段名称大写
    CHANGING
      data        = ls_req ). "数据源CREATE OBJECT json_des.

  lt_input = ls_req-item[].
  IF lt_input[] IS NOT INITIAL.
*    LOOP AT lt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
*      CLEAR:<fs_input>-flag,<fs_input>-msg.
    READ TABLE lt_input INTO DATA(ls_input_1) INDEX 1.
    "检查导入凭证号+行号唯一
    DATA(lt_pk_line_no_1) = lt_input.
    SORT lt_pk_line_no_1 BY pk_no pk_line_no_1.
    DELETE ADJACENT DUPLICATES FROM lt_pk_line_no_1 COMPARING pk_no pk_line_no_1.
    LOOP AT lt_pk_line_no_1 INTO DATA(ls_pk_line_no_1).
      CLEAR:lv_count_line_no.
      LOOP AT lt_input ASSIGNING FIELD-SYMBOL(<fs_input_1>).
        IF <fs_input_1>-pk_no = ls_pk_line_no_1-pk_no AND <fs_input_1>-pk_line_no_1 = ls_pk_line_no_1-pk_line_no_1.
          lv_count_line_no = lv_count_line_no + 1.
        ENDIF.
      ENDLOOP.
      IF lv_count_line_no > 1.
        lv_flag = 'E'.
        lv_msg = |导入凭证号{ ls_pk_line_no_1-pk_no }行号{ ls_pk_line_no_1-pk_line_no_1 }存在重复条目，请修正后重新导入|.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF lv_flag NE 'E' AND ls_input_1-pk_no IS NOT INITIAL.
      "1.检查该导入凭证号是否存在
      SELECT *
          FROM i_journalentry
         WHERE documentreferenceid = @ls_input_1-pk_no
          INTO TABLE @DATA(lt_journalentry).
      IF sy-subrc = 0.
        lv_flag = 'E'.
        lv_msg = |导入凭证{ ls_input_1-pk_no }已存在对应的SAP会计凭证|.
        LOOP AT lt_journalentry INTO DATA(ls_journalentry).
          lv_msg = |{ lv_msg }/{ ls_journalentry-accountingdocument }|.
        ENDLOOP.
      ENDIF.
    ENDIF.
    IF lv_flag NE 'E'.
      LOOP AT lt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
        <fs_input>-pk_no = ls_req-pk_no.
        <fs_input>-companycode = ls_req-companycode.
        <fs_input>-accountingdocumenttype = ls_req-accountingdocumenttype.
        <fs_input>-documentdate = ls_req-documentdate.
        <fs_input>-postingdate = ls_req-postingdate.
        <fs_input>-documentheadertext = ls_req-documentheadertext.
        <fs_input>-transactioncurrency = ls_req-transactioncurrency.
        CLEAR:<fs_input>-flag,<fs_input>-msg.
      ENDLOOP.
      DATA(lv_lines) = lines( lt_input ).
      IF lv_lines <= 999.
        zclui5_odata_util_0002=>post( CHANGING lt_input = lt_input
                                               flag = lv_flag
                                               msg  = lv_msg ).
      ELSE.
        lv_div = lv_lines DIV p_num.
        lv_mod = lv_lines MOD p_num.
        IF lv_mod = 0.
          lv_do_time = lv_div.
        ELSE.
          lv_do_time = lv_div + 1.
        ENDIF.
        DO lv_do_time TIMES.
          CLEAR:lt_input_one[].
          lv_count = lv_count + 1.
          lv_count_numc = lv_count.
          lv_start_idnex = ( lv_count - 1 ) * p_num + 1.
          IF lv_count = lv_do_time.
            lv_end_idnex = lv_lines.
          ELSE.
            lv_end_idnex =  lv_count * p_num.
          ENDIF.
          lv_s_str = |拆分凭证{ lv_count },行{ lv_start_idnex }-行{ lv_end_idnex }:|.
          LOOP AT lt_input INTO DATA(ls_input) FROM lv_start_idnex TO lv_end_idnex.
            APPEND ls_input TO lt_input_one.
          ENDLOOP.
          zclui5_odata_util_0002=>post_cf( EXPORTING s_str = lv_s_str
                                           CHANGING lt_input = lt_input_one
                                               flag = lv_flag
                                               msg  = lv_msg ).
          LOOP AT lt_input_one INTO DATA(ls_input_one).
            READ TABLE lt_input ASSIGNING <fs_input> WITH KEY pk_no = ls_input_one-pk_no
                                                              pk_line_no_1 = ls_input_one-pk_line_no_1.
            IF sy-subrc = 0.
              MOVE-CORRESPONDING ls_input_one TO <fs_input>.
            ENDIF.
          ENDLOOP.
          IF lv_flag = 'E'.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.
    ELSE.
      LOOP AT lt_input ASSIGNING <fs_input>.
        <fs_input>-flag = lv_flag.
        <fs_input>-msg = lv_msg.
      ENDLOOP.
    ENDIF.
*    ENDLOOP.
  ELSE.
    return_code = 'E'.
    return_message = '传入值为空'.
    RETURN.
  ENDIF.
  return_code = 'S'.
  return_message = '处理完成'.
  DATA(ls_out) = ls_req.
  ls_out-item = lt_input[].
  IF lv_lines <= 999.
    READ TABLE lt_input INTO ls_input_1 INDEX 1.
    IF sy-subrc = 0.
      ls_out-accountingdocument = ls_input_1-accountingdocument.
      ls_out-flag = ls_input_1-flag.
      ls_out-msg = ls_input_1-msg.
    ENDIF.
  ELSE.
    READ TABLE lt_input TRANSPORTING NO FIELDS WITH KEY flag = 'E'.
    IF sy-subrc = 0.
      ls_out-flag = 'E'.
      DATA(lt_err) = lt_input.
      DELETE lt_err WHERE flag NE 'E'.
      SORT lt_err BY msg.
      DELETE ADJACENT DUPLICATES FROM lt_err COMPARING msg.
      LOOP AT lt_err INTO DATA(ls_err).
        ls_out-msg = |{ ls_out-msg }/{ ls_err-msg }|.
      ENDLOOP.
      DATA(lt_success) = lt_input.
      DELETE lt_success WHERE flag NE 'S'.
      SORT lt_success BY accountingdocument.
      DELETE ADJACENT DUPLICATES FROM lt_success COMPARING accountingdocument.
      LOOP AT lt_success INTO DATA(ls_success).
        IF ls_out-accountingdocument IS INITIAL.
          ls_out-accountingdocument = |{ ls_success-accountingdocument }|.
        ELSE.
          ls_out-accountingdocument = |{ ls_out-accountingdocument }/{ ls_success-accountingdocument }|.
        ENDIF.
      ENDLOOP.
    ELSE.
      ls_out-flag = 'S'.
      lt_success = lt_input.
      DELETE lt_success WHERE flag NE 'S'.
      SORT lt_success BY accountingdocument.
      DELETE ADJACENT DUPLICATES FROM lt_success COMPARING accountingdocument.
      LOOP AT lt_success INTO ls_success.
        IF ls_out-accountingdocument IS INITIAL.
          ls_out-accountingdocument = |{ ls_success-accountingdocument }|.
        ELSE.
          ls_out-accountingdocument = |{ ls_out-accountingdocument }/{ ls_success-accountingdocument }|.
        ENDIF.
      ENDLOOP.
    ENDIF.
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
