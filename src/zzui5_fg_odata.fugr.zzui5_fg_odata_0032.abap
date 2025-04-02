FUNCTION zzui5_fg_odata_0032.
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
          extend1                 TYPE zztfi_0007-extend1,
          transactionserialnumber TYPE zztfi_0007-transactionserialnumber,
          companycode             TYPE zztfi_0007-companycode,
          merchantcode            TYPE zztfi_0007-merchantcode,
          claimsuccesstimestart   TYPE zztfi_0007-claimsuccesstime,
          claimsuccesstimeend     TYPE zztfi_0007-claimsuccesstime,
        END OF ty_input.


  DATA:ls_input TYPE ty_input.
  DATA:lv_where TYPE string.

  lv_where = | transactionserialnumber ne '' |.

  "3. JSON报文转换为结构
  CALL METHOD /ui2/cl_json=>deserialize(
    EXPORTING
      json        = request_parameter
      pretty_name = /ui2/cl_json=>pretty_mode-low_case "格式化参数，NONE：字段名称大写
    CHANGING
      data        = ls_input ). "数据源CREATE OBJECT json_des.

  IF ls_input-extend1 IS NOT INITIAL.
    lv_where = | { lv_where } and extend1 like '%{ ls_input-extend1 }%'|.
  ENDIF.
  IF ls_input-transactionserialnumber IS NOT INITIAL.
    lv_where = | { lv_where } and transactionserialnumber like '%{ ls_input-transactionserialnumber }%'|.
  ENDIF.
  IF ls_input-companycode IS NOT INITIAL.
    lv_where = | { lv_where } and companycode like '%{ ls_input-companycode }%'|.
  ENDIF.
  IF ls_input-merchantcode IS NOT INITIAL.
    lv_where = | { lv_where } and merchantcode like '%{ ls_input-merchantcode }%'|.
  ENDIF.
  IF ls_input-claimsuccesstimestart IS NOT INITIAL.
    lv_where = | { lv_where } and claimsuccesstime >= '{ ls_input-claimsuccesstimestart }'|.
  ENDIF.
  IF ls_input-claimsuccesstimeend IS NOT INITIAL.
    lv_where = | { lv_where } and claimsuccesstime <= '{ ls_input-claimsuccesstimeend }'|.
  ENDIF.

  SELECT *
    FROM zztfi_0007
    WHERE (lv_where)
    INTO TABLE @DATA(lt_out).

  IF lt_out[] IS NOT INITIAL.
    DATA(lv_lines) = lines( lt_out ).
    SELECT *
      FROM i_journalentry WITH PRIVILEGED ACCESS
      FOR ALL ENTRIES IN @lt_out
     WHERE fiscalyear = @lt_out-fiscalyear
       AND accountingdocument = @lt_out-accountingdocument
       AND companycode = @lt_out-companycode
       AND isreversed = 'X'
      INTO TABLE @DATA(lt_journalentry).
    SORT lt_journalentry BY fiscalyear accountingdocument companycode.
    LOOP AT lt_out ASSIGNING FIELD-SYMBOL(<fs_out>).
      READ TABLE lt_journalentry INTO DATA(ls_journalentry) WITH KEY fiscalyear = <fs_out>-fiscalyear
                                                                     accountingdocument = <fs_out>-accountingdocument
                                                                     companycode = <fs_out>-companycode BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_out>-reversedfiscalyear = ls_journalentry-reversedocumentfiscalyear.
        <fs_out>-reversedaccountingdocument = ls_journalentry-reversalreferencedocument.
      ENDIF.
    ENDLOOP.
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
