CLASS zclui5_odata_util_0009 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS post
      CHANGING ls_input TYPE zztfi_0007.
    CLASS-METHODS check
      CHANGING ls_input TYPE zztfi_0007.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCLUI5_ODATA_UTIL_0009 IMPLEMENTATION.


  METHOD check.
    clear:ls_input-flag,ls_input-msg.
    IF ls_input-transactionserialnumber IS INITIAL.
      ls_input-flag = 'E'.
      ls_input-msg = '【交易流水号】不能为空'.
      RETURN.
    ELSE.
      SELECT SINGLE a~*
               FROM zztfi_0007 WITH PRIVILEGED ACCESS AS a
               INNER JOIN i_journalentry WITH PRIVILEGED ACCESS AS b
                 ON a~accountingdocument = b~accountingdocument AND a~fiscalyear = b~fiscalyear AND a~companycode = b~companycode
              WHERE transactionserialnumber = @ls_input-transactionserialnumber
                AND flag = 'S'
                AND b~isreversed = ''
               INTO @DATA(ls_zztfi_0007).
      IF sy-subrc = 0.
        ls_input-flag = 'E'.
        ls_input-msg = |流水号{ ls_input-transactionserialnumber }已生成对应SAP会计凭证{ ls_zztfi_0007-accountingdocument }|
        && |-{ ls_zztfi_0007-fiscalyear },请勿重复创建！|.
        RETURN.
      ENDIF.
      SELECT SINGLE a~*
               FROM zztfi_0007 WITH PRIVILEGED ACCESS AS a
              WHERE transactionserialnumber = @ls_input-transactionserialnumber
                AND flag = 'R'
               INTO @ls_zztfi_0007.
      IF sy-subrc = 0.
        ls_input-flag = 'E'.
        ls_input-msg = |流水号{ ls_input-transactionserialnumber }正在后台处理中，请勿重复处理|.
        RETURN.
      ENDIF.
    ENDIF.
    IF ls_input-extend1 IS INITIAL.
      ls_input-flag = 'E'.
      ls_input-msg = '【款项性质】不能为空'.
      RETURN.
    ENDIF.
    IF ls_input-currency IS INITIAL.
      ls_input-flag = 'E'.
      ls_input-msg = '【币种】不能为空'.
      RETURN.
    ENDIF.
    IF ls_input-accountno IS INITIAL.
      ls_input-flag = 'E'.
      ls_input-msg = '【银行账户】不能为空'.
      RETURN.
    ENDIF.
    IF ls_input-claimamount IS INITIAL.
      ls_input-flag = 'E'.
      ls_input-msg = '【认领金额】不能为空'.
      RETURN.
    ENDIF.
    IF ls_input-claimsuccesstime IS INITIAL.
      ls_input-flag = 'E'.
      ls_input-msg = '【认领日期】不能为空'.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD post.
    zzcl_job_fi002=>postcbs(  EXPORTING i_wbbs = 'X'
                              CHANGING i_zztfi_0007 = ls_input ).
  ENDMETHOD.
ENDCLASS.
