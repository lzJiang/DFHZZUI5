FUNCTION zzui5_fg_odata_0009.
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
          pk_no            TYPE zztfi_0003-pk_no,
          companycode      TYPE zztfi_0003-companycode,
          customer         TYPE zztfi_0003-customer,
          postingdatestart TYPE zztfi_0003-postingdate,
          postingdateend   TYPE zztfi_0003-postingdate,
        END OF ty_input.
  TYPES BEGIN OF ty_output.
  TYPES:pk_no                  TYPE zztfi_0003-pk_no,
        uuid16                 TYPE zztfi_0003-uuid16,
        accountingdocument     TYPE zztfi_0003-accountingdocument,
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
  TYPES END OF ty_output.

  DATA:ls_input TYPE ty_input,
       lt_out   TYPE TABLE OF ty_output.
  DATA:lv_where TYPE string.

  lv_where = | accountingdocument ne '' and flag = 'S'|.

  "3. JSON报文转换为结构
  CALL METHOD /ui2/cl_json=>deserialize(
    EXPORTING
      json        = request_parameter
      pretty_name = /ui2/cl_json=>pretty_mode-low_case "格式化参数，NONE：字段名称大写
    CHANGING
      data        = ls_input ). "数据源CREATE OBJECT json_des.

  IF ls_input-pk_no IS NOT INITIAL.
    lv_where = | { lv_where } and pk_no like '%{ ls_input-pk_no }%'|.
  ENDIF.
  IF ls_input-companycode IS NOT INITIAL.
    lv_where = | { lv_where } and companycode like '%{ ls_input-companycode }%'|.
  ENDIF.
  IF ls_input-customer IS NOT INITIAL.
    lv_where = | { lv_where } and customer like '%{ ls_input-customer }%'|.
  ENDIF.
  IF ls_input-postingdatestart IS NOT INITIAL.
    lv_where = | { lv_where } and postingdate >= '{ ls_input-postingdatestart }'|.
  ENDIF.
  IF ls_input-postingdateend IS NOT INITIAL.
    lv_where = | { lv_where } and postingdate <= '{ ls_input-postingdateend }'|.
  ENDIF.

  SELECT DISTINCT pk_no
    FROM zztfi_0003
    WHERE (lv_where)
    INTO TABLE @DATA(lt_pk_no).
  IF lt_pk_no[] IS NOT INITIAL.
    SELECT *
    FROM zztfi_0003
    FOR ALL ENTRIES IN @lt_pk_no
    WHERE pk_no = @lt_pk_no-pk_no
      AND flag = 'S'
    INTO TABLE @DATA(lt_item).
  ENDIF.

  IF lt_item[] IS NOT INITIAL.
    DATA(lt_head) = lt_item[].
    SORT lt_head BY accountingdocument postingdate.
    DELETE ADJACENT DUPLICATES FROM lt_head COMPARING accountingdocument postingdate.

    LOOP AT lt_head INTO DATA(ls_head).
      APPEND INITIAL LINE TO lt_out ASSIGNING FIELD-SYMBOL(<fs_out>).
      MOVE-CORRESPONDING ls_head TO <fs_out>.
      LOOP AT lt_item INTO DATA(ls_item) WHERE accountingdocument = ls_head-accountingdocument
                                           AND postingdate = ls_head-postingdate.
        APPEND INITIAL LINE TO <fs_out>-item ASSIGNING FIELD-SYMBOL(<fs_item>).
        MOVE-CORRESPONDING ls_item TO <fs_item>.
        IF ls_item-debitcreditcode = 'S'.
          <fs_out>-jxje = <fs_out>-jxje + ls_item-amountintransactioncurrency.
        ELSEIF ls_item-debitcreditcode = 'H'.
          <fs_out>-dxje = <fs_out>-dxje + ls_item-amountintransactioncurrency.
        ENDIF.
        IF ls_item-customer IS NOT INITIAL.
          <fs_out>-customer = ls_item-customer.
        ENDIF.
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
