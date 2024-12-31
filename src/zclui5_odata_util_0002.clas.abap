CLASS zclui5_odata_util_0002 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS SAVE_ZZTFI_0003
      IMPORTING ls_input TYPE zzt_ui5_odata.
    CLASS-METHODS post
      CHANGING lt_input TYPE ZZT_ZZTFI_0003
               flag     TYPE bapi_mtype
               msg      TYPE bapi_msg.
PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zclui5_odata_util_0002 IMPLEMENTATION.

  METHOD save_zztfi_0003.
    TYPES BEGIN OF ty_input.
    TYPES:pk_no                  TYPE zztfi_0003-pk_no,
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
    TYPES END OF ty_input.
    DATA:ls_result     TYPE ty_input,
         lt_zztfi_0003 TYPE TABLE OF zztfi_0003.
    IF ls_input-requestcode = 'FI0002'.
      "社交电商批导处理日志保存
      CALL METHOD /ui2/cl_json=>deserialize(
        EXPORTING
          json        = ls_input-returnresult
          pretty_name = /ui2/cl_json=>pretty_mode-none "格式化参数，NONE：字段名称大写
        CHANGING
          data        = ls_result ). "数据源CREATE OBJECT json_des.
      lt_zztfi_0003 = ls_result-item.
      DATA(lv_date) = cl_abap_context_info=>get_system_date( ).
      DATA(lv_time) = cl_abap_context_info=>get_system_time( ).
      DATA(lv_user) = cl_abap_context_info=>get_user_technical_name( ).

      LOOP AT lt_zztfi_0003 ASSIGNING FIELD-SYMBOL(<fs_zztfi_0003>).
        TRY.
            <fs_zztfi_0003>-uuid16 = cl_system_uuid=>create_uuid_x16_static( ).
          CATCH cx_uuid_error.

        ENDTRY.
        <fs_zztfi_0003>-created_date = lv_date.
        <fs_zztfi_0003>-created_time = lv_time.
        <fs_zztfi_0003>-created_by   = lv_user.
      endloop.
        MODIFY zztfi_0003 FROM TABLE @lt_zztfi_0003.
      ENDIF.
    ENDMETHOD.

  METHOD post.
    DATA:ls_zztfi_0003 TYPE zztfi_0003.
    DATA: lt_je_deep TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
          ls_je_deep LIKE LINE OF lt_je_deep,
          lv_cid     TYPE abp_behv_cid.
    READ TABLE lt_input INTO DATA(ls_input_1) INDEX 1.
    "1.检查该导入凭证号是否存在
    SELECT SINGLE *
             FROM i_journalentry
            WHERE documentreferenceid = @ls_input_1-pk_no
             INTO @DATA(ls_journalentry).
    IF sy-subrc = 0.
      flag = 'E'.
      msg = |导入凭证{ ls_input_1-pk_no }已存在对应的SAP会计凭证{ ls_journalentry-accountingdocument }|.
      LOOP AT lt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
        <fs_input>-flag = 'E'.
        <fs_input>-msg = msg.
        <fs_input>-accountingdocument = ls_journalentry-accountingdocument.
      ENDLOOP.
      RETURN.
    ENDIF.

    TRY.
        DATA(destination) = cl_soap_destination_provider=>create_by_comm_arrangement(
          comm_scenario  = 'ZZHTTP_INBOUND_API'
          service_id     = 'ZZ_OS_FI001_SPRX'
          comm_system_id = 'THIRD_TO_SAP'
        ).

        DATA(proxy) = NEW zco_journal_entry_create_reque( destination = destination ).

        " fill request
        DATA(request) = VALUE zjournal_entry_bulk_create_req( ).
        DATA(lv_date) = cl_abap_context_info=>get_system_date( ).
        DATA(lv_time) = cl_abap_context_info=>get_system_time( ).
        TRY.
            data(lv_zone) = cl_abap_context_info=>get_user_time_zone( ).
          CATCH cx_abap_context_info_error.
            "handle exception
        ENDTRY.
        CONVERT DATE lv_date TIME lv_time INTO TIME STAMP request-journal_entry_bulk_create_requ-message_header-creation_date_time TIME ZONE lv_zone.

*        request-journal_entry_bulk_create_requ-message_header-creation_date_time = |{ lv_date+0(4) }-{ lv_date+4(2) }-{ lv_date+6(2) }|
*        && |T{ lv_time+0(2) }-{ lv_time+2(2) }-{ lv_time+4(2) }Z|.
        APPEND INITIAL LINE TO request-journal_entry_bulk_create_requ-journal_entry_create_request ASSIGNING FIELD-SYMBOL(<fs_request>).
        <fs_request>-message_header-creation_date_time = request-journal_entry_bulk_create_requ-message_header-creation_date_time.
        ASSIGN <fs_request>-journal_entry TO FIELD-SYMBOL(<fs_entry>).
        "抬头
        <fs_entry>-original_reference_document_ty = 'BKPFF'.
*        <fs_entry>-original_reference_document = ls_input_1-pk_no.
        <fs_entry>-original_reference_document_lo = 'SAP'.
        <fs_entry>-business_transaction_type = 'RFBU'.
        <fs_entry>-accounting_document_type = ls_input_1-accountingdocumenttype.
        <fs_entry>-document_reference_id = ls_input_1-pk_no.
        <fs_entry>-document_header_text = ls_input_1-documentheadertext.
        <fs_entry>-created_by_user = cl_abap_context_info=>get_user_technical_name( ).
        <fs_entry>-company_code = ls_input_1-companycode.
        <fs_entry>-document_date = ls_input_1-documentdate.
        <fs_entry>-posting_date = ls_input_1-postingdate.
        "行项目
        LOOP AT lt_input INTO DATA(ls_input).
          CASE ls_input-koart.
            WHEN 'S'.
              APPEND INITIAL LINE TO <fs_entry>-item ASSIGNING FIELD-SYMBOL(<fs_item>).
              <fs_item>-reference_document_item = ls_input-pk_line_no.
              <fs_item>-glaccount-content = ls_input-glaccount.
              <fs_item>-debit_credit_code = ls_input-debitcreditcode.
              <fs_item>-account_assignment-profit_center = ls_input-profitcenter.
              <fs_item>-amount_in_transaction_currency-currency_code = ls_input-transactioncurrency.
              <fs_item>-amount_in_transaction_currency-content = ls_input-amountintransactioncurrency.
              <fs_item>-amount_in_group_currency-content = ls_input-amountingroupcurrency.
              <fs_item>-amount_in_group_currency-currency_code = ls_input-transactioncurrency.
              IF <fs_item>-debit_credit_code = 'H'.
                <fs_item>-amount_in_transaction_currency-content = - abs( ls_input-amountintransactioncurrency ).
                <fs_item>-amount_in_group_currency-content = - abs( ls_input-amountingroupcurrency ).
              ENDIF.
              <fs_item>-reason_code = ls_input-reasoncode.
              <fs_item>-house_bank = ls_input-housebank.
              <fs_item>-house_bank_account = ls_input-housebankaccount.
              <fs_item>-document_item_text = ls_input-documentitemtext.
            WHEN 'A'.
              APPEND INITIAL LINE TO <fs_entry>-debtor_item ASSIGNING FIELD-SYMBOL(<fs_ditem>).
              <fs_ditem>-reference_document_item = ls_input-pk_line_no.
              <fs_ditem>-debtor = ls_input-customer.
              <fs_ditem>-debit_credit_code = ls_input-debitcreditcode.
              <fs_ditem>-altv_recncln_accts-content = ls_input-altvrecnclnaccts.
              <fs_ditem>-amount_in_transaction_currency-currency_code = ls_input-transactioncurrency.
              <fs_ditem>-amount_in_transaction_currency-content = ls_input-amountintransactioncurrency.
              <fs_ditem>-amount_in_group_currency-currency_code = ls_input-transactioncurrency.
              <fs_ditem>-amount_in_group_currency-content = ls_input-amountingroupcurrency.
              IF <fs_ditem>-debit_credit_code = 'H'.
                <fs_ditem>-amount_in_transaction_currency-content = - abs( ls_input-amountintransactioncurrency ).
                <fs_ditem>-amount_in_group_currency-content = - abs( ls_input-amountingroupcurrency ).
              ENDIF.
              <fs_ditem>-assignment_reference = ls_input-assignmentreference.
              <fs_ditem>-document_item_text = ls_input-documentitemtext.
          ENDCASE.
        ENDLOOP.

        proxy->journal_entry_create_request_c(
          EXPORTING
            input = request
          IMPORTING
            output = DATA(response)
        ).
        DATA(lt_confirmat) = response-journal_entry_bulk_create_conf-journal_entry_create_confirmat.
        READ TABLE lt_confirmat INTO DATA(ls_confirmat) INDEX 1.
        IF ls_confirmat-journal_entry_create_confirmat-accounting_document IS NOT INITIAL
        AND ls_confirmat-journal_entry_create_confirmat-accounting_document NE '0000000000'.
          flag = 'S'.
          LOOP AT lt_input ASSIGNING <fs_input>.
            <fs_input>-flag = 'S'.
            <fs_input>-accountingdocument = ls_confirmat-journal_entry_create_confirmat-accounting_document.
            <fs_input>-fiscalperiod = ls_confirmat-journal_entry_create_confirmat-fiscal_year.
          ENDLOOP.
        ELSE.
          flag = 'E'.
          LOOP AT ls_confirmat-log-item[] INTO DATA(ls_item_log).
            msg = |{ msg }/{ ls_item_log-note }|.
          ENDLOOP.
          LOOP AT lt_input ASSIGNING <fs_input>.
            <fs_input>-flag = 'E'.
            <fs_input>-msg = msg.
          ENDLOOP.
        ENDIF.
        " handle response
      CATCH cx_soap_destination_error INTO DATA(destination_error).
        " handle error
        flag = 'E'.
        msg = '调用接口异常1:' && destination_error->get_longtext( ).
      CATCH cx_ai_system_fault INTO DATA(system_fault).
        " handle error
        flag = 'E'.
        msg = '调用接口异常2:' && system_fault->get_longtext( ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
