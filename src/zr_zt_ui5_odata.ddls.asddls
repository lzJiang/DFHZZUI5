@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_ZT_UI5_ODATA
  as select from zzt_ui5_odata
{
  key uuid as Uuid,
  key requestcode as Requestcode,
   requestparameter as Requestparameter,
   returncode as Returncode,
   returnmessage as Returnmessage,
   returnresult as Returnresult
  
}
