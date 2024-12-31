@Metadata.allowExtensions: true
@EndUserText.label: '###GENERATED Core Data Service Entity'
@AccessControl.authorizationCheck: #CHECK
define root view entity ZC_ZT_UI5_ODATA
  provider contract transactional_query
  as projection on ZR_ZT_UI5_ODATA
{
  key Uuid,
  key Requestcode,
  Requestparameter,
  Returncode,
  Returnmessage,
  Returnresult
  
}
