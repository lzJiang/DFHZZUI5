@Metadata.allowExtensions: true
@EndUserText.label: '###GENERATED Core Data Service Entity'
@AccessControl.authorizationCheck: #CHECK
define root view entity ZC_ZT_ODATA_CONFIG
  provider contract TRANSACTIONAL_QUERY
  as projection on ZR_ZT_ODATA_CONFIG
{
  key Code,
  Function,
  Description,
  CreatedBy,
  CreatedAt,
  LastChangedBy,
  LastChangedAt
  
}
