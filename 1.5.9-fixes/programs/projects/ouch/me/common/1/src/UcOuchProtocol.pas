unit UcOuchProtocol;

interface

const
  PROT_BUILD = 1;
  HEADER_VER = 1;

const
  PS_BASE                 = $400;
  PS_DEFINEUSER           = PS_BASE                 + 1;
  PS_DEFINEUSER_REPLY     = PS_DEFINEUSER           + 1;
  PS_CONNECT              = PS_DEFINEUSER_REPLY     + 1;
  PS_CONNECT_REPLY        = PS_CONNECT              + 1;
  PS_QUERYOFFLINES        = PS_CONNECT_REPLY        + 1;
  PS_QUERYOFFLINES_REPLY  = PS_QUERYOFFLINES        + 1;
  PS_NOTIFYSTATUS         = PS_QUERYOFFLINES_REPLY  + 1;
  PS_NOTIFYSTATUS_REPLY   = PS_NOTIFYSTATUS         + 1;
  PS_QUERYGROUPS          = PS_NOTIFYSTATUS_REPLY   + 1;
  PS_QUERYGROUPS_REPLY    = PS_QUERYGROUPS          + 1;
  PS_GROUPLOGON           = PS_QUERYGROUPS_REPLY    + 1;
  PS_GROUPLOGON_REPLY     = PS_GROUPLOGON           + 1;
  PS_SENDMESSAGE          = PS_GROUPLOGON_REPLY     + 1;
  PS_SENDMESSAGE_REPLY    = PS_SENDMESSAGE          + 1;

implementation

end.
 