unit Ouch;

interface

uses
  UeOuchData,
  UtOuchData,
  UiOuchProtocol;

type
  TOuchUserData                                 = UeOuchData.TOuchUserData;

const
  odUserFirstName                               = TOuchUserData(UeOuchData.odUserFirstName);
  odUserLastName                                = TOuchUserData(UeOuchData.odUserLastName);
  odUserNickName                                = TOuchUserData(UeOuchData.odUserNickName);
  odUserEmailAddress                            = TOuchUserData(UeOuchData.odUserEmailAddress);
  odUserNetPort                                 = TOuchUserData(UeOuchData.odUserNetPort);
  odUserNetAddress                              = TOuchUserData(UeOuchData.odUserNetAddress);

type
  Data                                          = UtOuchData.Data;

type
  ROuchGroup                                    = UiOuchProtocol.ROuchGroup;
  TOuchGroupArray                               = UiOuchProtocol.TOuchGroupArray;
  ROuchUser                                     = UiOuchProtocol.ROuchUser;
  TOuchUserArray                                = UiOuchProtocol.TOuchUserArray;

type
  TOuchUserStatus                               = UiOuchProtocol.TOuchUserStatus;

const
  usUnknown                                     = TOuchUserStatus(UiOuchProtocol.usUnknown);
  usAsk                                         = TOuchUserStatus(UiOuchProtocol.usAsk);      
  usValidate                                    = TOuchUserStatus(UiOuchProtocol.usValidate);
  usShutdown                                    = TOuchUserStatus(UiOuchProtocol.usShutdown);
  usOffline                                     = TOuchUserStatus(UiOuchProtocol.usOffline);  
  usOnline                                      = TOuchUserStatus(UiOuchProtocol.usOnline);   
  usBusy                                        = TOuchUserStatus(UiOuchProtocol.usBusy);     

type
  TOuchMessageKind                              = UiOuchProtocol.TOuchMessageKind;

const
  mkInstantMessage                              = TOuchMessageKind(UiOuchProtocol.mkInstantMessage);
  mkClipboard                                   = TOuchMessageKind(UiOuchProtocol.mkClipboard);     

type
  TOuchMessageStatus                            = UiOuchProtocol.TOuchMessageStatus;

const
  msReceived                                    = TOuchMessageStatus(UiOuchProtocol.msReceived);
  msPending                                     = TOuchMessageStatus(UiOuchProtocol.msPending);
  msOffline                                     = TOuchMessageStatus(UiOuchProtocol.msOffline);

type
  ROuchReply                                    = UiOuchProtocol.ROuchReply;
  ROuchDefineUserData                           = UiOuchProtocol.ROuchDefineUserData;    
  ROuchDefineUserReply                          = UiOuchProtocol.ROuchDefineUserReply;   
  ROuchConnectData                              = UiOuchProtocol.ROuchConnectData;       
  ROuchConnectReply                             = UiOuchProtocol.ROuchConnectReply;      
  ROuchQueryOfflinesData                        = UiOuchProtocol.ROuchQueryOfflinesData; 
  ROuchQueryOfflinesReply                       = UiOuchProtocol.ROuchQueryOfflinesReply;
  ROuchNotifyStatusData                         = UiOuchProtocol.ROuchNotifyStatusData;  
  ROuchNotifyStatusReply                        = UiOuchProtocol.ROuchNotifyStatusReply; 
  ROuchQueryGroupsData                          = UiOuchProtocol.ROuchQueryGroupsData;   
  ROuchQueryGroupsReply                         = UiOuchProtocol.ROuchQueryGroupsReply;  
  ROuchGroupLogonData                           = UiOuchProtocol.ROuchGroupLogonData;    
  ROuchGroupLogonReply                          = UiOuchProtocol.ROuchGroupLogonReply;   
  ROuchSendMessageData                          = UiOuchProtocol.ROuchSendMessageData;   
  ROuchSendMessageReply                         = UiOuchProtocol.ROuchSendMessageReply;

type
  IOuchProtocol                                 = UiOuchProtocol.IOuchProtocol;
  IOuchRequestPacker                            = UiOuchProtocol.IOuchRequestPacker;
  IOuchRequest                                  = UiOuchProtocol.IOuchRequest;
  IOuchReply                                    = UiOuchProtocol.IOuchReply;        

implementation
end.
