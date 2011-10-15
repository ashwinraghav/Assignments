unit UcClient;

interface

resourcestring
  CUserAdded                  = 'user created, session opened';
  CErrorAddingUser            = 'error adding user, session failed';
  CSessionOpened              = 'session opened';
  CSessionResumed             = 'session updated and resumed';
  CErrorOpeningSession        = 'error opening session';
  CInvalidUser                = 'invalid user';
  COfflineMessageDelivered    = 'offline message delivered';
  CUserInfoUpdated            = 'user info updated';
  CErrorUpdatingUserInfo      = 'error updating user info';
  CInvalidUserOrPassword      = 'invalid user or password';
  CInvalidSession             = 'invalid session';
  CLoggedIntoGroup            = 'logged into group';
  CInvalidGroupOrPassword     = 'invalid group or password';
  CUserStatusUpdated          = 'user status updated';
  CGroupListRetrieved         = 'group list retrieved';
  CErrorAccessingGroupList    = 'error accessing group list';
  COfflineMessageStored       = 'offline message stored';
  CErrorStoringOfflineMessage = 'error storing offline message';
  CQueryOfflineFinished       = 'offlines sent';

implementation

end.
 