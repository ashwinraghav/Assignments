unit UiStatus;

interface

type
  IStatusDaemon = interface
    ['{F34B32EC-D749-4F5A-B181-3E400E8E0822}']
    procedure Startup;
    procedure Shutdown;
  end;

implementation

end.
 