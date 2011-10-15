unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, SvcMgr,

  Sil,
  UiBerf;

type
  TsvBerf = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    FBerf: IBerfList;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  svBerf: TsvBerf;

implementation

{$R *.DFM}

uses
  UmBerf;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  svBerf.Controller(CtrlCode);
end;

function TsvBerf.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TsvBerf.ServiceStart(Sender: TService; var Started: Boolean);
begin
  try
    FBerf := TBerfList.Create;
    FBerf.Configure(Sil.OS.Process.Current.Info.Path + 'berf.xml');
    FBerf.Start;
  except
    on ex: Exception do
      LogMessage('ServiceStart.error: ' + ex.Message, EVENTLOG_ERROR_TYPE);
  end;
end;

procedure TsvBerf.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  try
    FBerf.Stop;
    FBerf := nil;
  except
    on ex: Exception do
      LogMessage('ServiceStop.error: ' + ex.Message, EVENTLOG_ERROR_TYPE);
  end;
end;

end.
