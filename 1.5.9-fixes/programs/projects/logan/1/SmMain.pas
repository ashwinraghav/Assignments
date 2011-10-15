unit SmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs,

  UiLogan,
  UmLogan;

type
  TLoganService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    FLogan: ILogan;
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  LoganService: TLoganService;

implementation

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  LoganService.Controller(CtrlCode);
end;

function TLoganService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TLoganService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  FLogan := TLogan.Create;
  FLogan.Start;
  
  Started := true;
end;

procedure TLoganService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  if Assigned(FLogan) then
  begin
    FLogan.Stop;
    FLogan := nil;
  end;

  Stopped := true;
end;

end.
