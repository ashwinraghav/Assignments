unit FmService;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, StdCtrls,

  Sil,
  UiAgent,
  UmAgent,
  DmAgentSource;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FAgent: IAgent;
    FSource: TdaAgentSource;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  UtLog;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Log.Initialize;

  FSource := TdaAgentSource.Create(Self);
  FAgent := TAgent.Create(FSource);

  FAgent.Start;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FAgent) then
  begin
    FAgent.Stop;
    FAgent := nil;
  end;

  Log.Finalize;
end;

end.
