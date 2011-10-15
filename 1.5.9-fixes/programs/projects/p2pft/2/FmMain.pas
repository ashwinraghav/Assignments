unit FmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls, FrPanel,

  Sil,

  UiFileServer,
  UmFileServer;

type
  TfoMain = class(TForm)
    MainMenu: TMainMenu;
    Archivo1: TMenuItem;
    Salir1: TMenuItem;
    faPanel1: TfaPanel;
    spMain: TSplitter;
    faPanel2: TfaPanel;
    Timer1: TTimer;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FServer: IFileServer;
    function DoReadConfig: IParameterList;
  public
    { Public declarations }
  end;

var
  foMain: TfoMain;

implementation

{$R *.dfm}

procedure TfoMain.FormResize(Sender: TObject);
begin
  faPanel1.Height := (Self.ClientHeight - spMain.Height) div 2;
end;

function TfoMain.DoReadConfig: IParameterList;
var
  Source: ITextFile;
  Line, Tag, Value: String;
begin
  Result := Sil.List.Parameters;
  Source := Sil.OS.FSys.OpenTextFile(Sil.OS.Process.Current.Info.Path + 'p2pft.conf', fmAccessRead, fmShareWrite, true);

  with Source.Stream do
    while ReadLn(Line) do
    begin
      Str.Split(Line, '=', Tag, Value);
      Result[Tag] := Value;
    end;
end;

procedure TfoMain.FormCreate(Sender: TObject);
begin
  FServer := TFileServer.Create;
  FServer.Start(DoReadConfig);
end;

procedure TfoMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FServer) then
  begin
    FServer.Stop;
    FServer := nil;
  end;
end;

procedure TfoMain.Timer1Timer(Sender: TObject);
begin
  caption := str.format('%d %d', [allocmemsize, allocmemcount]);
end;

end.
