unit FmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,

  Sil,
  UiServer,
  UmServer;

type
  TForm3 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FProxy: IServer;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.FormCreate(Sender: TObject);
var
  Info: IProxyInfo;
begin
  Info := TProxyInfo.Create('proxy.siderca.ot', 80, Sil.OS.Computer.Local.Name, 'SIDERCA', 't58738', 'azucarada');
  FProxy := TServer.Create(8080, Info);
  FProxy.Start;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  FProxy.Stop;
  FProxy := nil;
end;

end.
