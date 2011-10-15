unit amain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,

  Sil,

  UiData,

  UtData,
  UiServer,
  UmServer;

type
  TForm3 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FServer: IOuchServer;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.DFM}

procedure TForm3.FormCreate(Sender: TObject);
var
  DataMgr: IDataMgr;
  Connection: IServerSocketConnection;
  UdpServer: IServerSocket;
begin
  DataMgr := Db.CreateMgr;

  Connection := Sil.Sv.Socket.CreateServer;
  Connection.Parameters.Port := 23360;

  UdpServer := Sil.OS.Socket.DatagramServer;
  UdpServer.Parameters.Port := 23361;

  FServer := TOuchServer.Create(DataMgr, Connection, UdpServer);
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  if FServer <> nil then
  begin
    FServer.Shutdown;
    FServer := nil;
  end;
end;

end.
