unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  Sil,
  SilLayer,
  SilCoder,
  SilSiLayerConnection,
  SilSiLayerConnectionImate, ExtCtrls;

type
  TForm1 = class(TForm, ILayerConnectionManager, ILayerClientConnectionManager)
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    ListBox1: TListBox;
    Panel2: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  protected // ILayerConnectionManager
    procedure Initialize(const Connection: ILayerConnection; const Chain: ILayerChain);
    procedure Connected(const Connection: ILayerConnection);
    procedure Disconnected(const Connection: ILayerConnection);
    procedure ConnectFailed(const Connection: ILayerConnection);
  private
    FManager: ILayerImateConnectionManager;
    FProt: IClientSideFileProtocol;
    procedure DoStatus(const Sender: IInterface; Param: Pointer);
    procedure DoRead(const Path: String);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  SilSmLayerConnection,
  SilSmLayerConnectionImate,
  SilLtList;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FManager := TSilLayerImateConnectionPeer.Create(Self);
  FManager.Start;
end;

procedure TForm1.Initialize(const Connection: ILayerConnection; const Chain: ILayerChain);
var
  Params: IParameterList;
begin
  Params := Sil.List.Parameters;
  Params['Host'] := '127.0.0.1';
  Params['Port'] := 25670;

  Chain.Add(SilLayer.Device.SocketClient(Params));
end;

procedure TForm1.Connected(const Connection: ILayerConnection);
var
  Chain: ILayerChain;
begin
  FProt := SilLayer.Protocol.FileClient;
  FManager.RequestProtocol(FProt, IServerSideFileProtocol, Chain);

  Sil.OS.Thread.SyncCall(DoStatus, Pointer(1));
end;

procedure TForm1.Disconnected(const Connection: ILayerConnection);
begin
  Sil.OS.Thread.SyncCall(DoStatus, Pointer(0));
end;

procedure TForm1.ConnectFailed(const Connection: ILayerConnection);
begin
  Sil.OS.Thread.SyncCall(DoStatus, Pointer(2));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if Assigned(FManager) then
  begin
    FManager.Stop;
    FManager := nil;
    FProt := nil;
  end;

  ListBox1.Clear;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Button2Click(nil);
end;

procedure TForm1.DoStatus(const Sender: IUnknown; Param: Pointer);
const
  AStatus: array [0..2] of String = ('desconectado', 'conectado', 'no se conecta');
var
  Idx: Integer;
begin
  Idx := Integer(Param);
  Caption := AStatus[Idx];

  if Idx = 1 then DoRead('\');
end;

procedure TForm1.DoRead(const Path: String);
var
  Reader: IDirectoryReader;
  Enum: IEnumerator;
  Item: IFileInfo;
begin
  if Str.FirstChar(Path) <> '\' then Exit;

  ListBox1.Clear;

  if Assigned(FProt) then
  begin
    panel2.caption := Str.IIf(panel2.caption <> '\', panel2.caption) + Path;
    Reader := FProt.ReadDirectory(panel2.caption);

    while Reader.Read do;

    while Reader.List.Enumerate(Enum, Item) do
      if faDirectory in Item.Attributes then
        ListBox1.Items.Add('\' + Item.Name) else
        ListBox1.Items.Add(Item.Name);
  end;
end;

procedure TForm1.ListBox1DblClick(Sender: TObject);
begin
  DoRead(listbox1.Items[listbox1.ItemIndex]);
end;

end.
