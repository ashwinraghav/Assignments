unit FmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls,

  Sil,
  UiPeer,
  UmPeer;

type
  TfoMain = class(TForm, IPeerHook)
    Panel1: TPanel;
    cbPeers: TComboBox;
    Panel2: TPanel;
    btConnect: TButton;
    btDisconnect: TButton;
    lvFiles: TListView;
    procedure btConnectClick(Sender: TObject);
    procedure cbPeersChange(Sender: TObject);
  private
    procedure DoAddPeer(const Address: String; Port: Integer);
    function DoGetSelectedPeer: IPeer;
    function DoGetPeerIndex(const Peer: IPeer;
      out Index: Integer): Boolean;
    procedure DoReadDirs(const Peer: IPeer);
  protected // IPeerHook
    procedure Connected(const Peer: IPeer);
    procedure Content(const Peer: IPeer; const Path: String; Action: TContentAction; const List: IFileInfoList);
    procedure DownloadProgress(FileId, CurrentSize, Speed: LongWord);
    procedure DownloadCanceled(FileId: LongWord);
  end;

var
  foMain: TfoMain;

implementation

uses FmConectar;

{$R *.dfm}

procedure TfoMain.DoAddPeer(const Address: String; Port: Integer);
var
  Peer: IPeer;
begin
  Peer := TPeer.Create(Sil.OS.Computer.Local.Name, Address, Port, Self);
  cbPeers.Items.AddObject(Str.Format('-%s:%d', [Address, Port]), Pointer(Peer));
end;

function TfoMain.DoGetPeerIndex(const Peer: IPeer; out Index: Integer): Boolean;
var
  i: Integer;
begin
  for i := 0 to cbPeers.Items.Count - 1 do
    if Ref.SameObject(IPeer(Pointer(cbPeers.Items.Objects[i])), Peer) then
    begin
      Index := i;
      Result := true;
      Exit;
    end;

  Result := false;
end;

function TfoMain.DoGetSelectedPeer: IPeer;
begin
  if (cbPeers.ItemIndex >= 0) and (cbPeers.Items.Count > 0) then
    Result := IPeer(Pointer(cbPeers.Items.Objects[cbPeers.ItemIndex])) else
    Result := nil;
end;

procedure TfoMain.btConnectClick(Sender: TObject);
begin
  with TfoConectar.Create(Self) do
    try
      if ShowModal = mrOk then
        DoAddPeer(edAddress.Text, Str.ToInt(edPort.Text, 0));
    finally
      Free;
    end;
end;

procedure TfoMain.Connected(const Peer: IPeer);
var
  Index: Integer;
begin
  if DoGetPeerIndex(Peer, Index) then
  begin
    Text := '+' + Str.Copy(cbPeers.Items[Index], 2);
    cbPeers.Items[Index] := Text;
  end;
end;

procedure TfoMain.Content(const Peer: IPeer; const Path: String; Action: TContentAction; const List: IFileInfoList);
var
  Selected: IPeer;
  Item: IUnknown;
  Info: IFileInfo;
begin
  Selected := DoGetSelectedPeer;

  if Ref.SameObject(Selected, Peer) then
  begin
    if Action = caNew then
      lvFiles.Clear;

    while Info.Enumerate(Enum, Item) do
    begin
      Info := Item as IFileInfo;

      with lvFiles.Items.Add do
      begin
        Caption := Info.Name;
        Subitems.Add(s1);
      end;
    end;
  end;
end;

procedure TfoMain.DownloadCanceled(FileId: LongWord);
begin

end;

procedure TfoMain.DownloadProgress(FileId, CurrentSize, Speed: LongWord);
begin

end;

procedure TfoMain.DoReadDirs(const Peer: IPeer);
begin
  if Assigned(Peer) then Peer.ReadContent;
end;

procedure TfoMain.cbPeersChange(Sender: TObject);
begin
  DoReadDirs(DoGetSelectedPeer);
end;

end.
