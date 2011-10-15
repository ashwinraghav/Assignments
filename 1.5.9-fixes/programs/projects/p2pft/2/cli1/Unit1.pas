unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  Sil,
  SilLayer,
  SilCoder;

type
  TForm1 = class(TForm, ILayerActivationEvents)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  protected // ILayerActivationEvents
    procedure OnLayerActivated(const Event: RLayerActivated);
    procedure OnLayerDeactivated(const Event: RLayerDeactivated);
  private
    FEblis: IEblisProtocol;
    FBlind: IBlindProtocol;
    FProt: IClientSideFileProtocol;
    FSlot: ILayerSlot;
    FChain: ILayerChain;
    FKey: String;
    procedure DoStatus(const Sender: IInterface; Param: Pointer);
    procedure DoClean;
    procedure DoReqEblis;
    procedure DoReqFile;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses SilLtList;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Params: IParameterList;
  Chain: ILayerChain;
begin
  Params := Sil.List.Parameters;
  Params['Host'] := '127.0.0.1';
  Params['Port'] := 25670;

  FChain := SilLayer.Layer.Chain;

  FBlind := SilLayer.Protocol.Blind;
  FSlot := SilLayer.Layer.Slot;

  FChain.Add(SilLayer.Device.SocketClient(Params));
  FChain.Add(SilLayer.Packer.Imate);
  FChain.Add(FSlot);

  Chain := SilLayer.Layer.Chain;
  Chain.Add(SilLayer.Protocol.Imate);
  Chain.Add(FBlind);

  FSlot.Add(Chain);

  Sil.Sink.Connect(FChain, Self);
  FChain.Control.Activate;

  DoReqEblis;
  DoReqFile;
end;

procedure TForm1.DoReqEblis;
var
  Params: IParameterList;
  Id: LongWord;
  Chain: ILayerChain;
begin
  if FBlind.Request(IEblisProtocol, Id) then
  begin
    Params := Sil.List.Parameters;
    Params['id'] := Id;

    FEblis := SilLayer.Protocol.Eblis;

    Chain := SilLayer.Layer.Chain;
    Chain.Add(SilLayer.Protocol.Imate(Params));
    Chain.Add(FEblis);

    FSlot.Add(Chain);

    Chain.Control.Activate;

    FEblis.Negotiate(FKey, 10);
  end;
end;

procedure TForm1.DoReqFile;
var
  Params: IParameterList;
  Id: LongWord;
  Chain: ILayerChain;
begin
  if FBlind.Request(IServerSideFileProtocol, Id) then
  begin
    Chain := SilLayer.Layer.Chain;
    Params := Sil.List.Parameters;

    if Assigned(FEblis) then
    begin
      Params['Cipher'] := SilCoder.Cipher.SimpleMix;
      Params['Key'] := FKey;

      Chain.Add(SilLayer.Layer.Cipher(Params));
      Params.Clear;
    end;

    Params['id'] := Id;

    FProt := SilLayer.Protocol.FileClient;

    Chain.Add(SilLayer.Protocol.Imate(Params));
    Chain.Add(FProt);

    FSlot.Add(Chain);

    Chain.Control.Activate;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if Assigned(FChain) then
  begin
    FChain.Control.Deactivate;
    FChain.Clear(true);

    DoClean;

    FProt := nil;
    FBlind := nil;
    FChain := nil;
    FSlot := nil;
  end;
end;

procedure TForm1.DoClean;
begin
  Sil.Sink.Disconnect(FChain, Self);
  FProt := nil;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Reader: IDirectoryReader;
  Enum: IEnumerator;
  Item: IFileInfo;
begin
  ListBox1.Clear;
  
  if Assigned(FProt) then
  begin
    Reader := FProt.ReadDirectory('/');

    while Reader.Read do;

    while Reader.List.Enumerate(Enum, Item) do
      ListBox1.Items.Add(Item.FullName);
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Button2Click(nil);
end;

procedure TForm1.OnLayerActivated(const Event: RLayerActivated);
begin
  Sil.OS.Thread.SyncCall(DoStatus, Pointer(true));
end;

procedure TForm1.OnLayerDeactivated(const Event: RLayerDeactivated);
begin
  Sil.OS.Thread.SyncCall(DoStatus, Pointer(false));
end;

procedure TForm1.DoStatus(const Sender: IUnknown; Param: Pointer);
const
  AStatus: array [Boolean] of String = ('desconectado', 'conectado');
var
  IsOnline: Boolean;
begin
  IsOnline := Boolean(Param);
  Caption := AStatus[IsOnline];

  if not IsOnline then DoClean;
end;

end.
