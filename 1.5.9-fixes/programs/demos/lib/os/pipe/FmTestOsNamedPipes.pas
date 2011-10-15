unit FmTestOsNamedPipes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  Sil, 
  SilVCL;

const
  THREAD_CLIENT = WM_USER + 123;

type
  TFormTestOsNamedPipes = class(
    TForm,
    IDispatchable,
    IRunnable )
    Memo: TMemo;
    Connect: TButton;
    Edit1: TEdit;
    ListBox1: TListBox;
    edPC: TEdit;
    Listen: TButton;
    Write: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ConnectClick(Sender: TObject);
    procedure ListenClick(Sender: TObject);
    procedure WriteClick(Sender: TObject);
  private
    FThread: IThread;
    FServer: INamedPipeServer;
    FClients: IInterfaceList;
    procedure ThreadClient(var Msg: RThreadRunMessage); message THREAD_CLIENT;
  protected //- IRunnable
    procedure Run(const Thread: IThread);
    procedure DoAddStr(const Sender: IUnknown; Param: Pointer);
    procedure DoAddItem(const Sender: IUnknown; Param: Pointer);
  end;

  TClient = class(
    TSilObject,
    IRunnable )
  private
    FPipe: INamedPipeServerClient;
  protected //- IRunnable
    procedure Run(const Thread: IThread);
  public
    constructor Create(const Pipe: INamedPipeServerClient);
    destructor Destroy; override;
  end;

var
  FormTestOsNamedPipes: TFormTestOsNamedPipes;

implementation

{$R *.dfm}

{ TClient }

constructor TClient.Create(const Pipe: INamedPipeServerClient);
begin
  inherited Create;
  FPipe := Pipe;
  Sil.Os.Thread.Spawn(Self);
end;

destructor TClient.Destroy;
begin
  FPipe := nil;
  inherited;
end;

procedure TClient.Run(const Thread: IThread);
var
  S: String;
  N: LongWord;
begin
  while True do
  try
    SetLength(S, 1024);
    N := FPipe.Stream.Read(S[1], Length(S));
    if N = 0 then Break;
    SetLength(S, N);
    Sil.Os.Thread.SyncCall(FormTestOsNamedPipes.DoAddStr, PChar(S));
    S := 'OK: ' + S;
    FPipe.Stream.Write(S[1], Length(S));
  except on Ex: Exception do
    begin
      S := 'ERROR! : ' + Ex.Message;
      Sil.Os.Thread.SyncCall(FormTestOsNamedPipes.DoAddStr, PChar(S));
      Break;
    end;
  end;
end;

{ TFormTestOsNamedPipes }

procedure TFormTestOsNamedPipes.FormCreate(Sender: TObject);
begin
  edPC.Text := Sil.Os.Socket.Host.LocalName;
end;

procedure TFormTestOsNamedPipes.FormDestroy(Sender: TObject);
var
  Thread: IThread;
begin
  if Assigned(FClients) then
  begin
    FClients.Locked;
    FClients.Clear;
  end;

  Thread := FThread;
  FThread := nil;
  FServer := nil;

  if Assigned(Thread) then Sil.OS.Wait.Single(Thread, INFINITE, True);
  FClients := nil;
end;

procedure TFormTestOsNamedPipes.ListenClick(Sender: TObject);
begin
  FClients := Sil.List.InterfaceList();
  FServer := Sil.Os.Pipe.Create('\\.\pipe\prueba', paDuplex);
  FThread := Sil.Os.Thread.Spawn(Self);
end;

procedure TFormTestOsNamedPipes.Run(const Thread: IThread);
var
  Pipe: INamedPipeServerClient;
begin
  try
    while FServer.Connect(Pipe) do
      FClients.Add(TClient.Create(Pipe));
  except
    Application.HandleException(Self)
  end;
end;

procedure TFormTestOsNamedPipes.ThreadClient(var Msg: RThreadRunMessage);
var
  S: String;
  N: LongWord;
  Pipe: INamedPipeClient;
begin
  Pipe := Sil.Os.Pipe.Open('\\' + edPC.Text + '\pipe\prueba', paDuplex);

  while True do
  try
    S := Msg.Thread.Name + ': ' + Sil.Time.ToStr(Now);
    Pipe.Stream.Write(S[1], Length(S));

    SetLength(S, 255);
    N := Pipe.Stream.Read(S[1], Length(S));
    if N = 0 then Break;
    SetLength(S, N);
    Sil.Os.Thread.SyncCall(DoAddStr, PChar(S));

    Sleep(1000);
  except on Ex: Exception do
    begin
      S := 'ERROR! : ' + Ex.Message;
      Sil.Os.Thread.SyncCall(DoAddStr, PChar(S));
      Break;
    end;
  end;

end;

procedure TFormTestOsNamedPipes.ConnectClick(Sender: TObject);
begin
  Sil.OS.Thread.Spawn(THREAD_CLIENT, Edit1.Text, Self);
end;

procedure TFormTestOsNamedPipes.DoAddStr(const Sender: IInterface; Param: Pointer);
begin
  Memo.Lines.Add(PChar(Param));
end;

procedure TFormTestOsNamedPipes.DoAddItem(const Sender: IInterface; Param: Pointer);
begin
  ListBox1.Items.Add(PChar(Param));
end;

procedure TFormTestOsNamedPipes.WriteClick(Sender: TObject);
var
  S: String;
  Pipe: INamedPipeClient;
begin
  Pipe := Sil.Os.Pipe.Open('\\.\pipe\prueba', paDuplex);

  S := 'hola mundo';
  Pipe.Stream.Write(S[1], Length(S));
end;

end.
