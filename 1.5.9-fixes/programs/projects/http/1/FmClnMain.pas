unit FmClnMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,

  Sil, SilTool, SilUrl, SilLog, SilHttp, StdCtrls;

type
  TFormHttpClient = class(TForm)
    edUrl: TEdit;
    Get: TButton;
    edContent: TMemo;
    edContentType: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GetClick(Sender: TObject);
  private
    FHttp: IHttpFiler;
    FSocket: ISocketClient;
  private 
    function DoBuildQuery(const Url: IUrl): IHttpRequest;
    procedure DoWrite(const Socket: ISocketClient; const Data: IHttpObject);
    function DoRead(const Socket: ISocketClient): IHttpResponse;
  public
    { Public declarations }
  end;

var
  FormHttpClient: TFormHttpClient;

implementation

uses SilSiUrl;

{$R *.dfm}

function TFormHttpClient.DoBuildQuery(const Url: IUrl): IHttpRequest;
begin
  Result := SilHttp.Tk.Request;

  Result.Request.Method := httpGet;
  with Result.Request.Resource.Url do
    Path.Text := Url.Path.Text;

  IHttpTag(Result.Tags.Host).Value := Url.Address.Host.Server;

  Result.Header.Connection.Value := 'Keep-Alive';
  Result.Tags.UserAgent.Value := 'httpcln/1.00 sil/1.5.0 (Sil Http Client V1.0)';
  Result.Entity.Tags.Allow.Value := 'text/plain, text/html, */*';
end;

procedure TFormHttpClient.DoWrite(const Socket: ISocketClient; const Data: IHttpObject);
var
  Buffer: IMemoryStream;
begin
  Buffer := Sil.MemoryStream.Create();
  FHttp.Write(Buffer, Data);
  if Buffer.Size > 0 then
    Socket.Stream.Write(Buffer.Memory^, Buffer.Size);
end;

function TFormHttpClient.DoRead(const Socket: ISocketClient): IHttpResponse;
begin
  FHttp.Read(Socket.Stream, Result);
end;

procedure TFormHttpClient.FormCreate(Sender: TObject);
begin
  FHttp := SilHttp.Tk.Filer();
end;

procedure TFormHttpClient.FormDestroy(Sender: TObject);
begin
  FHttp := nil;
end;

procedure TFormHttpClient.GetClick(Sender: TObject);
var
  Url: IUrl;
  Query: IHttpRequest;
  Reply: IHttpResponse;
begin
  if Str.Assigned(edUrl.Text) then
  begin
    Url := SilUrl.Url.Create(edUrl.Text);
    if not Assigned(FSocket) then
    begin
      FSocket := Sil.Os.Socket.CreateClient(stStream, spTCP, Url.Address.Host.Server, Sil.Str.ToInt(Url.Address.Host.Port, 80));
      FSocket.Parameters.ReadTimeout := 30 * 1000;
      FSocket.Connect();
    end;
    try

      Query := DoBuildQuery(Url);

      DoWrite(FSocket, Query);
      Reply := DoRead(FSocket);

      edContent.Lines.Clear;
      edContent.Lines.Add(FHttp.ToStr(Reply.Tags.List));
      edContent.Lines.Add(Reply.Entity.Stream.Memory);

    finally
      if not Assigned(Reply) or (Str.CompareText(Reply.Header.Connection.Value, 'close', True) = 0) then
      begin
        FSocket.Disconnect;
        FSocket := nil;
      end;
    end;   
  end;
end;

end.
