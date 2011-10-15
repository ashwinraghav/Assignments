unit FmTestBaseBlockPacketTyped;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  Sil, SilLjFiler;

type
  TFormTestPacketTyped = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    procedure DoTest(const Factory: FilerFactoryType);
  protected
  public
    { Public declarations }
  end;

var
  FormTestPacketTyped: TFormTestPacketTyped;

implementation

uses
  SilLmTypePacket;

{$R *.dfm}

const
  MGuid: TGuid = '{E84EDDD0-FEDF-468D-9C56-6BF5B36CD6D8}';

procedure TFormTestPacketTyped.DoTest(const Factory: FilerFactoryType);
var
  w: IPacket;
begin
  w := Factory.Packet();
  
  w.Writer.WriteString('texto');
  w.Writer.WriteString('texto nuevo', 20);
  w.Writer.WriteWideString(WideString('wide texto'));
  w.Writer.WriteWideString(WideString('wide texto nuevo'), 30);
  w.Writer.WriteDate(Date.Now);
  w.Writer.WriteInteger(1234);
  w.Writer.WriteGuid(MGuid);

  w.Buffer.Position := 0;

  memo1.lines.Add(w.Reader.ReadString);
  memo1.lines.Add(w.Reader.ReadString(20));
  memo1.lines.Add(w.Reader.ReadWideString);
  memo1.lines.Add(w.Reader.ReadWideString(30));
  memo1.lines.Add(Date.ToStr(w.Reader.ReadDate));
  memo1.lines.Add(Int.ToStr(w.Reader.ReadInteger));
  memo1.lines.Add(Guid.ToStr(w.Reader.ReadGuid));
end;

procedure TFormTestPacketTyped.FormCreate(Sender: TObject);
begin
  DoTest(Sil.Stream.Typed);
  DoTest(Sil.Stream.Raw);
  Caption := Int.ToStr(Sil.Mem.Info.Used);
end;

end.
 