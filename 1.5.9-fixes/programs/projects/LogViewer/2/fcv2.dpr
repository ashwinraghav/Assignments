program fcv2;

uses
  SIL,
  Forms,
  Graphics,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

procedure ReadPos(Form: TForm1);
var
  Conf: INamedKey;
begin
  Conf := Sil.OS.Registry.Open('$System\SOFTWARE\Siderca\fcv', true);

  with Conf.Values do
  begin
    Form.SetBounds(
      ReadInteger('Left', 0, true),
      ReadInteger('Top', 0, true),
      ReadInteger('Width', 200, true),
      ReadInteger('Height', 400, true));

    Form.Color :=  StringToColor(ReadString('WindowColor', ColorToString(Form.Color), True));
    Form.Font.Color :=  StringToColor(ReadString('FontColor', ColorToString(Form.Font.Color), True));
    Form.Font.Name := ReadString('FontName', Form.Font.Name, True);
    Form.Font.Size := ReadInteger('FontSize', Form.Font.Size, True);
    if ReadBoolean('StayOnTop', Form.FormStyle = fsStayOnTop, True) then
      Form.FormStyle := fsStayOnTop else
      Form.FormStyle := fsNormal;
    Form.Interval := ReadInteger('Interval', Form.Interval, True);
    Form.Lines  := ReadInteger('Lines', Form.Lines, True);
  end;
end;

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  ReadPos(Form1);
  Application.Run;
end.
