program tail;

uses
  Sil,
  Forms,
  FmMainTail in 'FmMainTail.pas' {FormMainTail};

{$R *.RES}

procedure ReadPos(Form: TForm);
var
  Conf: INamedKey;
begin
  Conf := Sil.Os.Registry.Open('$System\SOFTWARE\SIL\tail', true);

  with Conf.Values do
    Form.SetBounds(
      ReadInteger('Left', 0, true),
      ReadInteger('Top', 0, true),
      ReadInteger('Width', 200, true),
      ReadInteger('Height', 400, true));
end;

begin
  Application.Initialize;
  Application.CreateForm(TFormMainTail, FormMainTail);
  ReadPos(Application.MainForm);
  Application.Run;
end.
