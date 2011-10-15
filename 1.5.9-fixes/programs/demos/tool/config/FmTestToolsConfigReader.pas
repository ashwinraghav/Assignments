unit FmTestToolsConfigReader;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  Sil,
  SilTool;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Label2: TLabel;
    Button3: TButton;
    read: TButton;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure readClick(Sender: TObject);
  private
    FConfig: IConfiguration;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

(*)
    xml://d:/dir/dir/dir/file.ext
    reg://[machine/]system/key/key/key
    ini://d:/dir/dir/file.ext
(*)

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  //edit1.text := 'xml://' + Sil.OS.Process.Current.Info.Path + 'test.xml';
  edit1.text := 'reg://system/software/sil/test/config';
end;

procedure TForm1.Button1Click(Sender: TObject);

  procedure DoEnum(const Item: IConfigNode; Level: Integer);
  var
    e1, e2: IEnumerator;
    Child: IConfigNode;
    Data: RConfigData;
    Spc: String;
    Value: IVariable;
  begin
    while Item.Enumerate(e1, Child) do
    begin
      DoEnum(Child, Level + 1);

      Spc := Str.ReplicateChar('-', Level);
      memo1.lines.Add(Spc + 'nodo ' + Child.Name);

      while Child.Data.Enumerate(e2, Data) do
        memo1.lines.Add(Str.Format('%s--data %s=%s', [Spc, Data.Name, Data.Item.AnsiString.Value]));
    end;

    if Item.Data.Find('@', Value) then
      memo1.lines.Add(Str.Format('%s--body %s', [Spc, Value.AnsiString.Value]));
  end;

begin
  FConfig := SilTool.Sv.Config.Open(edit1.Text, true);

  memo1.lines.add(FConfig.Content.Name);
  DoEnum(FConfig.Content, 0);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if Assigned(FConfig) then
    with FConfig.Content.Get('level1', true) do
      label2.Caption := 'arg3=' + Data.Get('arg3', true).AnsiString.Read('def');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if Assigned(FConfig) then
    with FConfig.Content.Get('level1', true) do
      Data.Get('arg3', true).Integer.Value := 3;
end;

procedure TForm1.readClick(Sender: TObject);
begin
  if Assigned(FConfig) then
    with FConfig.Content.Get('level1/level2', true) do
      label3.Caption := 'arg3=' + Data.Get('arg3', true).AnsiString.Read('def3');
end;

end.
