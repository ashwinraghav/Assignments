unit FmTestOsEnvironmentExpand;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Sil, ExtCtrls, ComCtrls, StdCtrls;

type
  TFormTestEnvironmentExpand = class(TForm)
    Panel: TPanel;
    lvPath: TListView;
    Panel1: TPanel;
    edPath: TEdit;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure edPathChange(Sender: TObject);
  private
  public
  end;

var
  FormTestEnvironmentExpand: TFormTestEnvironmentExpand;

implementation

uses SilBtInt;

{$R *.dfm}

procedure TFormTestEnvironmentExpand.FormCreate(Sender: TObject);
begin
  Panel.Caption := Sil.Os.Environment.Expand('%SystemRoot%\System32\Drivers;%CommonProgramFiles%;%Temp%');
  edPath.Text := Sil.Os.Environment.Variable('PATH').AsString;
end;

procedure TFormTestEnvironmentExpand.edPathChange(Sender: TObject);
var
  x, n: Integer;
  s: string;
begin
  lvPath.Items.BeginUpdate;
  try
    lvPath.Items.Clear;
    
    x := 0; n := 1;
    while Sil.Str.Enumerate(edPath.Text, CPathListSeparator, S, X) do
      with lvPath.Items.Add do
      begin
        Caption := Sil.Int.ToStr(n);
        Subitems.Add(S);
        Subitems.Add(Sil.Os.Environment.Expand(S));
        Inc(n);
      end;
  finally
    lvPath.Items.EndUpdate;
  end;
end;

end.
