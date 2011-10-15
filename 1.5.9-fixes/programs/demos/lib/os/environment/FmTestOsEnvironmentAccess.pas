unit FmTestOsEnvironmentAccess;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,

  Sil,
  StdCtrls;

type
  TFormTestEnvironmentAccess = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTestEnvironmentAccess: TFormTestEnvironmentAccess;

implementation

{$R *.DFM}

procedure TFormTestEnvironmentAccess.FormCreate(Sender: TObject);
var
  l: IValueList;
  e: IEnumerator;
  f: IFieldAccess;
begin
  memo1.lines.add('----------------------------------');
  memo1.lines.add('Environment Variable Lookup: ');
  memo1.lines.add('----------------------------------');
  memo1.lines.add('OS: ' + OS.Environment.Variable('OS').AsString);
  memo1.lines.add('USERPROFILE: ' + OS.Environment.Variable('USERPROFILE').AsString);

  memo1.lines.add('----------------------------------');
  memo1.lines.add('Environment Variable Enumeration: ');
  memo1.lines.add('----------------------------------');
  l := OS.Environment.Variables;
  while l.Enumerate(e, f) do
    memo1.lines.add(f.Name + ': ' + f.AsString);
end;

end.
