unit FmTestOsConfigRegistrySimple;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Registry,
  StdCtrls,

  Sil;

type
  TFormTestSimpleRegistry = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  end;

var
  FormTestSimpleRegistry: TFormTestSimpleRegistry;

implementation

{$R *.DFM}

procedure TFormTestSimpleRegistry.FormCreate(Sender: TObject);
var
  t: IEnumerator;
  i: String;
  k, s: INamedKey;
begin
  k := Sil.OS.Registry.Open('$System\Software\Prueba', kpReadWrite, true);
  s := k.Keys.Get('Key1\pepe2', true);
  k := nil;

  s.Values.WriteInteger('uno', 123);
  s.Values.WriteString('dos', 'hola registry!');

  Caption := s.Values.ReadString('uno');

  memo1.lines.add('keys');
  while s.Keys.Enumerate(t, i) do memo1.lines.add(i);

  memo1.lines.add('values');
  while s.Values.Enumerate(t, i) do memo1.lines.add(i);
end;

end.
