unit FmTestOsConfigRegistryNotification;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, 
  StdCtrls, Sil;

type
  TFormTestRegistryNotification = class(TForm, IEvNamedKeyChanged)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FKey: INamedKey;
    procedure DoChanged(const Sender: IUnknown; Param: Pointer);
    procedure OnChanged(const Key: INamedKey);
  end;

var
  FormTestRegistryNotification: TFormTestRegistryNotification;

implementation

{$R *.DFM}

procedure TFormTestRegistryNotification.FormCreate(Sender: TObject);
var
  t: IEnumerator;
  i: String;
  k: INamedKey;
begin
  k := Sil.OS.Registry.Open('$System\Software\Prueba', kpReadWrite, true);
  FKey := k.Keys.Get('key2', true).Notify(Self);

//  Sil.Lock.Take(FKey);

  FKey.Values.WriteInteger('uno', 123);
  FKey.Values.WriteString('dos', 'hola registry!');

  Caption := FKey.Values.ReadString('uno');

  memo1.lines.add('keys');
  while FKey.Keys.Enumerate(t, i) do memo1.lines.add(i);

  memo1.lines.add('values');
  while FKey.Values.Enumerate(t, i) do memo1.lines.add(i);
end;

procedure TFormTestRegistryNotification.FormDestroy(Sender: TObject);
begin
  Sil.Sink.Disconnect(FKey, Self);
  FKey := nil;
end;

procedure TFormTestRegistryNotification.OnChanged(const Key: INamedKey);
begin
  Sil.Os.Thread.SyncCall(DoChanged);
end;

procedure TFormTestRegistryNotification.DoChanged(const Sender: IInterface; Param: Pointer);
var
  t: IEnumerator;
  i: String;
begin
  memo1.lines.add('- change -');
  
  Caption := FKey.Values.ReadString('uno');

  memo1.lines.add('keys');
  while FKey.Keys.Enumerate(t, i) do memo1.lines.add(i);

  memo1.lines.add('values');
  while FKey.Values.Enumerate(t, i) do memo1.lines.add(i);
end;

end.                                                     
