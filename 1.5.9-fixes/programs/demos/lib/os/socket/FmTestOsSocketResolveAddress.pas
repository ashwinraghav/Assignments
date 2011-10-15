unit FmTestOsSocketResolveAddress;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask,

  Sil, ComCtrls, ExtCtrls;

type
  TFormTestOsSocketResolveAddress = class(TForm)
    Display: TMemo;
    Panel1: TPanel;
    btResolve: TButton;
    edAddress: TEdit;
    procedure btResolveClick(Sender: TObject);
  end;

var
  FormTestOsSocketResolveAddress: TFormTestOsSocketResolveAddress;

implementation

{$R *.dfm}

procedure TFormTestOsSocketResolveAddress.btResolveClick(Sender: TObject);
var
  a: ISocketAddress;
  s: String;
begin
  Display.Clear;

  a := Sil.OS.Socket.Host.GetByName(edAddress.Text);

  Display.Lines.Add('## direct lookup by name ' + edAddress.Text);
  if a <> nil then
  begin
    Display.Lines.Add('  host = ' + a.Host);
    s := Sil.OS.Socket.IP.ToStr(a.Address);
    Display.Lines.Add('  addr = ' + s);
    
    Display.Lines.Add('## reverse lookup by address ' + s);
    a := Sil.OS.Socket.Host.GetByAddress(s);

    if a <> nil then
    begin
      Display.Lines.Add('  host = ' + a.Host);
      s := Sil.OS.Socket.IP.ToStr(a.Address);
      Display.Lines.Add('  addr = ' + s);
    end else
      Display.Lines.Add('** Could not resolve address ' + s);

  end else
    Display.Lines.Add('** host not found');
end;

end.

