unit FmTestOsSocketDumpProtocols;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask,

  Sil, ComCtrls, ExtCtrls;

type
  TFormTestOsSocketDumpProtocols = class(TForm)
    lvList: TListView;
    Panel1: TPanel;
    btDumpProtocols: TButton;
    procedure btDumpProtocolsClick(Sender: TObject);
  end;

var
  FormTestOsSocketDumpProtocols: TFormTestOsSocketDumpProtocols;

implementation

{$R *.dfm}

procedure TFormTestOsSocketDumpProtocols.btDumpProtocolsClick(Sender: TObject);
var
  i: Integer;
  p: ISocketProtocol;
begin
  lvList.Items.BeginUpdate;
  try
    lvList.Clear;
    
    for i := 0 to 255 do
    begin
      p := Sil.OS.Socket.Info.Protocol(i);
      if p.IsValid then
      begin
        with lvList.Items.Add do
        begin
          Caption := Sil.Int.ToStr(p.Id);
          Subitems.Add(p.Name);
          Subitems.Add(p.Aliases.GetText('; '));
          Subitems.Add(p.Comment);
        end;
      end;
    end;
    
  finally
    lvList.Items.EndUpdate;
  end;
end;

end.

