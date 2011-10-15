unit FmTestOsSocketDumpServices;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask,

  Sil, ComCtrls, ExtCtrls;

type
  TFormTestOsSocketDumpServices = class(TForm)
    lvList: TListView;
    Panel1: TPanel;
    btDumpServices: TButton;
    procedure btDumpServicesClick(Sender: TObject);
  end;

var
  FormTestOsSocketDumpServices: TFormTestOsSocketDumpServices;

implementation

{$R *.dfm}

procedure TFormTestOsSocketDumpServices.btDumpServicesClick(Sender: TObject);
var
  i: Integer;
  s: ISocketService;
begin
  lvList.Items.BeginUpdate;
  try
    lvList.Clear;

    for i := 0 to 1023 do
    begin
      s := Sil.OS.Socket.Info.Service(i);
      if s.IsValid then
      begin
        with lvList.Items.Add do
        begin
          Caption := s.Name;
          Subitems.Add(s.Protocol.Name);
          Subitems.Add(s.Aliases.GetText('; '));
          Subitems.Add(s.Comment);
          Subitems.Add(Sil.Int.ToStr(Sil.OS.Socket.Info.Service(s.Name).Port));
        end;
      end;
    end;

  finally
    lvList.Items.EndUpdate;
  end;
end;

end.

