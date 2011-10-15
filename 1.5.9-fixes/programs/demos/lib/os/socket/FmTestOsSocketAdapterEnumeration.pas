unit FmTestOsSocketAdapterEnumeration;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask,

  Sil, ComCtrls, ExtCtrls;

type
  TFormTestOsSocketAdapterEnumeration = class(TForm)
    Panel1: TPanel;
    lvList: TListView;
    btEnumerate: TButton;
    procedure btEnumerateClick(Sender: TObject);
  end;

var
  FormTestOsSocketAdapterEnumeration: TFormTestOsSocketAdapterEnumeration;

implementation

{$R *.dfm}

procedure TFormTestOsSocketAdapterEnumeration.btEnumerateClick(Sender: TObject);
var
  list: ISocketAddresses;
  e: IEnumerator;
  i: ISocketAddress;
begin
  lvList.Items.BeginUpdate;
  try
    lvList.Items.Clear;

    list := Sil.OS.Socket.Info.AdapterList;

    while list.Enumerate(e, i) do
    begin
      with lvList.Items.Add do
      begin
        Caption := i.Host; 
        Subitems.Add(Sil.OS.Socket.IP.ToStr(i.Address));
        Subitems.Add(Sil.OS.Socket.IP.ToStr(i.SubnetMask));
        Subitems.Add(Sil.OS.Socket.IP.ToStr(i.Network));
        Subitems.Add(Sil.OS.Socket.IP.ToStr(i.Broadcast));
        Subitems.Add(Sil.Enum.Name(TypeInfo(TSocketNetworkClass), Ord(i.NetworkClass), 'nc'));
      end;
    end;

  finally
    lvList.Items.EndUpdate;
  end;
end;

end.

