unit FmTestOsModules;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls,

  Sil, StdCtrls;

type
  TFormTestModules = class(TForm)
    lvList: TListView;
    pnBottom: TPanel;
    btRefresh: TButton;
    edModuleName: TEdit;
    btLookup: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btRefreshClick(Sender: TObject);
    procedure lvListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ShowModuleClick(Sender: TObject);
  private
    procedure DoRefreshList;
    procedure DoShowModule(const ModuleName: string);
  public
    { Public declarations }
  end;

var
  FormTestModules: TFormTestModules;

implementation

uses SilOiModule, SilOiFile, FmModuleProperties;

{$R *.dfm}

procedure TFormTestModules.FormCreate(Sender: TObject);
begin
  Caption := Sil.OS.Module.Current.FullName;
  DoRefreshList();
end;

procedure TFormTestModules.DoRefreshList;
var
  Enum: IEnumerator;
  Item: IModule2;
begin
  with Sil.Os.Process.Current.Modules do
    while Enumerate(Enum, Item) do
    begin
      with lvList.Items.Add() do
      begin
        Caption := Item.Info.Name;
        Subitems.Add(Sil.Int.ToHex(Item.Handle.Value, 8));
        Subitems.Add(Item.Info.Path);
        Subitems.Add(Item.Info.Version.Number.ToStr(CLongVersion));
        Subitems.Add(Item.Info.Version.Tags.Std.FileDescription);
        Subitems.Add(Item.Info.Version.Tags.Std.LegalCopyright);
      end;
    end;

end;

procedure TFormTestModules.btRefreshClick(Sender: TObject);
begin
  DoRefreshList();
end;

procedure TFormTestModules.lvListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Selected and Assigned(Item) then
    edModuleName.Text := Item.Caption else
    edModuleName.Text := '';
end;

procedure TFormTestModules.DoShowModule(const ModuleName: string);
begin
  TFormModuleProperties.Execute(Sil.Os.Module.Get(edModuleName.Text));
end;

procedure TFormTestModules.ShowModuleClick(Sender: TObject);
begin
  DoShowModule(edModuleName.Text)
end;

end.
