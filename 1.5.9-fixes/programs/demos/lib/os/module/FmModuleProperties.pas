unit FmModuleProperties;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SilVkCustomControl, SilVkControl, SilVmCustomButton,
  SilVmButton, ExtCtrls, ComCtrls,

  Sil;

type
  TFormModuleProperties = class(TForm)
    lvProperties: TListView;
    pnButtons: TPanel;
    btOK: TSilButton;
  private
    procedure DoLoadProperties(const Module: IModule2);
  public
    class function Execute(const Module: IModule): Boolean;
  end;

implementation

{$R *.dfm}

{ TFormModuleProperties }

class function TFormModuleProperties.Execute(const Module: IModule): Boolean;
begin
  with Create(nil) do
  try
    DoLoadProperties(Module as IModule2);
    Result := ShowModal() = mrOK; 
  finally
    Free;
  end;
end;

procedure TFormModuleProperties.DoLoadProperties(const Module: IModule2);
  procedure DoAdd(const Name, Value: string);
  begin
    with lvProperties.Items.Add do
    begin
      Caption := Name;
      Subitems.Add(Value);
    end;
  end;
begin
  DoAdd('Name', Module.Info.Name);
  DoAdd('Base', Sil.Int.ToHex(Module.Handle.Value, 8));
  DoAdd('Path', Module.Info.Path);
  DoAdd('Size', Sil.Int.ToStr(Module.Info.Size));
  DoAdd('FileVersion', Module.Info.Version.Number.ToStr(CLongVersion));
  DoAdd('ProductName', Module.Info.Version.Tags.Std.ProductName);
  DoAdd('CompanyName', Module.Info.Version.Tags.Std.CompanyName);
  DoAdd('FileDescription', Module.Info.Version.Tags.Std.FileDescription);
  DoAdd('InternalName', Module.Info.Version.Tags.Std.InternalName);
  DoAdd('LegalCopyright', Module.Info.Version.Tags.Std.LegalCopyright);
  DoAdd('OriginalFilename', Module.Info.Version.Tags.Std.OriginalFilename);
  DoAdd('Comments', Module.Info.Version.Tags.Tag['Comments'].AsString);
end;

end.
