unit FmTestOsProcessModules;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  Sil, ComCtrls;

type
  TFormTestOsProcessModules = class(TForm)
    Show: TButton;
    Mask: TEdit;
    Processes: TListView;
    Modules: TListView;
    procedure ShowClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ProcessesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTestOsProcessModules: TFormTestOsProcessModules;

implementation

uses SilBtInt, SilOtProcess;

{$R *.dfm}

procedure TFormTestOsProcessModules.FormCreate(Sender: TObject);
begin
  with Processes.Columns do
  begin
    with Add do
    begin
      Caption := '#';
      AutoSize := False;
      Width := 0;
      MinWidth := 0;
      MaxWidth := 0;
    end;
    with Add do
    begin
      Caption := 'id';
      Width := 70;
    end;
    with Add do
    begin
      Caption := 'name';
      Width := 250;
    end;
    with Add do
    begin
      Caption := 'file';
      Width := 300;
    end;
  end;
  with Modules.Columns do
  begin
    with Add do
    begin
      Caption := '#';
      AutoSize := False;
      Width := 0;
      MinWidth := 0;
      MaxWidth := 0;
    end;
    with Add do
    begin
      Caption := 'base';
      Width := 70;
    end;
    with Add do
    begin
      Caption := 'name';
      Width := 250;
    end;
    with Add do
    begin
      Caption := 'file';
      Width := 300;
    end;
  end;
end;

procedure TFormTestOsProcessModules.ShowClick(Sender: TObject);
var
  List: IProcessList;
  Item: IProcess;
  Enum: IEnumerator;
begin
  List := Sil.OS.Process.GetList(Mask.Text);

  Processes.Clear;
  while List.Enumerate(Enum, Item) do
  begin
    with Processes.Items.Add do
    begin
      Caption := Sil.Int.ToStr(Enum.Iteration);
      SubItems.Add(Sil.Int.ToStr(Item.PID));
      SubItems.Add(Item.Info.Name);
      SubItems.Add(Item.Info.FullName);
    end;
  end;
  
end;

procedure TFormTestOsProcessModules.ProcessesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  PID: Integer;
  Enum: IEnumerator;
  Process: IProcess;
  Module: IModule2;
begin
  if Selected then
  begin
    PID := Sil.Str.ToInt(Item.Subitems[0]);

    Process := Sil.Os.Process.Open(PID);
    with Process.Modules do
    begin
      Modules.Items.Clear;
      while Enumerate(Enum, Module) do
      begin
        with Modules.Items.Add do
        begin
          Caption := Sil.Int.ToStr(Enum.Iteration);
          SubItems.Add(Sil.Int.ToHex(Module.Handle.Value, 8));
          SubItems.Add(Module.Info.Name);
          SubItems.Add(Module.Info.FullName);
        end;
      end;
    end;
  end;
end;

end.
