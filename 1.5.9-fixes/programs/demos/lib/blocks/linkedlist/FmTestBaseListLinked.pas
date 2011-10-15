unit FmTestBaseListLinked;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Sil;

type
  TFormTestLinked = class(TForm)
    LinkedAdd: TButton;
    Listbox: TListBox;
    LinkedEnum: TButton;
    LinkedRemove: TButton;
    LinkedClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LinkedAddClick(Sender: TObject);
    procedure LinkedEnumClick(Sender: TObject);
    procedure LinkedRemoveClick(Sender: TObject);
    procedure LinkedClearClick(Sender: TObject);
  private
    FList: ILinkedList;
  end;

var
  FormTestLinked: TFormTestLinked;

implementation

{$R *.dfm}

procedure TFormTestLinked.FormCreate(Sender: TObject);
begin
  // Sil.List.LinkedList(Locked = False, TypeHandler = StringHandler)
  //    nonlocked list with items handled by StringHandler
  FList := Sil.List.LinkedList(False, StringHandler);
end;

procedure TFormTestLinked.FormDestroy(Sender: TObject);
begin
  FList := nil;
end;

procedure TFormTestLinked.LinkedAddClick(Sender: TObject);
var
  S: string;
begin
//  List.Add(const Item): the Type handler receives this
//    takes the responsibility of copy and destruction of
//    items
  S := Sil.DateTime.ToStr(Now);
  FList.Add(S);
end;

procedure TFormTestLinked.LinkedEnumClick(Sender: TObject);
var
  Enum: IEnumerator;
  Item: string;
begin
  Listbox.Items.Clear;
  while FList.Enumerate(Enum, Item) do
    //  Enumerate always returns a copy of the items inside the list
    Listbox.Items.Add(Item);
end;

procedure TFormTestLinked.LinkedRemoveClick(Sender: TObject);
var
  Item: string;
begin
  with Listbox do
    if ItemIndex <> -1 then
      Item := Items[ItemIndex] else
      Exit;

  FList.Remove(Item);
end;

procedure TFormTestLinked.LinkedClearClick(Sender: TObject);
begin
  FList.Clear;
end;

end.

