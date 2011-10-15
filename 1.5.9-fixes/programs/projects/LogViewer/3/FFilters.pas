unit FFilters;

interface

uses
  Sil,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, Buttons,
  UDef, Menus;

type
  TFormFilters = class(TForm)
    gbListaGrupos: TGroupBox;
    Panel1: TPanel;
    lvGrupos: TListView;
    gbGrupo: TGroupBox;
    Panel4: TPanel;
    gbAcciones: TGroupBox;
    Panel2: TPanel;
    gbGroupExcluded: TGroupBox;
    Panel3: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel5: TPanel;
    btDelete: TBitBtn;
    btNew: TBitBtn;
    pnGroupProperties: TPanel;
    Label1: TLabel;
    edGroupName: TEdit;
    btOk: TBitBtn;
    btCancel: TBitBtn;
    PopupMenu1: TPopupMenu;
    Nuevo1: TMenuItem;
    edActions: TMemo;
    edGroupDontContains: TMemo;
    btUp: TBitBtn;
    btDown: TBitBtn;
    chkMustExclude: TCheckBox;
    gbGroupIncluded: TGroupBox;
    Panel6: TPanel;
    edGroupContains: TMemo;
    chkMustInclude: TCheckBox;
    chkStopEvaluation: TCheckBox;
    procedure btNewClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btOkClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure EditGroupInfo(Sender: TObject);
    procedure lvGruposCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure btUpClick(Sender: TObject);
    procedure btDownClick(Sender: TObject);
    procedure ChageGropInfo(Sender: TObject);
    procedure lvGruposChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lvGruposClick(Sender: TObject);
  private
    FUpdatingControls: integer;
    FFilters: IFilters;
    FGroup: IGroup;
    FEditing: boolean;
  protected
    procedure DoUpdateAll;
    procedure DoUpdateInfo( PutInfo: boolean = true );
    function DoGetGroupInfo: boolean;
    function DoPutGroupInfo: boolean;
  public
    class function Execute(const AFilters: IFilters): Boolean;
  public
    property Filters: IFilters read FFilters write FFilters;
  end;

var
  FormFilters: TFormFilters;

implementation

uses SilLiStringList;

{$R *.dfm}

{ TFormFilters }

class function TFormFilters.Execute(const AFilters: IFilters): Boolean;
begin
  with TFormFilters.Create( nil ) do
    try
      Filters := AFilters;
      DoUpdateAll;
      Result := ShowModal = mrOK;
    finally
      Free;
    end;
end;

procedure TFormFilters.lvGruposClick(Sender: TObject);
begin
  if Assigned( lvGrupos.Selected ) then
    FGroup := FFilters.Groups.GroupByName[ lvGrupos.Selected.Caption ] else
    FGroup := nil;

  DoUpdateInfo;
end;

procedure TFormFilters.lvGruposChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if ( FUpdatingControls = 0 ) then
    if Assigned( Item ) and ( Length( Item.Caption ) > 0 ) then
      FFilters.Groups.GroupByName[ Item.Caption ].Enabled := Item.Checked;
end;

procedure TFormFilters.btNewClick(Sender: TObject);
begin
  FEditing := true;
  FGroup := FFilters.Groups.New;
  DoUpdateInfo;
  edGroupName.SetFocus;
end;

procedure TFormFilters.btDeleteClick(Sender: TObject);
begin
  FFilters.Groups.Remove( FGroup );
  FGroup := nil;
  DoUpdateAll;
end;

procedure TFormFilters.EditGroupInfo(Sender: TObject);
begin
  if not FEditing and ( FUpdatingControls = 0 ) then
  begin
    FEditing := true;
    DoUpdateInfo( false );
  end;
end;

procedure TFormFilters.ChageGropInfo(Sender: TObject);
begin
  if ( FUpdatingControls = 0 ) then
    DoGetGroupInfo;
end;

procedure TFormFilters.btOkClick(Sender: TObject);
begin
  if not DoGetGroupInfo then
    exit;

  FEditing := false;
  DoUpdateAll;
end;

procedure TFormFilters.btCancelClick(Sender: TObject);
begin
  if ( Length( FGroup.Name ) = 0 ) then
  begin
    FFilters.Groups.Remove( FGroup );
    FGroup := nil;
  end;

  FEditing := false;
  DoUpdateAll;
end;

procedure TFormFilters.btUpClick(Sender: TObject);
begin
  FGroup.Order := FGroup.Order + 1;
  DoUpdateAll;
end;

procedure TFormFilters.btDownClick(Sender: TObject);
begin
  FGroup.Order := FGroup.Order - 1;
  DoUpdateAll;
end;

procedure TFormFilters.DoUpdateAll;
var
  enum: IEnumerator;
  grp: IGroup;
  itm, sel: TListItem;
begin
  Inc( FUpdatingControls );
  try
    sel := nil;
    lvGrupos.items.Clear;
    while FFilters.Groups.Enumerate( enum, grp ) do
    begin
      itm := lvGrupos.Items.Add;
      with itm do
      begin
        Caption := grp.Name;
        Checked := grp.Enabled;
        Data := Pointer( grp.Order );
        if Assigned( FGroup ) and ( grp.Name = FGroup.Name ) then
          sel := itm;
      end;
    end;

    DoUpdateInfo;
    lvGrupos.Selected := sel;
    if Visible and lvGrupos.Enabled then
      lvGrupos.SetFocus;
  finally
    Dec( FUpdatingControls );
  end;
end;

procedure TFormFilters.DoUpdateInfo( PutInfo: boolean );
var
  isgroup: boolean;
  isnormal: boolean;
begin
  Inc( FUpdatingControls );
  try
    isgroup := Assigned( FGroup );
    isnormal := isgroup and not FGroup.System;

    btNew.Enabled := not FEditing;
    lvGrupos.Enabled := not FEditing;
    pnGroupProperties.Visible := FEditing;

    gbGrupo.Enabled := isgroup;
    gbGroupIncluded.Enabled := isnormal;
    gbGroupExcluded.Enabled := isnormal;
    edGroupName.ReadOnly := not isnormal;
    btDelete.Enabled := isnormal;
    btUp.Enabled := isgroup and ( FGroup.Order + 1 < FFilters.Groups.Count );
    btDown.Enabled := isgroup and ( FGroup.Order > 0 );

    if PutInfo then
      DoPutGroupInfo;
  finally
    Dec( FUpdatingControls );
  end;
end;

function TFormFilters.DoGetGroupInfo: boolean;
begin
  result := false;
  if Assigned( FGroup ) then
    try
      FGroup.Name := edGroupName.Text;
      FGroup.Actions := edActions.Text;
      FGroup.KeysIncluded := edGroupContains.Text;
      FGroup.KeysExcluded := edGroupDontContains.Text;
      FGroup.KeysIncludedEnabled := chkMustInclude.Checked;
      FGroup.KeysExcludedEnabled := chkMustExclude.Checked;
      FGroup.StopEvaluation := chkStopEvaluation.Checked;
      result := true;
    except on ex: Exception do
      ShowMessage( ex.Message );
    end;
end;

function TFormFilters.DoPutGroupInfo: boolean;
begin
  Inc( FUpdatingControls );
  try
    result := false;
    if Assigned( FGroup ) then
      try
        gbGrupo.Caption := ' Grupo: ' + FGroup.Name + ' ';
        edActions.Text := FGroup.Actions;
        edGroupName.Text := FGroup.Name;
        edGroupContains.Text := FGroup.KeysIncluded;
        edGroupDontContains.Text := FGroup.KeysExcluded;
        chkMustInclude.Checked := FGroup.KeysIncludedEnabled;
        chkMustExclude.Checked := FGroup.KeysExcludedEnabled;
        chkStopEvaluation.Checked := FGroup.StopEvaluation;
        result := true;
      except on ex: Exception do
        ShowMessage( ex.Message );
      end
    else
    begin
      gbGrupo.Caption := ' (no hay grupo seleccionado) ';
      edActions.Text := '';
      edGroupContains.Text := '';
      edGroupDontContains.Text := '';
      chkMustInclude.Checked := false;
      chkMustExclude.Checked := false;
    end;
  finally
    Dec( FUpdatingControls );
  end;
end;

procedure TFormFilters.lvGruposCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare := Integer( Item2.Data ) - Integer( Item1.Data );
end;

end.

