unit UDef;

interface

uses
  Sil;

type
  IFilters = interface;
  IGroupList = interface;
  IGroup = interface;

  IFilters = interface
    ['{A265A38D-4A6B-48B2-A516-4A78F19DCF34}']
    function GetActions(const AText: string; out AActions: string): boolean;
    function GetGroups: IGroupList;
    // properties
    property Groups: IGroupList read GetGroups;
  end;

  IGroupList = interface
    ['{1F5FF034-01BE-40F6-92DF-05C4A1CB5BF4}']
    function GetGroup(Index: integer): IGroup;
    function GetGroupByName(const Name: string): IGroup;
    function New(System: boolean = false): IGroup;
    procedure Remove(const Item: IGroup);
    function GetCount: integer;
    function GetOrderOf(const Name: string): integer;
    procedure SetOrderOf(const Name: string; Value: integer);
    function Enumerate(var Enum: IEnumerator; out Item: IGroup): boolean;
    // properties
    property Count: integer read GetCount;
    property Group[Index: integer]: IGroup read GetGroup;
    property GroupByName[const Name: string]: IGroup read GetGroupByName;
    property OrderOf[const Name: string]: integer read GetOrderOf write SetOrderOf;
  end;

  IGroup = interface
    ['{94D57F2D-F2E1-4D01-AFDF-E9FEE1D6B213}']
    function GetEnabled: boolean;
    function GetName: string;
    function GetSystem: boolean;
    function GetIncludes: IStringList;
    procedure SetEnabled(Value: boolean);
    procedure SetName(const Value: string);
    function GetKeysExcluded: string;
    function GetKeysIncluded: string;
    procedure SetKeysExcluded(const Value: string);
    procedure SetKeysIncluded(const Value: string);
    function GetOrder: integer;
    procedure SetOrder(const Value: integer);
    function GetKeysExcludedEnabled: boolean;
    function GetKeysIncludedEnabled: boolean;
    procedure SetKeysExcludedEnabled(const Value: boolean);
    procedure SetKeysIncludedEnabled(const Value: boolean);
    function GetActions: string;
    procedure SetActions(const Value: string);
    function GetStopEvaluation: boolean;
    procedure SetStopEvaluation(const Value: boolean);
    function Match(const AText: string): boolean;
    // properties
    property Enabled: boolean read GetEnabled write SetEnabled;
    property Name: string read GetName write SetName;
    property System: boolean read GetSystem;
    property Includes: IStringList read GetIncludes;
    property Actions: string read GetActions write SetActions;
    property Order: integer read GetOrder write SetOrder;
    property StopEvaluation: boolean read GetStopEvaluation write SetStopEvaluation;
    property KeysIncluded: string read GetKeysIncluded write SetKeysIncluded;
    property KeysExcluded: string read GetKeysExcluded write SetKeysExcluded;
    property KeysIncludedEnabled: boolean read GetKeysIncludedEnabled write SetKeysIncludedEnabled;
    property KeysExcludedEnabled: boolean read GetKeysExcludedEnabled write SetKeysExcludedEnabled;
  end;

  AppTool = class
    class function CreateFilters: IFilters;
    class function ShowFilters(const AFilters: IFilters): Boolean;
    class procedure ReadConfig(const Filters: IFilters; const RootKey: INamedKey);
    class procedure SaveConfig(const Filters: IFilters; const RootKey: INamedKey);
  end;

implementation

uses
  Controls,
  UFilters, FFilters, SilLiKey;

{ AppTool }

class function AppTool.CreateFilters: IFilters;
begin
  result := TFilters.Create;
end;

class procedure AppTool.ReadConfig(const Filters: IFilters; const RootKey: INamedKey);
var
  Enum: IEnumerator;
  SubKey, GroupKey: INamedKey;
  GroupItem: IGroup;
  GroupName: string;
begin
  if not RootKey.Keys.Exists('filtros') then
    Exit;

  SubKey := RootKey.Keys.Get('filtros');

  with SubKey.Keys do
    while Enumerate(Enum, GroupName) do
    begin
      GroupKey := Get(GroupName);
      
      if (Filters.Groups.Count > 0) then
        GroupItem := Filters.Groups.GroupByName[GroupName] else
        GroupItem := nil;

      with GroupKey.Values do
      begin
        if not Assigned(GroupItem) then
        begin
          GroupItem := Filters.Groups.New(ReadBoolean('System', False, True));
          GroupItem.Name := GroupName;
        end;

        GroupItem.Enabled := ReadBoolean('Enabled', GroupItem.Enabled, True);
        GroupItem.Includes.AddStrings(ReadStrings('Includes', GroupItem.Includes, True));
        GroupItem.Actions := ReadString('Actions', GroupItem.Actions, True);
        GroupItem.Order := ReadInteger('Order', GroupItem.Order, True);
        GroupItem.StopEvaluation := ReadBoolean('StopEvaluation', GroupItem.StopEvaluation, True);
        GroupItem.KeysIncluded := ReadString('KeysIncluded', GroupItem.KeysIncluded, True);
        GroupItem.KeysExcluded := ReadString('KeysExcluded', GroupItem.KeysExcluded, True);
        GroupItem.KeysIncludedEnabled := ReadBoolean('KeysIncludedEnabled', GroupItem.KeysIncludedEnabled, True);
        GroupItem.KeysExcludedEnabled := ReadBoolean('KeysExcludedEnabled', GroupItem.KeysExcludedEnabled, True);
      end;

    end;

end;

class procedure AppTool.SaveConfig(const Filters: IFilters; const RootKey: INamedKey);
var
  Enum: IEnumerator;
  GroupItem: IGroup;
  GroupList: IStringList;
  SubKey, GroupKey: INamedKey;
  GroupName: string;
  GroupIndex: Integer;
begin
  SubKey := RootKey.Keys.Get('filtros', True);
  
  GroupList := Sil.List.StringList();
  with SubKey.Keys do
    while Enumerate(Enum, GroupName) do
      GroupList.Add(GroupName);

  with Filters.Groups do
    while Enumerate(Enum, GroupItem) do
    begin
      GroupKey := SubKey.Keys.Get(GroupItem.Name, True);
      with GroupKey.Values do
      begin
        WriteBoolean('Enabled', GroupItem.Enabled);
        WriteBoolean('System', GroupItem.System);
        WriteStrings('Includes', GroupItem.Includes);
        WriteString('Actions', GroupItem.Actions);
        WriteInteger('Order', GroupItem.Order);
        WriteBoolean('StopEvaluation', GroupItem.StopEvaluation);
        WriteString('KeysIncluded', GroupItem.KeysIncluded);
        WriteString('KeysExcluded', GroupItem.KeysExcluded);
        WriteBoolean('KeysIncludedEnabled', GroupItem.KeysIncludedEnabled);
        WriteBoolean('KeysExcludedEnabled', GroupItem.KeysExcludedEnabled);
      end;
      GroupIndex := GroupList.IndexOf(GroupItem.Name);
      if GroupIndex >= 0 then
        GroupList.Delete(GroupIndex);
    end;

  with GroupList do
    while Enumerate(Enum, GroupName) do
      SubKey.Keys.Remove(GroupName);
end;

class function AppTool.ShowFilters(const AFilters: IFilters): Boolean;
begin
  Result := TFormFilters.Execute( AFilters );
end;

end.

