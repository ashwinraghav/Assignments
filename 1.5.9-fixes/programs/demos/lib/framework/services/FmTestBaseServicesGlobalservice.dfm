object FormTestGlobalservices: TFormTestGlobalservices
  Left = 14
  Top = 103
  Width = 927
  Height = 620
  Caption = 
    'Tester that demonstrates the functionality of the Global Service' +
    ' List (see code for details)'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter3: TSplitter
    Left = 288
    Top = 0
    Height = 593
  end
  object Panel1: TPanel
    Left = 291
    Top = 0
    Width = 628
    Height = 593
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 0
      Top = 405
      Width = 628
      Height = 3
      Cursor = crVSplit
      Align = alTop
    end
    object Splitter2: TSplitter
      Left = 0
      Top = 242
      Width = 628
      Height = 3
      Cursor = crVSplit
      Align = alTop
    end
    object pnButtons: TPanel
      Left = 0
      Top = 0
      Width = 628
      Height = 73
      Align = alTop
      BevelOuter = bvSpace
      TabOrder = 0
      object RegisterV1: TButton
        Left = 8
        Top = 8
        Width = 101
        Height = 25
        Hint = 
          'Registers a Demo Service using the old GlobalService API which i' +
          's called V1'
        Caption = 'RegisterV1'
        TabOrder = 0
        OnClick = RegisterV1Click
      end
      object RegisterV2: TButton
        Left = 8
        Top = 32
        Width = 101
        Height = 25
        Hint = 
          'Registers a Demo Service using the new GlobalService API which i' +
          's called V2'
        Caption = 'RegisterV2'
        TabOrder = 1
        OnClick = RegisterV2Click
      end
      object UnregisterV1: TButton
        Left = 110
        Top = 8
        Width = 101
        Height = 25
        Hint = 
          'Unregisters the Demo Service using the old GlobalService API whi' +
          'ch is called V1'
        Caption = 'UnregisterV1'
        TabOrder = 2
        OnClick = UnregisterV1Click
      end
      object UnregisterV2: TButton
        Left = 110
        Top = 32
        Width = 101
        Height = 25
        Hint = 
          'Unregisters the Demo Service using the new GlobalService API whi' +
          'ch is called V2'
        Caption = 'UnregisterV2'
        TabOrder = 3
        OnClick = UnregisterV2Click
      end
      object GetV1: TButton
        Left = 212
        Top = 8
        Width = 101
        Height = 25
        Hint = 
          'Requests an instance of the Demo Service using the old GlobalSer' +
          'vice API which is called V1'
        Caption = 'GetV1'
        TabOrder = 4
        OnClick = GetV1Click
      end
      object GetV2: TButton
        Left = 212
        Top = 32
        Width = 101
        Height = 25
        Hint = 
          'Requests an instance of the Demo Service using the new GlobalSer' +
          'vice API which is called V2'
        Caption = 'GetV2'
        TabOrder = 5
        OnClick = GetV2Click
      end
      object ReleaseV1: TButton
        Left = 314
        Top = 8
        Width = 101
        Height = 25
        Hint = 
          'Releases the instance of the Demo Service using the old GlobalSe' +
          'rvice API which is called V1'
        Caption = 'ReleaseV1'
        TabOrder = 6
        OnClick = ReleaseV1Click
      end
      object ReleaseV2: TButton
        Left = 314
        Top = 32
        Width = 101
        Height = 25
        Hint = 
          'Releases the instance of the Demo Service using the new GlobalSe' +
          'rvice API which is called V2'
        Caption = 'ReleaseV2'
        TabOrder = 7
        OnClick = ReleaseV2Click
      end
      object EnumV1: TButton
        Left = 416
        Top = 8
        Width = 101
        Height = 25
        Hint = 
          'Enumerates the global services using the old GlobalService API w' +
          'hich is called V1'
        Caption = 'EnumV1'
        TabOrder = 8
        OnClick = EnumV1Click
      end
      object EnumV2: TButton
        Left = 416
        Top = 32
        Width = 101
        Height = 25
        Hint = 
          'Enumerates the global services using the new GlobalService API w' +
          'hich is called V2'
        Caption = 'EnumV2'
        TabOrder = 9
        OnClick = EnumV2Click
      end
      object RefsV1: TButton
        Left = 518
        Top = 8
        Width = 101
        Height = 25
        Hint = 
          'Enumerates the references to the global services using the old G' +
          'lobalService API which is called V1'
        Caption = 'RefsV1'
        TabOrder = 10
      end
      object RefsV2: TButton
        Left = 518
        Top = 32
        Width = 101
        Height = 25
        Hint = 
          'Enumerates the references to the global services using the new G' +
          'lobalService API which is called V2'
        Caption = 'RefsV2'
        TabOrder = 11
      end
    end
    object pnModule: TPanel
      Left = 0
      Top = 73
      Width = 628
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Spawn: TButton
        Left = 8
        Top = 8
        Width = 75
        Height = 25
        Hint = 
          'Spawns a thread in the selected module ... this is done by calli' +
          'ng an exported function, so the thread creation is done internal' +
          'ly to the module selected thus showing the global services mecha' +
          'nism in action.'
        Caption = 'Spawn'
        TabOrder = 0
        OnClick = SpawnClick
      end
      object Stop: TButton
        Left = 90
        Top = 8
        Width = 75
        Height = 25
        Hint = 'Terminates the thread started before.'
        Caption = 'Stop'
        TabOrder = 1
        OnClick = StopClick
      end
    end
    object Panel5: TPanel
      Left = 0
      Top = 114
      Width = 628
      Height = 128
      Align = alTop
      BevelInner = bvLowered
      BevelOuter = bvNone
      BorderWidth = 3
      Caption = 'Panel5'
      TabOrder = 2
      object lvGlobalServices: TListView
        Left = 4
        Top = 23
        Width = 620
        Height = 101
        Hint = 
          'This is the list of global services registered with the applicat' +
          'ion.'
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Columns = <
          item
            AutoSize = True
            Caption = 'Name'
          end
          item
            AutoSize = True
            Caption = 'GUID'
          end
          item
            AutoSize = True
            Caption = 'Module'
          end
          item
            AutoSize = True
            Caption = 'Scope'
          end
          item
            AutoSize = True
            Caption = 'Created?'
          end
          item
            AutoSize = True
            Caption = 'Refs'
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnDeletion = lvServicesDeletion
        OnSelectItem = lvServicesSelectItem
      end
      object Panel6: TPanel
        Left = 4
        Top = 4
        Width = 620
        Height = 19
        Align = alTop
        BevelInner = bvRaised
        BevelOuter = bvNone
        Caption = 'Registered Application Global Services'
        TabOrder = 1
      end
    end
    object Panel7: TPanel
      Left = 0
      Top = 245
      Width = 628
      Height = 160
      Align = alTop
      BevelInner = bvLowered
      BevelOuter = bvNone
      BorderWidth = 3
      Caption = 'Panel7'
      TabOrder = 3
      object lvLocalServices: TListView
        Left = 4
        Top = 22
        Width = 620
        Height = 134
        Hint = 
          'These are the Local services provided by the module selected in ' +
          'the left most list.'
        Align = alClient
        BorderStyle = bsNone
        Columns = <
          item
            AutoSize = True
            Caption = 'Name'
          end
          item
            AutoSize = True
            Caption = 'GUID'
          end
          item
            AutoSize = True
            Caption = 'Module'
          end
          item
            AutoSize = True
            Caption = 'Scope'
          end
          item
            AutoSize = True
            Caption = 'Created?'
          end
          item
            AutoSize = True
            Caption = 'Refs'
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnDeletion = lvServicesDeletion
        OnSelectItem = lvServicesSelectItem
      end
      object Panel8: TPanel
        Left = 4
        Top = 4
        Width = 620
        Height = 18
        Align = alTop
        Caption = 'Selected Module Local Services'
        TabOrder = 1
      end
    end
    object Panel9: TPanel
      Left = 0
      Top = 408
      Width = 628
      Height = 185
      Align = alClient
      BevelInner = bvLowered
      BevelOuter = bvNone
      BorderWidth = 3
      Caption = 'Panel9'
      TabOrder = 4
      object lvRefs: TListView
        Left = 4
        Top = 22
        Width = 620
        Height = 159
        Hint = 
          'This is the list of references to a services wether it is select' +
          'ed in the global service or in the local service list.'
        Align = alClient
        BorderStyle = bsNone
        Columns = <
          item
            AutoSize = True
            Caption = 'Ptr'
          end
          item
            AutoSize = True
            Caption = 'Module'
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
      object Panel10: TPanel
        Left = 4
        Top = 4
        Width = 620
        Height = 18
        Align = alTop
        Caption = 'References to selected Global or Local Service'
        TabOrder = 1
      end
    end
  end
  object pnModules: TPanel
    Left = 0
    Top = 0
    Width = 288
    Height = 593
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 288
      Height = 73
      Align = alTop
      BevelOuter = bvSpace
      TabOrder = 0
      object Label1: TLabel
        Left = 88
        Top = 2
        Width = 78
        Height = 13
        Caption = 'DLL file &name:'
        FocusControl = edDll
      end
      object LoadDll: TButton
        Left = 8
        Top = 18
        Width = 75
        Height = 25
        Hint = 
          'This loads the DLL indicated by the Edit box. After registering,' +
          ' the DLL will appear in the list below and when selected the Loc' +
          'al Services list will be refreshed to show the services register' +
          'ed within the DLL.'
        Caption = '&Load DLL'
        TabOrder = 0
        OnClick = LoadDllClick
      end
      object UnloadDll: TButton
        Left = 8
        Top = 42
        Width = 75
        Height = 25
        Hint = 'This will unload the selected DLL'
        Caption = '&Unload DLL'
        TabOrder = 1
        OnClick = UnloadDllClick
      end
      object edDll: TEdit
        Left = 86
        Top = 17
        Width = 192
        Height = 21
        TabOrder = 2
        Text = 'TestDLL'
      end
    end
    object Panel3: TPanel
      Left = 0
      Top = 73
      Width = 288
      Height = 520
      Align = alClient
      BevelOuter = bvLowered
      Caption = 'Panel3'
      TabOrder = 1
      object lvDlls: TListView
        Left = 1
        Top = 22
        Width = 286
        Height = 497
        Hint = 
          'This is the list of loaded shared modules. Initially it shows on' +
          'ly the main EXE module.'
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Columns = <
          item
            Caption = 'Base'
          end
          item
            AutoSize = True
            Caption = 'Name'
          end
          item
            AutoSize = True
            Caption = 'Path'
          end
          item
            Caption = 'S/D'
            Width = 26
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnDeletion = lvDllsDeletion
        OnSelectItem = lvDllsSelectItem
      end
      object Panel4: TPanel
        Left = 1
        Top = 1
        Width = 286
        Height = 21
        Align = alTop
        Caption = 'Currently loaded Modules'
        TabOrder = 1
      end
    end
  end
end
