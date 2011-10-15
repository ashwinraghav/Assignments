unit UiDefs;

interface

uses
  Sil;

type
  IFmTreeView = interface;
  IFmTreeNode = interface;
  

  IFmTreeView = interface
    ['{8598096E-CF6D-464B-AB79-7548FAC000BA}']
  end;

  IFmTreeNode = interface
    ['{0ECC8136-63E8-4854-981B-9091765CE7BE}']
    function GetText: string;
    function GetInfo: IFileInfo;
    property Text: string read GetText;
    property Info: IFileInfo read GetInfo;
  end;

  IFmFileNode = interface
    ['{E4C08160-6D6A-4D12-8B13-9EE490DAD965}']
    function GetInfo: IFileInfo;
    property Info: IFileInfo read GetInfo;
  end;

  IFmTreeOnSelected = interface
    ['{EC211B11-AE4F-4CE4-BC24-13B72419693C}']
    procedure OnTreeSelected(const Sender: IFmTreeView; const Item: IFmTreeNode);
  end;

implementation

end.
