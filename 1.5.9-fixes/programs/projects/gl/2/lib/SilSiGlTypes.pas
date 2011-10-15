unit SilSiGlTypes;

interface

uses
  Sil,
  SilSeGlTypes;

type
  IGlData = interface
    ['{2F2B6EB0-F6FD-4F7B-9760-36F950AFB03F}']
    function GetValue: Pointer;
    property Value: Pointer read GetValue;
  end;

  IGl3dPosition = interface
    ['{72E7DB89-B59C-44B0-8050-6EA6B151A798}']
    function GetValue: P3dPoint;
    property Value: P3dPoint read GetValue;
  end;

  IGl3dDirection = interface
    ['{9463CB76-A10B-4BC3-A1AE-DE08696A3D6B}']
    function GetValue: P3dVector;
    property Value: P3dVector read GetValue;
  end;

  IGl2iRect = interface
    ['{DB2A8C22-AD6A-4506-9011-8EA73258D423}']
    function GetValue: P2iRect;
    property Value: P2iRect read GetValue;
  end;

implementation
end.
 