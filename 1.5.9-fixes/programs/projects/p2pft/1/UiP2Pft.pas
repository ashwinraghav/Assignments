unit UiP2Pft;

interface

type
  IP2Pft = interface
    ['{3B7D7374-DA50-4514-9B92-E93AC8CF2D74}']
    procedure 
  end;

  RTextEvent = record
    Sender: IP2Pft;
    Buffer: String;
  end;

  IP2PftHook = interface
    ['{BC65AFD7-81CC-4AE1-8E9E-D7767F578BE9}']
  end;

implementation

end.
 