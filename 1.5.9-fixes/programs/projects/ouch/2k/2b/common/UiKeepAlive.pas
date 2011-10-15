unit UiKeepAlive;

interface

type
  IKeepAlive = interface
    ['{1D748E50-0617-439D-B6C3-8857D084E9F9}']
    procedure Start;
    procedure Stop;
  end;

  IKeepAliveEvents = interface
    ['{F80BA96A-BBF1-43DF-AF38-1CBF17006DEF}']
    procedure OnKeepAlive(const Sender: IKeepAlive; out Success: Boolean);
  end;

implementation

end.
 