unit CryptoVirtualTreeUnit;

interface

uses
  System.Classes, virtualtrees;

type
  TCryptoVirtualTree = class(TCustomVirtualStringTree)

  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TCryptoVirtualTree }

constructor TCryptoVirtualTree.Create(AOwner: TComponent);
var
  aNode: PVirtualNode;
begin
  inherited;

  aNode := self.AddChild(nil);

  AddChild(aNode);
  AddChild(aNode);
  AddChild(aNode);
  AddChild(aNode);

end;

end.
