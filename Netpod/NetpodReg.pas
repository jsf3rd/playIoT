unit NetpodReg;

interface

uses
  Netpod;

procedure Register;

implementation

uses
  Classes;

procedure Register;
begin
 RegisterComponents('ATMACS', [TNetpod]);
end;

end.
