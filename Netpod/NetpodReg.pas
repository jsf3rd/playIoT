unit NetpodReg;

interface

uses
  _Netpod;

procedure Register;

implementation

uses
  Classes;

procedure Register;
begin
  RegisterComponents('ATMACS', [TNetpod]);
end;

end.
