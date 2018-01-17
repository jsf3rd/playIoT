// *******************************************************
//
// playIoT Coinone API Library
//
// Copyright(c) 2017 playIoT.
//
// Writer : jsf3rd@playiot.biz
//
// API Doc : http://doc.coinone.co.kr/#api-_
//
//
// *******************************************************

unit Coinone;

interface

uses System.SysUtils, System.Variants, System.Classes, Windows,
  EncdDecd, IdGlobal, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP,
  REST.JSON, System.JSON, IdCoder, IdCoderMIME, IdHash, IdHashSHA, IdSSLOpenSSL,
  System.DateUtils, IdHMACSHA1, IdHMAC, System.NetEncoding, IdURI, JdcGlobal.ClassHelper;

type
  TRequestType = ( //
    // Account
    rtBalance, rtDailyBalance, rtDepositAddress, rtUserInformation, rtVirtualAccount,
    // Order
    rtCancelOrder, rtLimitBuy, rtLimitSell, rtMyCompleteOrders, rtMyLimitOrders,
    rtMyOrderInformation,
    // Public
    rtOrderbook, rtRecentCompleteOrders, rtTicker);

  TCoinone = class
  private
    FToken: string;
    FHashKey: string;
    function GetURL(AType: TRequestType): string;
    function Get(AType: TRequestType; AParam: string): TJSONObject;
    function Post(AType: TRequestType; AParams: TJSONObject): TJSONObject;

    function CreateHttp: TIdHTTP;
  public
    constructor Create(AToken, AKey: string);
    destructor Destroy; override;

    function AccountInfo(AType: TRequestType): TJSONObject;
    function Order(AType: TRequestType; AParams: TJSONObject): TJSONObject;
    function PublicInfo(AType: TRequestType; AParam: string): TJSONObject;

    class function RequestName(AType: TRequestType): string; overload;
    class function RequestName(AType: Integer): string; overload;
    class function MinOrderCount(ACurrency: string): double;
  end;

  TOrder = record
    timestamp: string;
    price: string;
    order_type: string;
    qty: string;
    feeRate: string;
    fee: string;
    orderId: string;
    function GetValue: Integer;

    function WasSold: boolean;
    function WasBought: boolean;

    function ToString: string;
  end;

const
  // Account
  URL_BALANCE = 'https://api.coinone.co.kr/v2/account/balance/';
  URL_DAILY_BALANCE = 'https://api.coinone.co.kr/v2/account/daily_balance/';
  URL_DEPOSIT_ADDRESS = 'https://api.coinone.co.kr/v2/account/deposit_address/';
  URL_USER_INFORMATION = 'https://api.coinone.co.kr/v2/account/user_info/';
  URL_VIRTUAL_ACCOUNT = 'https://api.coinone.co.kr/v2/account/virtual_account/';

  // Order
  URL_CANCEL_ORDER = 'https://api.coinone.co.kr/v2/order/cancel/';
  URL_LIMIT_BUY = 'https://api.coinone.co.kr/v2/order/limit_buy/';
  URL_LIMIT_SELL = 'https://api.coinone.co.kr/v2/order/limit_sell/';
  URL_MY_COMPLETE_ORDERS = 'https://api.coinone.co.kr/v2/order/complete_orders/';
  URL_MY_LIMIT_ORDERS = 'https://api.coinone.co.kr/v2/order/limit_orders/';
  URL_MY_ORDER_INFORMATION = 'https://api.coinone.co.kr/v2/order/order_info/';

  // Public
  URL_ORDER_BOOK = 'https://api.coinone.co.kr/orderbook/';
  URL_RECENT_COMPLETE_ORDERS = 'https://api.coinone.co.kr/trades/';
  URL_TICKER = 'https://api.coinone.co.kr/ticker/';

  Coins: array [0 .. 9] of string = ('btc', 'bch', 'eth', 'etc', 'xrp', 'qtum', 'iota', 'ltc',
    'btg', 'krw');

  MinCount: array [0 .. 9] of double = (0.0001, 0.001, 0.01, 0.01, 1, 0.01, 0.1, 0.1, 0.01, 0);

  RES_SUCCESS = 'success';

implementation

const
  RequestType: array [0 .. 13] of string = ('Balance', 'DailyBalance', 'DepositAddress',
    'UserInformation', 'VirtualAccount', 'CancelOrder', 'LimitBuy', 'LimitSell',
    'MyCompleteOrders', 'MyLimitOrders', 'MyOrderInformation', 'Orderbook',
    'RecentCompleteOrders', 'Ticker');

  { TCoinone }

function TCoinone.AccountInfo(AType: TRequestType): TJSONObject;
var
  msg: string;
  AParams: TJSONObject;
begin
  result := nil;

  msg := Format('Cmd=%s,Type=%s,', ['AccountInfo', RequestName(AType)]);
  AParams := TJSONObject.Create;
  try
    try
      result := Post(AType, AParams);
    except
      on E: Exception do
        raise Exception.Create(msg + 'E=' + E.Message);
    end;

    if result.GetString('result') <> RES_SUCCESS then
      raise Exception.Create(msg + 'res=' + result.ToString);
  finally
    AParams.Free;
  end;
end;

constructor TCoinone.Create(AToken, AKey: string);
begin
  if not LoadOpenSSLLibrary then
    raise Exception.Create('LoadOpenSSLLibrary Error');

  FToken := AToken;
  FHashKey := AKey;
end;

function TCoinone.CreateHttp: TIdHTTP;
var
  SSL: TIdSSLIOHandlerSocketOpenSSL;
begin
  result := TIdHTTP.Create(nil);
  SSL := TIdSSLIOHandlerSocketOpenSSL.Create(result);
  result.IOHandler := SSL;
  result.HandleRedirects := True;
  result.ConnectTimeout := 10000;
  result.ReadTimeout := 10000;
end;

destructor TCoinone.Destroy;
begin
  inherited;
end;

function TCoinone.Get(AType: TRequestType; AParam: string): TJSONObject;
var
  _IdHttp: TIdHTTP;
  Response: TBytesStream;
  Res: string;
begin
  _IdHttp := CreateHttp;
  try
    _IdHttp.Request.Clear;
    _IdHttp.Request.Accept := 'application/json';
    _IdHttp.Request.ContentType := 'application/json';

    Response := TBytesStream.Create;
    try
      _IdHttp.Get(GetURL(AType) + '?' + AParam, Response);
      Response.SetSize(Response.Size);
      Res := TEncoding.UTF8.GetString(Response.Bytes);
      result := TJSONObject.ParseJSONValue(Res) as TJSONObject;
    finally
      Response.Free;;
    end;
  finally
    _IdHttp.IOHandler.Free;
    _IdHttp.Free;
  end;
end;

function TCoinone.GetURL(AType: TRequestType): string;
begin
  case AType of
    rtBalance:
      result := URL_BALANCE;
    rtDailyBalance:
      result := URL_DAILY_BALANCE;
    rtDepositAddress:
      result := URL_DEPOSIT_ADDRESS;
    rtUserInformation:
      result := URL_USER_INFORMATION;
    rtVirtualAccount:
      result := URL_VIRTUAL_ACCOUNT;
    rtCancelOrder:
      result := URL_CANCEL_ORDER;
    rtLimitBuy:
      result := URL_LIMIT_BUY;
    rtLimitSell:
      result := URL_LIMIT_SELL;
    rtMyCompleteOrders:
      result := URL_MY_COMPLETE_ORDERS;
    rtMyLimitOrders:
      result := URL_MY_LIMIT_ORDERS;
    rtMyOrderInformation:
      result := URL_MY_ORDER_INFORMATION;
    rtOrderbook:
      result := URL_ORDER_BOOK;
    rtRecentCompleteOrders:
      result := URL_RECENT_COMPLETE_ORDERS;
    rtTicker:
      result := URL_TICKER;

  else
    raise Exception.Create('Unknown RequestType,' + Integer(AType).ToString);
  end;
end;

class function TCoinone.MinOrderCount(ACurrency: string): double;
var
  I: Integer;
begin
  result := 0;
  for I := Low(Coins) to High(Coins) do
  begin
    if Coins[I] = ACurrency then
    begin
      result := MinCount[I];
      Exit;
    end;
  end;
end;

function TCoinone.Order(AType: TRequestType; AParams: TJSONObject): TJSONObject;
var
  msg: string;
begin
  msg := Format('Cmd=%s,Type=%s,Params=%s,', ['Order', RequestName(AType), AParams.ToString]);
  try
    result := Post(AType, AParams);
  except
    on E: Exception do
      raise Exception.Create(msg + 'E=' + E.Message);
  end;

  if result.GetString('result') <> RES_SUCCESS then
    raise Exception.Create(msg + 'res=' + result.ToString);
end;

function TCoinone.Post(AType: TRequestType; AParams: TJSONObject): TJSONObject;

// Build base64 encoded payLoad
  function GetPayLoad: string;
  var
    Bytes: TIdBytes;
    Encoded: string;
  begin
    AParams.AddPair('access_token', FToken);
    AParams.AddPair('nonce', GetTickCount().ToString);
    Bytes := ToBytes(AParams.ToString);
    Encoded := WideString(EncodeBase64(Bytes, Length(Bytes)));
    result := Encoded.Replace(sLineBreak, '', [rfReplaceAll]);
  end;

  function SHA512Hash(AVAlue: string): string;
  var
    Hasher: TIdHMACSHA512;
    Bytes: TIdBytes;
  begin
    Hasher := TIdHMACSHA512.Create;
    try
      Hasher.Key := ToBytes(FHashKey);
      Bytes := ToBytes(AVAlue);
      result := LowerCase(ToHex(Hasher.HashValue(Bytes)));
    finally
      Hasher.Free;
    end;
  end;

var
  Signature: String;
  Response: TBytesStream;
  FPost: TStrings;

  Res: string;
  PayLoad: string;
  _IdHttp: TIdHTTP;
begin
  PayLoad := GetPayLoad;

  try
    Signature := SHA512Hash(PayLoad);
  except
    on E: Exception do
      raise Exception.Create('SHA512 Error,' + E.Message);
  end;

  _IdHttp := CreateHttp;
  try
    _IdHttp.Request.Clear;
    _IdHttp.Request.Accept := 'application/json';
    _IdHttp.Request.ContentType := 'application/json';
    _IdHttp.Request.CustomHeaders.AddValue('X-COINONE-PAYLOAD', PayLoad);
    _IdHttp.Request.CustomHeaders.AddValue('X-COINONE-SIGNATURE', Signature);

    FPost := TStringList.Create;
    try
      Response := TBytesStream.Create;
      try
        _IdHttp.Post(GetURL(AType), FPost, Response);
        Response.SetSize(Response.Size);
        Res := TEncoding.UTF8.GetString(Response.Bytes);
        result := TJSONObject.ParseJSONValue(Res) as TJSONObject;
      finally
        Response.Free;;
      end;
    finally
      FPost.Free;
    end;
  finally
    _IdHttp.IOHandler.Free;
    _IdHttp.Free;
  end;
end;

function TCoinone.PublicInfo(AType: TRequestType; AParam: string): TJSONObject;
var
  msg: string;
begin
  msg := Format('Cmd=%s,Type=%s,Param=%s,', ['PublicInfo', RequestName(AType), AParam]);
  try
    result := Get(AType, AParam);
  except
    on E: Exception do
      raise Exception.Create(msg + 'E=' + E.Message);
  end;

  if result.GetString('result') <> RES_SUCCESS then
    raise Exception.Create(msg + 'res=' + result.ToString);
end;

class function TCoinone.RequestName(AType: Integer): string;
begin
  result := RequestType[AType];
end;

class function TCoinone.RequestName(AType: TRequestType): string;
begin
  result := RequestType[Integer(AType)];
end;

{ TOrder }

function TOrder.GetValue: Integer;
begin
  result := round(Self.price.ToInteger * Self.qty.ToDouble);
end;

function TOrder.ToString: string;
begin
  result := 'Price=' + Self.price + ',Count=' + Self.qty + ',Type=' + Self.order_type;
end;

function TOrder.WasBought: boolean;
begin
  result := order_type = 'bid';
end;

function TOrder.WasSold: boolean;
begin
  result := order_type = 'ask';
end;

end.
