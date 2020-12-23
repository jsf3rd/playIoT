unit SmartCombo;

{
  -Origin Source
  https://stackoverflow.com/questions/9466547/how-to-make-a-combo-box-with-fulltext-search-autocomplete-support

  -Reference
  http://blog.devquest.co.kr/imp/219

  -Edit
  jsf3rd@nate.com
  @Prevents a bug - typing values are appended when selecting a list while typing in Unicode
  @Filtering is applied to each Unicode being typed if it is being entered after the end of the text.
  @Exception handling in case of entering Unicode after selecting several texts
  @Exception handling in case of additional input of Unicode when there is already a character in edit and the listbox is closed
}

interface

uses StdCtrls, Classes, Messages, Controls, Windows, SysUtils, StrUtils;

type
  TSmartComboBox = class(TComboBox)
    // Usage:
    // Same as TComboBox, just invoke InitSmartCombo after Items list is filled with data.
    // After InitSmartCombo is invoked, StoredItems is assigned and combo starts to behave as a smart combo.
    // If InitSmartCombo is not invoked it acts as standard TComboBox, it is safe to bulk replace all TComboBox in application with TSmartComboBox
  private
    FChar: Char; // @for UNICODE Filter
    FIgnoreChar: boolean; // @for UNICODE Edit
    FStoredItems: TStringList;
    doFilter: boolean;
    StoredItemIndex: Integer;

    procedure StoredItemsChange(Sender: TObject);
    procedure SetStoredItems(const Value: TStringList);
    procedure CNCommand(var AMessage: TWMCommand); message CN_COMMAND;
    function GetXText(var Key: Char): string;
    function GetXSelStart: Integer;
  protected
    procedure KeyPress(var Key: Char); override;

    // @Prevents a bug - typing values are appended when selecting a list while typing in Unicode
    procedure EditWndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure FilterItems;
    procedure InitSmartCombo;

    property StoredItems: TStringList read FStoredItems write SetStoredItems;
  end;

implementation

function TSmartComboBox.GetXText(var Key: Char): string;
var
  tmp: string;
begin
  if (Text = '') then // @empty edit box
    result := ''
  else if SelLength > 0 then // @has selection
  begin
    tmp := Copy(Text, SelStart + 1, SelLength);
    result := ReplaceStr(Text, tmp, '');
  end
  else // @not empty edit box and no selection
  begin
    tmp := Copy(Text, 1, SelStart);
    result := tmp + Key;
    result := result + Copy(Text, SelStart + 1, Length(Text) - SelStart);
    Key := #0;
  end;
end;

function TSmartComboBox.GetXSelStart: Integer;
begin
  // @empty edit box or has selection
  if (Text = '') or (SelLength > 0) then
    result := SelStart
  else // @not empty edit box and no selection
    result := SelStart + 1;
end;

procedure TSmartComboBox.KeyPress(var Key: Char);
// combo dropdown must be done in keypress, if its done on CBN_EDITUPDATE it messes up whole message processing mumbo-jumbo
var
  xSelStart: Integer;
  xText: string;
begin
  inherited;

  if Ord(Key) = 8 then
    FChar := Key;

  if doFilter and not(Ord(Key) in [8, 13, 27]) then // BackSpace, Enter, ESC
  begin
    FChar := Key;

    if DroppedDown then
      Exit;

    if Items.Count = 0 then
      Exit;

    // backup
    xSelStart := GetXSelStart;
    xText := GetXText(Key);

    // dropdown
    SendMessage(Handle, CB_SHOWDROPDOWN, 1, 0);

    if xText.IsEmpty then
      Exit;

    // restore
    Text := xText;
    SelStart := xSelStart;
  end;
end;

procedure TSmartComboBox.InitSmartCombo;
begin
  FStoredItems.OnChange := nil;
  StoredItems.Assign(Items);
  AutoComplete := False;
  FStoredItems.OnChange := StoredItemsChange;
  doFilter := True;
  StoredItemIndex := -1;
end;

constructor TSmartComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FStoredItems := TStringList.Create;
  FIgnoreChar := False;
  doFilter := False;
end;

destructor TSmartComboBox.Destroy;
begin
  FStoredItems.Free;
  inherited;
end;

procedure TSmartComboBox.EditWndProc(var Message: TMessage);
var
  OldText: string;
begin
  case Message.Msg of
    WM_IME_ENDCOMPOSITION:
      begin
        OldText := Self.Text;
        inherited;
        FIgnoreChar := Self.Text = OldText;
      end;
    WM_CHAR:
      begin
        FIgnoreChar := False;
        inherited;
      end;
    WM_IME_CHAR:
      begin
        if FIgnoreChar then
          FIgnoreChar := False
        else
          inherited;
      end;
  else
    inherited;
  end;
end;

procedure TSmartComboBox.CNCommand(var AMessage: TWMCommand);
begin
  // we have to process everything from our ancestor
  inherited;

  // @Filtering is applied to each Unicode being typed if it is being entered after the end of the text.
  // @If you are typing in the middle of the text, do not apply filtering to the Unicode being typed
  // (filtering is applied in units of completed Unicode characters)
  if (SelStart < Length(Text)) and (FChar = #0) then
    Exit;

  // if we received the CBN_EDITUPDATE notification
  if (AMessage.NotifyCode = CBN_EDITUPDATE) and doFilter then
  begin
    // fill the items with the matches
    FilterItems;
  end;

  FChar := #0;
end;

procedure TSmartComboBox.FilterItems;
var
  I: Integer;
  Selection: TSelection;
begin
  // store the current combo edit selection
  SendMessage(Handle, CB_GETEDITSEL, WPARAM(@Selection.StartPos), LPARAM(@Selection.EndPos));

  // begin with the items update
  Items.BeginUpdate;
  try
    // if the combo edit is not empty, then clear the items
    // and search through the FStoredItems
    if Text <> '' then
    begin
      // clear all items
      Items.Clear;
      // iterate through all of them
      for I := 0 to FStoredItems.Count - 1 do
      begin
        // check if the current one contains the text in edit, case insensitive
        if ContainsText(FStoredItems[I], Text) then
        begin
          // and if so, then add it to the items
          Items.Add(FStoredItems[I]);
        end;
      end;
    end
    else
    begin
      // else the combo edit is empty
      // so then we'll use all what we have in the FStoredItems
      Items.Assign(FStoredItems);
    end;
  finally
    // finish the items update
    Items.EndUpdate;
  end;

  // and restore the last combo edit selection
  SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(Selection.StartPos, Selection.EndPos));
end;

procedure TSmartComboBox.StoredItemsChange(Sender: TObject);
begin
  if Assigned(FStoredItems) then
    FilterItems;
end;

procedure TSmartComboBox.SetStoredItems(const Value: TStringList);
begin
  if Assigned(FStoredItems) then
    FStoredItems.Assign(Value)
  else
    FStoredItems := Value;
end;

end.
