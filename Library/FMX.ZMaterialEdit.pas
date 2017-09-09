unit FMX.ZMaterialEdit;

{
  author: Ravil (ZuBy) Zaripov
  email: rzaripov1990@gmail.com
  http://github.com/rzaripov1990

  2017
}

interface

uses
  System.SysUtils, System.Classes, System.UITypes, System.Types,
  FMX.Types, FMX.Controls, FMX.Layouts, FMX.Edit, FMX.StdCtrls, FMX.Graphics,
  FMX.ZMaterialTypes, FMX.Objects, FMX.TextLayout;

type

  [ComponentPlatformsAttribute(cpaAllPlatforms)]
  TZMaterialEdit = class(TLayout)
  private const
    Xmargin = 2;
    Ymargin = 1;

  private
    FEdit: TEdit;
    FLabel: TLabel;

    FAnimation: Boolean;

    FPromptUpTextSettings: TTextSettings;
    FPromptUpStylledSettings: TStyledSettings;

    FPromptDownTextSettings: TTextSettings;
    FPromptDownStylledSettings: TStyledSettings;

    FTextOnChangeEvent: TNotifyEvent;
    FTextOnTypingEvent: TNotifyEvent;

    procedure DoMyEnter;
    procedure DoMyExit;
    procedure DoResize;

    procedure OnEnterEvent(Sender: TObject);
    procedure OnExitEvent(Sender: TObject);
    procedure OnChangeEvent(Sender: TObject);
    procedure OnTypingEvent(Sender: TObject);
    procedure OnKeyUpEvent(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure OnKeyDownEvent(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);

    procedure OnApplyStyleLookupPrompt(Sender: TObject);
    procedure OnApplyStyleLookupText(Sender: TObject);

    function GetAnimation: Boolean;
    procedure SetAnimation(const Value: Boolean);

    { Edit }
    function GetText: string;
    procedure SetText(const Value: string);

    function GetTextSettings: TTextSettings;
    procedure SetTextSettings(const Value: TTextSettings);

    function GetTextStyledSettings: TStyledSettings;
    procedure SetTextStyledSettings(const Value: TStyledSettings);

    procedure SetKeyboardType(Value: TVirtualKeyboardType);
    function GetKeyboardType: TVirtualKeyboardType;

    procedure SetReturnKeyType(Value: TReturnKeyType);
    function GetReturnKeyType: TReturnKeyType;

    procedure SetMaxLength(const Value: Integer);
    function GetMaxLength: Integer;

    procedure SetPassword(const Value: Boolean);
    function GetPassword: Boolean;

    procedure SetReadOnly(const Value: Boolean);
    function GetReadOnly: Boolean;

    procedure SetKillFocusByReturn(const Value: Boolean);
    function GetKillFocusByReturn: Boolean;

    function GetCheckSpelling: Boolean;
    procedure SetCheckSpelling(const Value: Boolean);

    procedure SetControlType(const Value: TControlType);
    function GetControlType: TControlType;

    function GetImeMode: TImeMode;
    procedure SetImeMode(const Value: TImeMode);

    function GetFilterChar: string;
    procedure SetFilterChar(const Value: string);

    function GetDisableFocusEffect: Boolean;
    procedure SetDisableFocusEffect(const Value: Boolean);

    { Label }
    function GetPrompt: string;
    procedure SetPrompt(const Value: string);

    function GetPromptSettings: TTextSettings;
    procedure SetPromptSettings(const Value: TTextSettings);

    function GetPromptStyledSettings: TStyledSettings;
    procedure SetPromptStyledSettings(const Value: TStyledSettings);

    function GetAutoTranslatePrompt: Boolean;
    procedure SetAutoTranslatePrompt(const Value: Boolean);

    function GetPromptUpTextSettings: TTextSettings;
    procedure SetPromptUpTextSettings(const Value: TTextSettings);

    function GetTextSettingsClass: TTextSettingsClass;
  protected
    { Protected declarations }
    procedure Resize; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFocus; overload;

    function GetEdit: TEdit;
    function GetLabel: TLabel;
  published
    { Published declarations }
    property Text: string read GetText write SetText;
    property Prompt: string read GetPrompt write SetPrompt;

    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
    property StyledSettings: TStyledSettings read GetTextStyledSettings write SetTextStyledSettings;

    property TextSettingsPrompt: TTextSettings read GetPromptSettings write SetPromptSettings;
    property StyledSettingsPrompt: TStyledSettings read GetPromptStyledSettings write SetPromptStyledSettings;

    property TextSettingsPromptUp: TTextSettings read GetPromptUpTextSettings write SetPromptUpTextSettings;
    property StyledSettingsPromptUp: TStyledSettings read FPromptUpStylledSettings write FPromptUpStylledSettings;

    property Animation: Boolean read GetAnimation write SetAnimation default True;

    { Events }
    property OnApplyStyleLookup;

    property OnClick;

    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

    property OnEnter;
    property OnExit;

    property OnKeyDown;
    property OnKeyUp;

    property OnResize;

    property OnChange: TNotifyEvent read FTextOnChangeEvent write FTextOnChangeEvent;
    property OnTyping: TNotifyEvent read FTextOnTypingEvent write FTextOnTypingEvent;

    property AutoTranslatePrompt: Boolean read GetAutoTranslatePrompt write SetAutoTranslatePrompt default True;

    property DisableFocusEffect: Boolean read GetDisableFocusEffect write SetDisableFocusEffect default False;
    property ControlType: TControlType read GetControlType write SetControlType default TControlType.Styled;
    property CheckSpelling: Boolean read GetCheckSpelling write SetCheckSpelling default False;
    property KeyboardType: TVirtualKeyboardType read GetKeyboardType write SetKeyboardType
      default TVirtualKeyboardType.Default;
    property ReturnKeyType: TReturnKeyType read GetReturnKeyType write SetReturnKeyType default TReturnKeyType.Default;
    property Password: Boolean read GetPassword write SetPassword default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property KillFocusByReturn: Boolean read GetKillFocusByReturn write SetKillFocusByReturn default False;
    property ImeMode: TImeMode read GetImeMode write SetImeMode default TImeMode.imDontCare;
    property FilterChar: string read GetFilterChar write SetFilterChar;
    property MaxLength: Integer read GetMaxLength write SetMaxLength default 0;
  end;

procedure Register;

implementation

uses
  System.Math, FMX.Ani;

procedure Register;
begin
  RegisterComponents('ZMaterial', [TZMaterialEdit]);
end;

type
  TPromptTextSettings = class(TTextSettings)
  public
    constructor Create(const AOwner: TPersistent); override;
  published
    property Font;
    property FontColor;
    property HorzAlign;
    property VertAlign;
    property WordWrap default True;
    property Trimming default TTextTrimming.Character;
  end;

  { TMaterialEdit }

function GetHeightLabel(const aLabel: TLabel; aFontStyle: TFontStyles; aFontSize: Single): Single;
var
  L: TTextLayout;
begin
  L := TTextLayoutManager.DefaultTextLayout.Create;
  L.BeginUpdate;
  try
    L.Text := aLabel.Text;
    L.MaxSize := TPointF.Create(aLabel.Width, 1000.00);
    L.Font := aLabel.Font;
    L.Font.Size := aFontSize;
    L.Font.Style := aFontStyle;
    L.WordWrap := aLabel.WordWrap;
    L.HorizontalAlign := aLabel.TextAlign;
    L.VerticalAlign := aLabel.VertTextAlign;
  finally
    L.EndUpdate;
  end;
  Result := L.Height;
  FreeAndNil(L);
end;

constructor TZMaterialEdit.Create(AOwner: TComponent);
var
  LClass: TTextSettingsClass;
begin
  inherited Create(AOwner);

  FAnimation := True;

  FEdit := TEdit.Create(Self);
  FEdit.Parent := Self;
  FEdit.Stored := False;
  FEdit.Align := TAlignLayout.Bottom;
  FEdit.Size.PlatformDefault := True;

  FEdit.OnChange := OnChangeEvent;
  FEdit.OnTyping := OnTypingEvent;
  FEdit.OnEnter := OnEnterEvent;
  FEdit.OnExit := OnExitEvent;
  FEdit.OnKeyUp := OnKeyUpEvent;
  FEdit.OnKeyDown := OnKeyDownEvent;
  FEdit.OnApplyStyleLookup := OnApplyStyleLookupText;

  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Stored := False;
  FLabel.Align := TAlignLayout.None;
  FLabel.Size.PlatformDefault := True;
  FLabel.WordWrap := False;
  FLabel.AutoSize := True;
  FLabel.HitTest := False;
  FLabel.StyledSettings := FLabel.StyledSettings - [TStyledSetting.Size, TStyledSetting.FontColor,
    TStyledSetting.Style];
  FLabel.BringToFront;
  FLabel.OnApplyStyleLookup := OnApplyStyleLookupPrompt;

  HitTest := False;
  Stored := True;

  LClass := GetTextSettingsClass;
  // if LClass = nil then
  // LClass := TPromptTextSettings;

  FPromptUpTextSettings := LClass.Create(Self);
  FPromptUpTextSettings.Assign(FLabel.TextSettings);

  FPromptDownTextSettings := LClass.Create(Self);
  FPromptDownTextSettings.Assign(FLabel.TextSettings);

  FPromptUpStylledSettings := FLabel.StyledSettings;
  FPromptDownStylledSettings := FLabel.StyledSettings;

  Height := 2 * FEdit.Height;
  DoResize;
  FLabel.Position.Y := FEdit.Position.Y + FEdit.Height / 2 - GetHeightLabel(FLabel, FPromptUpTextSettings.Font.Style,
    FPromptUpTextSettings.Font.Size) / 2;
end;

destructor TZMaterialEdit.Destroy;
begin
  FreeAndNil(FPromptUpTextSettings);
  FreeAndNil(FPromptDownTextSettings);
{$IFDEF AUTOREFCOUNT}
  FEdit.DisposeOf;
  FEdit := nil;
  FLabel.DisposeOf;
  FLabel := nil;
{$ELSE}
  FreeAndNil(FEdit);
  FreeAndNil(FLabel);
{$ENDIF}
  inherited;
end;

procedure TZMaterialEdit.OnApplyStyleLookupPrompt(Sender: TObject);
begin
  DoResize;
end;

procedure TZMaterialEdit.OnApplyStyleLookupText(Sender: TObject);
begin
  DoResize;
end;

procedure TZMaterialEdit.OnChangeEvent(Sender: TObject);
begin
  if (FEdit.Text.Trim.IsEmpty) and (not FEdit.IsFocused) then
    DoMyExit
  else
    DoMyEnter;

  if Assigned(FTextOnChangeEvent) then
    FTextOnChangeEvent(Sender);
end;

procedure TZMaterialEdit.OnTypingEvent(Sender: TObject);
begin
  if (FEdit.Text.Trim.IsEmpty) and (not FEdit.IsFocused) then
    DoMyExit
  else
    DoMyEnter;

  if Assigned(FTextOnTypingEvent) then
    FTextOnTypingEvent(Sender);
end;

procedure TZMaterialEdit.OnEnterEvent(Sender: TObject);
begin
  DoMyEnter;
end;

procedure TZMaterialEdit.OnExitEvent(Sender: TObject);
begin
  DoMyExit;
end;

procedure TZMaterialEdit.OnKeyDownEvent(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, KeyChar, Shift);
end;

procedure TZMaterialEdit.OnKeyUpEvent(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Assigned(OnKeyUp) then
    OnKeyUp(Self, Key, KeyChar, Shift);
end;

procedure TZMaterialEdit.DoMyEnter;
var
  NewPositionY: Single;
begin
  NewPositionY := FEdit.Position.Y - Ymargin - GetHeightLabel(FLabel, FPromptUpTextSettings.Font.Style,
    FPromptUpTextSettings.Font.Size);
  if not SameValue(FLabel.Position.Y, NewPositionY) then
  begin
    if FAnimation then
      TAnimator.AnimateFloatWait(FLabel, 'Position.Y', NewPositionY, 0.08)
    else
      FLabel.Position.Y := NewPositionY;
    FLabel.StyledSettings := FPromptUpStylledSettings;
    FLabel.TextSettings.Assign(FPromptUpTextSettings);
  end;
  if Assigned(OnEnter) then
    OnEnter(Self);
end;

procedure TZMaterialEdit.DoMyExit;
var
  NewPositionY: Single;
begin
  NewPositionY := FEdit.Position.Y + FEdit.Height / 2 - GetHeightLabel(FLabel, FPromptDownTextSettings.Font.Style,
    FPromptDownTextSettings.Font.Size) / 2;
  if (FEdit.Text.Trim.IsEmpty) and (not SameValue(FLabel.Position.Y, NewPositionY)) then
  begin
    if FAnimation then
      TAnimator.AnimateFloatWait(FLabel, 'Position.Y', NewPositionY, 0.08)
    else
      FLabel.Position.Y := NewPositionY;
    FLabel.StyledSettings := FPromptDownStylledSettings;
    FLabel.TextSettings.Assign(FPromptDownTextSettings);
    FLabel.Repaint;
  end;
  if Assigned(OnExit) then
    OnExit(Self);
end;

procedure TZMaterialEdit.DoResize;
begin
  // Height := FEdit.Height + FLabel.Height + (Ymargin * 4);
  FLabel.Width := FEdit.Width;

  FLabel.Position.X := Xmargin + FEdit.Position.X;
  if not FEdit.Text.IsEmpty then
  begin
    FLabel.Position.Y := FEdit.Position.Y - Ymargin - GetHeightLabel(FLabel, FPromptUpTextSettings.Font.Style,
      FPromptUpTextSettings.Font.Size);
    FLabel.StyledSettings := FPromptUpStylledSettings;
    FLabel.TextSettings.Assign(FPromptUpTextSettings);
  end
  else
  begin
    FLabel.Position.Y := FEdit.Position.Y + FEdit.Height / 2 -
      GetHeightLabel(FLabel, FPromptDownTextSettings.Font.Style, FPromptDownTextSettings.Font.Size) / 2;
    FLabel.StyledSettings := FPromptDownStylledSettings;
    FLabel.TextSettings.Assign(FPromptDownTextSettings);
  end;
end;

function TZMaterialEdit.GetAnimation: Boolean;
begin
  Result := FAnimation;
end;

function TZMaterialEdit.GetAutoTranslatePrompt: Boolean;
begin
  Result := FLabel.AutoTranslate;
end;

function TZMaterialEdit.GetCheckSpelling: Boolean;
begin
  Result := FEdit.CheckSpelling;
end;

function TZMaterialEdit.GetControlType: TControlType;
begin
  Result := FEdit.ControlType;
end;

function TZMaterialEdit.GetDisableFocusEffect: Boolean;
begin
  Result := FEdit.DisableFocusEffect;
end;

function TZMaterialEdit.GetEdit: TEdit;
begin
  Result := FEdit;
end;

function TZMaterialEdit.GetFilterChar: string;
begin
  Result := FEdit.FilterChar;
end;

function TZMaterialEdit.GetImeMode: TImeMode;
begin
  Result := FEdit.ImeMode;
end;

function TZMaterialEdit.GetKeyboardType: TVirtualKeyboardType;
begin
  Result := FEdit.KeyboardType;
end;

function TZMaterialEdit.GetKillFocusByReturn: Boolean;
begin
  Result := FEdit.KillFocusByReturn;
end;

function TZMaterialEdit.GetLabel: TLabel;
begin
  Result := FLabel;
end;

function TZMaterialEdit.GetMaxLength: Integer;
begin
  Result := FEdit.MaxLength;
end;

function TZMaterialEdit.GetPassword: Boolean;
begin
  Result := FEdit.Password;
end;

function TZMaterialEdit.GetPrompt: string;
begin
  Result := FLabel.Text;
end;

function TZMaterialEdit.GetPromptSettings: TTextSettings;
begin
  Result := FPromptDownTextSettings;
end;

function TZMaterialEdit.GetPromptStyledSettings: TStyledSettings;
begin
  Result := FLabel.StyledSettings;
end;

function TZMaterialEdit.GetPromptUpTextSettings: TTextSettings;
begin
  Result := FPromptUpTextSettings;
end;

function TZMaterialEdit.GetReadOnly: Boolean;
begin
  Result := FEdit.ReadOnly;
end;

function TZMaterialEdit.GetReturnKeyType: TReturnKeyType;
begin
  Result := FEdit.ReturnKeyType;
end;

function TZMaterialEdit.GetText: string;
begin
  Result := FEdit.Text;
end;

function TZMaterialEdit.GetTextSettings: TTextSettings;
begin
  Result := FEdit.TextSettings;
end;

function TZMaterialEdit.GetTextSettingsClass: TTextSettingsClass;
begin
  Result := TPromptTextSettings;
end;

function TZMaterialEdit.GetTextStyledSettings: TStyledSettings;
begin
  Result := FEdit.StyledSettings;
end;

procedure TZMaterialEdit.Resize;
begin
  inherited;
  DoResize;
end;

procedure TZMaterialEdit.SetAnimation(const Value: Boolean);
begin
  FAnimation := Value;
end;

procedure TZMaterialEdit.SetAutoTranslatePrompt(const Value: Boolean);
begin
  FLabel.AutoTranslate := Value;
end;

procedure TZMaterialEdit.SetCheckSpelling(const Value: Boolean);
begin
  FEdit.CheckSpelling := Value;
end;

procedure TZMaterialEdit.SetControlType(const Value: TControlType);
begin
  FEdit.ControlType := Value;
end;

procedure TZMaterialEdit.SetDisableFocusEffect(const Value: Boolean);
begin
  FEdit.DisableFocusEffect := Value;
end;

procedure TZMaterialEdit.SetFilterChar(const Value: string);
begin
  FEdit.FilterChar := Value;
end;

procedure TZMaterialEdit.SetFocus;
begin
  // inherited;
  FEdit.SetFocus;
end;

procedure TZMaterialEdit.SetImeMode(const Value: TImeMode);
begin
  FEdit.ImeMode := Value;
end;

procedure TZMaterialEdit.SetKeyboardType(Value: TVirtualKeyboardType);
begin
  FEdit.KeyboardType := Value;
end;

procedure TZMaterialEdit.SetKillFocusByReturn(const Value: Boolean);
begin
  FEdit.KillFocusByReturn := Value;
end;

procedure TZMaterialEdit.SetMaxLength(const Value: Integer);
begin
  FEdit.MaxLength := Value;
end;

procedure TZMaterialEdit.SetPassword(const Value: Boolean);
begin
  FEdit.Password := Value;
end;

procedure TZMaterialEdit.SetPrompt(const Value: string);
begin
  FLabel.Text := Value;
end;

procedure TZMaterialEdit.SetPromptSettings(const Value: TTextSettings);
begin
  FPromptDownTextSettings.Assign(Value);
  FLabel.TextSettings.Assign(FPromptDownTextSettings);
  DoResize;
end;

procedure TZMaterialEdit.SetPromptStyledSettings(const Value: TStyledSettings);
begin
  FLabel.StyledSettings := Value;
  FPromptDownStylledSettings := Value;
end;

procedure TZMaterialEdit.SetPromptUpTextSettings(const Value: TTextSettings);
begin
  FPromptUpTextSettings.Assign(Value);
end;

procedure TZMaterialEdit.SetReadOnly(const Value: Boolean);
begin
  FEdit.ReadOnly := Value;
end;

procedure TZMaterialEdit.SetReturnKeyType(Value: TReturnKeyType);
begin
  FEdit.ReturnKeyType := Value;
end;

procedure TZMaterialEdit.SetText(const Value: string);
begin
  FEdit.Text := Value;
end;

procedure TZMaterialEdit.SetTextSettings(const Value: TTextSettings);
begin
  FEdit.TextSettings.Assign(Value);
end;

procedure TZMaterialEdit.SetTextStyledSettings(const Value: TStyledSettings);
begin
  FEdit.StyledSettings := Value;
end;

{ TPromptTextSettings }

constructor TPromptTextSettings.Create(const AOwner: TPersistent);
begin
  inherited;
  WordWrap := False;
  Trimming := TTextTrimming.Character;
end;

end.
