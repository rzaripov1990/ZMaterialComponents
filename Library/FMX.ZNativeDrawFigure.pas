unit FMX.ZNativeDrawFigure;

{
  author: Ravil (ZuBy) Zaripov
  email: rzaripov1990@gmail.com
  http://github.com/rzaripov1990

  2017
}

{$I NativeDraw.inc}

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, System.UITypes, System.Types, System.Math.Vectors,
  FMX.Types, FMX.Controls, FMX.Layouts, FMX.Objects, FMX.Graphics,
  FMX.Graphics.Native,
  FMX.ZMaterialTypes;

type

  TZCustomNativeDrawFigure = class(TControl)
  public type
    TZNativeDrawFigureKind = (Line, DiagonalLine1, DiagonalLine2, Rectangle, RoundRect, Circle, Ellipse, Arc);
  private
    { Private declarations }
    FFill: TBrush;
    FStroke: TStrokeBrush;
    FKind: TZNativeDrawFigureKind;
    FXRadius: Single;
    FYRadius: Single;
    FSides: TSides;
    FCorners: TCorners;
    FCornerType: TCornerType;
    FEndAngle: Single;
    FStartAngle: Single;
    procedure SetFill(const Value: TBrush);
    procedure SetStroke(const Value: TStrokeBrush);
    procedure FillStrokeChanged(Sender: TObject);
    procedure SetKind(const Value: TZNativeDrawFigureKind);
    procedure SetXRadius(const Value: Single);
    procedure SetYRadius(const Value: Single);
    function IsSidesStored: Boolean;
    procedure SetSides(const Value: TSides);
    function IsCornersStored: Boolean;
    procedure SetCorners(const Value: TCorners);
    procedure SetCornerType(const Value: TCornerType);
    function GetShapeRect: TRectF;
    procedure SetEndAngle(const Value: Single);
    procedure SetStartAngle(const Value: Single);
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Kind: TZNativeDrawFigureKind read FKind write SetKind default TZNativeDrawFigureKind.Line;
    property Fill: TBrush read FFill write SetFill;
    property Stroke: TStrokeBrush read FStroke write SetStroke;
    { Round Rect }
    property XRadius: Single read FXRadius write SetXRadius;
    property YRadius: Single read FYRadius write SetYRadius;
    { Rectangle }
    property Sides: TSides read FSides write SetSides stored IsSidesStored;
    property Corners: TCorners read FCorners write SetCorners stored IsCornersStored;
    property CornerType: TCornerType read FCornerType write SetCornerType default TCornerType.Round;
    { Arc }
    property StartAngle: Single read FStartAngle write SetStartAngle;
    property EndAngle: Single read FEndAngle write SetEndAngle;
  published
    { Published declarations }
  end;

  TZNativeDrawFigureTemplateWithEvents = class(TZCustomNativeDrawFigure)
  published
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HitTest default True;
    property Padding;
    property Margins;
    property Opacity;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property Visible default True;
    property Width;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Mouse events }
    property OnClick;
    property OnDblClick;

    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    property OnPainting;
    property OnPaint;
    property OnResize;
  end;

  [ComponentPlatformsAttribute(cpaAllPlatforms)]
  TZNativeDrawFigureLine = class(TZNativeDrawFigureTemplateWithEvents)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Stroke;
  end;

  [ComponentPlatformsAttribute(cpaAllPlatforms)]
  TZNativeDrawFigureRectangle = class(TZNativeDrawFigureTemplateWithEvents)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Fill;
    property Stroke;
    property XRadius;
    property YRadius;
    property Sides;
  end;

  [ComponentPlatformsAttribute(cpaAllPlatforms)]
  TZNativeDrawFigureRoundRect = class(TZNativeDrawFigureTemplateWithEvents)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Fill;
    property Stroke;
    property Corners;
    property CornerType;
  end;

  [ComponentPlatformsAttribute(cpaAllPlatforms)]
  TZNativeDrawFigureEllipse = class(TZNativeDrawFigureTemplateWithEvents)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Fill;
    property Stroke;
  end;

  [ComponentPlatformsAttribute(cpaAllPlatforms)]
  TZNativeDrawFigureCircle = class(TZNativeDrawFigureTemplateWithEvents)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Fill;
    property Stroke;
  end;

  [ComponentPlatformsAttribute(cpaAllPlatforms)]
  TZNativeDrawFigureArc = class(TZNativeDrawFigureTemplateWithEvents)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Fill;
    property Stroke;
    property StartAngle;
    property EndAngle;
  end;

procedure Register;

implementation

uses
  System.Math;

procedure Register;
begin
  RegisterComponents('ZMaterial', [TZNativeDrawFigureLine, TZNativeDrawFigureRectangle, TZNativeDrawFigureRoundRect,
    TZNativeDrawFigureEllipse, TZNativeDrawFigureCircle, TZNativeDrawFigureArc]);
end;

// copied from FMX.Objects \\
function GetDrawingShapeRectAndSetThickness(const AFill: TBrush; const AStroke: TStrokeBrush; ALocalRect: TRectF;
  const Fit: Boolean; var FillShape, DrawShape: Boolean; var StrokeThicknessRestoreValue: Single): TRectF;
const
  MinRectAreaSize = 0.01;
begin
  FillShape := (AFill <> nil) and (AFill.Kind <> TBrushKind.None);
  DrawShape := (AStroke <> nil) and (AStroke.Kind <> TBrushKind.None);

  if Fit then
    Result := TRectF.Create(0, 0, 1, 1).FitInto(ALocalRect)
  else
    Result := ALocalRect;

  if DrawShape then
  begin
    if Result.Width < AStroke.Thickness then
    begin
      StrokeThicknessRestoreValue := AStroke.Thickness;
      FillShape := False;
      AStroke.Thickness := Min(Result.Width, Result.Height);
      Result.Left := (Result.Right + Result.Left) * 0.5;
      Result.Right := Result.Left + MinRectAreaSize;
    end
    else
      Result.Inflate(-AStroke.Thickness * 0.5, 0);

    if Result.Height < AStroke.Thickness then
    begin
      if StrokeThicknessRestoreValue < 0.0 then
        StrokeThicknessRestoreValue := AStroke.Thickness;
      FillShape := False;
      AStroke.Thickness := Min(Result.Width, Result.Height);
      Result.Top := (Result.Bottom + Result.Top) * 0.5;
      Result.Bottom := Result.Top + MinRectAreaSize;
    end
    else
      Result.Inflate(0, -AStroke.Thickness * 0.5);
  end;
end;

{ TZNativeDrawFigure }

constructor TZCustomNativeDrawFigure.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCorners := AllCorners;
  FXRadius := 0;
  FYRadius := 0;
  FSides := AllSides;

  FCornerType := TCornerType.Round;

  FStartAngle := 0;
  FEndAngle := -90;

  FFill := TBrush.Create(TBrushKind.Solid, $FFE0E0E0);
  FFill.OnChanged := FillStrokeChanged;
  FStroke := TStrokeBrush.Create(TBrushKind.Solid, $FF000000);
  FStroke.OnChanged := FillStrokeChanged;

  Repaint;
end;

destructor TZCustomNativeDrawFigure.Destroy;
begin
  FStroke.Free;
  FFill.Free;
  inherited Destroy;
end;

procedure TZCustomNativeDrawFigure.FillStrokeChanged(Sender: TObject);
begin
  if FUpdating = 0 then
    Repaint;
end;

function TZCustomNativeDrawFigure.GetShapeRect: TRectF;
begin
  Result := Self.LocalRect;
  if FStroke.Kind <> TBrushKind.None then
    InflateRect(Result, -(FStroke.Thickness / 2), -(FStroke.Thickness / 2));
end;

function TZCustomNativeDrawFigure.IsCornersStored: Boolean;
begin
  Result := FCorners <> AllCorners;
end;

function TZCustomNativeDrawFigure.IsSidesStored: Boolean;
begin
  Result := FSides * AllSides <> AllSides
end;

procedure TZCustomNativeDrawFigure.Paint;
var
  Off, Radius: Single;
  StrokeThicknessRestoreValue: Single;
  FillShape, DrawShape: Boolean;
  LShapeRect: TRectF;
  MidPoint: TPointF;
begin
  StrokeThicknessRestoreValue := FStroke.Thickness;

  Canvas.Fill.Assign(FFill);
  Canvas.Stroke.Assign(FStroke);

  try
    LShapeRect := GetDrawingShapeRectAndSetThickness(FFill, FStroke, Self.LocalRect,
      FKind = TZNativeDrawFigureKind.Circle, FillShape, DrawShape, StrokeThicknessRestoreValue);

{$IFDEF UseNativeDraw}
    Canvas.NativeDraw(LocalRect,
      procedure
      begin
{$ENDIF}
        Canvas.BeginScene;
        try
          case FKind of
            TZNativeDrawFigureKind.Line:
              begin
                Canvas.DrawLine(PointF(0, 0), PointF(LocalRect.Width, 0), AbsoluteOpacity);
              end;
            TZNativeDrawFigureKind.DiagonalLine1:
              begin
                Canvas.DrawLine(PointF(0, LocalRect.Height), PointF(LocalRect.Width, 0), AbsoluteOpacity);
              end;
            TZNativeDrawFigureKind.DiagonalLine2:
              begin
                Canvas.DrawLine(PointF(0, 0), PointF(LocalRect.Width, LocalRect.Height), AbsoluteOpacity);
              end;
            TZNativeDrawFigureKind.Rectangle:
              begin
                if FSides <> AllSides then
                begin
                  Off := LShapeRect.Left;
                  if not(TSide.Top in FSides) then
                    LShapeRect.Top := LShapeRect.Top - Off;
                  if not(TSide.Left in FSides) then
                    LShapeRect.Left := LShapeRect.Left - Off;
                  if not(TSide.Bottom in FSides) then
                    LShapeRect.Bottom := LShapeRect.Bottom + Off;
                  if not(TSide.Right in FSides) then
                    LShapeRect.Right := LShapeRect.Right + Off;

                  if FillShape then
                    Canvas.FillRect(LShapeRect, FXRadius, FYRadius, FCorners, AbsoluteOpacity, FFill, FCornerType);
                  if DrawShape then
                    Canvas.DrawRectSides(GetShapeRect, FXRadius, FYRadius, FCorners, AbsoluteOpacity, FSides, FStroke,
                      FCornerType);
                end
                else
                begin
                  if FillShape then
                    Canvas.FillRect(LShapeRect, FXRadius, FYRadius, FCorners, AbsoluteOpacity, FFill, FCornerType);
                  if DrawShape then
                    Canvas.DrawRect(LShapeRect, FXRadius, FYRadius, FCorners, AbsoluteOpacity, FStroke, FCornerType);
                end;
              end;
            TZNativeDrawFigureKind.RoundRect:
              begin
                if Height < Width then
                  Radius := Self.LocalRect.Height / 2
                else
                  Radius := Self.LocalRect.Width / 2;

                if FillShape then
                  Canvas.FillRect(LShapeRect, Radius, Radius, FCorners, AbsoluteOpacity, FFill, FCornerType);
                if DrawShape then
                  Canvas.DrawRect(LShapeRect, Radius, Radius, FCorners, AbsoluteOpacity, FStroke, FCornerType);
              end;
            TZNativeDrawFigureKind.Circle:
              begin
                if FillShape then
                  Canvas.FillEllipse(LShapeRect, AbsoluteOpacity, FFill);
                if DrawShape then
                  Canvas.DrawEllipse(LShapeRect, AbsoluteOpacity, FStroke);
              end;
            TZNativeDrawFigureKind.Ellipse:
              begin
                if FillShape then
                  Canvas.FillEllipse(LShapeRect, AbsoluteOpacity, FFill);
                if DrawShape then
                  Canvas.DrawEllipse(LShapeRect, AbsoluteOpacity, FStroke);
              end;
            TZNativeDrawFigureKind.Arc:
              begin
                MidPoint := LShapeRect.CenterPoint;
                if FillShape then
                  Canvas.FillArc(MidPoint, TPointF.Create(LShapeRect.Width * 0.5, LShapeRect.Height * 0.5), FStartAngle,
                    FEndAngle, AbsoluteOpacity, FFill);
                if DrawShape then
                  Canvas.DrawArc(MidPoint, TPointF.Create(LShapeRect.Width * 0.5, LShapeRect.Height * 0.5), FStartAngle,
                    FEndAngle, AbsoluteOpacity, FStroke);
              end;
          end;
        finally
          Canvas.EndScene;
        end;
{$IFDEF UseNativeDraw}
      end);
{$ENDIF}
  finally
    if StrokeThicknessRestoreValue <> FStroke.Thickness then
      FStroke.Thickness := StrokeThicknessRestoreValue;
  end;
end;

procedure TZCustomNativeDrawFigure.SetCorners(const Value: TCorners);
begin
  if FCorners = Value then
    exit;

  FCorners := Value;
  Repaint;
end;

procedure TZCustomNativeDrawFigure.SetCornerType(const Value: TCornerType);
begin
  if FCornerType = Value then
    exit;

  FCornerType := Value;
  Repaint;
end;

procedure TZCustomNativeDrawFigure.SetEndAngle(const Value: Single);
begin
  if SameValue(FEndAngle, Value, TEpsilon.Vector) then
    exit;

  FEndAngle := Value;
  Repaint;
end;

procedure TZCustomNativeDrawFigure.SetFill(const Value: TBrush);
begin
  FFill.Assign(Value);
end;

procedure TZCustomNativeDrawFigure.SetKind(const Value: TZNativeDrawFigureKind);
begin
  if FKind = Value then
    exit;

  FKind := Value;
  Repaint;
end;

procedure TZCustomNativeDrawFigure.SetSides(const Value: TSides);
begin
  if FSides = Value then
    exit;

  FSides := Value;
  Repaint;
end;

procedure TZCustomNativeDrawFigure.SetStartAngle(const Value: Single);
begin
  if SameValue(FStartAngle, Value, TEpsilon.Vector) then
    exit;

  FStartAngle := Value;
  Repaint;
end;

procedure TZCustomNativeDrawFigure.SetStroke(const Value: TStrokeBrush);
begin
  FStroke.Assign(Value);
end;

procedure TZCustomNativeDrawFigure.SetXRadius(const Value: Single);
var
  NewValue: Single;
begin
  if csDesigning in ComponentState then
    NewValue := Min(Value, Min(Width / 2, Height / 2))
  else
    NewValue := Value;

  if SameValue(FXRadius, NewValue, TEpsilon.Vector) then
    exit;

  FXRadius := Value;
  Repaint;
end;

procedure TZCustomNativeDrawFigure.SetYRadius(const Value: Single);
var
  NewValue: Single;
begin
  if csDesigning in ComponentState then
    NewValue := Min(Value, Min(Width / 2, Height / 2))
  else
    NewValue := Value;

  if SameValue(FYRadius, NewValue, TEpsilon.Vector) then
    exit;

  FYRadius := Value;
  Repaint;
end;

{ TZNativeDrawFigureLine }

constructor TZNativeDrawFigureLine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Kind := TZNativeDrawFigureKind.Line;
end;

{ TZNativeDrawFigureRectangle }

constructor TZNativeDrawFigureRectangle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Kind := TZNativeDrawFigureKind.Rectangle;
end;

{ TZNativeDrawFigureRoundRect }

constructor TZNativeDrawFigureRoundRect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Kind := TZNativeDrawFigureKind.RoundRect;
end;

{ TZNativeDrawFigureEllipse }

constructor TZNativeDrawFigureEllipse.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Kind := TZNativeDrawFigureKind.Ellipse;
end;

{ TZNativeDrawFigureCircle }

constructor TZNativeDrawFigureCircle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Kind := TZNativeDrawFigureKind.Circle;
end;

{ TZNativeDrawFigureArc }

constructor TZNativeDrawFigureArc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Kind := TZNativeDrawFigureKind.Arc;
  Fill.Kind := TBrushKind.None;
end;

end.
