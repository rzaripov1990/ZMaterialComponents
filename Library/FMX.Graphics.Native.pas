 { ------------------------------------------ }
{ }
{ (c) 2017 by Aone }
{ }
{ QQ: 1467948783 }
{ }
{ http://www.cnblogs.com/onechen }
{ }
{ ------------------------------------------ }
{ Start: 2017.01.16 }
{ ------------------------------------------ }

// [原创] 改善 Firemonkey Canvas 几何绘图质量问题（移动平台）by Aone

  unit FMX.Graphics.Native;

{$I NativeDraw.inc}

interface

uses
  System.Types,
  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.UIConsts,
  System.Math,
  System.Math.Vectors,
  System.Generics.Collections,

{$IFDEF ANDROID}
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.Helpers,
  Androidapi.Bitmap,
  Androidapi.JNI.App,
  FMX.Surfaces,
  FMX.Helpers.Android,
{$ENDIF}
{$IFDEF IOS}
  iOSapi.CocoaTypes,
  iOSapi.UIKit,
  iOSapi.Foundation,
  iOSapi.CoreGraphics,
  Macapi.CoreFoundation,
  Macapi.ObjectiveC,
  Macapi.Helpers,
  FMX.Helpers.iOS,
{$ENDIF}
  FMX.Types,
  FMX.Graphics,
  FMX.Platform;

type
  TDrawProc = reference to procedure;

  TCanvasHelper = class helper for TCanvas
  public
    // 原生绘图
    procedure NativeDraw(const ARect: TRectF; const ADrawProc: TDrawProc);
    // 涂色 + 线色一次完成
    procedure DrawRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const AFill: TBrush; const AStroke: TStrokeBrush;
      const ACornerType: TCornerType = TCornerType.Round; const Inside: Boolean = False); overload;
    procedure DrawPath(const APath: TPathData; const AOpacity: Single; const AFill: TBrush;
      const AStroke: TStrokeBrush); overload;
    procedure DrawEllipse(const ARect: TRectF; const AOpacity: Single; const AFill: TBrush; const AStroke: TStrokeBrush;
      const Inside: Boolean = False); overload;
    procedure DrawArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single;
      const AFill: TBrush; const AStroke: TStrokeBrush; const Inside: Boolean = False); overload;
    procedure DrawPolygon(const Points: TPolygon; const AOpacity: Single; const AFill: TBrush;
      const AStroke: TStrokeBrush); overload;
{$IFDEF UseNativeDraw}
  private
{$IF Defined(IOS) or Defined(ANDROID)}
    procedure ApplyGradient({$IFDEF ANDROID}const Paint1: JPaint; {$ENDIF} const ABrush: TBrush; const ARect: TRectF);
    procedure ApplyFill({$IFDEF ANDROID}const Paint1: JPaint;
      {$ENDIF} const ABrush: TBrush; const ARect: TRectF; const AOpacity: Single);
    procedure DrawFill({$IFDEF ANDROID}const Paint1: JPaint;
      {$ENDIF} const ABrush: TBrush; const SrcRect, DesRect: TRectF; const AOpacity: Single);
    procedure ApplyStroke({$IFDEF ANDROID}const Paint1: JPaint;
      {$ENDIF} const AStroke: TStrokeBrush; const ARect: TRectF; const AOpacity: Single);
{$ENDIF}
  public
{$IF Defined(IOS) or Defined(ANDROID)}
    // 下列为 Canvas 原有函数
    procedure DrawLine(const APt1, APt2: TPointF; const AOpacity: Single); overload;
    procedure DrawLine(const APt1, APt2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush); overload;

    procedure FillRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ACornerType: TCornerType = TCornerType.Round); overload;
    procedure FillRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ABrush: TBrush; const ACornerType: TCornerType = TCornerType.Round); overload;
    procedure DrawRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ACornerType: TCornerType = TCornerType.Round); overload;
    procedure DrawRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ABrush: TStrokeBrush; const ACornerType: TCornerType = TCornerType.Round); overload;

    procedure FillPath(const APath: TPathData; const AOpacity: Single); overload;
    procedure FillPath(const APath: TPathData; const AOpacity: Single; const ABrush: TBrush); overload;
    procedure DrawPath(const APath: TPathData; const AOpacity: Single); overload;
    procedure DrawPath(const APath: TPathData; const AOpacity: Single; const ABrush: TStrokeBrush); overload;

    procedure FillEllipse(const ARect: TRectF; const AOpacity: Single); overload;
    procedure FillEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); overload;
    procedure DrawEllipse(const ARect: TRectF; const AOpacity: Single); overload;
    procedure DrawEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); overload;

    procedure FillArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single); overload;
    procedure FillArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single;
      const ABrush: TBrush); overload;
    procedure DrawArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single); overload;
    procedure DrawArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single;
      const ABrush: TStrokeBrush); overload;

    procedure FillPolygon(const Points: TPolygon; const AOpacity: Single); overload;
    procedure DrawPolygon(const Points: TPolygon; const AOpacity: Single); overload;

    procedure IntersectClipRect(const ARect: TRectF); overload;
    procedure ExcludeClipRect(const ARect: TRectF); overload;
{$ENDIF}
{$ENDIF}
  end;

implementation

{$IF not Defined(UseNativeDraw) or Defined(MSWINDOWS) or Defined(MACOSONLY)}

procedure TCanvasHelper.NativeDraw(const ARect: TRectF; const ADrawProc: TDrawProc);
begin
  // 绘图函数
  if Assigned(ADrawProc) then
    ADrawProc;
end;

procedure TCanvasHelper.DrawRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
  const AOpacity: Single; const AFill: TBrush; const AStroke: TStrokeBrush;
  const ACornerType: TCornerType = TCornerType.Round; const Inside: Boolean = False);
var
  R: TRectF;
begin
  R := ARect;

  // 线在区内
  if Inside and (AStroke <> nil) and (AStroke.Kind <> TBrushKind.None) then
    InflateRect(R, -(AStroke.Thickness / 2), -(AStroke.Thickness / 2));

  FillRect(R, XRadius, YRadius, ACorners, AOpacity, AFill, ACornerType);
  DrawRect(R, XRadius, YRadius, ACorners, AOpacity, AStroke, ACornerType);
end;

procedure TCanvasHelper.DrawPath(const APath: TPathData; const AOpacity: Single; const AFill: TBrush;
  const AStroke: TStrokeBrush);
begin
  FillPath(APath, AOpacity, AFill);
  DrawPath(APath, AOpacity, AStroke);
end;

procedure TCanvasHelper.DrawEllipse(const ARect: TRectF; const AOpacity: Single; const AFill: TBrush;
  const AStroke: TStrokeBrush; const Inside: Boolean = False);
var
  R: TRectF;
begin
  R := ARect;

  // 线在区内
  if Inside and (AStroke <> nil) and (AStroke.Kind <> TBrushKind.None) then
    InflateRect(R, -(AStroke.Thickness / 2), -(AStroke.Thickness / 2));

  FillEllipse(R, AOpacity, AFill);
  DrawEllipse(R, AOpacity, AStroke);
end;

procedure TCanvasHelper.DrawArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single;
  const AFill: TBrush; const AStroke: TStrokeBrush; const Inside: Boolean = False);
var
  R: TRectF;
  P: TPointF;
begin
  R := RectF(Center.X - Radius.X, Center.Y - Radius.Y, Center.X + Radius.X, Center.Y + Radius.Y);

  // 线在区内
  if Inside and (AStroke <> nil) and (AStroke.Kind <> TBrushKind.None) then
  begin
    R.Offset(-R.Left, -R.Top);
    InflateRect(R, -(AStroke.Thickness / 2), -(AStroke.Thickness / 2));
  end;

  P := PointF(R.Width / 2, R.Height / 2);

  FillArc(Center, P, StartAngle, SweepAngle, AOpacity, AFill);
  DrawArc(Center, P, StartAngle, SweepAngle, AOpacity, AStroke);
end;

procedure TCanvasHelper.DrawPolygon(const Points: TPolygon; const AOpacity: Single; const AFill: TBrush;
  const AStroke: TStrokeBrush);
begin
  if AFill <> nil then
    Self.Fill.Assign(AFill);

  if AStroke <> nil then
    Self.Stroke.Assign(AStroke);

  FillPolygon(Points, AOpacity);
  DrawPolygon(Points, AOpacity);
end;
{$ENDIF}
{$IFDEF UseNativeDraw}
{$IFDEF ANDROID}

var
  GlobalCanvas: JCanvas;

function JBitmapToBitmap(const AImage: JBitmap): TBitmap;
var
  Surface: TBitmapSurface;
begin
  Surface := TBitmapSurface.Create;
  Result := nil;
  try
    if JBitmapToSurface(AImage, Surface) then
    begin
      Result := TBitmap.Create;
      Result.Assign(Surface);
    end;
  finally
    Surface.Free;
  end;
end;

procedure TCanvasHelper.NativeDraw(const ARect: TRectF; const ADrawProc: TDrawProc);
var
  Bitmap1: JBitmap;
  Paint: JPaint;
  Bitmap: TBitmap;
  ScreenService: IFMXScreenService;
  Scale1: Single;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenService) then
    Scale1 := ScreenService.GetScreenScale
  else
    Scale1 := 1;

  Bitmap1 := TJBitmap.JavaClass.createBitmap(Ceil(ARect.Width * Scale1), Ceil(ARect.Height * Scale1),
    TJBitmap_Config.JavaClass.ARGB_8888);
  GlobalCanvas := TJCanvas.JavaClass.init(Bitmap1);
  GlobalCanvas.save;
  GlobalCanvas.scale(Scale1, Scale1);

  // 透明底色
  Paint := TJPaint.Wrap(TJPaint.JavaClass.init(TJPaint.JavaClass.ANTI_ALIAS_FLAG));
  Paint.setStyle(TJPaint_Style.Wrap(TJPaint_Style.JavaClass.Fill));
  Paint.setARGB(0, 255, 255, 255);
  GlobalCanvas.DrawRect(GlobalCanvas.getClipBounds, Paint);

  // 绘图函数
  if Assigned(ADrawProc) then
    ADrawProc;

  GlobalCanvas.restore;

  // 显示
  Bitmap := JBitmapToBitmap(Bitmap1);
  DrawBitmap(Bitmap, RectF(0, 0, Bitmap.Width, Bitmap.Height), ARect, 1);
  FreeAndNil(Bitmap);
end;

procedure TCanvasHelper.ApplyGradient(const Paint1: JPaint; const ABrush: TBrush; const ARect: TRectF);
var
  i: Integer;
  aColors: TJavaArray<Integer>;
  aStops: TJavaArray<Single>;
  aLinearShader: JLinearGradient;
  aRadialShader: JRadialGradient;
begin
  aColors := TJavaArray<Integer>.Create(ABrush.Gradient.Points.Count);
  aStops := TJavaArray<Single>.Create(ABrush.Gradient.Points.Count);

  for i := 0 to ABrush.Gradient.Points.Count - 1 do
  begin
    aColors[ABrush.Gradient.Points.Count - 1 - i] := Integer(ABrush.Gradient.Points[i].Color);
    aStops[ABrush.Gradient.Points.Count - 1 - i] := 1 - ABrush.Gradient.Points[i].Offset;
  end;

  case ABrush.Gradient.Style of
    // 线渐层
    TGradientStyle.Linear:
      begin
        aLinearShader := TJLinearGradient.JavaClass.init(ARect.Left + ABrush.Gradient.StopPosition.X * ARect.Width,
          ARect.Top + ABrush.Gradient.StopPosition.Y * ARect.Height, ARect.Left + ABrush.Gradient.StartPosition.X *
          ARect.Width, ARect.Top + ABrush.Gradient.StartPosition.Y * ARect.Height, aColors, aStops,
          TJShader_TileMode.JavaClass.CLAMP);
        Paint1.setShader(aLinearShader);
        aLinearShader := nil;
      end;
    // 圆渐层
    TGradientStyle.Radial:
      begin
        aRadialShader := TJRadialGradient.JavaClass.init(ARect.CenterPoint.X, ARect.CenterPoint.Y, ARect.Width / 2,
          aColors, aStops, TJShader_TileMode.JavaClass.CLAMP);
        Paint1.setShader(aRadialShader);
        aRadialShader := nil;
      end;
  else
    Paint1.setShader(nil);
  end;

  FreeAndNil(aColors);
  FreeAndNil(aStops);
end;

procedure TCanvasHelper.ApplyFill(const Paint1: JPaint; const ABrush: TBrush; const ARect: TRectF;
  const AOpacity: Single);
begin
  if (ABrush.Kind = TBrushKind.Resource) and (ABrush.Resource <> nil) and (ABrush.Resource.Brush <> nil) then
    ABrush.Assign(ABrush.Resource.Brush);

  Paint1.setStyle(TJPaint_Style.Wrap(TJPaint_Style.JavaClass.Fill));

  case ABrush.Kind of
    TBrushKind.Solid:
      Paint1.setARGB(TColorRec(ABrush.Color).A, TColorRec(ABrush.Color).B, TColorRec(ABrush.Color).G,
        TColorRec(ABrush.Color).R);
    TBrushKind.Gradient:
      ApplyGradient(Paint1, ABrush, ARect);
  else
    Paint1.setARGB(0, 255, 255, 255);
  end;
end;

procedure TCanvasHelper.DrawFill(const Paint1: JPaint; const ABrush: TBrush; const SrcRect, DesRect: TRectF;
  const AOpacity: Single);
begin
  if (ABrush.Kind = TBrushKind.Resource) and (ABrush.Resource <> nil) and (ABrush.Resource.Brush <> nil) then
    ABrush.Assign(ABrush.Resource.Brush);

  if ABrush.Kind = TBrushKind.Bitmap then
  begin
    // 未完成
  end;
end;

procedure TCanvasHelper.ApplyStroke(const Paint1: JPaint; const AStroke: TStrokeBrush; const ARect: TRectF;
  const AOpacity: Single);
var
  i: Integer;
  Dash: TJavaArray<Single>;
begin
  if (AStroke.Kind = TBrushKind.Resource) and (AStroke.Resource <> nil) and (AStroke.Resource.Brush <> nil) then
    AStroke.Assign(AStroke.Resource.Brush);

  Paint1.setStyle(TJPaint_Style.Wrap(TJPaint_Style.JavaClass.Stroke));

  // Thickness = 0 还是有线
  if AStroke.Thickness > 0 then
  begin
    Paint1.setStrokeWidth(AStroke.Thickness);

    case AStroke.Kind of
      TBrushKind.Solid, TBrushKind.Bitmap:
        Paint1.setARGB(TColorRec(AStroke.Color).A, TColorRec(AStroke.Color).B, TColorRec(AStroke.Color).G,
          TColorRec(AStroke.Color).R);
      TBrushKind.Gradient:
        ApplyGradient(Paint1, AStroke, ARect);
    else
      Paint1.setARGB(0, 0, 0, 0);
    end;

    case AStroke.Cap of
      TStrokeCap.Flat:
        Paint1.setStrokeCap(TJPaint_Cap.JavaClass.BUTT);
      TStrokeCap.Round:
        Paint1.setStrokeCap(TJPaint_Cap.JavaClass.Round);
    end;

    if Length(AStroke.DashArray) > 0 then
    begin
      Dash := TJavaArray<Single>.Create(Length(AStroke.DashArray));

      for i := 0 to High(AStroke.DashArray) do
      begin
        Dash[i] := AStroke.DashArray[i] * AStroke.Thickness;
        if AStroke.Cap = TStrokeCap.Round then
        begin
          if Odd(i) then
            Dash[i] := (AStroke.DashArray[i] + 0.9) * AStroke.Thickness
          else
            Dash[i] := (AStroke.DashArray[i] - 0.9) * AStroke.Thickness;
        end;
      end;

      Paint1.setPathEffect(TJDashPathEffect.JavaClass.init(Dash, 0));
    end;

    case AStroke.Join of
      TStrokeJoin.Miter:
        Paint1.setStrokeJoin(TJPaint_Join.JavaClass.Miter);
      TStrokeJoin.Round:
        Paint1.setStrokeJoin(TJPaint_Join.JavaClass.Round);
      TStrokeJoin.Bevel:
        Paint1.setStrokeJoin(TJPaint_Join.JavaClass.Bevel);
    end;
  end
  else
    Paint1.setARGB(0, 0, 0, 0);
end;

procedure TCanvasHelper.DrawLine(const APt1, APt2: TPointF; const AOpacity: Single);
begin
  DrawLine(APt1, APt2, AOpacity, Self.Stroke);
end;

procedure TCanvasHelper.DrawLine(const APt1, APt2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush);
var
  Paint1: JPaint;
begin
  if GlobalCanvas = nil then
    Exit;

  if ABrush.Kind <> TBrushKind.None then
  begin
    Paint1 := TJPaint.Wrap(TJPaint.JavaClass.init(TJPaint.JavaClass.ANTI_ALIAS_FLAG));
    ApplyStroke(Paint1, ABrush, TRectF.Create(APt1.X, APt1.Y, APt2.X, APt2.Y), AOpacity);
    GlobalCanvas.DrawLine(APt1.X, APt1.Y, APt2.X, APt2.Y, Paint1);
  end;
end;

procedure TCanvasHelper.DrawPath(const APath: TPathData; const AOpacity: Single; const AFill: TBrush;
  const AStroke: TStrokeBrush);
var
  i: Integer;
  Path1: JPath;
  Paint1: JPaint;
  SrcRect: TRectF;
begin
  if GlobalCanvas = nil then
    Exit;

  SrcRect := APath.GetBounds;
  Path1 := TJPath.Wrap(TJPath.JavaClass.init);

  i := 0;
  while i < APath.Count do
  begin
    case APath.Points[i].Kind of
      // 移到
      TPathPointKind.MoveTo:
        Path1.MoveTo(APath.Points[i].Point.X, APath.Points[i].Point.Y);
      // 线到
      TPathPointKind.LineTo:
        Path1.LineTo(APath.Points[i].Point.X, APath.Points[i].Point.Y);
      // 曲线
      TPathPointKind.CurveTo:
        begin
          Path1.cubicTo(APath.Points[i].Point.X, APath.Points[i].Point.Y, APath.Points[i + 1].Point.X,
            APath.Points[i + 1].Point.Y, APath.Points[i + 2].Point.X, APath.Points[i + 2].Point.Y);
          Inc(i, 2);
        end;
      // 关闭
      TPathPointKind.Close:
        Path1.Close;
    end;
    Inc(i);
  end;

  GlobalCanvas.save;
  Paint1 := TJPaint.Wrap(TJPaint.JavaClass.init(TJPaint.JavaClass.ANTI_ALIAS_FLAG));

  if (AFill <> nil) and (AFill.Kind <> TBrushKind.None) then
  begin
    if AFill.Kind = TBrushKind.Bitmap then
    begin
      GlobalCanvas.DrawPath(Path1, Paint1);
      DrawFill(Paint1, AFill, SrcRect, SrcRect, AOpacity);
    end
    else
    begin
      Path1.setFillType(TJPath_FillType.Wrap(TJPath_FillType.JavaClass.EVEN_ODD));
      ApplyFill(Paint1, AFill, SrcRect, AOpacity);
      GlobalCanvas.DrawPath(Path1, Paint1);
    end;
  end;

  if (AStroke <> nil) and (AStroke.Kind <> TBrushKind.None) then
  begin
    ApplyStroke(Paint1, AStroke, SrcRect, AOpacity);
    GlobalCanvas.DrawPath(Path1, Paint1);
  end;

  GlobalCanvas.restore;
end;

procedure TCanvasHelper.IntersectClipRect(const ARect: TRectF);
var
  JR: JRectF;
begin
  if GlobalCanvas <> nil then
  begin
    JR := TJRectF.JavaClass.init(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);

    GlobalCanvas.clipRect(JR, TJRegion_Op.JavaClass.INTERSECT);
  end;
end;

procedure TCanvasHelper.ExcludeClipRect(const ARect: TRectF);
var
  JR: JRectF;
begin
  if GlobalCanvas <> nil then
  begin
    JR := TJRectF.JavaClass.init(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);

    GlobalCanvas.clipRect(JR, TJRegion_Op.JavaClass.REPLACE);
  end;
end;
{$ENDIF}
{$IFDEF IOS}

var
  GlobalCanvas: CGContextRef;

function PointToCGPoint(const P: TPointF): CGPoint;
begin
  Result := CGPointMake(P.X, P.Y);
end;

procedure TCanvasHelper.NativeDraw(const ARect: TRectF; const ADrawProc: TDrawProc);
var
  NativeImage: UIImage;
  PNGRepresentation: NSData;
  PNGMemoryStream: TMemoryStream;
  Bitmap: TBitmap;
begin
  NativeImage := nil;

  UIGraphicsBeginImageContextWithOptions(CGSizeMake(ARect.Width, ARect.Height), False, 0 { 目前机子Scale } );

  GlobalCanvas := UIGraphicsGetCurrentContext;

  CGContextSaveGState(GlobalCanvas);

  // 透明底色
  TUIColor.Wrap(TUIColor.OCClass.colorWithHue(0, 0, 1, 0)).setFill;
  CGContextFillRect(GlobalCanvas, CGContextGetClipBoundingBox(GlobalCanvas));

  // 绘图函数
  if Assigned(ADrawProc) then
    ADrawProc;

  CGContextRestoreGState(GlobalCanvas);

  NativeImage := TUIImage.Wrap(UIGraphicsGetImageFromCurrentImageContext);

  if Assigned(NativeImage) then
  begin
    PNGRepresentation := TNSData.Wrap(UIImagePNGRepresentation((NativeImage as ILocalObject).GetObjectID));

    if Assigned(PNGRepresentation) then
    begin
      Bitmap := TBitmap.Create(0, 0);
      PNGMemoryStream := TMemoryStream.Create;
      PNGMemoryStream.Write(PNGRepresentation.bytes^, PNGRepresentation.Length);
      Bitmap.LoadFromStream(PNGMemoryStream);
      PNGMemoryStream.Free;

      // 显示
      DrawBitmap(Bitmap, RectF(0, 0, Bitmap.Width, Bitmap.Height), ARect, 1);
      FreeAndNil(Bitmap);
    end;
  end;

  UIGraphicsEndImageContext;
end;

procedure TCanvasHelper.ApplyGradient(const ABrush: TBrush; const ARect: TRectF);
var
  i: Integer;
  Locations: TArray<CGFloat>;
  colorSpace: CGColorSpaceRef;
  Gradient: CGGradientRef;
  colors: NSMutableArray;
  RCenter: CGPoint;
begin
  if GlobalCanvas = nil then
    Exit;

  SetLength(Locations, ABrush.Gradient.Points.Count);
  colors := TNSMutableArray.Wrap(TNSMutableArray.OCClass.arrayWithCapacity(ABrush.Gradient.Points.Count));

  for i := 0 to ABrush.Gradient.Points.Count - 1 do
  begin
    colors.addObject(AlphaColorToUIColor(ABrush.Gradient.Points[i].Color).CGColor);
    Locations[i] := ABrush.Gradient.Points[i].Offset;
  end;

  colorSpace := CGColorSpaceCreateDeviceRGB;
  Gradient := CGGradientCreateWithColors(colorSpace, (colors as ILocalObject).GetObjectID, @Locations[0]);

  case ABrush.Gradient.Style of
    // 线渐层
    TGradientStyle.Linear:
      begin
        CGContextDrawLinearGradient(GlobalCanvas, Gradient,
          CGPointMake(ARect.Left + ABrush.Gradient.StartPosition.X * ARect.Width,
          ARect.Top + ABrush.Gradient.StartPosition.Y * ARect.Height),
          CGPointMake(ARect.Left + ABrush.Gradient.StopPosition.X * ARect.Width,
          ARect.Top + ABrush.Gradient.StopPosition.Y * ARect.Height), 0);
      end;
    // 圆渐层
    TGradientStyle.Radial:
      begin
        RCenter.Create(PointF(ABrush.Gradient.RadialTransform.RotationCenter.X * ARect.Width,
          ABrush.Gradient.RadialTransform.RotationCenter.Y * ARect.Height) + ARect.TopLeft);
        CGContextDrawRadialGradient(GlobalCanvas, Gradient, RCenter, ARect.Width / 2, RCenter, 0,
          kCGGradientDrawsBeforeStartLocation or kCGGradientDrawsAfterEndLocation);
      end;
  end;

  CFRelease(colorSpace);
  CFRelease(Gradient);
end;

procedure TCanvasHelper.ApplyFill(const ABrush: TBrush; const ARect: TRectF; const AOpacity: Single);
var
  LColor: TAlphaColorF;
begin
  if GlobalCanvas = nil then
    Exit;

  if (ABrush.Kind = TBrushKind.Resource) and (ABrush.Resource <> nil) and (ABrush.Resource.Brush <> nil) then
    ABrush.Assign(ABrush.Resource.Brush);

  case ABrush.Kind of
    TBrushKind.Solid:
      begin
        LColor := TAlphaColorF.Create(MakeColor(ABrush.Color, AOpacity));
        CGContextSetRGBFillColor(GlobalCanvas, LColor.R, LColor.G, LColor.B, LColor.A);
      end;
  else
    CGContextSetRGBFillColor(GlobalCanvas, 0, 0, 0, 0);
  end;

  // 渐层
  if (ABrush.Kind = TBrushKind.Gradient) and (CGContextIsPathEmpty(GlobalCanvas) = 0) then
  begin
    CGContextClip(GlobalCanvas);
    ApplyGradient(ABrush, ARect);
  end;
end;

procedure TCanvasHelper.DrawFill(const ABrush: TBrush; const SrcRect, DesRect: TRectF; const AOpacity: Single);
begin
  if ABrush.Kind = TBrushKind.Bitmap then
  begin
    // 未完成
  end
  else
  begin
    ApplyFill(ABrush, DesRect, AOpacity);
    CGContextEOFillPath(GlobalCanvas);
  end;
end;

procedure TCanvasHelper.ApplyStroke(const AStroke: TStrokeBrush; const ARect: TRectF; const AOpacity: Single);
var
  Dash: TDashArray;
  i: Integer;
  LColor: TAlphaColorF;
  R: TRectF;
begin
  if GlobalCanvas = nil then
    Exit;

  if (AStroke.Kind = TBrushKind.Resource) and (AStroke.Resource <> nil) and (AStroke.Resource.Brush <> nil) then
    AStroke.Assign(AStroke.Resource.Brush);

  case AStroke.Kind of
    TBrushKind.Solid, TBrushKind.Bitmap:
      begin
        LColor := TAlphaColorF.Create(MakeColor(AStroke.Color, AOpacity));
        CGContextSetRGBStrokeColor(GlobalCanvas, LColor.R, LColor.G, LColor.B, LColor.A);
      end;
  else
    CGContextSetRGBStrokeColor(GlobalCanvas, 0, 0, 0, 0);
  end;

  case AStroke.Cap of
    TStrokeCap.Flat:
      CGContextSetLineCap(GlobalCanvas, kCGLineCapButt);
    TStrokeCap.Round:
      CGContextSetLineCap(GlobalCanvas, kCGLineCapRound);
  end;

  if Length(AStroke.DashArray) > 0 then
  begin
    // select the proper dash array for the printer
    if FPrinter <> nil then
      if AStroke.Dash <> TStrokeDash.Custom then
        Dash := TStrokeBrush.StdDash[TStrokeBrush.TDashDevice.Printer, AStroke.Dash].DashArray
      else
        Dash := AStroke.DashArray
    else // adjust the line dashes for the screen
    begin
      SetLength(Dash, Length(AStroke.DashArray));
      for i := 0 to High(AStroke.DashArray) do
      begin
        Dash[i] := AStroke.DashArray[i] * AStroke.Thickness;
        if AStroke.Cap = TStrokeCap.Round then
        begin
          if Odd(i) then
            Dash[i] := (AStroke.DashArray[i] + 1) * AStroke.Thickness
          else
            Dash[i] := (AStroke.DashArray[i] - 1) * AStroke.Thickness;
        end;
      end;
    end;
    CGContextSetLineDash(GlobalCanvas, AStroke.DashOffset, @Dash[0], Length(AStroke.DashArray));
  end
  else
    CGContextSetLineDash(GlobalCanvas, 0, nil, 0);

  case AStroke.Join of
    TStrokeJoin.Miter:
      CGContextSetLineJoin(GlobalCanvas, kCGLineJoinMiter);

    TStrokeJoin.Round:
      CGContextSetLineJoin(GlobalCanvas, kCGLineJoinRound);

    TStrokeJoin.Bevel:
      CGContextSetLineJoin(GlobalCanvas, kCGLineJoinBevel);
  end;

  CGContextSetLineWidth(GlobalCanvas, AStroke.Thickness);

  // 渐层
  if (AStroke.Kind = TBrushKind.Gradient) and (CGContextIsPathEmpty(GlobalCanvas) = 0) then
  begin
    CGContextReplacePathWithStrokedPath(GlobalCanvas);
    CGContextClip(GlobalCanvas);
    R := ARect;
    InflateRect(R, AStroke.Thickness / 2, AStroke.Thickness / 2);
    ApplyGradient(AStroke, R);
  end;
end;

procedure TCanvasHelper.DrawLine(const APt1, APt2: TPointF; const AOpacity: Single);
begin
  DrawLine(APt1, APt2, AOpacity, Self.Stroke);
end;

procedure TCanvasHelper.DrawLine(const APt1, APt2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush);
var
  R: TRectF;
begin
  if GlobalCanvas = nil then
    Exit;

  if ABrush.Kind <> TBrushKind.None then
  begin
    CGContextSaveGState(GlobalCanvas);
    CGContextBeginPath(GlobalCanvas);

    CGContextMoveToPoint(GlobalCanvas, APt1.X, APt1.Y);
    CGContextAddLineToPoint(GlobalCanvas, APt2.X, APt2.Y);

    // 加上线粗
    R := TRectF.Create(APt1.X, APt1.Y, APt2.X, APt2.Y);
    InflateRect(R, ABrush.Thickness / 2, ABrush.Thickness / 2);

    ApplyStroke(ABrush, R, AOpacity);
    CGContextStrokePath(GlobalCanvas);

    CGContextRestoreGState(GlobalCanvas);
  end;
end;

procedure TCanvasHelper.DrawPath(const APath: TPathData; const AOpacity: Single; const AFill: TBrush;
  const AStroke: TStrokeBrush);

  procedure ShowPath;
  var
    i: Integer;
    CurvePoint1, CurvePoint2: TPointF;
  begin
    i := 0;
    while i < APath.Count do
    begin
      case APath[i].Kind of
        TPathPointKind.MoveTo:
          CGContextMoveToPoint(GlobalCanvas, APath[i].Point.X, APath[i].Point.Y);
        TPathPointKind.LineTo:
          CGContextAddLineToPoint(GlobalCanvas, APath[i].Point.X, APath[i].Point.Y);
        TPathPointKind.CurveTo:
          begin
            CurvePoint1 := APath[i].Point;
            Inc(i);
            CurvePoint2 := APath[i].Point;
            Inc(i);
            CGContextAddCurveToPoint(GlobalCanvas, CurvePoint1.X, CurvePoint1.Y, CurvePoint2.X, CurvePoint2.Y,
              APath[i].Point.X, APath[i].Point.Y);
          end;
        TPathPointKind.Close:
          CGContextClosePath(GlobalCanvas);
      end;

      Inc(i);
    end;
  end;

var
  SrcRect: TRectF;
begin
  if GlobalCanvas = nil then
    Exit;

  SrcRect := APath.GetBounds;

  // 涂色
  if (AFill <> nil) and (AFill.Kind <> TBrushKind.None) then
  begin
    CGContextSaveGState(GlobalCanvas);
    CGContextBeginPath(GlobalCanvas);
    ShowPath;
    DrawFill(AFill, SrcRect, SrcRect, AOpacity);
    CGContextRestoreGState(GlobalCanvas);
  end;

  // 画线
  if (AStroke <> nil) and (AStroke.Kind <> TBrushKind.None) then
  begin
    CGContextSaveGState(GlobalCanvas);
    CGContextBeginPath(GlobalCanvas);
    ShowPath;
    ApplyStroke(AStroke, SrcRect, AOpacity);
    CGContextStrokePath(GlobalCanvas);
    CGContextRestoreGState(GlobalCanvas);
  end;
end;

procedure TCanvasHelper.IntersectClipRect(const ARect: TRectF);
begin
  if GlobalCanvas <> nil then
    CGContextClipToRect(GlobalCanvas, CGRectFromRect(ARect));
end;

procedure TCanvasHelper.ExcludeClipRect(const ARect: TRectF);
var
  LRect: array [0 .. 3] of CGRect;
begin
  if GlobalCanvas <> nil then
  begin
    LRect[0] := CGRectFromRect(TRectF.Create(-FWidth, -FWidth, ARect.Left, FHeight));
    LRect[1] := CGRectFromRect(TRectF.Create(ARect.Right, -FHeight, FWidth, FHeight));
    LRect[2] := CGRectFromRect(TRectF.Create(ARect.Left, -FHeight, ARect.Right, ARect.Top));
    LRect[3] := CGRectFromRect(TRectF.Create(ARect.Left, ARect.Bottom, ARect.Right, FHeight));
    CGContextClipToRects(GlobalCanvas, @LRect[0], 4);
  end;
end;
{$ENDIF}
{$IF Defined(IOS) or Defined(ANDROID)}

procedure TCanvasHelper.DrawRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
  const AOpacity: Single; const AFill: TBrush; const AStroke: TStrokeBrush;
  const ACornerType: TCornerType = TCornerType.Round; const Inside: Boolean = False);
var
  Path: TPathData;
  R: TRectF;
begin
  R := ARect;

  // 线在区内
  if Inside and (AStroke <> nil) and (AStroke.Kind <> TBrushKind.None) then
    InflateRect(R, -(AStroke.Thickness / 2), -(AStroke.Thickness / 2));

  Path := TPathData.Create;
  try
    Path.AddRectangle(R, XRadius, YRadius, ACorners, ACornerType);
    DrawPath(Path, AOpacity, AFill, AStroke);
  finally
    Path.Free;
  end;
end;

procedure TCanvasHelper.DrawEllipse(const ARect: TRectF; const AOpacity: Single; const AFill: TBrush;
  const AStroke: TStrokeBrush; const Inside: Boolean = False);
var
  Path: TPathData;
  R: TRectF;
begin
  R := ARect;

  // 线在区内
  if Inside and (AStroke <> nil) and (AStroke.Kind <> TBrushKind.None) then
  begin
    R.Offset(-R.Left, -R.Top);
    InflateRect(R, -(AStroke.Thickness / 2), -(AStroke.Thickness / 2));
  end;

  Path := TPathData.Create;
  try
    Path.AddEllipse(R);
    DrawPath(Path, AOpacity, AFill, AStroke);
  finally
    Path.Free;
  end;
end;

procedure TCanvasHelper.DrawArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single;
  const AFill: TBrush; const AStroke: TStrokeBrush; const Inside: Boolean = False);
var
  R: TRectF;
  P: TPointF;
  Path: TPathData;
begin
  R := RectF(Center.X - Radius.X, Center.Y - Radius.Y, Center.X + Radius.X, Center.Y + Radius.Y);

  // 线在区内
  if Inside and (AStroke <> nil) and (AStroke.Kind <> TBrushKind.None) then
  begin
    R.Offset(-R.Left, -R.Top);
    InflateRect(R, -(AStroke.Thickness / 2), -(AStroke.Thickness / 2));
  end;

  P := PointF(R.Width / 2, R.Height / 2);

  Path := TPathData.Create;
  try
    Path.AddArc(Center, P, StartAngle, SweepAngle);
    DrawPath(Path, AOpacity, AFill, AStroke);
  finally
    Path.Free;
  end;
end;

procedure TCanvasHelper.DrawPolygon(const Points: TPolygon; const AOpacity: Single; const AFill: TBrush;
  const AStroke: TStrokeBrush);
var
  i: Integer;
  Path: TPathData;
  PathBreakFound: Boolean;
begin
  Path := TPathData.Create;

  try
    PathBreakFound := False;

    for i := 0 to High(Points) do
    begin
      if i = 0 then
      begin
        Path.MoveTo(Points[i]);
      end
      else if (Points[i].X = PolygonPointBreak.X) and (Points[i].Y = PolygonPointBreak.Y) then
      begin
        Path.ClosePath;
        PathBreakFound := True;
      end
      else
        Path.LineTo(Points[i]);
    end;

    if not PathBreakFound then
      Path.ClosePath;

    DrawPath(Path, AOpacity, AFill, AStroke);
  finally
    Path.Free;
  end;
end;

procedure TCanvasHelper.FillRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
  const AOpacity: Single; const ACornerType: TCornerType = TCornerType.Round);
begin
  DrawRect(ARect, XRadius, YRadius, ACorners, AOpacity, Self.Fill, nil, ACornerType);
end;

procedure TCanvasHelper.FillRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
  const AOpacity: Single; const ABrush: TBrush; const ACornerType: TCornerType = TCornerType.Round);
begin
  DrawRect(ARect, XRadius, YRadius, ACorners, AOpacity, ABrush, nil, ACornerType);
end;

procedure TCanvasHelper.DrawRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
  const AOpacity: Single; const ACornerType: TCornerType = TCornerType.Round);
begin
  DrawRect(ARect, XRadius, YRadius, ACorners, AOpacity, nil, Self.Stroke, ACornerType);
end;

procedure TCanvasHelper.DrawRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
  const AOpacity: Single; const ABrush: TStrokeBrush; const ACornerType: TCornerType = TCornerType.Round);
begin
  DrawRect(ARect, XRadius, YRadius, ACorners, AOpacity, nil, ABrush, ACornerType);
end;

procedure TCanvasHelper.FillPath(const APath: TPathData; const AOpacity: Single);
begin
  DrawPath(APath, AOpacity, Self.Fill, nil);
end;

procedure TCanvasHelper.FillPath(const APath: TPathData; const AOpacity: Single; const ABrush: TBrush);
begin
  DrawPath(APath, AOpacity, ABrush, nil);
end;

procedure TCanvasHelper.DrawPath(const APath: TPathData; const AOpacity: Single);
begin
  DrawPath(APath, AOpacity, nil, Self.Stroke);
end;

procedure TCanvasHelper.DrawPath(const APath: TPathData; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  DrawPath(APath, AOpacity, nil, ABrush);
end;

procedure TCanvasHelper.FillEllipse(const ARect: TRectF; const AOpacity: Single);
begin
  DrawEllipse(ARect, AOpacity, Self.Fill, nil);
end;

procedure TCanvasHelper.FillEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush);
begin
  DrawEllipse(ARect, AOpacity, ABrush, nil);
end;

procedure TCanvasHelper.DrawEllipse(const ARect: TRectF; const AOpacity: Single);
begin
  DrawEllipse(ARect, AOpacity, nil, Self.Stroke);
end;

procedure TCanvasHelper.DrawEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  DrawEllipse(ARect, AOpacity, nil, ABrush);
end;

procedure TCanvasHelper.FillArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single);
begin
  DrawArc(Center, Radius, StartAngle, SweepAngle, AOpacity, Self.Fill, nil);
end;

procedure TCanvasHelper.FillArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single;
  const ABrush: TBrush);
begin
  DrawArc(Center, Radius, StartAngle, SweepAngle, AOpacity, ABrush, nil);
end;

procedure TCanvasHelper.DrawArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single);
begin
  DrawArc(Center, Radius, StartAngle, SweepAngle, AOpacity, nil, Self.Stroke);
end;

procedure TCanvasHelper.DrawArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single;
  const ABrush: TStrokeBrush);
begin
  DrawArc(Center, Radius, StartAngle, SweepAngle, AOpacity, nil, ABrush);
end;

procedure TCanvasHelper.FillPolygon(const Points: TPolygon; const AOpacity: Single);
begin
  DrawPolygon(Points, AOpacity, Self.Fill, nil);
end;

procedure TCanvasHelper.DrawPolygon(const Points: TPolygon; const AOpacity: Single);
begin
  DrawPolygon(Points, AOpacity, nil, Self.Stroke);
end;
{$ENDIF}
{$ENDIF}

end.
