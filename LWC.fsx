open System.Windows.Forms
open System.Drawing
open System


// Libreria

type WVMatrix () =
  let wv = new Drawing2D.Matrix()
  let vw = new Drawing2D.Matrix()

  member this.TranslateW (tx, ty) =
    wv.Translate(tx, ty)
    vw.Translate(-tx, -ty, Drawing2D.MatrixOrder.Append)

  member this.ScaleW (sx, sy) =
    wv.Scale(sx, sy)
    vw.Scale(1.f /sx, 1.f/ sy, Drawing2D.MatrixOrder.Append)

  member this.RotateW (a) =
    wv.Rotate(a)
    vw.Rotate(-a, Drawing2D.MatrixOrder.Append)

  member this.RotateV (a, p) =
    vw.RotateAt(a, p)
    wv.RotateAt(-a, p,  Drawing2D.MatrixOrder.Append)

  member this.TranslateV (tx, ty) =
    vw.Translate(tx, ty)
    wv.Translate(-tx, -ty, Drawing2D.MatrixOrder.Append)

  member this.ScaleV (sx, sy) =
    vw.Scale(sx, sy)
    wv.Scale(1.f /sx, 1.f/ sy, Drawing2D.MatrixOrder.Append)
  
  member this.TransformPointV (p:PointF) =
    let a = [| p |]
    vw.TransformPoints(a)
    a.[0]

  member this.TransformPointW (p:PointF) =
    let a = [| p |]
    wv.TransformPoints(a)
    a.[0]

  member this.VW with get() = vw
  member this.WV with get() = wv

 type LWCControl(w, h) =
  
  let wv = WVMatrix()
  
  let mutable sz = SizeF(w, h)
  let mutable pos = PointF()
  let mutable scaled = 0
  let mutable sel = false
  let mutable rotated = 0
  let mutable moving = false
  let mutable arr : ResizeArray<LWCControl> option = None
  let mutable impiling = false
  let mutable height = 0.f
  let mutable width = 0.f
  let mutable piled = false
  let mutable parent : UserControl option = None
  
  member this.WV = wv
  
  member this.Scaled 
    with get() = scaled
    and set(s) = scaled <- s
  
  member this.Rotated
    with get() = rotated
    and set(r) = rotated <- r

  member this.Piled
    with get() = piled
    and set(p) = piled <- p

  member this.IsSelected
    with get() = sel
    and set(s) = sel <-s

  member this.IsMoving
    with get() = moving
    and set(m) = moving <- m 

  member this.Parent
    with get() = parent
    and set(v) = parent <- v
   
   member this.Impiling
    with get() = impiling
    and set(i) = impiling <- i

   member this.Arr
    with get() = arr
    and set(a) = arr <- a 

  abstract OnPaint : PaintEventArgs -> unit
  default this.OnPaint (e) = ()

  abstract OnMouseDown : MouseEventArgs -> unit
  default this.OnMouseDown (e) = ()

  abstract OnMouseUp : MouseEventArgs -> unit
  default this.OnMouseUp (e) = ()

  abstract OnMouseMove : MouseEventArgs -> unit
  default this.OnMouseMove (e) = ()

  abstract OnKeyDown : KeyEventArgs -> unit
  default this.OnKeyDown (e) = ()

  member this.Invalidate() =
    match parent with
    | Some p -> p.Invalidate()
    | None -> ()

  member this.HitTest(p:Point) =
    let pt = wv.TransformPointV(PointF(single p.X, single p.Y))
    let boundingbox = RectangleF(0.f, 0.f, sz.Width, sz.Height)
    boundingbox.Contains(pt)
  //riporta l'elemento a forma iniziale scalando e ruotando
  member this.Change(c : LWCControl) = 
    if(c.Scaled > 0) then 
      for i = 0 to c.Scaled-1 do
        c.WV.ScaleW(1.f/1.1f, 1.f/1.1f);
    else
      for i = 0 to Math.Abs(c.Scaled-1) do
        c.WV.ScaleW(1.1f, 1.1f);
    c.WV.RotateV((single) -c.Rotated, c.WV.TransformPointW(PointF((c.Width / 2.f), (c.Height / 2.f))));
  //riapplica le modifiche alla forma scalando e ruotando
  member this.Restore(c : LWCControl) =
    c.WV.RotateV((single) c.Rotated, c.WV.TransformPointW(PointF((c.Width / 2.f), (c.Height / 2.f))));
    if(c.Scaled > 0) then 
      for i = 0 to c.Scaled-1 do
        c.WV.ScaleW(1.1f, 1.1f);
      else
      for i = 0 to Math.Abs(c.Scaled-1) do
        c.WV.ScaleW(1.f/1.1f, 1.f/1.1f);

  member this.MoveTest() =
    this.IsMoving

  member this.ClientSize
    with get() = sz
    and set(v) = 
      sz <- v
      this.Invalidate()
      
  member this.Position
    with get() = pos
    and set(v) =
      wv.TranslateV(pos.X, pos.Y)
      pos <- v
      wv.TranslateV(-pos.X, -pos.Y)
      this.Invalidate()

  member this.PositionInt with get() = Point(int pos.X, int pos.Y)
  member this.ClientSizeInt with get() = Size(int sz.Width, int sz.Height)

  member this.Left = pos.X
  member this.Top = pos.Y
  member this.Width 
    with get() = width
    and set(inp) = width <- inp
  member this.Height
    with get() = height
    and set(inp) = height <- inp