open System.Collections.Generic
open System
open System.Collections
open System.Drawing.Imaging
#load "LWC.fsx"
open System.Windows.Forms
open System.Drawing
open LWC

type LWCContainer() as this =
  inherit UserControl()

  
  let mutable drawing = false //bottone per disegnare premuto
  let controls = System.Collections.ObjectModel.ObservableCollection<LWCControl>()

  do
    this.SetStyle(ControlStyles.OptimizedDoubleBuffer, true)
    this.SetStyle(ControlStyles.AllPaintingInWmPaint ||| ControlStyles.OptimizedDoubleBuffer, true)
    controls.CollectionChanged.Add(fun e ->
      if( e.Action.Equals(Specialized.NotifyCollectionChangedAction.Add)) then for i in e.NewItems do (i :?> LWCControl).Parent <- Some(this :> UserControl);
      else ()
    ) 
  member this.LWControls with get() = controls
  
  member this.Drawing 
    with get() = drawing
    and set(s) = drawing <- s

  override this.OnMouseDown (e) =
    let oc =
      controls |> Seq.tryFindBack(fun c -> c.HitTest(e.Location))
    match oc with
    | Some c ->
      let p = c.WV.TransformPointV(PointF(single e.X, single e.Y))
      let evt = MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
      c.OnMouseDown(evt)
      this.Invalidate();
    | None -> ()

  override this.OnMouseUp (e) =
    let oc =
      controls |> Seq.tryFindBack(fun c -> c.HitTest(e.Location))
    match oc with
    | Some c ->
      let p = c.WV.TransformPointV(PointF(single e.X, single e.Y))
      let evt = MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
      c.OnMouseUp(evt)
      this.Invalidate();
    | None -> ()

    override this.OnMouseMove (e) =
      let oc = controls |> Seq.tryFindBack(fun c -> c.MoveTest())
      match oc with
      | Some c ->
      let evt = MouseEventArgs(e.Button, e.Clicks, int e.X, int e.Y, e.Delta)
      c.OnMouseMove(evt)
      this.Invalidate();
      | None -> ()

  override this.OnPaint(e) =
    controls
    |> Seq.iter(fun c ->
      let bkg = e.Graphics.Save()
      let evt = new PaintEventArgs(e.Graphics, Rectangle(c.PositionInt, c.ClientSizeInt))
      e.Graphics.Transform <- c.WV.WV
      c.OnPaint(evt)
      e.Graphics.Restore(bkg)
    )
  override this.OnKeyDown(e) =
    controls |> Seq.forall(fun c ->
      if( c.IsSelected ) then c.OnKeyDown(KeyEventArgs(e.KeyData)); this.Invalidate(); 
      true;
    ) |> ignore;
    ()

type LWImage( im : Image, w, h) as this=
  inherit LWCControl( w, h )
  let mutable cont = new LWCContainer()
  let mutable strokesList = List<List<PointF>>() //lista di linee
  let mutable currList = List<PointF>() //lista di punti che rappresenta una linea
  let mutable (arr : PointF array) = [||]
  let mutable boundingbox = RectangleF()
  do
    this.Width <- w
    this.Height <- h
    this.WV.TranslateW(10.f, 180.f)
 
  member this.Container
    with get() = cont
    and set( c ) = cont <- c
  
  override this.OnPaint(e) =
    let g = e.Graphics
    
    g.DrawImage(im, 0, 0)
    for el in strokesList do //disegna le linee
      if(el.Count > 0) then ( arr <- el.ToArray(); for i = 1 to arr.Length-1 do g.DrawLine(Pens.Blue, arr.[i-1], arr.[i])
      );
    
    if ( this.IsSelected) then (
      boundingbox <- RectangleF(0.f, 0.f, (single) this.Width, (single) this.Height); 
      g.DrawRectangle( Pens.Red, 0.f, 0.f, (single) this.Width, (single) this.Height); ())
  
  override this.OnMouseDown(e) =
    if( not cont.Drawing ) then (this.IsSelected <- not this.IsSelected; if(this.IsSelected) then this.IsMoving <- true; ())
    else ( this.IsSelected<-true; currList <- new List<PointF>(); strokesList.Add(currList); this.IsMoving <- true;)

  override this.OnMouseUp(e) =
    this.IsMoving <- false;
  
  override this.OnMouseMove(e) =
    let mutable p = PointF(0.f, 0.f)
    if( not cont.Drawing ) then ( //Drag and drop
                                  p <- this.Position |> this.WV.TransformPointW ;                                 
                                  this.WV.TranslateW( single e.Location.X - p.X - this.Width/2.f, single e.Location.Y - p.Y - this.Height/2.f ))
    else ( //disegno
      if(boundingbox.Contains(this.WV.TransformPointV <| PointF( single e.Location.X, single e.Location.Y))) 
      then currList.Add(this.WV.TransformPointV <| PointF( single e.Location.X, single e.Location.Y))
      else this.IsMoving <- false; )
    
  override this.OnKeyDown(e) =
    match e.KeyCode with
      | Keys.W ->
       this.Change(this)
       this.WV.TranslateW(0.f, -10.f);
       this.Restore(this)
       this.Invalidate();
      | Keys.S -> 
       this.Change(this)
       this.WV.TranslateW(0.f, 10.f);
       this.Restore(this)
       this.Invalidate();  
      | Keys.A -> 
       this.Change(this)
       this.WV.TranslateW(-10.f, 0.f);
       this.Restore(this)
       this.Invalidate();
      | Keys.D -> 
       this.Change(this)
       this.WV.TranslateW(10.f, 0.f);
       this.Restore(this)
       this.Invalidate();
      | Keys.R ->
       this.WV.RotateV(10.f, this.WV.TransformPointW(PointF((this.Width / 2.f), (this.Height / 2.f)))); this.Rotated <- this.Rotated + 10;
       this.Invalidate()
      | Keys.E -> 
        this.WV.RotateV(-10.f, this.WV.TransformPointW(PointF((this.Width / 2.f), (this.Height / 2.f)))); this.Rotated <- this.Rotated - 10;
        this.Invalidate();   
      | Keys.Z ->
       this.WV.TranslateW( this.Width /2.f, this.Height/2.f);
       this.WV.ScaleW(1.1f, 1.1f); this.Scaled <- this.Scaled+1;this.Invalidate(); //this.Height<- this.Height*1.1f;this.Width<- this.Width*1.1f;this.Invalidate();
       this.WV.TranslateW( -this.Width /2.f, -this.Height/2.f);
      | Keys.X ->
       this.WV.TranslateW( this.Width /2.f, this.Height/2.f);
       this.WV.ScaleW( 1.f/1.1f, 1.f/1.1f); this.Scaled <- this.Scaled-1; this.Invalidate();//this.Height<- this.Height*(1.f/1.1f);this.Width<- this.Width*(1.f/1.1f);this.Invalidate();
       this.WV.TranslateW( -this.Width /2.f, -this.Height/2.f);
      | _ -> ()

type LWButton(w, h) as this =
  inherit LWCControl(w, h)
  let mutable cont = new LWCContainer()
  do
    this.Width <- w
    this.Height <- h
  member this.Container
    with get() = cont
    and set( c ) = cont <- c
  
  override this.OnPaint(e) =
    let g = e.Graphics
    g.FillRectangle(Brushes.Red, 0.f, 0.f, w, h)
    g.DrawLine(Pens.Black, PointF(10.f, h / 2.f), PointF( w - 10.f, h / 2.f))
    g.DrawLine(Pens.Black, PointF(w / 2.f, 10.f), PointF( w / 2.f, h - 10.f))
  
  override this.OnMouseDown( e ) =
    if(not cont.Drawing) then(
      let mutable dialog = new OpenFileDialog()
      dialog.InitialDirectory <- "C:\\Users\\Lorenzo Venchi\\Desktop\\Mid"
      dialog.Filter <- "Image files (*.jpg, *.jpeg, *.jpe, *.jfif, *.png, *.gif) | *.jpg; *.jpeg; *.jpe; *.jfif; *.png; *.gif"
      dialog.FilterIndex <- 2;
  
      match(dialog.ShowDialog()) with
      | DialogResult.OK -> 
      let ima = Bitmap.FromFile(dialog.FileName)
      //adatto la dimensione alla risoluzione
      let im = LWImage(ima,single ima.Width * 96.f/ima.HorizontalResolution,single ima.Height * 96.f/ima.VerticalResolution , Container = this.Container )
      cont.LWControls.Add(im)
      this.Invalidate();
      | _ -> ()
    );
//attiva o disattiva il disegno su immagini
type DrawOnImageButton(w, h) as this =
  inherit LWCControl(w, h)
  let mutable cont = new LWCContainer()
  do
    this.Width <- w
    this.Height <- h
  member this.Container
    with get() = cont
    and set( c ) = cont <- c
  
  override this.OnPaint(e) =
    let g = e.Graphics
    if(cont.Drawing) then g.FillRectangle(Brushes.Green, 0.f, 0.f, w, h) else g.FillRectangle(Brushes.Red, 0.f, 0.f, w, h)
    g.DrawString("DRAW", new Font(FontFamily.GenericSansSerif, 10.f ), Brushes.Black, PointF(5.f, 20.f))
  
  override this.OnMouseDown( e ) =
    cont.Drawing <- not cont.Drawing

type Pile(w, h, pilemembers : ResizeArray<LWCControl>, contain : LWCContainer) as this =
  inherit LWCControl(w, h)
  let mutable cont = contain
  let mutable members = pilemembers
  do
    contain.LWControls.Add(this)
    this.Width <- w
    this.Height <- h
    this.WV.TranslateW( 150.f,180.f); this.Invalidate();
    this.Arr <- Some(pilemembers)

  member this.Container
    with get() = cont
    and set( c ) = cont <- c

  //impile images not rotated
  member this.Impile() =  
    pilemembers.ForEach(fun c -> 
    (
      c.WV.RotateV((single) -c.Rotated, c.WV.TransformPointW(PointF((c.Width / 2.f), (c.Height / 2.f))));
      if(c.Scaled > 0) then 
        for i = 0 to c.Scaled-1 do
          c.WV.TranslateW( c.Width /2.f, c.Height/2.f);
          c.WV.ScaleW(1.f/1.1f, 1.f/1.1f);
          c.WV.TranslateW( -c.Width /2.f, -c.Height/2.f);
      else if( c.Scaled < 0) then
        for i = 0 to Math.Abs(c.Scaled-1) do
          c.WV.TranslateW( c.Width /2.f, c.Height/2.f);
          c.WV.ScaleW(1.1f, 1.1f);
          c.WV.TranslateW( -c.Width /2.f, -c.Height/2.f);
      let vpos = (this.WV.TransformPointW(this.Position))
      let cpos =  (c.WV.TransformPointW(c.Position))
      let mutable dx = vpos.X - cpos.X + single (pilemembers.IndexOf(c) * 5)
      let mutable dy = vpos.Y - cpos.Y + single (pilemembers.IndexOf(c) * 5)
      let t = new Timer(Interval=30)
      let stepx = Math.Abs(dx / 30.f)
      let stepy = Math.Abs(dy / 30.f)
      
      
       
      t.Tick.Add(fun _ ->
        if( Math.Abs(dx) < 1.f && Math.Abs(dy) < 1.f)
        then 
          t.Stop(); c.Rotated <- 0; c.Scaled <-0
          c.Invalidate(); 
        else
        let trX = (float32 (Math.Sign( single (dx))))*stepx
        let trY = (float32 (Math.Sign( single (dy))))*stepy
        
        c.WV.TranslateW( trX , trY );c.Invalidate(); dx <- dx - trX; dy <- dy - trY;);
      t.Start()
      c.Invalidate()
    ))
  override this.OnPaint(e) =
    let g = e.Graphics
    if(this.IsSelected) then g.DrawRectangle(Pens.Green,0.f, 0.f, this.Width, this.Height) else g.DrawRectangle(Pens.Black,0.f, 0.f, this.Width, this.Height)
  
  override this.OnMouseDown(e) =
    if( not cont.Drawing && pilemembers.Count > 0 ) then ( 
      //controlla click interno alla pila
      let c = pilemembers |> Seq.tryFindBack(fun c -> RectangleF(0.f, 0.f, this.Width - 5.f, this.Height - 5.f).Contains( PointF( single e.Location.X,single e.Location.Y)))
      match c with
      //rimuovi dalla pila l'ultima immagine aggiunta
      |Some c ->
      pilemembers.[pilemembers.Count-1].IsSelected<-true; 
      pilemembers.[pilemembers.Count-1].Piled<-false;
      pilemembers.RemoveAt(pilemembers.Count-1);
      this.IsSelected <- false;
      if(pilemembers.Count = 0 ) then (cont.LWControls.Remove(this) |> ignore) //cancella pila se è vuota
                                 else ( //ridimensiona la pila se necessario
                                   let mutable MH = 0.f
                                   let mutable MW = 0.f
                                   for i = 0 to pilemembers.Count-1 do
                                              if( pilemembers.[i].Height + (float32 (i * 5)) > MH ) then MH <- pilemembers.[i].Height + (float32 (i * 5))
                                              if( pilemembers.[i].Width + (float32 (i * 5)) > MW ) then MW <- pilemembers.[i].Width + (float32 (i * 5))
                                   if( this.Height > MH ) then (this.Height <- MH + 5.f);
                                   if( this.Width > MW ) then (this.Width <-  MW + 5.f));

      |None -> (); this.IsSelected <- not this.IsSelected; if(this.IsSelected) then this.IsMoving <- true;
    )
  //muove la pila
  override this.OnMouseMove(e) =
    let mutable p = PointF(0.f, 0.f)
    let mutable x = 0.f
    let mutable y = 0.f
    if( not cont.Drawing && this.IsMoving) then (
                                  p <- this.Position |> this.WV.TransformPointW ;
                                  x <- single e.Location.X - p.X - this.Width/2.f;
                                  y <- single e.Location.Y - p.Y - this.Height/2.f;
                                  this.WV.RotateW((single) -this.Rotated);
                                  this.WV.TranslateW( x , y )
                                  this.WV.RotateW((single) this.Rotated);
                                  pilemembers.ForEach( fun c -> 
                                    ( c.WV.RotateW((single) -c.Rotated));
                                      c.WV.TranslateW(x,y);
                                      c.WV.RotateW((single) c.Rotated);
                                  )
    )
    
    override this.OnKeyDown(e) =
    match e.KeyCode with
      | Keys.W ->
        this.Change(this)
        this.WV.TranslateW(0.f, -10.f);
        this.Restore(this)
        this.Invalidate();
        pilemembers.ForEach( fun c ->
        if( not c.IsSelected) then (
         c.Change(c)
         c.WV.TranslateW(0.f, -10.f);
         c.Restore(c)
         c.Invalidate();
        ));
       
      | Keys.S -> 
       this.Change(this)
       this.WV.TranslateW(0.f, 10.f);
       this.Restore(this)
       this.Invalidate();
       pilemembers.ForEach( fun c ->
       if( not c.IsSelected) then (
         c.Change(c)
         c.WV.TranslateW(0.f, 10.f);
         c.Restore(c)
         c.Invalidate();
      ));
      | Keys.A -> 
       this.Change(this)
       this.WV.TranslateW(-10.f, 0.f);
       this.Restore(this)
       this.Invalidate();
       pilemembers.ForEach( fun c ->
       if( not c.IsSelected) then (
         c.Change(c)
         c.WV.TranslateW(-10.f, 0.f);
         c.Restore(c)
         c.Invalidate();
      ));
      | Keys.D -> 
       this.Change(this)
       this.WV.TranslateW(10.f, 0.f);
       this.Restore(this)
       this.Invalidate();
       pilemembers.ForEach( fun c ->
       if( not c.IsSelected) then (
         c.Change(c)
         c.WV.TranslateW(10.f, 0.f);
         c.Restore(c)
         c.Invalidate();
      ));
      | Keys.R -> 
       this.WV.RotateV(10.f, this.WV.TransformPointW(PointF((this.Width / 2.f), (this.Height / 2.f)))); this.Rotated <- this.Rotated + 10; this.Invalidate();
       pilemembers.ForEach( 
         fun c -> (  if( not c.IsSelected) then 
                      c.WV.RotateV(10.f, this.WV.TransformPointW(PointF((this.Width / 2.f), (this.Height / 2.f)))); c.Rotated <- c.Rotated + 10; this.Invalidate(); ))
      | Keys.E -> 
       this.WV.RotateV(-10.f, this.WV.TransformPointW(PointF((this.Width / 2.f), (this.Height / 2.f)))); this.Rotated <- this.Rotated - 10; this.Invalidate();
       pilemembers.ForEach( 
         fun c -> (  if( not c.IsSelected) then 
                      c.WV.RotateV(-10.f, this.WV.TransformPointW(PointF((this.Width / 2.f), (this.Height / 2.f)))); c.Rotated <- c.Rotated - 10; this.Invalidate(); ))
      | Keys.Z ->
       this.WV.TranslateW( this.Width /2.f, this.Height/2.f);
       this.WV.ScaleW(1.1f, 1.1f); this.Scaled <- this.Scaled + 1; this.Invalidate();
       this.WV.TranslateW( -this.Width /2.f, -this.Height/2.f);
       pilemembers.ForEach( 
         fun c -> (  if( not c.IsSelected) then(
                      c.WV.TranslateW( this.Width /2.f, this.Height/2.f);
                      c.WV.ScaleW(1.1f, 1.1f); c.Scaled <- c.Scaled + 1; c.Invalidate();
                      c.WV.TranslateW( -this.Width /2.f, -this.Height/2.f);)))

      | Keys.X ->
       this.WV.TranslateW( this.Width /2.f, this.Height/2.f);
       this.WV.ScaleW(1.f/1.1f, 1.f/1.1f); this.Scaled <- this.Scaled - 1; this.Invalidate()//this.Height<- this.Height*(1.f/1.1f);this.Width<- this.Width*(1.f/1.1f);this.Invalidate();
       this.WV.TranslateW( -this.Width /2.f, -this.Height/2.f);
       pilemembers.ForEach( 
         fun c -> ( if( not c.IsSelected) then(
                     c.WV.TranslateW( this.Width /2.f, this.Height/2.f);
                     c.WV.ScaleW(1.f/1.1f, 1.f/1.1f); c.Scaled <- c.Scaled - 1; this.Invalidate() //c.Height<- c.Height*(1.f/1.1f);c.Width<- c.Width*(1.f/1.1f);c.Invalidate();
                     c.WV.TranslateW( -this.Width /2.f, -this.Height/2.f);)))
      | _ -> ()
  
    override this.OnMouseUp(e) =
      this.IsMoving <- false
  
type Impile(w, h) as this =
  inherit LWCControl(w, h)
  let mutable cont = new LWCContainer()
  do
    this.Width <- w
    this.Height <- h
  member this.Container
    with get() = cont
    and set( c ) = cont <- c
  
  override this.OnPaint(e) =
    let g = e.Graphics
    if(this.Impiling) then g.FillRectangle(Brushes.Green, 0.f, 0.f, w, h) else g.FillRectangle(Brushes.Red, 0.f, 0.f, w, h)
    g.DrawString("IMPILE", new Font(FontFamily.GenericSansSerif, 10.f ), Brushes.Black, PointF(5.f, 20.f))
  
  override this.OnMouseDown( e ) =
    //impila tutte le immagini e le pile selezionate in una specifica locazione
    let toadd = ResizeArray<LWCControl>()
    let toremove = ResizeArray<LWCControl>()
    let mutable pilesize = PointF(0.f,0.f)
    cont.LWControls |> Seq.forall(
      fun c -> ( if(c.IsSelected && not c.Piled) 
                 then ( match c.Arr with
                        | Some p -> //aggiunge elementi già presenti in una pila
                          p |> Seq.forall( fun c -> 
                                           toadd.Add(c); 
                                           if( pilesize.Y < c.Height + (float32 (toadd.Count * 5) )) then (pilesize.Y <-  c.Height + float32(toadd.Count*5));
                                           if( pilesize.X < c.Width + (float32 (toadd.Count * 5) )) then (pilesize.X <-  c.Width + float32(toadd.Count*5)); 
                                           true; ) |>ignore; p.Clear(); c.IsSelected<-false; c.IsMoving <- false; toremove.Add(c); c.Piled <- true; true;
                        | None ->  //immagine singola
                          c.Piled <- true; c.IsSelected <-false; c.IsMoving <- false; toadd.Add(c);
                          if( pilesize.Y < c.Height + (float32 (toadd.Count * 5) )) then (pilesize.Y <-  c.Height + float32(toadd.Count*5));
                          if( pilesize.X < c.Width + (float32 (toadd.Count * 5) )) then (pilesize.X <-  c.Width + float32(toadd.Count*5)); true; else true;) 
               else (true);)
    ) |> ignore
    toremove |> Seq.forall( fun c -> cont.LWControls.Remove(c) |> ignore; true;) |> ignore
    if(toadd.Count > 0) then
      let pp = Pile(pilesize.X, pilesize.Y, toadd, this.Container);
      pp.Impile() |> ignore

type ControViewButton(w, h, t ) as this =
  inherit LWCControl(w, h)
  let mutable cont = new LWCContainer()
  do
    this.Width <- w
    this.Height <- h
  member this.Container
    with get() = cont
    and set( c ) = cont <- c
  
  override this.OnPaint(e) =
    let g = e.Graphics
    g.FillRectangle(Brushes.Red, 0.f, 0.f, w, h)
    g.DrawString(t, new Font(FontFamily.GenericSansSerif, 10.f ), Brushes.Black, PointF(5.f, 20.f))
    
  override this.OnMouseDown( e ) =
    let mutable key = Keys.Alt;
    match t with
     | "UP" -> key <- Keys.W;
     | "DOWN" -> key <- Keys.S;
     | "LEFT" -> key <- Keys.A;
     | "RIGHT" -> key <- Keys.D;
     | "R R" -> key <- Keys.R;
     | "R L" -> key <- Keys.E;
     | "S U" -> key <- Keys.Z;
     | "S D" -> key <- Keys.X;
     | _ -> ();

    cont.LWControls |> Seq.forall(fun c ->
        if( c.IsSelected ) then c.OnKeyDown(KeyEventArgs(key));
        true;
    ) |> ignore; 
    ();

let lwcc = new LWCContainer(Dock=DockStyle.Fill)
let f = new Form( Text="Mid")
f.WindowState <- FormWindowState.Maximized
f.Controls.Add(lwcc)
f.Show()
lwcc.LWControls.Add(ControViewButton(w = 50.f, h = 50.f, t = "UP", Position = PointF(250.f, 10.f), Container = lwcc))
lwcc.LWControls.Add(ControViewButton(w = 50.f, h = 50.f, t = "DOWN", Position = PointF(250.f, 110.f), Container = lwcc))
lwcc.LWControls.Add(ControViewButton(w = 50.f, h = 50.f, t = "LEFT", Position = PointF(200.f, 60.f), Container = lwcc))
lwcc.LWControls.Add(ControViewButton(w = 50.f, h = 50.f, t = "RIGHT", Position = PointF(300.f, 60.f), Container = lwcc))
lwcc.LWControls.Add(ControViewButton(w = 50.f, h = 50.f, t = "R R", Position = PointF(200.f, 10.f), Container = lwcc))
lwcc.LWControls.Add(ControViewButton(w = 50.f, h = 50.f, t = "R L", Position = PointF(200.f, 110.f), Container = lwcc))
lwcc.LWControls.Add(ControViewButton(w = 50.f, h = 50.f, t = "S U", Position = PointF(300.f, 10.f), Container = lwcc))
lwcc.LWControls.Add(ControViewButton(w = 50.f, h = 50.f, t = "S D", Position = PointF(300.f, 110.f), Container = lwcc))
lwcc.LWControls.Add(Impile( w = 50.f, h = 50.f, Position = PointF(100.f, 10.f), Container = lwcc ))
lwcc.LWControls.Add(DrawOnImageButton( w = 50.f, h = 50.f, Position = PointF(250.f, 60.f),Container = lwcc ))
lwcc.LWControls.Add(LWButton(w = 50.f, h = 50.f, Position=PointF(20.f, 10.f), Container = lwcc))



