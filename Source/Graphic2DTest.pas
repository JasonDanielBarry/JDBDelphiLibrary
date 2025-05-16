unit Graphic2DTest;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, system.UITypes, system.Math,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, vcl.Styles, vcl.Themes,
  CustomComponentPanelClass, Graphic2DComponent, GraphicDrawingTypes,
  GraphicsListClass, GraphicArrowClass,
  GeometryTypes,
  GeomLineClass, GeomPolyLineClass, GeomPolygonClass, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    JDBGraphic2D1: TJDBGraphic2D;
    PanelTop: TPanel;
    ComboBox1: TComboBox;
    LabelSelectGraphic: TLabel;
    procedure JDBGraphic2D1UpdateGraphics(  ASender             : TObject;
                                            var AGraphicsList   : TGraphicsList );
    procedure FormShow(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    //different graphics
        procedure BlueBoxGraphic(var graphicsListInOut : TGraphicsList);
        procedure XYGraphs(var graphicsListInOut : TGraphicsList);
        procedure FinPlateGraphic(var graphicsListInOut : TGraphicsList);
        procedure SoilNailWallGraphic(var graphicsListInOut : TGraphicsList);
        procedure BendingBeamSection(var graphicsListInOut : TGraphicsList);
  public
    { Public declarations }
        constructor create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

    //different graphics
        procedure TForm1.BlueBoxGraphic(var graphicsListInOut : TGraphicsList);
            var
                i               : integer;
                x, y            : double;
                point1, point2  : TGeomPoint;
                line            : TGeomLine;
                polyline        : TGeomPolyLine;
                polygon         : TGeomPolygon;
            begin
                //polygon
                    graphicsListInOut.setCurrentDrawingLayer('Polygon Layer');

                    polygon := TGeomPolygon.create();

                    polygon.addVertex(10, 10);
                    polygon.addVertex(50, 10);
                    polygon.addVertex(100, 40);
                    polygon.addVertex(100, 70);
                    polygon.addVertex(50, 100);
                    polygon.addVertex(10, 100);

                    polygon.rotate(45);

                    polygon.setCentrePoint(50, 100);

                    graphicsListInOut.addPolygon( polygon, True, 9, TColors.Aqua, TColors.Darkred, TPenStyle.psDashDot );

                    FreeAndNil( polygon );

                    graphicsListInOut.addText(50, 100, 'This is a polygon'#13'rotated 45'#176, True, 9, 45, taCenter, tlCenter);

                //line 1
                    graphicsListInOut.setCurrentDrawingLayer('Line Layer');

                     point1 := TGeomPoint.create(-5, -5);
                     point2 := TGeomPoint.create(115, 115);

                     line := TGeomLine.create(point1, point2);

                     graphicsListInOut.addLine(line, 4, TColors.Black);

                    graphicsListInOut.addText(115, 115, 'This is a line');

                //polyline
                    graphicsListInOut.setCurrentDrawingLayer('Polyline Layer');

                    polyline := TGeomPolyLine.create();

                    const NUM_POINTS : integer = 3000;

                    for i := 0 to NUM_POINTS do
                        begin
                            x := -100 + ((300 - 10) / NUM_POINTS) * i;
                            y := 5 * sin(x - 10);

                            polyline.addVertex(x, y);
                        end;

                    polyline.setCentrePoint( line.calculateCentrePoint() );

                    polyline.rotate(30);

                    polyline.scale( 1.25 );

                    graphicsListInOut.addPolyline(polyline, 3, TColors.Blue);

                    FreeAndNil( line );

                    graphicsListInOut.addText( polyline.boundingBox().xMax, polyline.boundingBox().yMax, 'This is a polyline'#13'rotated 30'#176 );

                    FreeAndNil( polyline );

                //rectangle
                    graphicsListInOut.setCurrentDrawingLayer('Rectangle');

                    graphicsListInOut.addRectangle( 50, 75, 250, 10, True, 4, 10, TColors.Burlywood, TColors.Darkblue, TPenStyle.psDash );

                    graphicsListInOut.addText(250, 10, 'This is a round rectangle');

                //ellipse
                    graphicsListInOut.setCurrentDrawingLayer('Ellipse');

                    graphicsListInOut.addEllipse(75, 50, -100, 50, True, 6, Tcolors.Lightseagreen);

                    graphicsListInOut.addText(-100, 50, 'This is an ellipse');

                //text
                    graphicsListInOut.setCurrentDrawingLayer('Text Layer 1');

                    graphicsListInOut.addText(0, -30, 'This is a short'#13'sentence of'#13'4 lines'#13'at (0, -30)');

                    graphicsListInOut.addText(100, -30, 'This is a short'#13'sentence of'#13'4 lines'#13'at (100, -30)', False, 9, 0, taCenter, tlCenter);

                    graphicsListInOut.addText(200, -30, 'This is a short'#13'sentence of'#13'4 lines'#13'at (200, -30)', False, 9, 0, taRightJustify, tlBottom);

                //vertical boundary test
                    graphicsListInOut.setCurrentDrawingLayer('Text Layer 2');

                    graphicsListInOut.addText( 150, 200,
                                                'This is a short'#13'sentence of'#13'3 lines',
                                                False,
                                                18,
                                                35,
                                                TAlignment.taLeftJustify,
                                                TTextLayout.tlTop,
                                                Tcolors.Darkred,
                                                [TFontStyle.fsBold, TFontStyle.fsItalic, TFontStyle.fsUnderline] );

                    graphicsListInOut.addText(150, -50, 'This is a short'#13'sentence of'#13'3 lines');

                //arrow
                    graphicsListInOut.setCurrentDrawingLayer('Arrow Layer');

                    var arrowOriginPoint : TGeomPoint := TGeomPoint.create( -50, 150 );

                    for var angle : double in [0, 30, 60, 90, 120, 150, 180] do
                        graphicsListInOut.addArrow( 25, angle, arrowOriginPoint );

                //arrow group
                    graphicsListInOut.setCurrentDrawingLayer('Arrow Group Layer');

                    polyline := TGeomPolyLine.create();

                    polyline.addVertex( -200, 0 );
                    polyline.addVertex( -200, 100 );
                    polyline.addVertex( -100, 200 );
                    polyline.addVertex( 0, 200 );

                    graphicsListInOut.addArrowGroup( 25, polyline, EArrowOrigin.aoHead );

                    FreeAndNil( polyline );

                //arc
                    graphicsListInOut.setCurrentDrawingLayer('Arc Layer');

                    graphicsListInOut.addArc( -100, -125, 25, 25, 90, -90, True, 5, TColors.Red );

                    graphicsListInOut.addArc( 0, -125, 20, 20, 45, 360-45, True, 5, TColors.Yellow );
                    graphicsListInOut.addEllipse( 8, 8, 0, -114, True, 1, tcolors.Black );


                    graphicsListInOut.addArc( 100, -125, 50, 50, 0, -90, False, 5 );
                    graphicsListInOut.addArc( 100, -125, 50, 50, 350, -10 );
            end;

        procedure TForm1.XYGraphs(var graphicsListInOut : TGraphicsList);
            const
                X_MAX = 500;
                Y_MAX = 250;
            var
                x, y        : double;
                polyLine    : TGeomPolyLine;
            begin
                graphicsListInOut.setCurrentDrawingLayer('XY - Axes');

                //x-axis
                    graphicsListInOut.addText( X_MAX, 10, 'X', False, 15 );

                    graphicsListInOut.addArrow( X_MAX, 0, TGeomPoint.create(0, 0) );

                //y-axis
                    graphicsListInOut.addText( 10, Y_MAX, 'Y', False, 15 );

                    graphicsListInOut.addArrow( Y_MAX, 90, TGeomPoint.create(0, 0) );

                //quadratic curve
                    graphicsListInOut.setCurrentDrawingLayer('Quadratic curve');

                    polyLine := TGeomPolyLine.create();

                    x := 0;
                    y := 0;

                    while (x < X_MAX) do
                        begin
                            y := ( 250 / power(500, 2) ) * power(x, 2);

                            polyLine.addVertex(x, y);

                            x := x + 0.5;
                        end;

                    graphicsListInOut.addPolyline(polyLine, 3, TColors.Blueviolet);

                    graphicsListInOut.addText( x, y, 'y = x'#178 );

                    FreeAndNil( polyLine );

                //Trig curve
                    graphicsListInOut.setCurrentDrawingLayer('Trig curve');

                    polyLine := TGeomPolyLine.create();

                    x := 0;

                    while (x < X_MAX) do
                        begin
                            y := ( 250 / power(500, 2) ) * power(x, 2) + 15 * sin(x / 5);

                            polyLine.addVertex(x, y);

                            x := x + 0.2;
                        end;

                    graphicsListInOut.addPolyline(polyLine, 3, TColors.Green);

                    graphicsListInOut.addText( x, y, 'y = sin(x) + x'#178 );

                    FreeAndNil( polyLine );
            end;

    procedure TForm1.FinPlateGraphic(var graphicsListInOut : TGraphicsList);
        var
            i, j    : integer;
            line    : TGeomLine;
            polygon : TGeomPolygon;
        function
            _creatBoltPolygon(const centreX, centreY : double) : TGeomPolygon;
                var
                    r, h        : double;
                    polygonOut  : TGeomPolygon;
                begin
                    r := 10;
                    h := (2/sqrt(3)) * r;

                    polygonOut := TGeomPolygon.create();

                    polygonOut.addVertex( h, 0 );
                    polygonOut.addVertex( h/2, r );
                    polygonOut.addVertex( -h/2, r );
                    polygonOut.addVertex( -h, 0 );
                    polygonOut.addVertex( -h/2, -r );
                    polygonOut.addVertex( h/2, -r );

                    polygonOut.shift( centreX, centreY );

                    result := polygonOut;
                end;
        begin
            //members
                //beam
                    graphicsListInOut.setCurrentDrawingLayer('Beam');

                    graphicsListInOut.addRectangle( 300, 500, 250 + 50, 300, True, 1, 0, TColors.Lightgreen );

                    //flanges
                        //bottom
                            line := TGeomLine.create();
                            line.setStartPoint(0, 15);
                            line.setEndPoint(300, 15);

                            line.shift(250 + 50, 300);

                            graphicsListInOut.addLine( line, 1 );

                        //top
                            line := TGeomLine.create();
                            line.setStartPoint(0, 500 - 15);
                            line.setEndPoint(300, 500 - 15);

                            line.shift(250 + 50, 300);

                            graphicsListInOut.addLine( line, 1 );

                            FreeAndNil( line );

                //column
                    graphicsListInOut.setCurrentDrawingLayer('Column');

                    graphicsListInOut.addRectangle( 250, 1000, 0, 0, True, 1, 0, TColors.Cornflowerblue );

                    //flanges
                        //left
                            line := TGeomLine.create();
                            line.setStartPoint(15, 0);
                            line.setEndPoint(15, 1000);

                            graphicsListInOut.addLine( line, 1 );

                            FreeAndNil( line );

                        //right
                            line := TGeomLine.create();
                            line.setStartPoint(250 - 15, 0);
                            line.setEndPoint(250 - 15, 1000);

                            graphicsListInOut.addLine( line, 1 );

                            FreeAndNil( line );

                //plate
                    graphicsListInOut.setCurrentDrawingLayer('Plate');

                    graphicsListInOut.addRectangle( 250, 350, 250, 400, True, 1, 0, TColors.Yellow );

            //weld
                graphicsListInOut.setCurrentDrawingLayer('Weld');

                polygon := TGeomPolygon.create();

                polygon.addVertex(0, -8);
                polygon.addVertex(8, 0);
                polygon.addVertex(8, 350);
                polygon.addVertex(0, 350 + 8);

                polygon.shift(250, 400);

                graphicsListInOut.addPolygon( polygon, True, 1, TColors.Blue, TColors.Black );

                FreeAndNil( polygon );

            //bolts
                graphicsListInOut.setCurrentDrawingLayer('Bolts');

                for i := 0 to 5 do
                    for j := 0 to 2 do
                        begin
                            var centreX, centreY : double;

                            centreX := 100 + 50 * j;
                            centreY := 50 + i * 50;

                            polygon := _creatBoltPolygon( centreX, centreY );

                            polygon.shift(250, 400);

                            graphicsListInOut.addPolygon( polygon, True, 2, TColors.Lightseagreen, TColors.Black );

                            FreeAndNil( polygon );

                            graphicsListInOut.addEllipse(14, 14, centreX + 250, centreY + 400, False, 1);
                        end;
        end;

    procedure TForm1.SoilNailWallGraphic(var graphicsListInOut: TGraphicsList);
        var
            line    : TGeomLine;
            polygon : TGeomPolygon;
        begin
            graphicsListInOut.setCurrentDrawingLayer('Soil');

            polygon := TGeomPolygon.create();

            polygon.addVertex(-2, -3);
            polygon.addVertex(30, -3);
            polygon.addVertex(30, 15);
            polygon.addVertex(0, 15);
            polygon.addVertex(0, 0);
            polygon.addVertex(-2, 0);

            graphicsListInOut.addPolygon( polygon, True, 2, TColors.Lightgreen );

            FreeAndNil( polygon );

            graphicsListInOut.setCurrentDrawingLayer('Failure Wedge');

            polygon := TGeomPolygon.create();

            polygon.addVertex(0, 0);
            polygon.addVertex(20, 15);
            polygon.addVertex(0, 15);

            graphicsListInOut.addPolygon( polygon, True, 2, TColors.Orangered );

            FreeAndNil( polygon );



            graphicsListInOut.setCurrentDrawingLayer('Load');

            line := TGeomLine.create();

            line.setStartPoint(0, 15.15);
            line.setEndPoint(20, 15.15);

            graphicsListInOut.addArrowGroup( 1.5, line, EArrowOrigin.aoHead );

            FreeAndNil( line );



            graphicsListInOut.setCurrentDrawingLayer('Soil Nails');

            begin
                var y : double;

                y := 15 - 1.0;

                line := TGeomLine.create();

                line.setStartPoint(0, 0);
                line.setEndPoint(20, 0);

                line.shift(0, y);
                line.rotate( -10, line.getStartPoint() );

                while (y > 1.0) do
                    begin
                        graphicsListInOut.addLine( line, 16, TColors.Grey);
                        graphicsListInOut.addLine( line, 4, TColors.Darkblue );

                        line.shift(0, -1.5);

                        y := y - 1.5;
                    end;

                FreeAndNil( line );
            end;

            graphicsListInOut.setCurrentDrawingLayer('Wall');

            polygon := TGeomPolygon.create();

            polygon.addVertex(0, 0);
            polygon.addVertex(0, 15);
            polygon.addVertex(-0.35, 15);
            polygon.addVertex(-0.35, 0);

            graphicsListInOut.addPolygon( polygon, True, 2, TColors.Yellow );

            FreeAndNil( polygon );
        end;

    procedure TForm1.BendingBeamSection(var graphicsListInOut : TGraphicsList);
        var
            line : TGeomLine;
        begin
            //concrete
                graphicsListInOut.setCurrentDrawingLayer('Concrete');

                graphicsListInOut.addRectangle( 450, 450, -450, 0, True, 2, 0, TColors.Lightgreen );

            //rebar
                graphicsListInOut.setCurrentDrawingLayer('Rebar');

                graphicsListInOut.addRectangle( 500, 16, -450, 75-8, True, 2, 0, TColors.Dodgerblue );

            //compressing stress
                graphicsListInOut.setCurrentDrawingLayer('Compression Stress');

                line := TGeomLine.create();

                line.setStartPoint( 5, 450 );
                line.setEndPoint( 5, 275 );

                graphicsListInOut.addArrowGroup( 33, line, EArrowOrigin.aoHead );

                FreeAndNil( line );

            //rebar force
                graphicsListInOut.setCurrentDrawingLayer('Tension Force');

                graphicsListInOut.addArrow( 200, 0, TGeomPoint.create( 55, 75 ) );
        end;

    constructor TForm1.create(AOwner: TComponent);
        begin
            inherited create(nil);

            ComboBox1.ItemIndex := 0;

//            TStyleManager.SetStyle('Windows11 Modern Dark');
        end;

    procedure TForm1.ComboBox1Change(Sender: TObject);
        begin
            self.LockDrawing();

            JDBGraphic2D1.updateGraphics();
            JDBGraphic2D1.zoomAll();

            self.Refresh();

            self.UnlockDrawing();
        end;

    procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
        begin
            Action := TCloseAction.caFree;
        end;

    procedure TForm1.FormShow(Sender: TObject);
        begin
            JDBGraphic2D1.updateGraphics();
            JDBGraphic2D1.zoomAll();
        end;

    procedure TForm1.JDBGraphic2D1UpdateGraphics(   ASender             : TObject;
                                                    var AGraphicsList   : TGraphicsList );
        begin
            case (ComboBox1.ItemIndex) of
                0:
                    BlueBoxGraphic( AGraphicsList );
                1:
                    XYGraphs( AGraphicsList );
                2:
                    FinPlateGraphic( AGraphicsList );
                3:
                    SoilNailWallGraphic( AGraphicsList );
                4:
                    BendingBeamSection( AGraphicsList );
            end;
        end;

end.

