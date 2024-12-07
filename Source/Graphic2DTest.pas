unit Graphic2DTest;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, system.UITypes, system.Math,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, vcl.Styles, vcl.Themes,
  CustomComponentPanelClass, Graphic2DComponent, GraphicDrawerObjectAdderClass,
  GeometryTypes,
  GeomLineClass, GeomPolyLineClass, GeomPolygonClass, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    JDBGraphic2D1: TJDBGraphic2D;
    PanelTop: TPanel;
    ComboBox1: TComboBox;
    LabelSelectGraphic: TLabel;
    procedure JDBGraphic2D1UpdateGeometry(  ASender         : TObject;
                                            var AGeomDrawer : TGraphicDrawerObjectAdder );
    procedure FormShow(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    //different graphics
        procedure BlueBoxGraphic(var GraphicDrawerInOut : TGraphicDrawerObjectAdder);
        procedure XYGraphs(var GraphicDrawerInOut : TGraphicDrawerObjectAdder);
        procedure FinPlateGraphic(var GraphicDrawerInOut : TGraphicDrawerObjectAdder);
        procedure SoilNailWallGraphic(var GraphicDrawerInOut : TGraphicDrawerObjectAdder);
  public
    { Public declarations }
        constructor create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

    //different graphics
        procedure TForm1.BlueBoxGraphic(var GraphicDrawerInOut : TGraphicDrawerObjectAdder);
            var
                i               : integer;
                x, y            : double;
                point1, point2  : TGeomPoint;
                line            : TGeomLine;
                polyline        : TGeomPolyLine;
                polygon         : TGeomPolygon;
            begin
                //box
                    GraphicDrawerInOut.setCurrentDrawingLayer('Polygon Layer');

                    polygon := TGeomPolygon.create();

                    polygon.addVertex(10, 10);
                    polygon.addVertex(100, 10);
                    polygon.addVertex(100, 100);
                    polygon.addVertex(10, 100);

                    GraphicDrawerInOut.addPolygon( polygon, 9, TColors.Aqua, TColors.Darkred, TPenStyle.psDashDot );

                    FreeAndNil( polygon );

                    GraphicDrawerInOut.addText('This is a box', 50, 50);

                //line 1
                    GraphicDrawerInOut.setCurrentDrawingLayer('Line Layer');

                     point1 := TGeomPoint.create(-5, -5);
                     point2 := TGeomPoint.create(115, 115);

                     line := TGeomLine.create(point1, point2);

                     GraphicDrawerInOut.addLine(line, 4, TColors.Black);

                     FreeAndNil( line );

                    GraphicDrawerInOut.addText('This is a line', 115, 115);

                //polyline
                    GraphicDrawerInOut.setCurrentDrawingLayer('Polyline Layer');

                    polyline := TGeomPolyLine.create();

                    const NUM_POINTS : integer = 1500;

                    for i := 0 to NUM_POINTS do
                        begin
                            x := -50 + ((200 - 10) / NUM_POINTS) * i;
                            y := 5 * sin(x - 10) + x;

                            polyline.addVertex(x, y);
                        end;

                    GraphicDrawerInOut.addPolyline(polyline, 3, TColors.Blue);

                    FreeAndNil( polyline );

                    GraphicDrawerInOut.addText('This is a polyline', 140, 140);

                //text
                    GraphicDrawerInOut.setCurrentDrawingLayer('Text Layer');

                    GraphicDrawerInOut.addText('This is a short'#13'sentence of'#13'4 lines'#13'at (0, 0)', 0, 0);

                    GraphicDrawerInOut.addText('This is a short'#13'sentence of'#13'4 lines'#13'at (200, 10)', 200, 10);
            end;

        procedure TForm1.XYGraphs(var GraphicDrawerInOut : TGraphicDrawerObjectAdder);
            const
                X_MAX = 500;
                Y_MAX = 250;
            var
                x, y        : double;
                line        : TGeomLine;
                polyLine    : TGeomPolyLine;
            begin
                GraphicDrawerInOut.setCurrentDrawingLayer('XY - Axes');

                //x-axis
                    line := TGeomLine.create(
                                                TGeomPoint.create(0, 0), TGeomPoint.create(X_MAX, 0)
                                            );

                    GraphicDrawerInOut.addLine(line);

                    FreeAndNil( line );

                //y-axis
                    line := TGeomLine.create(
                                                TGeomPoint.create(0, 0), TGeomPoint.create(0, Y_MAX)
                                            );

                    GraphicDrawerInOut.addLine(line);

                    FreeAndNil( line );

                //quadratic curve
                    GraphicDrawerInOut.setCurrentDrawingLayer('Quadratic curve');

                    polyLine := TGeomPolyLine.create();

                    x := 0;

                    while (x < X_MAX) do
                        begin
                            y := ( 250 / power(500, 2) ) * power(x, 2);

                            polyLine.addVertex(x, y);

                            x := x + 0.5;
                        end;

                    GraphicDrawerInOut.addPolyline(polyLine, 3, TColors.Blueviolet);

                    FreeAndNil( polyLine );

                //Trig curve
                    GraphicDrawerInOut.setCurrentDrawingLayer('Trig curve');

                    polyLine := TGeomPolyLine.create();

                    x := 0;

                    while (x < X_MAX) do
                        begin
                            y := ( 250 / power(500, 2) ) * power(x, 2) + 15 * sin(x / 5);

                            polyLine.addVertex(x, y);

                            x := x + 0.25;
                        end;

                    GraphicDrawerInOut.addPolyline(polyLine, 3, TColors.Green);

                    FreeAndNil( polyLine );
            end;

    procedure TForm1.FinPlateGraphic(var GraphicDrawerInOut : TGraphicDrawerObjectAdder);
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
                    GraphicDrawerInOut.setCurrentDrawingLayer('Beam');

                    polygon := TGeomPolygon.create();

                    polygon.addVertex(0, 0);
                    polygon.addVertex(300, 0);
                    polygon.addVertex(300, 500);
                    polygon.addVertex(0, 500);

                    polygon.shift(250 + 50, 300);

                    GraphicDrawerInOut.addPolygon( polygon, 1, TColors.Lightgreen, TColors.Black );

                    FreeAndNil( polygon );

                    //flanges
                        //bottom
                            line := TGeomLine.create();
                            line.setStartPoint(0, 15);
                            line.setEndPoint(300, 15);

                            line.shift(250 + 50, 300);

                            GraphicDrawerInOut.addLine( line, 1 );

                        //top
                            line := TGeomLine.create();
                            line.setStartPoint(0, 500 - 15);
                            line.setEndPoint(300, 500 - 15);

                            line.shift(250 + 50, 300);

                            GraphicDrawerInOut.addLine( line, 1 );

                            FreeAndNil( line );

                //column
                    GraphicDrawerInOut.setCurrentDrawingLayer('Column');

                    polygon := TGeomPolygon.create();

                    polygon.addVertex(0, 0);
                    polygon.addVertex(250, 0);
                    polygon.addVertex(250, 1000);
                    polygon.addVertex(0, 1000);

                    GraphicDrawerInOut.addPolygon( polygon, 1, TColors.Cornflowerblue, TColors.Black );

                    FreeAndNil( polygon );

                    //flanges
                        //left
                            line := TGeomLine.create();
                            line.setStartPoint(15, 0);
                            line.setEndPoint(15, 1000);

                            GraphicDrawerInOut.addLine( line, 1 );

                            FreeAndNil( line );

                        //right
                            line := TGeomLine.create();
                            line.setStartPoint(250 - 15, 0);
                            line.setEndPoint(250 - 15, 1000);

                            GraphicDrawerInOut.addLine( line, 1 );

                            FreeAndNil( line );

                //plate
                    GraphicDrawerInOut.setCurrentDrawingLayer('Plate');

                    polygon := TGeomPolygon.create();

                    polygon.addVertex(0, 0);
                    polygon.addVertex(250, 0);
                    polygon.addVertex(250, 350);
                    polygon.addVertex(0, 350);

                    polygon.shift(250, 400);

                    GraphicDrawerInOut.addPolygon( polygon, 1, TColors.Yellow, TColors.Black );

                    FreeAndNil( polygon );

            //weld
                GraphicDrawerInOut.setCurrentDrawingLayer('Weld');

                polygon := TGeomPolygon.create();

                polygon.addVertex(0, -8);
                polygon.addVertex(8, 0);
                polygon.addVertex(8, 350);
                polygon.addVertex(0, 350 + 8);

                polygon.shift(250, 400);

                GraphicDrawerInOut.addPolygon( polygon, 1, TColors.Blue, TColors.Black );

                FreeAndNil( polygon );

            //bolts
                GraphicDrawerInOut.setCurrentDrawingLayer('Bolts');

                for i := 0 to 5 do
                    begin
                        for j := 0 to 2 do
                            begin
                                polygon := _creatBoltPolygon(100 + 50 * j, 50 + i * 50);

                                polygon.shift(250, 400);

                                GraphicDrawerInOut.addPolygon( polygon, 3, TColors.Lightslategrey, TColors.Black );

                                FreeAndNil( polygon );
                            end;
                    end;
        end;

    procedure TForm1.SoilNailWallGraphic(var GraphicDrawerInOut: TGraphicDrawerObjectAdder);
        var
            line    : TGeomLine;
            polygon : TGeomPolygon;
        begin
            GraphicDrawerInOut.setCurrentDrawingLayer('Soil');

            polygon := TGeomPolygon.create();

            polygon.addVertex(-2, -3);
            polygon.addVertex(30, -3);
            polygon.addVertex(30, 15);
            polygon.addVertex(0, 15);
            polygon.addVertex(0, 0);
            polygon.addVertex(-2, 0);

            GraphicDrawerInOut.addPolygon( polygon, 2, TColors.Lightgreen );

            FreeAndNil( polygon );

            GraphicDrawerInOut.setCurrentDrawingLayer('Failure Wedge');

            polygon := TGeomPolygon.create();

            polygon.addVertex(0, 0);
            polygon.addVertex(20, 15);
            polygon.addVertex(0, 15);

            GraphicDrawerInOut.addPolygon( polygon, 2, TColors.Orangered );

            FreeAndNil( polygon );



            GraphicDrawerInOut.setCurrentDrawingLayer('Soil Nails');

            begin
                var y : double;

                y := 15 - 1.0;

                while (y > 1.0) do
                    begin
                        line := TGeomLine.create();

                        line.setStartPoint(0, 0);
                        line.setEndPoint(20, -4);

                        line.shift(0, y);

                        GraphicDrawerInOut.addLine( line, 16, TColors.Grey);
                        GraphicDrawerInOut.addLine( line, 4, TColors.Darkblue );

                        FreeAndNil( line );


                        y := y - 1.5;
                    end;
            end;



            GraphicDrawerInOut.setCurrentDrawingLayer('Wall');

            polygon := TGeomPolygon.create();

            polygon.addVertex(0, 0);
            polygon.addVertex(0, 15);
            polygon.addVertex(-0.35, 15);
            polygon.addVertex(-0.35, 0);

            GraphicDrawerInOut.addPolygon( polygon, 2, TColors.Yellow );

            FreeAndNil( polygon );
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

            JDBGraphic2D1.updateGeometry();
            JDBGraphic2D1.zoomAll();

            self.Refresh();

            self.UnlockDrawing();
        end;

    procedure TForm1.FormShow(Sender: TObject);
        begin
            JDBGraphic2D1.updateGeometry();
            JDBGraphic2D1.zoomAll();
        end;

    procedure TForm1.JDBGraphic2D1UpdateGeometry(   ASender         : TObject;
                                                    var AGeomDrawer : TGraphicDrawerObjectAdder );
        begin
            case (ComboBox1.ItemIndex) of
                0:
                    BlueBoxGraphic( AGeomDrawer );
                1:
                    XYGraphs( AGeomDrawer );
                2:
                    FinPlateGraphic( AGeomDrawer );
                3:
                    SoilNailWallGraphic( AGeomDrawer );
            end;
        end;

end.
