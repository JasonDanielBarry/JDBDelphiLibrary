unit Graphic2DTest;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, system.UITypes, system.Math,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, vcl.Styles, vcl.Themes,
  CustomComponentPanelClass, Graphic2DComponent, GeomDrawerBaseClass,
  GeometryTypes,
  GeomLineClass, GeomPolyLineClass, GeomPolygonClass, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    JDBGraphic2D1: TJDBGraphic2D;
    PanelTop: TPanel;
    ComboBox1: TComboBox;
    LabelSelectGraphic: TLabel;
    procedure JDBGraphic2D1UpdateGeometry(  ASender         : TObject;
                                            var AGeomDrawer : TGeomDrawer   );
    procedure FormShow(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    //different graphics
        procedure BlueBoxGraphic(var GeomDrawerInOut : TGeomDrawer);
        procedure XYGraphs(var GeomDrawerInOut : TGeomDrawer);
  public
    { Public declarations }
        constructor create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

    //different graphics
        procedure TForm1.BlueBoxGraphic(var GeomDrawerInOut : TGeomDrawer);
            var
                i               : integer;
                x, y            : double;
                point1, point2  : TGeomPoint;
                line            : TGeomLine;
                polyline        : TGeomPolyLine;
                polygon         : TGeomPolygon;
            begin
                //box
                    GeomDrawerInOut.setCurrentDrawingLayer('Polygon Layer');

                    polygon := TGeomPolygon.create();

                    polygon.addVertex(10, 10);
                    polygon.addVertex(100, 10);
                    polygon.addVertex(100, 100);
                    polygon.addVertex(10, 100);

                    GeomDrawerInOut.addPolygon( polygon, 10, TAlphaColors.Aqua, TAlphaColors.Darkred );

                //line 1
                    GeomDrawerInOut.setCurrentDrawingLayer('Line Layer');

                     point1 := TGeomPoint.create(-5, -5);
                     point2 := TGeomPoint.create(115, 115);

                     line := TGeomLine.create(point1, point2);

                     GeomDrawerInOut.addLine(line, 2, TAlphaColors.Black);

                //polyline
                    GeomDrawerInOut.setCurrentDrawingLayer('Polyline Layer');

                    polyline := TGeomPolyLine.create();

                    const NUM_POINTS : integer = 1500;

                    for i := 0 to NUM_POINTS do
                        begin
                            x := -50 + ((200 - 10) / NUM_POINTS) * i;
                            y := 5 * sin(x - 10) + x;

                            polyline.addVertex(x, y);
                        end;

                    GeomDrawerInOut.addPolyline(polyline, 3, TAlphaColors.Blue);
            end;

        procedure TForm1.XYGraphs(var GeomDrawerInOut : TGeomDrawer);
            const
                X_MAX = 500;
                Y_MAX = 250;
            var
                x, y        : double;
                line        : TGeomLine;
                polyLine    : TGeomPolyLine;
            begin
                GeomDrawerInOut.setCurrentDrawingLayer('XY - Axes');

                //x-axis
                    line := TGeomLine.create(
                                                TGeomPoint.create(0, 0), TGeomPoint.create(X_MAX, 0)
                                            );

                    GeomDrawerInOut.addLine(line);

                //y-axis
                    line := TGeomLine.create(
                                                TGeomPoint.create(0, 0), TGeomPoint.create(0, Y_MAX)
                                            );

                    GeomDrawerInOut.addLine(line);

                //quadratic curve
                    GeomDrawerInOut.setCurrentDrawingLayer('Quadratic curve');

                    polyLine := TGeomPolyLine.create();

                    x := 0;

                    while (x < X_MAX) do
                        begin
                            y := ( 250 / power(500, 2) ) * power(x, 2);

                            polyLine.addVertex(x, y);

                            x := x + 0.5;
                        end;

                    GeomDrawerInOut.addPolyline(polyLine, 3, TAlphaColors.Blueviolet);

                //Trig curve
                    GeomDrawerInOut.setCurrentDrawingLayer('Trig curve');

                    polyLine := TGeomPolyLine.create();

                    x := 0;

                    while (x < X_MAX) do
                        begin
                            y := ( 250 / power(500, 2) ) * power(x, 2) + 15 * sin(x / 5);

                            polyLine.addVertex(x, y);

                            x := x + 0.25;
                        end;

                    GeomDrawerInOut.addPolyline(polyLine, 3, TAlphaColors.Green);
            end;

    constructor TForm1.create(AOwner: TComponent);
        begin
            inherited create(nil);

            ComboBox1.ItemIndex := 0;

    //        TStyleManager.SetStyle('Windows11 Modern Dark');
        end;

    procedure TForm1.ComboBox1Change(Sender: TObject);
        begin
            JDBGraphic2D1.updateGeometry();
            JDBGraphic2D1.zoomAll();
        end;

    procedure TForm1.FormShow(Sender: TObject);
        begin
            JDBGraphic2D1.updateGeometry();
            JDBGraphic2D1.zoomAll();
        end;

    procedure TForm1.JDBGraphic2D1UpdateGeometry(   ASender : TObject;
                                                    var AGeomDrawer : TGeomDrawer);
        begin
            case (ComboBox1.ItemIndex) of
                0:
                    BlueBoxGraphic( AGeomDrawer );
                1:
                    XYGraphs( AGeomDrawer );
            end;
        end;

end.
