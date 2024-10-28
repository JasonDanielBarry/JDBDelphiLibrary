unit Graphic2DTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, system.UITypes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  CustomComponentPanelClass, Graphic2DComponent, SkiaDrawingClass,
  GeometryTypes,
  GeomLineClass, GeomPolyLineClass, GeomPolygonClass;

type
  TForm1 = class(TForm)
    JDBGraphic2D1: TJDBGraphic2D;
    procedure JDBGraphic2D1UpdateGeometry(  ASender         : TObject;
                                            var ASkiaDrawer : TSkiaGeomDrawer   );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.JDBGraphic2D1UpdateGeometry(   ASender         : TObject;
                                                var ASkiaDrawer : TSkiaGeomDrawer);
var
    i               : integer;
    x, y            : double;
    point1, point2  : TGeomPoint;
    line            : TGeomLine;
    polyline        : TGeomPolyLine;
    polygon         : TGeomPolygon;
begin
    //line 1
         point1 := TGeomPoint.create(10, 10);
         point2 := TGeomPoint.create(100, 100);

         line := TGeomLine.create(point1, point2);

         ASkiaDrawer.addLine(line, 2, TAlphaColors.Black);

    //polyline
        polyline := TGeomPolyLine.create();

        const NUM_POINTS : integer = 600;

        for i := 0 to NUM_POINTS do
            begin
                x := 10 + ((100 - 10) / NUM_POINTS) * i;
                y := 5 * sin(x - 10) + x;

                polyline.addVertex(x, y);
            end;

        ASkiaDrawer.addPolyline(polyline, 3, TAlphaColors.Deepskyblue);

    //box
        polygon := TGeomPolygon.create();

        polygon.addVertex(10, 10);
        polygon.addVertex(100, 10);
        polygon.addVertex(100, 100);
        polygon.addVertex(10, 100);

        ASkiaDrawer.addPolygon( polygon, 3, TAlphaColors.Null, TAlphaColors.Darkred );
end;

end.
