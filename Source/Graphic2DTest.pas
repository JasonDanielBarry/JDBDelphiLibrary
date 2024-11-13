unit Graphic2DTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, system.UITypes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, vcl.Styles, vcl.Themes,
  CustomComponentPanelClass, Graphic2DComponent, SkiaDrawingClass,
  GeometryTypes,
  GeomLineClass, GeomPolyLineClass, GeomPolygonClass;

type
  TForm1 = class(TForm)
    JDBGraphic2D1: TJDBGraphic2D;
    procedure JDBGraphic2D1UpdateGeometry(  ASender         : TObject;
                                            var ASkiaDrawer : TSkiaGeomDrawer   );
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
        constructor create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

constructor TForm1.create(AOwner: TComponent);
    begin
        inherited create(nil);

//        TStyleManager.SetStyle('Windows11 Modern Dark');
    end;

procedure TForm1.FormShow(Sender: TObject);
begin
    JDBGraphic2D1.updateGeometry();
    JDBGraphic2D1.zoomAll();
    JDBGraphic2D1.redrawGraphic();

end;

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
//    canvas


    //box
        polygon := TGeomPolygon.create();

        polygon.addVertex(10, 10);
        polygon.addVertex(100, 10);
        polygon.addVertex(100, 100);
        polygon.addVertex(10, 100);

        ASkiaDrawer.addPolygon( polygon, 10, TAlphaColors.Aqua, TAlphaColors.Darkred );

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

        ASkiaDrawer.addPolyline(polyline, 3, TAlphaColors.Blue);
end;

end.
