unit GraphicLinePlotClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes, System.UIConsts,
            Winapi.D2D1, Vcl.Direct2D,
            vcl.Graphics,
        //custom
            GraphicDrawingTypes,
            DrawingAxisConversionClass,
            GeometryTypes,
            GeomBox,
            GeomPolyLineClass,
            GraphicObjectBaseClass,
            GraphicPolylineClass;

    type
        TGraphicLinePlot = class(TGraphicObject)
            private
                var
                    graphicPolyline : TGraphicPolyline;
            public
                //constructor
                    constructor create( const   lineThicknessIn : integer;
                                        const   lineColourIn    : TColor;
                                        const   lineStyleIn     : TPenStyle;
                                        const   arrPlotPointsIn : TArray<TGeomPoint>);
                //destructor
                    destructor destroy(); override;
                //draw to canvas
                    procedure drawToCanvas( const axisConverterIn   : TDrawingAxisConverter;
                                            var canvasInOut         : TDirect2DCanvas       ); override;
                //bounding box
                    function determineBoundingBox() : TGeomBox; override;
        end;

implementation

    //public
        //constructor
            constructor TGraphicLinePlot.create(const   lineThicknessIn : integer;
                                                const   lineColourIn    : TColor;
                                                const   lineStyleIn     : TPenStyle;
                                                const   arrPlotPointsIn : TArray<TGeomPoint>);
                var
                    tempPolyline : TGeomPolyLine;
                begin
                    inherited create();

                    tempPolyline := TGeomPolyLine.create( arrPlotPointsIn );

                    graphicPolyline := TGraphicPolyline.create( lineThicknessIn, lineColourIn, lineStyleIn, tempPolyline );

                    FreeAndNil( tempPolyline );
                end;

        //destructor
            destructor TGraphicLinePlot.destroy();
                begin
                    FreeAndNil( graphicPolyline );

                    inherited destroy();
                end;

        //draw to canvas
            procedure TGraphicLinePlot.drawToCanvas(const axisConverterIn   : TDrawingAxisConverter;
                                                    var canvasInOut         : TDirect2DCanvas       );
                begin
                    graphicPolyline.drawToCanvas( axisConverterIn, canvasInOut );
                end;

        //bounding box
            function TGraphicLinePlot.determineBoundingBox() : TGeomBox;
                begin
                    result := graphicPolyline.determineBoundingBox();
                end;


end.
