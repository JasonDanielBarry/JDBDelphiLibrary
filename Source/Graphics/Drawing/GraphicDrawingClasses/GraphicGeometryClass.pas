unit GraphicGeometryClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes, System.UIConsts,
            Winapi.D2D1, Vcl.Direct2D,
            vcl.Graphics,
        //custom
            GraphicObjectBaseClass,
            DrawingAxisConversionClass,
            GeometryTypes,
            GeometryBaseClass
            ;

    type
        TGraphicGeometry = class(TGraphicObject)
            strict private
                //set the geometry object
                    procedure setGeometry(const geometryIn : TGeomBase);
            protected
                var
                    geometryPoints : TArray<TGeomPoint>;
                //drawing helper methods
                    function createPathGeometry(const figureBeginIn     : D2D1_FIGURE_BEGIN;
                                                const figureEndIn       : D2D1_FIGURE_END;
                                                const axisConverterIn   : TDrawingAxisConverter) : ID2D1PathGeometry;
            public
                //constructor
                    constructor create( const   lineThicknessIn : integer;
                                        const   fillColourIn,
                                                lineColourIn    : TColor;
                                        const   lineStyleIn     : TPenStyle;
                                        const   geometryIn      : TGeomBase ); overload;
                    constructor create( const   lineThicknessIn : integer;
                                        const   lineColourIn    : TColor;
                                        const   lineStyleIn     : TPenStyle;
                                        const   geometryIn      : TGeomBase ); overload;
                //destructor
                    destructor destroy(); override;
        end;

implementation

    //private
        //set the geometry object
            procedure TGraphicGeometry.setGeometry(const geometryIn : TGeomBase);
                begin
                    //get the drawing points
                        geometryPoints := geometryIn.getDrawingPoints();

                    //free the incoming object
                        FreeAndNil( geometryIn );
                end;

    //protected
        //drawing helper methods
            function TGraphicGeometry.createPathGeometry(   const figureBeginIn     : D2D1_FIGURE_BEGIN;
                                                            const figureEndIn       : D2D1_FIGURE_END;
                                                            const axisConverterIn   : TDrawingAxisConverter ) : ID2D1PathGeometry;
                var
                    geometrySink    : ID2D1GeometrySink;
                    pathGeometryOut : ID2D1PathGeometry;
                    drawingPoints   : TArray<TPointF>;
                begin
                    //convert geometry into canvas drawing points
                        drawingPoints := axisConverterIn.arrXY_to_arrLT(
                                                                            geometryPoints
                                                                       );

                    //create path geometry
                        D2DFactory( D2D1_FACTORY_TYPE.D2D1_FACTORY_TYPE_MULTI_THREADED ).CreatePathGeometry( pathGeometryOut );

                    //open path geometry
                        pathGeometryOut.Open( geometrySink );

                    //create geometry sink
                        geometrySink.BeginFigure( D2D1PointF( drawingPoints[0].x, drawingPoints[0].y ), figureBeginIn );

                        //add lines
                            //single line
                                if ( length(drawingPoints) < 3 ) then
                                    geometrySink.AddLine( D2D1PointF( drawingPoints[1].x, drawingPoints[1].y ) )
                            //polyline
                                else
                                    begin
                                        var i : integer;

                                        for i := 1 to ( length(drawingPoints) - 1 ) do
                                            geometrySink.AddLine( D2D1PointF( drawingPoints[i].x, drawingPoints[i].y ) );
                                    end;

                        geometrySink.EndFigure( figureEndIn );

                        geometrySink.Close();

                    result := pathGeometryOut;
                end;

    //public
        //constructor
            constructor TGraphicGeometry.create(const   lineThicknessIn : integer;
                                                const   fillColourIn,
                                                        lineColourIn    : TColor;
                                                const   lineStyleIn     : TPenStyle;
                                                const   geometryIn      : TGeomBase );
                begin
                    inherited create(   lineThicknessIn,
                                        geometryIn.getDrawingType(),
                                        fillColourIn,
                                        lineColourIn,
                                        lineStyleIn                 );

                    setGeometry( geometryIn );
                end;

            constructor TGraphicGeometry.create(const   lineThicknessIn : integer;
                                                const   lineColourIn    : TColor;
                                                const   lineStyleIn     : TPenStyle;
                                                const   geometryIn      : TGeomBase );
                begin
                    create( lineThicknessIn,
                            TColors.Null,
                            lineColourIn,
                            lineStyleIn,
                            geometryIn          );
                end;

        //destructor
            destructor TGraphicGeometry.destroy();
                begin
                    inherited destroy();
                end;


end.
