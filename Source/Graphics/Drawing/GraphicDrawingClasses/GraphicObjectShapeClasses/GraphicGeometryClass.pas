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
            GeomBox,
            GeometryBaseClass
            ;

    type
        TGraphicGeometry = class(TGraphicObject)
            private
                //drawing helper methods
                    class function createPathGeometry(  const figureBeginIn     : D2D1_FIGURE_BEGIN;
                                                        const figureEndIn       : D2D1_FIGURE_END;
                                                        const geometryPointsIn  : TArray<TGeomPoint>;
                                                        const axisConverterIn   : TDrawingAxisConverter ) : ID2D1PathGeometry; static;
            protected
                var
                    geometryPoints : TArray<TGeomPoint>;
            public
                //constructor
                    constructor create( const   filledIn            : boolean;
                                        const   lineThicknessIn     : integer;
                                        const   fillColourIn,
                                                lineColourIn        : TColor;
                                        const   lineStyleIn         : TPenStyle;
                                        const   geometryPointsIn    : TArray<TGeomPoint> );
                //destructor
                    destructor destroy(); override;
                //create path geometry
                    //closed
                        class function createClosedPathGeometry(const geometryPointsIn  : TArray<TGeomPoint>;
                                                                const axisConverterIn   : TDrawingAxisConverter) : ID2D1PathGeometry; static;
                    //open
                        class function createOpenPathGeometry(  const geometryPointsIn  : TArray<TGeomPoint>;
                                                                const axisConverterIn   : TDrawingAxisConverter ) : ID2D1PathGeometry; static;
                //bounding box
                    function determineBoundingBox() : TGeomBox; override;
        end;

implementation

    //private
        //drawing helper methods
            class function TGraphicGeometry.createPathGeometry( const figureBeginIn     : D2D1_FIGURE_BEGIN;
                                                                const figureEndIn       : D2D1_FIGURE_END;
                                                                const geometryPointsIn  : TArray<TGeomPoint>;
                                                                const axisConverterIn   : TDrawingAxisConverter ) : ID2D1PathGeometry;
                var
                    geometrySink    : ID2D1GeometrySink;
                    pathGeometryOut : ID2D1PathGeometry;
                    drawingPoints   : TArray<TPointF>;
                begin
                    //convert geometry into canvas drawing points
                        drawingPoints := axisConverterIn.arrXY_to_arrLT(
                                                                            geometryPointsIn
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

                        //end geometry
                            geometrySink.EndFigure( figureEndIn );

                            geometrySink.Close();

                    result := pathGeometryOut;
                end;

    //public
        //constructor
            constructor TGraphicGeometry.create(const   filledIn            : boolean;
                                                const   lineThicknessIn     : integer;
                                                const   fillColourIn,
                                                        lineColourIn        : TColor;
                                                const   lineStyleIn         : TPenStyle;
                                                const   geometryPointsIn    : TArray<TGeomPoint>);
                begin
                    inherited create(   filledIn,
                                        lineThicknessIn,
                                        fillColourIn,
                                        lineColourIn,
                                        lineStyleIn         );

                    TGeomPoint.copyPoints( geometryPointsIn, geometryPoints );
                end;

        //destructor
            destructor TGraphicGeometry.destroy();
                begin
                    inherited destroy();
                end;

        //create path geometry
            //closed
                class function TGraphicGeometry.createClosedPathGeometry(   const geometryPointsIn  : TArray<TGeomPoint>;
                                                                            const axisConverterIn   : TDrawingAxisConverter ) : ID2D1PathGeometry;
                    begin
                        result := createPathGeometry(
                                                        D2D1_FIGURE_BEGIN.D2D1_FIGURE_BEGIN_FILLED,
                                                        D2D1_FIGURE_END.D2D1_FIGURE_END_CLOSED,
                                                        geometryPointsIn,
                                                        axisConverterIn
                                                    );
                    end;

            //open
                class function TGraphicGeometry.createOpenPathGeometry( const geometryPointsIn  : TArray<TGeomPoint>;
                                                                        const axisConverterIn   : TDrawingAxisConverter ) : ID2D1PathGeometry;
                    begin
                        result := createPathGeometry(
                                                        D2D1_FIGURE_BEGIN.D2D1_FIGURE_BEGIN_HOLLOW,
                                                        D2D1_FIGURE_END.D2D1_FIGURE_END_OPEN,
                                                        geometryPointsIn,
                                                        axisConverterIn
                                                    );
                    end;

        //bounding box
            function TGraphicGeometry.determineBoundingBox() : TGeomBox;
                begin
                    result := TGeomBox.determineBoundingBox( geometryPoints );
                end;


end.
