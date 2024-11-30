unit GeomDrawerBaseClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UIConsts, system.UITypes, system.Generics.Collections,
            vcl.Graphics,
        //custom
            ColourMethods,
            DrawingAxisConversionClass,
            DrawingGeometryClass,
            GeometryTypes, GeomBox,
            GeometryBaseClass,
            GeomLineClass, GeomPolyLineClass, GeomPolygonClass
            ;

    type
        TGeomDrawer = class
            strict protected
                var
                    drawingBackgroundColour : TColor;
                    axisConverter           : TDrawingAxisConverter;
                //add drawing geometry to the drawing object container
                    procedure addGeometry(const drawingGeometryIn : TDrawingGeometry); virtual; abstract;
                //drawing procedures
                    //draw a drawing geometry object
                        procedure drawGeometry(const drawingGeometryIn : TDrawingGeometry); virtual; abstract;
                    //draw all geometry
                        procedure drawAllGeometry(const canvasWidthIn, canvasHeightIn : integer); virtual;
            public
                //constructor
                    constructor create(); virtual;
                //destructor
                    destructor destroy(); override;
                //add different drawing geometry
                    procedure addLine(  const lineIn            : TGeomLine;
                                        const lineThicknessIn   : integer = 2;
                                        const colourIn          : TColor = TColors.Black;
                                        const styleIn           : TPenStyle = TPenStyle.psSolid );
                    procedure addPolyline(  const polylineIn        : TGeomPolyLine;
                                            const lineThicknessIn   : integer = 2;
                                            const colourIn          : TColor = TColors.Black;
                                            const styleIn           : TPenStyle = TPenStyle.psSolid );
                    procedure addPolygon(   const polygonIn         : TGeomPolygon;
                                            const lineThicknessIn   : integer = 2;
                                            const fillColourIn      : TColor = TColors.Null;
                                            const lineColourIn      : TColor = TColors.Black;
                                            const lineStyleIn       : TPenStyle = TPenStyle.psSolid     );
                //modifiers
                    procedure setDrawingBackgroundColour(const colourIn : TColor);
                    procedure setCurrentDrawingLayer(const layerKeyIn : string); virtual; abstract;
        end;

implementation

    //protected
        //drawing procedures
            //draw all geometry
                procedure TGeomDrawer.drawAllGeometry(const canvasWidthIn, canvasHeightIn : integer);
                    begin
                        //set axis converter canvas dimensions
                            axisConverter.setCanvasDimensions( canvasHeightIn, canvasWidthIn );

                        //set the drawing space ratio
                            axisConverter.setDrawingSpaceRatio( 1 );
                    end;

    //public
        //constructor
            constructor TGeomDrawer.create();
                begin
                    inherited create();

                    axisConverter := TDrawingAxisConverter.create();
                end;

        //destructor
            destructor TGeomDrawer.destroy();
                begin
                    FreeAndNil( axisConverter );

                    inherited destroy();
                end;

        //add drawing geometry
            procedure TGeomDrawer.addLine(  const lineIn            : TGeomLine;
                                            const lineThicknessIn   : integer = 2;
                                            const colourIn          : TColor = TColors.Black;
                                            const styleIn           : TPenStyle = TPenStyle.psSolid );
                var
                    newDrawingGeometry : TDrawingGeometry;
                begin
                    newDrawingGeometry := TDrawingGeometry.create(  lineThicknessIn,
                                                                    colourIn,
                                                                    styleIn,
                                                                    lineIn          );

                    addGeometry( newDrawingGeometry );
                end;

            procedure TGeomDrawer.addPolyline(  const polylineIn        : TGeomPolyLine;
                                                const lineThicknessIn   : integer = 2;
                                                const colourIn          : TColor = TColors.Black;
                                                const styleIn           : TPenStyle = TPenStyle.psSolid );
                var
                    newDrawingGeometry : TDrawingGeometry;
                begin
                    newDrawingGeometry := TDrawingGeometry.create(  lineThicknessIn,
                                                                    colourIn,
                                                                    styleIn,
                                                                    polylineIn      );

                    addGeometry( newDrawingGeometry );
                end;

            procedure TGeomDrawer.addPolygon(   const polygonIn         : TGeomPolygon;
                                                const lineThicknessIn   : integer = 2;
                                                const fillColourIn      : TColor = TColors.Null;
                                                const lineColourIn      : TColor = TColors.Black;
                                                const lineStyleIn       : TPenStyle = TPenStyle.psSolid     );
                var
                    newDrawingGeometry : TDrawingGeometry;
                begin
                    newDrawingGeometry := TDrawingGeometry.create(  lineThicknessIn,
                                                                    fillColourIn,
                                                                    lineColourIn,
                                                                    lineStyleIn,
                                                                    polygonIn       );

                    addGeometry( newDrawingGeometry );
                end;

        //modifiers
            procedure TGeomDrawer.setDrawingBackgroundColour(const colourIn : TColor);
                begin
                    drawingBackgroundColour := colourIn;
                end;

end.
