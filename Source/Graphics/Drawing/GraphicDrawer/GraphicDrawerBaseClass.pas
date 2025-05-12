unit GraphicDrawerBaseClass;

interface

    uses
        //Delphi
            system.SysUtils, System.Math, system.types, system.UIConsts, system.UITypes, system.Generics.Collections,
            vcl.Graphics,
            vcl.Direct2D, Winapi.D2D1,
        //custom
            ColourMethods,
            DrawingAxisConversionClass,
            GraphicObjectBaseClass,
            GraphicGeometryClass, GraphicLineClass, GraphicPolylineClass, GraphicPolygonClass,
            GeometryTypes
            ;

    type
        TGraphicDrawerBase = class
            strict protected
                var
                    axisConverter : TDrawingAxisConverter;
                //add graphic drawing object to the drawing object container
                    procedure addGraphicObject(const drawingObjectIn : TGraphicObject); virtual; abstract;
                //drawing procedures
                    //draw all geometry
                        procedure drawAll(  const canvasWidthIn, canvasHeightIn : integer;
                                            const drawingBackgroundColourIn     : TColor;
                                            var D2DCanvasInOut                  : TDirect2DCanvas);
            protected
                var
                    //these variables are declared here to be used in the drawAll() procedure
                    //but are set in GraphicDrawerAxisConversionInterfaceClass
                        drawingSpaceRatioEnabled    : boolean;
                        drawingSpaceRatio           : double;
            public
                //constructor
                    constructor create(); virtual;
                //destructor
                    destructor destroy(); override;
        end;

implementation

    //protected
        //drawing procedures
            //draw all geometry
                procedure TGraphicDrawerBase.drawAll(   const canvasWidthIn, canvasHeightIn : integer;
                                                        const drawingBackgroundColourIn     : TColor;
                                                        var D2DCanvasInOut                  : TDirect2DCanvas);
                    begin
                        //set axis converter canvas dimensions
                            axisConverter.setCanvasDimensions( canvasWidthIn, canvasHeightIn );

                        //set the drawing space ratio
                            if ( drawingSpaceRatioEnabled ) then
                                axisConverter.setDrawingSpaceRatio( drawingSpaceRatio );

                        //clear the canvas
                            D2DCanvasInOut.Brush.Color := drawingBackgroundColourIn;
                            D2DCanvasInOut.FillRect( Rect(0, 0, canvasWidthIn, canvasHeightIn) );
                    end;

    //public
        //constructor
            constructor TGraphicDrawerBase.create();
                begin
                    inherited create();

                    axisConverter := TDrawingAxisConverter.create();
                end;

        //destructor
            destructor TGraphicDrawerBase.destroy();
                begin
                    FreeAndNil( axisConverter );

                    inherited destroy();
                end;

end.
