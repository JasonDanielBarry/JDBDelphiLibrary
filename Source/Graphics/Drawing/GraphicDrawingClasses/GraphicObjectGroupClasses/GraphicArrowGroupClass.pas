unit GraphicArrowGroupClass;

interface

    uses
        Winapi.D2D1,
        system.SysUtils, system.Math, system.UITypes,
        Vcl.Direct2D, vcl.Graphics,
        GeomBox, GeometryTypes, GeomSpaceVectorClass,
        GeomLineClass, GeomPolygonClass,
        GraphicDrawingTypes,
        DrawingAxisConversionClass,
        GraphicLineClass, GraphicPolygonClass,
        GraphicObjectGroupClass, GraphicArrowClass
        ;

    type
        TGraphicArrowGroup = class(TGraphicObjectGroup)
            private
                //arrow spacing
                    function calculateArrowSpacing(const arrowLengthIn, lineLengthIn : double) : double;
                //calculate group arrow points
                    function calculateArrowPoints(  const arrowCountIn      : integer;
                                                    const arrowSpacingIn    : double;
                                                    const lineIn            : TGeomLine) : TArray<TGeomPoint>;
            public
                //constructor
                    constructor create( const   filledIn            : boolean;
                                        const   lineThicknessIn     : integer;
                                        const   arrowLengthIn,
                                                directionAngleIn    : double;
                                        const   fillColourIn,
                                                lineColourIn        : TColor;
                                        const   lineStyleIn         : TPenStyle;
                                        const   arrowOriginIn       : EArrowOrigin;
                                        const   arrowGroupLineIn    : TGeomLine     );
                //destructor
                    destructor destroy(); override;

        end;


implementation

    //private
        //arrow spacing
            function TGraphicArrowGroup.calculateArrowSpacing(const arrowLengthIn, lineLengthIn : double) : double;
                var
                    arrowGroupCount : integer;
                    arrowSpacingOut : double;
                begin
                    //determine how many arrows to draw in the group
                        arrowGroupCount := round( lineLengthIn / (arrowLengthIn / 3) );

                        arrowGroupCount := max( arrowGroupCount, 3 );

                    //calculate the required spacing
                        arrowSpacingOut := lineLengthIn / (arrowGroupCount - 1);

                    result := arrowLengthIn
                end;

        //calculate group arrow point
            function TGraphicArrowGroup.calculateArrowPoints(   const arrowCountIn      : integer;
                                                                const arrowSpacingIn    : double;
                                                                const lineIn            : TGeomLine) : TArray<TGeomPoint>;
                var
                    i                   : integer;
                    dx, dy, x, y        : double;
                    lineStartPoint      : TGeomPoint;
                    arrArrowPointsOut   : TArray<TGeomPoint>;
                    lineVector          : TGeomSpaceVector;
                begin
                    //get the line vector components
                        lineVector := lineIn.unitVector();

                        dx := lineVector[0] * arrowSpacingIn;
                        dy := lineVector[1] * arrowSpacingIn;

                        FreeAndNil( lineVector );

                    //get the line start point
                        lineStartPoint := lineIn.getStartPoint();

                    //calculate the arrow points
                        SetLength( arrArrowPointsOut, arrowCountIn );

                        for i := 0 to ( arrowCountIn - 1 ) do
                            begin
                                x := lineStartPoint.x + (i * dx);
                                y := lineStartPoint.y + (i * dy);

                                arrArrowPointsOut[i] := TGeomPoint.create( x, y );
                            end;

                    result := arrArrowPointsOut;
                end;

    //public
        //constructor
            constructor TGraphicArrowGroup.create(  const   filledIn            : boolean;
                                                    const   lineThicknessIn     : integer;
                                                    const   arrowLengthIn,
                                                            directionAngleIn    : double;
                                                    const   fillColourIn,
                                                            lineColourIn        : TColor;
                                                    const   lineStyleIn         : TPenStyle;
                                                    const   arrowOriginIn       : EArrowOrigin;
                                                    const   arrowGroupLineIn    : TGeomLine     );
//                var

                begin

                end;

        //destructor
            destructor TGraphicArrowGroup.destroy();
                begin
                    inherited destroy();
                end;

end.
