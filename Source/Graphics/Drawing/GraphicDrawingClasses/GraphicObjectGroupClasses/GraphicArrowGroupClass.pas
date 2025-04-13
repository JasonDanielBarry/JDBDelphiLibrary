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
        GraphicObjectBaseClass,
        GraphicLineClass, GraphicPolygonClass,
        GraphicObjectGroupClass, GraphicArrowClass
        ;

    type
        TGraphicArrowGroup = class(TGraphicObjectGroup)
            private
                //arrow group count
                    function calculateArrowGroupCount(const arrowLengthIn, lineLengthIn : double) : integer;
                //arrow spacing
                    function calculateArrowSpacing( const arrowGroupCountIn : integer;
                                                    const lineLengthIn      : double ) : double;
                //determine the arrow group angle
                    function determineArrowGroupDirectionAngle( const   userDirectionAngleIn    : double;
                                                                const   arrowGroupDirectionIn   : EArrowGroupDirection;
                                                                const   arrowGroupLineIn        : TGeomLine             ) : double;
                //calculate group arrow points
                    function calculateArrowPoints(  const arrowCountIn      : integer;
                                                    const arrowSpacingIn    : double;
                                                    const lineIn            : TGeomLine) : TArray<TGeomPoint>;

            public
                //constructor
                    constructor create( const   filledIn                : boolean;
                                        const   lineThicknessIn         : integer;
                                        const   arrowLengthIn,
                                                userDirectionAngleIn    : double;
                                        const   fillColourIn,
                                                lineColourIn            : TColor;
                                        const   lineStyleIn             : TPenStyle;
                                        const   arrowOriginIn           : EArrowOrigin;
                                        const   arrowGroupDirectionIn   : EArrowGroupDirection;
                                        const   arrowGroupLineIn        : TGeomLine             );
                //destructor
                    destructor destroy(); override;

        end;


implementation

    //private
        //arrow group count
            function TGraphicArrowGroup.calculateArrowGroupCount(const arrowLengthIn, lineLengthIn : double) : integer;
                var
                    arrowGroupCountOut : integer;
                begin
                    //determine how many arrows to draw in the group
                        arrowGroupCountOut := round( lineLengthIn / (arrowLengthIn / 4) );

                        arrowGroupCountOut := max( arrowGroupCountOut, 3 );

                    result := arrowGroupCountOut;
                end;

        //arrow spacing
            function TGraphicArrowGroup.calculateArrowSpacing(  const arrowGroupCountIn : integer;
                                                                const lineLengthIn      : double) : double;
                begin
                    result := lineLengthIn / (arrowGroupCountIn - 1);
                end;

        //determine the arrow group angle
            function TGraphicArrowGroup.determineArrowGroupDirectionAngle(  const   userDirectionAngleIn    : double;
                                                                            const   arrowGroupDirectionIn   : EArrowGroupDirection;
                                                                            const   arrowGroupLineIn        : TGeomLine             ) : double;
                begin
                    result := 0;

                    case ( arrowGroupDirectionIn ) of
                        EArrowGroupDirection.agdRight:
                            exit( 0 );

                        EArrowGroupDirection.agdUp:
                            exit( 90 );

                        EArrowGroupDirection.agdLeft:
                            exit( 180 );

                        EArrowGroupDirection.agdDown:
                            exit( 270 );

                        EArrowGroupDirection.agdNormal:
                            begin
                                var dx, dy,
                                    lineAngleRad,
                                    normalAngleDeg  : double;
                                var lineVector      : TGeomSpaceVector;

                                //get the line vector components
                                    lineVector := arrowGroupLineIn.unitVector();

                                    dx := lineVector[0];
                                    dy := lineVector[1];

                                    FreeAndNil( lineVector );

                                //calculate the angle of the line and the normal angle
                                    lineAngleRad := ArcTan2( dy, dx );

                                    normalAngleDeg := RadToDeg( lineAngleRad ) - 90;

                                exit( normalAngleDeg );
                            end;

                        EArrowGroupDirection.agdUserDefined:
                            exit( userDirectionAngleIn );
                    end;
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
            constructor TGraphicArrowGroup.create(  const   filledIn                : boolean;
                                                    const   lineThicknessIn         : integer;
                                                    const   arrowLengthIn,
                                                            userDirectionAngleIn    : double;
                                                    const   fillColourIn,
                                                            lineColourIn            : TColor;
                                                    const   lineStyleIn             : TPenStyle;
                                                    const   arrowOriginIn           : EArrowOrigin;
                                                    const   arrowGroupDirectionIn   : EArrowGroupDirection;
                                                    const   arrowGroupLineIn        : TGeomLine             );
                var
                    i, arrowGroupCount          : integer;
                    arrowGroupAngle,
                    arrowSpacing, lineLength    : double;
                    arrArrowPoints              : TArray<TGeomPoint>;
                    arrGraphicArrows            : TArray<TGraphicObject>;
                begin
                    lineLength := arrowGroupLineIn.calculateLength();

                    //calculate number of arrows needed
                        arrowGroupCount := calculateArrowGroupCount( arrowLengthIn, lineLength );

                    //calculate spacing
                        arrowSpacing := calculateArrowSpacing( arrowGroupCount, lineLength );

                    //calculate arrow group angle
                        arrowGroupAngle := determineArrowGroupDirectionAngle( userDirectionAngleIn, arrowGroupDirectionIn, arrowGroupLineIn );

                    //calculate the arrow points
                        arrArrowPoints := calculateArrowPoints( arrowGroupCount, arrowSpacing, arrowGroupLineIn );

                    //create each arrow
                        clearGraphicObjectsGroup( False );

                        SetLength( arrGraphicArrows, arrowGroupCount );

                        for i := 0 to ( arrowGroupCount - 1 ) do
                                arrGraphicArrows[i] := TGraphicArrow.create(
                                                                                filledIn,
                                                                                lineThicknessIn, arrowLengthIn, arrowGroupAngle,
                                                                                fillColourIn, lineColourIn,
                                                                                lineStyleIn,
                                                                                arrowOriginIn,
                                                                                arrArrowPoints[i]
                                                                           );

                    addGraphicObjectsToGroup( arrGraphicArrows );
                end;

        //destructor
            destructor TGraphicArrowGroup.destroy();
                begin
                    inherited destroy();
                end;

end.
