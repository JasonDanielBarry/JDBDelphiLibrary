unit ColourMethods;

interface

    uses
         Winapi.Windows, system.UIConsts, system.UITypes, Vcl.GraphUtil
         ;

    //make colours from RGBA combinations
        //TAlphaColor
            function makeAlphaColour(   const redIn, greenIn, blueIn    : byte;
                                        const opacityIn                 : byte = 255) : TAlphaColor; overload;

            function makeAlphaColour(const RGBQuadIn : TRGBQuad) : TAlphaColor; overload;

        //TColor
            function makeColour(const redIn, greenIn, blueIn : byte) : TColor; overload;

            function makeColour(const RGBQuadIn : TRGBQuad) : TColor; overload;

    //convert a TColor to TAlphaColor
        function colourToAlphaColour(   const colourIn  : TColor;
                                        const opacityIn : byte = 255) : TAlphaColor;

implementation

    //get alpha value
        function getAlphaValue(const alphaColourIn : TAlphaColor) : byte;
            var
                colourReferenceHexiDec : COLORREF;
            begin
                colourReferenceHexiDec := TAlphaColorRec.ColorToRGB( alphaColourIn );

                result := ( (colourReferenceHexiDec shr 24) AND $FF );
            end;

    //extract a colour's RGBA values
        function extractColourRGB(const colourIn : Tcolor) : TRGBQuad;
            var
                redGreenBlueOut : TRGBQuad;
            begin
                GetRGB( colourIn, redGreenBlueOut.rgbRed, redGreenBlueOut.rgbGreen, redGreenBlueOut.rgbBlue );
                redGreenBlueOut.rgbReserved := 255;

                result := redGreenBlueOut;
            end;

        function extractAlphaColourRGBA(const alphaColourIn : TAlphaColor) : TRGBQuad;
            var
                tempColour              : TColor;
                redGreenBlueAlphaOut    : TRGBQuad;
            begin
                tempColour := AlphaColorToColor( alphaColourIn );

                redGreenBlueAlphaOut                := extractColourRGB( tempColour );
                redGreenBlueAlphaOut.rgbReserved    := getAlphaValue( alphaColourIn );

                result := redGreenBlueAlphaOut;
            end;

    //make colours from RGBA combinations
        //TAlphaColor
            function makeAlphaColour(   const redIn, greenIn, blueIn    : byte;
                                        const opacityIn                 : byte = 255) : TAlphaColor;
                begin
                    result := MakeColor( redIn, greenIn, blueIn, opacityIn );
                end;

            function makeAlphaColour(const RGBQuadIn : TRGBQuad) : TAlphaColor;
                begin
                    result := makeAlphaColour( RGBQuadIn.rgbRed, RGBQuadIn.rgbGreen, RGBQuadIn.rgbBlue, RGBQuadIn.rgbReserved );
                end;

        //TColor
            function makeColour(const redIn, greenIn, blueIn : byte) : TColor;
                var
                    tempAlphaColour : TAlphaColor;
                begin
                    tempAlphaColour := makeAlphaColour(redIn, greenIn, blueIn, 255);

                    result := AlphaColorToColor( tempAlphaColour );
                end;

            function makeColour(const RGBQuadIn : TRGBQuad) : TColor;
                begin
                    result := makeColour( RGBQuadIn.rgbRed, RGBQuadIn.rgbGreen, RGBQuadIn.rgbBlue );
                end;

    //convert a TColor to TAlphaColor
        function colourToAlphaColour(   const colourIn  : TColor;
                                        const opacityIn : byte = 255) : TAlphaColor;
            var
                redGreenBlueAlpha : TRGBQuad;
            begin
                redGreenBlueAlpha := extractColourRGB( colourIn );

                result := makeAlphaColour( redGreenBlueAlpha );
            end;

end.
