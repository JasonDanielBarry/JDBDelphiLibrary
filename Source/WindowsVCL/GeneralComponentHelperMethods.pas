unit GeneralComponentHelperMethods;

interface

    uses
        system.SysUtils,
        winapi.Windows,
        Vcl.Controls, Vcl.Forms, vcl.Buttons;

    function strToFloatZero(const stringValueIn : string) : double;

    procedure setSpeedButtonDown(   const  groupIndexIn  : integer;
                                    var speedButtonInOut : TSpeedButton );

    procedure orderComponentsLeftToRight( arrControlsIn : TArray<TControl> ); overload;

implementation

    function strToFloatZero(const stringValueIn : string) : double;
        var
            stringIsFloat   : boolean;
            valueOut        : double;
        begin
            stringIsFloat := TryStrToFloat(stringValueIn, valueOut);

            case (stringIsFloat) of
                True:
                    result := valueOut;
                False:
                    result := 0;
            end;
        end;

    procedure setSpeedButtonDown(   const  groupIndexIn  : integer;
                                    var speedButtonInOut : TSpeedButton );
        begin
            speedButtonInOut.AllowAllUp := True;
            speedButtonInOut.GroupIndex := groupIndexIn;
            speedButtonInOut.Down       := True;
        end;

    procedure orderComponentsLeftToRight(   const leftComponentIn   : TControl;
                                            var rightComponentInOut : TControl  ); overload;
        begin
            rightComponentInOut.Left := leftComponentIn.left + leftComponentIn.Width + 1;
        end;

    procedure orderComponentsLeftToRight( arrControlsIn : TArray<TControl> );
        var
            i, arrLen                   : integer;
            leftControl, rightControl   : TControl;
        begin
            arrLen := length(arrControlsIn);

            leftControl := arrControlsIn[0];

            for i := 1 to (arrLen - 1) do
                begin
                    rightControl := arrControlsIn[i];

                    orderComponentsLeftToRight(leftControl, rightControl);

                    leftControl := rightControl;
                end;
        end;

end.
