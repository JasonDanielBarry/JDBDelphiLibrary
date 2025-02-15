unit InputManagerClass;

interface

    uses
        system.SysUtils, system.Generics.Collections, System.UITypes,
        Vcl.StdCtrls
        ;

    type
        TInputManager = class
            private
                var
                    errorList       : TList<string>;
                    ListBoxErrors   : TListBox;
                //send errors to error list box
                    procedure populateErrorListBox();
            protected
                const
                    CONTROL_LEFT : integer = 5; //the VCL controls must have left = 5
                //add an error
                    procedure addError(const errorMessageIn : string); inline;
                //check for input errors
                    function checkForInputErrors() : boolean; virtual;
            public
                //constructor
                    constructor create(const errorListBoxIn : TListBox);
                //destructor
                    destructor destroy(); override;
                //read input
                    function readInput() : boolean; virtual; abstract;
                //write to input controls
                    procedure writeToInputControls(); virtual;
        end;

implementation

    //private
        //send errors to error list box
            procedure TInputManager.populateErrorListBox();
                var
                    i, errorCount : integer;
                begin
                    //count the errors and clear the list box
                        errorCount := errorList.Count;

                        ListBoxErrors.Clear();

                    //exit if there are not errors
                        if (errorCount < 1) then
                            begin
                                ListBoxErrors.Visible := False;
                                exit();
                            end;

                    //add the error message to the list box and show
                        errorList.Add('Errors:');

                        for i := 0 to (errorCount - 1) do
                            ListBoxErrors.Items.Add( errorList[i] );

                        ListBoxErrors.Visible := True;
                end;

    //protected
        //add an error
            procedure TInputManager.addError(const errorMessageIn : string);
                begin
                    errorList.Add( errorMessageIn );
                end;

        //check for input errors
            function TInputManager.checkForInputErrors() : boolean;
                begin
                    errorList.Clear();

                    result := false;
                end;

    //public
        //constructor
            constructor TInputManager.create(const errorListBoxIn : TListBox);
                var
                    boxEdgeSpace    : integer;
                    VCL_ScaleFactor : double;
                begin
                    inherited create();

                    //create error list
                        errorList := TList<string>.create();
                        errorList.Clear();

                    //store error list (***this is a >>>POINTER/REFERENCE<<< on the heap - changes here take effect everywhere***)
                        ListBoxErrors := errorListBoxIn;
                        ListBoxErrors.Visible := False;

                    //place the error box in its position
                        VCL_ScaleFactor := ListBoxErrors.ScaleFactor;

                        ListBoxErrors.Anchors := [ TAnchorKind.akLeft, TAnchorKind.akBottom ];

                        boxEdgeSpace := round( VCL_ScaleFactor * CONTROL_LEFT );

                        ListBoxErrors.Left  := boxEdgeSpace;
                        ListBoxErrors.top   := ListBoxErrors.Parent.Height - ListBoxErrors.Height - boxEdgeSpace;
                end;

        //destructor
            destructor TInputManager.destroy();
                begin
                    FreeAndNil( errorList );

                    inherited destroy();
                end;

        //write to input controls
            procedure TInputManager.writeToInputControls();
                begin
                    checkForInputErrors();
                end;



end.
