unit InputManagerClass;

interface

    uses
        system.SysUtils, System.Classes, system.Generics.Collections, System.UITypes,
        Vcl.StdCtrls
        ;

    type
        TInputManager = class
            private
                var
                    errorList       : TStringList;
                    ListBoxErrors   : TListBox;
                //send errors to error list box
                    procedure populateErrorListBox();
            protected
                const
                    CONTROL_EDGE_SPACE : integer = 5; //the VCL controls must have and edge space = 5
                //add an error
                    procedure addError(const errorMessageIn : string); inline;
                //check for input errors
                    procedure checkForInputErrors(); virtual;
            public
                //constructor
                    constructor create(const errorListBoxIn : TListBox);
                //destructor
                    destructor destroy(); override;
                //setup input controls
                    procedure setupInputControls(); virtual;
                //process input
                    //read input
                        function readFromInputControls() : boolean; virtual;
                    //write to input controls
                        procedure writeToInputControls(const updateEmptyControlsIn : boolean = False); virtual;
                //count errors
                    function errorCount() : integer;
                //file management
                    //read to file
                        function readFromFile(const fileNameIn : string) : boolean; virtual; abstract;
                    //save to file
                        procedure saveToFile(const fileNameIn : string); virtual; abstract;
        end;

implementation

    //private
        //send errors to error list box
            procedure TInputManager.populateErrorListBox();
                var
                    i : integer;
                begin
                    //perform error checking
                        checkForInputErrors();

                    //initialise list box for error posting
                        ListBoxErrors.Clear();

                    //exit if there are not errors
                        if ( errorCount() < 1 ) then
                            begin
                                ListBoxErrors.Visible := False;
                                exit();
                            end;

                    //add the error message to the list box and show
                        ListBoxErrors.Items.Add( 'ERRORS:' );

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
            procedure TInputManager.checkForInputErrors();
                begin
                    errorList.Clear();
                end;

        //setup input controls
            procedure TInputManager.setupInputControls();
                var
                    boxEdgeSpace    : integer;
                    VCL_ScaleFactor : double;
                begin
                    //set list box initially to non visible
                        ListBoxErrors.Visible := False;

                    //place the error box in its position
                        VCL_ScaleFactor := ListBoxErrors.ScaleFactor;

                        ListBoxErrors.Anchors := [ TAnchorKind.akLeft, TAnchorKind.akBottom ];

                        boxEdgeSpace := round( VCL_ScaleFactor * CONTROL_EDGE_SPACE );

                        ListBoxErrors.Left  := boxEdgeSpace;
                        ListBoxErrors.top   := ListBoxErrors.Parent.Height - ListBoxErrors.Height - boxEdgeSpace;
                end;

    //public
        //constructor
            constructor TInputManager.create(const errorListBoxIn : TListBox);

                begin
                    inherited create();

                    //create error list
                        errorList := TStringList.create();
                        errorList.Clear();

                    //store error list (***this is a >>>POINTER/REFERENCE<<< on the heap - changes here take effect everywhere***)
                        ListBoxErrors := errorListBoxIn;

                    setupInputControls();
                end;

        //destructor
            destructor TInputManager.destroy();
                begin
                    FreeAndNil( errorList );

                    inherited destroy();
                end;

        //process input
            //read input
                function TInputManager.readFromInputControls() : boolean;
                    begin
                        //nothing here for now
                    end;

            //write to input controls
                procedure TInputManager.writeToInputControls(const updateEmptyControlsIn : boolean = False);
                    begin
                        populateErrorListBox();
                    end;

        //count errors
            function TInputManager.errorCount() : integer;
                begin
                    result := errorList.Count;
                end;

end.
