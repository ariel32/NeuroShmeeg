IncludeFile "NeuroShmeegGUI.pbf"

Enumeration MyCONST
  #ExitFromProgramm = 64000
EndEnumeration


UseSQLiteDatabase()

Global dbFile.s = "database.sqlite"
Global cID.q = 0
Global cMode = 0
Global sessID, ts.q
Global query.s

sET = ElapsedMilliseconds()

Procedure FillState(query.s)
  If FileSize(dbFile) > 0
    OpenDatabase(0, dbFile, "", "")
  Else
    CreateFile(0, dbFile) : CloseFile(0)
    OpenDatabase(0, dbFile, "", "")
    
    makeDB.s = "CREATE TABLE provisors(orgName TEXT, CSNumber INT, CSCategory TEXT, CSAddres TEXT, CSDescription TEXT, "+
               "provisorName TEXT, provisorTNumber TEXT, provisorSex TEXT, provisorAge INT,  provisorEducation TEXT, provisorCategory INT, provisorDescription TEXT, PRIMARY KEY (CSNumber, provisorTNumber));"+
               "CREATE TABLE observations (sessid INT, inqnum INT, tos INT, stdabbrev VARCHAR(50), timestamp INT)"
    ;Debug makeDB
    If DatabaseUpdate(0, makeDB) = 0
      Debug DatabaseError()
    EndIf
  EndIf
  If DatabaseUpdate(0, query) = 0
    Debug DatabaseError()
  EndIf
  CloseDatabase(0)
EndProcedure
Procedure EDnableIMT(state.i)
  If(state = -1 ) : state = 1 : Else : state = 0 : EndIf
  state = state * (-1)
  DisableGadget(#b08, state) : DisableGadget(#b09, state)
  SetGadgetState(#b08, 0) : SetGadgetState(#b09, 0)
EndProcedure
Procedure EDnableRxPlus(state.i)
  If(state = -1 ) : state = 1 : Else : state = 0 : EndIf
  For x = #b02 To #b07 : DisableGadget(x, state) : SetGadgetState(x, 0) : Next
  For x = #b11 To #ButtonInstructionCopy : DisableGadget(x, state) : SetGadgetState(x, 0) : Next
  If (GetGadgetState(#ButtonRxL) <> 1) : DisableGadget(#b13, 1) : SetGadgetState(#b13, 0) : EndIf
EndProcedure
Procedure EDnableRxL(state.i)
  If(state = -1 ) : state = 1 : Else : state = 0 : EndIf
  If (GetGadgetState(#ButtonRx) = 0) : SetGadgetState(#ButtonRx, 1) : EDnableRxPlus(0) : EndIf
EndProcedure
Procedure TogleCommon()
  SetGadgetState(#b04, 0)
  SetGadgetState(#b05, 0)
  SetGadgetState(#b06, 0)
  SetGadgetState(#b07, 0)
  SetGadgetState(#b10, 0)
EndProcedure
Procedure EDnableCommon(state.i)
  DisableGadget(#b04, state) ;: SetGadgetState(#b04, 0)
  DisableGadget(#b05, state) ;: SetGadgetState(#b05, 0)
  DisableGadget(#b06, state) ;: SetGadgetState(#b06, 0)
  DisableGadget(#b07, state) ;: SetGadgetState(#b07, 0)
  DisableGadget(#b10, state) ;: SetGadgetState(#b10, 0)  
  If(state = 1) : TogleCommon() : EndIf    
EndProcedure
Procedure EndMaintaince()
  EDnableCommon(1) : EDnableIMT(-1) : EDnableRxPlus(-1)
  RemoveGadgetItem(#ListIconQueue, GetGadgetState(#ListIconQueue))
  cMode = 0 : SetGadgetState(#ButtonRx, 0) : SetGadgetState(#ButtonNotRx, 0) : SetGadgetState(#ButtonIMT, 0)
  For x = 0 To CountGadgetItems(#ListIconQueue)
    SetGadgetItemText(#ListIconQueue, x, Str(x+1), 0)
  Next  
EndProcedure
Procedure GadgetsUpdate()
  If OpenDatabase(0, dbFile, "", "")
    If DatabaseQuery(0, "SELECT orgName, provisorName FROM provisors")
      ClearGadgetItems(#ComboCSName) : ClearGadgetItems(#ComboOrganisationName) : ClearGadgetItems(#ComboProvisor)
      While NextDatabaseRow(0)
        AddGadgetItem(#ComboOrganisationName, -1, GetDatabaseString(0, 0))
        AddGadgetItem(#ComboProvisor, -1, GetDatabaseString(0, 1))
      Wend
      FinishDatabaseQuery(0)
    EndIf
    CloseDatabase(0)
  Else
    Debug "Can't open database !"
  EndIf
  
  ;   #ComboCSName
  ;   #ComboOrganisationName
  ;   #ComboProvisor
EndProcedure
Procedure FixAdOps(Gadget, opName.s)
  mode.s
  If GetGadgetState(Gadget) = 1 : mode = opname+"_on" : Else : mode = opname+"_off" : EndIf
  ts = Date()*1000
  cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
  query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', '"+mode+"', '"+Str(ts)+"')"
  FillState(query)
EndProcedure

OpenWindowMain() : StickyWindow(#WindowMain, 1)
SetWindowTitle(#WindowMain, "NS 0.4."+Str(#Pb_editor_BuildCount)+"."+Str(#Pb_Editor_CompileCount))

AddKeyboardShortcut(#WindowMain, #PB_Shortcut_Escape, #ExitFromProgramm)
AddKeyboardShortcut(#WindowMain, #PB_Shortcut_F1, #ButtonRx)
AddKeyboardShortcut(#WindowMain, #PB_Shortcut_F2, #ButtonNotRx)
AddKeyboardShortcut(#WindowMain, #PB_Shortcut_F3, #ButtonIMT)
AddKeyboardShortcut(#WindowMain, #PB_Shortcut_Return, #bExit)

For x = #b02 To #ButtonInstructionCopy: DisableGadget(x, 1) : Next
DisableGadget(#b00, 1)
GadgetsUpdate()
FillState("")

;{ TODO
; + сделать так, чтобы при открытии окон автоматически обновлялись гаджеты
; + таксирование рецепта
; + снятие копии рецепта
; + снятие копии рецепта
; + ПОВЫСИТЬ ГРАНУЛЯРНОСТЬ!""2221111
; - сделать так, чтобы при выборе провизора или аптеки обновлялась информация о них
;}





Repeat
  Event=WaitWindowEvent()
  Gadget=EventGadget()
  Type=EventType()
  Window=EventWindow()
  Menu=EventMenu()
  Timer=EventTimer()
  
  
  If (Event = #PB_Event_Gadget And (Type = #PB_EventType_LeftClick Or Type = #PB_EventType_Change)) Or Event = #PB_Event_Menu
    If Menu = #ExitFromProgramm : End : EndIf
    ;{ заставляем ввести провизора при отстуствии такового в #ComboProvisor
    If GetGadgetText(#ComboProvisor) = "" And GetGadgetState(#Panel) = 0
      OpenDatabase(0, dbFile, "", "")
      DatabaseQuery(0, "SELECT COUNT(value) FROM employeesDescription")
      NextDatabaseRow(0)
      If GetDatabaseString(0, 0) <> "0"
        MessageRequester("", "Для начала работы с системой выберите провизора!", #MB_ICONERROR)
        SetGadgetState(Gadget, 0)
      Else
        MessageRequester("", "Для начала работы с системой добавьте провизора!", #MB_ICONERROR)
        SetGadgetState(Gadget, 0) : SetGadgetState(#Panel, 1)
      EndIf
      CloseDatabase(0)
      GadgetsUpdate()
      Continue
    EndIf
    ;}
    ;{ если событие сгененрировано шорткатом, присваиваем его значение гаджету и обрабатываем как кнопку
    If Event = #PB_Event_Menu
      Gadget = Menu
      If Gadget <> #ComboProvisor
        SetGadgetState(Gadget, 1)
      EndIf
    EndIf
    ;}

    Select Gadget
      Case #b00 ; постановка в очередь
        ts = Date()*1000+ElapsedMilliseconds()-sET
        AddGadgetItem(#ListIconQueue, -1, Str(CountGadgetItems(#ListIconQueue)+1)+Chr(10)+Str(ts))
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'inq', '"+Str(ts)+"')"
        FillState(query)
      Case #b01 ; поступил на обслуживание
        ts = Date()*1000+ElapsedMilliseconds()-sET
        If CountGadgetItems(#ListIconQueue) = 0
          AddGadgetItem(#ListIconQueue, -1, Str(CountGadgetItems(#ListIconQueue)+1)+Chr(10)+Str(ts))
        EndIf
        If (GetGadgetState(#ListIconQueue) = -1) : SetGadgetState(#ListIconQueue, 0) : EndIf
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'inm', '"+Str(ts)+"')"
        FillState(query)
      Case #b02
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'phe', '"+Str(ts)+"')"
        FillState(query)
      Case #b03
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'sea', '"+Str(ts)+"')"
        FillState(query)
      Case #b04
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'phc', '"+Str(ts)+"')"
        FillState(query)
      Case #b05
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'sis', '"+Str(ts)+"')"
        FillState(query)
      Case #b06
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'ssp', '"+Str(ts)+"')"
        FillState(query)
      Case #b07
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'fch', '"+Str(ts)+"')"
        FillState(query)
      Case #b08
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'prd', '"+Str(ts)+"')"
        FillState(query)
      Case #b09
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'gar', '"+Str(ts)+"')"
        FillState(query)
      Case #b10
        If MessageRequester("Буэээ","Расчет осуществлен при помощи наличных денеждных значков?", #MB_YESNO | #MB_ICONQUESTION) = #PB_MessageRequester_Ok
          ts = Date()*1000
          cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
          query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'pcs', '"+Str(ts)+"')"
          FillState(query)
        Else
          ts = Date()*1000
          cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
          query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'pcr', '"+Str(ts)+"')"
          FillState(query)
        EndIf
      Case #b11
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'bsd', '"+Str(ts)+"')"
        FillState(query)
      Case #bExit
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'END', '"+Str(ts)+"')"
        FillState(query)
        EndMaintaince()        
        SetGadgetState(#b01, 0)
        ;--------------------------------------------------------
      Case #bops01
        FixAdOps(Gadget, "tinf") ; #bops01 "Информирование по телефону"
      Case #bops02
        FixAdOps(Gadget, "qcon") ; #bops02 "Контроль качества"
      Case #bops03
        FixAdOps(Gadget, "vitf") ; #bops03 "Оформление витрин"
      Case #bops04
        FixAdOps(Gadget, "docf") ; #bops04 "Ведение учета и отчетности"
      Case #bops05
        FixAdOps(Gadget, "sang") ; #bops05 "Сан-просветительная работа"
      Case #bops06
        FixAdOps(Gadget, "cmdt") ; #bops06 "Приемка товара"
      Case #bops07
        FixAdOps(Gadget, "ctrn") ; #bops07 "Пополнение отделов аптеки"
      Case #bops08
        FixAdOps(Gadget, "expd") ; #bops08 "Контроль сроков годности"
      Case #bops09
        FixAdOps(Gadget, "clms") ; #bops09 "Работа с замечаниями"
      Case #bops10
        FixAdOps(Gadget, "sedt") ; #bops10 "Выявление побочных реакций"
        ; -------------------------------------------------------  
      Case #ButtonRx
        If GetGadgetState(#ButtonRx) = 0 : i = -1 : Else : i = 1 : EndIf
        If GetGadgetState(#ButtonRx) = 0 : SetGadgetState(#ButtonRxL, 0) : EndIf
        cMode = cMode + i
        EDnableRxPlus(i)
      Case  #ButtonRxL
        If GetGadgetState(#ButtonRxL) = 0 : i = -1 : Else : i = 1 : EndIf
        If GetGadgetState(#ButtonRxL) = 1 And GetGadgetState(#ButtonRx) = 0 : subMode = 1 : Else : subMode = 0 : EndIf
        If GetGadgetState(#ButtonRxL) = 1 : SetGadgetState(#ButtonRx, 1) : EndIf        
        cMode = cMode + i*15 + subMode
        EDnableRxL(i)
      Case #ButtonNotRx
        If GetGadgetState(#ButtonNotRx) = 0 : i = -1 : Else : i = 1 : EndIf
        DisableGadget(#ButtonInstructionCopy, 0)
        cMode = cMode + i*2
      Case #ButtonIMT
        Debug 3
        If GetGadgetState(#ButtonIMT) = 0 : i = -1 : Else : i = 1 : EndIf
        cMode = cMode + i*4
        EDnableIMT(i)
        ;-------------------------------------------------------------
      Case #ComboProvisor
        sessID = Date()
        SetWindowTitle(#WindowMain, "NS 0.4."+Str(#Pb_editor_BuildCount)+"."+Str(#Pb_Editor_CompileCount)+" - "+GetGadgetText(#ComboProvisor))
      Case #ButtonAdd
        For x = #EditorCSDescription To #EditorProvisorDescription
          If GetGadgetText(x) = ""
            MessageRequester("Заполнены не все поля!","Полей не так и много, я уверен, еще раз и все получится!", #MB_ICONERROR)
            isAllFieldFilled = #False
            Break
          Else
            isAllFieldFilled = #True
          EndIf
        Next
        If isAllFieldFilled
          query.s = "INSERT INTO provisors (orgName, CSNumber, CSCategory, CSAddres, CSDescription, provisorName, provisorTNumber, provisorSex, provisorAge, provisorEducation, provisorCategory, provisorDescription)"+
                    " VALUES ('"+GetGadgetText(#ComboOrganisationName)+"', '"+GetGadgetText(#ComboCSName)+"', '"+GetGadgetText(#ComboCSCategory)+"', "+
                    "'"+GetGadgetText(#StringCSAddres)+"', '"+GetGadgetText(#EditorCSDescription)+"','"+GetGadgetText(#StringProvisorName)+"',"+
                    "'"+GetGadgetText(#StringProvisorTNumber)+"', '"+GetGadgetText(#ComboProvisorSex)+"', '"+GetGadgetText(#StringProvisorAge)+"', '"+GetGadgetText(#ComboProvisorEducation)+"', '"+GetGadgetText(#ComboProvisorCategory)+"', '"+GetGadgetText(#EditorProvisorDescription)+"')"
          FillState(query)
          Debug query
          SetGadgetText(#ComboOrganisationName, "")  : SetGadgetText(#ComboCSName, "")
          SetGadgetText(#ComboCSCategory, "")        : SetGadgetText(#StringCSAddres, "")
          SetGadgetText(#EditorCSDescription, "")    : SetGadgetText(#StringProvisorName, "")
          SetGadgetText(#StringProvisorTNumber, "")  : SetGadgetText(#ComboProvisorSex, "")
          SetGadgetText(#StringProvisorAge, "")      : SetGadgetText(#ComboProvisorEducation, "")
          SetGadgetText(#ComboProvisorCategory, "")  : SetGadgetText(#EditorProvisorDescription, "")
        EndIf
    EndSelect
    
    If cMode = 0 : For x = #b02 To #ButtonInstructionCopy: DisableGadget(x, 1) : Next : EndIf  
    If GetGadgetState(#ButtonRx) : EDnableRxPlus(1) : EndIf
    If GetGadgetState(#buttonRx) = 0 And GetGadgetState(#buttonNotRX) = 0 And GetGadgetState(#buttonRxL) = 0 And GetGadgetState(#buttonIMT) = 0
      For x = #b02 To #buttonRxCopy : DisableGadget(x, 1) : SetGadgetState(x, 0) : Next
    EndIf
  EndIf
  
  If CountGadgetItems(#ListIconQueue) = 0 : DisableGadget(#b00, 1) : Else : DisableGadget(#b00, 0) : EndIf
  If cMode = 0 : EDnableCommon(1) : Else : EDnableCommon(0) : EndIf
  
Until Event=#PB_Event_CloseWindow
; IDE Options = PureBasic 5.20 LTS (Windows - x86)
; CursorPosition = 319
; FirstLine = 254
; Folding = -P-
; EnableUnicode
; EnableXP
; EnableCompileCount = 29
; EnableBuildCount = 0
; IncludeVersionInfo