IncludeFile "NeuroShmeegGUI.pbf"

Enumeration MyCONST
  #PopupMenu = 4000
  #MenuY
  #MenuA
  #MenuE
  #MenuCount
  #ExitFromProgramm = 64000
EndEnumeration


UseSQLiteDatabase()

Global.s dbFile = "database.sqlite", query, vCategory, NS_version = "0.6"
Global cID.q = 0
Global cMode = 0
Global sessID, ts.q

sET = ElapsedMilliseconds()

Procedure FillState(query.s)
  If FileSize(dbFile) > 0
    OpenDatabase(0, dbFile, "", "")
  Else
    CreateFile(0, dbFile) : CloseFile(0)
    OpenDatabase(0, dbFile, "", "")
    
    makeDB.s = "CREATE TABLE provisors(orgName TEXT, CSNumber INT, CSCategory TEXT, CSAddres TEXT, CSDescription TEXT, "+
               "provisorName TEXT, provisorTNumber TEXT, provisorSex TEXT, provisorAge INT,  provisorEducation TEXT, provisorCategory INT, provisorDescription TEXT, provisorExpCont INT, provisorExpGeneral INT, PRIMARY KEY (CSNumber, provisorTNumber));"+
               "CREATE TABLE observations (sessid INT, inqnum INT, tos INT, stdabbrev VARCHAR(50), visitorCategory VARCHAR(50), timestamp INT)"
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
  DisableGadget(#b10, state) : SetGadgetState(#b10, 0)
  DisableGadget(#b11, state) : SetGadgetState(#b11, 0)
  DisableGadget(#b12, state) : SetGadgetState(#b12, 0)
EndProcedure
Procedure EDnableRx(state.i)
  If(state = -1) : state = 1 : Else : state = 0 : EndIf
  If (state = 1)
    SetGadgetState(#b02, 0)
    SetGadgetState(#b04, 0)
    SetGadgetState(#b07, 0)
    For x = #b13 To #b20: SetGadgetState(x, 0) : Next
  EndIf
  DisableGadget(#b02, state)
  DisableGadget(#b04, state)
  DisableGadget(#b07, state)
  For x = #b13 To #b20: DisableGadget(x, state) : Next
  If (GetGadgetState(#ButtonRx) = 1) : DisableGadget(#b15, 0) : Else : DisableGadget(#b15, 1) : EndIf
  If (GetGadgetState(#ButtonRx) = 1 Or GetGadgetState(#ButtonRx2) = 1 Or GetGadgetState(#ButtonRx3) = 1 Or GetGadgetState(#ButtonRxL) = 1)
    DisableGadget(#b02, 0)
    DisableGadget(#b07, 0)
    DisableGadget(#b14, 0)
    DisableGadget(#b19, 0)
    DisableGadget(#b20, 0)
  EndIf
  If GetGadgetState(#ButtonNotRx) = 1 : DisableGadget(#b14, 0) : EndIf
EndProcedure
Procedure EDnableRxL(state.i)
  If(state = -1 ) : state = 1 : Else : state = 0 : EndIf
  DisableGadget(#b03, state) ;: SetGadgetState(#b04, 0)
  DisableGadget(#b08, state) ;: SetGadgetState(#b05, 0)
  DisableGadget(#b09, state) ;: SetGadgetState(#b06, 0)
  EDnableRx(state)
EndProcedure
Procedure TogleCommon()
  SetGadgetState(#b04, 0)
  SetGadgetState(#b05, 0)
  SetGadgetState(#b06, 0)  
  SetGadgetState(#b13, 0)  
  SetGadgetState(#b16, 0)
  SetGadgetState(#b17, 0)
  SetGadgetState(#b18, 0)
EndProcedure
Procedure EDnableCommon(state.i)
  DisableGadget(#b04, state) ;: SetGadgetState(#b04, 0)
  DisableGadget(#b05, state) ;: SetGadgetState(#b05, 0)
  DisableGadget(#b06, state) ;: SetGadgetState(#b06, 0)
  DisableGadget(#b13, state) ;: SetGadgetState(#b10, 0)  
  DisableGadget(#b16, state) ;: SetGadgetState(#b10, 0)  
  DisableGadget(#b17, state) ;: SetGadgetState(#b10, 0)  
  DisableGadget(#b18, state) ;: SetGadgetState(#b10, 0)  
  If(state = 1) : TogleCommon() : EndIf    
EndProcedure
Procedure EndMaintaince()
  EDnableCommon(1) : EDnableIMT(-1) : EDnableRx(-1)
  RemoveGadgetItem(#ListIconQueue, GetGadgetState(#ListIconQueue))
  cMode = 0
  SetGadgetState(#ButtonRx, 0)
  SetGadgetState(#ButtonRx2, 0)
  SetGadgetState(#ButtonRx3, 0)
  SetGadgetState(#ButtonNotRx, 0)
  SetGadgetState(#ButtonIMT, 0)
  SetGadgetState(#ButtonRxL, 0)
  For x = 0 To CountGadgetItems(#ListIconQueue)
    SetGadgetItemText(#ListIconQueue, x, Str(x+1), 0)
  Next  
EndProcedure
Procedure GadgetsUpdate()
  If OpenDatabase(0, dbFile, "", "")
    If DatabaseQuery(0, "SELECT DISTINCT(orgName) FROM provisors")
      ClearGadgetItems(#ComboOrganisationName)
      While NextDatabaseRow(0)
        AddGadgetItem(#ComboOrganisationName, -1, GetDatabaseString(0, 0))
      Wend
      FinishDatabaseQuery(0)
    EndIf
    If DatabaseQuery(0, "SELECT provisorName, CSNumber, provisorTNumber FROM provisors")
      ClearGadgetItems(#ComboProvisor) : ClearGadgetItems(#ComboCSName): ClearGadgetItems(#ComboProvisorTNumber)
      While NextDatabaseRow(0)
        AddGadgetItem(#ComboProvisor, -1, GetDatabaseString(0, 0))
        AddGadgetItem(#ComboCSName, -1, GetDatabaseString(0,1))
        AddGadgetItem(#ComboProvisorTNumber, -1, GetDatabaseString(0,2))
      Wend
      FinishDatabaseQuery(0)
    EndIf
    CloseDatabase(0)
  Else
    Debug "Can't open database !"
  EndIf
EndProcedure
Procedure FixAdOps(Gadget, opName.s)
  mode.s
  If GetGadgetState(Gadget) = 1 : mode = opname+"_on" : Else : mode = opname+"_off" : EndIf
  ts = Date()*1000
  cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
  vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
  query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', '"+mode+"', '"+vCategory+"','"+Str(ts)+"')"
  FillState(query)
EndProcedure

If CreatePopupMenu(#PopupMenu)
  MenuItem(#MenuY, "Y")
  MenuItem(#MenuA, "A")
  MenuItem(#MenuE, "E")
  MenuBar()
  ;   OpenSubMenu("Options")
  ;     MenuItem(4, "Window...")
  ;     MenuItem(5, "Gadget...")
  ;   CloseSubMenu()
  MenuItem(#MenuCount, "Ввести количество товаров")
  DisableMenuItem(#PopupMenu, 4, 1)
EndIf


OpenWindowMain() : StickyWindow(#WindowMain, 1)
SetWindowTitle(#WindowMain, "NS "+NS_version+"."+Str(#Pb_editor_BuildCount)+"."+Str(#Pb_Editor_CompileCount))

AddKeyboardShortcut(#WindowMain, #PB_Shortcut_Escape, #ExitFromProgramm)
AddKeyboardShortcut(#WindowMain, #PB_Shortcut_F1, #ButtonRx)
AddKeyboardShortcut(#WindowMain, #PB_Shortcut_F2, #ButtonNotRx)
AddKeyboardShortcut(#WindowMain, #PB_Shortcut_F3, #ButtonIMT)
AddKeyboardShortcut(#WindowMain, #PB_Shortcut_Return, #bExit)

For x = #b02 To #b20: DisableGadget(x, 1) : Next
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
  
  
  If (Event = #PB_Event_Gadget And (Type = #PB_EventType_LeftClick Or Type = #PB_EventType_RightClick Or Type = #PB_EventType_Change)) Or Event = #PB_Event_Menu
    If Menu = #ExitFromProgramm : End : EndIf
    ;{ заставляем ввести провизора при отстуствии такового в #ComboProvisor
    If GetGadgetText(#ComboProvisor) = "" And GetGadgetState(#Panel) = 0 And Type = #PB_EventType_LeftClick; And Gadget <> #Panel
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
    If Event = #PB_Event_Menu And Menu <> #MenuA And Menu <> #MenuE And Menu <> #MenuY And Menu <> #MenuCount
      Gadget = Menu
      If Gadget <> #ComboProvisor
        SetGadgetState(Gadget, 1)
      EndIf
    EndIf
    ;}
    
    Select Gadget
      Case  #ListIconQueue
        If Type = #PB_EventType_RightClick And CountGadgetItems(#ListIconQueue) > 0 And GetGadgetState(#ListIconQueue) <> -1
          DisplayPopupMenu(#PopupMenu, WindowID(#WindowMain))
        EndIf
      Case #ComboOrganisationName, #ComboCSName, #ComboProvisorTNumber
        ;{ autofilling gadgets in panel01
        If Gadget = #ComboOrganisationName; And Type = #PB_EventType_LeftClick
          query = "SELECT DISTINCT(CSNumber) FROM provisors WHERE orgName = '"+GetGadgetText(Gadget)+"'"
          If OpenDatabase(0, dbFile, "", "")
            If DatabaseQuery(0, query)
              ;ClearGadgetItems(Gadget)
              While NextDatabaseRow(0)
                Debug GetDatabaseString(0, 0)
                ;                AddGadgetItem(#ComboOrganisationName, -1, GetDatabaseString(0, 0))
              Wend
              FinishDatabaseQuery(0)
            EndIf
            CloseDatabase(0)
          Else
            Debug "Can't open database !"
          EndIf
        EndIf
        If Gadget = #ComboCSName
          query = "SELECT CSCategory, CSAddres, CSDescription FROM provisors WHERE CSNumber = '"+GetGadgetText(Gadget)+"'"
          If OpenDatabase(0, dbFile, "", "")
            If DatabaseQuery(0, query)
              While NextDatabaseRow(0)
                SetGadgetText(#ComboCSCategory, GetDatabaseString(0, 0))
                SetGadgetText(#StringCSAddres, GetDatabaseString(0, 1))
                SetGadgetText(#EditorCSDescription, GetDatabaseString(0, 2))
              Wend
              FinishDatabaseQuery(0)
            EndIf
            CloseDatabase(0)
          Else
            Debug "Can't open database !"
          EndIf
        EndIf
        ; provisorSex TEXT, provisorAge INT,  provisorEducation TEXT, provisorCategory INT, provisorDescription TEXT, provisorExpCont INT, provisorExpGeneral INT
        If Gadget = #ComboProvisorTNumber
          query = "SELECT provisorName, provisorSex, provisorAge,  provisorEducation, provisorCategory, provisorDescription, provisorExpCont, provisorExpGeneral FROM provisors WHERE provisorTNumber = '"+GetGadgetText(Gadget)+"'"
          If OpenDatabase(0, dbFile, "", "")
            If DatabaseQuery(0, query)
              While NextDatabaseRow(0)
                SetGadgetText(#StringProvisorName, GetDatabaseString(0, 0))
                SetGadgetText(#ComboProvisorSex, GetDatabaseString(0, 1))
                SetGadgetText(#StringProvisorAge, GetDatabaseString(0, 2))
                SetGadgetText(#ComboProvisorEducation, GetDatabaseString(0, 3))
                SetGadgetText(#ComboProvisorCategory, GetDatabaseString(0, 4))
                SetGadgetText(#EditorProvisorDescription, GetDatabaseString(0, 5))
                SetGadgetText(#StringProvisorExpCont, GetDatabaseString(0, 6))
                SetGadgetText(#StringProvisorExpGeneral, GetDatabaseString(0, 7))
              Wend
              FinishDatabaseQuery(0)
            EndIf
            CloseDatabase(0)
          Else
            Debug "Can't open database !"
          EndIf
        EndIf        
        ;}
      Case #MenuA : SetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), "A", 2)
      Case #MenuE : SetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), "E", 2)
      Case #MenuY : SetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), "Y", 2)
        ;---------------------------------
      Case #b00 ; постановка в очередь
        ts = Date()*1000+ElapsedMilliseconds()-sET
        AddGadgetItem(#ListIconQueue, -1, Str(CountGadgetItems(#ListIconQueue)+1)+Chr(10)+Str(ts))
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b00', '"+vCategory+"','"+Str(ts)+"')"
        FillState(query)
      Case #b01 ; поступил на обслуживание
        ts = Date()*1000+ElapsedMilliseconds()-sET
        If CountGadgetItems(#ListIconQueue) = 0
          AddGadgetItem(#ListIconQueue, -1, Str(CountGadgetItems(#ListIconQueue)+1)+Chr(10)+Str(ts))
        EndIf
        If (GetGadgetState(#ListIconQueue) = -1) : SetGadgetState(#ListIconQueue, 0) : EndIf
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b01', '"+vCategory+"','"+Str(ts)+"')"
        FillState(query)
      Case #b02
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b02', '"+vCategory+"','"+Str(ts)+"')"
        FillState(query)
      Case #b03
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b03', '"+vCategory+"','"+Str(ts)+"')"
        FillState(query)
      Case #b04
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b04', '"+vCategory+"','"+Str(ts)+"')"
        FillState(query)
      Case #b05
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b05', '"+vCategory+"','"+Str(ts)+"')"
        FillState(query)
      Case #b06
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b06', '"+vCategory+"','"+Str(ts)+"')"
        FillState(query)
      Case #b07
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b07', '"+vCategory+"','"+Str(ts)+"')"
        FillState(query)
      Case #b08
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b08', '"+vCategory+"','"+Str(ts)+"')"
        FillState(query)
      Case #b09
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b09', '"+vCategory+"','"+Str(ts)+"')"
        FillState(query)
      Case #b10
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b10', '"+vCategory+"','"+Str(ts)+"')"
        FillState(query)        
      Case #b11
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b11', '"+vCategory+"','"+Str(ts)+"')"
        FillState(query)
      Case #b12
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b12', '"+vCategory+"','"+Str(ts)+"')"
        FillState(query)
      Case #b13
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b13', '"+vCategory+"','"+Str(ts)+"')"
        FillState(query)
      Case #b14
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b14', '"+vCategory+"','"+Str(ts)+"')"
        FillState(query)        
      Case #b15
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b15', '"+vCategory+"','"+Str(ts)+"')"
        FillState(query)
      Case #b16
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b16', '"+vCategory+"','"+Str(ts)+"')"
        FillState(query)
      Case #b17
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b17', '"+vCategory+"','"+Str(ts)+"')"
        FillState(query)        
      Case #b18
        If MessageRequester("---","Расчет осуществлен при помощи наличных денеждных значков?", #MB_YESNO | #MB_ICONQUESTION) = #PB_MessageRequester_Ok
          ts = Date()*1000
          cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
          query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b18a', '"+vCategory+"','"+Str(ts)+"')"
          FillState(query)
        Else
          ts = Date()*1000
          cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
          query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b18b', '"+vCategory+"','"+Str(ts)+"')"
          FillState(query)
        EndIf
      Case #b19
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b19', '"+vCategory+"','"+Str(ts)+"')"
        FillState(query)
      Case #b20
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'b20', '"+vCategory+"','"+Str(ts)+"')"
        FillState(query)          
      Case #bExit
        ts = Date()*1000
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1)) : vCategory = GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 2)
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, visitorCategory, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'END', '"+vCategory+"','"+Str(ts)+"')"
        FillState(query)
        EndMaintaince()        
        SetGadgetState(#b01, 0)
        SetGadgetState(#ListIconQueue, 0)
        ;--------------------------------------------------------
      Case #bops01 : FixAdOps(Gadget, "bops01")
      Case #bops02 : FixAdOps(Gadget, "bops02")
      Case #bops03 : FixAdOps(Gadget, "bops03")
      Case #bops04 : FixAdOps(Gadget, "bops04")
      Case #bops05 : FixAdOps(Gadget, "bops05")
      Case #bops06 : FixAdOps(Gadget, "bops06")
      Case #bops07 : FixAdOps(Gadget, "bops07")
      Case #bops08 : FixAdOps(Gadget, "bops08")
      Case #bops09 : FixAdOps(Gadget, "bops09")
      Case #bops10 : FixAdOps(Gadget, "bops10")
      Case #bops11 : FixAdOps(Gadget, "bops11")
      Case #bops12 : FixAdOps(Gadget, "bops12")
      Case #bops13 : FixAdOps(Gadget, "bops13")
      Case #bops14 : FixAdOps(Gadget, "bops14")
      Case #bops15 : FixAdOps(Gadget, "bops15")
      Case #bops16 : FixAdOps(Gadget, "bops16")
        ; -------------------------------------------------------  
      Case #ButtonRx
        If GetGadgetState(#ButtonRx) = 0 : i = -1 : Else : i = 1 : EndIf
        ;If GetGadgetState(#ButtonRx) = 0 : SetGadgetState(#ButtonRxL, 0) : EndIf
        Debug 111111111111
        cMode = cMode + i
        EDnableRx(i)
      Case #ButtonRx2
        If GetGadgetState(#ButtonRx2) = 0 : i = -1 : Else : i = 1 : EndIf       
        cMode = cMode + i*2
        EDnableRx(i)
      Case #ButtonRx3
        If GetGadgetState(#ButtonRx3) = 0 : i = -1 : Else : i = 1 : EndIf
        cMode = cMode + i*4
        EDnableRx(i)
      Case #ButtonRxL
        If GetGadgetState(#ButtonRxL) = 0 : i = -1 : Else : i = 1 : EndIf
        cMode = cMode + i*8
        EDnableRxL(i)
      Case #ButtonNotRx
        If GetGadgetState(#ButtonNotRx) = 0 : i = -1 : Else : i = 1 : EndIf
        cMode = cMode + i*16
        EDnableCommon(i)
        If(state = -1) : DisableGadget(#b14, 1) : Else : DisableGadget(#b14, 0) : EndIf
      Case #ButtonIMT
        If GetGadgetState(#ButtonIMT) = 0 : i = -1 : Else : i = 1 : EndIf
        cMode = cMode + i*32
        EDnableIMT(i)

        ;-------------------------------------------------------------
      Case #ComboProvisor
        sessID = Date()
        SetWindowTitle(#WindowMain, "NS "+NS_version+"."+Str(#Pb_editor_BuildCount)+"."+Str(#Pb_Editor_CompileCount)+" - "+GetGadgetText(#ComboProvisor))
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
          query.s = "INSERT INTO provisors (orgName, CSNumber, CSCategory, CSAddres, CSDescription, provisorName, provisorTNumber, provisorSex, provisorAge, provisorEducation, provisorCategory, provisorDescription, provisorExpCont, provisorExpGeneral)"+
                    " VALUES ('"+GetGadgetText(#ComboOrganisationName)+"', '"+GetGadgetText(#ComboCSName)+"', '"+GetGadgetText(#ComboCSCategory)+"', "+
                    "'"+GetGadgetText(#StringCSAddres)+"', '"+GetGadgetText(#EditorCSDescription)+"','"+GetGadgetText(#StringProvisorName)+"',"+
                    "'"+GetGadgetText(#ComboProvisorTNumber)+"', '"+GetGadgetText(#ComboProvisorSex)+"', '"+GetGadgetText(#StringProvisorAge)+"', '"+GetGadgetText(#ComboProvisorEducation)+"', '"+GetGadgetText(#ComboProvisorCategory)+"', '"+GetGadgetText(#EditorProvisorDescription)+"', '"+GetGadgetText(#StringProvisorExpCont)+"', '"+GetGadgetText(#StringProvisorExpGeneral)+"')"
          FillState(query)
          Debug query
          SetGadgetText(#ComboOrganisationName, "")  : SetGadgetText(#ComboCSName, "")
          SetGadgetText(#ComboCSCategory, "")        : SetGadgetText(#StringCSAddres, "")
          SetGadgetText(#EditorCSDescription, "")    : SetGadgetText(#StringProvisorName, "")
          SetGadgetText(#ComboProvisorTNumber, "")   : SetGadgetText(#ComboProvisorSex, "")
          SetGadgetText(#StringProvisorAge, "")      : SetGadgetText(#ComboProvisorEducation, "")
          SetGadgetText(#ComboProvisorCategory, "")  : SetGadgetText(#EditorProvisorDescription, "")
          SetGadgetText(#StringProvisorExpCont, "")  : SetGadgetText(#StringProvisorExpGeneral, "")
        EndIf
        GadgetsUpdate()
    EndSelect
    If cMode = 0 : For x = #b02 To #b20: DisableGadget(x, 1) : Next : EndIf  
    If GetGadgetState(#ButtonRx) : EDnableRx(1) : EndIf
    If GetGadgetState(#buttonRx) = 0 And GetGadgetState(#buttonRx2) = 0 And GetGadgetState(#buttonRx3) = 0 And GetGadgetState(#buttonNotRX) = 0 And GetGadgetState(#buttonRxL) = 0 And GetGadgetState(#buttonIMT) = 0
      For x = #b02 To #b20: DisableGadget(x, 1) : SetGadgetState(x, 0) : Next
    EndIf
  EndIf
  
  If CountGadgetItems(#ListIconQueue) = 0 : DisableGadget(#b00, 1) : Else : DisableGadget(#b00, 0) : EndIf
  If cMode = 0 : EDnableCommon(1) : Else : EDnableCommon(0) : EndIf
  
Until Event=#PB_Event_CloseWindow
; IDE Options = PureBasic 5.20 LTS (Windows - x86)
; CursorPosition = 61
; Folding = 1I+
; EnableUnicode
; EnableXP
; EnableCompileCount = 29
; EnableBuildCount = 0
; IncludeVersionInfo