--[[
	Assign value to property

	RailCOMPLETE(R) script, (C) Railcomplete AS 2021
	
	2021-11-01_000 CLFEY Created

	Description:
	* Note: Lua accepts both " and ' around strings. We suggest using single quote ' for Lua,
	and use double quotes " inside Lua strings to represent quoted things for AutoCAD.
	
	* To find out whether a specific property in an object contains a Lua formula at present,
	read the obj.luaExpressions and filter on the property you are interested in:
	
	obj.LuaExpressions:filter(function(x) return x.Name:lower() == "code" end).count > 0
	
	* To replace an existing Lua formula in a property, assign '=newLuaExpression' to it
	* To remove an existing Lua formula in a property, assign '=' to it (equality sign)
	* To place a value in a property which does not contain a Lua formula, assign directly.
--]]

function writeln(t) return t and write(t..'\n') or write('\n') end
function identify(x) return (x.name and x.name) or (x.code and x.code or x.id) end
newCode = askForString('Please enter new code (a string)')
obj = askForObject('Select object')
writeln('You selected object '..obj.RcType..' : '..identify(obj)) --(identify by name or code or id)
oldCode = obj.code
obj.code = '='
obj.code = newCode
writeln('Code changed from '..oldCode..' to '..obj.code)
