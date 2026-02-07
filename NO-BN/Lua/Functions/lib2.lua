--[[
	lib2.lua
	========
	Lua library functions callable from a Lua script context.
	
	The base folder for includeLuaFile() calls is: "...\RC.bundle\Adm\XX-YY", where 'XX-YY'
	is your railway administration's DNA abbreviation.

	Usage: 
	
	--Include library functions for use in a Lua script or in a Lua function:
	lib2 = includeLuaFile("Lua\\Functions\\lib2.lua")
	return lib1.p2s(getPoint3D()) --Pretty-print X Y Z coordinates to  3 decimal places.

	2026-01-30 v1.0 CLFEY Created.
--]]

_DNA_COUNTRY_ = DocumentData.DnaIri:match("^(%w%w)%-%w%w.+$")

function trace(t) if _TRACE_ then writeln(t) end end

function writeln(t, symbol) return t and write(t.."\n", symbol or _noSymbol) or write("\n", symbol or _noSymbol) end

function stop(t) writeln("ERROR : "..t) local x = ""..nil end --Provoke error here, to see Lua source code line number

function show(t, header, symbol)
	writeln(t, symbol)
	askForKeyword(t, {"OK"}, header) --Window caption becomes "Keyword" if header is nil.
end

function selectAll() runCommand('_PICKADD 2 ') return runCommand('_SELECT _AL \n').result end --Modifies PICKADD

function selectLast() return runCommand('_SELECT _L \n').result[0] end


--Returns a table with paths to shapefiles. Each entry is a table of file paths. 
function getShapefilePaths()
	local function getShapefilesInFolder(folder, shapefileTable)
		local filesInFolder = table.select(getFilesInFolder(folder))
		for _, file in pairs(filesInFolder) do
			local a = string.sub(file, #file - 3)
			if a == ".shp" then
				table.insert(shapefileTable, file)
			end
		end
		local foldersInFolder = table.select(getFoldersInFolder(folder))
		for _, f in pairs(foldersInFolder) do
			getShapefilesInFolder(f, shapefileTable)
		end
	end

	local shapefileFolder = askForFolderName(lang("Select folder containing shapefiles"))
	if not shapefileFolder then return nil end
	local t = {}
	
	getShapefilesInFolder(shapefileFolder, t)
	
	return t
end


function setDefaultCadSettings()
	runCommand("_PICKADD 0 ") -- Désactive PICKADD. Les derniers objets sélectionnés deviennent le jeu de sélection.
	runCommand("_FILEDIA 1 ") -- Ouvrez les fichiers avec une fenêtre d'explorateur normale.
	runCommand("_ORTHOMODE 0 ") -- Le curseur n'est pas verrouillé sur les lignes de la grille.
	runCommand("_GRID _OFF ") -- Ne pas afficher les lignes de la grille.
	runCommand("_SNAP _OFF ") -- Ne pas encliqueter sur la grille.
	runCommand("_PICKBOX 5 ") -- Définir la taille par défaut du curseur.
	runCommand("_NAVVCUBE _OFF ")
	runCommand("_NAVVCUBE _ON ") -- Activez le cube de navigation en haut à droite.
	runCommand("_DYNMODE 3 ") -- Afficher la saisie de texte et les commandes suggérées à côté du curseur pendant la saisie.
	runCommand("_SELECTIONCYCLING 2 ") -- Si vous cliquez sur plusieurs objets en même temps, une fenêtre apparaîtra pour vous permettre de choisir celui que vous souhaitez.
	runCommand("_-UNITS 2 3 1 3 0 _NO ") -- Utilisez des mesures avec une précision de 3 décimales. L'est est à droite, les angles sont mesurés dans le sens inverse des aiguilles d'une montre.
	runCommand("_-OSNAP _END,_MID,_CEN,_GCE,_NOD,_QUA,_INT,_EXT,_INS,_PER,_TAN,_NEA,_APP,_PAR ") -- Activer tous les OSNAP.
	runCommand("_-COLOR _BYLAYER ") -- Définir la couleur par défaut.
end


--Hide polyline grips (0=hide grips / 1=display grips / 2=display additional midpoint grips on polyline segments (default))
function gripsOff()
	runCommand("_GRIPS 0 ")
end

--Show polyline grips (0=hide grips / 1=display grips / 2=display additional midpoint grips on polyline (default))
function gripsOn()
	runCommand("_GRIPS 2 ")
end
