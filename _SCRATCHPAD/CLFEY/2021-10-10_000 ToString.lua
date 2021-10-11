function RC__CollectionToString(c,separator)
	separator = separator and ", "
	first = true
	t = ""
	for i=0, getCollectionLength(c)-1 do
		item = c[i]
		itemType = type(item)
		if itemType = "string" then s = item
		elseif itemType = "
		t = t .. (not first and separator) .. RC__identify(c[i])
	end
end

return RC__CollectionToString(getBlockNames())
--return getBlockNames()
