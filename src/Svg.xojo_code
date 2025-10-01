#tag Module
Protected Module SVG
	#tag Method, Flags = &h21
		Private Sub ApplyValues(Extends Item As JSONItem, withItem As JSONItem)
		  Var i As Integer
		  
		  i = 0
		  while i < withItem.Count
		    Item.Value(withItem.Name(i)) = withItem.Value(withItem.Name(i))
		    i = i + 1
		  wend
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function buildStyleItem(node As XmlNode) As JSONItem
		  Var result as new JSONItem("{}")
		  Var i As Integer
		  Var j As Integer
		  Var xAttr As XmlAttribute
		  Var styleArr() As String
		  Var itemArr() As String
		  Var className As String
		  Var classProperties As JSONItem
		  
		  if mClasses.HasName(node.Name.Lowercase) then
		    classProperties = mClasses.Value(node.Name.Lowercase)
		    result.ApplyValues classProperties
		  end if
		  
		  i = 0
		  while i < node.AttributeCount
		    xAttr = node.GetAttributeNode(i)
		    
		    if xAttr.Name = "class" then
		      
		      //className = Trim(Lowercase(node.GetCIAttribute(xAttr.Name)))
		      className = Trim(Lowercase(node.GetAttribute(xAttr.Name)))
		      if mClasses.HasName("." + className) then
		        classProperties = mClasses.Value("." + className)
		        result.ApplyValues classProperties
		      end if
		      
		    elseif xAttr.Name = "style" then
		      
		      // process style attribute
		      
		      //styleArr = node.GetCIAttribute(xAttr.Name).Split(";")
		      styleArr = node.GetAttribute(xAttr.Name).Split(";")
		      j = 0
		      while j <= styleArr.Ubound
		        itemArr = styleArr(j).Split(":")
		        if itemArr.Ubound = 1 then
		          result.Value(itemArr(0).Trim.Lowercase) = itemArr(1)
		        end if
		        j = j + 1
		      wend
		      
		    elseif Instr(0, xAttr.Name, ":") <= 0 then
		      
		      //result.Value(xAttr.Name.Lowercase) = node.GetCIAttribute(xAttr.Name)
		      result.Value(xAttr.Name.Lowercase) = node.GetAttribute(xAttr.Name)
		      
		    end if
		    
		    i = i + 1
		  wend
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function buildTransformationMatrix(transform As String) As Double()
		  Var result() As Double = Array( _
		  1.0, 0.0, 0.0, _
		  0.0, 1.0, 0.0, _
		  0.0, 0.0, 1.0)
		  Var mulMatrix() As Double = Array( _
		  0.0, 0.0, 0.0, _
		  0.0, 0.0, 0.0, _
		  0.0, 0.0, 0.0)
		  Var pos As Integer
		  Var openBracket As Integer
		  Var closeBracket As Integer
		  Var functionName As String
		  Var parms As String
		  Var strArr() As String
		  
		  pos = 0
		  
		  do
		    pos = pos + 1
		    openBracket = Instr(pos, transform, "(")
		    if openBracket > 0 then
		      
		      closeBracket = Instr(openBracket, transform, ")")
		      if closeBracket > 0 then
		        
		        functionName = Lowercase(Trim(Mid(transform, pos, openBracket - pos)))
		        parms = Mid(transform, openBracket + 1, closeBracket - openBracket - 1)
		        parms = parms.ReplaceAll(",", " ")
		        while parms.InStr(0, "  ") > 0 
		          parms = parms.ReplaceAll("  ", " ")
		        wend
		        strArr = parms.Split(" ")
		        
		        select case functionName
		          
		        case "matrix"
		          if strArr.Ubound = 5 then
		            mulMatrix = matrix(val(strArr(0)), _ ' a
		            val(strArr(1)), _ ' b
		            val(strArr(2)), _ ' c
		            val(strArr(3)), _ ' d
		            val(strArr(4)), _ ' e
		            val(strArr(5)) ) ' f
		            result = matrixMultiply(result, mulMatrix)
		          end if
		          
		        case "rotate"
		          if strArr.Ubound = 0 then // around origin
		            mulMatrix = rotationMatrix(val(strArr(0)))
		            result = matrixMultiply(result, mulMatrix)
		          elseif strArr.Ubound = 2 then // around point
		            mulMatrix = translationMatrix(val(strArr(1)), val(strArr(2)))
		            result = matrixMultiply(result, mulMatrix)
		            mulMatrix = rotationMatrix(val(strArr(0)))
		            result = matrixMultiply(result, mulMatrix)
		            mulMatrix = translationMatrix(-val(strArr(1)), -val(strArr(2)))
		            result = matrixMultiply(result, mulMatrix)
		          end if
		          
		        case "scale"
		          if strArr.Ubound >= 1 then
		            mulMatrix = scaleMatrix(val(strArr(0)), val(strArr(1)))
		          else
		            mulMatrix = scaleMatrix(val(strArr(0)), val(strArr(0)))
		          end if
		          result = matrixMultiply(result, mulMatrix)
		          
		        case "skewx"
		          if strArr.Ubound >= 0 then
		            mulMatrix = skewXMatrix(val(strArr(0)))
		            result = matrixMultiply(result, mulMatrix)
		          end if
		          
		        case "skewy"
		          if strArr.Ubound >= 0 then
		            mulMatrix = skewYMatrix(val(strArr(0)))
		            result = matrixMultiply(result, mulMatrix)
		          end if
		          
		        case "translate"
		          if strArr.Ubound >= 1 then
		            mulMatrix = translationMatrix(val(strArr(0)), val(strArr(1)))
		          else
		            mulMatrix = translationMatrix(val(strArr(0)), 0)
		          end if
		          result = matrixMultiply(result, mulMatrix)
		          
		        end select
		        
		        pos = closeBracket
		      else
		        pos = 0
		      end if
		      
		    else
		      pos = 0
		    end if
		    
		  loop until (pos >= Len(transform)) or (pos = 0)
		  
		  
		  return result
		  
		  
		  
		  
		  'return resultMatrix
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function colorFromHex(s As String) As Color
		  Var result As Color
		  Var colVariant As Variant
		  Var tmpStr As String
		  
		  if Left(s, 1) = "#" then
		    tmpStr = Right(s, Len(s) - 1)
		  else
		    tmpStr = s
		  end if
		  
		  if Len(tmpStr) = 3 then
		    tmpStr = Left(tmpStr, 1) + Left(tmpStr, 1) + Mid(tmpStr, 2, 1) + Mid(tmpStr, 2, 1) + Right(tmpStr, 1) + Right(tmpStr, 1)
		  end if
		  
		  tmpStr = "&c" + tmpStr
		  
		  colVariant = tmpStr
		  result = colVariant.ColorValue
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function determineColor(s As String) As Color
		  Var col As Color
		  Var colStr As String
		  Var startPos As Integer
		  Var endPos As Integer
		  Var tmpStr As String
		  Var tmpArr() As String
		  
		  Static ColorTable As new Dictionary("aliceblue" : &cf0f8ff, "antiquewhite" : &cfaebd7, "aqua" : &c00ffff, "azure" : &c0fffff,  _
		  "beige": &cf5f5dc, "bisque" : &cffe4c4, "black" : &c000000, "blanchedalmond" : &cffebcd, "blue" : &c0000ff, _
		  "blueviolet" : &c8a2be2, "brown" : &ca52a2a, "burlywood" : &cdeb887, "cadetblue" : &c5f9ea0, "chartreuse" : &c7fff00, _
		  "chocolate" : &cd2691e, "coral" : &cff7f50, "cornflowerblue" : &c6495ed, "cornsilk" : &cfff8dc, "crimson" : &cdc143c, _
		  "cyan" : &c00ffff, "darkblue" : &c00008b, "darkcyan" : &c008b8b, "darkgoldenrod" : &cb8860b, "darkgray" : &ca9a9a9, _
		  "darkgreen" : &c006400, "darkgrey" : &ca9a9a9, "darkkhaki" : &cbdb76b, "darkmagenta" : &c8b008b, "darkolivegreen" : &c556b2f, _
		  "darkorange" : &cff8c00, "darkorchid" : &c9932cc, "darkred" : &c8b0000, "darksalmon" : &c39967a, "darkseagreen" : &c8fbc8f, _
		  "darkslateblue" : &c483d8b, "darkslategray" : &c2f4f4f, "darkslategrey" : &c2f4f4f, "darkturquoise" : &c00ced1, _
		  "darkviolet" : &c9400d3, "deeppink" : &cff1493, "deepskyblue" : &c00bfff, "dimgray" : &c696969, "dimgrey" : &c696969, _
		  "dodgerblue" : &c1e90ff, "firebrick" : &cb22222, "floralwhite" : &cfffaf0, "forestgreen" : &c228b22, "fuchsia" : &cff00ff, _
		  "gainsboro" : &cdcdcdc, "ghostwhite" : &cf8f8ff, "gold" : &cffd700, "goldenrod" : &cdaa520, "gray" : &c8080, "grey" : &c8080, _
		  "green" : &c008000, "greenyellow" : &cadff2f, "honeydew" : &cf0fff0, "hotpink" : &cff69b4, "indianred" : &ccd5c5c, _
		  "indigo" : &c4b0082, "ivory" : &cfffff0, "khaki" : &cf0e68c, "lavender" : &ce6e6fa, "lavenderblush" : &cfff0f5, _
		  "lawngreen" : &c7cfc00, "lemonchiffon" : &cfffacd, "lightblue" : &cadd8e6, "lightcoral" : &cf08080, "lightcyan" : &ce0ffff, _
		  "lightgoldenrodyellow" : &cfafad2, "lightgray" : &cd3d3d3, "lightgreen" : &c90ee90, "lightgrey" : &cd3d3d3, _
		  "lightpink" : &cffb6c1, "lightsalmon" : &cffa07a, "lightseagreen" : &c20b2aa, "lightskyblue" : &c87cefa, "lightslategray" : &c778899, _
		  "lightslategrey" : &c778899, "lightsteelblue" : &cb0c4de, "lightyellow" : &cffffe0, "lime" : &c00ff00, "limegreen" : &c32cd32, _
		  "linen" : &cfaf0e6, "magenta" : &cff00ff, "maroon" : &c800000, "mediumaquamarine" : &c66cdaa, "mediumblue" : &c0000cd, _
		  "mediumorchid" : &cba55d3, "mediumpurple" : &c9370db, "mediumseagreen" : &c3cb371, "mediumslateblue" : &c7b68ee, _
		  "mediumspringgreen" : &c00fa9a, "mediumturquoise" : &c48d1cc, "mediumvioletred" : &cc71585, "midnightblue" : &c191970, _
		  "mintcream" : &cf5fffa, "mistyrose" : &cffe4e1, "moccasin" : &cffe4b5, "navajowhite" : &cffdead, "navy" : &c000080, _
		  "oldlace" : &cfdf5e6, "olive" : &c808000, "olivedrab" : &c6b8e23, "orange" : &cffa500, "orangered" : &cff4500, "orchid" : &cda70d6, _
		  "palegoldenrod" : &ceee8aa, "palegreen" : &c98fb98, "paleturquoise" : &cafeeee, "palevioletred" : &cdb7093, _
		  "papayawhip" : &cffefd5, "peachpuff" : &cffdab9, "peru" : &ccd853f, "pink" : &cffc0cb, "plum" : &cdda0dd, _
		  "powderblue" : &cb0e0e6, "purple" : &c800080, "red" : &cff0000, "rosybrown" : &cbc8f8f, "royalblue" : &c4169e1, _
		  "saddlebrown" : &c8b4513, "salmon" : &cfa8072, "sandybrown" : &cf4a460, "seagreen" : &c2e8b57, "seashell" : &cfff5ee, _
		  "sienna" : &ca0522d, "silver" : &cc0c0c0, "skyblue" : &c87ceeb, "slateblue" : &c6a5acd, "slategray" : &c708090, _
		  "slategrey" : &c708090, "snow" : &cfffafa, "springgreen" : &c00ff7f, "steelblue" : &c4682b4, "tan" : &cd2b4bc, "teal" : &c008080, _
		  "thistle" : &cd8bfd8, "tomato" : &cff6347, "turquoise" : &c40e0d0, "violet" : &cee82ee, "wheat" : &cf5deb3, "white" : &cffffff, _
		  "whitesmoke" : &cf5f5f5, "yellow" : &cffff00, "yellowgreen" : &c9acd32)
		  
		  colStr = Lowercase(Trim(s))
		  
		  if ColorTable.HasKey(colStr) then
		    col = ColorTable.Value(colStr)
		  elseif left(colStr, 3) = "rgb" then
		    startPos = Instr(0, colStr, "(")
		    endPos = Instr(startPos + 1, colStr, ")")
		    if (startPos > 0) and (endPos > 0) then
		      tmpStr = Mid(colStr, startPos + 1, endPos - startPos - 1)
		      tmpArr = tmpStr.Split(",")
		      if tmpArr.Ubound = 2 then
		        col = RGB(Val(tmpArr(0)), Val(tmpArr(1)), Val(tmpArr(2)))
		      end if
		    end if
		  else
		    col = colorFromHex(colStr)
		  end if
		  
		  return col
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DrawSVG(Extends g As Graphics, svg As String, x As Integer, y As Integer, w1 As Integer = -10000, h1 As Integer = -10000, sx As Integer = 0, sy As Integer = 0, w2 As Integer = -10000, h2 As Integer = -10000)
		  Var xdoc As XmlDocument
		  Var e As SVG.SVGException
		  
		  if svg.Length > 0 then
		    
		    try
		      
		      xdoc = new XmlDocument(svg)
		      renderXML g, xdoc, x, y, w1, h1, sx, sy, w2, h2
		      
		    catch xmlException As XmlException
		      
		      // invalid xml, so raise an exception
		      
		      e = new SVG.SVGException()
		      e.ErrorNumber = Integer(SVGErrorEnum.MalformedXML)
		      e.Message = "Malformed XML."
		      Raise e
		      
		    end try
		    
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function identityMatrix() As Double()
		  Var result() As Double = Array( _
		  1.0, 0.0, 0.0, _
		  0.0, 1.0, 0.0, _
		  0.0, 0.0, 1.0)
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function LookupDouble(Extends item As JSONItem, name As String, defaultValue As Double = 0) As Double
		  return Item.Lookup(name, defaultValue)
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function LookupString(Extends item As JSONItem, name As String, defaultValue As String = "") As String
		  return item.Lookup(name, defaultValue)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function matrix(a As Double, b As Double, c As Double, d As Double, e As Double, f As Double) As Double()
		  Var result() As Double = Array( _
		  a, c, e, _
		  b, d, f, _
		  0.0, 0.0, 1.0)
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function matrixMultiply(m1() As Double, m2() As Double) As Double()
		  Var result(8) As Double
		  
		  result(0) = m1(0) * m2(0) + m1(1) * m2(3) + m1(2) * m2(6)
		  result(1) = m1(0) * m2(1) + m1(1) * m2(4) + m1(2) * m2(7)
		  result(2) = m1(0) * m2(2) + m1(1) * m2(5) + m1(2) * m2(8)
		  
		  result(3) = m1(3) * m2(0) + m1(4) * m2(3) + m1(5) * m2(6)
		  result(4) = m1(3) * m2(1) + m1(4) * m2(4) + m1(5) * m2(7)
		  result(5) = m1(3) * m2(2) + m1(4) * m2(5) + m1(5) * m2(8)
		  
		  result(6) = m1(6) * m2(0) + m1(7) * m2(3) + m1(8) * m2(6)
		  result(7) = m1(6) * m2(1) + m1(7) * m2(4) + m1(8) * m2(7)
		  result(8) = m1(6) * m2(2) + m1(7) * m2(5) + m1(8) * m2(8)
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub renderNode(node As XmlNode, g As Graphics, parentMatrix() As Double, parentStyle As JSONItem)
		  Var e As SVG.SVGException
		  
		  select case node.Name
		    
		    //case "#comment"
		    //// we ignore xml comments
		    
		  case "circle"
		    render_circle(node, g, parentMatrix, parentStyle)
		    
		    //case "defs"
		    //// we ignore these tags
		    //
		    //case "desc"
		    //// we ignore these tags
		    //
		    //case "ellipse"
		    //render_ellipse(node, g, parentMatrix, parentStyle)
		    //
		    //case "g"
		    //render_g(node, g, parentMatrix, parentStyle)
		    //
		    //case "image"
		    //render_image(node, g, parentMatrix, parentStyle)
		    //
		    //case "line"
		    //render_line(node, g, parentMatrix, parentStyle)
		    //
		    //case "metadata"
		    //// we ignore these tags
		    //
		    //case "path"
		    //render_path(node, g, parentMatrix, parentStyle)
		    //
		    //case "polygon"
		    //render_polygon(node, g, parentMatrix, parentStyle)
		    //
		    //case "polyline"
		    //render_polyline(node, g, parentMatrix, parentStyle)
		    //
		    //case "rect"
		    //render_rect(node, g, parentMatrix, parentStyle)
		    //
		    //case "style"
		    //process_style(node)
		    
		  case "svg"
		    render_svg(node, g, parentMatrix, parentStyle)
		    
		    //case "text"
		    //render_text(node, g, parentMatrix, parentStyle)
		    //
		    //case "title"
		    //// we ignore these tags
		    
		  case else
		    
		    // we only want to raise the unknown element exception during debugging,
		    // and during runtime we simply ignore unknown elements
		    
		    #if DebugBuild then
		      e = new SVG.SVGException()
		      e.ErrorNumber = 4
		      e.Message = "Unknown element: " + node.Name
		      Raise e
		    #endif
		    
		  end select
		  
		  
		  //if node.Name.Left(9) = "sodipodi:" then
		  //// we ignore sodipodi tags
		  //
		  //else
		  //
		  //select case node.Name
		  //
		  //case "#comment"
		  //// we ignore xml comments
		  //
		  //case "circle"
		  //render_circle(node, g, parentMatrix, parentStyle)
		  //
		  //case "defs"
		  //// we ignore these tags
		  //
		  //case "desc"
		  //// we ignore these tags
		  //
		  //case "ellipse"
		  //render_ellipse(node, g, parentMatrix, parentStyle)
		  //
		  //case "g"
		  //render_g(node, g, parentMatrix, parentStyle)
		  //
		  //case "image"
		  //render_image(node, g, parentMatrix, parentStyle)
		  //
		  //case "line"
		  //render_line(node, g, parentMatrix, parentStyle)
		  //
		  //case "metadata"
		  //// we ignore these tags
		  //
		  //case "path"
		  //render_path(node, g, parentMatrix, parentStyle)
		  //
		  //case "polygon"
		  //render_polygon(node, g, parentMatrix, parentStyle)
		  //
		  //case "polyline"
		  //render_polyline(node, g, parentMatrix, parentStyle)
		  //
		  //case "rect"
		  //render_rect(node, g, parentMatrix, parentStyle)
		  //
		  //case "style"
		  //process_style(node)
		  //
		  //case "svg"
		  //render_svg(node, g, parentMatrix, parentStyle)
		  //
		  //case "text"
		  //render_text(node, g, parentMatrix, parentStyle)
		  //
		  //case "title"
		  //// we ignore these tags
		  //
		  //case else
		  //
		  //// we only want to raise the unknown element exception during debugging
		  //// during runtime we simply ignore unknown elements
		  //
		  //#if DebugBuild then
		  //e = new DrawSVG.SVGException()
		  //e.ErrorNumber = 4
		  //e.Message = "Unknown element: " + node.Name
		  //Raise e
		  //#endif
		  //
		  //end select
		  //
		  //end if
		  //
		  //
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub renderXML(g As Graphics, xdoc As XmlDocument, x As Integer, y As Integer, w1 As Integer = -10000, h1 As Integer = -10000, sx As Integer = 0, sy As Integer = 0, w2 As Integer = -10000, h2 As Integer = -10000)
		  Var i As Integer
		  Var matrix() As Double
		  Var w As Double
		  Var h As Double
		  Var wStr As String
		  Var hStr As String
		  Var svgImage As Picture
		  
		  mClasses = new JSONItem("{}")
		  matrix = identityMatrix()
		  
		  i = 0
		  while (i < xdoc.ChildCount) 
		    if xdoc.Child(i).Name = "svg" then
		      
		      // determine graphics context width and height
		      
		      w = 0
		      h = 0
		      
		      //wStr = Trim(xdoc.Child(i).GetCIAttribute("width"))
		      if wStr <> "" then
		        if IsNumeric(wStr) then
		          w = Val(wStr)
		        elseif Right(wStr, 1) = "%" then
		          w = g.Width * (Val(Left(wStr, Len(wStr) - 1)) / 100)
		        end if
		      end if
		      
		      //hStr = Trim(xdoc.Child(i).GetCIAttribute("height"))
		      hStr = Trim(xdoc.Child(i).GetAttribute("width"))
		      if hStr <> "" then
		        if IsNumeric(hStr) then
		          h = Val(hStr)
		        elseif Right(hStr, 1) = "%" then
		          h = g.Height * (Val(Left(hStr, Len(hStr) - 1)) / 100)
		        end if
		      end if
		      
		      if w = 0 then
		        w = g.Width
		      end if
		      if h = 0 then
		        h = g.Height
		      end if
		      
		      //// apply viewbox if there is one
		      //
		      //viewbox = Trim(xdoc.Child(i).GetCIAttribute("viewbox"))
		      //if viewbox <> "" then
		      //while viewbox.InStr(0, "  ") > 0 
		      //viewbox = viewbox.ReplaceAll("  ", " ")
		      //wend
		      //viewboxArr = viewbox.Split(" ")
		      //if viewboxArr.Ubound = 3 then
		      //xScale = w / Val(viewboxArr(2)) 
		      //yScale = h / Val(viewboxArr(3)) 
		      //if xScale < yScale then
		      //scale = xScale
		      //xOffset = 0
		      //yOffset = (h - (Val(viewboxArr(3))  * scale)) / 2
		      //else
		      //scale = yScale
		      //xOffset = (w - (Val(viewboxArr(2))  * scale)) / 2
		      //yOffset = 0
		      //end if
		      //mulMatrix = initTranslationMatrix(xOffset, yOffset)
		      //matrix = matrixMultiply(matrix, mulMatrix)
		      //mulMatrix = initScaleMatrix(scale, scale)
		      //matrix = matrixMultiply(matrix, mulMatrix)
		      //
		      //end if
		      //end if
		      
		      //' Smoohing algoritm courtesy of Marco Hof.
		      //
		      //mulMatrix = initScaleMatrix(2, 2)
		      //matrix = matrixMultiply(matrix, mulMatrix)
		      
		      //svgImage = new Picture(w * 2, h * 2)
		      //renderNode(xdoc.Child(i), svgImage.Graphics, matrix, new JSONItem("{}"))
		      //finalImage = svgImage.ScalePicture(w, h)
		      //g.DrawPicture finalImage, x, y, w1, h1, sx, sy, w2, h2
		      
		      svgImage = new Picture(w, h)
		      renderNode(xdoc.Child(i), svgImage.Graphics, matrix, new JSONItem("{}"))
		      
		      g.DrawPicture svgImage, x, y, w1, h1, sx, sy, w2, h2
		      
		    end if
		    i = i + 1
		  wend
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_circle(node As XmlNode, g As Graphics, parentMatrix() As Double, parentStyle As JSONItem)
		  Var localStyle As JSONItem
		  Var style As JSONItem
		  Var matrix() As Double
		  Var i As Integer
		  Var tmpX As Double
		  Var tmpY As Double
		  Var cx As Double
		  Var cy As Double
		  Var r As Double
		  Var fill As String
		  Var stroke As String
		  Var strokeWidth As Double
		  Var pointCount As Integer
		  Var theta As Double
		  Var path As GraphicsPath
		  
		  style = new JSONItem("{}")
		  style.ApplyValues parentStyle
		  localStyle = buildStyleItem(node)
		  style.ApplyValues localStyle
		  matrix = buildTransformationMatrix(localStyle.Lookup("transform", ""))
		  matrix = matrixMultiply(parentMatrix, matrix)
		  
		  cx = style.LookupDouble("cx")
		  cy = style.LookupDouble("cy")
		  fill = style.LookupString("fill", "#000000")
		  if (fill <> "none") and style.HasName("fill-opacity") then
		    if Val(style.Value("fill-opacity")) = 0 then
		      fill = "none"
		    elseif Val(style.Value("fill-opacity")) = 1 then
		      // do nothing
		    else
		      'break // todo
		    end if
		  end if
		  stroke = style.LookupString("stroke", "")
		  strokeWidth = style.LookupDouble("stroke-width", 1) * matrix(0)
		  r = style.LookupDouble("r") 
		  
		  if (r > 0) then
		    
		    // build path
		    
		    path = new GraphicsPath()
		    
		    pointCount = 128
		    i = 0
		    
		    theta = Pi * (i / (pointCount / 2))
		    tmpX = cx + r * cos(theta) // center a + radius x * cos(theta)
		    tmpY = cy + r * sin(theta) // center b + radius y * sin(theta)
		    transformPoint tmpX, tmpY, matrix
		    path.MoveToPoint tmpX, tmpY
		    
		    i = 1 
		    while i <= pointCount 
		      theta = Pi * (i / (pointCount / 2))
		      
		      tmpX = cx + r * cos(theta) // center a + radius x * cos(theta)
		      tmpY = cy + r * sin(theta) // center b + radius y * sin(theta)
		      transformPoint tmpX, tmpY, matrix
		      
		      path.AddLineToPoint tmpX, tmpY
		      
		      i = i + 1
		    wend
		    
		    // fill
		    
		    if fill <> "none" then
		      g.DrawingColor = determineColor(fill)
		      g.FillPath path, true
		    end if
		    
		    // stroke
		    
		    if (stroke <> "none") and (stroke <> "") and (strokeWidth > 0) then
		      g.DrawingColor = determineColor(stroke)
		      g.PenSize = strokeWidth
		      g.DrawPath path, true
		    end if
		    
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_svg(node As XmlNode, g As Graphics, parentMatrix() As Double, parentStyle As JSONItem)
		  Var localStyle As JSONItem
		  Var style As JSONItem
		  Var matrix() As Double
		  Var i As Integer
		  
		  style = new JSONItem("{}")
		  style.ApplyValues parentStyle
		  localStyle = buildStyleItem(node)
		  style.ApplyValues localStyle
		  matrix = buildTransformationMatrix(localStyle.Lookup("transform", ""))
		  matrix = matrixMultiply(parentMatrix, matrix)
		  
		  i = 0
		  while i < node.ChildCount
		    renderNode node.Child(i), g, matrix, style
		    i = i + 1
		  wend
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function rotationMatrix(angle As Double) As Double()
		  Var result() As Double = Array( _
		  cos(angle * DegToRad), -sin(angle * DegToRad), 0.0, _
		  sin(angle * DegToRad), cos(angle * DegToRad), 0.0, _
		  0.0, 0.0, 1.0)
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function scaleMatrix(sx As Double, sy As Double) As Double()
		  Var result() As Double = Array( _
		  sx, 0.0, 0.0, _
		  0.0, sy, 0.0, _
		  0.0, 0.0, 1.0)
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function skewXMatrix(angle As Double) As Double()
		  Var result() As Double = Array( _
		  1.0, tan(angle * DegToRad), 0.0, _
		  0.0, 1.0, 0.0, _
		  0.0, 0.0, 1.0)
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function skewYMatrix(angle As Double) As Double()
		  Var result() As Double = Array( _
		  1.0, 0.0, 0.0, _
		  tan(angle * DegToRad), 1.0, 0.0, _
		  0.0, 0.0, 1.0)
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub transformPoint(ByRef x As Double, ByRef y As Double, matrix() As Double)
		  Var cx As Double
		  Var cy As Double
		  Var cw As Double
		  
		  cx = matrix(0) * x + matrix(1) * y + matrix(2)
		  cy = matrix(3) * x + matrix(4) * y + matrix(5)
		  cw = matrix(6) * x + matrix(7) * y + matrix(8)
		  
		  x = (cx / cw)
		  y = (cy / cw)
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function translationMatrix(tx As Double, ty As Double) As Double()
		  Var result() As Double = Array( _
		  1.0, 0.0, tx, _
		  0.0, 1.0, ty, _
		  0.0, 0.0, 1.0)
		  
		  return result
		  
		End Function
	#tag EndMethod


	#tag Property, Flags = &h21
		Private mClasses As JSONItem
	#tag EndProperty


	#tag Constant, Name = DegToRad, Type = Double, Dynamic = False, Default = \"0.0174533", Scope = Private
	#tag EndConstant

	#tag Constant, Name = Pi, Type = Double, Dynamic = False, Default = \"3.1415927", Scope = Private
	#tag EndConstant

	#tag Constant, Name = RadToDeg, Type = Double, Dynamic = False, Default = \"57.2958", Scope = Private
	#tag EndConstant


	#tag Enum, Name = SVGErrorEnum, Type = Integer, Flags = &h0
		MalformedXML=1
	#tag EndEnum


	#tag ViewBehavior
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Module
#tag EndModule
