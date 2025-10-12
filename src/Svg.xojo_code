#tag Module
Protected Module SVG
	#tag Method, Flags = &h21
		Private Function angleBetweenVectors(ux As Double, uy As Double, vx As Double, vy As Double) As Double
		  // Angle of vector V from the positive X-axis
		  Dim angleV As Double
		  angleV = Atan2(vy, vx) * (180 / Acos(-1)) // Convert Radians to Degrees
		  
		  // Angle of vector U from the positive X-axis
		  Dim angleU As Double
		  angleU = Atan2(uy, ux) * (180 / Acos(-1)) // Convert Radians to Degrees
		  
		  // Calculate the difference
		  Dim angleDelta As Double = angleV - angleU
		  
		  // Normalize the angle to be within [0, 360) degrees
		  // Use Modulo operator for normalization
		  angleDelta = angleDelta Mod 360
		  
		  // If the result is negative, normalize it back to positive [0, 360)
		  if angleDelta < 0 then
		    angleDelta = angleDelta + 360
		  end if
		  
		  Return angleDelta
		  
		End Function
	#tag EndMethod

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

	#tag Method, Flags = &h0
		Sub DrawSVG(Extends g As Graphics, svg As XmlDocument, x As Integer, y As Integer, w1 As Integer = -10000, h1 As Integer = -10000, sx As Integer = 0, sy As Integer = 0, w2 As Integer = -10000, h2 As Integer = -10000)
		  renderXML g, svg, x, y, w1, h1, sx, sy, w2, h2
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub DrawTransformedPicture(Extends g As Graphics, image As Picture, matrix() As Double)
		  Var srcWidth as Integer
		  Var srcHeight as Integer 
		  Var destinationQuadrilateral() as REALbasic.Point
		  Var tmpX As Integer
		  Var tmpY As Integer
		  Var startX As Integer
		  Var startY As Integer
		  Var stopX As Integer
		  Var stopY As Integer
		  Var minXY as REALbasic.Point
		  Var maxXY as REALbasic.Point
		  Var srcRect(3) as REALbasic.Point
		  Var transMatrix(8) As Double
		  Var x As Integer
		  Var y As Integer
		  Var  factor, srcX, srcY as Double
		  Var tgtPic as Picture
		  Var srcRGB as RGBSurface
		  Var tgtRGB as RGBSurface
		  Var srcWidthM1 As Integer
		  Var srcHeightM1 As Integer
		  Var dx1, dy1, dx2, dy2 as Double 'coordinates of source points
		  Var sx1, sy1, sx2, sy2 as Integer
		  Var p1,p2,p3, p4 as Color ' temporary pixels
		  Var r, gp , b, a as Integer
		  
		  srcWidth = image.Width
		  srcHeight = image.Height
		  
		  // determine destination quadrilateral using transformation matrix
		  
		  tmpX = 0
		  tmpY = 0
		  transformPoint(tmpX, tmpY, matrix)
		  destinationQuadrilateral.Append new REALbasic.Point(tmpX, tmpY)
		  
		  tmpX = srcWidth -1
		  tmpY = 0
		  transformPoint(tmpX, tmpY, matrix)
		  destinationQuadrilateral.Append new REALbasic.Point(tmpX, tmpY)
		  
		  tmpX = srcWidth -1
		  tmpY = srcHeight - 1
		  transformPoint(tmpX, tmpY, matrix)
		  destinationQuadrilateral.Append new REALbasic.Point(tmpX, tmpY)
		  
		  tmpX = 0
		  tmpY = srcHeight - 1
		  transformPoint(tmpX, tmpY, matrix)
		  destinationQuadrilateral.Append new REALbasic.Point(tmpX, tmpY)
		  
		  ' get bounding rectangle of the quadrilateral
		  
		  getBoundingRectangle destinationQuadrilateral, minXY, maxXY
		  
		  startX = minXY.X
		  startY = minXY.Y
		  stopX = maxXY.X 
		  stopY = maxXY.Y 
		  
		  'calculate tranformation matrix
		  
		  srcRect(0) = new REALbasic.Point(0,0)
		  srcRect(1) = new REALbasic.Point(srcWidth -1 ,0)
		  srcRect(2) = new REALbasic.Point(srcWidth - 1, srcHeight - 1)
		  srcRect(3) = new REALbasic.Point(0, srcHeight - 1)
		  transMatrix = MapQuadToQuad(destinationQuadrilateral, srcRect)
		  
		  tgtPic = new Picture(g.Width, g.Height)
		  srcRGB = image.RGBSurface
		  tgtRGB = tgtPic.RGBSurface
		  
		  srcWidthM1 = srcWidth - 1
		  srcHeightM1 = srcHeight - 1
		  
		  ' for each row
		  for y = startY to stopY
		    'for each pixel
		    for x = startX to stopX
		      factor = transMatrix(2) * x + transMatrix(5) * y + transMatrix(8)
		      srcX = ( transMatrix(0) * x + transMatrix(3) * y + transMatrix(6) ) / factor
		      srcY = ( transMatrix(1) * x + transMatrix(4) * y + transMatrix(7) ) / factor
		      if srcX >= 0 and srcY >= 0 and srcX< srcWidth and srcY < srcHeight then
		        sx1 = srcX
		        if sx1 = srcWidthM1 then
		          sx2 = sx1
		        else
		          sx2 = sx1 + 1
		        end if
		        dx1 = srcX - sx1
		        dx2 = 1.0 - dx1
		        
		        sy1 = srcY
		        if sy1 = srcHeightM1 then
		          sy2 = sy1
		        else
		          sy2 = sy1 + 1
		        end if
		        dy1 = srcY - sy1
		        dy2 = 1.0 - dy1
		        
		        ' copy the pixel from the source to the target using interpolation of 4 points
		        p1 = srcRGB.Pixel(sx1, sy1)
		        p2 = srcRGB.Pixel(sx2, sy1)
		        p3 = srcRGB.Pixel(sx1, sy2)
		        p4 = srcRGB.Pixel(sx2, sy2)
		        
		        r = dy2 * ( dx2 * ( p1.red ) + dx1 * ( p2.red ) ) + dy1 * ( dx2 * ( p3.red ) + dx1 * ( p4.red ) )
		        gp = dy2 * ( dx2 * ( p1.green ) + dx1 * ( p2.green ) ) + dy1 * ( dx2 * ( p3.green ) + dx1 * ( p4.green ) )
		        b = dy2 * ( dx2 * ( p1.blue ) + dx1 * ( p2.blue ) ) + dy1 * ( dx2 * ( p3.blue ) + dx1 * ( p4.blue ) )
		        a = dy2 * ( dx2 * ( p1.Alpha ) + dx1 * ( p2.Alpha ) ) + dy1 * ( dx2 * ( p3.Alpha ) + dx1 * ( p4.Alpha ) )
		        
		        tgtRGB.Pixel(x,y) = RGB(r, gp, b, a)
		      end if
		    next
		  next
		  
		  g.DrawPicture tgtPic, 0, 0
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub getBoundingRectangle(cloud() as REALbasic.Point, byref minXY as REALbasic.Point, byref maxXY as REALbasic.Point)
		  Var minX as integer = 10e6
		  Var maxX as integer = -10e6
		  Var minY as integer = 10e6
		  Var maxY as integer = -10e6
		  
		  Var i as integer
		  for i = 0 to UBound(cloud)
		    if cloud(i).x < minX then minX = cloud(i).x
		    if cloud(i).x > maxX then maxX = cloud(i).x
		    if cloud(i).y < minY then minY = cloud(i).y
		    if cloud(i).y > maxY then maxY = cloud(i).y
		  next
		  
		  minXY = new REALbasic.Point(minX, minY)
		  maxXY = new REALbasic.Point(maxX, maxY)
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
		Private Function isTranslationMatrix(matrix() As Double) As Boolean
		  Dim result As Boolean
		  
		  result = (matrix(0) = 1) and (matrix(1) = 0) and (matrix(3) = 0) and (matrix(4) = 1) and _
		  (matrix(6) = 0) and (matrix(7) = 0) and (matrix(8) = 1)
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub loadCSS(styleData As String)
		  Var className As String
		  Var i As Integer
		  Var ch As String
		  Var dataLen As Integer
		  Var state As Integer // 0 = next class, 1 = class name, 2 = property name, 3 = property value
		  Var classProperties As JSONItem
		  Var propName As String
		  Var propValue As String
		  
		  dataLen = len(styleData)
		  state = 0 // next class
		  i = 1
		  while i <= dataLen
		    ch = Mid(styleData, i, 1)
		    
		    if Asc(ch) <= 32 then
		      // do nothing
		      
		    elseif ch = "{" then
		      classProperties = new JSONItem("{}")
		      propName = ""
		      state = 2 // property name
		      
		    elseif ch = "}" then
		      if propName <> "" then
		        classProperties.Value(Lowercase(propName)) = propValue
		      end if
		      mClasses.Value(Lowercase(Trim(className))) = classProperties
		      state = 0 // next class
		      
		    elseif ch = ";" then
		      classProperties.Value(Lowercase(propName)) = propValue
		      propName = ""
		      propValue = ""
		      state = 2 // property name
		      
		    elseif ch = ":" then
		      propValue = ""
		      state = 3 // property value
		      
		    else
		      select case state
		      case 0 // next class
		        className = ch
		        state = 1 // class name
		      case 1 // class name
		        className = className + ch
		      case 2 // property name
		        propName = propName + ch
		      case 3 // property value
		        propValue = propValue + ch
		      end select
		    end if
		    
		    i = i + 1
		  wend
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function loadImage(data As String) As Picture
		  Dim image As Picture
		  Dim alphaImage As Picture
		  Dim imageData As MemoryBlock
		  Dim commaPos As Integer
		  
		  commaPos = Instr(0, data, ",")
		  if commaPos > 0 then
		    imageData = DecodeBase64(Right(data, Len(data) - commaPos))
		    image = Picture.FromData(imageData)
		    alphaImage = new Picture(image.Width, image.Height)
		    alphaImage.Graphics.DrawPicture image, 0, 0
		  end if
		  
		  return alphaImage
		  
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
		Private Function mapQuadToQuad(Quad() as REALbasic.Point) As Double()
		  Var sq(8) as Double
		  Var px, py as Double
		  
		  Var TOLERANCE as double = 1e-13
		  
		  px = quad(0).X - quad(1).X + quad(2).X - quad(3).X
		  py = quad(0).Y - quad(1).Y + quad(2).Y - quad(3).Y
		  
		  if ( ( px < TOLERANCE ) And ( px > -TOLERANCE ) And ( py < TOLERANCE ) And ( py > -TOLERANCE ) ) then
		    sq(0) = quad(1).X - quad(0).X
		    sq(3) = quad(2).X - quad(1).X
		    sq(6) = quad(0).X
		    
		    sq(1) = quad(1).Y - quad(0).Y
		    sq(4) = quad(2).Y - quad(1).Y
		    sq(7) = quad(0).Y
		    
		    sq(2) = 0.0
		    sq(5) = 0.0
		    sq(8) = 1.0
		  else
		    
		    dim dx1, dx2, dy1, dy2, del as Double
		    
		    dx1 = quad(1).X - quad(2).X
		    dx2 = quad(3).X - quad(2).X
		    dy1 = quad(1).Y - quad(2).Y
		    dy2 = quad(3).Y - quad(2).Y
		    
		    del = matrixDeterminant2x2( dx1, dx2, dy1, dy2 )
		    
		    if ( del = 0 ) then
		      return sq
		    end if
		    
		    sq(2) = matrixDeterminant2x2( px, dx2, py, dy2 ) / del
		    sq(5) = matrixDeterminant2x2( dx1, px, dy1, py ) / del
		    sq(8) = 1.0
		    
		    sq(0) = quad(1).X - quad(0).X + sq(2) * quad(1).X
		    sq(3) = quad(3).X - quad(0).X + sq(5) * quad(3).X
		    sq(6) = quad(0).X
		    
		    sq(1) = quad(1).Y - quad(0).Y + sq(2) * quad(1).Y
		    sq(4) = quad(3).Y - quad(0).Y + sq(5) * quad(3).Y
		    sq(7) = quad(0).Y
		  end if
		  
		  return sq
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function mapQuadToQuad(input() as REALbasic.Point, output() as REALbasic.Point) As Double()
		  Var squareToInput(8) as Double = MapQuadToQuad(input)
		  Var squareToOutput(8) as Double = MapQuadToQuad(output)
		  
		  Return matrixMultiply(matrixAdjugate(squareToInput), squareToOutput)
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
		Private Function matrixAdjugate(a() as double) As double()
		  ' Calculates adjugate 3x3 matrix
		  
		  Var b(8) as double
		  
		  b(0) = matrixDeterminant2x2( a(4), a(7), a(5), a(8) )
		  b(1) = matrixDeterminant2x2( a(7), a(1), a(8), a(2) )
		  b(2) = matrixDeterminant2x2( a(1), a(4), a(2), a(5) )
		  b(3) = matrixDeterminant2x2( a(5), a(8), a(3), a(6) )
		  b(4) = matrixDeterminant2x2( a(8), a(2), a(6), a(0) )
		  b(5) = matrixDeterminant2x2( a(2), a(5), a(0), a(3) )
		  b(6) = matrixDeterminant2x2( a(3), a(6), a(4), a(7) )
		  b(7) = matrixDeterminant2x2( a(6), a(0), a(7), a(1) )
		  b(8) = matrixDeterminant2x2( a(0), a(3), a(1), a(4) )
		  
		  return b
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function matrixDeterminant2x2(a as double, b as double, c as double, d as double) As double
		  ' Caclculates determinant of a 2x2 matrix
		  return ( a * d - b * c )
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
		Private Sub process_defs(node As XmlNode)
		  Var i As Integer
		  Var id As String
		  
		  i = 0
		  while i < node.ChildCount
		    select case node.Child(i).Name
		      
		    case "style"
		      process_style(node.Child(i))
		      
		    case else
		      id = node.Child(i).GetAttribute("id")
		      if id <> "" then
		        mNodes.Value("#" + id) = node.Child(i)
		      end if
		      
		    end select
		    i = i + 1
		  wend
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub process_style(node As XmlNode)
		  Var styleData As String
		  Var typeStr As String
		  
		  typeStr = node.GetAttribute("type")
		  select case typeStr
		    
		  case "", "text/css"
		    styleData = node.FirstChild.Value
		    loadCSS(styleData)
		    
		  case else
		    'break
		    
		  end select
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub renderNode(node As XmlNode, g As Graphics, parentMatrix() As Double, parentStyle As JSONItem)
		  Var e As SVG.SVGException
		  Var nodeId As String
		  Var useNode As XMLNode
		  Var style As JSONItem
		  
		  select case node.Name
		    
		  case "#comment"
		    // we ignore xml comments
		    
		  case "circle"
		    render_circle(node, g, parentMatrix, parentStyle)
		    
		  case "defs"
		    process_defs(node)
		    
		  case "desc"
		    // we ignore these tags
		    
		  case "ellipse"
		    render_ellipse(node, g, parentMatrix, parentStyle)
		    
		  case "g"
		    render_g(node, g, parentMatrix, parentStyle)
		    
		  case "image"
		    render_image(node, g, parentMatrix, parentStyle)
		    
		  case "line"
		    render_line(node, g, parentMatrix, parentStyle)
		    
		  case "metadata"
		    // we ignore these tags
		    
		  case "path"
		    render_path(node, g, parentMatrix, parentStyle)
		    
		  case "polygon"
		    render_polygon(node, g, parentMatrix, parentStyle)
		    
		  case "polyline"
		    render_polyline(node, g, parentMatrix, parentStyle)
		    
		  case "rect"
		    render_rect(node, g, parentMatrix, parentStyle)
		    
		  case "style"
		    process_style(node)
		    
		  case "svg"
		    render_svg(node, g, parentMatrix, parentStyle)
		    
		  case "text"
		    render_text(node, g, parentMatrix, parentStyle)
		    
		  case "title"
		    // we ignore these tags
		    
		  case "use"
		    nodeId = node.GetAttribute("xlink:href")
		    if mNodes.HasKey(nodeId) then
		      useNode = mNodes.Value(nodeId)
		      
		      style = new JSONItem("{}")
		      style.ApplyValues parentStyle
		      style.ApplyValues buildStyleItem(node)
		      
		      renderNode(useNode, g, parentMatrix, style)
		    else
		      e = new SVG.SVGException()
		      e.ErrorNumber = Integer(SVGErrorEnum.NodeNotFound)
		      e.Message = "Node not found: " + nodeId
		      Raise e
		    end if
		    
		  case else
		    
		    if node.Name.Left(9) <> "sodipodi:" then // we ignore sodipodi tags
		      
		      // we only want to raise the unknown element exception during debugging,
		      // and during runtime we simply ignore unknown elements
		      
		      #if DebugBuild then
		        e = new SVG.SVGException()
		        e.ErrorNumber = Integer(SVGErrorEnum.UnknownElement)
		        e.Message = "Unknown element: " + node.Name
		        Raise e
		      #endif
		      
		    end if
		    
		  end select
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub RenderPath(g As Graphics, path As GraphicsPath, style As JSONItem, scale As Double, closed As Boolean, doFill As Boolean, doStroke As Boolean)
		  Var fill As String
		  Var fillOpacity As Double
		  Var stroke As String
		  Var strokeWidth As Double
		  Var drawColor As Color
		  
		  fill = style.LookupString("fill", "#000000")
		  fillOpacity = 1
		  if (fill <> "none") and style.HasName("fill-opacity") then
		    if Val(style.Value("fill-opacity")) = 0 then
		      fill = "none"
		    else
		      fillOpacity = Val(style.Value("fill-opacity"))
		    end if
		  end if
		  stroke = style.LookupString("stroke", "")
		  strokeWidth = style.LookupDouble("stroke-width", 1) * scale
		  
		  // fill
		  
		  if fill <> "none" and doFill then
		    drawColor = determineColor(fill)
		    drawColor = RGB(drawColor.Red, drawColor.Green, drawColor.Blue, (1 - fillOpacity) * 255)
		    g.DrawingColor = drawColor
		    g.FillPath path, true
		  end if
		  
		  // stroke
		  
		  if (stroke <> "none") and (stroke <> "") and (strokeWidth > 0) and doStroke then
		    g.DrawingColor = determineColor(stroke)
		    g.PenSize = strokeWidth
		    g.LineCap = Graphics.LineCapTypes.Butt
		    g.DrawPath path, closed
		  end if
		  
		  
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
		  //Var viewbox As String
		  //Var viewboxArr() As String
		  //Var xScale As Double
		  //Var yScale As Double
		  //Var scale As Double
		  //Var xOffset As Double
		  //Var yOffset As Double
		  //Var mulMatrix() As Double
		  
		  mClasses = new JSONItem("{}")
		  mNodes = new Dictionary()
		  matrix = identityMatrix()
		  
		  i = 0
		  while (i < xdoc.ChildCount) 
		    if xdoc.Child(i).Name = "svg" then
		      
		      // determine graphics context width and height
		      
		      w = 0
		      h = 0
		      
		      //wStr = Trim(xdoc.Child(i).GetCIAttribute("width"))
		      wStr = Trim(xdoc.Child(i).GetAttribute("width"))
		      if wStr <> "" then
		        if IsNumeric(wStr) then
		          w = Val(wStr)
		        elseif Right(wStr, 1) = "%" then
		          w = g.Width * (Val(Left(wStr, Len(wStr) - 1)) / 100)
		        end if
		      end if
		      
		      //hStr = Trim(xdoc.Child(i).GetCIAttribute("height"))
		      hStr = Trim(xdoc.Child(i).GetAttribute("height"))
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
		      
		      // apply viewbox if there is one
		      
		      //viewbox = Trim(xdoc.Child(i).GetAttribute("viewbox"))
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
		      //mulMatrix = translationMatrix(xOffset, yOffset)
		      //matrix = matrixMultiply(matrix, mulMatrix)
		      //mulMatrix = scaleMatrix(scale, scale)
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
		    
		    RenderPath g, path, style, matrix(0), true, true, true
		    
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_ellipse(node As XmlNode, g As Graphics, parentMatrix() As Double, parentStyle As JSONItem)
		  Var localStyle As JSONItem
		  Var style As JSONItem
		  Var matrix() As Double
		  Var i As Integer
		  Var tmpX As Double
		  Var tmpY As Double
		  Var cx As Double
		  Var cy As Double
		  Var rx As Double
		  Var ry As Double
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
		  rx = style.LookupDouble("rx")
		  ry = style.LookupDouble("ry")
		  
		  if (rx > 0) and (ry > 0) then
		    
		    // build path
		    
		    path = new GraphicsPath()
		    
		    pointCount = 128
		    i = 0
		    
		    theta = Pi * (i / (pointCount / 2))
		    tmpX = cx + rx * cos(theta) // center a + radius x * cos(theta)
		    tmpY = cy + ry * sin(theta) // center b + radius y * sin(theta)
		    transformPoint tmpX, tmpY, matrix
		    path.MoveToPoint tmpX, tmpY
		    
		    while i <= pointCount 
		      theta = Pi * (i / (pointCount / 2))
		      tmpX = cx + rx * cos(theta) // center a + radius x * cos(theta)
		      tmpY = cy + ry * sin(theta) // center b + radius y * sin(theta)
		      transformPoint tmpX, tmpY, matrix
		      
		      path.AddLineToPoint tmpX, tmpY
		      
		      i = i + 1
		    wend
		    
		    RenderPath g, path, style, matrix(0), true, true, true
		    
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_g(node As XmlNode, g As Graphics, parentMatrix() As Double, parentStyle As JSONItem)
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
		Private Sub render_image(node As XmlNode, g As Graphics, parentMatrix() As Double, parentStyle As JSONItem)
		  
		  Dim localStyle As JSONItem
		  Dim style As JSONItem
		  Dim matrix() As Double
		  Dim mulMatrix() As Double
		  Dim imageData As String
		  Dim image As Picture
		  Dim x As Double
		  Dim y As Double
		  Dim width As Double
		  Dim height As Double
		  
		  style = new JSONItem("{}")
		  style.ApplyValues parentStyle
		  localStyle = buildStyleItem(node)
		  style.ApplyValues localStyle
		  matrix = buildTransformationMatrix(localStyle.Lookup("transform", ""))
		  matrix = matrixMultiply(parentMatrix, matrix)
		  
		  x = style.LookupDouble("x")
		  y = style.LookupDouble("y")
		  width = style.LookupDouble("width")
		  height = style.LookupDouble("height")
		  
		  imageData = node.GetAttribute("xlink:href")
		  image = loadImage(imageData)
		  
		  if image <> nil then
		    
		    mulMatrix = translationMatrix(x, y)
		    matrix = matrixMultiply(matrix, mulMatrix)
		    
		    // to speed up rendering, we only use DrawTransformedPicture when needed
		    
		    if isTranslationMatrix(matrix) then
		      g.DrawPicture image, matrix(2), matrix(5)
		    else
		      g.DrawTransformedPicture image, matrix
		    end if
		    
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_line(node As XmlNode, g As Graphics, parentMatrix() As Double, parentStyle As JSONItem)
		  Var localStyle As JSONItem
		  Var style As JSONItem
		  Var matrix() As Double
		  Var x1 As Double
		  Var y1 As Double
		  Var x2 As Double
		  Var y2 As Double
		  Var path As GraphicsPath
		  
		  style = new JSONItem("{}")
		  style.ApplyValues parentStyle
		  localStyle = buildStyleItem(node)
		  style.ApplyValues localStyle
		  matrix = buildTransformationMatrix(localStyle.Lookup("transform", ""))
		  matrix = matrixMultiply(parentMatrix, matrix)
		  
		  x1 = style.LookupDouble("x1") 
		  y1 = style.LookupDouble("y1") 
		  x2 = style.LookupDouble("x2") 
		  y2 = style.LookupDouble("y2") 
		  
		  // build path
		  
		  path = new GraphicsPath()
		  
		  transformPoint x1, y1, matrix
		  path.MoveToPoint x1, y1
		  
		  transformPoint x2, y2, matrix
		  path.AddLineToPoint x2, y2 
		  
		  RenderPath g, path, style, matrix(0), false, false, true
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_path(node As XmlNode, g As Graphics, parentMatrix() As Double, parentStyle As JSONItem)
		  Var localStyle As JSONItem
		  Var style As JSONItem
		  Var matrix() As Double
		  Var i As UInt64
		  Var fill As String
		  Var stroke As String
		  Var strokeWidth As Double
		  Var d As String
		  Var ch As String
		  Var penX As Double
		  Var penY As Double
		  Var tmpX As Double
		  Var tmpY As Double
		  Var path() As String
		  Var continueImplicit As Boolean
		  Var prevCCommand As Boolean
		  Var prevQCommand As Boolean
		  Var prevControlX As Double
		  Var prevControlY As Double
		  Var itemFill As Boolean
		  Var itemFillColor As Color
		  Var itemStroke As Boolean
		  Var itemStrokeColor As Color
		  Var prevClosed As Boolean
		  Var additionalPath() As String
		  Var e As SVG.SVGException
		  Var x1 As Double
		  Var y1 As Double
		  Var x2 As Double
		  Var y2 As Double
		  Var flagA As Integer
		  Var flagS As Integer
		  Var rx As Double
		  Var ry As Double
		  Var theta As Double
		  Var x1Comp As Double
		  Var y1Comp As Double
		  Var cxComp As Double
		  Var cyComp As Double
		  Var cx As Double
		  Var cy As Double
		  Var theta1 As Integer
		  Var thetaDelta As Integer
		  Var tmpDbl As Double
		  Var currentAngle As Double
		  Var angleStep As Double
		  Var pathMB As MemoryBlock
		  Var adjustValue As Integer
		  Var relativeCommand As Boolean
		  Var tmpMatrix() As Double
		  Var tmpMatrix2() As Double
		  Var radiScale As Double
		  Var shape As GraphicsPath
		  Var ux As Double
		  Var uy As Double
		  Var vx As Double
		  Var vy As Double
		  Var controlX1 As Double
		  Var controlY1 As Double
		  Var controlX2 As Double
		  Var controlY2 As Double
		  Var tmpStr As String
		  Var startX As Double
		  Var startY As Double
		  
		  shape = new GraphicsPath()
		  
		  style = new JSONItem("{}")
		  style.ApplyValues parentStyle
		  localStyle = buildStyleItem(node)
		  style.ApplyValues localStyle
		  matrix = buildTransformationMatrix(localStyle.Lookup("transform", ""))
		  matrix = matrixMultiply(parentMatrix, matrix)
		  
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
		  
		  // fill
		  
		  if (fill <> "none") then
		    itemFill = true
		    itemFillColor = determineColor(fill)
		  else
		    itemFill = false
		  end if
		  
		  // stroke
		  
		  if (stroke <> "none") and (stroke <> "") and (strokeWidth > 0) then
		    itemStroke = true
		    itemStrokeColor = determineColor(stroke)
		  else
		    itemStroke = false
		  end if
		  
		  // build figure shape
		  
		  tmpStr = parentStyle.Lookup("x", "0")
		  penX = Val(tmpStr)
		  tmpStr = parentStyle.Lookup("y", "0")
		  penY = Val(tmpStr)
		  prevClosed = false
		  
		  tmpX = penX
		  tmpY = penY
		  transformPoint tmpX, tmpY, matrix
		  shape.MoveToPoint tmpX, tmpY
		  
		  d = Trim(style.LookupString("d", ""))
		  d = d.ReplaceAll(",", " ")
		  
		  pathMB = d
		  
		  Redim path(-1)
		  path.Append ""
		  i = 0
		  while i < pathMB.Size
		    ch = pathMB.StringValue(i, 1)
		    
		    if ch = " " then
		      
		      if path(path.Ubound) <> "" then
		        path.Append ""
		      end if
		      
		    elseif ch = "-" then
		      
		      if path(path.Ubound) <> "" then
		        if right(path(path.Ubound), 1) = "e" then
		          path(path.Ubound) = path(path.Ubound) + ch
		        else
		          path.Append "-"
		        end if
		      else
		        path(path.Ubound) = ch
		      end if
		      
		    elseif not IsNumeric(ch) and (ch <> ".") and (ch <> "-") and (ch <> "e") then
		      
		      if path(path.Ubound) <> "" then
		        path.Append ch
		      else
		        path(path.Ubound) = ch
		      end if
		      path.Append ""
		      
		    elseif ch = "." then
		      
		      if Instr(0, path(path.Ubound), ".") > 0 then
		        path.Append "."
		      else
		        path(path.Ubound) = path(path.Ubound) + ch
		      end if
		      
		    else
		      
		      path(path.Ubound) = path(path.Ubound) + ch
		      
		    end if
		    i = i + 1
		  wend
		  
		  if path(path.Ubound) = "" then
		    path.Remove(path.Ubound)
		  end if
		  
		  if additionalPath.Ubound > 4 then
		    additionalPath.Append "z"
		    if relativeCommand then
		      additionalPath.Append "M"
		      additionalPath.Append "0"
		      additionalPath.Append "0"
		    end if
		    
		    i = 0
		    while i <= additionalPath.Ubound
		      path.Insert(i, additionalPath(i))
		      i = i + 1
		    wend
		  end if
		  
		  // draw path
		  
		  prevCCommand = false
		  prevQCommand = false
		  
		  i = 0
		  while i <= path.Ubound
		    
		    // absolute elliptical arc AND relative elliptical arc
		    
		    if (StrComp(path(i), "A", 0) = 0) or (StrComp(path(i), "a", 0) = 0) then 
		      
		      Var isAbsolute As Boolean
		      if StrComp(path(i), "A", 0) = 0 then
		        isAbsolute = true
		      else
		        isAbsolute = false
		      end if
		      
		      do
		        
		        x1 = penX
		        y1 = penY
		        i = i + 1
		        rx = Val(path(i))
		        i = i + 1
		        ry = Val(path(i))
		        i = i + 1
		        theta = Val(path(i)) * DegToRad
		        i = i + 1
		        flagA = Val(path(i))
		        i = i + 1
		        flagS = Val(path(i))
		        
		        if isAbsolute then
		          i = i + 1
		          x2 = Val(path(i))
		          i = i + 1
		          y2 = Val(path(i))
		        else
		          i = i + 1
		          x2 = penX + Val(path(i))
		          i = i + 1
		          y2 = penY + Val(path(i))
		        end if
		        
		        // Step 1: Compute (x1', y1')
		        
		        x1Comp = cos(theta) * ((x1 - x2) / 2) +  sin(theta) * ((y1 - y2) / 2)
		        y1Comp = -sin(theta) * ((x1 - x2) / 2) +  cos(theta) * ((y1 - y2) / 2)
		        
		        // correction of out-of-range radii
		        
		        radiScale = (x1Comp^2 / rx^2) + (y1Comp^2 / ry^2)
		        if radiScale > 1 then
		          rx = Sqrt(radiScale) * rx
		          ry = Sqrt(radiScale) * ry
		        end if
		        
		        // Step 2: Compute(cx', cy')
		        
		        tmpDbl = (rx^2 * ry^2) - (rx^2 * y1Comp^2) - (ry^2 * x1Comp^2)
		        tmpDbl = tmpDbl / ((rx^2 * y1Comp^2) + (ry^2 * x1Comp^2))
		        if tmpDbl < 0 then
		          tmpDbl = 0
		        end if
		        tmpDbl = Sqrt(tmpDbl)
		        
		        if flagA = flagS then
		          tmpDbl = -tmpDbl
		        end if
		        
		        cxComp = tmpDbl * (rx * y1Comp / ry)
		        cyComp = tmpDbl * -(ry * x1Comp / rx)
		        
		        // Step 3: Compute (cx, cy) from (cx', cy')
		        
		        cx = (cos(theta) * cxComp - sin(theta) * cyComp) + ((x1 + x2) / 2)
		        cy = (sin(theta) * cxComp + cos(theta) * cyComp) + ((y1 + y2) / 2)
		        
		        // Step 4: Compute theta1 and thetaDelta
		        
		        ux = 1
		        uy = 0
		        vx = (x1Comp - cxComp) / rx
		        vy = (y1Comp - cyComp) / ry
		        theta1 = angleBetweenVectors(ux, uy, vx, vy)
		        
		        ux = (x1Comp - cxComp) / rx
		        uy = (y1Comp - cyComp) / ry
		        vx = (-x1Comp - cxComp) / rx
		        vy = (-y1Comp - cyComp) / ry
		        thetaDelta = angleBetweenVectors(ux, uy, vx, vy)
		        thetaDelta = thetaDelta mod 360
		        
		        if (flagS = 0) and (thetaDelta > 0) then
		          thetaDelta = thetaDelta - 360
		        elseif (flagS = 1) and (thetaDelta < 0) then
		          thetaDelta = thetaDelta + 360
		        end if
		        
		        // Build path using calculated values
		        
		        if thetaDelta <> 0 then
		          
		          adjustValue = thetaDelta / Abs(thetaDelta)
		          
		          angleStep = (thetaDelta / 360) 
		          
		          currentAngle = theta1 + angleStep
		          
		          tmpMatrix = translationMatrix(0, 0) 
		          
		          tmpMatrix2 = translationMatrix(cx, cy)
		          tmpMatrix = matrixMultiply(tmpMatrix, tmpMatrix2)
		          tmpMatrix2 = rotationMatrix(theta * RadToDeg)
		          tmpMatrix = matrixMultiply(tmpMatrix, tmpMatrix2)
		          tmpMatrix2 = translationMatrix(-cx, -cy)
		          tmpMatrix = matrixMultiply(tmpMatrix, tmpMatrix2)
		          
		          tmpMatrix = matrixMultiply(tmpMatrix, matrix)
		          
		          // build arc path
		          
		          while currentAngle * adjustValue <= (theta1 + thetaDelta) * adjustValue
		            
		            tmpX = cx + rx  * cos(currentAngle * DegToRad) 
		            tmpY = cy + ry * sin(currentAngle * DegToRad) 
		            
		            transformPoint tmpX, tmpY, tmpMatrix
		            
		            shape.AddLineToPoint tmpX, tmpY 
		            
		            currentAngle = currentAngle + angleStep
		            
		          wend 
		          
		        end if
		        
		        penX = x2
		        penY = y2
		        
		        continueImplicit = false
		        if i < path.Ubound then
		          if IsNumeric(path(i + 1)) then
		            continueImplicit = true
		          end if
		        end if
		        
		      loop until not continueImplicit
		      
		      prevCCommand = false
		      prevQCommand = false
		      
		    elseif StrComp(path(i), "C", 0) = 0 then // absolute curveto
		      do
		        i = i + 1
		        tmpX = Val(path(i))
		        i = i + 1
		        tmpY = Val(path(i))
		        transformPoint tmpX, tmpY, matrix
		        controlX1 = tmpX
		        controlY1 = tmpY
		        i = i + 1
		        tmpX = Val(path(i))
		        i = i + 1
		        tmpY = Val(path(i))
		        transformPoint tmpX, tmpY, matrix
		        controlX2 = tmpX
		        controlY2 = tmpY
		        prevControlX = tmpX
		        prevControlY = tmpY
		        i = i + 1
		        tmpX = Val(path(i))
		        i = i + 1
		        tmpY = Val(path(i))
		        penX = tmpX
		        penY = tmpY
		        transformPoint tmpX, tmpY, matrix
		        
		        shape.AddCurveToPoint controlX1, controlY1, controlX2, controlY2, tmpX, tmpY
		        
		        continueImplicit = false
		        if i < path.Ubound then
		          if IsNumeric(path(i + 1)) then
		            continueImplicit = true
		          end if
		        end if
		        
		      loop until not continueImplicit
		      
		      prevCCommand = true
		      prevQCommand = false
		      
		    elseif StrComp(path(i), "c", 0) = 0 then // relative curveto
		      do
		        i = i + 1
		        tmpX = penX + Val(path(i))
		        i = i + 1
		        tmpY = penY + Val(path(i))
		        transformPoint tmpX, tmpY, matrix
		        controlX1 = tmpX
		        controlY1 = tmpY
		        i = i + 1
		        tmpX = penX + Val(path(i))
		        i = i + 1
		        tmpY = penY + Val(path(i))
		        transformPoint tmpX, tmpY, matrix
		        controlX2 = tmpX
		        controlY2 = tmpY
		        prevControlX = tmpX 
		        prevControlY = tmpY
		        i = i + 1
		        tmpX = penX + Val(path(i))
		        i = i + 1
		        tmpY = penY + Val(path(i))
		        penX = tmpX 
		        penY = tmpY
		        transformPoint tmpX, tmpY, matrix
		        
		        shape.AddCurveToPoint controlX1, controlY1, controlX2, controlY2, tmpX, tmpY
		        
		        continueImplicit = false
		        if i < path.Ubound then
		          if IsNumeric(path(i + 1)) then
		            continueImplicit = true
		          end if
		        end if
		        
		      loop until not continueImplicit
		      
		      prevCCommand = true
		      prevQCommand = false
		      
		    elseif StrComp(path(i), "H", 0) = 0 then // absolute horizontal lineto
		      do
		        i = i + 1
		        tmpX = Val(path(i))
		        penX = tmpX
		        tmpY = penY
		        transformPoint tmpX, tmpY, matrix
		        
		        shape.AddLineToPoint tmpX, tmpY
		        
		        continueImplicit = false
		        if i < path.Ubound then
		          if IsNumeric(path(i + 1)) then
		            continueImplicit = true
		          end if
		        end if
		        
		      loop until not continueImplicit
		      
		      prevCCommand = false
		      prevQCommand = false
		      
		    elseif StrComp(path(i), "h", 0) = 0 then // relative horizontal lineto
		      do
		        i = i + 1
		        tmpX = penX + Val(path(i))
		        penX = tmpX
		        tmpY = penY
		        transformPoint tmpX, tmpY, matrix
		        
		        shape.AddLineToPoint tmpX, tmpY
		        
		        continueImplicit = false
		        if i < path.Ubound then
		          if IsNumeric(path(i + 1)) then
		            continueImplicit = true
		          end if
		        end if
		        
		      loop until not continueImplicit
		      
		      prevCCommand = false
		      prevQCommand = false
		      
		    elseif StrComp(path(i), "L", 0) = 0 then // absolute lineto
		      
		      do
		        
		        i = i + 1
		        tmpX = Val(path(i))
		        i = i + 1
		        tmpY = Val(path(i))
		        penX = tmpX
		        penY = tmpY
		        transformPoint tmpX, tmpY, matrix
		        
		        shape.AddLineToPoint tmpX, tmpY
		        
		        continueImplicit = false
		        if i < path.Ubound then
		          if IsNumeric(path(i + 1)) then
		            continueImplicit = true
		          end if
		        end if
		        
		      loop until not continueImplicit
		      
		      prevCCommand = false
		      prevQCommand = false
		      
		    elseif StrComp(path(i), "l", 0) = 0 then // relative lineto
		      
		      do
		        i = i + 1
		        tmpX = penX + Val(path(i))
		        i = i + 1
		        tmpY = penY + Val(path(i))
		        penX = tmpX
		        penY = tmpY
		        transformPoint tmpX, tmpY, matrix
		        
		        shape.AddLineToPoint tmpX, tmpY
		        
		        continueImplicit = false
		        if i < path.Ubound then
		          if IsNumeric(path(i + 1)) then
		            continueImplicit = true
		          end if
		        end if
		        
		      loop until not continueImplicit
		      
		      prevCCommand = false
		      prevQCommand = false
		      
		    elseif StrComp(path(i), "M", 0) = 0 then // absolute move
		      
		      i = i + 1
		      tmpX = Val(path(i))
		      i = i + 1
		      tmpY = Val(path(i))
		      
		      penX = tmpX
		      penY = tmpY
		      startX = penX
		      startY = penY
		      
		      transformPoint tmpX, tmpY, matrix
		      shape.MoveToPoint tmpX, tmpY
		      
		      // apply  implicit lineto commands
		      
		      do
		        continueImplicit = false
		        if i < (path.Ubound - 1) then
		          if IsNumeric(path(i + 1)) then
		            i = i + 1
		            tmpX = Val(path(i))
		            i = i + 1
		            tmpY = Val(path(i))
		            penX = tmpX
		            penY = tmpY
		            transformPoint tmpX, tmpY, matrix
		            
		            shape.AddLineToPoint tmpX, tmpY
		            
		            continueImplicit = true
		          end if
		        end if
		      loop until (i > path.Ubound) or not continueImplicit
		      
		      prevCCommand = false
		      prevQCommand = false
		      
		    elseif StrComp(path(i), "m", 0) = 0 then // relative move
		      
		      i = i + 1
		      tmpX = Val(path(i))
		      i = i + 1
		      tmpY = Val(path(i))
		      
		      penX = penX + tmpX
		      penY = penY + tmpY
		      startX = penX
		      startY = penY
		      
		      tmpX = penX
		      tmpY = penY
		      
		      transformPoint tmpX, tmpY, matrix
		      shape.MoveToPoint tmpX, tmpY
		      
		      // apply  implicit lineto commands
		      
		      do
		        continueImplicit = false
		        if i < (path.Ubound - 1) then
		          if IsNumeric(path(i + 1)) then
		            i = i + 1
		            tmpX = Val(path(i))
		            i = i + 1
		            tmpY = Val(path(i))
		            penX = penX + tmpX
		            penY = penY + tmpY
		            tmpX = penX
		            tmpY = penY
		            transformPoint tmpX, tmpY, matrix
		            
		            shape.AddLineToPoint tmpX, tmpY
		            
		            continueImplicit = true
		          end if
		        end if
		      loop until (i > path.Ubound) or not continueImplicit
		      
		      prevCCommand = false
		      prevQCommand = false
		      
		    elseif StrComp(path(i), "Q", 0) = 0 then // absolute quadratic Bézier curveto
		      do
		        i = i + 1
		        tmpX = Val(path(i))
		        i = i + 1
		        tmpY = Val(path(i))
		        transformPoint tmpX, tmpY, matrix
		        controlX1 = tmpX
		        controlY1 = tmpY
		        prevControlX = tmpX
		        prevControlY = tmpY
		        i = i + 1
		        tmpX = Val(path(i))
		        i = i + 1
		        tmpY = Val(path(i))
		        penX = tmpX
		        penY = tmpY
		        transformPoint tmpX, tmpY, matrix
		        
		        shape.AddQuadraticCurveToPoint controlX1, controlY1, tmpX, tmpY
		        
		        continueImplicit = false
		        if i < path.Ubound then
		          if IsNumeric(path(i + 1)) then
		            continueImplicit = true
		          end if
		        end if
		        
		      loop until not continueImplicit
		      
		      prevCCommand = false
		      prevQCommand = true
		      
		    elseif StrComp(path(i), "q", 0) = 0 then // relative quadratic Bézier curveto
		      do
		        
		        tmpX = penX
		        tmpY = penY
		        transformPoint tmpX, tmpY, matrix
		        i = i + 1
		        tmpX = penX + Val(path(i))
		        i = i + 1
		        tmpY = penY + Val(path(i))
		        transformPoint tmpX, tmpY, matrix
		        prevControlX = tmpX
		        prevControlY = tmpY
		        i = i + 1
		        tmpX = penX + Val(path(i))
		        i = i + 1
		        tmpY = penY + Val(path(i))
		        penX = tmpX
		        penY = tmpY
		        transformPoint tmpX, tmpY, matrix
		        //cs.X2 = tmpX
		        //cs.Y2 = tmpY
		        
		        // TODO: draw shape
		        shape.AddQuadraticCurveToPoint prevControlX, prevControlY, tmpX, tmpY
		        
		        continueImplicit = false
		        if i < path.Ubound then
		          if IsNumeric(path(i + 1)) then
		            continueImplicit = true
		          end if
		        end if
		        
		      loop until not continueImplicit
		      
		      prevCCommand = false
		      prevQCommand = true
		      
		    elseif StrComp(path(i), "S", 0) = 0 then // absolute smooth curveto
		      
		      do
		        
		        tmpX = penX
		        tmpY = penY
		        transformPoint tmpX, tmpY, matrix
		        
		        if prevCCommand then
		          controlX1 = (tmpX - prevControlX) + tmpX
		          controlY1 = (tmpY - prevControlY) + tmpY
		        else
		          controlX1 = tmpX
		          controlY1 = tmpY
		        end if
		        i = i + 1
		        tmpX = Val(path(i))
		        i = i + 1
		        tmpY = Val(path(i))
		        transformPoint tmpX, tmpY, matrix
		        controlX2 = tmpX
		        controlY2 = tmpY
		        prevControlX = tmpX
		        prevControlY = tmpY
		        i = i + 1
		        tmpX = Val(path(i))
		        i = i + 1
		        tmpY = Val(path(i))
		        penX = tmpX
		        penY = tmpY
		        transformPoint tmpX, tmpY, matrix
		        
		        shape.AddCurveToPoint controlX1, controlY1, controlX2, controlY2, tmpX, tmpY
		        
		        continueImplicit = false
		        if i < path.Ubound then
		          if IsNumeric(path(i + 1)) then
		            continueImplicit = true
		          end if
		        end if
		        
		      loop until not continueImplicit
		      
		      prevCCommand = true
		      prevQCommand = false
		      
		    elseif StrComp(path(i), "s", 0) = 0 then // relative smooth curveto
		      
		      do
		        
		        tmpX = penX
		        tmpY = penY
		        transformPoint tmpX, tmpY, matrix
		        
		        if prevCCommand then
		          controlX1 = (tmpX - prevControlX) + tmpX
		          controlY1 = (tmpY - prevControlY) + tmpY
		        else
		          controlX1 = tmpX
		          controlY1 = tmpY
		        end if
		        i = i + 1
		        tmpX = penX + Val(path(i))
		        i = i + 1
		        tmpY = penY + Val(path(i))
		        transformPoint tmpX, tmpY, matrix
		        controlX2 = tmpX
		        controlY2 = tmpY
		        prevControlX = tmpX
		        prevControlY = tmpY
		        i = i + 1
		        tmpX = penX + Val(path(i))
		        i = i + 1
		        tmpY = penY + Val(path(i))
		        penX = tmpX
		        penY = tmpY
		        transformPoint tmpX, tmpY, matrix
		        
		        shape.AddCurveToPoint controlX1, controlY1, controlX2, controlY2, tmpX, tmpY
		        
		        continueImplicit = false
		        if i < path.Ubound then
		          if IsNumeric(path(i + 1)) then
		            continueImplicit = true
		          end if
		        end if
		        
		      loop until not continueImplicit
		      
		      prevCCommand = true
		      prevQCommand = false
		      
		    elseif StrComp(path(i), "T", 0) = 0 then // absolute smooth quadratic Bézier curveto
		      do
		        tmpX = penX
		        tmpY = penY 
		        transformPoint tmpX, tmpY, matrix
		        
		        if prevQCommand then
		          controlX1 = (tmpX - prevControlX) + tmpX
		          controlY1 = (tmpY - prevControlY) + tmpY
		        else
		          controlX1 = tmpX
		          controlY1 = tmpY
		        end if
		        prevControlX = tmpX
		        prevControlY = tmpY
		        i = i + 1
		        tmpX = Val(path(i))
		        i = i + 1
		        tmpY = Val(path(i))
		        penX = tmpX
		        penY = tmpY
		        transformPoint tmpX, tmpY, matrix
		        
		        shape.AddQuadraticCurveToPoint controlX1, controlY1, tmpX, tmpY
		        
		        continueImplicit = false
		        if i < path.Ubound then
		          if IsNumeric(path(i + 1)) then
		            continueImplicit = true
		          end if
		        end if
		        
		      loop until not continueImplicit
		      
		      prevCCommand = false
		      prevQCommand = true
		      
		    elseif StrComp(path(i), "t", 0) = 0 then // relative smooth quadratic Bézier curveto
		      do
		        //cs = new CurveShape
		        //fs.Append cs
		        
		        tmpX = penX
		        tmpY = penY
		        transformPoint tmpX, tmpY, matrix
		        //cs.X = tmpX
		        //cs.Y = tmpY
		        //cs.Order = 1
		        if prevQCommand then
		          controlX1 = (tmpX - prevControlX) + tmpX
		          controlY1 = (tmpY - prevControlY) + tmpY
		          //cs.ControlX(0) = (tmpX - prevControlX) + tmpX
		          //cs.ControlY(0) = (tmpY - prevControlY) + tmpY
		        else
		          controlX1 = tmpX
		          controlY1 = tmpY
		          //cs.ControlX(0) = tmpX
		          //cs.ControlY(0) = tmpY
		        end if
		        prevControlX = tmpX
		        prevControlY = tmpY
		        i = i + 1
		        tmpX = penX + Val(path(i))
		        i = i + 1
		        tmpY = penY + Val(path(i))
		        penX = tmpX
		        penY = tmpY
		        transformPoint tmpX, tmpY, matrix
		        //cs.X2 = tmpX
		        //cs.Y2 = tmpY
		        
		        shape.AddQuadraticCurveToPoint controlX1, controlY1, tmpX, tmpY
		        
		        continueImplicit = false
		        if i < path.Ubound then
		          if IsNumeric(path(i + 1)) then
		            continueImplicit = true
		          end if
		        end if
		        
		      loop until not continueImplicit
		      
		      prevCCommand = false
		      prevQCommand = true
		      
		    elseif StrComp(path(i), "V", 0) = 0 then // absolute vertical lineto
		      
		      do
		        tmpX = penX
		        i = i + 1
		        tmpY = Val(path(i))
		        penY = tmpY
		        transformPoint tmpX, tmpY, matrix
		        
		        shape.AddLineToPoint tmpX, tmpY
		        
		        continueImplicit = false
		        if i < path.Ubound then
		          if IsNumeric(path(i + 1)) then
		            continueImplicit = true
		          end if
		        end if
		        
		      loop until not continueImplicit
		      
		      prevCCommand = false
		      prevQCommand = false
		      
		    elseif StrComp(path(i), "v", 0) = 0 then // relative vertical lineto
		      
		      do
		        
		        tmpX = penX
		        i = i + 1
		        tmpY = penY + Val(path(i))
		        penY = tmpY
		        transformPoint tmpX, tmpY, matrix
		        
		        shape.AddLineToPoint tmpX, tmpY
		        
		        continueImplicit = false
		        if i < path.Ubound then
		          if IsNumeric(path(i + 1)) then
		            continueImplicit = true
		          end if
		        end if
		        
		      loop until not continueImplicit
		      
		      prevCCommand = false
		      prevQCommand = false
		      
		    elseif path(i) = "z" then // close path
		      
		      prevClosed = true
		      
		      penX = startX
		      penY = startY
		      
		      prevCCommand = false
		      prevQCommand = false
		      
		    else
		      if IsNumeric(path(i)) then
		        e = new SVG.SVGException()
		        e.ErrorNumber = Integer(SVGErrorEnum.ExpectedPathCommand)
		        e.Message = "Expected path command: " + Str(path(i))
		        Raise e
		        i = path.Ubound
		      end if
		      
		      prevCCommand = false
		      
		    end if
		    
		    if path(i) <> "z" then
		      prevClosed = false
		    end if
		    
		    i = i + 1
		  wend
		  
		  RenderPath g, shape, style, matrix(0), prevClosed, true, true
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_polygon(node As XmlNode, g As Graphics, parentMatrix() As Double, parentStyle As JSONItem)
		  Var localStyle As JSONItem
		  Var style As JSONItem
		  Var matrix() As Double
		  Var i As Integer
		  Var tmpX As Double
		  Var tmpY As Double
		  Var points() As Integer
		  Var tmpArr() As String
		  Var coord() As String
		  Var path As GraphicsPath
		  
		  style = new JSONItem("{}")
		  style.ApplyValues parentStyle
		  localStyle = buildStyleItem(node)
		  style.ApplyValues localStyle
		  matrix = buildTransformationMatrix(localStyle.Lookup("transform", ""))
		  matrix = matrixMultiply(parentMatrix, matrix)
		  
		  // build path
		  
		  path = new GraphicsPath()
		  
		  tmpArr = style.LookupString("points", "").Split(" ")
		  
		  if tmpArr.Count > 1 then
		    
		    i = 0
		    coord = tmpArr(i).Split(",")
		    if coord.Ubound = 1 then
		      tmpX = Val(coord(0))
		      tmpY = Val(coord(1))
		      transformPoint tmpX, tmpY, matrix
		    end if
		    path.MoveToPoint tmpX, tmpY
		    
		    i = 0
		    while i <= tmpArr.Ubound
		      coord = tmpArr(i).Split(",")
		      if coord.Ubound = 1 then
		        tmpX = Val(coord(0))
		        tmpY = Val(coord(1))
		        transformPoint tmpX, tmpY, matrix
		        path.AddLineToPoint tmpX, tmpY
		      end if
		      i = i + 1
		    wend
		    
		    RenderPath g, path, style, matrix(0), true, true, true
		    
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_polyline(node As XmlNode, g As Graphics, parentMatrix() As Double, parentStyle As JSONItem)
		  Dim localStyle As JSONItem
		  Dim style As JSONItem
		  Dim matrix() As Double
		  Dim i As Integer
		  Var tmpX As Double
		  Var tmpY As Double
		  Var tmpArr() As String
		  Var coord() As String
		  Var path As GraphicsPath
		  
		  style = new JSONItem("{}")
		  style.ApplyValues parentStyle
		  localStyle = buildStyleItem(node)
		  style.ApplyValues localStyle
		  matrix = buildTransformationMatrix(localStyle.Lookup("transform", ""))
		  matrix = matrixMultiply(parentMatrix, matrix)
		  
		  // build path
		  
		  path = new GraphicsPath()
		  
		  tmpArr = style.LookupString("points", "").Split(" ")
		  
		  if tmpArr.Count > 1 then
		    
		    i = 0
		    coord = tmpArr(i).Split(",")
		    if coord.Ubound = 1 then
		      tmpX = Val(coord(0))
		      tmpY = Val(coord(1))
		      transformPoint tmpX, tmpY, matrix
		    end if
		    path.MoveToPoint tmpX, tmpY
		    
		    i = i + 1
		    while i <= tmpArr.Ubound
		      coord = tmpArr(i).Split(",")
		      if coord.Ubound = 1 then
		        tmpX = Val(coord(0))
		        tmpY = Val(coord(1))
		        transformPoint tmpX, tmpY, matrix
		        path.AddLineToPoint tmpX, tmpY
		      end if
		      i = i + 1
		    wend
		    
		    RenderPath g, path, style, matrix(0), false, true, true
		    
		  end if
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_rect(node As XmlNode, g As Graphics, parentMatrix() As Double, parentStyle As JSONItem)
		  Var localStyle As JSONItem
		  Var style As JSONItem
		  Var matrix() As Double
		  Var tmpX As Double
		  Var tmpY As Double
		  Var x As Double
		  Var y As Double
		  Var width As Double
		  Var height As Double
		  Var path As GraphicsPath
		  
		  style = new JSONItem("{}")
		  style.ApplyValues parentStyle
		  localStyle = buildStyleItem(node)
		  style.ApplyValues localStyle
		  matrix = buildTransformationMatrix(localStyle.Lookup("transform", ""))
		  matrix = matrixMultiply(parentMatrix, matrix)
		  
		  x = style.LookupDouble("x")
		  y = style.LookupDouble("y")
		  width = style.LookupDouble("width")
		  height = style.LookupDouble("height")
		  
		  if (width > 0) and (height > 0) then
		    
		    // build path
		    
		    path = new GraphicsPath()
		    
		    tmpX = x
		    tmpY = y 
		    transformPoint tmpX, tmpY, matrix
		    path.MoveToPoint tmpX, tmpY
		    
		    tmpX = x
		    tmpY = y + height
		    transformPoint tmpX, tmpY, matrix
		    path.AddLineToPoint tmpX, tmpY
		    
		    tmpX = x + width 
		    tmpY = y + height
		    transformPoint tmpX, tmpY, matrix
		    path.AddLineToPoint tmpX, tmpY
		    
		    tmpX = x + width
		    tmpY = y
		    transformPoint tmpX, tmpY, matrix
		    path.AddLineToPoint tmpX, tmpY
		    
		    RenderPath g, path, style, matrix(0), true, true, true
		    
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
		Private Sub render_text(node As XmlNode, g As Graphics, parentMatrix() As Double, parentStyle As JSONItem)
		  Var localStyle As JSONItem
		  Var style As JSONItem
		  Var elementStyle As JSONItem
		  Var matrix() As Double
		  Var mulMatrix() As Double
		  Var elementMatrix() As Double
		  Var element As Picture
		  Var eg As Graphics
		  Var tspanStyle As JSONItem
		  Var textStr As String
		  Var x As Double
		  Var y As Double
		  Var fill As String
		  Var strShape as new StringShape
		  Var i As Integer
		  
		  style = new JSONItem("{}")
		  style.ApplyValues parentStyle
		  localStyle = buildStyleItem(node)
		  style.ApplyValues localStyle
		  matrix = buildTransformationMatrix(localStyle.Lookup("transform", ""))
		  matrix = matrixMultiply(parentMatrix, matrix)
		  
		  x = style.LookupDouble("x")
		  y = style.LookupDouble("y")
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
		  
		  // fill
		  
		  if fill <> "none" then
		    
		    i = 0
		    while i < node.ChildCount
		      
		      textStr = ""
		      
		      elementStyle = new JSONItem(style.ToString())
		      
		      if node.Child(i).Name = "#text" then
		        textStr = Trim(node.FirstChild.Value)
		      elseif node.Child(i).Name = "tspan" then
		        
		        tspanStyle = buildStyleItem(node.Child(i))
		        elementStyle.ApplyValues(tspanStyle)
		        if node.Child(i).FirstChild <> nil then
		          if node.Child(i).FirstChild.Name = "#text" then
		            textStr = Trim(node.Child(i).FirstChild.Value)
		          end if
		        end if
		        
		        x = elementStyle.LookupDouble("x")
		        y = elementStyle.LookupDouble("y")
		        
		      end if
		      
		      if textStr <> "" then
		        
		        g.TextFont = elementStyle.LookupString("font-family", "Arial")
		        g.TextUnit = FontUnits.Pixel
		        g.TextSize = elementStyle.LookupDouble("font-size", 16)
		        g.Bold = false
		        if elementStyle.LookupString("font-weight", "") = "bold" then
		          g.Bold = true
		        end if
		        
		        mulMatrix = translationMatrix(x, y)
		        elementMatrix = matrixMultiply(matrix, mulMatrix)
		        
		        strShape.FillColor = determineColor(fill)
		        strShape.TextFont = g.TextFont
		        strShape.TextUnit = g.TextUnit
		        strShape.TextSize = g.TextSize * elementMatrix(0)
		        strShape.Bold = g.Bold
		        select case elementStyle.Lookup("text-anchor", "start")
		        case "end"
		          strShape.HorizontalAlignment = StringShape.Alignment.Right
		        case "middle"
		          strShape.HorizontalAlignment = StringShape.Alignment.Left
		          mulMatrix = translationMatrix(-g.StringWidth(textStr) / 2, 0)
		          elementMatrix = matrixMultiply(elementMatrix, mulMatrix)
		        case else
		          strShape.HorizontalAlignment = StringShape.Alignment.Left
		        end select
		        strShape.VerticalAlignment = StringShape.Alignment.BaseLine
		        strShape.Text = textStr
		        
		        // to speed up rendering and improve quality, we only use DrawTransformedPicture when needed
		        
		        if (elementMatrix(1) = 0) and (elementMatrix(3) = 0)  and (elementMatrix(6) = 0) and _
		          (elementMatrix(7) = 0) and (elementMatrix(8) = 1) and _
		          (elementMatrix(0) = elementMatrix(4)) then
		          
		          g.DrawObject strShape, elementMatrix(2), elementMatrix(5)
		          
		        else
		          element = new Picture(g.StringWidth(textStr), g.TextHeight)
		          eg = element.Graphics
		          
		          eg.DrawObject strShape, _
		          0, _
		          0
		          
		          g.DrawTransformedPicture element, elementMatrix
		        end if
		        
		      end if
		      
		      i = i + 1
		      
		    wend
		    
		  end if
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
		Private Sub transformPoint(ByRef x As Integer, ByRef y As Integer, matrix() As Double)
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

	#tag Property, Flags = &h21
		Private mNodes As Dictionary
	#tag EndProperty


	#tag Constant, Name = DegToRad, Type = Double, Dynamic = False, Default = \"0.0174533", Scope = Private
	#tag EndConstant

	#tag Constant, Name = Pi, Type = Double, Dynamic = False, Default = \"3.1415927", Scope = Private
	#tag EndConstant

	#tag Constant, Name = RadToDeg, Type = Double, Dynamic = False, Default = \"57.2958", Scope = Private
	#tag EndConstant


	#tag Enum, Name = SVGErrorEnum, Type = Integer, Flags = &h0
		MalformedXML=1
		  ExpectedPathCommand = 2
		  NodeNotFound = 3
		UnknownElement = 4
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
