#tag Module
Protected Module SVG
	#tag Method, Flags = &h0
		Sub DrawSVG(Extends g As Graphics, svg As String, x As Integer, y As Integer, w1 As Integer = -10000, h1 As Integer = -10000, sx As Integer = 0, sy As Integer = 0, w2 As Integer = -10000, h2 As Integer = -10000)
		  Dim xdoc As XmlDocument
		  Dim e As SVG.SVGException
		  
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
		Private Sub renderNode(node As XmlNode, g As Graphics, parentMatrix() As Double)
		  Var e As SVG.SVGException
		  
		  select case node.Name
		    
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
		    
		  case "svg"
		    render_svg(node, g, parentMatrix)
		    
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
		  Dim w As Double
		  Dim h As Double
		  Dim wStr As String
		  Dim hStr As String
		  Dim svgImage As Picture
		  
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
		      renderNode(xdoc.Child(i), svgImage.Graphics, matrix)
		      
		    end if
		    i = i + 1
		  wend
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub render_svg(node As XmlNode, g As Graphics, parentMatrix() As Double)
		  Var matrix() As Double
		  Var i As Integer
		  
		  //matrix = buildTransformationMatrix(localStyle.Lookup("transform", ""))
		  //matrix = matrixMultiply(parentMatrix, matrix)
		  matrix = parentMatrix
		  
		  i = 0
		  while i < node.ChildCount
		    renderNode node.Child(i), g, matrix
		    i = i + 1
		  wend
		  
		End Sub
	#tag EndMethod


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
