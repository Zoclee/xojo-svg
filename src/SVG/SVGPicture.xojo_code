#tag Class
Protected Class SVGPicture
	#tag Method, Flags = &h0
		Shared Function FromData(data As MemoryBlock) As SVG.SVGPicture
		  Var img As SVG.SVGPicture
		  
		  img = new SVG.SVGPicture()
		  img.SVGString = data
		  
		  return img
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub InitProperties()
		  Var foundProps As Boolean
		  Var i As Integer
		  Var node As XMLNode
		  Var viewBox As String
		  Var parts() As String
		  
		  mWidth = 0
		  mHeight = 0
		  
		  if mSVGDocument <> nil then
		    
		    foundProps = false
		    i = 0
		    while (i < mSVGDocument.ChildCount) and not foundProps
		      node = mSVGDocument.Child(i)
		      if node.Name = "svg" then
		        mWidth = Val(node.GetAttribute("width"))
		        mHeight = Val(node.GetAttribute("height"))
		        
		        // Fallback to viewBox dimensions when width/height are omitted.
		        if (mWidth <= 0) or (mHeight <= 0) then
		          viewBox = node.GetAttribute("viewBox").Trim()
		          if viewBox = "" then
		            viewBox = node.GetAttribute("viewbox").Trim()
		          end if
		          
		          if viewBox <> "" then
		            viewBox = viewBox.ReplaceAll(",", " ")
		            while viewBox.IndexOf("  ") >= 0
		              viewBox = viewBox.ReplaceAll("  ", " ")
		            wend
		            
		            parts = viewBox.Split(" ")
		            if parts.LastIndex >= 3 then
		              if mWidth <= 0 then
		                mWidth = Val(parts(2))
		              end if
		              if mHeight <= 0 then
		                mHeight = Val(parts(3))
		              end if
		            end if
		          end if
		        end if
		        
		        foundProps = true
		      end if
		      i = i + 1
		    wend
		    
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Shared Function Open(file As FolderItem) As SVG.SVGPicture
		  Var img As SVG.SVGPicture
		  Var tis As TextInputStream
		  
		  img = new SVG.SVGPicture()
		  
		  tis = TextInputStream.Open(file)
		  img.SVGString = tis.ReadAll()
		  tis.Close
		  
		  return img
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ToPicture() As Picture
		  if mPicture = nil then
		    if mWidth > 0 and mHeight > 0 then
		      mPicture = new Picture(mWidth, mHeight)
		      mPicture.Graphics.DrawSVG(mSVGDocument, 0, 0, mWidth, mHeight)
		    end if
		  end if
		  
		  return mPicture
		  
		End Function
	#tag EndMethod


	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  Return mHeight
			End Get
		#tag EndGetter
		Height As Integer
	#tag EndComputedProperty

	#tag Property, Flags = &h21
		Private mHeight As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		mPicture As Picture
	#tag EndProperty

	#tag Property, Flags = &h21
		Private mSVGDocument As XmlDocument
	#tag EndProperty

	#tag Property, Flags = &h21
		Private mSVGString As String
	#tag EndProperty

	#tag Property, Flags = &h21
		Private mWidth As Integer
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  Return mSVGDocument
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  mSVGDocument = value
			  mSVGString = mSVGDocument.ToString()
			  
			  InitProperties()
			End Set
		#tag EndSetter
		SVGDocument As XmlDocument
	#tag EndComputedProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  Return mSVGString
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  Var e As SVG.SVGException
			  
			  mSVGString = value
			  
			  mSVGDocument = nil
			  
			  if mSVGString.Length > 0 then
			    
			    try
			      
			      mSVGDocument = new XmlDocument(mSVGString)
			      InitProperties()
			      
			    catch xmlException As XmlException
			      
			      // invalid xml, so raise an exception
			      
			      e = new SVG.SVGException()
			      e.ErrorNumber = Integer(SVGErrorEnum.MalformedXML)
			      e.Message = "Malformed XML."
			      Raise e
			      
			    end try
			    
			  end if
			  
			End Set
		#tag EndSetter
		SVGString As String
	#tag EndComputedProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  Return mWidth
			End Get
		#tag EndGetter
		Width As Integer
	#tag EndComputedProperty


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
End Class
#tag EndClass
