#tag Class
Protected Class SVGPicture
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


	#tag Property, Flags = &h21
		Private mSVGString As String
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  Return mSVGString
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  mSVGString = value
			End Set
		#tag EndSetter
		SVGString As String
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
