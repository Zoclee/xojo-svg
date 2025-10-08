# xojo-svg
Library to render SVG images in Xojo.

## Installation

Copy the SVG module from the project located in the [source folder](https://github.com/Zoclee/xojo-svg/tree/main.src) to your own project.

## Example Code

	// Draw SVG stored as a string

	Sub Paint(g As Graphics, areas() As REALbasic.Rect)
		g.DrawSVG "Some SVG XML", 100, 100
	End Sub

	// Draw SVG stored in an XMLDocument object
	
	Sub Paint(g As Graphics, areas() As REALbasic.Rect)
		Dim myDoc As XmlDocument
		myDoc = new XmlDocument("Some SVG XML")
		g.DrawSVG myDoc, 100, 100
	End Sub	

## Support the Project

[![Donate](https://img.shields.io/badge/Donate-PayPal-blue.svg)](https://www.paypal.com/donate/?business=accounts@zoclee.com&no_recurring=0&currency_code=USD)
