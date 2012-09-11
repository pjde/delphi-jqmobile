Installation Notes

These components utilises the following third party software :-

1)	JQuery and JQuery Mobile Javascript and Image files (c) 2010-2012 The JQuery Foundation

	These can be downloaded for either rhe JQuery website :-

		http://code.jquery.com/mobile/1.0.1/jquery.mobile-1.0.1.zip

	or from Git Hub :-

		https://github.com/jquery/jquery-mobile
		
	Unzip the files into the wwwroot directory.
		
2)	The TWServerSocket and THTTPServer components from the ICS (Internet Component Suite) (c) 1999-2010 François PIETTE

	These can be downloaded as part of the ICS Suite from the Overbyte website :-
	
		http://www.overbyte.be
		
3)	The SHA1 encoder unit written by Dave Barton (davebarton@bigfoot.com). This file is included in the download.

Note that there has been a problem with some versions of Delphi registering the components. 
The demo now does not require the components to the registered within the IDE.

Note on Demo

To run the demo, ensure that the JSFolder property (look in FormCreate procedure) points to the location of the 
wwwroot folder.


              

			  
