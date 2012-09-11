/*
   AJAX and WebSocket functionality for Delphi JQuery Mobile Components

  Copyright (c) 2012, PJ Design Engineering P/L (pjde)

  JQuery, JQuery Mobile (c) 2010-2012 The JQuery Foundation
  ICS (Internet Component Suite) (c) 1999-2010 François PIETTE
  uSHA1.pas written by Dave Barton (davebarton@bigfoot.com)

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
     * Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.
     * Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.
     * Neither the name of the copyright holder nor the
       names of its contributors may be used to endorse or promote products
       derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL MIKKO KOPPANEN BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  
  Version 1.1	Jul 12
	Keypad changed to handle columns 
	PageSetup called upon SessionInit
*/

var timerID = 0;
var socket = null;
var uws = new Boolean ();		// use ws
var WSParams = "";
var pin = "";
var hashes = "********************************";

/* WebSocket States
0: request not initialized
1: server connection established
2: request received
3: processing request
4: request finished and response is ready */

function GetParams ()	// extract all hidden inputs on page for html
{
	var res = "";
	
	$("input[type=hidden]").each (function (i) 
	{
		res = res + "&" + $(this).attr ("name") + "=" + $(this).attr ("value");
	});
	return res;
}

function GetParams2 ()	// extract all hidden inputs on page for xml 
{
	var res = "";
	
	$("input[type=hidden]").each (function (i) 
	{
		res = res + ' ' + $(this).attr ('name') + '="' + $(this).attr ('value') + '"';
	});
	return res;
}

function decode (xml)
{
	var elems;
	var elem;
	var index;
	var reftype = "";
	var x = 0;
	var y = 0;
	var group;
	
	var xmlDoc = $.parseXML (xml);
	if (xmlDoc)
	{
		elems = $(xmlDoc).find ("state");
		$(elems).each (function (i)
		{
			elem = $(this).attr ("name");
			index = parseInt ($(this).attr ("index"));
			if (index != index)	index = 0;						// default is 0	if not defined
			if (elem != undefined)
			{
				reftype = $("#" + elem).attr ("reftype");				
				if (reftype == "check")		// check box - actually label
				{
					elem = $("input[name=" + elem + "]");	// find corresponding box 
					if ($(this).attr ("value") == "true")
						$(elem).attr ("checked", true);
					else
						$(elem).attr ("checked", false);
					$(elem).checkboxradio ("refresh");
				}
				else if (reftype == "switch")	// slider switch
				{
					if ($(this).attr ("value") == "true")
						$("#" + elem).val ("on");
					else
						$("#" + elem).val ("off");
					$("#" + elem).slider ('refresh');						// refresh slider
				}
				else if (reftype == "checkgroup")
				{
					group = $("#" + elem).find ("input");
					if (index >= 0 && index < group.length)
					{
						if ($(this).attr ("value") == "true")
							$(group[index]).attr ("checked", true);
						else
							$(group[index]).attr ("checked", false);
						$(group[index]).checkboxradio ("refresh");
					}
				}
				else if (reftype == "radiogroup")
				{
					if ($(this).attr ("value") == "true" && index >= 0)
					{
						group = $("#" + elem).find ("input");
						if (index < group.length) 
						{
							for (y = 0; y < group.length; y ++)
							{
								if ($(group[y]).attr ("checked") == undefined)
								{
									if (y == index)	$(group[y]).attr ("checked", "checked");
								}
								else
								{
									if (y != index) $(group[y]).removeAttr ("checked");
								}
								$(group[y]).checkboxradio ("refresh");		
							}
						}
					}
				}
				else if (reftype == "menu")
				{
					if ($(this).attr ("value") == "true" && index >= 0)
					{
						$("#" + elem).prop ("selectedIndex", index); 	
						$("#" + elem).selectmenu ('refresh');
					}
				}
			}
		});
		elems = $(xmlDoc).find ("amount");
		$(elems).each (function (i)
		{
			elem = $(this).attr ("name");
			index = parseInt ($(this).attr ("index"));
			if (index != index)	index = 0;						// default is 0	if not defined
			if (elem != undefined)
			{
				reftype = $("#" + elem).attr ("reftype");
				if (reftype == "slider")									// range slider
				{
					$("#" + elem).attr ("value", $(this).attr ("value")); 	// set value of slider
					$("#" + elem).slider ('refresh');						// refresh slider
				}
				else if (reftype == "menu")									// context menu 
				{
					$("#" + elem).prop ("selectedIndex", $(this).attr ("value")); 	
					$("#" + elem).selectmenu ('refresh');
				}
				else if (reftype = "radiogroup")							// radio group
				{
					group = $("#" + elem).find ("input");
					x = parseInt ($(this).attr ("value"));
					if (x == x && x < group.length) 
					{
						for (y = 0; y < group.length; y ++)
						{
							if ($(group[y]).attr ("checked") == undefined)
							{
								if (y == x)	$(group[y]).attr ("checked", "checked");
							}
							else
							{
								if (y != x) $(group[y]).removeAttr ("checked");
							}
							$(group[y]).checkboxradio ("refresh");		
						}						
					}	
				}
			}
		});
		elems = $(xmlDoc).find ("text");
		$(elems).each (function (i)
		{
			elem = $(this).attr ("name");
			index = parseInt ($(this).attr ("index"));
			if (index != index)	index = 0;						// default is 0	if not defined
			if (elem != undefined || elem != "")
			{
				reftype = $("#" + elem).attr ("reftype");
				if (reftype == "btn")
					$("#" + elem + " .ui-btn-text").text ($(this).attr ("value")); 		// buttons & others
				else if (reftype == "footer" || reftype == "header")
					$("#" + elem + " .ui-title").text ($(this).attr ("value"));			// headers & footers
				else if (reftype == "label")
				{
					group = $("#" + elem).find ("td");
					if (index >= 0 && index < group.length)
						$(group[index]).find (".ui-label-text").text ($(this).attr ("value"));
				}
				else if (reftype == "radiogroup")
				{
					group = $("#" + elem).find ("label");
					if (index >= 0 && index < group.length)
						$(group[index]).find (".ui-btn-text").text ($(this).attr ("value"));
				}
				else if (reftype == "checkgroup")
				{
					group = $("#" + elem).find ("label");
					if (index >= 0 && index < group.length)
						$(group[index]).find (".ui-btn-text").text ($(this).attr ("value"));
				}
				else if (reftype == "slider")
				{
//					alert ($("#" + elem).html());
				}
				else if (reftype == "menu")	// this does not work yet 
				{
					group = $("#" + elem).find ("option");
//					alert (group.length);
//					for (x = 0; x < group.length; x ++)
//						alert ($(group[x]).attr ("id"));
					if (index >= 0 && index < group.length)
					{	
						$(group[index]).val ($(this).attr ("value")); // find (".ui-label-text").html ($(this).attr ("value"));
//						$(group[index]).find (".ui-label-text").html ($(this).attr ("value"));
					}
					$("#" + elem).selectmenu ('refresh');
//					alert ($(group[index]).html());
//					$(group[index]).find (".ui-label-text").text ($(this).attr ("value"));
				}
				else
				{
//					alert ("text name " + elem + " ref type " + reftype + " index " + index);				
//					alert ($(this).html());
				}
			}
		});
		elems = $(xmlDoc).find ("page");
		$(elems).each (function (i)
		{
			elem = $(this).attr ("name");
			if (elem != undefined)
			{
				var extras = $(this).attr ("value");
				if (extras.length > 0)
					$.mobile.changePage (elem, extras);
				else
					$.mobile.changePage (elem);
			}
		});
	}
	else
		alert ("error in xms document");
}

function BtnTapped (name)
{
	if (uws)		// use web scripts
	{
		if (socket != null) 
			socket.send ('<tapped name="' + name + '"' + GetParams2 () +  '/>\r');
	}
	else
	{ 
		$.get ("/tapped.cgi", "name=" + name + GetParams (), function (data) 
		{	
			decode (data);
		});
	}
}

function BtnTapHeld (name)
{
	if (uws)		// use web scripts
	{
		if (socket != null) 
			socket.send ('<tapheld name="' + name + '"' + GetParams2 () +  '/>\r');
	}
	else
	{
		$.get ("/tapheld.cgi", "name=" + name + GetParams (), function (data) 
		{	
			decode (data);
		});
	}
}

function PageSwiped (dir)
{
	if (uws)		// use web scripts
	{
		if (socket != null) 
			socket.send ('<swiped name="' + dir + '"' + GetParams2 () +  '/>\r');
	}
	else
	{
		$.get ("/swiped.cgi", "name=" + dir + GetParams (), function (data) 
		{	
			decode (data);
		});
	}
}

function StateChanged (name, index, value)	
{
	if (uws)		// use web scripts
	{
		if (socket != null) 
			socket.send ('<statechanged name="' + name + '" index="' +  index + '" value="' + value + '"' + GetParams2 () +  '/>\r');
	}
	else
	{
		$.get ("/statechanged.cgi", "name=" + name + "&index=" + index + "&value=" + value + GetParams (), function (data) 
		{	
			decode (data);
		});
	}
}

function ValueChanged (name, index, value)
{
	if (uws)		// use web scripts
	{
		if (socket != null) 
			socket.send ('<valuechanged name="' + name + '" index="' +  index + '" value="' + value + '"' + GetParams2 () +  '/>\r');
	}
	else
	{
		$.get ("/valuechanged.cgi", "name=" + name +  "&index=" + index + "&value=" + value + GetParams (), function (data) 
		{	
			decode (data);
		});
	}
}

function TextChanged (name, index, value)
{
	if (uws)		// use web scripts
	{
		if (socket != null) 
			socket.send ('<textchanged name="' + name + '" index="' +  index + '" value="' + value + '"' + GetParams2 () +  '/>\r');
	}
	else
	{
		$.get ("/textchanged.cgi", "name=" + name +  "&index=" + index + "&value=" + value + GetParams (), function (data) 
		{	
			decode (data);
		});
	}
}

function WSConnect ()
{
	if ("MozWebSocket" in window)
	{
		socket = new MozWebSocket ("ws://" + WSParams + "/websocket", "klingon");  
	}
	else if ("WebSocket" in window)
	{
		socket = new WebSocket ("ws://" + WSParams + "/websocket", "klingon");  
	}
	else
		return;
	socket.onopen = function (evt)
	{  
		var id = "";
		var i, n;
		
		uws = true;									// using ws
		var cookies = document.cookie.split (";");
		for (i = 0; i < cookies.length; i ++)		// send session id as firefox no longer sends cookies 
		{											// thru websocket initialisation
			n = cookies[i].substr (0, cookies[i].indexOf ("="));
			n = n.replace (/^\s+|\s+$/g,"");
			if (n == "JQSID")
			{
				id = unescape (cookies[i].substr (cookies[i].indexOf ("=") + 1));
				break;
			}
		} 
		socket.send ('<session id="' + id + '"/>\r');
	}  
    socket.onmessage = function (msg)
	{  
		decode (msg.data);
	}  
    socket.onclose = function (evt)
	{
		uws = false;
		// attempt to reconnect
		//alert ("Socket Status: " + socket.readyState + " (Closed)" + evt);
	}
	socket.onerror = function (evt)
	{
	//	alert ("Socket error " + evt.type); 
	}
}

function PageSetup (page)
{
	var x = 0;
	
	pin = "";
	if (uws)							// use web sockets
	{
		if (socket != null) 
			socket.send ('<page name="' + $(page).attr ("id") + '"' + GetParams2 () +  '/>\r');
	}
	else
	{
		$.get ("/page.cgi", "name=" + $(page).attr ("id") + GetParams (), function (data) 
		{	
			decode (data);
		});
	}
	$(page).swipeleft (function () 
	{
		PageSwiped ("left");
	});
	$(page).swiperight (function () 
	{
		PageSwiped ("right");
	});
	$("[data-role=button]").each (function (i)						// buttons
	{
		if ($(this).attr ("href") == "" || $(this).attr ("href") == undefined)
		{
			if ($(this).attr ("id"))
			{
				$(this).tap (function () 
				{
					BtnTapped ($(this).attr ("id"));
				});
				$(this).taphold (function () 
				{
					BtnTapHeld ($(this).attr ("id"));
				});
			}
			else if ($(this).attr ("tag")) 						// key pad
			{
				$(this).tap (function () 
				{
					x = parseInt ($(this).attr ("tag"));
					var y = $(this).attr ("for");			// get id of table
										
					if (x == 11)
					{
						TextChanged (y, 0, pin);
						pin = "";
					}
					else if (x >= 0 && x <= 8)
						pin += x + 1;
					else if (x == 10)
						pin += "0";
					else if (x == 9)
						pin = "";
					if (pin.length == 0)
						$("#" + y + " .ui-btn-text").text ($("#" + y).attr ("caption"));
					else if ($("#" + y).attr ("pass"))
						$("#" + y + " .ui-btn-text").text (hashes.substring (0, pin.length));	
					else
						$("#" + y + " .ui-btn-text").text (pin);
				});
			}
		}
	});
	$("[data-role=header]").each (function (i)
	{
		if ($(this).attr ("id"))
		{
			$(this).tap (function () 
			{
				BtnTapped ($(this).attr ("id"));
			});
			$(this).taphold (function () 
			{
				BtnTapHeld ($(this).attr ("id"));
			});
		}
	});
	$("[data-role=footer]").each (function (i)
	{
		if ($(this).attr ("id"))
		{
			$(this).tap (function () 
			{
				BtnTapped ($(this).attr ("id"));
			});
			$(this).taphold (function () 
			{
				BtnTapHeld ($(this).attr ("id"));
			});
		}
	});
	$("input[type=number]").each (function (i) 						// slider
	{
		$(this).change (function () 
		{
			ValueChanged ($(this).attr ("id"), 0, $(this).attr ("value"));
		});
	});
	$("input[type=checkbox]").each (function (i) // checkbox 
	{
		$(this).change (function () 
		{
			var id = $(this).attr ("name");
			x = id.lastIndexOf ("-");
			if (x >= 0)
			{
				var i = parseInt (id.substring (x + 1));
				if (i == i) 
				{
					if ($(this).attr ("checked") != undefined)
						StateChanged ($(this).attr ("for"), i, "on");
					else
						StateChanged ($(this).attr ("for"), i, "off");
				}
			}
		});
	}); 
	$("fieldset[reftype=radiogroup]").each (function (i) 			// radio button group 
	{
		$(this).change (function () 
		{
			var group = $(this).find ("input");
			for (var y = 0; y < group.length; y ++)
			{
				if ($(group[y]).attr ("checked") == "checked")
				{
					ValueChanged ($(this).attr ("id"), 0, y);
					break;
				}
			}
		});
	});	
	$("select[data-role=slider]").each (function (i) 				// slider buttons 
	{
		$(this).change (function () 
		{
			StateChanged ($(this).attr ("name"), 0, $(this).val ());
		});
	});
	$("select[data-native-menu=false]").each (function (i) 			// custom menus  
	{
		$(this).change (function () 
		{
			ValueChanged ($(this).attr ("name"), 0,  $(this).prop ("selectedIndex"));
		});
	});
	
	// setup timer if required 
	var interval = 0;
	$("input[type=hidden]").each (function (i) 
	{
		if ($(this).attr ("name") == "timer")
			interval = $(this).attr ('value');
	});	
	if (interval == undefined) interval = 0;
	if (interval > 0)
	{
		if (timerID > 0) window.clearInterval (timerID);
		timerID = window.setInterval (function () 
		{
			var	params = GetParams ();
			if (params.length > 0) params = params.substring (1); // remove first &
			$.get ("/timer.cgi", params, function (data) 
			{	
				decode (data);
			});
		}, interval);
	}
	else if (timerID > 0)
		window.clearInterval (timerID);
}

function SessionInit (params)
{
	uws = false;
	WSParams = params;
	WSConnect ();
	var p = $("div[data-role=page]");
	if (p.length > 0) PageSetup (p[0]);
}

