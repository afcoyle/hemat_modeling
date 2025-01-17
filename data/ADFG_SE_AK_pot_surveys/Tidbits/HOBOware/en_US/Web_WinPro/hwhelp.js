// {{MadCap}} //////////////////////////////////////////////////////////////////
// Copyright: MadCap Software, Inc - www.madcapsoftware.com ////////////////////
////////////////////////////////////////////////////////////////////////////////
// <version>4.0.0.0</version>
////////////////////////////////////////////////////////////////////////////////

//    Syntax:
//    function FMCOpenHelp( id, skinName )
//
//    id          - Identifier that was created in Flare. This can be either the identifier name or value. The topic and skin
//                  that is associated with the id will be used. If no skin is associated with the id, skinName will be used.
//                  Alternatively, id may contain a topic path. In this case, the specified topic will be loaded with the skin
//                  that is specified in skinName. Specify null to use the help system's default starting topic.
//    skinName    - This is a string indicating the name of the skin to use when opening the help system. Specify null to use
//                  the default skin or to use the skin that is associated with id. If a skin is associated with id AND a skin
//                  is specified in skinName, skinName will take precedence.
//    searchQuery - This is a string indicating the search query used when opening the help system. If a search query is specified,
//                  the help system will start with the search pane open and the search query executed. Specify null to open
//                  the help system without a search query.
//    firstPick   - This is a boolean indicating whether to automatically open the topic from the first search result that is
//                  returned by the search query (see searchQuery parameter). Use null if no search query was specified.
//
//    Examples:
//
//    In the following example, topic and skin associated with "FILE_NEW" will be used:
//    FMCOpenHelp( 'FILE_NEW', null, null, null );
//
//    In the following example, topic associated with "FILE_NEW" will be used. "BlueSkin" will override the skin associated with "FILE_NEW":
//    FMCOpenHelp( 'FILE_NEW', 'BlueSkin', null, null );
//
//    In the following example, topic and skin associated with identifier value 1 will be used:
//    FMCOpenHelp( 1, null, null, null );
//
//    In the following example, topic associated with identifier value 1 will be used. "BlueSkin" will override the skin associated with identifier value 1:
//    FMCOpenHelp( 1, 'BlueSkin', null, null );
//
//    In the following example, "Company/Employees.htm" will be used with the default skin:
//    FMCOpenHelp( 'Company/Employees.htm', null, null, null );
//
//    In the following example, both the default topic and skin will be used:
//    FMCOpenHelp( null, null, null, null );
//
//    In the following example, the default topic will be used with "BlueSkin":
//    FMCOpenHelp( null, 'BlueSkin', null, null );
//
//    In the following example, both the default topic and skin will be used. The help system will be started with the search pane
//    displaying the search results for the query 'quarterly report'. The topic from the first result will not be opened:
//    FMCOpenHelp( null, null, 'quarterly report', false );
//
//    In the following example, both the default topic and skin will be used. The help system will be started with the search pane
//    displaying the search results for the query 'quarterly report'. The topic from the first result will be opened:
//    FMCOpenHelp( null, null, 'quarterly report', true );

var gHelpSystemName = "hwhelp.htm";

function FMCOpenHelp( id, skinName, searchQuery, firstPick )
{
	var cshFileName	= gHelpSystemName.substring( 0, gHelpSystemName.lastIndexOf( "." ) ) + ".js";
	var scriptNodes	= document.getElementsByTagName( "script" );
	var webHelpPath	= null;
	var webHelpFile	= null;

	for ( var i = 0; i < scriptNodes.length; i++ )
	{
		var src		= scriptNodes[i].src;
		var srcUrl	= new CMCUrl( src );
		
		if ( srcUrl.NameWithExtension.toLowerCase() == cshFileName.toLowerCase() )
		{
			var locUrl	= new CMCUrl( document.location.href ).ToFolder();
			
			webHelpPath = locUrl.AddFile( srcUrl );
			webHelpPath = webHelpPath.ToFolder();

			var pos			= gHelpSystemName.lastIndexOf( "." );
			var fileName	= gHelpSystemName.substring( 0, pos );
			var fileExt		= gHelpSystemName.substring( pos );
			var cshFileUrl	= new CMCUrl( fileName + "_CSH" + fileExt );
			
			webHelpFile = webHelpPath.AddFile( cshFileUrl );

			break;
		}
	}
	
	if ( webHelpFile == null )
	{
		throw "CSH failed: could not find MadCap CSH script in page.";
	}

	FMCOpenHelp2( webHelpFile, id, skinName, searchQuery, firstPick );
}

function FMCOpenHelp2( webHelpFileUrl, id, skinName, searchQuery, firstPick )
{
	var webHelpPath		= webHelpFileUrl.ToFolder().FullPath;
	var helpSystemFile	= webHelpPath + gHelpSystemName.substring( 0, gHelpSystemName.lastIndexOf( "." ) ) + ".xml";
	var helpSystem		= new CMCHelpSystem( null, webHelpPath, helpSystemFile, null );
	var idInfo			= helpSystem.LookupCSHID( id );
	var topic			= idInfo.Topic;
	var skin			= idInfo.Skin;
	
	if ( skinName )
	{
		skin = skinName;
	}

	// Browser setup options

	var browserOptions	= "";
	var size			= "";

	if ( skin )
	{
		xmlDoc = CMCXmlParser.GetXmlDoc( webHelpPath + "Data/Skin" + skin + "/Skin.xml", false, null, null );

		if ( xmlDoc )
		{
			var xmlHead		= xmlDoc.getElementsByTagName( "CatapultSkin" )[0];
			var useDefault	= FMCGetAttributeBool( xmlHead, "UseDefaultBrowserSetup", false );

			if ( !useDefault )
			{
				var toolbar		= "no";
				var menu		= "no";
				var locationBar	= "no";
				var statusBar	= "no";
				var resizable	= "no";
				var setup		= xmlHead.getAttribute( "BrowserSetup" );

				if ( setup )
				{
					toolbar     = (setup.indexOf( "Toolbar" ) > -1)     ? "yes" : "no";
					menu        = (setup.indexOf( "Menu" ) > -1)        ? "yes" : "no";
					locationBar = (setup.indexOf( "LocationBar" ) > -1) ? "yes" : "no";
					statusBar   = (setup.indexOf( "StatusBar" ) > -1)   ? "yes" : "no";
					resizable   = (setup.indexOf( "Resizable" ) > -1)   ? "yes" : "no";
				}

				browserOptions = "toolbar=" + toolbar + ", menubar=" + menu + ", location=" + locationBar + ", status=" + statusBar + ", resizable=" + resizable;
			}

			var windowSize	= FMCLoadSize( xmlDoc );

			if ( windowSize )
			{
				size = ", top=" + windowSize.topPx + ", left=" + windowSize.leftPx + ", width=" + windowSize.widthPx + ", height=" + windowSize.heightPx;
			}
		}
	}

	//
	
	var cshString	= webHelpFileUrl.FullPath;
	
	if ( searchQuery )
	{
		cshString += "?" + searchQuery;

		if ( firstPick )
		{
			cshString += "|FirstPick";
		}
	}

	if ( topic )
	{
		cshString += "#Topic=" + topic;
	}

	if ( skin )
	{
		if ( cshString.indexOf( "#" ) != -1 )
		{
			cshString += "|";
		}
		else
		{
			cshString += "#";
		}

		cshString += "Skin=" + skin;
	}
	
	if ( cshString.indexOf( "#" ) != -1 )
	{
		cshString += "|";
	}
	else
	{
		cshString += "#";
	}
	
	cshString += "OpenType=Javascript";

	window.open( cshString, "_MCWebHelpCSH", browserOptions + size );
}

function FMCLoadSize( xmlDoc )
{
	var xmlHead			= xmlDoc.documentElement;
	var useDefaultSize	= FMCGetAttributeBool( xmlHead, "UseBrowserDefaultSize", false );

	if ( useDefaultSize )
	{
		return null;
	}

	var topPx		= FMCConvertToPx( document, xmlHead.getAttribute( "Top" ), null, 0 );
	var leftPx		= FMCConvertToPx( document, xmlHead.getAttribute( "Left" ), null, 0 );
	var bottomPx	= FMCConvertToPx( document, xmlHead.getAttribute( "Bottom" ), null, 0 );
	var rightPx		= FMCConvertToPx( document, xmlHead.getAttribute( "Right" ), null, 0 );
	var widthPx		= FMCConvertToPx( document, xmlHead.getAttribute( "Width" ), "Width", 800 );
	var heightPx	= FMCConvertToPx( document, xmlHead.getAttribute( "Height" ), "Height", 600 );

	var anchors = xmlHead.getAttribute( "Anchors" );

	if ( anchors )
	{
		var aTop	= (anchors.indexOf( "Top" ) > -1)    ? true : false;
		var aLeft	= (anchors.indexOf( "Left" ) > -1)   ? true : false;
		var aBottom	= (anchors.indexOf( "Bottom" ) > -1) ? true : false;
		var aRight	= (anchors.indexOf( "Right" ) > -1)  ? true : false;
		var aWidth	= (anchors.indexOf( "Width" ) > -1)  ? true : false;
		var aHeight	= (anchors.indexOf( "Height" ) > -1) ? true : false;
	}

	if ( aLeft && aRight )
	{
		widthPx = screen.width - (leftPx + rightPx);
	}
	else if ( !aLeft && aRight )
	{
		leftPx = screen.width - (widthPx + rightPx);
	}
	else if ( aWidth )
	{
		leftPx = (screen.width / 2) - (widthPx / 2);
	}

	if ( aTop && aBottom )
	{
		heightPx = screen.height - (topPx + bottomPx);
	}
	else if ( !aTop && aBottom )
	{
		topPx = screen.height - (heightPx + bottomPx);
	}
	else if ( aHeight )
	{
		topPx = (screen.height / 2) - (heightPx / 2);
	}

	//

	var windowSize	= new Object();

	windowSize.topPx = topPx;
	windowSize.leftPx = leftPx;
	windowSize.widthPx = widthPx;
	windowSize.heightPx = heightPx;

	return windowSize;
}


//
//    Class CMCAliasFile
//

function CMCAliasFile( xmlFile, helpSystem )
{
	// Private member variables

	var mXmlDoc		= null;
	var mHelpSystem	= helpSystem;
	var mNameMap	= null;
	var mIDMap		= null;

	// Public properties

	// Constructor

	(function()
	{
		mXmlDoc = CMCXmlParser.GetXmlDoc( xmlFile, false, null, null );
	})();

	// Public member functions
	
	this.GetIDs	= function()
	{
		var ids	= new Array();
		
		AssureInitializedMap();

		mIDMap.ForEach( function( key, value )
		{
			ids[ids.length] = key;
			
			return true;
		} );
		
		return ids;
	};
	
	this.GetNames	= function()
	{
		var names	= new Array();
		
		AssureInitializedMap();

		mNameMap.ForEach( function( key, value )
		{
			names[names.length] = key;
			
			return true;
		} );
		
		return names;
	};

	this.LookupID	= function( id )
	{
		var found	= false;
		var topic	= null;
		var skin	= null;

		if ( id )
		{
			if ( typeof( id ) == "string" && id.indexOf( "." ) != -1 )
			{
				var pipePos	= id.indexOf( "|" );

				if ( pipePos != -1 )
				{
					topic = id.substring( 0, pipePos );
					skin = id.substring( pipePos + 1 );
				}
				else
				{
					topic = id;
				}
			}
			else
			{
				var mapInfo	= GetFromMap( id );
				
				if ( mapInfo != null )
				{
					found = true;
					topic = mapInfo.Topic;
					skin = mapInfo.Skin;
				}
			}
		}
		else
		{
			found = true;
		}

		if ( !skin )
		{
			if ( mXmlDoc )
			{
				skin = mXmlDoc.documentElement.getAttribute( "DefaultSkinName" );
			}
		}
		
		if ( topic )
		{
			if ( !mHelpSystem.MoveOutputContentToRoot )
			{
				topic = mHelpSystem.ContentFolder + topic;
			}
		}
		
		return { Found: found, Topic: topic, Skin: skin };
	};

	// Private member functions
	
	function GetFromMap( id )
	{
		var mapInfo	= null;
		
		AssureInitializedMap();

		if ( mNameMap != null )
		{
			if ( typeof( id ) == "string" )
			{
				mapInfo = mNameMap.GetItem( id );
				
				if ( mapInfo == null )
				{
					mapInfo = mIDMap.GetItem( id );
				}
			}
			else if ( typeof( id ) == "number" )
			{
				mapInfo = mIDMap.GetItem( id.toString() );
			}
		}

		return mapInfo;
	}
	
	function AssureInitializedMap()
	{
		if ( mNameMap == null )
		{
			if ( mXmlDoc )
			{
				mNameMap = new CMCDictionary();
				mIDMap = new CMCDictionary();
				
				var maps	= mXmlDoc.documentElement.getElementsByTagName( "Map" );

				for ( var i = 0; i < maps.length; i++ )
				{
					var topic	= maps[i].getAttribute( "Link" );
					var skin	= maps[i].getAttribute( "Skin" );
					
					if ( skin )
					{
						skin = skin.substring( "Skin".length, skin.indexOf( "/" ) );
					}
					
					var currMapInfo	= { Topic: topic, Skin: skin };
					
					var name	= maps[i].getAttribute( "Name" );
					
					if ( name != null )
					{
						mNameMap.Add( name, currMapInfo );
					}
					
					var resolvedId	= maps[i].getAttribute( "ResolvedId" );
					
					if ( resolvedId != null )
					{
						mIDMap.Add( resolvedId, currMapInfo );
					}
				}
			}
		}
	}
}

//
//    End class CMCAliasFile
//


//
//    Class CMCHelpSystem
//

function CMCHelpSystem( parentSubsystem, parentPath, xmlFile, tocPath )
{
	// Private member variables

	var mSelf				= this;
	var mParentSubsystem	= parentSubsystem;
	var mPath				= parentPath;
	var mXmlFile			= xmlFile;
	var mSubsystems			= new Array();
	var mTocPath			= tocPath;
	var mConceptMap			= null;
	var mViewedConceptMap	= new CMCDictionary();
	var mExists				= false;
	var mAliasFile			= new CMCAliasFile( parentPath + "Data/Alias.xml", this );

	// Public properties

	this.TargetType			= null;
	this.LiveHelpOutputId	= null;
	this.LiveHelpServer		= null;
	this.LiveHelpEnabled	= false;
	this.IsWebHelpPlus		= false;
	this.ContentFolder		= null;
	
	this.GlossaryUrl		= null;

	// Constructor

	(function()
	{
		var xmlDoc	= CMCXmlParser.GetXmlDoc( xmlFile, false, null, null );
	    
		mExists = xmlDoc != null;
	    
		if ( !mExists )
		{
			return;
		}

		if ( xmlDoc.getElementsByTagName( "Subsystems" ).length > 0 )
		{
			var urlNodes    = xmlDoc.getElementsByTagName( "Subsystems" )[0].getElementsByTagName( "Url" );
	        
			for ( var i = 0; i < urlNodes.length; i++ )
			{
				var urlNode	= urlNodes[i];
				var url		= urlNode.getAttribute( "Source" );
				var subPath	= url.substring( 0, url.lastIndexOf( "/" ) + 1 );
				var tocPath	= urlNode.getAttribute( "TocPath" );
	            
				mSubsystems.push( new CMCHelpSystem( mSelf, mPath + subPath, mPath + url.substring( 0, url.lastIndexOf( "." ) ) + ".xml", tocPath ) );
			}
		}
		
		mSelf.TargetType = xmlDoc.documentElement.getAttribute( "TargetType" );
		mSelf.LiveHelpOutputId = xmlDoc.documentElement.getAttribute( "LiveHelpOutputId" );
		mSelf.LiveHelpServer = xmlDoc.documentElement.getAttribute( "LiveHelpServer" );
		mSelf.LiveHelpEnabled = mSelf.LiveHelpOutputId != null;
		mSelf.IsWebHelpPlus = mSelf.TargetType == "WebHelpPlus" && document.location.protocol.StartsWith( "http", false );
		
		var moveOutputContentToRoot	= FMCGetAttributeBool( xmlDoc.documentElement, "MoveOutputContentToRoot", false );
		var makeFileLowerCase		= FMCGetAttributeBool( xmlDoc.documentElement, "MakeFileLowerCase", false );
		var contentFolder			= "";
		
		if ( !moveOutputContentToRoot )
		{
			contentFolder = "Content/";
		}
		
		if ( makeFileLowerCase )
		{
			contentFolder = contentFolder.toLowerCase();
		}
		
		mSelf.ContentFolder = contentFolder;

		mSelf.GlossaryUrl = GetGlossaryUrl( xmlDoc );
	})();

	// Public member functions
    
	this.GetExists      = function()
	{
		return mExists;
	};
    
	this.GetParentSubsystem = function()
	{
		return mParentSubsystem;
	};
    
	this.GetPath        = function()
	{
		return mPath;
	};
    
	this.GetTocPath     = function()
	{
		return mTocPath;
	};
    
	this.GetFullTocPath = function( tocPath )
	{
		if ( mParentSubsystem )
		{
			if ( !String.IsNullOrEmpty( mTocPath ) )
			{
				tocPath.tocPath = tocPath.tocPath ? mTocPath + "|" + tocPath.tocPath : mTocPath;
			}
			
			mParentSubsystem.GetFullTocPath( tocPath );
		}
	};
    
	this.GetHelpSystem  = function( path )
	{
		var helpSystem	= null;
	    
		for ( var i = 0; i < mSubsystems.length; i++ )
		{
			helpSystem = mSubsystems[i].GetHelpSystem( path );
	        
			if ( helpSystem != null )
			{
				return helpSystem;
			}
		}
		
		if ( path.StartsWith( mPath, false ) )
		{
			return this;
		}
	    
		return null;
	};
    
	this.GetSubsystem   = function( id )
	{
		return mSubsystems[id];
	};
	
	this.GetMergedAliasIDs	= function()
	{
		var ids	= mAliasFile.GetIDs();
		
		for ( var i = 0, length = mSubsystems.length; i < length; i++ )
		{
			var subsystem	= mSubsystems[i];
			var subIDs		= subsystem.GetMergedAliasIDs();
			
			for ( var j = 0, length2 = subIDs.length; j < length2; j++ )
			{
				ids[ids.length] = subIDs[j];
			}
		}
		
		return ids;
	};
	
	this.GetMergedAliasNames	= function()
	{
		var names	= mAliasFile.GetNames();
		
		for ( var i = 0, length = mSubsystems.length; i < length; i++ )
		{
			var subsystem	= mSubsystems[i];
			var subNames	= subsystem.GetMergedAliasNames();
			
			for ( var j = 0, length2 = subNames.length; j < length2; j++ )
			{
				names[names.length] = subNames[j];
			}
		}
		
		return names;
	};
    
	this.LookupCSHID	= function( id )
	{
		var idInfo	= mAliasFile.LookupID( id );

		if ( !idInfo.Found )
		{
			var subIDInfo	= null;
			
			for ( var i = 0; i < mSubsystems.length; i++ )
			{
				var subsystem	= mSubsystems[i];
				
				subIDInfo = subsystem.LookupCSHID( id );
				
				if ( subIDInfo.Found )
				{
					idInfo = subIDInfo;
					
					var myPathUrl	= new CMCUrl( this.GetPath() );
					var subPathUrl	= new CMCUrl( subsystem.GetPath() );
					var relUrl		= subPathUrl.ToRelative( myPathUrl );
					
					idInfo.Topic = relUrl.FullPath + idInfo.Topic;
					
					break;
				}
			}
		}
		
		return idInfo;
	};
    
	this.GetIndex       = function( onCompleteFunc, onCompleteArgs )
	{
		if ( !this.IsWebHelpPlus )
		{
			var xmlDoc		= LoadFirstIndex();
			var preMerged	= FMCGetAttributeBool( xmlDoc.documentElement, "PreMerged", false );
	        
			if ( !preMerged && mSubsystems.length != 0 )
			{
				xmlDoc = LoadEntireIndex();
	            
				for ( var i = 0; i < mSubsystems.length; i++ )
				{
					var subsystem	= mSubsystems[i];
	                
					if ( !subsystem.GetExists() )
					{
						continue;
					}
	                
					var xmlDoc2	= subsystem.GetMergedIndex();
	                
					MergeIndexEntries( xmlDoc.getElementsByTagName( "IndexEntry" )[0], xmlDoc2.getElementsByTagName( "IndexEntry" )[0] );
				}
			}
	        
			onCompleteFunc( xmlDoc, onCompleteArgs );
		}
		else
		{
			function OnGetIndexComplete( xmlDoc, args )
			{
				onCompleteFunc( xmlDoc, onCompleteArgs );
			}

			var xmlDoc	= CMCXmlParser.CallWebService( MCGlobals.RootFolder + "Service/Service.asmx/GetIndex", true, OnGetIndexComplete, null );
		}
	};
    
	this.GetMergedIndex = function()
	{
		var xmlDoc  = LoadEntireIndex();
        
		for ( var i = 0; i < mSubsystems.length; i++ )
		{
			var subsystem   = mSubsystems[i];
            
			if ( !subsystem.GetExists() )
			{
				continue;
			}
            
			var xmlDoc2 = subsystem.GetMergedIndex();
            
			MergeIndexEntries( xmlDoc.getElementsByTagName( "IndexEntry" )[0], xmlDoc2.getElementsByTagName( "IndexEntry" )[0] );
		}
        
		return xmlDoc;
	};
    
	this.HasBrowseSequences	= function()
	{
		var xmlFile	= mXmlFile.substring( 0, mXmlFile.lastIndexOf( "." ) ) + ".xml";
		var xmlDoc	= CMCXmlParser.GetXmlDoc( xmlFile, false, null, null );
		
		return xmlDoc.documentElement.getAttribute( "BrowseSequence" ) != null;
	};
    
	this.HasToc				= function()
	{
		var xmlFile	= mXmlFile.substring( 0, mXmlFile.lastIndexOf( "." ) ) + ".xml";
		var xmlDoc	= CMCXmlParser.GetXmlDoc( xmlFile, false, null, null );
		
		return xmlDoc.documentElement.getAttribute( "Toc" ) != null;
	};
    
	this.IsMerged       = function()
	{
		return (mSubsystems.length > 0);
	};
    
	this.GetConceptsLinks	= function( conceptTerms, callbackFunc, callbackArgs )
	{
		if ( this.IsWebHelpPlus )
		{
			function OnGetTopicsForConceptsComplete( xmlDoc, args )
			{
				var links		= new Array();
				var nodes		= xmlDoc.documentElement.getElementsByTagName( "Url" );
				var nodeLength	= nodes.length;
				
				for ( var i = 0; i < nodeLength; i++ )
				{
					var node	= nodes[i];
					var title	= node.getAttribute( "Title" );
					var url		= node.getAttribute( "Source" );
	                
					url = mPath + ((url.charAt( 0 ) == "/") ? url.substring( 1, url.length ) : url);
	                
					links[links.length] = title + "|" + url;
				}
				
				callbackFunc( links, callbackArgs );
			}
			
			var xmlDoc	= CMCXmlParser.CallWebService( MCGlobals.RootFolder + "Service/Service.asmx/GetTopicsForConcepts?Concepts=" + conceptTerms, true, OnGetTopicsForConceptsComplete, null );
		}
		else
		{
			var links	= null;

			conceptTerms = conceptTerms.replace( "\\;", "%%%%%" );
			
			if ( conceptTerms == "" )
			{
				links = new Array();
				callbackFunc( links, callbackArgs );
			}
			
			var concepts	= conceptTerms.split( ";" );
			
			links = this.GetConceptsLinksLocal( concepts );
			
			callbackFunc( links, callbackArgs );
		}
	};
		
	this.GetConceptsLinksLocal	= function( concepts )
	{
		var links	= new Array();
		
		for ( var i = 0; i < concepts.length; i++ )
		{
			var concept	= concepts[i];
			
			concept = concept.replace( "%%%%%", ";" );
			concept = concept.toLowerCase();
			
			var currLinks	= this.GetConceptLinksLocal( concept );
	        
			for ( var j = 0; j < currLinks.length; j++ )
			{
				links[links.length] = currLinks[j];
			}
		}
		
		return links;
	};
    
	this.GetConceptLinksLocal	= function( concept )
	{
		LoadConcepts();
		
		var links	= mViewedConceptMap.GetItem( concept );
	        
		if ( !links )
		{
			links = mConceptMap.GetItem( concept );
            
			if ( !links )
			{
				links = new Array( 0 );
			}
			
			for ( var i = 0; i < mSubsystems.length; i++ )
			{
				var subsystem   = mSubsystems[i];
	            
				if ( !subsystem.GetExists() )
				{
					continue;
				}
	            
				MergeConceptLinks( links, subsystem.GetConceptLinksLocal( concept ) );
			}

			mViewedConceptMap.Add( concept, links );
		}
		
		return links;
	};
    
	this.LoadGlossary   = function( onCompleteFunc, onCompleteArgs )
	{
		if ( !this.IsWebHelpPlus )
		{
			if ( !this.IsMerged() )
			{
				return;
			}
	        
			var xmlDoc	= this.GetGlossary();
			
			onCompleteFunc( xmlDoc, onCompleteArgs );
		}
		else
		{
			function OnGetGlossaryComplete( xmlDoc, args )
			{
				onCompleteFunc( xmlDoc, onCompleteArgs );
			}

			var xmlDoc	= CMCXmlParser.CallWebService( MCGlobals.RootFolder + "Service/Service.asmx/GetGlossary", true, OnGetGlossaryComplete, null );
		}
	}
    
	this.GetGlossary    = function()
	{
		var xmlDoc	= CMCXmlParser.GetXmlDoc( this.GlossaryUrl, false, null, null );
        
		for ( var i = 0; i < mSubsystems.length; i++ )
		{
			var subsystem   = mSubsystems[i];
            
			if ( !subsystem.GetExists() )
			{
				continue;
			}
            
			MergeGlossaries( xmlDoc, subsystem );
		}
        
		return xmlDoc;
	};
    
	this.GetSearchDBs   = function()
	{
		var searchDBs	= new Array();
		var rootFrame	= FMCGetRootFrame();
		var xmlDoc      = CMCXmlParser.GetXmlDoc( mPath + "Data/Search.xml", false, null, null );
		var preMerged	= FMCGetAttributeBool( xmlDoc.documentElement, "PreMerged", false );

		searchDBs[searchDBs.length] = new rootFrame.frames["navigation"].frames["search"].CMCSearchDB( "Data/Search.xml", this );
        
		if ( !preMerged )
		{
			for ( var i = 0; i < mSubsystems.length; i++ )
			{
				var subsystem   = mSubsystems[i];
	            
				if ( !subsystem.GetExists() )
				{
					continue;
				}
	            
				var searchDBs2  = subsystem.GetSearchDBs();
	            
				for ( var j = 0; j < searchDBs2.length; j++ )
				{
					searchDBs[searchDBs.length] = searchDBs2[j];
				}
			}
		}
        
		return searchDBs;
	};
    
	// Private member functions
    
	function GetGlossaryUrl( xmlDoc )
	{
		var glossaryUrlRel	= xmlDoc.documentElement.getAttribute( "Glossary" );
		var pos				= glossaryUrlRel.lastIndexOf( "." );
		
		glossaryUrlRel = glossaryUrlRel.substring( 0, pos + 1 ) + "xml";
		
		return mPath + glossaryUrlRel;
	}
    
	function LoadFirstIndex()
	{
		var xmlDoc	= CMCXmlParser.GetXmlDoc( mPath + "Data/Index.xml", false, null, null );
        
		return xmlDoc;
	}
    
	function LoadEntireIndex()
	{
		var xmlDoc      = LoadFirstIndex();
		var head        = xmlDoc.documentElement;
		var chunkNodes  = xmlDoc.getElementsByTagName( "Chunk" );
        
		if ( chunkNodes.length > 0 )
		{
			// Remove all attributes except "Count"
            
			var attributesClone = head.cloneNode( false ).attributes;
            
			for ( var i = 0; i < attributesClone.length; i++ )
			{
				if ( attributesClone[i].nodeName != "Count" && attributesClone[i].nodeName != "count" )
				{
					head.removeAttribute( attributesClone[i].nodeName );
				}
			}
            
			// Merge all chunks
            
			for ( var i = 0; i < chunkNodes.length; i++ )
			{
				var xmlDoc2 = CMCXmlParser.GetXmlDoc( mPath + "Data/" + FMCGetAttribute( chunkNodes[i], "Link" ), false, null, null );
                
				MergeIndexEntries( xmlDoc.getElementsByTagName( "IndexEntry" )[0], xmlDoc2.getElementsByTagName( "IndexEntry" )[0] );
			}
            
			head.removeChild( chunkNodes[0].parentNode );
		}
        
		// Make links absolute
        
		for ( var i = 0; i < xmlDoc.documentElement.childNodes.length; i++ )
		{
			if ( xmlDoc.documentElement.childNodes[i].nodeName == "IndexEntry" )
			{
				ConvertIndexLinksToAbsolute( xmlDoc.documentElement.childNodes[i] );
                
				break;
			}
		}
        
		//
        
		return xmlDoc;
	}
    
	function MergeIndexEntries( indexEntry1, indexEntry2 )
	{
		var xmlDoc1     = indexEntry1.ownerDocument;
		var entries1    = indexEntry1.getElementsByTagName( "Entries" )[0];
		var entries2    = indexEntry2.getElementsByTagName( "Entries" )[0];
		var entries     = xmlDoc1.createElement( "IndexEntry" ).appendChild( xmlDoc1.createElement( "Entries" ) );
        
		if ( entries1.getElementsByTagName( "IndexEntry" ).length == 0 )
		{
			if ( xmlDoc1.importNode )
			{
				entries = xmlDoc1.importNode( entries2, true );
			}
			else
			{
				entries = entries2.cloneNode( true );
			}
		}
		else if ( entries2.getElementsByTagName( "IndexEntry" ).length == 0 )
		{
			entries = entries1.cloneNode( true );
		}
		else
		{
			for ( var i = 0, j = 0; i < entries1.childNodes.length && j < entries2.childNodes.length; )
			{
				var currIndexEntry1 = entries1.childNodes[i];
				var currIndexEntry2 = entries2.childNodes[j];
                
				if ( currIndexEntry1.nodeType != 1 )
				{
					i++;
					continue;
				}
				else if ( currIndexEntry2.nodeType != 1 )
				{
					j++;
					continue;
				}
				
				var term1	= FMCGetAttribute( currIndexEntry1, "Term" ).toLowerCase();
				var term2	= FMCGetAttribute( currIndexEntry2, "Term" ).toLowerCase();
                
				if ( term1 == term2 )
				{
					MergeIndexEntries( currIndexEntry1, currIndexEntry2 );
                    
					var links1      = FMCGetChildNodesByTagName( currIndexEntry1, "Links" )[0];
					var indexLinks2 = FMCGetChildNodesByTagName( currIndexEntry2, "Links" )[0].getElementsByTagName( "IndexLink" );
                    
					for ( var k = 0; k < indexLinks2.length; k++ )
					{
						if ( xmlDoc1.importNode )
						{
							links1.appendChild( xmlDoc1.importNode( indexLinks2[k], true ) );
						}
						else
						{
							links1.appendChild( indexLinks2[k].cloneNode( true ) );
						}
					}
                    
					entries.appendChild( currIndexEntry1.cloneNode( true ) );
					i++;
					j++;
				}
				else if ( term1 > term2 )
				{
					if ( xmlDoc1.importNode )
					{
						entries.appendChild( xmlDoc1.importNode( currIndexEntry2, true ) );
					}
					else
					{
						entries.appendChild( currIndexEntry2.cloneNode( true ) );
					}
                    
					j++;
				}
				else
				{
					entries.appendChild( currIndexEntry1.cloneNode( true ) );
					i++;
				}
			}
            
			// Append remaining nodes. There should never be a case where BOTH entries1 AND entries2 have remaining nodes.
            
			for ( ; i < entries1.childNodes.length; i++ )
			{
				entries.appendChild( entries1.childNodes[i].cloneNode( true ) );
			}
            
			for ( ; j < entries2.childNodes.length; j++ )
			{
				if ( xmlDoc1.importNode )
				{
					entries.appendChild( xmlDoc1.importNode( entries2.childNodes[j], true ) );
				}
				else
				{
					entries.appendChild( entries2.childNodes[j].cloneNode( true ) );
				}
			}
		}
        
		indexEntry1.replaceChild( entries, entries1 );
	}
    
	function ConvertGlossaryPageEntryToAbsolute( glossaryPageEntry, path )
	{
		var entryNode	= glossaryPageEntry.getElementsByTagName( "a" )[0];
		var href		= FMCGetAttribute( entryNode, "href" );

		entryNode.setAttribute( "href", path + href );
	}
    
	function ConvertIndexLinksToAbsolute( indexEntry )
	{
		for ( var i = 0; i < indexEntry.childNodes.length; i++ )
		{
			var currNode    = indexEntry.childNodes[i];
            
			if ( currNode.nodeName == "Entries" )
			{
				for ( var j = 0; j < currNode.childNodes.length; j++ )
				{
					ConvertIndexLinksToAbsolute( currNode.childNodes[j] );
				}
			}
			else if ( currNode.nodeName == "Links" )
			{
				for ( var j = 0; j < currNode.childNodes.length; j++ )
				{
					if ( currNode.childNodes[j].nodeType == 1 )
					{
						var link    = FMCGetAttribute( currNode.childNodes[j], "Link" );
                        
						link = mPath + ((link.charAt( 0 ) == "/") ? link.substring( 1, link.length ) : link);
						currNode.childNodes[j].setAttribute( "Link", link );
					}
				}
			}
		}
	}
    
	function LoadConcepts()
	{
		if ( mConceptMap )
		{
			return;
		}
        
		mConceptMap = new CMCDictionary();
        
		var xmlDoc	= CMCXmlParser.GetXmlDoc( mPath + "Data/Concepts.xml", false, null, null );
		var xmlHead	= xmlDoc.documentElement;
        
		for ( var i = 0; i < xmlHead.childNodes.length; i++ )
		{
			var entry   = xmlHead.childNodes[i];
            
			if ( entry.nodeType != 1 ) { continue; }
            
			var term    = entry.getAttribute( "Term" ).toLowerCase();
			var links   = new Array();
            
			for ( var j = 0; j < entry.childNodes.length; j++ )
			{
				var link    = entry.childNodes[j];
                
				if ( link.nodeType != 1 ) { continue; }
                
				var title   = link.getAttribute( "Title" );
				var url     = link.getAttribute( "Link" );
                
				url = mPath + ((url.charAt( 0 ) == "/") ? url.substring( 1, url.length ) : url);
                
				links[links.length] = title + "|" + url;
			}
            
			mConceptMap.Add( term, links );
		}
	}
    
	function MergeConceptLinks( links1, links2 )
	{
		if ( !links2 )
		{
			return;
		}
        
		for ( var i = 0; i < links2.length; i++ )
		{
			links1[links1.length] = links2[i];
		}
	}
    
	function MergeGlossaries( xmlDoc1, subsystem )
	{
		var xmlDoc2	= subsystem.GetGlossary();
		var divs1   = xmlDoc1.getElementsByTagName( "div" );
		var divs2   = xmlDoc2.getElementsByTagName( "div" );
		var body1   = null;
		var body2   = null;
		var body    = xmlDoc1.createElement( "div" );
        
		body.setAttribute( "id", "GlossaryBody" );
        
		for ( var i = 0; i < divs1.length; i++ )
		{
			if ( divs1[i].getAttribute( "id" ) == "GlossaryBody" )
			{
				body1 = divs1[i];
				break;
			}
		}
        
		for ( var i = 0; i < divs2.length; i++ )
		{
			if ( divs2[i].getAttribute( "id" ) == "GlossaryBody" )
			{
				body2 = divs2[i];
				break;
			}
		}
        
		//
        
		var glossUrl	= subsystem.GlossaryUrl;
		var pos			= glossUrl.lastIndexOf( "/" );
		var subPath		= glossUrl.substring( 0, pos + 1 );
        
		//
        
		if ( body1.getElementsByTagName( "div" ).length == 0 )
		{
			if ( xmlDoc1.importNode )
			{
				body = xmlDoc1.importNode( body2, true );
			}
			else
			{
				body = body2.cloneNode( true );
			}
            
			for ( var i = 0; i < body.childNodes.length; i++ )
			{
				var currNode	= body.childNodes[i];
				
				if ( currNode.nodeType != 1 || currNode.nodeName != "div" )
				{
					continue;
				}
				
				ConvertGlossaryPageEntryToAbsolute( currNode, subPath );
			}
		}
		else if ( body2.getElementsByTagName( "div" ).length == 0 )
		{
			body = body1.cloneNode( true );
		}
		else
		{
			for ( var i = 0, j = 0; i < body1.childNodes.length && j < body2.childNodes.length; )
			{
				var currGlossaryPageEntry1  = body1.childNodes[i];
				var currGlossaryPageEntry2  = body2.childNodes[j];
                
				if ( currGlossaryPageEntry1.nodeType != 1 )
				{
					i++;
					continue;
				}
				else if ( currGlossaryPageEntry2.nodeType != 1 )
				{
					j++;
					continue;
				}
                
				var term1   = currGlossaryPageEntry1.getElementsByTagName( "div" )[0].getElementsByTagName( "a" )[0].firstChild.nodeValue;
				var term2   = currGlossaryPageEntry2.getElementsByTagName( "div" )[0].getElementsByTagName( "a" )[0].firstChild.nodeValue;
                
				if ( term1.toLowerCase() == term2.toLowerCase() )
				{
					body.appendChild( currGlossaryPageEntry1.cloneNode( true ) );
					i++;
					j++;
				}
				else if ( term1.toLowerCase() > term2.toLowerCase() )
				{
					var newGlossaryPageEntry	= null;
					
					if ( xmlDoc1.importNode )
					{
						newGlossaryPageEntry = xmlDoc1.importNode( currGlossaryPageEntry2, true );
					}
					else
					{
						newGlossaryPageEntry = currGlossaryPageEntry2.cloneNode( true );
					}
                    
					ConvertGlossaryPageEntryToAbsolute( newGlossaryPageEntry, subPath );
					body.appendChild( newGlossaryPageEntry )
                    
					j++;
				}
				else
				{
					body.appendChild( currGlossaryPageEntry1.cloneNode( true ) );
					i++;
				}
			}
            
			// Append remaining nodes. There should never be a case where BOTH entries1 AND entries2 have remaining nodes.
            
			for ( ; i < body1.childNodes.length; i++ )
			{
				body.appendChild( body1.childNodes[i].cloneNode( true ) );
			}
            
			for ( ; j < body2.childNodes.length; j++ )
			{
				var currNode	= body2.childNodes[j];
				
				if ( currNode.nodeType != 1 )
				{
					continue;
				}
				
				var newNode		= null;
				
				if ( xmlDoc1.importNode )
				{
					newNode = xmlDoc1.importNode( body2.childNodes[j], true );
				}
				else
				{
					newNode = body2.childNodes[j].cloneNode( true );
				}
                
				ConvertGlossaryPageEntryToAbsolute( newNode, subPath );
				body.appendChild( newNode );
			}
		}
        
		body1.parentNode.replaceChild( body, body1 );
	}
}

//
//    End class CMCHelpSystem
//


function FMCStringToBool( stringValue )
{
	var boolValue		= false;
	var stringValLower	= stringValue.toLowerCase();

	boolValue = stringValLower == "true" || stringValLower == "1" || stringValLower == "yes";

	return boolValue;
}


function FMCGetAttributeBool( node, attributeName, defaultValue )
{
	var boolValue	= defaultValue;
	var value		= FMCGetAttribute( node, attributeName );
	
	if ( value )
	{
		boolValue = FMCStringToBool( value );
	}
	
	return boolValue;
}


function FMCGetAttribute( node, attribute )
{
    var value   = null;
    
    if ( node.getAttribute( attribute ) != null )
    {
        value = node.getAttribute( attribute );
    }
    else if ( node.getAttribute( attribute.toLowerCase() ) != null )
    {
        value = node.getAttribute( attribute.toLowerCase() );
    }
    else
    {
		var namespaceIndex	= attribute.indexOf( ":" );
		
		if ( namespaceIndex != -1 )
		{
			value = node.getAttribute( attribute.substring( namespaceIndex + 1, attribute.length ) );
		}
    }
    
    if ( typeof( value ) == "string" && value == "" )
    {
		value = null;
    }
    
    return value;
}


function FMCGetComputedStyle( node, style )
{
    var value   = null;
    
    if ( node.currentStyle )
    {
        value = node.currentStyle[style];
    }
    else if ( document.defaultView && document.defaultView.getComputedStyle )
    {
		var computedStyle	= document.defaultView.getComputedStyle( node, null );
		
		if ( computedStyle )
		{
			value = computedStyle[style];
		}
    }
    
    return value;
}


function FMCConvertToPx( doc, str, dimension, defaultValue )
{
    if ( !str || str.charAt( 0 ) == "-" )
    {
        return defaultValue;
    }
    
    if ( str.charAt( str.length - 1 ) == "\%" )
    {
        switch (dimension)
        {
            case "Width":
                return parseInt( str ) * screen.width / 100;
                
                break;
            case "Height":
                return parseInt( str ) * screen.height / 100;
                
                break;
        }
    }
    else
    {
		if ( parseInt( str ).toString() == str )
		{
			str += "px";
		}
    }
    
    try
    {
        var div	= doc.createElement( "div" );
    }
    catch ( err )
    {
        return defaultValue;
    }
    
    doc.body.appendChild( div );
    
    var value	= defaultValue;
    
    try
    {
        div.style.width = str;
        
        if ( div.currentStyle )
		{
			value = div.offsetWidth;
		}
		else if ( document.defaultView && document.defaultView.getComputedStyle )
		{
			value = parseInt( FMCGetComputedStyle( div, "width" ) );
		}
    }
    catch ( err )
    {
    }
    
    doc.body.removeChild( div );
    
    return value;
}


//
//    Class CMCXmlParser
//

function CMCXmlParser( args, LoadFunc )
{
	// Private member variables and functions
	
	var mSelf		= this;
    this.mXmlDoc	= null;
    this.mXmlHttp	= null;
    this.mArgs		= args;
    this.mLoadFunc	= LoadFunc;
    
    this.OnreadystatechangeLocal	= function()
	{
		if ( mSelf.mXmlDoc.readyState == 4 )
		{
			mSelf.mLoadFunc( mSelf.mXmlDoc, mSelf.mArgs );
		}
	};
	
	this.OnreadystatechangeRemote	= function()
	{
		if ( mSelf.mXmlHttp.readyState == 4 )
		{
			mSelf.mLoadFunc( mSelf.mXmlHttp.responseXML, mSelf.mArgs );
		}
	};
}

CMCXmlParser.prototype.LoadLocal	= function( xmlFile, async )
{
	if ( window.ActiveXObject )
    {
        this.mXmlDoc = new ActiveXObject( "Microsoft.XMLDOM" );
        this.mXmlDoc.async = async;
        
        if ( this.mLoadFunc )
        {
			this.mXmlDoc.onreadystatechange = this.OnreadystatechangeLocal;
        }
        
        try
        {
            if ( !this.mXmlDoc.load( xmlFile ) )
            {
                this.mXmlDoc = null;
            }
        }
        catch ( err )
        {
			this.mXmlDoc = null;
        }
    }
    else if ( window.XMLHttpRequest )
    {
        this.LoadRemote( xmlFile, async ); // window.XMLHttpRequest also works on local files
    }

    return this.mXmlDoc;
};

CMCXmlParser.prototype.LoadRemote	= function( xmlFile, async )
{
	if ( window.ActiveXObject )
    {
        this.mXmlHttp = new ActiveXObject( "Msxml2.XMLHTTP" );
    }
    else if ( window.XMLHttpRequest )
    {
        xmlFile = xmlFile.replace( /;/g, "%3B" );   // For Safari
        this.mXmlHttp = new XMLHttpRequest();
    }
    
    if ( this.mLoadFunc )
    {
		this.mXmlHttp.onreadystatechange = this.OnreadystatechangeRemote;
    }
    
    try
    {
		this.mXmlHttp.open( "GET", xmlFile, async );
        this.mXmlHttp.send( null );
        
        if ( !async && (this.mXmlHttp.status == 0 || this.mXmlHttp.status == 200) )
		{
			this.mXmlDoc = this.mXmlHttp.responseXML;
		}
    }
    catch ( err )
    {
		this.mXmlHttp.abort();
    }
    
    return this.mXmlDoc;
};

// Public member functions

CMCXmlParser.prototype.Load	= function( xmlFile, async )
{
	var xmlDoc			= null;
	var protocolType	= document.location.protocol;
	
	if ( protocolType == "file:" || protocolType == "mk:" || protocolType == "app:" )
	{
		xmlDoc = this.LoadLocal( xmlFile, async );
	}
	else if ( protocolType == "http:" || protocolType == "https:" )
	{
		xmlDoc = this.LoadRemote( xmlFile, async );
	}
	
	return xmlDoc;
};

// Static member functions

CMCXmlParser.GetXmlDoc	= function( xmlFile, async, LoadFunc, args )
{
	var xmlParser	= new CMCXmlParser( args, LoadFunc );
    var xmlDoc		= xmlParser.Load( xmlFile, async );
    
    return xmlDoc;
}

CMCXmlParser.LoadXmlString	= function( xmlString )
{
	var xmlDoc	= null;
	
	if ( window.ActiveXObject )
	{
		xmlDoc = new ActiveXObject( "Microsoft.XMLDOM" );
		xmlDoc.async = false;
		xmlDoc.loadXML( xmlString );
	}
	else if ( DOMParser )
	{
		var parser	= new DOMParser();
		
		xmlDoc = parser.parseFromString( xmlString, "text/xml" );
	}
    
    return xmlDoc;
}

CMCXmlParser.CallWebService	= function( webServiceUrl, async, onCompleteFunc, onCompleteArgs )
{
	var xmlParser	= new CMCXmlParser( onCompleteArgs, onCompleteFunc );
	var xmlDoc		= xmlParser.LoadRemote( webServiceUrl, async );
    
    return xmlDoc;
}

//
//    End class CMCXmlParser
//


//
//    Class CMCUrl
//

function CMCUrl( src )
{
	// Private member variables
	
	var mSelf	= this;
	
	// Public properties

	this.FullPath			= null;
	this.Path				= null;
	this.PlainPath			= null;
	this.Name				= null;
	this.Extension			= null;
	this.NameWithExtension	= null;
	this.Fragment			= null;
	this.Query				= null;
	this.IsAbsolute			= false;

	// Constructor

	(function()
	{
		var fragment	= "";
		var query		= "";
		var fragmentPos	= src.indexOf( "#" );
		var queryPos	= src.indexOf( "?" );
		
		if ( fragmentPos != -1 )
		{
			if ( fragmentPos > queryPos )
			{
				fragment = src.substring( fragmentPos );
			}
			else
			{
				fragment = src.substring( fragmentPos, queryPos );
			}
		}
		
		if ( queryPos != -1 )
		{
			if ( queryPos > fragmentPos )
			{
				query = src.substring( queryPos );
			}
			else
			{
				query = src.substring( queryPos, fragmentPos );
			}
		}
		
		var pos			= Math.max( fragmentPos, queryPos );
		var plainPath	= src.substring( 0, pos == -1 ? src.length : pos );
		pos = plainPath.lastIndexOf( "/" );
		var path		= plainPath.substring( 0, pos + 1 );
		var nameWithExt	= plainPath.substring( pos + 1 );
		pos = nameWithExt.lastIndexOf( "." );
		var name		= nameWithExt.substring( 0, pos );
		var ext			= nameWithExt.substring( pos + 1 );
		
		var scheme		= "";
		pos = src.indexOf( ":" );
		
		if ( pos >= 0 )
		{
			scheme = src.substring( 0, pos );
		}
		
		mSelf.FullPath = src;
		mSelf.Path = path;
		mSelf.PlainPath = plainPath;
		mSelf.Name = name;
		mSelf.Extension = ext;
		mSelf.NameWithExtension = nameWithExt;
		mSelf.Scheme = scheme;
		mSelf.IsAbsolute = !String.IsNullOrEmpty( scheme );
		mSelf.Fragment = fragment;
		mSelf.Query = query;
	})();
}

CMCUrl.prototype.AddFile	= function( otherUrl )
{
	if ( otherUrl.IsAbsolute )
	{
		return otherUrl;
	}
	
	return new CMCUrl( this.FullPath + otherUrl.FullPath );
};

CMCUrl.prototype.ToFolder	= function()
{
	var fullPath	= this.FullPath;
	var pos			= fullPath.lastIndexOf( "/" );
	var fragmentPos	= fullPath.indexOf( "#" );
	var queryPos	= fullPath.indexOf( "?" );
	var minPos		= Math.min( fragmentPos, queryPos );
	var newPath		= fullPath.substring( 0, pos + 1 );

	if ( minPos >= 0 )
	{
		newPath = newPath + fullPath.substring( minPos );
	}

	return new CMCUrl( newPath );
};

CMCUrl.prototype.ToRelative	= function( otherUrl )
{
	var path		= otherUrl.FullPath;
	var otherPath	= this.FullPath;
	var pos			= otherPath.indexOf( path );
	var relPath		= null;
	
	if ( pos == 0 )
	{
		relPath = otherPath.substring( path.length );
	}
	else
	{
		relPath = otherPath;
	}
	
	return new CMCUrl( relPath );
};

//
//    End class CMCUrl
//


//
//    Class CMCDictionary
//

function CMCDictionary()
{
    // Public properties
    
    this.mMap		= new Object();
    this.mOverflows	= new Array();
    this.mLength	= 0;
}

CMCDictionary.prototype.GetLength	= function( key )
{
	return this.mLength;
};

CMCDictionary.prototype.ForEach	= function( func )
{
	var map	= this.mMap;
	
	for ( var key in map )
	{
		var value	= map[key];
		
		if ( !func( key, value ) )
		{
			return;
		}
	}
	
	var overflows	= this.mOverflows;
	
	for ( var i = 0, length = overflows.length; i < length; i++ )
	{
		var item	= overflows[i];
		
		if ( !func( item.Key, item.Value ) )
		{
			return;
		}
	}
};

CMCDictionary.prototype.GetItem	= function( key )
{
	var item	= null;
	
	if ( typeof( this.mMap[key] ) == "function" )
	{
		var index	= this.GetItemOverflowIndex( key );
		
		if ( index >= 0 )
		{
			item = this.mOverflows[index].Value;
		}
	}
	else
	{
		item = this.mMap[key];
		
		if ( typeof( item ) == "undefined" )
		{
			item = null;
		}
	}

    return item;
};

CMCDictionary.prototype.GetItemOverflowIndex	= function( key )
{
	var overflows	= this.mOverflows;
	
	for ( var i = 0, length = overflows.length; i < length; i++ )
	{
		if ( overflows[i].Key == key )
		{
			return i;
		}
	}
	
	return -1;
}

CMCDictionary.prototype.Remove	= function( key )
{
	if ( typeof( this.mMap[key] ) == "function" )
	{
		var index	= this.GetItemOverflowIndex( key );
		
		if ( index >= 0 )
		{
			this.mOverflows.splice( index, 1 )
			
			this.mLength--;
		}
	}
	else
	{
		if ( this.mMap[key] != "undefined" )
		{
			delete( this.mMap[key] );
			
			this.mLength--;
		}
	}
};

CMCDictionary.prototype.Add	= function( key, value )
{
	if ( typeof( this.mMap[key] ) == "function" )
	{
		var item	= this.GetItem( key );
		
		if ( item != null )
		{
			this.Remove( key );
		}
		
		this.mOverflows[this.mOverflows.length] = { Key: key, Value: value };
	}
	else
	{
		this.mMap[key] = value;
    }
    
    this.mLength++;
};

CMCDictionary.prototype.AddUnique	= function( key, value )
{
	var savedValue	= this.GetItem( key );
	
	if ( typeof( savedValue ) == "undefined" || !savedValue )
	{
		this.Add( key, value );
	}
};

//
//    End class CMCDictionary
//


//
//    String helpers
//

String.IsNullOrEmpty = function( str )
{
	if ( str == null )
	{
		return true;
	}
	
	if ( str.length == 0 )
	{
		return true;
	}
	
	return false;
}

String.prototype.StartsWith = function( str, caseSensitive )
{
	if ( str == null )
	{
		return false;
	}
	
	if ( this.length < str.length )
	{
		return false;
	}
	
	var value1	= this;
	var value2	= str;
	
	if ( !caseSensitive )
	{
		value1 = value1.toLowerCase();
		value2 = value2.toLowerCase();
	}
	
	if ( value1.substring( 0, value2.length ) == value2 )
	{
		return true;
	}
	else
	{
		return false;
	}
}

String.prototype.EndsWith = function( str, caseSensitive )
{
	if ( str == null )
	{
		return false;
	}
	
	if ( this.length < str.length )
	{
		return false;
	}
	
	var value1	= this;
	var value2	= str;
	
	if ( !caseSensitive )
	{
		value1 = value1.toLowerCase();
		value2 = value2.toLowerCase();
	}
	
	if ( value1.substring( value1.length - value2.length ) == value2 )
	{
		return true;
	}
	else
	{
		return false;
	}
}

String.prototype.Contains = function( str, caseSensitive )
{
	var value1	= this;
	var value2	= str;
	
	if ( !caseSensitive )
	{
		value1 = value1.toLowerCase();
		value2 = value2.toLowerCase();
	}
	
	return value1.indexOf( value2 ) != -1;
}

String.prototype.Equals = function( str, caseSensitive )
{
	var value1	= this;
	var value2	= str;
	
	if ( !caseSensitive )
	{
		value1 = value1.toLowerCase();
		value2 = value2.toLowerCase();
	}
	
	return value1 == value2;
}

String.prototype.CountOf = function( str, caseSensitive )
{
	var count	= 0;
	var value1	= this;
	var value2	= str;
	
	if ( !caseSensitive )
	{
		value1 = value1.toLowerCase();
		value2 = value2.toLowerCase();
	}
	
	var lastIndex	= -1;
	
	while ( true )
	{
		lastIndex = this.indexOf( str, lastIndex + 1 );
		
		if ( lastIndex == -1 )
		{
			break;
		}
		
		count++;
	}
	
	return count;
}

//
//    End String helpers
//

