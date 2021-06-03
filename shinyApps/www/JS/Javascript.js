
// resize plotly dimension
var dimension = [0, 0];

// initiate all links 
var alllinks = ["home", "portal", "about", "contact", "sign_in"];

// keep track of url history
var url_history = 0;
var url_history_link = [];

window.onpopstate = function(event) {
  
  if(url_history > 1){
    
    url_history = url_history - 1;
  
    var site = url_history_link[url_history];
    
    //alert(site);
    
    for (var i=0, len=alllinks.length|0; i<len; i=i+1|0) {
        
      var content = alllinks[i];
        
      if(content !== site) {
        document.getElementById(content).style.color="#40424a";
        document.getElementById(content).style.fontWeight="normal";      
      }else{
        document.getElementById(content).style.color="#3182bd";
        document.getElementById(content).style.fontWeight="700";      
      }
        
    }
    
    //alert(url_history);

    url_history_link = url_history_link.slice(0,url_history+1);
    
    //alert(url_history_link);
    
    Shiny.setInputValue("portal_tab", site);
  
    //alert(url_history); alert(url_history_link);
    
  }else if(url_history === 0 || url_history === 1){
    
    var site = "home";
    
    for (var i=0, len=alllinks.length|0; i<len; i=i+1|0) {
        
      var content = alllinks[i];
        
      if(content !== site) {
        document.getElementById(content).style.color="#40424a";
        document.getElementById(content).style.fontWeight="normal";      
      }else{
        document.getElementById(content).style.color="#3182bd";
        document.getElementById(content).style.fontWeight="700";      
      }
        
    }
    
    const state = {'page': 'home'};
    const title = '';
    const url = '?page=home';
    
    history.pushState(state, title, url);

    Shiny.setInputValue("portal_tab", "home");
    
  }

};

// initiate URL link 
shinyjs.init = function() {
  window.onload = function (event) {
    
    var link = window.location.search.substring(1); var site;
    
    if(String(link) === ""){ 
      
      site = "home"; 
      
    }else{ 
      
      site_link = String(link); 
      site_link = site_link.split("page=");
      
      for (var i=0, len=site_link.length|0; i<len; i=i+1|0) {
        
        var url_site = site_link[i];
        
        if(url_site !== "" || url_site !== null) {
          site = url_site; 
        }else{
          site = "home"; 
        }
        
      }
    }
    
    for (var i=0, len=alllinks.length|0; i<len; i=i+1|0) {
      
      var content = alllinks[i];
      
      if(content !== site) {
        document.getElementById(content).style.color="#40424a";
        document.getElementById(content).style.fontWeight="normal";      
      }else{
        document.getElementById(content).style.color="#3182bd";
        document.getElementById(content).style.fontWeight="700";      
      }
      
    }
    
    dimension[0] = window.innerWidth;
    dimension[1] = window.innerHeight;
    Shiny.setInputValue("dimension", dimension);

    Shiny.setInputValue("hm_de_generate", 0);
    Shiny.setInputValue("hm_es_generate", 0);
    Shiny.setInputValue("hm_conn_generate", 0);
    
    url_history_link.push(site);

  };
};

//Login warning function
function loginfunction(e){
  if (e.which === 13) {
		document.getElementById("sign_in_btn").click();
	}
}

//Logged function
function curlinkFun(link){
  
  if(String(link) === "null"){ 
    site = "home"; 
  }else{ 
    site = String(link);
  }
  
  for (var i=0, len=alllinks.length|0; i<len; i=i+1|0) {
    var content = alllinks[i];
    
    if(content !== site) {
      document.getElementById(content).style.color="#40424a";
      document.getElementById(content).style.fontWeight="normal";      
    }else{
      document.getElementById(content).style.color="#3182bd";
      document.getElementById(content).style.fontWeight="700";  
    }
  }
  
  url_history = url_history + 1; //alert(url_history);
  url_history_link.push(site); //alert(url_history_link);
    
  Shiny.setInputValue("portal_tab", site);
  
}

// Resizable function
function resizableGrid(table) {
  
 var row = table.getElementsByTagName('tr')[0],
 cols = row ? row.children : undefined;
 if (!cols) return;
 
 table.style.overflow = 'hidden';
 
 var tableHeight = table.offsetHeight;
 
 for (var i=0; i<cols.length; i++){
   
  var div = createDiv(tableHeight);
  cols[i].appendChild(div);
  cols[i].style.position = 'relative';
  setListeners(div);
  
 }

 function setListeners(div){
   
  var pageX,curCol,nxtCol,curColWidth,nxtColWidth;

  div.addEventListener('mousedown', function (e) {
   curCol = e.target.parentElement;
   nxtCol = curCol.nextElementSibling;
   pageX = e.pageX; 
 
   var padding = paddingDiff(curCol);
 
   curColWidth = curCol.offsetWidth - padding;
   if (nxtCol)
    nxtColWidth = nxtCol.offsetWidth - padding;
  });

  div.addEventListener('mouseover', function (e) {
   e.target.style.borderRight = '2px solid #0000ff';
   e.target.style.height = '3000px';
  });

  div.addEventListener('mouseout', function (e) {
   e.target.style.borderRight = '';
  });

  document.addEventListener('mousemove', function (e) {
   if (curCol) {
    var diffX = e.pageX - pageX;
 
    if (nxtCol)
     nxtCol.style.width = (nxtColWidth - (diffX))+'px';

    curCol.style.width = (curColWidth + diffX)+'px';
   }
  });

  document.addEventListener('mouseup', function (e) { 
   curCol = undefined;
   nxtCol = undefined;
   pageX = undefined;
   nxtColWidth = undefined;
   curColWidth = undefined;
  });
  
 }
 
 function createDiv(height){
   
  var div = document.createElement('div');
  div.style.top = 0;
  div.style.right = 0;
  div.style.width = '5px';
  div.style.position = 'absolute';
  div.style.cursor = 'col-resize';
  div.style.userSelect = 'none';
  div.style.height = height + 'px';
  
  return div;
  
 }
 
 function paddingDiff(col){
 
  if (getStyleVal(col,'box-sizing') == 'border-box'){
   return 0;
  }
 
  var padLeft = getStyleVal(col,'padding-left');
  var padRight = getStyleVal(col,'padding-right');
  return (parseInt(padLeft) + parseInt(padRight));

 }

 function getStyleVal(elm,css){
  return (window.getComputedStyle(elm, null).getPropertyValue(css));
 }
 
}

var counter = 0;

function heatmapFun(id){
    var Id = String(id);
    counter = counter + 1;
    Shiny.setInputValue(Id, counter);
}

Shiny.addCustomMessageHandler("ResizeK2Table", function(id){
    var Id = String(id);
    var table = document.getElementById(Id);
    resizableGrid(table);
});

// select the portal tab
Shiny.addCustomMessageHandler("SelectPortalTab", function(id){
    var link = String(id);
    curlinkFun(link);
});

// select the portal tab
Shiny.addCustomMessageHandler("SelectedPortal", function(id){
    var link = String(id);
    Shiny.setInputValue("portal_id", link);
});

// select the portal subtab
Shiny.addCustomMessageHandler("SelectedSubTab", function(id){
    var link = String(id);
    Shiny.setInputValue("main_page", link);
});

// select the chemical tab
Shiny.addCustomMessageHandler("SelectedChemicalTab", function(id){
    var link = String(id);
    Shiny.setInputValue("chemical_tab", link);
});

// select the chemical id 
Shiny.addCustomMessageHandler("SelectedChemicalId", function(id){
    var link = String(id);
    Shiny.setInputValue("chem", link);
});

// activate tooltip for bootstrap
$(function () {
  $('[data-toggle="tooltip"]').tooltip()
})
