
// resize plotly dimension
var dimension = [0, 0];

// initiate all links 
var alllinks = ["home", "about", "contact", "sign_in"];

// initiate URL link 
shinyjs.init = function() {
  window.onload = function (event) {
    
    var link = document.location.search.substring(1);
    var site;
    
    if(String(link) === ""){ site = "home"; }else{ site = String(link); }
    
    for (var i=0, len=alllinks.length|0; i<len; i=i+1|0) {
      
      var content = alllinks[i];
      
      if(content !== site) {
        document.getElementById(content).style.color="#40424a";
        document.getElementById(content).style.textDecoration="none";      
      }else{
        document.getElementById(content).style.color="#3182bd";
        document.getElementById(content).style.textDecoration="underline";
      }
      
    }
    
    dimension[0] = window.innerWidth;
    dimension[1] = window.innerHeight;
    Shiny.onInputChange("dimension", dimension);

    Shiny.onInputChange("hm_de_generate", 0);
    Shiny.onInputChange("hm_es_generate", 0);
    Shiny.onInputChange("hm_conn_generate", 0);
  
  };
};

//Login warning function
function loginfunction(e){
  if (e.which === 13) {
		document.getElementById("SignInButton").click();
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
      document.getElementById(content).style.textDecoration="none";      
    }else{
      document.getElementById(content).style.color="#3182bd";
      document.getElementById(content).style.textDecoration="underline";
    }
  }
  
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

Shiny.addCustomMessageHandler("ResizeK2Table", function(id){

    var Id = String(id);
    var table = document.getElementById(Id);
    resizableGrid(table);

});

var counter = 0;

function heatmapFun(id){
  
    var Id = String(id);
    counter = counter + 1;
    Shiny.onInputChange(Id, counter);

}


