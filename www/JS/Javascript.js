

//Login warning function
function loginfunction(e){
	if (e.which === 13) {
		document.getElementById("SignInButton").click();
	}
}

//rating warning function
function ratingwarningfunction(){
  
	var round=document.getElementById("Round");  
	var roundoptseleted = round.options[round.selectedIndex].value; 
	
	//alert(round); alert(roundoptseleted);
  
	if(roundoptseleted==1){
	  
	    var lmk=document.getElementById("LMK").checked;
        var t1=document.getElementById("T1").checked;
        var smk=document.getElementById("SMK").checked;
        var t2=document.getElementById("T2").checked;
		var amk=document.getElementById("AMK").checked;
  
		if(lmk===true | t1===true | smk===true | t2===true | amk===true){
		  if(lmk===true){
			Shiny.onInputChange("RatingDecision", "LMK");
		  }else if(t1===true){
			Shiny.onInputChange("RatingDecision", "T1");
		  }else if(smk===true){
			Shiny.onInputChange("RatingDecision", "SMK");
		  }else if(t2===true){
			Shiny.onInputChange("RatingDecision", "T2");
		  }else if(amk===true){
			Shiny.onInputChange("RatingDecision", "AMK");
		  }
		}else{
		  Shiny.onInputChange("RatingDecision", null);
		} 
    
	}else{
	  
		var yes=document.getElementById("BookmarkYes").checked;
		var no=document.getElementById("BookmarkNo").checked;
	  
		if(yes===true | no===true){
			if(yes===true){
				alert("Your bookmark has been saved and updated!");
				Shiny.onInputChange("RatingDecision", 1);
			}else if(no===true){
				Shiny.onInputChange("RatingDecision", 0);
			}
		}else{
			Shiny.onInputChange("RatingDecision", null);
		}
    
	}
  
}


function ThresholdSelectedFunction(){
	
	var round=document.getElementById("Round");  
	var roundoptseleted = round.options[round.selectedIndex].value; 
	
	if(roundoptseleted==1){
		Shiny.onInputChange("Threshold", null);
	}
	
}











