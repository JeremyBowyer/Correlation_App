function loadingScreen(load,runanalysis) {
    if(load && !runanalysis) {
        $('.container-fluid').addClass('blur');
        $('.loading-div').show();
    } else if(!runanalysis) {
        $('.container-fluid').removeClass('blur');
        $('.loading-div').hide();
    } else{
        $('.container-fluid').addClass('blur');
    }
    
}

$(document).on({

    'shiny:connected': function(event) {
        var body, loading, center, dot1, dot2, dot3;

        body     = document.getElementsByTagName("BODY")[0];
        loading  = document.createElement("div");
        center   = document.createElement("div");
        dot1     = document.createElement("div");
        dot2     = document.createElement("div");
        dot3     = document.createElement("div");

        loading.className  = "loading-div";
        center.className   = "center";
        dot1.className     = "dot-1";
        dot2.className     = "dot-2";
        dot3.className     = "dot-3";

        center.append(dot1);
        center.append(dot2);
        center.append(dot3);

        loading.append(center);

        body.prepend(loading);
    },

    'shiny:busy': function(event) {
          myTimeout = setTimeout(function(){ if(!document.getElementById("shiny-notification-panel")){loadingScreen(true,runanalysis=false) }
            else{loadingScreen(true,true)}}, 500)
          
    },
    
    'shiny:idle': function(event) {
        clearTimeout(myTimeout);
        loadingScreen(false,false);
    }
});