function getSelectionStartPos() {	  
    var textArea = document.getElementById("erlSource");
    return textArea.selectionStart; //not working in IE
}

function select(input,x,y) {
    if (input.selectionStart === undefined) {   // Internet Explorer
        var inputRange = input.createTextRange ();
        inputRange.moveStart ("character", x);
        inputRange.collapse ();
        inputRange.moveEnd ("character", y);
        inputRange.select ();
    }
    else {    // Firefox, Opera, Google Chrome and Safari
        input.selectionStart = x-1;
        input.selectionEnd = y;
        input.focus();
    }
}

function showSelection(file,startPos,endPos) {
    loadAndSelect("show_erl.yaws?" + encodeURIComponent(file),startPos,endPos);
} 

function getBody(content) { // searches for body, extracts and returns the content
    test = content.toLowerCase();  // to eliminate case sensitivity
    var x = test.indexOf("<body");
    if(x == -1) return "";
    
    x = test.indexOf(">", x);
    if(x == -1) return "";
    
    var y = test.lastIndexOf("</body>");
    if(y == -1) y = test.lastIndexOf("</html>");
    if(y == -1) y = content.length;   
    // If no HTML then just grab everything till end
    return content.slice(x + 1, y);   
}

function loadAndSelect(url,startPos,endPos) {
    var x = document.getElementById("erlSource");
    var xhr = createXHR();
    xhr.onreadystatechange=function() { 
        if(xhr.readyState == 4) {
            x.style.display="inline";
            x.value = getBody(xhr.responseText);
            setTimeout(function (){
                    select(x,startPos,endPos);
                    resize(x); 
                    },0)   
        } 
    }; 
    xhr.open("GET", url, true);
    xhr.send(null); 
}
  
function resize(ta) {
    var lines = ta.value.split('\n');
    var width = ta.cols;
    var height = 1;
    for (var i = 0; i < lines.length; i++) {
        var linelength = lines[i].length;
        if (linelength >= width) {
            height += Math.ceil(linelength / width);
        }
    }
    height += lines.length;
    ta.rows = height;
}
