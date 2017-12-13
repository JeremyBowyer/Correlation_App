for(i = 2; i < $('#summaryTable th').length; i++) {
colorTableByCol('summaryTable', i);
}

for(i = 7; i < $('#dateCorrelations th').length; i++) {
colorTableByCol('dateCorrelations', i);
}

for(i = 0; i < $('#datePerformance th').length; i++) {
colorTableByCol('datePerformance', i);
}

function colorTableByCol(tableid, colindex){

var columnarray, maxval, minval, max, min, n;

columnarray = [];
$('#' + tableid + ' tr:not(:first)').each(function(){
var val = parseFloat($(this).find('td').eq(colindex).text());
if(val === val) {
columnarray.push(val);
}
})

maxval = Math.max(...columnarray);
minval = Math.min(...columnarray);

min = 0;
if (minval > 0) {
max = maxval;
} else if (maxval < 0) {
max = Math.abs(minval);
} else {
max = Math.max(Math.abs(maxval), Math.abs(minval));
}

n = max-min;

$('#' + tableid + ' tr td:nth-child(' + (colindex + 1) + ')').each(function() {

var val = parseFloat($(this).text());    
var xr, xg, xb, yr, yg, yb;

// Define the min color, which is white
xr = 255; // Red value
xg = 255; // Green value
xb = 255; // Blue value

// Define max color, depending on sign of val
if (val >= 0) {

// Green if val > 0, #2ca25f
yr = 44; // Red value
yg = 162; // Green value
yb = 95; // Blue value

} else {

// Red if val < 0, #a12b2b
yr = 161; // Red value
yg = 43; // Green value
yb = 43; // Blue value

val = Math.abs(val);

}

// Find value's position relative to range
var pos = ((val - min) / (n));

// Generate RGB code
red = parseInt((xr + (( pos * (yr - xr)))).toFixed(0));
green = parseInt((xg + (( pos * (yg - xg)))).toFixed(0));
blue = parseInt((xb + (( pos * (yb - xb)))).toFixed(0));

clr = 'rgb('+red+','+green+','+blue+')';

// Apply to cell

$(this).css('background-color', clr);

})
}