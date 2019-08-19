for(i = 1; i < $('#summaryTable th').length; i++) {
	if(i == 1){
		colorTableByCol('summaryTable', i, false, true);}
	else{
		colorTableByCol('summaryTable', i, true, false);
	}
}

for(i = 6; i < $('#dateCorrelations th').length; i++) {
colorTableByCol('dateCorrelations', i, true, false);
}

for(i = 0; i < $('#datePerformance th').length; i++) {
colorTableByCol('datePerformance', i, true, false);
}

for(i = 1; i < $('#dateDataPointsDF th').length; i++) {
colorTableByCol('dateDataPointsDF', i, true, false);
}

function colorTableByCol(tableid, colindex, descending, zerobound){

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

	if(zerobound){
		columnarray = columnarray.sort();
		min = columnarray[parseInt(columnarray.length / 2)];
		max = maxval;
	} else {
		min = 0;
		if (minval > 0) {
		max = maxval;
		} else if (maxval < 0) {
		max = Math.abs(minval);
		} else {
		max = Math.max(Math.abs(maxval), Math.abs(minval));
		}
	}
	n = max-min;

	$('#' + tableid + ' tr td:nth-child(' + (colindex + 1) + ')').each(function() {

		var val = parseFloat($(this).text());    
		var xr, xg, xb, yr, yg, yb;
		
		// Define the min color
		xr = 255; // Red value
		xg = 255; // Green value
		xb = 255; // Blue value

		// Define max color, depending on sign of val
		if (val >= min) {
			
			if(descending){
				// Green if val > 0, #2ca25f
				yr = 44; // Red value
				yg = 162; // Green value
				yb = 95; // Blue value
			} else {
				// Red if val < 0, #a12b2b
				yr = 161; // Red value
				yg = 43; // Green value
				yb = 43; // Blue value
			}
		} else {
			if(descending){
				// Red if val < 0, #a12b2b
				yr = 161; // Red value
				yg = 43; // Green value
				yb = 43; // Blue value
			} else {
				// Green if val > 0, #2ca25f
				yr = 44; // Red value
				yg = 162; // Green value
				yb = 95; // Blue value
			}
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