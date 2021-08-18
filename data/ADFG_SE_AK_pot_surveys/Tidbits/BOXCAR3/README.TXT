Onset Computer Corp.                                      May 2001
		  BoxCar for Windows, Vers. 3.7


Installing BoxCar for Windows on a Hard Drive
----------------------------------------------
To install BoxCar software on your hard drive, insert the BoxCar 
diskette into the floppy drive of your computer. For Windows 3.1x,
from the Program Manager, choose Run from the File menu, or for 
Windows 95 click Start then choose Run. Then type a:\setup (or
b:\setup) and select OK. It will prompt you for the directory 
(BOXCAR3 is the default) and install the program.
(It will overwrite BoxCar 3.x if installed to same directory)

For more information about BoxCar for Windows, please refer to the 
User's Manual.

On this disk
------------------------------------

*****LOGUTIL*****
Also on this disk is LogUtil for DOS.  Go to the logutil directory
(cd c:\boxcar3\logutil) and type logutil to run the Utility. 
For more information about this utility read the utilhelp.txt file.
(Please substitute the name of your hard drive for c:\ in the 
commands above as appropriate.) 

If you are interested in other utilities and patches, you may download
them from: 
http://www.onsetcomputer.com/Support/HS_Support/2279_hsfiles.html.

Note about Time
---------------
When you Launch a HOBO with BoxCar, it will reset the logger's 
clock according to the current time inside the host computer.  If 
you launch a logger on one machine and read it out on another 
machine, the original start time set will be used--not the time of
the computer you read it out on.  No compensation is done for 
travel across time zones, daylight savings time, etc.

Notes
-----
* Due to time scaling (BoxCar rounds the times displayed on the 
x-axis to semi-even numbers), when zooming, sometimes slightly more
or less than was selected will be displayed.
* When printing, the scaling option doesn't work.
* Due to a bug in Windows, when printing a zoomed plot, parts of 
the plot may be drawn outside the axes.
* When printing, the plot will always expand to cover a whole page.
* The file info box's data window will scroll 2 points per click 
instead of one for files greater than 32K.

Known Bugs in this version
-------------------------------
* When selecting a custom launch interval, for intervals greater
than one hour, the menu will not show seconds. For example 1:01:01
will show up as 1 Hr 1 Min (76 days 6 Hrs)  The logger will be 
launched with the correct interval.
* When a data or log file is open in a different application, and 
BoxCar tries to save over it, BoxCar may crash.
* The file information box does not scroll with the arrow keys. Use
mouse to control scroll bars and see all data.

