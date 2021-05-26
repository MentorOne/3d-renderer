#include once "fbgfx.bi"

Dim shared as integer SIZE
SIZE = 800 ' Set to 320 if you want to use Screen 13

' Change these using FreeBasic ScreenRes and ImageCreate.
' bscreen = _NewImage(SIZE, SIZE, 32)
' Screen bscreen

' FOV is a calculated value that gets multiplied with X and Y to deliver a field of view
' ZMul and ZAdd both transform the Z axis based on a Near and Far value
Dim shared as double _
  FOV, ZMul, ZAdd
' x y and z are used in this program to simulate the camera moving
Dim shared as double _
  x, y, z
' useXY and Z are static variables used by "applyRotation" to save writing three rotation functions
' Write your Call applyRotation(x, y, z)
Dim shared as double _
  useX, useY, useZ

' _time is incremented by 0.1 every frame
' Resolution is the step of the triangle; ex a resolution of 4 will draw pixels 4x4 wide
Dim shared as double _
  _time, resolution

resolution = 4

' The 20.00 in this statement is the FOV Angle
FOV = 1.0 / Tan(20.0 * 0.5 * 3.14159265359 / 180.0)

dim as double _
  Far = 100, _
  Near = 0.1

ZMul = -Far / (Far - Near)
ZAdd = -Far * Near / (Far - Near)

_time = 0

z = -100

' Two depth related arrays, enveloping the whole (square) screen.
' Depth stores distance from the camera
' DepthTime stores the "_time" that a pixel was last updated. So that I can continue using Depth without having to reset it every _time
Dim shared as double _
  Depth((SIZE / resolution) ^ 2), _
  DepthTime((SIZE / resolution) ^ 2)

' Rotation in X, Y, and Z
Dim RX As Double, RY As Double, RZ As Double

' Returns the max between two numbers
Function max (x as double, y as double) as double
    If x < y Then max = y
    If y <= x Then max = x
End Function

' Returns the min between two numbers
Function min (x as double, y as double) as double
    If y < x Then min = y
    If x <= y Then min = x
End Function

' Returns a screenspace position of a physical position
Function proj ( x as double, z as double) as double
  dim as double _
    za = max(z * ZMul + ZAdd, 0.0001), _
    xa = (x * FOV) / za
  
  proj = xa * SIZE / 10.0 + SIZE / 2.0 ' Translates output to center of the screen, and scales it a bit
End Function

' Applies rotation to useX, Y, and Z
Sub applyRotation (rx as double, ry as double, rz as double)
    ' Rotation
  dim as double _
    outX = (useX * Cos(rz) * Cos(ry)) + (useY * Sin(rz) * Cos(ry)) - (useZ * Sin(ry)), _
    outY = (useX * (Cos(rz) * Sin(ry) * Sin(rx) - Sin(rz) * Cos(rx))) + (useY * (Sin(rz) * Sin(ry) * Sin(rx) + Cos(rz) * Cos(rx))) + (useZ * Cos(ry) * Sin(rx)), _
    outZ = (useX * (Cos(rz) * Sin(ry) * Cos(rx) + Sin(rz) * Sin(rx))) + (useY * (Sin(rz) * Sin(ry) * Cos(rx) - Cos(rz) * Sin(rx))) + (useZ * Cos(ry) * Cos(rx))

  ' Output
  useX = outX
  useY = outY
  useZ = outZ
End Sub

' Used to draw a 2d rectangle from x,y to x+w,y+h
Sub rect (x as double, y as double, w as double, h as double)
    Line (x, y)-(x + w, y)
    Line (x + w, y)-(x + w, y + h)
    Line (x + w, y + h)-(x, y + h)
    Line (x, y + h)-(x, y)
End Sub

' Can be used to draw lines between 2 3d points
Sub Line3D (x1 as double, y1 as double, z1 as double, x2 as double, y2 as double, z2 as double)
    Line (proj(x1, z1), proj(y1, z1))-(proj(x2, z2), proj(y2, z2))
End Sub

' Draw a triangle between 3 3d points. Details inside
Sub Triangle (_
  x1 as double, y1 as double, z1 as double, _
  x2 as double, y2 as double, z2 as double, _
  x3 as double, y3 as double, z3 as double)
    '    Line3D x1, y1, z1, x2, y2, z2
    '    Line3D x2, y2, z2, x3, y3, z3
    '    Line3D x1, y1, z1, x3, y3, z3

    ' Get the screenspace/pixel each point is projected to
  dim as double _  
    px1 = proj(x1, z1), _
    py1 = proj(y1, z1), _
    px2 = proj(x2, z2), _
    py2 = proj(y2, z2), _
    px3 = proj(x3, z3), _
    py3 = proj(y3, z3)

    ' Get the distance between (2d) point 1 and 2, and 1 and 3. Take the max distance in order to not skip over any pixels.
  dim as double distanceT = Sqr(max((px2 - px1) ^ 2 + (py2 - py1) ^ 2, (px3 - px1) ^ 2 + (py3 - py1) ^ 2))

    ' Declare lots of variables; probably why this can't run on DOS
    '' The change in x and y going down line1-2, and line1-3
  dim as double _
    dx2 = 0, _
    dy2 = 0, _
    dx3 = 0, _
    dy3 = 0

    '' The calculated physical position of the current point (on line1-2, and line1-3)
  dim as double _
    phx2 = 0, _
    phy2 = 0, _
    phz2 = 0, _
    phx3 = 0, _
    phy3 = 0, _
    phz3 = 0

    '' Distance down the triangle. As k approaches distanceT, the triangle renderer approaches the end of the triangle. And dx/y 2 and 3 approach the end of their respective lines
    dim as double k = 0.0
    Do
        ' Find the ration between k and the target distance.
      dim as double _
        ra = k / distanceT, _
        nr = 1.0 - ra
    
      ' Declare and define
      dim as double _
        dx1 = 0, _
        dy1 = 0, _
        phx1 = 0, _
        phy1 = 0, _
        phz1 = 0

        ' Calculate distance between the pixels alone line1-2 and line1-3
      dim as double distance23 = Sqr((dx2 - dx3) ^ 2 + (dy2 - dy3) ^ 2)
        ' Calculate physical position of points along line1-2 and line1-3
        phx3 = x1 * nr + x3 * ra
        phy3 = y1 * nr + y3 * ra
        phz3 = z1 * nr + z3 * ra

        phx2 = x1 * nr + x2 * ra
        phy2 = y1 * nr + y2 * ra
        phz2 = z1 * nr + z2 * ra

        ' Declare variable similar to k, except it represents a line along dx/y2 to dx/y3 instead of lines 1-2 and 1-3
        dim as double i = 0.0
        Do
            ' Get physical location of current pixel
          dim as double _
            phx1 = phx3 * (1 - (i / distance23)) + phx2 * (i / distance23), _
            phy1 = phy3 * (1 - (i / distance23)) + phy2 * (i / distance23), _
            phz1 = phz3 * (1 - (i / distance23)) + phz2 * (i / distance23)

            ' Grab distance from camera (0, 0). No need for SQR, inefficient as it's not necessary to just compare distance.
          dim as double D = (phx1 ^ 2 + phy1 ^ 2 + phz1 ^ 2)

            ' Set color
            Color RGB((k / distanceT) * 255.0, (i / distance23) * 255.0, D * 0.1) ' Comment this out to set color differently!!
            ' Set depth
            '' Grab an index for Depth; i.e. converting x and y to an array index.
            dim as integer index = CInt((py1 + dy3 + dy1) / resolution) * (SIZE / resolution) + CInt((px1 + dx3 + dx1) / resolution)
            index = min(max(index, 0), (SIZE / resolution) ^ 2) ' Make sure it doesn't go out of range
            If Depth(index) > D Or DepthTime(index) < _time Then ' If it's closer than any preexisting depth, OR if it's a different frame than the last depth, continue
                DepthTime(index) = _time ' Update the frame stamp
                Depth(index) = D ' Update the depth
              dim as integer _
                xpix = CInt((px1 + dx3 + dx1) / resolution) * resolution, _ ' Calculate x pixel position
                ypix = CInt((py1 + dy3 + dy1) / resolution) * resolution ' Calculate y pixel position
                ' Loop + Loop in order to fill a resolution*resolution set of pixels
                For xoff as integer = 0 To resolution
                    For yoff as integer = 0 To resolution
                        PSet (xpix + xoff, ypix + yoff) ' Set the pixel!
                    Next
                Next
            End If
            ' Update dx1 and dy1 for the next frame. Essentially being the position down a line from dx/y2 to dx/y3
            dx1 = dx1 + resolution * (dx2 - dx3) / distance23
            dy1 = dy1 + resolution * (dy2 - dy3) / distance23

            ' Increment i according to resolution
            i = i + resolution
        Loop While i <= distance23 ' Loop until it reaches the end of it's segment
        ' A simple line function that was replaced by the previous loop in order to do per-pixel coloring, and custom resolutions
        ' Line (px1 + dx3, py1 + dy3)-(px1 + dx2, py1 + dy2)

        ' Calculate position along line1-2 and line1-3
        dx2 = dx2 + resolution * (px2 - px1) / distanceT
        dy2 = dy2 + resolution * (py2 - py1) / distanceT
        dx3 = dx3 + resolution * (px3 - px1) / distanceT
        dy3 = dy3 + resolution * (py3 - py1) / distanceT

        ' Increment k according to resolution
        k = k + resolution
    Loop While k <= distanceT
End Sub

' A clunky cube sub. Takes in position, size, and rotation.
Sub cube ( _
  x as double, y as double, z as double, _
  w as double, h as double, d as double, _
  rx as double, ry as double, rz as double)

    ' Individual number calculation that makes me miss OOP vector x matrix math

    ' Calculate all points with rotation
    '' Left front top
    dim as double _
      centerX = x + w / 2, _
      centerY = y + h / 2, _
      centerZ = z + d / 2, _
      useX = x - centerX, _
      useZ = z - centerZ, _
      useY = y - centerY
    
    applyRotation rx, ry, rz
    
    dim as double _
      xlft = useX + centerX, _
      zlft = useZ + centerZ, _
      ylft = useY + centerY

    '' Right front top
    useX = x + w - centerX
    useZ = z - centerZ
    useY = y - centerY
    
    applyRotation rx, ry, rz
    
    dim as double _
      xrft = useX + centerX, _
      zrft = useZ + centerZ, _
      yrft = useY + centerY

    '' Left back top
    useX = x - centerX
    useZ = z + d - centerZ
    useY = y - centerY
    
    applyRotation rx, ry, rz
    
    dim as double _
      xlbt = useX + centerX, _
      zlbt = useZ + centerZ, _
      ylbt = useY + centerY

    '' Right back top
    useX = x + w - centerX
    useZ = z + d - centerZ
    useY = y - centerY
    
    applyRotation rx, ry, rz
    
    dim as double _
      xrbt = useX + centerX, _
      zrbt = useZ + centerZ, _
      yrbt = useY + centerY

    '' Left front bottom
    useX = x - centerX
    useZ = z - centerZ
    useY = y + h - centerY
    
    applyRotation rx, ry, rz
    
    dim as double _
      xlfb = useX + centerX, _
      zlfb = useZ + centerZ, _
      ylfb = useY + centerY

    '' Right front bottom
    useX = x + w - centerX
    useZ = z - centerZ
    useY = y + h - centerY
    
    applyRotation rx, ry, rz
    
    dim as double _
      xrfb = useX + centerX, _
      zrfb = useZ + centerZ, _
      yrfb = useY + centerY

    '' Left back bottom
    useX = x - centerX
    useZ = z + d - centerZ
    useY = y + h - centerY
    
    applyRotation rx, ry, rz
    
    dim as double _
      xlbb = useX + centerX, _
      zlbb = useZ + centerZ, _
      ylbb = useY + centerY

    '' Right back bottom
    useX = x + w - centerX
    useZ = z + d - centerZ
    useY = y + h - centerY
    
    applyRotation rx, ry, rz
    
    dim as double _
      xrbb = useX + centerX, _
      zrbb = useZ + centerZ, _
      yrbb = useY + centerY

    '' Line functions that were used to draw an outline
    ' Front face
    'Color _RGB(255, 255, 255)
    'Line3D xlft, ylft, zlft, xrft, yrft, zrft
    'Line3D xrft, yrft, zrft, xrfb, yrfb, zrfb
    'Line3D xrfb, yrfb, zrfb, xlfb, ylfb, zlfb
    'Line3D xlfb, ylfb, zlfb, xlft, ylft, zlft
    ' Back face
    'Line3D xlbt, ylbt, zlbt, xrbt, yrbt, zrbt
    'Line3D xrbt, yrbt, zrbt, xrbb, yrbb, zrbb
    'Line3D xrbb, yrbb, zrbb, xlbb, ylbb, zlbb
    'Line3D xlbb, ylbb, zlbb, xlbt, ylbt, zlbt
    ' Connecting front and back
    'Line3D xlft, ylft, zlft, xlbt, ylbt, zlbt
    'Line3D xrft, yrft, zrft, xrbt, yrbt, zrbt
    'Line3D xlfb, ylfb, zlfb, xlbb, ylbb, zlbb
    'Line3D xrfb, yrfb, zrfb, xrbb, yrbb, zrbb
    'Color _RGB(172, 78, 127)

    'Color 1 ' If you wanna do per-face coloring instead of per pixel or per cube, uncomment these colors and comment out the colors in Triangle function
    Triangle(xlft, ylft, zlft, xrft, yrft, zrft, xrfb, yrfb, zrfb)
    'Color 2
    Triangle(xlfb, ylfb, zlfb, xrfb, yrfb, zrfb, xlft, ylft, zlft)
    'Color 3
    Triangle(xrbt, yrbt, zrbt, xlbt, ylbt, zlbt, xlbb, ylbb, zlbb)
    'Color 4
    Triangle(xrbb, yrbb, zrbb, xlbb, ylbb, zlbb, xrbt, yrbt, zrbt)
    'Color 5
    Triangle(xlbt, ylbt, zlbt, xlft, ylft, zlft, xlfb, ylfb, zlfb)
    'Color 6
    Triangle(xlbb, ylbb, zlbb, xlfb, ylfb, zlfb, xlbt, ylbt, zlbt)
    'Color 7
    Triangle(xrft, yrft, zrft, xrbt, yrbt, zrbt, xrbb, yrbb, zrbb)
    'Color 8
    Triangle(xrfb, yrfb, zrfb, xrbb, yrbb, zrbb, xrft, yrft, zrft)
    'Color 9
    Triangle(xrft, yrft, zrft, xlbt, ylbt, zlbt, xrbt, yrbt, zrbt)
    'Color 10
    Triangle(xrft, yrft, zrft, xlbt, ylbt, zlbt, xlft, ylft, zlft)
    'Color 11
    Triangle(xlfb, ylfb, zlfb, xrbb, yrbb, zrbb, xlbb, ylbb, zlbb)
    'Color 12
    Triangle(xlfb, ylfb, zlfb, xrbb, yrbb, zrbb, xrfb, yrfb, zrfb)
End Sub

screenRes( 800, 600, 32, 2 )
screenSet( 0, 1 )

Do
    Cls
    ' Avoided using DepthTime
    'For i = 0 To (SIZE/resolution) ^ 2
    'Depth(i) = 0
    'Next
    Print _time ' Optional, was used to show how fast each frame was happening

    ' Color _RGB(255, 120, 0) 'Comment out colors in triangles in order to have single solid-color cubes
    cube(x - 10, y - 10, z + 50, 20, 20, 20, RX, RY, RZ)
    cube(x - 10, y - 10, z + 50, 20, 20, 20, 0, 0, 0)

    ' Controls
    If multiKey(Fb.SC_UP) Then z = z + 1
    If multiKey(Fb.SC_DOWN) Then z = z - 1
    If multiKey(Fb.SC_LEFT) Then x = x - 1
    If multiKey(Fb.SC_RIGHT) Then x = x + 1
    If multiKey(Fb.SC_Q) Then RY = RY - 0.01
    If multiKey(Fb.SC_W) Then RY = RY + 0.01
    If multiKey(Fb.SC_E) Then RX = RX - 0.01
    If multiKey(Fb.SC_R) Then RX = RX + 0.01
    If multiKey(Fb.SC_A) Then y = y - 1
    If multiKey(Fb.SC_Z) Then y = y + 1

    ' _time increment, display, and frame limit. Limit not necessary with low resolution (resolution VARIABLE, not screen resolution) as it's slow enough
    _time = _time + 0.1
    '_Display
    '_Limit 100
    flip()
    sleep( 1, 1 )
Loop until multiKey( Fb.SC_ESCAPE )
