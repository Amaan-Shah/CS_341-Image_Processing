//
// F# image processing functions.
//
// Library to perform various image processing functions
//
// Syed Amaan Shah
// UIC Spring 2022
//

namespace ImageLibrary

module Operations =
  //
  // all functions must be indented
  //

  //
  // Grayscale:
  //
  // Converts the image into grayscale and returns the 
  // resulting image as a list of lists. Conversion to 
  // grayscale is done by averaging the RGB values for 
  // a pixel, and then replacing them all by that average.
  // So if the RGB values were 25 75 250, the average 
  // would be 116, and then all three RGB values would 
  // become 116 — i.e. 116 116 116.
  //
  // Returns: updated image.
  //
  let rec average L =
    match L with
    | [] -> []
    | (x, y, z)::rest -> let avg = (x + y + z) / 3
                         let next = (avg, avg, avg)
                         next :: average rest

  let rec getImage I =
    let rec getImageInner I A = 
      match I with
      | [] -> A
      | first::rest -> getImageInner rest (average first :: A)
    getImageInner I [] |> List.rev

  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
    getImage image

  //
  // Threshold
  //
  // Thresholding increases image separation --- dark values 
  // become darker and light values become lighter. Given a 
  // threshold value in the range 0 < threshold < color depth,
  // each RGB value is compared to see if it's > threshold.
  // If so, that RGB value is replaced by the color depth;
  // if not, that RGB value is replaced with 0. 
  //
  // Example: if threshold is 100 and depth is 255, then given 
  // a pixel (80, 120, 160), the new pixel is (0, 255, 255).
  //
  // Returns: updated image.
  //
  let check value T D = 
    if value <= T then
      0
    else
      D

  let rec thresh L T D=
    match L with
    | [] -> []
    | (x, y, z)::rest ->  let r = check x T D
                          let g = check y T D
                          let b = check z T D
                          let next = (r,g,b)
                          next::thresh rest T D

  let rec adjustImage I T D= 
    let rec adjustInner I A T D= 
      match I with
      | [] -> A
      | first::rest -> adjustInner rest (thresh first T D :: A) T D
    adjustInner I [] T D |> List.rev

  let rec Threshold (width:int) (height:int) (depth:int) (image:(int*int*int) list list) (threshold:int) = 
    adjustImage image threshold depth


  //
  // FlipHorizontal:
  //
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //
  let rec getRevImg I = 
    match I with
    | [] -> []
    | first::rest -> (List.rev first)::getRevImg rest

  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) =
    getRevImg image


  //
  // Zoom:
  //
  // Zooms the image by the given zoom factor, which is an 
  // integer 0 < factor < 5. The function uses the nearest 
  // neighbor approach where each pixel P in the original 
  // image is replaced by a factor*factor block of P pixels.
  // For example, if the zoom factor is 4, then each pixel 
  // is replaced by a 4x4 block of 16 identical pixels. 
  // The nearest neighbor algorithm is the simplest zoom 
  // algorithm, but results in jagged images.
  //
  // Returns: updated image.
  //

  let rec zoomList L F N= 
    let newL = L |> List.collect (fun x -> List.replicate F x)
    match F with
    | 1 -> newL :: N
    | 2 -> newL :: newL :: N
    | 3 -> newL :: newL:: newL :: N
    | 4 -> newL :: newL :: newL :: newL :: N
    | _ -> N

  let rec zoomImg I F newImg= 
    match I with
    | [] -> List.rev newImg
    | first::rest -> zoomImg rest F (zoomList first F newImg)

  let rec Zoom (width:int)
               (height:int)
               (depth:int)
               (image:(int*int*int) list list)
               (factor:int) = 
    zoomImg image factor []
    

  //
  // RotateRight90:
  //
  // Rotates the image to the right 90 degrees.
  //
  // Returns: updated image.
  //
  let rec rotate I =
    match I with
    | [] -> []
    | hd::tl -> (List.rev hd) :: rotate tl

  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    rotate (List.transpose image)

