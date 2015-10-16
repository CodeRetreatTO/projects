window.addEventListener('load', function() {
  var el = document.getElementById.bind(document)

  var img = el("megaman")
  var src = el("src")
  var dest = el("dest")
  
  var src_context = src.getContext('2d')
  var dest_context = dest.getContext('2d')
  var image_data = extract_image_data(img)

  resize(image_data, 2.5)

  src.addEventListener('mousemove', function(e) {
    resize(image_data, e.offsetX/10, Math.floor(e.offsetY/25) + 1)
  })

  function extract_image_data(el) {
    src_context.drawImage(el, 0, 0)
    return src_context.getImageData(0, 0, 100, 100)
  }

  function resize(old_image, scale, fudge) {
    var ow = old_image.width
    var oh = old_image.height

    var nw = Math.ceil(ow * scale)
    var nh = Math.ceil(oh * scale)

    var new_image = dest_context.createImageData(nw, nh)

    var old_data = old_image.data
    var new_data = new_image.data

    for(var y = 0; y < nh; y++) {
      for(var x = 0; x < nw; x++) {
        var f = fudge || 4
        var ox = Math.ceil(x / scale)
        var oy = Math.ceil(y / scale)
        var oi = 4 * oy * ow + 4 * ox
        var ni = f *  y * nw + f *  x
        new_data[ni+0] = old_data[oi+0]
        new_data[ni+1] = old_data[oi+1]
        new_data[ni+2] = old_data[oi+2]
        new_data[ni+3] = old_data[oi+3]
      }
    }

    dest_context.putImageData(new_image, 0, 0)
  }
})
