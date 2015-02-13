require 'chunky_png'

class NearestNeighbor
  DEFAULT_COLOR = ChunkyPNG::Color.rgb(255, 255, 0)

  def self.resize(image, scale: 1)
    result = ChunkyPNG::Image.new(image.width * scale, image.height * scale, DEFAULT_COLOR)
    0.upto(result.width-1) do |x|
      0.upto(result.height-1) do |y|
        src_color = image.get_pixel(x/scale, y/scale)
        result.set_pixel(x, y, src_color)
      end
    end
    result
  end
end
