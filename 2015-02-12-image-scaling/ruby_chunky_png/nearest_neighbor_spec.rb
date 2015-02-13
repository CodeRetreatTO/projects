require 'rspec'
require 'time'
require './nearest_neighbor'

RSpec.describe 'NearestNeighbor' do
  let(:red) { ChunkyPNG::Color.rgb(255, 0, 0)}
  let(:blue) { ChunkyPNG::Color.rgb(0, 0, 255)}

  def new_image
    #    0   1  <- x
    # 0|red|blue|
    # 1|red|blue|
    image = ChunkyPNG::Image.new(2, 2)
    image.set_pixel(0, 0, red)
    image.set_pixel(0, 1, red)
    image.set_pixel(1, 0, blue)
    image.set_pixel(1, 1, blue)
    image
  end

  def write_to_disk(image, name: 'result.png')
    image.save(name)
  end

  describe 'resize' do
    it 'resizes an image by an integer amount' do
      image = NearestNeighbor.resize(new_image, scale: 2)
      expect(image.get_pixel(1, 1)).to eq(red)
      expect(image.get_pixel(3, 1)).to eq(blue)
      expect(image.get_pixel(1, 3)).to eq(red)
      expect(image.get_pixel(3, 3)).to eq(blue)
    end

    it 'resizes an image by a decimal amount' do
      write_to_disk(new_image, 'before.png')
      image = NearestNeighbor.resize(new_image, scale: 1.5)
      write_to_disk(image, 'after.png')
    end
  end
end
