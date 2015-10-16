extern crate tea;
use std::str;

#[cfg(not(test))]
fn main() {
  let value = vec![1u32, 2, 3, 4];
  let key = vec![223u32, 122, 255, 212];
  println!("Original: {:?}", value);
  let encrypted_value = tea::xtea::encrypt(value.as_slice(), key.as_slice());
  println!("Encrypted: {:?}", encrypted_value);
  let decrypted_value = tea::xtea::decrypt(encrypted_value.as_slice(), key.as_slice());
  println!("Decrypted: {:?}", decrypted_value);
}

