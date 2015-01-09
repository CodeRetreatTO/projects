extern crate tea;

#[cfg(not(test))]
fn main() {
  let value = "somestring".to_string();
  let key: Vec<u32> = vec![223, 122, 255, 212];
  let encrypted_value = tea::xtea::encrypt(&value, value.len(), &key);
  println!("Encrypted: {}", encrypted_value);
  let decrypted_value = tea::xtea::decrypt(&encrypted_value, value.len(), &key);

  println!("Decrypted: {}", decrypted_value);
}

