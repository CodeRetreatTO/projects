extern crate tea;

#[test]
fn decrypt_equals_pre_encrypt() {
  let mut value = "somestring".to_string();
  let key: Vec<u32> = vec![223, 122, 255, 212];
  let encrypted_value = tea::xtea::encrypt(&value, value.len(), &key);
  let decrypted_value = tea::xtea::decrypt(&encrypted_value, value.len(), &key);
  assert_eq!(decrypted_value, value);
}