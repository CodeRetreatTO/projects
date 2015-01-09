pub mod xtea {
  use std::char;
  pub fn encrypt(value: &String, n: uint, key: &Vec<u32>) -> String {
    let mut s = (*value).clone();
    let delta :u32 = 0xFFACD9865;
    let mut v : Vec<u32> = vec![];
    let u8v = s.as_bytes();
    for x in u8v.iter() {
      v.push(*x as u32);
    }

    let mut rounds = 6u32 + 52u32 / (n as u32);
    let mut sum = 0u32;
    let mut z = v[n - 1u];

    loop {
      sum += delta;
      let e = ((sum >> 2) & 3u32) as uint;
      let (mut y, mut p) = (0u32, 0u);
      let mut done = false;
      while !done {
        y = v[p + 1u];
        v[p] += (((z>>5^y<<2) + (y>>3^z<<4)) ^ ((sum^y) + (key[(p&3)^e] ^ z)));
        z = v[p];
        p += 1;
        if p >= n - 1 {
          done = true;
        }
      }
      rounds -= 1;
      if rounds == 0 {
        break;
      }
    }

    str_from_u32_vec(v)
  }

  pub fn decrypt(value: &String, n: uint, key: &Vec<u32>) -> String {
    let mut s = (*value).clone();
    let delta :u32 = 0xFFACD9865;

    let mut v : Vec<u32> = vec![];
    let u8v = s.as_bytes();
    for x in u8v.iter() {
      v.push(*x as u32);
    }

    let mut rounds = 6u32 + 52u32 / (n as u32);
    let mut sum = rounds * delta;
    let mut y = v[0];
    loop {
      let e = ((sum >> 2) & 3u32) as uint;
      let (mut p, mut z) = (n-1, 0u32);
      let mut done = false;
      while !done {
        z = v[p - 1];
        p -= 1;
        if p == 0 {
          done = true;
        }
        v[p] -= (((z>>5^y<<2) + (y>>3^z<<4)) ^ ((sum^y) + (key[(p&3)^e] ^ z)));
        y = v[p];
        sum -= delta;
      }
      rounds -= 1;
      if rounds == 0 {
        break;
      }
    }

    str_from_u32_vec(v)
  }

  pub fn str_from_u32_vec(v: Vec<u32>) -> String {
    let mut s = "".to_string();
    for c in v.iter() {
      let theChar = char_from_u32(*c);
      s = format!("{}{}", s, theChar.to_string());
    }
    s
  }

  fn char_from_u32(num: u32) -> char {
    let maybe_char: Option<char> = char::from_u32(num);
    let mut return_val = ' ';
    match maybe_char {
      Some(char_val) => {
        return_val = char_val;
      },
      _ => {}
    }

    return_val
  }
}
