pub mod xtea {

  const DELTA: u32 = 0x9e3779b9;
  pub fn encrypt(value: &[u32], key: &[u32]) -> Vec<u32> {
    let n = value.len();
    if n == 0 {
     return Vec::new();
    }
    // copy value into our working buffers
    let mut v: Vec<u32> = value.to_vec();
    let mut rounds: u32 = 6 + 52 / (n as u32);
    let mut sum: u32;
    let mut y: u32;
    let mut z: u32;

    sum = 0;
    z = v[n - 1];

    while rounds != 0 {
      sum += DELTA;
      let e = ((sum >> 2) & 3) as usize;
      let mut p = 0u;
      while p < n - 1 {
        y = v[p + 1];
        v[p] += ((z>>5^y<<2) + (y>>3^z<<4)) ^ ((sum^y) + (key[(p&3)^e] ^ z));
        z = v[p];
        p += 1;
      }

      y = v[0];
      v[n - 1] += ((z>>5^y<<2) + (y>>3^z<<4)) ^ ((sum^y) + (key[(p&3)^e] ^ z));
      z = v[n - 1];

      rounds -= 1;
    }

    v
  }

  pub fn decrypt(value: &[u32], key: &[u32]) -> Vec<u32> {
    let n = value.len();
    if n == 0 {
      return Vec::new();
    }
    // copy value into our working buffers
    let mut v: Vec<u32> = value.to_vec();
    let mut rounds: u32 = 6 + 52 / (n as u32);
    let mut sum: u32;
    let mut y: u32;
    let mut z: u32;

    sum = rounds * DELTA;
    y = v[0];

    while rounds != 0 {
      let e = ((sum >> 2) & 3) as usize;
      let mut p = n - 1;
      while p > 0 {
        z = v[p - 1];
        v[p] -= ((z>>5^y<<2) + (y>>3^z<<4)) ^ ((sum^y) + (key[(p&3)^e] ^ z));
        y = v[p];
        p -= 1;
      }

      z = v[n - 1];
      v[0] -= ((z>>5^y<<2) + (y>>3^z<<4)) ^ ((sum^y) + (key[(p&3)^e] ^ z));
      y = v[0];
      sum -= DELTA;

      rounds -= 1;
    }

    v
  }
}
