/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
//package javaapplication3;
import java.lang.String;
/**
 *
 * @author Lia
 */
public class Tea {

    /**
     * @param args the command line arguments
     * 
     */
    public static void main(String[] args) {
        long[] key = new long[4];
        key[0] = 1;
        key[1] = 2;
        key[2] = 3;
        key[3] = 4;
        
        //System.out.println("Truncate "+                                                              
        
        long[] v = new long[2];
        v[0] = Long.parseLong(args[1], 10);
        v[1] = Long.parseLong(args[2], 10);
        System.out.println(args[0]);
        if (args[0].equals("encode")) {
             v = encrypt(key, v);
        } 
        
        if (args[0].equals("decode"))  {
            v = decrypt(key,v);
        }
        
        System.out.println("F" + v[0] + "-" + v[1]);
        
        
    }
    
    public static long truncate (long v){
        long mask = 0;
        for(int i = 0; i < 32; i++)
        {
            mask = mask & 1<<i;
        }
        return v & mask;
    }
    
    public static long[] encrypt(long[] k, long[] v) {
        long delta = 0x9e3779b9;
        long sum = 0;
        for (int i=0; i < 32; i++ ) {
            sum += delta;
            v[0] += (truncate((v[1]<<4)) + k[0]) ^ (v[1] + sum) ^ (truncate((v[1]>>5)) + k[1]);
            v[1] += (truncate((v[0]<<4)) + k[2]) ^ (v[0] + sum) ^ (truncate((v[0]>>5)) + k[3]);
        }
        
        System.out.println("E" + v[0] + "-" + v[1]);
        return v;
    }
    
    public static long[] decrypt(long[] k, long[] v) {
        long delta = 0x9e3779b9;
        long sum = 0xC6EF3720; //32*delta
        for (int i=0; i < 32; i++ ) {
            v[0] -= (truncate((v[1]<<4)) + k[0]) ^ (v[1] + sum) ^ (truncate((v[1]>>5)) + k[1]);
            v[1] -= (truncate((v[0]<<4)) + k[2]) ^ (v[0] + sum) ^ (truncate((v[0]>>5)) + k[3]);
            sum -= delta;
        }
        
        System.out.println("D" + v[0] + "-" + v[1]);
        return v;
    }
    
}
