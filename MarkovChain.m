%effectiveness per method
p1= .78;
p2= .76;
p3= .99;
p4= .85;
p5= .94;
p6= .91;
p7= .99;
%building main model P matrix
matorig = [.08*p1, .02*p1, .16*p1, .24*p1, .07*p1, .41*p1, .02*p1, 1-p1;
    .08*p2, .02*p2, .16*p2, .24*p2, .07*p2, .41*p2, .02*p2, 1-p2;
    .08*p3, .02*p3, .16*p3, .24*p3, .07*p3, .41*p3, .02*p3, 1-p3;
    .08*p4, .02*p4, .16*p4, .24*p4, .07*p4, .41*p4, .02*p4, 1-p4;
    .08*p5, .02*p5, .16*p5, .24*p5, .07*p5, .41*p5, .02*p5, 1-p5;
    .08*p6, .02*p6, .16*p6, .24*p6, .07*p6, .41*p6, .02*p6, 1-p6;
    .08*p7, .02*p7, .16*p7, .24*p7, .07*p7, .41*p7, .02*p7, 1-p7;
    0, 0, 0, 0, 0, 0, 0, 1];
newmat = eye(8);
mat = matorig;
i=1;
%determining number of iterations for initial model to reach equalibrium
e = 0.00005 %feel free to change this
while(not(sum(sum(newmat - mat < e))==64))
    mat=newmat;
    newmat = mat*matorig;
    i= i+1;
    
end
i
newmat
%calculating n-step matrices
p2v1 = array2table(matorig^2);
p5v1 = array2table(matorig^5);
p10v1 = array2table(matorig^10);
p30v1 = array2table(matorig^30);
p2v1
p5v1
p10v1
p30v1

%what if 1 ... don't switch methods
matorig2 = [.78, 0, 0, 0, 0, 0, 0, .22; 
    0, .76, 0, 0, 0, 0, 0, .24;
    0, 0, .99, 0, 0, 0, 0, .01;
    0, 0, 0, .85, 0, 0, 0, .15;
    0, 0, 0, 0, .94, 0, 0, .06;
    0, 0, 0, 0, 0, .91, 0, .09;
    0, 0, 0, 0, 0, 0, .99, .01;
    0, 0, 0, 0, 0, 0, 0, 1];
newmat2 = eye(8);
mat2 = matorig2;
i=1;
%i,IUD, pill, FAM
record = [0,0,0,0];
%determining number of iterations for initial model to reach equalibrium
while(not(sum(sum(newmat2 - mat2 < e))==64))
    mat2=newmat2;
    newmat2 = mat2*matorig2;
    i= i+1;
    recording = [i,newmat2(3,8),newmat2(6,8),newmat2(2,8)];
    temp = [record; recording];
    record = temp;
    
end
i
newmat2
%calculating n-step matrices
p2v2 = matorig2^2;
p5v2 = matorig2^5;
p10v2 = matorig2^10;
p30v2 = matorig2^30;
p2v2
p5v2
p10v2
p30v2

%what if 2... cycle pregant people back in with fresh start
matorig3 = [.08*p1, .02*p1, .16*p1, .24*p1, .07*p1, .41*p1, .02*p1, 1-p1
    .08*p2, .02*p2, .16*p2, .24*p2, .07*p2, .41*p2, .02*p2, 1-p2
    .08*p3, .02*p3, .16*p3, .24*p3, .07*p3, .41*p3, .02*p3, 1-p3;
    .08*p4, .02*p4, .16*p4, .24*p4, .07*p4, .41*p4, .02*p4, 1-p4;
    .08*p5, .02*p5, .16*p5, .24*p5, .07*p5, .41*p5, .02*p5, 1-p5;
    .08*p6, .02*p6, .16*p6, .24*p6, .07*p6, .41*p6, .02*p6, 1-p6;
    .08*p7, .02*p7, .16*p7, .24*p7, .07*p7, .41*p7, .02*p7, 1-p7;
    .08, .02, .16, .24, .07, .41, .02, 0;];
newmat3 = eye(8);
mat3 = matorig3;
i=1;
%determining number of iterations for initial model to reach equalibrium
while(not(sum(sum(newmat3 - mat3 < e))==64))
    mat3=newmat3;
    newmat3 = mat3*matorig3;
    i= i+1;
    
end
i
newmat3
%calculating n-step matrices
p2v3 = matorig3^2;
p5v3 = matorig3^5;
p10v3 = matorig3^10;
p30v3 = matorig3^30;
p2v3
p5v3
p10v3
p30v3

