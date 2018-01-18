p1= .78;
p2= .76;
p3= .99;
p4= .85;
p5= .94;
p6= .91;
p7= .99;
matorig = [.08*p1, .02*p1, .16*p1, .24*p1, .07*p1, .41*p1, .02*p1, 1-p1;
    .08*p2, .02*p2, .16*p2, .24*p2, .07*p2, .41*p2, .02*p2, 1-p2;
    .08*p3, .02*p3, .16*p3, .24*p3, .07*p3, .41*p3, .02*p3, 1-p3;
    .08*p4, .02*p4, .16*p4, .24*p4, .07*p4, .41*p4, .02*p4, 1-p4;
    .08*p5, .02*p5, .16*p5, .24*p5, .07*p5, .41*p5, .02*p5, 1-p5;
    .08*p6, .02*p6, .16*p6, .24*p6, .07*p6, .41*p6, .02*p6, 1-p6;
    .08*p7, .02*p7, .16*p7, .24*p7, .07*p7, .41*p7, .02*p7, 1-p7;
    0, 0, 0, 0, 0, 0, 0, 1;];
newmat = eye(8);
mat = matorig;
i=1;
while(not(sum(sum(newmat == mat))==64))
    mat=newmat;
    newmat = mat*matorig;
    i= i+1
    
end


rowNames = {'a','b','c'};
colNames = {'x','y','z'};
sTable = array2table(sample,'RowNames',rowNames,'VariableNames',colNames)