      SUBROUTINE   ARITOP                                                        
      COMMON   B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13                        
      COMMON   D1,D2,D3,D4,D5,D6,D7,D8,D9,D10                                    
      COMMON   E1,E2,E3,E4,E5,E6,E7,E8,E9,E10                                    
      COMMON   IFA(10),IFB(50),IFC(100),IFD(10,10),IFE(50,50),                   
     1         IFF(10,10,10)                                                     
      COMMON   IP0,IP1,IP2,IP3,IP5,IP6,IP7,IP8,IP9,IP10,IP11,IP12,IP13,          
     1         IP14,IP15,IP16                                                    
      COMMON   I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I13,I14,I15,I16,           
     1         I17,I18,I19,I20,I21,I22,I23,I24,I25,I26,I27,I28,I29,I30,          
     2         I31,I32,I33,I34,I35,I36,I37,I38,I39,I40                           
      COMMON   L1,L2,L3,L4,L5,L6,L7,L8,L9,L10                                    
      COMMON   R1,R2,R3,R4,R5,R6,R7,R8,R9,R10                                    
      COMMON   S1,S2,S3,S4,S5,S6,S7,S8,S9,S10                                    
      COMMON   CRA,PRA,FL1,FL2,PR                                                
      DOUBLE PRECISION   S1,S2,S3,S4,S5,S6,S7,S8,S9,S10                          
      DOUBLE PRECISION   D1,D2,D3,D4,D5,D6,D7,D8,D9,D10                          
      LOGICAL     B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13                     
  200 IC=0                                                                       
  201 IF(IC-IP8)       202,203,203                                               
  202 R1=E1*E1+E2*E2+E3*E3+E4*E4-E6/E2+E5                                        
      R2=E1-E8-E2/E4-E3/E1-E4*E5+E6*E6+E7                                        
      R3=E1*E2+E3*E4+E5*E6+E7*E8-E9/E1+R1                                        
      R4=E1-E2-E3/E2-E4/E1-E5*E5+E6*E7+E8                                        
      R5=E1*E3+E2*E4+E5*E7+E8*E8-E9/E1+E6                                        
      R6=E1-E3-E4/E2-E2/E1-E5*E8+E6*E7+E9                                        
      R7=E9*E1+E8*E2+E7*E3+E6*E4-E5/E1+R1                                        
      R8=E9-E8-E7/E1-E6/E3-E5*E3+E4*E1+E2                                        
      R9=E8*E1+E9*E2+E7*E4+E3*E6-E5/E2+R1                                        
      R10=E6-E2-E3/E4-E1/E2-E5*E8+E4*E9+E7                                       
      IC=IC+1                                                                    
      GOTO   201                                                                 
  203 IC=0                                                                       
  204 IF(IC-IP9)       205,206,206                                               
  205 S1=D1*D1+D2*D2+D3*D3+D4*D4-D6/D2+D5                                        
      S2=D1-D8-D2/D4-D3/D1-D4*D5+D6*D6+D7                                        
      S3=D1*D2+D3*D4+D5*D6+D7*D8-D9/D1+S1                                        
      S4=D1-D2-D3/D2-D4/D1-D5*D5+D6*D7+D8                                        
      S5=D1*D3+D2*D4+D5*D7+D8*D8-D9/D1+D6                                        
      S6=D1-D3-D4/D2-D2/D1-D5*D8+D6*D7+D9                                        
      S7=D9*D1+D8*D2+D7*D3+D6*D4-D5/D1+S1                                        
      S8=D9-D8-D7/D1-D6/D3-D5*D3+D4*D1+D2                                        
      S9=D8*D1+D9*D2+D7*D4+D3*D6-D5/D2+S1                                        
      S10=D6-D2-D3/D4-D1/D2-D5*D8+D4*D9+D7                                       
      IC=IC+1                                                                    
      GOTO   204                                                                 
  206 IC=0                                                                       
  207 IF(IC-IP10)      208,209,209                                               
  208 J1=E1                                                                      
      J2=E2                                                                      
      J3=E3                                                                      
      J4=E4                                                                      
      J5=E5                                                                      
      J6=E6                                                                      
      J7=E7                                                                      
      T1=J1                                                                      
      T2=J2                                                                      
      T3=J3                                                                      
      T4=J4                                                                      
      T5=J5                                                                      
      T6=J6                                                                      
      T7=J7                                                                      
      J1=E1                                                                      
      J2=E2                                                                      
      J3=E3                                                                      
      J4=E4                                                                      
      J5=E5                                                                      
      J6=E6                                                                      
      J7=E7                                                                      
      T1=J1                      