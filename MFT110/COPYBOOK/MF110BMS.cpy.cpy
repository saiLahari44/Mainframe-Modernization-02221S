      * ***************************************************************
      * Created: 10 Jun, 2020 8:38:53 PM Asia/Calcutta
      * Generated by: IBM Developer for z/OS
      * ***************************************************************
       01 MF110BMSI.
          02 FILLER                    PIC X(12).
      *
          02 SelectOptionfL            PIC S9(4) COMP.
          02 SelectOptionfF            PIC X.
          02 FILLER          REDEFINES SelectOptionfF.
             03 SelectOptionfA           PIC X.
          02 FILLER                    PIC X(6).
          02 SelectOptionfI            PIC X.
      *
          02 MESSAGEL                  PIC S9(4) COMP.
          02 MESSAGEF                  PIC X.
          02 FILLER          REDEFINES MESSAGEF.
             03 MESSAGEA               PIC X.
          02 FILLER                    PIC X(6).
          02 MESSAGEI                  PIC X(26).
      *
          02 SSP5L                     PIC S9(4) COMP.
          02 SSP5F                     PIC X.
          02 FILLER          REDEFINES SSP5F.
             03 SSP5A                  PIC X.
          02 FILLER                    PIC X(6).
          02 SSP5I                     PIC X(6).
      *
          02 GENERALL                  PIC S9(4) COMP.
          02 GENERALF                  PIC X.
          02 FILLER          REDEFINES GENERALF.
             03 GENERALA               PIC X.
          02 FILLER                    PIC X(6).
          02 GENERALI                  PIC X(46).
      *
          02 ClaimInqL                 PIC S9(4) COMP.
          02 ClaimInqF                 PIC X.
          02 FILLER          REDEFINES ClaimInqF.
             03 ClaimInqA              PIC X.
          02 FILLER                    PIC X(6).
          02 ClaimInqI                 PIC X(16).
      *
          02 ClaimNoL                  PIC S9(4) COMP.
          02 ClaimNoF                  PIC X.
          02 FILLER          REDEFINES ClaimNoF.
             03 ClaimNoA               PIC X.
          02 FILLER                    PIC X(6).
          02 ClaimNoI                  PIC X(16).
      *
          02 INPUTL                    PIC S9(4) COMP.
          02 INPUTF                    PIC X.
          02 FILLER          REDEFINES INPUTF.
             03 INPUTA                 PIC X.
          02 FILLER                    PIC X(6).
          02 INPUTI                    PIC X(10).
      *
          02 ClaimAddL                 PIC S9(4) COMP.
          02 ClaimAddF                 PIC X.
          02 FILLER          REDEFINES ClaimAddF.
             03 ClaimAddA              PIC X.
          02 FILLER                    PIC X(6).
          02 ClaimAddI                 PIC X(16).
      *
          02 INPUT1L                   PIC S9(4) COMP.
          02 INPUT1F                   PIC X.
          02 FILLER          REDEFINES INPUT1F.
             03 INPUT1A                PIC X.
          02 FILLER                    PIC X(6).
          02 INPUT1I                   PIC X(10).
      *
          02 CustNoL                   PIC S9(4) COMP.
          02 CustNoF                   PIC X.
          02 FILLER          REDEFINES CustNoF.
             03 CustNoA                PIC X.
          02 FILLER                    PIC X(6).
          02 CustNoI                   PIC X(16).
      *
          02 INPUT2L                   PIC S9(4) COMP.
          02 INPUT2F                   PIC X.
          02 FILLER          REDEFINES INPUT2F.
             03 INPUT2A                PIC X.
          02 FILLER                    PIC X(6).
          02 INPUT2I                   PIC X(10).
      *
          02 ClaimDateL                PIC S9(4) COMP.
          02 ClaimDateF                PIC X.
          02 FILLER          REDEFINES ClaimDateF.
             03 ClaimDateA             PIC X.
          02 FILLER                    PIC X(6).
          02 ClaimDateI                PIC X(16).
      *
          02 INPUT3L                   PIC S9(4) COMP.
          02 INPUT3F                   PIC X.
          02 FILLER          REDEFINES INPUT3F.
             03 INPUT3A                PIC X.
          02 FILLER                    PIC X(6).
          02 INPUT3I                   PIC X(10).
      *
          02 DATEL                     PIC S9(4) COMP.
          02 DATEF                     PIC X.
          02 FILLER          REDEFINES DATEF.
             03 DATEA                  PIC X.
          02 FILLER                    PIC X(6).
          02 DATEI                     PIC X(13).
      *
          02 PaidL                     PIC S9(4) COMP.
          02 PaidF                     PIC X.
          02 FILLER          REDEFINES PaidF.
             03 PaidA                  PIC X.
          02 FILLER                    PIC X(6).
          02 PaidI                     PIC X(16).
      *
          02 INPUT4L                   PIC S9(4) COMP.
          02 INPUT4F                   PIC X.
          02 FILLER          REDEFINES INPUT4F.
             03 INPUT4A                PIC X.
          02 FILLER                    PIC X(6).
          02 INPUT4I                   PIC X(10).
      *
          02 ValueL                    PIC S9(4) COMP.
          02 ValueF                    PIC X.
          02 FILLER          REDEFINES ValueF.
             03 ValueA                 PIC X.
          02 FILLER                    PIC X(6).
          02 ValueI                    PIC X(16).
      *
          02 INPUT5L                   PIC S9(4) COMP.
          02 INPUT5F                   PIC X.
          02 FILLER          REDEFINES INPUT5F.
             03 INPUT5A                PIC X.
          02 FILLER                    PIC X(6).
          02 INPUT5I                   PIC X(10).
      *
          02 CauseL                    PIC S9(4) COMP.
          02 CauseF                    PIC X.
          02 FILLER          REDEFINES CauseF.
             03 CauseA                 PIC X.
          02 FILLER                    PIC X(6).
          02 CauseI                    PIC X(16).
      *
          02 INPUT6L                   PIC S9(4) COMP.
          02 INPUT6F                   PIC X.
          02 FILLER          REDEFINES INPUT6F.
             03 INPUT6A                PIC X.
          02 FILLER                    PIC X(6).
          02 INPUT6I                   PIC X(18).
      *
          02 ObservationL              PIC S9(4) COMP.
          02 ObservationF              PIC X.
          02 FILLER          REDEFINES ObservationF.
             03 ObservationA           PIC X.
          02 FILLER                    PIC X(6).
          02 ObservationI              PIC X(16).
      *
          02 INPUT8L                   PIC S9(4) COMP.
          02 INPUT8F                   PIC X.
          02 FILLER          REDEFINES INPUT8F.
             03 INPUT8A                PIC X.
          02 FILLER                    PIC X(6).
          02 INPUT8I                   PIC X(18).
      *
          02 SelectOptL                PIC S9(4) COMP.
          02 SelectOptF                PIC X.
          02 FILLER          REDEFINES SelectOptF.
             03 SelectOptA             PIC X.
          02 FILLER                    PIC X(6).
          02 SelectOptI                PIC X(15).
      * *******************************************
       01 MF110BMSO REDEFINES MF110BMSI.
          02 FILLER                    PIC X(12).
      *
          02 FILLER                    PIC X(3).
          02 SelectOptionfC            PIC X.
          02 SelectOptionfP            PIC X.
          02 SelectOptionfH            PIC X.
          02 SelectOptionfV            PIC X.
          02 SelectOptionfU            PIC X.
          02 SelectOptionfM            PIC X.
          02 SelectOptionfO            PIC X.
      *
          02 FILLER                    PIC X(3).
          02 MESSAGEC                  PIC X.
          02 MESSAGEP                  PIC X.
          02 MESSAGEH                  PIC X.
          02 MESSAGEV                  PIC X.
          02 MESSAGEU                  PIC X.
          02 MESSAGEM                  PIC X.
          02 MESSAGEO                  PIC X(26).
      *
          02 FILLER                    PIC X(3).
          02 SSP5C                     PIC X.
          02 SSP5P                     PIC X.
          02 SSP5H                     PIC X.
          02 SSP5V                     PIC X.
          02 SSP5U                     PIC X.
          02 SSP5M                     PIC X.
          02 SSP5O                     PIC X(6).
      *
          02 FILLER                    PIC X(3).
          02 GENERALC                  PIC X.
          02 GENERALP                  PIC X.
          02 GENERALH                  PIC X.
          02 GENERALV                  PIC X.
          02 GENERALU                  PIC X.
          02 GENERALM                  PIC X.
          02 GENERALO                  PIC X(46).
      *
          02 FILLER                    PIC X(3).
          02 ClaimInqC                 PIC X.
          02 ClaimInqP                 PIC X.
          02 ClaimInqH                 PIC X.
          02 ClaimInqV                 PIC X.
          02 ClaimInqU                 PIC X.
          02 ClaimInqM                 PIC X.
          02 ClaimInqO                 PIC X(16).
      *
          02 FILLER                    PIC X(3).
          02 ClaimNoC                  PIC X.
          02 ClaimNoP                  PIC X.
          02 ClaimNoH                  PIC X.
          02 ClaimNoV                  PIC X.
          02 ClaimNoU                  PIC X.
          02 ClaimNoM                  PIC X.
          02 ClaimNoO                  PIC X(16).
      *
          02 FILLER                    PIC X(3).
          02 INPUTC                    PIC X.
          02 INPUTP                    PIC X.
          02 INPUTH                    PIC X.
          02 INPUTV                    PIC X.
          02 INPUTU                    PIC X.
          02 INPUTM                    PIC X.
          02 INPUTO                    PIC X(10).
      *
          02 FILLER                    PIC X(3).
          02 ClaimAddC                 PIC X.
          02 ClaimAddP                 PIC X.
          02 ClaimAddH                 PIC X.
          02 ClaimAddV                 PIC X.
          02 ClaimAddU                 PIC X.
          02 ClaimAddM                 PIC X.
          02 ClaimAddO                 PIC X(16).
      *
          02 FILLER                    PIC X(3).
          02 INPUT1C                   PIC X.
          02 INPUT1P                   PIC X.
          02 INPUT1H                   PIC X.
          02 INPUT1V                   PIC X.
          02 INPUT1U                   PIC X.
          02 INPUT1M                   PIC X.
          02 INPUT1O                   PIC X(10).
      *
          02 FILLER                    PIC X(3).
          02 CustNoC                   PIC X.
          02 CustNoP                   PIC X.
          02 CustNoH                   PIC X.
          02 CustNoV                   PIC X.
          02 CustNoU                   PIC X.
          02 CustNoM                   PIC X.
          02 CustNoO                   PIC X(16).
      *
          02 FILLER                    PIC X(3).
          02 INPUT2C                   PIC X.
          02 INPUT2P                   PIC X.
          02 INPUT2H                   PIC X.
          02 INPUT2V                   PIC X.
          02 INPUT2U                   PIC X.
          02 INPUT2M                   PIC X.
          02 INPUT2O                   PIC X(10).
      *
          02 FILLER                    PIC X(3).
          02 ClaimDateC                PIC X.
          02 ClaimDateP                PIC X.
          02 ClaimDateH                PIC X.
          02 ClaimDateV                PIC X.
          02 ClaimDateU                PIC X.
          02 ClaimDateM                PIC X.
          02 ClaimDateO                PIC X(16).
      *
          02 FILLER                    PIC X(3).
          02 INPUT3C                   PIC X.
          02 INPUT3P                   PIC X.
          02 INPUT3H                   PIC X.
          02 INPUT3V                   PIC X.
          02 INPUT3U                   PIC X.
          02 INPUT3M                   PIC X.
          02 INPUT3O                   PIC X(10).
      *
          02 FILLER                    PIC X(3).
          02 DATEC                     PIC X.
          02 DATEP                     PIC X.
          02 DATEH                     PIC X.
          02 DATEV                     PIC X.
          02 DATEU                     PIC X.
          02 DATEM                     PIC X.
          02 DATEO                     PIC X(13).
      *
          02 FILLER                    PIC X(3).
          02 PaidC                     PIC X.
          02 PaidP                     PIC X.
          02 PaidH                     PIC X.
          02 PaidV                     PIC X.
          02 PaidU                     PIC X.
          02 PaidM                     PIC X.
          02 PaidO                     PIC X(16).
      *
          02 FILLER                    PIC X(3).
          02 INPUT4C                   PIC X.
          02 INPUT4P                   PIC X.
          02 INPUT4H                   PIC X.
          02 INPUT4V                   PIC X.
          02 INPUT4U                   PIC X.
          02 INPUT4M                   PIC X.
          02 INPUT4O                   PIC X(10).
      *
          02 FILLER                    PIC X(3).
          02 ValueC                    PIC X.
          02 ValueP                    PIC X.
          02 ValueH                    PIC X.
          02 ValueV                    PIC X.
          02 ValueU                    PIC X.
          02 ValueM                    PIC X.
          02 ValueO                    PIC X(16).
      *
          02 FILLER                    PIC X(3).
          02 INPUT5C                   PIC X.
          02 INPUT5P                   PIC X.
          02 INPUT5H                   PIC X.
          02 INPUT5V                   PIC X.
          02 INPUT5U                   PIC X.
          02 INPUT5M                   PIC X.
          02 INPUT5O                   PIC X(10).
      *
          02 FILLER                    PIC X(3).
          02 CauseC                    PIC X.
          02 CauseP                    PIC X.
          02 CauseH                    PIC X.
          02 CauseV                    PIC X.
          02 CauseU                    PIC X.
          02 CauseM                    PIC X.
          02 CauseO                    PIC X(16).
      *
          02 FILLER                    PIC X(3).
          02 INPUT6C                   PIC X.
          02 INPUT6P                   PIC X.
          02 INPUT6H                   PIC X.
          02 INPUT6V                   PIC X.
          02 INPUT6U                   PIC X.
          02 INPUT6M                   PIC X.
          02 INPUT6O                   PIC X(18).
      *
          02 FILLER                    PIC X(3).
          02 ObservationC              PIC X.
          02 ObservationP              PIC X.
          02 ObservationH              PIC X.
          02 ObservationV              PIC X.
          02 ObservationU              PIC X.
          02 ObservationM              PIC X.
          02 ObservationO              PIC X(16).
      *
          02 FILLER                    PIC X(3).
          02 INPUT8C                   PIC X.
          02 INPUT8P                   PIC X.
          02 INPUT8H                   PIC X.
          02 INPUT8V                   PIC X.
          02 INPUT8U                   PIC X.
          02 INPUT8M                   PIC X.
          02 INPUT8O                   PIC X(18).
      *
          02 FILLER                    PIC X(3).
          02 SelectOptC                PIC X.
          02 SelectOptP                PIC X.
          02 SelectOptH                PIC X.
          02 SelectOptV                PIC X.
          02 SelectOptU                PIC X.
          02 SelectOptM                PIC X.
          02 SelectOptO                PIC X(15).
