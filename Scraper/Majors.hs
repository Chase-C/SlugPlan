module Degree where

import Prelude

type Concentrations = [String]

data Degree = BA    String | BAC    String Concentrations
            | BS    String | BSC    String Concentrations
            | Minor String | MinorC String Concentrations
    deriving (Show, Eq)

subjectDegrees :: [(Maybe Subject, Degree)]
subjectDegrees =
    [ (Just Anthropology,     BA    "Anthropology")
    , (Just Anthropology,     Minor "Anthropology")
    , (Just AMS,              Minor "Applied Mathematics")
    , (Just Physics,          BS    "Applied Physics")
    , (Just Art,              BA    "Art")
    , (Just ArtDesign,        BA    "Art and Design: Games and Playable Media")
    , (Just Astronomy,        Minor "Astrophysics")
    , (Biochemistry,          BS    "Biochemistry and Molecular Biology")
    , (Nothing,               BSC   "Bioengineering"
        [ "Assistive Technology: Cognitive/Perceptual"
        , "Assistive Technology: Motor"
        , "Bioelectronics"
        , "Biomolecular"
        ])
    , (Just BiomolecularEng,  BS    "Bioinformatics")
    , (Just BiomolecularEng,  Minor "Bioinformatics")
    , (Just BioEvolutionary,  BA    "Biology")
    , (Just BioEvolutionary,  BS    "Biology")
    , (Just BioEvolutionary,  Minor "Biology")
    , (Just BioEvolutionary,  BAC   "Biology" ["Bioeducation"])
    , (Just BioEvolutionary,  BS    "Ecology and Evolution")
    , (Just BioMolecular,     BS    "Human Biology")
    , (Just BioEvolutionary,  BS    "Marine Biology")
    , (Just BioMolecular,     BS    "Molecular, Cell, and Developmental Biology")
    , (Just BioMolecular,     BS    "Neuroscience")
    , (Just BioEvolutionary,  BS    "Plant Sciences")
    , (Just Chemistry,        BA    "Chemistry")
    , (Just Chemistry,        BS    "Chemistry")
    , (Just Chemistry,        Minor "Chemistry")
    , (Just Chemistry,        BSC   "Chemistry"
        [ "Biochemistry"
        , "Environmental Chemistry"
        ])
    , (Just ClassicalStudies, BA    "Classical Studies")
    , (Just ClassicalStudies, Minor "Classical Studies")
    , (Just CognitiveScience, BS    "Cognitive Science")
    , (Just CommunityStudies, BA    "Community Studies")
    , (Just CompEngineering,  Minor "Computer Engineering")
    , (Just CompEngineering,  BSC   "Computer Engineering"
        [ "Computer Systems"
        , "Digital Hardware"
        , "Networks"
        , "Systems Programming"
        ])
    , (Just CompScience,      BS    "Computer Science")
    , (Just CompScience,      BA    "Computer Science")
    , (Just CompScience,      Minor "Computer Science")
    , (Just CompScience,      BSC   "Computer Science" ["Computer Game Design"])
    , (Just CriticalRace,     BA    "Critical Race and Ethnic Studies")
    , (Just EarthSci,         BS    "Earth Sciences")
    , (Just EarthSci,         Minor "Earth Sciences")
    , (Just EarthSci,         BSC   "Earth Sciences"
        [ "Environmental Geology"
        , "Ocean Sciences"
        , "Planetary Sciences"
        , "Science Education"
        ])
    , (Nothing,               Minor "East Asian Studies")
    , (Just BioEvolutionary,  BS    "Ecology and Evolution")
    , (Just Economics,        BA    "Economics")
    , (Just Economics,        Minor "Economics")
    , (Just Economics,        BA    "Business Management Economics")
    , (Just Economics,        BA    "Global Economics")
    , (Just Education,        Minor "Education")
    , (Just Education,        Minor "Science, Technology, Engineering, and Mathematics")
    , (Just ElectricalEng,    Minor "Electrical Engineering")
    , (Just ElectricalEng,    BSC   "Electrical Engineering"
        [ "Electronics/Optics"
        , "Communications, Signals, Systems, and Controls"
        ])
    , (Just EnviStudies,      BA    "Environmental Studies")
    , (Just EnviStudies,      BAC   "Environmental Studies"
        [ "Agroecology and Sustainable Food Systems" ])
    , (Just FeministStudies,  BAC   "Feminist Studies"
        [ "Culture, Power, and Representation"
        , "Law, Politics, and Social Change"
        , "Science, Technology, and Medicine"
        , "Sexuality Studies"
        ])
    , (Just Film,             BA    "Film and Digital Media")
    , (Just Film,             Minor "Film and Digital Media")
    , (Just Film,             BAC   "Film and Digital Media"
        [ "Critical Studies"
        , "Integrated Critical Practice"
        , "Production"
        ])
    , (Just Nothing,          BA    "German Studies")
    , (Just Sociology,        Minor "GISES")
    , (Just History,          Minor "History")
    , (Just History,          BAC   "History"
        [ "Americas and Africa"
        , "Asia and the Islamic World"
        , "Europe"
        ])
    , (Just HistoryArt,       BA     "History of Art")
    , (Just HistoryArt,       Minor  "History of Art")
    , (Just HistoryArt,       BAC    "History of Art" ["Religion and Visual Culture"])
    , (Just Nothing,          BS     "Italian Studies")
    , (Just Nothing,          Minor  "Italian Studies")
    , (Just JewishStudies,    BS     "Jewish Studies")
    , (Just JewishStudies,    Minor  "Jewish Studies")
    , (Just Nothing,          BAC    "Language Studies" LanguageConcentrations)
    , (Just Nothing,          MinorC "Language Studies" LanguageConcentrations)
    , (LatinoStudies,         BA     "Latino Studies")
    , (LatinoStudies,         Minor  "Latino Studies")
    , (LegalStudies,       

let LanguageConcentrations =
    [ "Chinese"
    , "French"
    , "German"
    , "Italian"
    , "Japanese"
    , "Spanish"
    ]


baseMajor :: Subject -> Maybe Major
baseMajor Anthropology         = Major Anthropology BA [("base", [])]
baseMajor Art                  = Major B [("base", [])]
baseMajor ArtDesign            = Major B [("base", [])]
baseMajor Astronomy            = Major B [("base", [])]
baseMajor Biochemistry         = Major B [("base", [])]
baseMajor BioEvolutionary      = Major B [("base", [])]
baseMajor BioMolecular         = Major B [("base", [])]
baseMajor Chemistry            = Major B [("base", [])]
baseMajor Chinese              = Major B [("base", [])]
baseMajor ClassicalStudies     = Major B [("base", [])]
baseMajor College8             = Major B [("base", [])]
baseMajor College9             = Major B [("base", [])]
baseMajor College10            = Major B [("base", [])]
baseMajor CommunityStudies     = Major B [("base", [])]
baseMajor Cowell               = Major B [("base", [])]
baseMajor CriticalRace         = Major B [("base", [])]
baseMajor CognitiveScience     = Major B [("base", [])]
baseMajor Crown                = Major B [("base", [])]
baseMajor DigitalArts          = Major B [("base", [])]
baseMajor EarthSci             = Major B [("base", [])]
baseMajor Economics            = Major B [("base", [])]
baseMajor Education            = Major B [("base", [])]
baseMajor AMS                  = Major B [("base", [])]
baseMajor BiomolecularEng      = Major B [("base", [])]
baseMajor CompMedia            = Major B [("base", [])]
baseMajor GameMedia            = Major B [("base", [])]
baseMajor CompEngineering      = Major B [("base", [])]
baseMajor CompScience          = Major B [("base", [])]
baseMajor ElectricalEng        = Major B [("base", [])]
baseMajor TechManagement       = Major B [("base", [])]
baseMajor EnviStudies          = Major B [("base", [])]
baseMajor FeministStudies      = Major B [("base", [])]
baseMajor Film                 = Major B [("base", [])]
baseMajor French               = Major B [("base", [])]
baseMajor German               = Major B [("base", [])]
baseMajor Greek                = Major B [("base", [])]
baseMajor Hebrew               = Major B [("base", [])]
baseMajor History              = Major B [("base", [])]
baseMajor HistoryArt           = Major B [("base", [])]
baseMajor HistoryConsciousness = Major B [("base", [])]
baseMajor Italian              = Major B [("base", [])]
baseMajor Japanese             = Major B [("base", [])]
baseMajor JewishStudies        = Major B [("base", [])]
baseMajor Kresge               = Major B [("base", [])]
baseMajor Languages            = Major B [("base", [])]
baseMajor AppliedLinguistics   = Major B [("base", [])]
baseMajor Punjabi              = Major B [("base", [])]
baseMajor Latin                = Major B [("base", [])]
baseMajor LatinoStudies        = Major B [("base", [])]
baseMajor LegalStudies         = Major B [("base", [])]
baseMajor Linguistics          = Major B [("base", [])]
baseMajor Literature           = Major B [("base", [])]
baseMajor CreativeWriting      = Major B [("base", [])]
baseMajor EnglishLit           = Major B [("base", [])]
baseMajor FrenchLit            = Major B [("base", [])]
baseMajor GermanLit            = Major B [("base", [])]
baseMajor GreekLit             = Major B [("base", [])]
baseMajor ItalianLit           = Major B [("base", [])]
baseMajor LatinLit             = Major B [("base", [])]
baseMajor ModernLitStudies     = Major B [("base", [])]
baseMajor PreModernLit         = Major B [("base", [])]
baseMajor RussianLit           = Major B [("base", [])]
baseMajor SpanishLit           = Major B [("base", [])]
baseMajor WorldLit             = Major B [("base", [])]
baseMajor Math                 = Major B [("base", [])]
baseMajor Merrill              = Major B [("base", [])]
baseMajor Microbiology         = Major B [("base", [])]
baseMajor Music                = Major B [("base", [])]
baseMajor Oakes                = Major B [("base", [])]
baseMajor OceanSci             = Major B [("base", [])]
baseMajor Philosophy           = Major B [("base", [])]
baseMajor PhysEd               = Major B [("base", [])]
baseMajor Physics              = Major B [("base", [])]
baseMajor Politics             = Major B [("base", [])]
baseMajor Porter               = Major B [("base", [])]
baseMajor Portuguese           = Major B [("base", [])]
baseMajor Psychology           = Major B [("base", [])]
baseMajor Russian              = Major B [("base", [])]
baseMajor ScienceCom           = Major B [("base", [])]
baseMajor SocialDoc            = Major B [("base", [])]
baseMajor Sociology            = Major B [("base", [])]
baseMajor SpanishHeritage      = Major B [("base", [])]
baseMajor Spanish              = Major B [("base", [])]
baseMajor Stevenson            = Major B [("base", [])]
baseMajor TheaterArts          = Major B [("base", [])]
baseMajor UCDC                 = Major B [("base", [])]
baseMajor Writing              = Major B [("base", [])]
baseMajor Yiddish              = Major B [("base", [])]

subjectSection :: Subject -> String
subjectSection BioEvolutionary    = "Biology: Ecology and Evolutionary Biology"
subjectSection GameMedia          = subjectSection CompMedia
subjectSection Languages          = "Languages and Applied Linguistics"
subjectSection AppliedLinguistics = subjectSection Languages
subjectSection Punjabi            = subjectSection Languages
subjectSection CreativeWriting    = subjectSection Literature
subjectSection EnglishLit         = subjectSection Literature
subjectSection FrenchLit          = subjectSection Literature
subjectSection GermanLit          = subjectSection Literature
subjectSection GreekLit           = subjectSection Literature
subjectSection ItalianLit         = subjectSection Literature
subjectSection LatinLit           = subjectSection Literature
subjectSection ModernLitStudies   = subjectSection Literature
subjectSection PreModernLit       = subjectSection Literature
subjectSection RussianLit         = subjectSection Literature
subjectSection SpanishLit         = subjectSection Literature
subjectSection WorldLit           = subjectSection Literature
subjectSection SpanishHeritage    = "Spanish and Spanish for Heritage Speakers"
subjectSection Spanish            = subjectSection SpanishHeritage
subjectSection UCDC               = "UCDC Program"
subjectSection subject            = subjectName subject

subjectPrefix :: Subject -> String
subjectPrefix AcadEnglish          = "ACEN"
subjectPrefix Anthropology         = "ANTH"
subjectPrefix Art                  = "ART"
subjectPrefix ArtDesign            = "ARTG"
subjectPrefix Astronomy            = "ASTR"
subjectPrefix Biochemistry         = "BIOC"
subjectPrefix BioEvolutionary      = "BIOE"
subjectPrefix BioMolecular         = "BIOL"
subjectPrefix Chemistry            = "CHEM"
subjectPrefix Chinese              = "CHIN"
subjectPrefix ClassicalStudies     = ""
subjectPrefix CognitiveScience     = ""
subjectPrefix College8             = "CLEI"
subjectPrefix College9             = "CLNI"
subjectPrefix College10            = "CLTE"
subjectPrefix CommunityStudies     = "CMMU"
subjectPrefix Cowell               = "COWL"
subjectPrefix CriticalRace         = "CRES"
subjectPrefix Crown                = "CRWN"
subjectPrefix DigitalArts          = "DANM"
subjectPrefix EarthSci             = "EART"
subjectPrefix Economics            = "ECON"
subjectPrefix Education            = "EDUC"
subjectPrefix AMS                  = "AMS"
subjectPrefix BiomolecularEng      = "BME"
subjectPrefix CompMedia            = "CMPM"
subjectPrefix GameMedia            = "GAME"
subjectPrefix CompEngineering      = "CMPE"
subjectPrefix CompScience          = "CMPS"
subjectPrefix ElectricalEng        = "EE"
subjectPrefix TechManagement       = "TIM"
subjectPrefix EnviStudies          = "ENVS"
subjectPrefix FeministStudies      = "FMST"
subjectPrefix Film                 = "FILM"
subjectPrefix French               = "FREN"
subjectPrefix German               = "GERM"
subjectPrefix Greek                = "GREE"
subjectPrefix Hebrew               = "HEBR"
subjectPrefix History              = "HIST"
subjectPrefix HistoryArt           = "HAVC"
subjectPrefix HistoryConsciousness = "HISC"
subjectPrefix Italian              = "ITAL"
subjectPrefix Japanese             = "JAPN"
subjectPrefix JewishStudies        = ""
subjectPrefix Kresge               = "KRSG"
subjectPrefix Languages            = "LAAD"
subjectPrefix AppliedLinguistics   = "APLX"
subjectPrefix Punjabi              = "PUNJ"
subjectPrefix Latin                = "LATN"
subjectPrefix LatinoStudies        = "LALS"
subjectPrefix LegalStudies         = "LGST"
subjectPrefix Linguistics          = "LLING"
subjectPrefix Literature           = "LIT"
subjectPrefix CreativeWriting      = "LTCR"
subjectPrefix EnglishLit           = "LTEL"
subjectPrefix FrenchLit            = "LTFR"
subjectPrefix GermanLit            = "LTGE"
subjectPrefix GreekLit             = "LTGR"
subjectPrefix ItalianLit           = "LTIT"
subjectPrefix LatinLit             = "LTIN"
subjectPrefix ModernLitStudies     = "LTMO"
subjectPrefix PreModernLit         = "LTPR"
subjectPrefix RussianLit           = ""
subjectPrefix SpanishLit           = "LTSP"
subjectPrefix WorldLit             = "LTWL"
subjectPrefix Math                 = "MATH"
subjectPrefix Merrill              = "MERR"
subjectPrefix Microbiology         = "METX"
subjectPrefix Music                = "MUSC"
subjectPrefix Oakes                = "OAKS"
subjectPrefix OceanSci             = "OCEA"
subjectPrefix Philosophy           = "PHIL"
subjectPrefix PhysEd               = "PHYE"
subjectPrefix Physics              = "PHYS"
subjectPrefix Politics             = "POLI"
subjectPrefix Porter               = "PRTR"
subjectPrefix Portuguese           = "PORT"
subjectPrefix Psychology           = "PSYC"
subjectPrefix Russian              = "RUSS"
subjectPrefix ScienceCom           = "SCIC"
subjectPrefix SocialDoc            = "SOCD"
subjectPrefix Sociology            = "SOCY"
subjectPrefix SpanishHeritage      = "SPHS"
subjectPrefix Spanish              = "SPAN"
subjectPrefix Stevenson            = "STEV"
subjectPrefix TheaterArts          = "THEA"
subjectPrefix UCDC                 = "UCDC"
subjectPrefix Writing              = "WRIT"
subjectPrefix Yiddish              = "YIDD"

getSubjectFromString :: String -> Subject
getSubjectFromString strn = fst $ head candidate
    where subjects = [AcadEnglish ..]
          subStrns = zip subjects $ map (\s -> (subjectName s, subjectPrefix s)) subjects
          candidate = filter (\(s, (n, p)) -> (n == strn) || (p == strn)) subStrns
