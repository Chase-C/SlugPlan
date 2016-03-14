module Scraper.Subjects where

import Prelude

data Subject = AcadEnglish
             | Anthropology
             | Art
             | ArtDesign
             | Astronomy
             | Biochemistry
             | BioEvolutionary
             | BioMolecular
             | Chemistry
             | Chinese
             | ClassicalStudies
             | CognitiveScience
             | College8
             | College9
             | College10
             | CommunityStudies
             | Cowell
             | CriticalRace
             | Crown
             | DigitalArts
             | EarthSci
             | Economics
             | Education
             | AMS
             | BiomolecularEng
             | CompMedia
             | GameMedia
             | CompEngineering
             | CompScience
             | ElectricalEng
             | TechManagement
             | EnviStudies
             | FeministStudies
             | Film
             | French
             | German
             | Greek
             | Hebrew
             | History
             | HistoryArt
             | HistoryConsciousness
             | Italian
             | Japanese
             | JewishStudies
             | Kresge
             | Languages
             | AppliedLinguistics
             | Punjabi
             | Latin                -- go 48
             | LatinoStudies
             | LegalStudies
             | Linguistics
             | Literature
             | CreativeWriting
             | EnglishLit
             | FrenchLit
             | GermanLit
             | GreekLit
             | ItalianLit
             | LatinLit
             | ModernLitStudies
             | PreModernLit
             | RussianLit
             | SpanishLit
             | WorldLit             -- go 64
             | Math
             | Merrill
             | Microbiology
             | Music
             | Oakes
             | OceanSci
             | Philosophy
             | PhysEd
             | Physics
             | Politics
             | Porter
             | Portuguese
             | Psychology
             | Russian
             | ScienceCom
             | SocialDoc            -- go 80
             | Sociology
             | SpanishHeritage
             | Spanish
             | Stevenson
             | TheaterArts
             | UCDC
             | Writing
             | Yiddish
             deriving (Show, Eq, Ord, Enum)

subjectName :: Subject -> String
subjectName AcadEnglish          = "Academic English"
subjectName Anthropology         = "Anthropology"
subjectName Art                  = "Art"
subjectName ArtDesign            = "Art and Design: Games and Playable Media"
subjectName Astronomy            = "Astronomy and Astrophysics"
subjectName Biochemistry         = "Biochemistry and Molecular Biology"
subjectName BioEvolutionary      = "Ecology and Evolutionary Biology"
subjectName BioMolecular         = "Biology: Molecular, Cell, and Developmental Biology"
subjectName Chemistry            = "Chemistry and Biochemistry"
subjectName Chinese              = "Chinese"
subjectName ClassicalStudies     = "Classical Studies"
subjectName College8             = "College Eight"
subjectName College9             = "College Nine"
subjectName College10            = "College Ten"
subjectName CommunityStudies     = "Community Studies"
subjectName Cowell               = "Cowell College"
subjectName CriticalRace         = "Critical Race and Ethnic Studies"
subjectName CognitiveScience     = "Cognitive Science"
subjectName Crown                = "Crown College"
subjectName DigitalArts          = "Digital Arts and New Media"
subjectName EarthSci             = "Earth and Planetary Sciences"
subjectName Economics            = "Economics"
subjectName Education            = "Education"
subjectName AMS                  = "Applied Mathematics and Statistics"
subjectName BiomolecularEng      = "Biomolecular Engineering"
subjectName CompMedia            = "Computational Media"
subjectName GameMedia            = "Games and Playable Media"
subjectName CompEngineering      = "Computer Engineering"
subjectName CompScience          = "Computer Science"
subjectName ElectricalEng        = "Electrical Engineering"
subjectName TechManagement       = "Technology Management"
subjectName EnviStudies          = "Environmental Studies"
subjectName FeministStudies      = "Feminist Studies"
subjectName Film                 = "Film and Digital Media"
subjectName French               = "French"
subjectName German               = "German"
subjectName Greek                = "Greek"
subjectName Hebrew               = "Hebrew"
subjectName History              = "History"
subjectName HistoryArt           = "History of Art and Visual Culture"
subjectName HistoryConsciousness = "History of Consciousness"
subjectName Italian              = "Italian"
subjectName Japanese             = "Japanese"
subjectName JewishStudies        = "Jewish Studies"
subjectName Kresge               = "Kresge College"
subjectName Languages            = "Languages"
subjectName AppliedLinguistics   = "Applied Linguistics"
subjectName Punjabi              = "Punjabi"
subjectName Latin                = "Latin"
subjectName LatinoStudies        = "Latin American and Latino Studies"
subjectName LegalStudies         = "Legal Studies"
subjectName Linguistics          = "Linguistics"
subjectName Literature           = "Literature"
subjectName CreativeWriting      = "Creative Writing"
subjectName EnglishLit           = "English-Language Literatures"
subjectName FrenchLit            = "French Literature"
subjectName GermanLit            = "German Literature"
subjectName GreekLit             = "Greek Literature"
subjectName ItalianLit           = "Italian Literature"
subjectName LatinLit             = "Latin Literature"
subjectName ModernLitStudies     = "Modern Literary Studies"
subjectName PreModernLit         = "Pre- and Early Modern Literature"
subjectName RussianLit           = "Russian Literature"
subjectName SpanishLit           = "Spanish/Latin American/Latino Literatures"
subjectName WorldLit             = "World Literature and Cultural Studies"
subjectName Math                 = "Mathematics"
subjectName Merrill              = "Merrill College"
subjectName Microbiology         = "Microbiology and Environmental Toxicology"
subjectName Music                = "Music"
subjectName Oakes                = "Oakes College"
subjectName OceanSci             = "Ocean Sciences"
subjectName Philosophy           = "Philosophy"
subjectName PhysEd               = "Physical Education"
subjectName Physics              = "Physics"
subjectName Politics             = "Politics"
subjectName Porter               = "Porter College"
subjectName Portuguese           = "Portuguese"
subjectName Psychology           = "Psychology"
subjectName Russian              = "Russian"
subjectName ScienceCom           = "Science Communication"
subjectName SocialDoc            = "Social Documentation"
subjectName Sociology            = "Sociology"
subjectName SpanishHeritage      = "Spanish for Heritage Speakers"
subjectName Spanish              = "Spanish"
subjectName Stevenson            = "Stevenson College"
subjectName TheaterArts          = "Theater Arts"
subjectName UCDC                 = "UCDC"
subjectName Writing              = "Writing Program"
subjectName Yiddish              = "Yiddish"

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
          candidate = filter (\(_, (n, p)) -> (n == strn) || (p == strn)) subStrns
