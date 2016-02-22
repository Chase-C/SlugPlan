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
             | Latin
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
             | WorldLit
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
             | SocialDoc
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

subjectPrefix :: Subject -> String
subjectPrefix AcadEnglish          = ""
subjectPrefix Anthropology         = ""
subjectPrefix Art                  = ""
subjectPrefix ArtDesign            = ""
subjectPrefix Astronomy            = ""
subjectPrefix Biochemistry         = ""
subjectPrefix BioEvolutionary      = ""
subjectPrefix BioMolecular         = ""
subjectPrefix Chemistry            = ""
subjectPrefix Chinese              = ""
subjectPrefix ClassicalStudies     = ""
subjectPrefix CognitiveScience     = ""
subjectPrefix College8             = ""
subjectPrefix College9             = ""
subjectPrefix College10            = ""
subjectPrefix CommunityStudies     = ""
subjectPrefix Cowell               = ""
subjectPrefix CriticalRace         = ""
subjectPrefix Crown                = ""
subjectPrefix DigitalArts          = ""
subjectPrefix EarthSci             = ""
subjectPrefix Economics            = ""
subjectPrefix Education            = ""
subjectPrefix AMS                  = ""
subjectPrefix BiomolecularEng      = ""
subjectPrefix CompMedia            = ""
subjectPrefix CompEngineering      = ""
subjectPrefix CompScience          = ""
subjectPrefix ElectricalEng        = ""
subjectPrefix TechManagement       = ""
subjectPrefix EnviStudies          = ""
subjectPrefix FeministStudies      = ""
subjectPrefix Film                 = ""
subjectPrefix French               = ""
subjectPrefix German               = ""
subjectPrefix Greek                = ""
subjectPrefix Hebrew               = ""
subjectPrefix History              = ""
subjectPrefix HistoryArt           = ""
subjectPrefix HistoryConsciousness = ""
subjectPrefix Italian              = ""
subjectPrefix Japanese             = ""
subjectPrefix JewishStudies        = ""
subjectPrefix Kresge               = ""
subjectPrefix Languages            = ""
subjectPrefix AppliedLinguistics   = ""
subjectPrefix Punjabi              = ""
subjectPrefix Latin                = ""
subjectPrefix LatinoStudies        = ""
subjectPrefix LegalStudies         = ""
subjectPrefix Linguistics          = ""
subjectPrefix Literature           = ""
subjectPrefix CreativeWriting      = ""
subjectPrefix EnglishLit           = ""
subjectPrefix FrenchLit            = ""
subjectPrefix GermanLit            = ""
subjectPrefix GreekLit             = ""
subjectPrefix ItalianLit           = ""
subjectPrefix LatinLit             = ""
subjectPrefix ModernLitStudies     = ""
subjectPrefix PreModernLit         = ""
subjectPrefix RussianLit           = ""
subjectPrefix SpanishLit           = ""
subjectPrefix WorldLit             = ""
subjectPrefix Math                 = ""
subjectPrefix Merrill              = ""
subjectPrefix Microbiology         = ""
subjectPrefix Music                = ""
subjectPrefix Oakes                = ""
subjectPrefix OceanSci             = ""
subjectPrefix Philosophy           = ""
subjectPrefix PhysEd               = ""
subjectPrefix Physics              = ""
subjectPrefix Politics             = ""
subjectPrefix Porter               = ""
subjectPrefix Portuguese           = ""
subjectPrefix Psychology           = ""
subjectPrefix Russian              = ""
subjectPrefix ScienceCom           = ""
subjectPrefix SocialDoc            = ""
subjectPrefix Sociology            = ""
subjectPrefix SpanishHeritage      = ""
subjectPrefix Spanish              = ""
subjectPrefix Stevenson            = ""
subjectPrefix TheaterArts          = ""
subjectPrefix UCDC                 = ""
subjectPrefix Writing              = ""
subjectPrefix Yiddish              = ""
