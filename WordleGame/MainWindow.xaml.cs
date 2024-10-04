using Microsoft.UI;
using Microsoft.UI.Xaml;
using Microsoft.UI.Xaml.Controls;
using Microsoft.UI.Xaml.Controls.Primitives;
using Microsoft.UI.Xaml.Data;
using Microsoft.UI.Xaml.Documents;
using Microsoft.UI.Xaml.Input;
using Microsoft.UI.Xaml.Media;
using Microsoft.UI.Xaml.Navigation;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices.WindowsRuntime;
using Windows.Foundation;
using Windows.Foundation.Collections;
using WordleGame.Models;
using static System.Net.Mime.MediaTypeNames;
//using WordleGame

// To learn more about WinUI, the WinUI project structure,
// and more about our project templates, see: http://aka.ms/winui-project-info.

namespace WordleGame
{
    /// <summary>
    /// An empty window that can be used on its own or navigated to within a Frame.
    /// </summary>
    public sealed partial class MainWindow : Window
    {
        public MainWindow()
        {
            this.InitializeComponent();
            main();
        }

        //private void myButton_Click(object sender, RoutedEventArgs e)
        //{
        //    myButton.Content = "Clicked";
        //}
        
        public string wordleWord;
        public TextBlock[] row1;
        public TextBlock[] row2;
        public TextBlock[] row3;
        public TextBlock[] row4;
        public TextBlock[] row5;
        public TextBlock[] row6;
        public int TryCount = 1;
        
        public  string[] fiveLetterWords = {
        "aback", "abase", "abate", "abbey", "abbot", "abhor", "abide", "abled", "abode", "abort",
        "about", "above", "abuse", "abyss", "acorn", "acrid", "actor", "acute", "adage", "adapt",
        "adept", "admin", "admit", "adobe", "adopt", "adore", "adorn", "adult", "affix", "afire",
        "afoot", "afoul", "after", "again", "agape", "agate", "agent", "agile", "aging", "aglow",
        "agony", "agree", "ahead", "aider", "aisle", "alarm", "album", "alert", "algae", "alibi",
        "alien", "align", "alike", "alive", "allay", "alley", "allot", "allow", "alloy", "aloft",
        "alone", "along", "aloof", "aloud", "alpha", "altar", "alter", "amass", "amaze", "amber",
        "amble", "amend", "amiss", "amity", "among", "ample", "amply", "amuse", "angel", "anger",
        "angle", "angry", "angst", "anime", "ankle", "annex", "annoy", "annul", "anode", "antic",
        "anvil", "aorta", "apart", "aphid", "aping", "apple", "apply", "apron", "aptly", "arbor",
        "ardor", "arena", "argue", "arise", "armor", "aroma", "arose", "array", "arrow", "arson",
        "artsy", "ascot", "ashen", "aside", "askew", "assay", "asset", "atoll", "atone", "attic",
        "audio", "audit", "augur", "aunty", "avail", "avert", "avian", "avoid", "await", "awake",
        "award", "aware", "awash", "awful", "awoke", "axial", "axiom", "axion", "azure", "bacon",
        "badge", "badly", "bagel", "baggy", "baker", "baler", "balmy", "banal", "banjo", "barge",
        "baron", "basal", "basic", "basil", "basin", "basis", "baste", "batch", "bathe", "baton",
        "batty", "bawdy", "bayou", "beach", "beady", "beard", "beast", "beech", "beefy", "befit",
        "began", "begat", "beget", "begin", "begun", "being", "belch", "belie", "belle", "belly",
        "below", "bench", "beret", "berry", "berth", "beset", "betel", "bevel", "bezel", "bible",
        "bicep", "biddy", "bigot", "bilge", "billy", "binge", "bingo", "biome", "birch", "birth",
        "bison", "bitty", "black", "blade", "blame", "bland", "blank", "blare", "blast", "blaze",
        "bleak", "bleat", "bleed", "bleep", "blend", "bless", "blimp", "blind", "blink", "bliss",
        "blitz", "bloat", "block", "bloke", "blond", "blood", "bloom", "blown", "bluer", "bluff",
        "blunt", "blurb", "blurt", "blush", "board", "boast", "bobby", "boney", "bongo", "bonus",
        "booby", "boost", "booth", "booty", "booze", "boozy", "borax", "borne", "bosom", "bossy",
        "botch", "bough", "boule", "bound", "bowel", "boxer", "brace", "braid", "brain", "brake",
        "brand", "brash", "brass", "brave", "bravo", "brawl", "brawn", "bread", "break", "breed",
        "briar", "bribe", "brick", "bride", "brief", "brine", "bring", "brink", "briny", "brisk",
        "broad", "broil", "broke", "brood", "brook", "broom", "broth", "brown", "brunt", "brush",
        "brute", "buddy", "budge", "buggy", "bugle", "build", "built", "bulge", "bulky", "bully",
        "bunch", "bunny", "burly", "burnt", "burst", "bused", "bushy", "butch", "butte", "buxom",
        "buyer", "bylaw", "cabal", "cabby", "cabin", "cable", "cacao", "cache", "cacti", "caddy",
        "cadet", "cagey", "cairn", "camel", "cameo", "canal", "candy", "canny", "canoe", "canon",
        "caper", "caput", "carat", "cargo", "carol", "carry", "carve", "caste", "catch", "cater",
        "catty", "caulk", "cause", "cavil", "cease", "cedar", "cello", "chafe", "chaff", "chain",
        "chair", "chalk", "champ", "chant", "chaos", "chard", "charm", "chart", "chase", "chasm",
        "cheap", "cheat", "check", "cheek", "cheer", "chess", "chest", "chick", "chide", "chief",
        "child", "chili", "chill", "chime", "china", "chirp", "chock", "choir", "choke", "chord",
        "chore", "chose", "chuck", "chump", "chunk", "churn", "chute", "cider", "cigar", "cinch",
        "circa", "civic", "civil", "clack", "claim", "clamp", "clang", "clank", "clash", "clasp",
        "class", "clean", "clear", "cleat", "cleft", "clerk", "click", "cliff", "climb", "cling",
        "clink", "cloak", "clock", "clone", "close", "cloth", "cloud", "clout", "clove", "clown",
        "cluck", "clued", "clump", "clung", "coach", "coast", "cobra", "cocoa", "colon", "color",
        "comet", "comfy", "comic", "comma", "conch", "condo", "conic", "copse", "coral", "corer",
        "corny", "couch", "cough", "could", "count", "coupe", "court", "coven", "cover", "covet",
        "covey", "cower", "coyly", "crack", "craft", "cramp", "crane", "crank", "crash", "crass",
        "crate", "crave", "crawl", "craze", "crazy", "creak", "cream", "credo", "creed", "creek",
        "creep", "creme", "crepe", "crept", "cress", "crest", "crick", "cried", "crier", "crime",
        "crimp", "crisp", "croak", "crock", "crone", "crony", "crook", "cross", "croup", "crowd",
        "crown", "crude", "cruel", "crumb", "crump", "crush", "crust", "crypt", "cubic", "cumin",
        "curio", "curly", "curry", "curse", "curve", "curvy", "cutie", "cyber", "cycle", "cynic",
        "daddy", "daily", "dairy", "daisy", "dally", "dance", "dandy", "datum", "daunt", "dealt",
        "death", "debar", "debit", "debug", "debut", "decal", "decay", "decor", "decoy", "decry",
        "defer", "deign", "deity", "delay", "delta", "delve", "demon", "demur", "denim", "depot",
        "depth", "derby", "deter", "detox", "deuce", "devil", "diary", "dicey", "digit", "dilly",
        "dimly", "diner", "dingy", "diode", "dirge", "dirty", "disco", "ditch", "ditto", "ditty",
        "diver", "dizzy", "dodgy", "dogma", "doing", "dolly", "donor", "donut", "dopey", "doubt",
        "dough", "dowdy", "dowel", "downy", "dowry", "dozen", "draft", "drain", "drake", "drama",
        "drank", "drape", "drawl", "drawn", "dread", "dream", "dress", "dried", "drier", "drift",
        "drill", "drink", "drive", "droid", "drone", "drool", "droop", "dross", "drove", "drown",
        "drunk", "dryly", "duchy", "dully", "dummy", "dumpy", "dunce", "dusky", "dusty", "dutch",
        "duvet", "dwarf", "dwell", "dwelt", "dying", "eager", "eagle", "early", "earth", "easel",
        "eaten", "eater", "ebony", "eclat", "edict", "edify", "eerie", "egret", "eight", "eject",
        "eking", "elate", "elbow", "elder", "elect", "elegy", "elfin", "elide", "elite", "elope",
        "elude", "email", "embed", "ember", "emcee", "empty", "enact", "endow", "enemy", "enjoy",
        "ennui", "ensue", "enter", "entry", "envoy", "epoch", "epoxy", "equal", "equip", "erase",
        "erect", "erode", "error", "erupt", "essay", "ester", "ether", "ethic", "ethos", "etude",
        "evade", "event", "every", "evict", "exact", "exalt", "excel", "exert", "exile", "exist",
        "expel", "extol", "extra", "exult", "eying", "fable", "facet", "faint", "fairy", "faith",
        "false", "fancy", "fanny", "farce", "fatal", "fatty", "fault", "fauna", "favor", "feast",
        "fecal", "feign", "fella", "felon", "femur", "fence", "feral", "ferry", "fetal", "fetch",
        "fetid", "fetus", "fever", "fewer", "fiber", "fibre", "ficus", "field", "fiend", "fiery",
        "fifth", "fifty", "fight", "filer", "filet", "filly", "filmy", "filth", "final", "finch",
        "finny", "first", "fishy", "fixer", "fizzy", "fjord", "flack", "flail", "flair", "flake",
        "flaky", "flame", "flank", "flare", "flash", "flask", "fleck", "fleet", "flesh", "flick",
        "flier", "fling", "flint", "flirt", "float", "flock", "flood", "floor", "flora", "floss",
        "flour", "flout", "flown", "fluff", "fluid", "fluke", "flume", "flung", "flunk", "flush",
        "flute", "flyer", "foamy", "focal", "focus", "foggy", "foist", "folio", "folly", "foray",
        "force", "forge", "forgo", "forte", "forth", "forty", "forum", "found", "foyer", "frail",
        "frame", "frank", "fraud", "freak", "freed", "freer", "fresh", "friar", "fried", "frill",
        "frisk", "fritz", "frock", "frond", "front", "frost", "froth", "frown", "froze", "fruit",
        "fudge", "fuels", "fugal", "fugue", "fully", "fungi", "funky", "funny", "furor", "furry",
        "fussy", "fuzzy", "gaffe", "gaily", "gamer", "gamma", "gamut", "gassy", "gaudy", "gauge",
        "gaunt", "gauze", "gavel", "gawky", "gayer", "gayly", "gazer", "gears", "gecko", "geeky",
        "geese", "genie", "genre", "ghost", "ghoul", "giant", "giddy", "gipsy", "girly", "girth",
        "given", "giver", "glade", "gland", "glare", "glass", "glaze", "gleam", "glean", "glide",
        "glint", "gloat", "globe", "gloom", "glory", "gloss", "glove", "glyph", "gnarl", "gnash",
        "gnome", "godly", "going", "golly", "gonad", "goner", "goofy", "goose", "gorge", "gouge",
        "gourd", "grace", "grade", "graft", "grail", "grain", "grand", "grant", "grape", "graph",
        "grasp", "grass", "grate", "grave", "gravy", "graze", "great", "greed", "green", "greet",
        "grief", "grill", "grime", "grimy", "grind", "gripe", "groan", "groin", "groom", "grope",
        "gross", "group", "grout", "grove", "growl", "grown", "gruel", "gruff", "grunt", "guard",
        "guava", "guess", "guest", "guide", "guild", "guile", "guilt", "guise", "gulch", "gully",
        "gumbo", "gummy", "guppy", "gusto", "gusty", "gypsy", "habit", "hairy", "halve", "hands",
        "handy", "happy", "hardy", "harem", "harpy", "harry", "harsh", "haste", "hasty", "hatch",
        "hater", "haunt", "haven", "havoc", "hazel", "heady", "heard", "heart", "heath", "heave",
        "heavy", "hedge", "hefty", "heist", "helix", "hello", "hence", "heron", "hilly", "hinge",
        "hippo", "hippy", "hitch", "hoard", "hoary", "hobby", "hoist", "holly", "homer", "honey",
        "honor", "horde", "horny", "horse", "hotel", "hotly", "hound", "house", "hovel", "hover",
        "howdy", "human", "humid", "humor", "humph", "humus", "hunch", "hunky", "hurry", "husky",
        "hussy", "hutch", "hydro", "hyena", "hymen", "hyper", "icily", "icing", "ideal", "idiom",
        "idiot", "idler", "idyll", "igloo", "iliac", "image", "imbue", "impel", "imply", "inane",
        "inbox", "incur", "index", "inept", "inert", "infer", "ingot", "inlay", "inlet", "inner",
        "input", "inter", "intro", "ionic", "irate", "irony", "islet", "issue", "itchy", "ivory",
        "jaunt", "jazzy", "jelly", "jerky", "jetty", "jiffy", "joint", "joist", "joker", "jolly",
        "joust", "judge", "juice", "juicy", "jumbo", "jumpy", "junta", "junto", "juror", "kappa",
        "karma", "kayak", "kebab", "khaki", "kinky", "kiosk", "kitty", "knack", "knave", "knead",
        "kneel", "knelt", "knife", "knock", "knoll", "known", "koala", "krill", "label", "labor",
        "lacey", "laddy", "lager", "lance", "lanky", "lapel", "lapse", "large", "larva", "lasso",
        "latch", "later", "lathe", "latte", "laugh", "layer", "leach", "leafy", "leaky", "leant",
        "leapt", "learn", "lease", "leash", "least", "leave", "ledge", "leech", "leery", "lefty",
        "legal", "leggy", "lemon", "lemur", "leper", "level", "lever", "libel", "liege", "light",
        "liken", "lilac", "limbo", "limit", "linen", "liner", "lingo", "lipid", "lithe", "liver",
        "livid", "llama", "loamy", "loath", "lobby", "local", "locus", "lodge", "lofty", "logic",
        "login", "loopy", "loose", "lorry", "loser", "louse", "lousy", "lover", "lower", "lowly",
        "loyal", "lucid", "lucky", "lumen", "lumpy", "lunar", "lunch", "lunge", "lupus", "lurch",
        "lurid", "lusty", "lying", "lymph", "lynch", "lyric", "macaw", "macho", "macro", "madam",
        "madly", "mafia", "magic", "magma", "maize", "major", "maker", "mambo", "mamma", "mammy",
        "manga", "mange", "mango", "mangy", "mania", "manic", "manly", "manor", "maple", "march",
        "marry", "marsh", "mason", "masse", "match", "matey", "mauve", "maxim", "maybe", "mayor",
        "mealy", "meant", "meaty", "medal", "media", "medic", "melee", "melon", "mercy", "merge",
        "merit", "merry", "messy", "metal", "meter", "metro", "micro", "midge", "midst", "might",
        "milky", "mimic", "mince", "miner", "minim", "minor", "minty", "minus", "mirth", "miser",
        "missy", "mocha", "modal", "model", "modem", "mogul", "moist", "molar", "moldy", "money",
        "month", "moody", "moose", "moral", "moron", "morph", "mossy", "motel", "motif", "motor",
        "motto", "moult", "mound", "mount", "mourn", "mouse", "mousy", "mouth", "mover", "movie",
        "mower", "mucky", "mucus", "muddy", "mulch", "mummy", "munch", "mural", "murky", "mushy",
        "music", "musky", "musty", "myrrh", "nabob", "naive", "nanny", "nasal", "nasty", "natal",
        "naval", "navel", "needy", "neigh", "nerdy", "nerve", "never", "newer", "newly", "nicer",
        "niche", "niece", "night", "ninja", "ninny", "ninth", "noble", "nobly", "noise", "noisy",
        "nomad", "noose", "north", "nosey", "notch", "novel", "nudge", "nurse", "nutty", "nylon",
        "nymph", "oaken", "obese", "occur", "ocean", "octal", "octet", "odder", "oddly", "offal",
        "offer", "often", "olden", "older", "olive", "ombre", "omega", "onion", "onset", "opera",
        "opine", "opium", "optic", "orbit", "order", "organ", "other", "otter", "ought", "ounce",
        "outdo", "outer", "outgo", "ovary", "ovate", "overt", "ovine", "owing", "owner", "oxide",
        "ozone", "paddy", "pagan", "paint", "paler", "palsy", "panel", "panic", "pansy", "paper",
        "parer", "parka", "parry", "parse", "party", "pasta", "paste", "pasty", "patch", "patio",
        "patsy", "patty", "pause", "payee", "payer", "peace", "peach", "pearl", "pecan", "pedal",
        "penal", "pence", "penne", "penny", "perch", "peril", "perky", "pesky", "pesto", "petal",
        "petty", "phase", "phone", "phony", "photo", "piano", "picky", "piece", "piety", "piggy",
        "piker", "pilot", "pinch", "piney", "pinky", "pinot", "pinto", "piper", "pique", "pitch",
        "pithy", "pivot", "pixel", "pixie", "pizza", "place", "plaid", "plain", "plait", "plane",
        "plank", "plant", "plate", "plaza", "plead", "pleat", "plied", "plier", "pluck", "plumb",
        "plume", "plump", "plunk", "plush", "poesy", "point", "poise", "poker", "polar", "polka",
        "polyp", "pouch", "pound", "pouty", "power", "preen", "press", "price", "prick", "pride",
        "pried", "prime", "primo", "print", "prior", "prism", "privy", "prize", "probe", "prone",
        "prong", "proof", "prose", "proud", "prove", "prowl", "proxy", "prude", "psalm", "pubic",
        "pudgy", "puffy", "pulpy", "pulse", "punch", "pupal", "pupil", "puppy", "puree", "purer",
        "purge", "purse", "pushy", "putty", "pygmy", "quack", "quaff", "quail", "quake", "qualm",
        "quark", "quart", "quash", "quasi", "queen", "queer", "quell", "query", "quest", "queue",
        "quick", "quiet", "quill", "quilt", "quirk", "quite", "quota", "quote", "quoth", "rabbi",
        "rabid", "racer", "radar", "radii", "radio", "rainy", "raise", "rajah", "rally", "ralph",
        "ramen", "ranch", "randy", "range", "rapid", "rarer", "raspy", "ratio", "ratty", "raven",
        "rayon", "razor", "reach", "react", "ready", "realm", "rearm", "rebar", "rebel", "rebus",
        "rebut", "recap", "recur", "recut", "reedy", "refer", "refit", "regal", "rehab", "reign",
        "relax", "relay", "relic", "remit", "renal", "renew", "repay", "repel", "reply", "rerun",
        "reset", "resin", "retro", "retry", "reuse", "revel", "revue", "rhino", "rhyme", "rider",
        "ridge", "rifle", "right", "rigid", "rigor", "rinse", "ripen", "riper", "risen", "riser",
        "risky", "rival", "river", "rivet", "roach", "roast", "robin", "robot", "rocky", "rodeo",
        "roger", "rogue", "roomy", "roost", "rotor", "rouge", "rough", "round", "rouse", "route",
        "rowdy", "rower", "royal", "ruddy", "ruder", "rugby", "ruler", "rumba", "rumor", "rupee",
        "rural", "rusty", "sadly", "safer", "saint", "salad", "sally", "salon", "salsa", "salty",
        "salve", "salvo", "sandy", "saner", "sappy", "sassy", "satin", "satyr", "sauce", "saucy",
        "sauna", "saute", "savor", "savvy", "scald", "scale", "scalp", "scaly", "scamp", "scant",
        "scarf", "scary", "scene", "scent", "scion", "scoff", "scold", "scone", "scoop", "scope",
        "score", "scorn", "scour", "scout", "scowl", "scram", "scrap", "scree", "screw", "scrub",
        "scrum", "scuba", "sedan", "seedy", "segue", "seize", "semen", "sense", "sepia", "serif",
        "serum", "serve", "setup", "seven", "sever", "sewer", "shack", "shade", "shady", "shaft",
        "shake", "shaky", "shale", "shall", "shalt", "shame", "shank", "shape", "shard", "share",
        "shark", "sharp", "shave", "shawl", "shear", "sheen", "sheep", "sheer", "sheet", "shelf",
        "shell", "shine", "shiny", "shire", "shirk", "shirt", "shock", "shone", "shook", "shoot",
        "shore", "shorn", "short", "shout", "shove", "shown", "showy", "shrew", "shrub", "shrug",
        "shuck", "shunt", "shush", "shyly", "sided", "siege", "sieve", "sight", "sigma", "signs",
        "silky", "silly", "since", "sinew", "singe", "siren", "sissy", "sixth", "sixty", "sized",
        "skate", "skeet", "skein", "skier", "skiff", "skill", "skimp", "skirt", "slack", "slain",
        "slang", "slant", "slash", "slate", "slaty", "slave", "sleek", "sleep", "sleet", "slept",
        "slice", "slick", "slide", "slime", "slimy", "sling", "slink", "sloop", "slope", "slosh",
        "sloth", "slump", "slung", "slurp", "slush", "slyly", "smack", "small", "smart", "smash",
        "smear", "smell", "smelt", "smile", "smite", "smith", "smock", "smoke", "smoky", "smote",
        "snack", "snafu", "snail", "snake", "snaky", "snare", "snarl", "sneak", "sneer", "snide",
        "snipe", "snood", "snoop", "snore", "snort", "snout", "snowy", "snuck", "snuff", "soapy",
        "sober", "soggy", "solar", "solid", "solve", "sonar", "sonic", "sooth", "sooty", "sorry",
        "sound", "soupy", "south", "sower", "space", "spade", "spank", "spare", "spark", "spasm",
        "spawn", "speak", "spear", "speck", "speed", "spell", "spelt", "spend", "spent", "sperm",
        "spice", "spicy", "spied", "spiel", "spike", "spiky", "spill", "spilt", "spine", "spiny",
        "spire", "spite", "spoil", "spoke", "spoof", "spook", "spool", "spoon", "spore", "sport",
        "spout", "spray", "spree", "sprig", "spunk", "spurn", "spurt", "squad", "squat", "squib",
        "stack", "staff", "stage", "stain", "stair", "stake", "stale", "stalk", "stall", "stamp",
        "stand", "stank", "stark", "start", "stash", "state", "stave", "steak", "steal", "steam",
        "steed", "steel", "steep", "steer", "stench", "stent", "steps", "stern", "stick", "stiff",
        "still", "sting", "stink", "stint", "stock", "stoke", "stole", "stomp", "stone", "stony",
        "stood", "stool", "stoop", "store", "stork", "storm", "story", "stout", "stove", "strap",
        "straw", "stray", "strip", "strut", "stuck", "study", "stuff", "stump", "stung", "stunk",
        "style", "suave", "sugar", "suing", "suite", "sulky", "sully", "sumac", "sunny", "super",
        "surge", "surly", "sushi", "swami", "swamp", "swarm", "swash", "swath", "swear", "sweat",
        "sweep", "sweet", "swell", "swept", "swift", "swill", "swing", "swirl", "swish", "sword",
        "swore", "sworn", "swung", "synod", "syrup", "tabby", "table", "taboo", "tacit", "tacky",
        "taffy", "taint", "taken", "taker", "tally", "talon", "tamer", "tango", "tangy", "taper",
        "tapir", "tardy", "tarot", "taste", "tasty", "tatty", "taunt", "tawny", "taxer", "tease",
        "teddy", "teeth", "tempo", "tenet", "tenor", "tense", "tenth", "tepee", "tepid", "terra",
        "terse", "testy", "thank", "theft", "their", "theme", "there", "these", "thick", "thief",
        "thigh", "thing", "think", "third", "thong", "thorn", "those", "three", "threw", "throb",
        "throw", "thrum", "thumb", "thump", "thyme", "tiara", "tibia", "tidal", "tiger", "tight",
        "tilde", "timer", "timid", "tipsy", "titan", "tithe", "title", "toast", "today", "toddy",
        "token", "tonal", "tonga", "tonic", "tooth", "topaz", "topic", "torch", "torso", "total",
        "totem", "touch", "tough", "towel", "tower", "toxic", "toxin", "trace", "track", "tract",
        "trade", "trail", "train", "trait", "tramp", "trash", "trawl", "tread", "treat", "trend",
        "tress", "triad", "trial", "tribe", "trice", "trick", "tried", "tripe", "trite", "troll",
        "troop", "trope", "trout", "trove", "truce", "truck", "truer", "truly", "trump", "trunk",
        "trust", "truth", "tryst", "tuber", "tulip", "tulle", "tumor", "tuned", "tuner", "tunic",
        "turbo", "turfy", "turnt", "tusky", "tutor", "twang", "tweak", "tweed", "tweet", "twice",
        "twine", "twirl", "twist", "twixt", "tying", "udder", "ulcer", "ultra", "uncle", "uncut",
        "under", "undid", "undue", "unfed", "unfit", "unify", "union", "unite", "unity", "untie",
        "until", "unwed", "unzip", "upper", "upset", "urban", "urged", "urine", "usage", "usher",
        "using", "usual", "usurp", "utter", "vague", "valet", "valid", "valor", "value", "valve",
        "vapid", "vapor", "vault", "vaunt", "vegan", "venom", "venue", "verge", "verse", "verso",
        "vicar", "video", "vigil", "vigor", "villa", "vinyl", "viola", "viper", "viral", "virgo",
        "visit", "visor", "vista", "vital", "vivid", "vixen", "vocal", "vodka", "vogue", "voice",
        "voila", "vomit", "voter", "vouch", "vowed", "vowel", "vying", "wacky", "wafer", "wager",
        "wagon", "waist", "waive", "waltz", "warty", "waste", "watch", "water", "waver", "waxen",
        "weary", "weave", "wedge", "weedy", "weigh", "weird", "welch", "welsh", "wench", "whack",
        "whale", "wharf", "wheat", "wheel", "whelp", "where", "which", "whiff", "while", "whine",
        "whiny", "whisk", "white", "whole", "whoop", "whose", "widen", "wider", "widow", "width",
        "wield", "wight", "wimpy", "wince", "winch", "windy", "wiser", "wispy", "witch", "witty",
        "woman", "women", "woody", "wooer", "wooly", "woozy", "wordy", "world", "worry", "worse",
        "worst", "worth", "would", "wound", "woven", "wrack", "wrath", "wreak", "wreck", "wrest",
        "wring", "wrist", "write", "wrong", "wrote", "wrung", "wryly", "xenon", "xylem", "yacht",
        "yearn", "yeast", "yield", "young", "youth", "zebra", "zesty", "zonal", "zoned", "tiles", "tests", "claps" };
        public void main()
        {
            Random random = new Random();
            int randomNumber = random.Next(0, fiveLetterWords.Length);
            wordleWord = fiveLetterWords[randomNumber];
            row1 = new TextBlock[] { TBRow1Letter1, TBRow1Letter2, TBRow1Letter3, TBRow1Letter4, TBRow1Letter5 };
            row2 = new TextBlock[] { TBRow2Letter1, TBRow2Letter2, TBRow2Letter3, TBRow2Letter4, TBRow2Letter5 };
            row3 = new TextBlock[] { TBRow3Letter1, TBRow3Letter2, TBRow3Letter3, TBRow3Letter4, TBRow3Letter5 };
            row4 = new TextBlock[] { TBRow4Letter1, TBRow4Letter2, TBRow4Letter3, TBRow4Letter4, TBRow4Letter5 };
            row5 = new TextBlock[] { TBRow5Letter1, TBRow5Letter2, TBRow5Letter3, TBRow5Letter4, TBRow5Letter5 };
            row6 = new TextBlock[] { TBRow6Letter1, TBRow6Letter2, TBRow6Letter3, TBRow6Letter4, TBRow6Letter5 };
        }

        private void SubmitGuessButton_Click(object sender, RoutedEventArgs e)
        {
            string input = GuessInput.Text.ToLower();
            if (input.Length == 5)
            {

                if (Array.Exists(fiveLetterWords, element => element == input))
                {
                    Function(input);
                    MessageTB.Text = "";
                }
                else
                {
                    MessageTB.Text = "Word is not in current library";
                }
            }
            else{
                MessageTB.Text = "Not long enough";
            }
        }

        private void UpdateRow(TextBlock[] row, WorldeGeus word)
        {
            for (int i = 0; i < row.Length; i++)
            {
                row[i].Text = word.Word[i].ToString();
                var border = (Border)row[i].Parent;

                switch (word.statusArray[i])
                {
                    case 1:
                        border.BorderBrush = new SolidColorBrush(Colors.Green);
                        break;
                    case 2:
                        border.BorderBrush = new SolidColorBrush(Colors.Orange);
                        break;
                    case 3:
                        border.BorderBrush = new SolidColorBrush(Colors.Red);
                        break;
                    case 0:
                        border.BorderBrush = new SolidColorBrush(Colors.Gray);
                        break;
                }
            }
        }
        private void Function( string input)
        {
            WorldeGeus test = new WorldeGeus();

            int i = 0;
            test.Word = input;
            foreach (char letter in input)
            {
                if (wordleWord[i] == letter)
                {
                    test.statusArray[i] = 1;

                }
                else if (wordleWord.Contains(letter))
                {
                    test.statusArray[i] = 2;
                }
                else
                {
                    test.statusArray[i] = 3;
                }
                i++;
            }

            switch (TryCount)
            {
                case 1:
                    UpdateRow(row1, test);
                    break;
                case 2:
                    UpdateRow(row2, test);
                    break;
                case 3:
                    UpdateRow(row3, test);
                    break;
                case 4:
                    UpdateRow(row4, test);
                    break;
                case 5:
                    UpdateRow(row5, test);
                    break;
                case 6:
                    UpdateRow(row6, test);
                    break;
            }
            TryCount++;
        }

        private void RestartButton_Click(object sender, RoutedEventArgs e)
        {
            WorldeGeus resetWordle = new WorldeGeus
            {
                Word = "     ", // Five spaces to reset the text
                statusArray = new int[5] // Array of 0s to reset the status
            };
            for (int i = 1; i <= 6; i++)
            {
                switch (i)
                {
                    case 1:
                        UpdateRow(row1, resetWordle);
                        break;
                    case 2:
                        UpdateRow(row2, resetWordle);
                        break;
                    case 3:
                        UpdateRow(row3, resetWordle);
                        break;
                    case 4:
                        UpdateRow(row4, resetWordle);
                        break;
                    case 5:
                        UpdateRow(row5, resetWordle);
                        break;
                    case 6:
                        UpdateRow(row6, resetWordle);
                        break;
                }
            }
            TryCount = 1;
            Random random = new Random();
            int randomNumber = random.Next(0, fiveLetterWords.Length);
            wordleWord = fiveLetterWords[randomNumber];
        }
    }
    
}
