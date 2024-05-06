import re
import spacy
import numpy as np
from nltk.corpus import words
from nltk.corpus import stopwords

contractions = {
    "ain't": "is not",
    "aren't": "are not",
    "can't": "cannot",
    "can't've": "cannot have",
    "'cause": "because",
    "could've": "could have",
    "couldn't": "could not",
    "couldn't've": "could not have",
    "didn't": "did not",
    "doesn't": "does not",
    "don't": "do not",
    "hadn't": "had not",
    "hadn't've": "had not have",
    "hasn't": "has not",
    "haven't": "have not",
    "he'd": "he would",
    "he'd've": "he would have",
    "he'll": "he will",
    "he'll've": "he will have",
    "he's": "he is",
    "how'd": "how did",
    "how'd'y": "how do you",
    "how'll": "how will",
    "how's": "how is",
    "i'd": "i would",
    "i'd've": "i would have",
    "i'll": "i will",
    "i'll've": "i will have",
    "i'm": "i am",
    "i've": "i have",
    "isn't": "is not",
    "it'd": "it would",
    "it'd've": "it would have",
    "it'll": "it will",
    "it'll've": "it will have",
    "it's": "it is",
    "let's": "let us",
    "ma'am": "madam",
    "mayn't": "may not",
    "might've": "might have",
    "mightn't": "might not",
    "mightn't've": "might not have",
    "must've": "must have",
    "mustn't": "must not",
    "mustn't've": "must not have",
    "needn't": "need not",
    "needn't've": "need not have",
    "o'clock": "of the clock",
    "oughtn't": "ought not",
    "oughtn't've": "ought not have",
    "shan't": "shall not",
    "sha'n't": "shall not",
    "shan't've": "shall not have",
    "she'd": "she would",
    "she'd've": "she would have",
    "she'll": "she will",
    "she'll've": "she will have",
    "she's": "she is",
    "should've": "should have",
    "shouldn't": "should not",
    "shouldn't've": "should not have",
    "so've": "so have",
    "so's": "so as",
    "that'd": "that would",
    "that'd've": "that would have",
    "that's": "that is",
    "there'd": "there would",
    "there'd've": "there would have",
    "there's": "there is",
    "they'd": "they would",
    "they'd've": "they would have",
    "they'll": "they will",
    "they'll've": "they will have",
    "they're": "they are",
    "they've": "they have",
    "to've": "to have",
    "wasn't": "was not",
    "we'd": "we would",
    "we'd've": "we would have",
    "we'll": "we will",
    "we'll've": "we will have",
    "we're": "we are",
    "we've": "we have",
    "weren't": "were not",
    "what'll": "what will",
    "what'll've": "what will have",
    "what're": "what are",
    "what's": "what is",
    "what've": "what have",
    "when's": "when is",
    "when've": "when have",
    "where'd": "where did",
    "where's": "where is",
    "where've": "where have",
    "who'll": "who will",
    "who'll've": "who will have",
    "who's": "who is",
    "who've": "who have",
    "why's": "why is",
    "why've": "why have",
    "will've": "will have",
    "won't": "will not",
    "won't've": "will not have",
    "would've": "would have",
    "wouldn't": "would not",
    "wouldn't've": "would not have",
    "y'all": "you all",
    "y'all'd": "you all would",
    "y'all'd've": "you all would have",
    "y'all're": "you all are",
    "y'all've": "you all have",
    "you'd": "you would",
    "you'd've": "you would have",
    "you'll": "you will",
    "you'll've": "you will have",
    "you're": "you are",
    "you've": "you have"
}

def clean(text):
    """ Function that returns text after cleaning it. 
        @param text: Input text.
        @return text: Clean text.
    """
    # Make lowercase.
    text = text.lower().strip()

    # Expand contractions.
    for key, val in contractions.items():
        text = re.sub(key, val, text)

    # Discard "'s" indicating belonging.
    text = re.sub("'s","",text)

    # Remove all special characters other than
    # letters, numbers and "%".
    text = re.sub("[^a-z0-9%]", " ", text)

    # Discard extra spaces.
    text = re.sub('\\s\\s+', " ", text).strip()
    
    # If final text after cleaning is empty, return "".
    if re.search("[a-z0-9%]", text): 
        return text
    else: return ""

# Remove stop words.
stop_words = set(stopwords.words('english'))
stop_words.add("wine") # Adding wine as stop word just to remove it from the description.
# This data set is about wine. So the word "wine" does not convey much information.
def remove_stopwords(text):
    """
    Function that removes stopwords from a given list of word
    and returns this new possibly smaller list.
    @param text: The list of words from which to remove stopwords.
    @return text: Text without stop words.
    """
    return " ".join([word for word in text.split(" ") if not word in stop_words])

# Filter text so as to only contain English words as much as possible.
english_words = set(words.words())
def eng_filter(text):
    """
    Filters out non-english words.
    @param text: Input text.
    @return: Sentence with english words only.
    """
    filtered_text = ' '.join(
        word for word in text.split() 
        if word.lower() in english_words
    )
    return filtered_text

spacy_nlp_en = spacy.load("en_core_web_lg")
def lemmatize(text):
    """ 
    Lemmatizes a given text string.
    @param text_list: List of strings.
    @return: Text containing lemmatized words.
    """
    doc = spacy_nlp_en(text)
    return " ".join([token.lemma_ for token in doc])

character_points_map = {
    "sweetness": {
        1: ["sweet", "sugar", "dessert", "fruit", 
            "honey", "sacchar", "lush", "ripe"], 
        -1: ["dry", "crisp", "tart", "austere", "sharp", 
             "astringent", "tangy", "brac", "acid", "sour"]
    },
    "acidity": {
        1: ["acidic", "tart", "tang", "zest", "sharp", 
            "crisp", "brac", "bright", "zing", "live"], 
        -1: ["sweet", "mellow", "smooth", "velvet", "round", 
             "soft", "lush", "creamy", "full", "rich"]
    },
    "tannin": {
        1: ["tannin", "astringent", "grip", "structure", "firm", "texture"],
        -1: ["smooth", "soft", "silk", "velvet", "mellow", "round"]
    },
    "alcohol": {
        1: ["alcohol", "drunk", "spirit", "booze", "bubbl", 
            "liquor", "hooch", "fort", "hot", "warm", "spic", "full"],
        -1: ["dry", "crisp", "tart", "austere", "sharp", 
             "acid", "bitter", "astringent", "tangy", "tannic"]
    },
    "body": {
        1: ["bod", "struct", "full", "weight", "mouthfeel", 
            "texture", "rich", "depth", "deep", "substance", "visc"],
        -1: ["light", "thin", "water", "delicate", "weak", 
             "insubstantial", "flimsy", "dilute", "lack", "air"]
    }
}

def get_character_scores(text):
    """ 
    Given some text returns corresponding wine
    characteristics (sweetness, acidity, tanning, alcohol, body)
    related scores based on words in the text.
    @param text: Text to analyze.
    @return: Dictionary of min max normalized characteristic scores
             such that 0.5 => neutral, 0 => opposite characteristic,
             and 1 => most positive indicator of characteristic.
    """
    text_vec = text.split()
    character_score = {"sweetness":0, "acidity":0, "tannin":0, "alcohol":0, "body":0}
    norm_num = 0
    for i in range(0, len(text_vec), 3): # Loop through text, 3 words at a time.
        norm_num += 1
        text_segment = " ".join(text_vec[i:i+3])
        for character in character_points_map.keys():
            score = 0
            for char_text in character_points_map[character][1]:
                if char_text in text_segment: score += 1
            for opp_char_text in character_points_map[character][-1]:
                if opp_char_text in text_segment: score -= 1
            character_score[character] += score
    for k, v in character_score.items():
        character_score[k] = np.round((v-(-1*norm_num))/(norm_num-(-1*norm_num)), 3)
    return character_score