library('readxl')
library('writexl')
library('plyr')
library('dplyr')
library('rJava')
library('KoNLP')
library('tidytext')
library('stringr')
library('widyr')
library('ggplot2')
library('wordcloud2')
library('extrafont')
font_import()
loadfonts(device='win')

data <- read_excel('C:/Users/olivi/Rdataframe/GreenHydrogen_Dataset.xlsx')

head(data)
summary(data)
str(data)
glimpse(data)

## 1.텍스트 칼럼 토크나이징 (+ 품사 태깅)
text_pos <- data %>% 
  unnest_tokens(pos, TEXT, SimplePos09)


## 2.명사 데이터 전처리 (불용어 필터링)
# 영어 명사 추출
text_pos %>%
  filter(str_detect(pos, '/f')) %>% # 명사만 남기기
  mutate(pos_clean = str_remove(pos, "/.*$")) -> eng_pos
# 영어 불용어 필터링
eng_rm <- c('and', 'the', 'for', 'from', 'with', 'where',
            'least', 'with', 'one', 'that', 'are', 'comprg', 'havg', 
            'which', 'comprg', 'meod', 'said', 'havg', '개질기', 
            'vol', 'bog', 'lng', '화합물', 'comprising', 'includes',
            'first', 'second', 'through', 'such', 'than', 'including',
            'between', 'having', 'into', 'comprises', 'being')
eng_df <- as.data.frame(sapply(eng_pos, function(x)
  gsub(paste(eng_rm, collapse='|'), '', x)))

eng_df %>% 
  filter(!str_detect(pos_clean, '[0-9가-힣]+')) %>% # 숫자,한글 2차 제거
  filter(nchar(pos_clean) > 2) -> eng_df # 글자수 3개 이상

# 한글 명사 추출
text_pos %>%
  filter(str_detect(pos, '/n')) %>% # 명사만 남기기
  mutate(pos_clean = str_remove(pos, "/.*$")) %>% # 특수문자 제거
  filter(!str_detect(pos_clean, '[a-zA-Z0-9]+')) %>% # 숫자, 영어 제거
  filter(nchar(pos_clean) > 1) -> kor_pos # 글자수 2개 이상
# 한글 불용어 필터링
kor_rm <- c('상기')
kor_df <- as.data.frame(sapply(kor_pos, function(x)
  gsub(paste(kor_rm, collapse='|'), '', x)))


## 3.소분류별 키워드 TF-IDF 계산
# 영어
eng_df %>% 
  group_by(`소분류`, 출원인대표명화) %>% 
  count(pos_clean, sort=T) %>% 
  bind_tf_idf(pos_clean, 출원인대표명화, n) %>% 
  arrange(desc(`소분류`), desc(tf_idf)) -> eng_ti
# 한글 3차 제거, 글자수 제한
eng_ti$pos_clean <- str_replace_all(eng_ti$pos_clean, '[^abcdefghijklmnopqrstuvwxyz]', '')
eng_ti %>% filter(nchar(pos_clean) > 3) -> eng_ti

# 한글
kor_df %>% 
  group_by(`소분류`, 출원인대표명화) %>% 
  count(pos_clean, sort=T) %>% 
  bind_tf_idf(pos_clean, 출원인대표명화, n) %>% 
  arrange(desc(`소분류`), desc(tf_idf)) -> kor_ti
# 글자수 제한
kor_ti %>% filter(nchar(pos_clean) > 1) -> kor_ti

# 5. 언어 칼럼 추가, 상위 200개 필터링
eng_ti %>% 
  select(pos_clean, tf_idf) %>% 
  arrange(desc(tf_idf)) %>% 
  head(200) -> eng_top
kor_ti %>% 
  select(pos_clean, tf_idf) %>% 
  arrange(desc(tf_idf)) %>% 
  head(206) -> kor_top
kor_top <- kor_top[-c(1:6),]

eng_top['언어'] <- '영어'
kor_top['언어'] <- '한글'

## 6.최종 유니온 테이블 생성
result <- rbind(eng_top, kor_top)

write_xlsx(result, 'C:/Users/olivi/Rdataframe/GreenHydrogen_keyword.xlsx')
