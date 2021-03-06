Day 6: Custom Customs
==============
<div align="justify">

### Part One
As your flight approaches the regional airport where you'll switch to a much larger plane, customs declaration forms are distributed to the passengers.

The form asks a series of 26 yes-or-no questions marked a through z. All you need to do is identify the questions for which anyone in your group answers "yes". Since your group is just you, this doesn't take very long.

However, the person sitting next to you seems to be experiencing a language barrier and asks if you can help. For each of the people in their group, you write down the questions for which they answer "yes", one per line. For example:

abcx
abcy
abcz
In this group, there are 6 questions to which anyone answered "yes": a, b, c, x, y, and z. (Duplicate answers to the same question don't count extra; each question counts at most once.)

Another group asks for your help, then another, and eventually you've collected answers from every group on the plane (your puzzle input). Each group's answers are separated by a blank line, and within each group, each person's answers are on a single line. For example:

```
abc

a
b
c

ab
ac

a
a
a
a

b
```

This list represents answers from five groups:

The first group contains one person who answered "yes" to 3 questions: a, b, and c.
The second group contains three people; combined, they answered "yes" to 3 questions: a, b, and c.
The third group contains two people; combined, they answered "yes" to 3 questions: a, b, and c.
The fourth group contains four people; combined, they answered "yes" to only 1 question, a.
The last group contains one person who answered "yes" to only 1 question, b.
In this example, the sum of these counts is 3 + 3 + 3 + 1 + 1 = 11.

For each group, count the number of questions to which anyone answered "yes". What is the sum of those counts?

Your puzzle answer was 6768.

R scripts:

```{r}
input <- read.table("day6/input.txt", quote="\"",blank.lines.skip = F)
input <- as.vector(t(input))
input[input==""] <- 'cuthere'
check <- paste0(input,collapse = '')
check <- unlist(strsplit(check,split = 'cuthere'))

n <- sapply(check, function(x){
  x <- unlist(strsplit(x,""))
  length(unique(x))
})

sum(n)
```

Python scripts:
```
import os

work_dir = r'C:\Project\R project\Project2020\AoC2020\AoC2020'
work_dir = os.path.normpath(work_dir)
os.chdir(work_dir)

## part1
with open('day6/input.txt') as f:
    content = f.readlines()

combined =''
for i in content:
    combined+=str(i)

divided = combined.split('\n\n')
divided = [x.replace('\n','') for x in divided]

splited = [list(set([l for l in x])) for x in divided]
n = [len(x) for x in splited]
sum(n)


```


### Part Two
As you finish the last group's customs declaration, you notice that you misread one word in the instructions:

You don't need to identify the questions to which anyone answered "yes"; you need to identify the questions to which everyone answered "yes"!

Using the same example as above:

```
abc

a
b
c

ab
ac

a
a
a
a

b
```

This list represents answers from five groups:

```
In the first group, everyone (all 1 person) answered "yes" to 3 questions: a, b, and c.
In the second group, there is no question to which everyone answered "yes".
In the third group, everyone answered yes to only 1 question, a. Since some people did not answer "yes" to b or c, they don't count.
In the fourth group, everyone answered yes to only 1 question, a.
In the fifth group, everyone (all 1 person) answered "yes" to 1 question, b.
In this example, the sum of these counts is 3 + 0 + 1 + 1 + 1 = 6.
```

For each group, count the number of questions to which everyone answered "yes". What is the sum of those counts?

Your puzzle answer was 3489.

R scripts

```{r}
input <- read.table("day6/input.txt", quote="\"",blank.lines.skip = F)
input <- as.vector(t(input))
input[input==""] <- 'cuthere'
check <- paste0(input,collapse = ';')
check <- unlist(strsplit(check,split = 'cuthere'))

n <- sapply(check, function(x){
  message(x)
  y <- unlist(strsplit(x = x,';'))
  y <- y[y!=""]
  np <- length(y)
  z <- paste0(y,"")
  z <- z[z!=""]
  w <- unlist(strsplit(z,""))
  valid <- table(w)[table(w)==np]
  length(valid)
})

sum(n)

```

Python scripts

```
divided1 = [x.replace('\n','') for x in divided0]
# divided2 = [list(x.strip('\n').split('\n')) for x in divided0]
divided2 = [list(filter(None,x.split('\n'))) for x in divided0]
splited = [[l for l in x] for x in divided1]
people = [len(x) for x in divided2]

result = []
for i,ele in enumerate(splited):
    all_dic = {x:ele.count(x) for x in ele}
    valid = [k for k,v in all_dic.items() if v == people[i]]
    result.append(len(valid))
    
sum(result)
```


</div>