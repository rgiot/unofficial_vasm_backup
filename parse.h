/* parse.h - global parser support functions */
/* (c) in 2009-2013 by Volker Barthelmann and Frank Wille */

#ifndef PARSE_H
#define PARSE_H 

/* defines */
#define MAXLINELENGTH 4096
#define MAXMACPARAMS 36

struct macarg {
  struct macarg *argnext;
  char argname[1];  /* extended to real argument length + '\0' */
};

struct macro {
  struct macro *next;
  char *name;
  char *text;
  size_t size;
  struct macarg *argnames;
};

typedef struct structfield {
  struct structfield *next; 
  int bitsize; 
  int isarray; 
  union {
    int defval;
    char *defarray;
  } content;
  char *name;
} structfield;

typedef struct structure {
  struct structure *next; 
  structfield *field;
  char *name;
  char *text;
  size_t size;
  size_t length;
} structure;

/* allow to convert a datatype to a number of bits */
struct datalen {
  unsigned char bitlen;
  char *name;
};

struct namelen {
  unsigned char len;
  char *name;
};


/* global variables */
extern int esc_sequences,nocase_macros,maxmacparams,namedmacparams;
extern struct datalen *structure_type_lookup;

/* functions */
char *escape(char *,char *);
char *parse_name(char **);
char *skip_identifier(char *);
char *parse_identifier(char **);
char *skip_string(char *,char,taddr *);
dblock *parse_string(char **,char,int);
int check_indir(char *,char *);
void include_binary_file(char *,long,unsigned long);
void new_repeat(int,struct namelen *,struct namelen *);
macro *new_macro(char *,struct namelen *,char *);
int execute_macro(char *,int,char **,int *,int,char *,int);
int leave_macro(void);
structure *new_structure(char *,struct namelen *,char *);
char *read_next_line(void);
int init_parse(void);

#endif /* PARSE_H */
