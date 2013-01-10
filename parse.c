/* parse.c - global parser support functions */
/* (c) in 2009-2013 by Volker Barthelmann and Frank Wille */

#include "vasm.h"

/* parse tuning */
int esc_sequences = 1;  /* handle escape sequences */
int nocase_macros = 0;  /* macro names are case-insensitive */
int maxmacparams = 10;  /* 10: \0..\9, 36: \0..\9+\a..\z */
int namedmacparams = 0; /* allow named macro arguments, like \argname */
int nocase_structure = 0; /* structure names are case-insensitive */

/* Has to be initialized by syntax module, when structures are supported: */
struct datalen *structure_type_lookup = NULL;

#ifndef MACROHTABSIZE
#define MACROHTABSIZE 0x800
#endif
static hashtable *macrohash;

#ifndef STRUCTHTABSIZE
#define STRUCTHTABSIZE 0x400
#endif
static hashtable *structhash;

static macro *first_macro;
static macro *cur_macro;
static structure *first_struct;
static structure *cur_struct;
static struct namelen *enddir_list;
static size_t enddir_minlen;
static struct namelen *reptdir_list;
static int rept_cnt = -1;
static char *rept_start;
#ifdef CARGSYM
static expr *carg1;
#endif

#define IDSTACKSIZE 100
static unsigned long id_stack[IDSTACKSIZE];
static int id_stack_index;


char *escape(char *s,char *code)
{
  if (*s++ != '\\')
    ierror(0);

  if (!esc_sequences) {
    *code='\\';
    return s;
  }

  switch (*s) {
    case 'b':
      *code='\b';
      return s+1;
    case 'f':
      *code='\f';
      return s+1;
    case 'n':
      *code='\n';
      return s+1;
    case 'r':
      *code='\r';
      return s+1;
    case 't':
      *code='\t';
      return s+1;
    case '\\':
      *code='\\';
      return s+1;
    case '\"':
      *code='\"';
      return s+1;
    case '\'':
      *code='\'';
      return s+1;
    case 'e':
      *code=27;
      return s+1;
    case '0': case '1': case '2': case '3': 
    case '4': case '5': case '6': case '7':
      *code = 0;
      while (*s>='0' && *s<='7') {
        *code = *code*8 + *s-'0';
        s++;
      }
      return s;
    case 'x': case 'X':
      *code=0;
      s++;
      while ((*s>='0' && *s<='9') ||
             (*s>='a' && *s<='f') || (*s>='A' && *s<='F')) {
        if (*s>='0' && *s<='9')
          *code = *code*16 + *s-'0';
        else if (*s>='a' && *s<='f')
          *code = *code*16 + *s-'a' + 10;
        else
          *code = *code*16 + *s -'A' + 10;
        s++;
      }    
      return s;
    default:
      general_error(35,*s);
      return s;
  }
}


char *parse_name(char **start)
/* parses a quoted or unquoted name-string and returns a pointer to it */
{
  char *s = *start;
  char c,*name;

  if (*s=='\"' || *s=='\'') {
    c = *s++;
    name = s;
    while (*s && *s!=c)
      s++;
    name = cnvstr(name,s-name);
    if (*s)
      s = skip(s+1);
  }
#ifdef VASM_CPU_M68K
  else if (*s=='<') {
    s++;
    name = s;
    while (*s && *s!='>')
      s++;
    name = cnvstr(name,s-name);
    if (*s)
      s = skip(s+1);
  }
#endif
  else {
    name = s;
    while (*s && !isspace((unsigned char)*s) && *s!=',' && *s!=commentchar)
      s++;
    if (s != name) {
      name = cnvstr(name,s-name);
      s = skip(s);
    }
    else
      name = NULL;  /* nothing read */
  }
  *start = s;
  return name;
}


static char *skip_eol(char *s,char *e)
{
  while (s<e && *s!='\0' && *s!='\n' && *s!='\r')
    s++;
  return s;
}


char *skip_identifier(char *s)
{
  char *name = s;

  if (ISIDSTART(*s)) {
    s++;
    while (ISIDCHAR(*s))
      s++;
    return CHKIDEND(name,s);
  }
  return NULL;
}


char *parse_identifier(char **s)
{
  char *name = *s;
  char *endname;

  if (endname = skip_identifier(*s)) {
    *s = endname;
    return cnvstr(name,endname-name);
  }
  return NULL;
}


char *skip_string(char *s,char delim,taddr *size)
/* skip a string, optionally store the size in bytes in size, when not NULL */
{
  taddr n = 0;
  char c;

  if (*s != delim)
    general_error(6,delim);  /* " expected */
  else
    s++;

  while (*s) {
    if (*s == '\\') {
      s = escape(s,&c);
    }
    else {
      if (*s++ == delim) {
        if (*s == delim)
          s++;  /* allow """" to be recognized as " */
        else
          break;
      }
    }
    n++;
  }

  if (*(s-1) != delim)
    general_error(6,delim);  /* " expected */
  if (size)
    *size = n;
  return s;
}


dblock *parse_string(char **str,char delim,int width)
{
  taddr size;
  dblock *db;
  char *p,c;
  char *s = *str;

  if (width & 7)
    ierror(0);
  width >>= 3;

  /* how many bytes do we need for the string? */
  skip_string(s,delim,&size);
  if (size == 1)
    return NULL; /* it's just one char, so use eval_expr() on it */

  db = new_dblock();
  db->size = size * width;
  db->data = db->size ? mymalloc(db->size) : NULL;

  /* now copy the string for real into the dblock */
  if (*s == delim)
    s++;
  p = db->data;

  while (*s) {
    if (*s == '\\') {
      s = escape(s,&c);
    }
    else {
      c = *s++;
      if (c == delim) {
        if (*s == delim)
          s++;  /* allow """" to be recognized as " */
        else
          break;
      }
    }
    setval(BIGENDIAN,p,width,(unsigned char)c);
    p += width;
  }

  *str = s;
  return db;
}


int check_indir(char *p,char *q)
/* returns true when the whole sequence between p and q starts and ends with */
/* parentheses and there are no unbalanced parentheses within */
{
  char c;
  int n;

  p = skip(p);
  if (*p++ != '(')
    return 0;

  n = 1;
  while (n>0 && p<q) {
    c = *p++;
    if (c == '(')
      n++;
    else if (c == ')')
      n--;
  }
  if (p < q)
    p = skip(p);

  return n==0 && p>=q;
}


void include_binary_file(char *inname,long nbskip,unsigned long nbkeep)
/* locate a binary file and convert into a data atom */
{
  char *filename;
  FILE *f;

  filename = convert_path(inname);
  if (f = locate_file(filename,"rb")) {
    taddr size = filesize(f);

    if (size > 0) {
      if (nbskip>=0 && nbskip<=size) {
        dblock *db = new_dblock();

        if (nbkeep > (unsigned long)(size - nbskip) || nbkeep==0)
          db->size = size - nbskip;
        else
          db->size = nbkeep;

        db->data = mymalloc(size);
        if (nbskip > 0)
          fseek(f,nbskip,SEEK_SET);

        fread(db->data,1,db->size,f);
        add_atom(0,new_data_atom(db,1));
      }
      else
        general_error(46);  /* bad file-offset argument */
    }
    fclose(f);
  }
  myfree(filename);
}


int get_bitsize_of_type(char *s, int len)
/* Return the number of bits required to store the parameter of the structure */
{
  struct datalen *list = structure_type_lookup;

  while (list->name != 0) {
    if (!strnicmp(s, list->name, len))
      return list->bitlen;
    list++;
  }
  return -1;
}


static struct namelen *dirlist_match(char *s,char *e,struct namelen *list)
/* check if a directive from the list matches the current source location */
{
  size_t len;
  size_t maxlen = e - s;

  while (len = list->len) {
    if (len <= maxlen) {
      if (!strnicmp(s,list->name,len) && isspace((unsigned char)*(s + len)))
        return list;
    }
    list++;
  }
  return NULL;
}


static size_t dirlist_minlen(struct namelen *list)
{
  size_t minlen;

  if (list == NULL)
    ierror(0);
  for (minlen=list->len; list->len; list++) {
    if (list->len < minlen)
      minlen = list->len;
  }
  return minlen;
}


void new_repeat(int rcnt,struct namelen *reptlist,struct namelen *endrlist)
{
  if (cur_macro==NULL && cur_src!=NULL && enddir_list==NULL) {
    enddir_list = endrlist;
    enddir_minlen = dirlist_minlen(endrlist);
    reptdir_list = reptlist;
    rept_cnt = rcnt;
    rept_start = cur_src->srcptr;
  }
  else
    ierror(0);
}


static int find_param_name(char *name,int *param_len)
{
  struct macarg *ma;
  int idx,len;

  len = skip_identifier(name) - name;
  if (ma = cur_src->param_names) {
    idx = 1;
    while (ma) {
      /* @@@ case-sensitive comparison? */
      if (len==strlen(ma->argname) && strncmp(ma->argname,name,len)==0) {
        *param_len = len;
        return idx;
      }
      ma = ma->argnext;
      idx++;
    }
  }
  return -1;
}


static void named_macro_arg(macro *m,char *start,char *end)
{
  struct macarg *lastarg,*newarg;
  int cnt = 1;

  /* count arguments */
  if (lastarg = m->argnames) {
    cnt++;
    while (lastarg->argnext) {
      lastarg = lastarg->argnext;
      cnt++;
    }
  }
  if (cnt >= MAXMACPARAMS)
    general_error(27,MAXMACPARAMS-1);  /* number of args exceeded */

  cnt = end - start;
  newarg = mymalloc(sizeof(struct macarg) + cnt);
  newarg->argnext = NULL;
  memcpy(newarg->argname,start,cnt);
  newarg->argname[cnt] = '\0';
  if (lastarg)
    lastarg->argnext = newarg;
  else
    m->argnames = newarg;
}


macro *new_macro(char *name,struct namelen *endmlist,char *args)
{
  macro *m = NULL;

  if (cur_macro==NULL && cur_src!=NULL && enddir_list==NULL) {
    m = mymalloc(sizeof(macro));
    m->name = mystrdup(name);
    if (nocase_macros)
      strtolower(m->name);
    m->text = cur_src->srcptr;
    m->argnames = NULL;
    cur_macro = m;
    enddir_list = endmlist;
    enddir_minlen = dirlist_minlen(endmlist);
    rept_cnt = -1;
    rept_start = NULL;

    if (args) {
      /* named arguments have been given */
      char *end;

      args = skip(args);
      while (*args != '\0') {
        if (*args == '\\')
          args++;
        end = skip_identifier(args);
        if (end!=NULL && end-args!=0) {
          /* add another argument name */
          named_macro_arg(m,args,end);
          args = end;
        }
        else
          general_error(42);  /* illegal macro argument */
        args = skip(args);
        if (*args == ',')
          args = skip(args+1);
      }
    }
  }
  else
    ierror(0);

  return m;
}


/* check if 'name' is a known macro, then execute macro context */
int execute_macro(char *name,int name_len,char **q,int *q_len,int nq,
                  char *s,int clev)
{
  hashdata data;
  macro *m;
  source *src;
  int n;
#ifdef CARGSYM
  symbol *carg;
#endif
#if MAX_QUALIFIERS>0
  char *defq[MAX_QUALIFIERS];
  int defq_len[MAX_QUALIFIERS];
#endif

  if (nocase_macros) {
    if (!find_namelen_nc(macrohash,name,name_len,&data))
      return 0;
  }
  else {
    if (!find_namelen(macrohash,name,name_len,&data))
      return 0;
  }

  /* it's a macro: read arguments and execute it */
  m = data.ptr;
  src = new_source(m->name,m->text,m->size);

#if MAX_QUALIFIERS>0
  /* put first qualifier into argument \0 */
  /* FIXME: what about multiple qualifiers? */
  if (nq) {
    src->param[0] = q[0];
    src->param_len[0] = q_len[0];
  }
  else if (nq = set_default_qualifiers(defq,defq_len)) {
    src->param[0] = defq[0];
    src->param_len[0] = defq_len[0];
  }
#endif
    
  /* read macro arguments from operand field */
  for (n=0,s=skip(s); *s!='\0' && *s!=commentchar && n<maxmacparams; ) {
    n++;

    if (*s=='\"' || *s=='\'') {
      /* macro argument in quotes */
      char dummy,c;

      src->param[n] = s;
      c = *s++;
      while (*s != '\0') {
        if (*s=='\\' && *(s+1)!='\0') {
          s = escape(s,&dummy);
        }
        else {
          if (*s++ == c) {
            if (*s == c)
              s++;  /* allow """" to be recognized as " */
            else
              break;
          }
        }
      }
      src->param_len[n] = s - src->param[n];
    }

    else if (*s == '<') {
      /* macro argument enclosed in < ... > */
      src->param[n] = ++s;
      while (*s != '\0') {
        if (*s =='>') {
          if (*(s+1) == '>') {
            /* convert ">>" into a single ">" */
            char *p;

            for (p=s+1; *p!='\0'; p++)
              *(p-1) = *p;
            *(p-1) = '\0';
          }
          else
            break;
        }
        s++;
      }
      src->param_len[n] = s - src->param[n];
      if (*s == '>')
        s++;
    }

    else {
      src->param[n] = s;
      s = skip_operand(s);
      while (isspace((unsigned char)*(s-1)))  /* cut trailing blanks */
        s--;
      src->param_len[n] = s - src->param[n];
    }

    s = skip(s);
    if (*s != ',')
      break;
    else
      s = skip(s+1);
  }

#ifdef CARGSYM
  /* reset the CARG symbol to 1, selecting the first macro parameter */
  carg = internal_abs(CARGSYM);
  cur_src->cargexp = carg->expr;  /* remember last CARG expression */
  carg->expr = carg1;
#endif
  eol(s);
  if (n >= maxmacparams) {
    general_error(27,maxmacparams-1);  /* number of args exceeded */
    n = maxmacparams - 1;
  }
  src->num_params = n;      /* >=0 indicates macro source */
  src->param_names = m->argnames;
  src->cond_level = clev;   /* remember level of conditional nesting */
  cur_src = src;            /* execute! */
  return 1;
}


int leave_macro(void)
{
  if (cur_src->num_params >= 0) {
    /* move srcptr to end of macro-source, effectively leaving the macro */
    cur_src->srcptr = cur_src->text + cur_src->size;
    return cur_src->cond_level;
  }
  general_error(36);  /* no current macro to exit */
  return -1;
}


structure *new_structure(char *name,struct namelen *endslist,char *args)
{
  structure *st = NULL;

  /* impossible to declare a structure inside a macro */
  if (cur_macro != NULL)
    ierror(0);

  if (cur_struct==NULL && cur_src!=NULL && enddir_list==NULL) {
    st = mymalloc(sizeof(structure));
    st->name = mystrdup(name);
    if (nocase_structure)
      strtolower(st->name);
    st->text = cur_src->srcptr;
    cur_struct = st;
    enddir_list = endslist;
    enddir_minlen = dirlist_minlen(endslist);
    rept_cnt = -1;
    rept_start = NULL;

    if (args) {
      /* TODO manage the args */
    }
  }  
  else
    ierror(0);

  return st;
}


/* check if 'name' is a known structure, make atoms from structure content */
int execute_struct(char *name,int name_len,char **q,int *q_len,int nq,
                   char *s,int clev)
{
  hashdata data;
  structure *st;
  structfield *field;
  atom *a;

  /* verify if structure exists */
  if (nocase_structure) {
    if (!find_namelen_nc(structhash,name,name_len,&data))
      return 0;
  }
  else {
    if (!find_namelen(structhash,name,name_len,&data))
      return 0;
  }

  st = data.ptr;
  field = st->field;
  s = skip(s);

  /* output fields value */
  while (field) {

    if (!field->isarray) {
      /* normal case */
      if (*s=='\0' || *s==',') {
        /* no more args, or empty arg: use default value */
        a = new_space_atom(number_expr(1),field->bitsize/8,
                           number_expr(field->content.defval));
      }
      else {
        operand *op = new_operand();
        char *opstart;

        opstart = s;
        s = skip_operand(s);
        if (parse_operand(opstart,s-opstart,op,DATA_OPERAND(field->bitsize))) {
          a = new_datadef_atom(field->bitsize,op);
        }
        else {
          general_error(24);  /* bad operand */
          return;
        }
      }
    }
    else {
      /* array information */
      int i;
      dblock *db;

      if (*s=='\0' || *s==',') {  /* no more args, or empty arg*/
        db       = new_dblock();
        db->data = field->content.defarray;
        db->size = field->bitsize/8;
      }
      else {   
        /* Read the definition in a row */
        s = skip(s);
        
        if (*s=='\"' || *s=='\'') {
          db = parse_string(&s,*s,8);
          if (db==NULL || db->size != field->bitsize/8) {
            general_error(51);
            return;
          }
        }
        else {
          general_error(50);
          return;
        }
      }
      a = new_data_atom(db,1);
    }

    add_atom(0,a);

    field = field->next;
    s = skip(s);
    if (*s == ',') {
      s++;
      s = skip(s);
      /* TODO verify there are no more parameters than expected */
    }

  }
  return 1;
}


static void start_repeat(char *rept_end)
{
  char buf[MAXPATHLEN];
  source *src;
  int i;

  reptdir_list = NULL;
  if (rept_cnt<0 || cur_src==NULL || strlen(cur_src->name) + 24 >= MAXPATHLEN)
    ierror(0);

  if (rept_cnt > 0) {
    sprintf(buf,"REPEAT:%s:line %d",cur_src->name,cur_src->line);
    src = new_source(mystrdup(buf),rept_start,rept_end-rept_start);
    src->repeat = (unsigned long)rept_cnt;
#ifdef REPTNSYM
    src->reptn = 0;
    set_internal_abs(REPTNSYM,0);
#endif

    if (cur_src->num_params > 0) {
      /* repetition in a macro: get parameters */
      src->num_params = cur_src->num_params;
      for (i=0; i<=src->num_params; i++) {
        src->param[i] = cur_src->param[i];
        src->param_len[i] = cur_src->param_len[i];
      }
      src->param_names = cur_src->param_names;
    }
    cur_src = src;  /* repeat it */
  }
}


static void add_macro(void)
{
  if (cur_macro!=NULL && cur_src!=NULL) {
    hashdata data;

    cur_macro->size = cur_src->srcptr - cur_macro->text;
    cur_macro->next = first_macro;
    first_macro = cur_macro;
    data.ptr = cur_macro;
    add_hashentry(macrohash,cur_macro->name,data);
    cur_macro = NULL;
  }
  else
    ierror(0);
}


static int copy_macro_param(int n,char *d,int len)
/* copy macro parameter n to line buffer */
{
  int i = 0;

  if (n<=cur_src->num_params && n<maxmacparams) {
    for (; i<cur_src->param_len[n] && len>0; i++,len--)
      *d++ = cur_src->param[n][i];
  }
  return i;
}


#ifdef CARGSYM
static int copy_macro_carg(int inc,char *d,int len)
/* copy macro parameter #CARG to line buffer, increment or decrement CARG */
{
  symbol *carg = internal_abs(CARGSYM);
  int nc;

  if (carg->type != EXPRESSION)
    return 0;
  simplify_expr(carg->expr);
  if (carg->expr->type != NUM) {
    general_error(30);  /* expression must be a constant */
    return 0;
  }
  nc = copy_macro_param(carg->expr->c.val,d,len);

  if (inc) {
    expr *new = make_expr(inc>0?ADD:SUB,copy_tree(carg->expr),number_expr(1));

    simplify_expr(new);
    carg->expr = new;
  }
  return nc;
}
#endif


static void add_structure(void)
/* Add the structure to the hashtable.
 * Extract the information of the structure and store it.
 */
{
  if (cur_struct!=NULL && cur_src!=NULL) {
    structfield *cur_field, *field;
    symbol *label;
    hashdata data;
    char *inner;
    int base;

    cur_struct->size = cur_src->srcptr - cur_struct->text;
    cur_struct->next = first_struct;
    cur_struct->length = 0;
    first_struct = cur_struct;
    data.ptr = cur_struct;
    add_hashentry(structhash,cur_struct->name,data);

    /* parse the structure content */
    inner = cur_struct->text;
    cur_field = NULL;

    while (inner < cur_src->srcptr) {
      char *id, *type;
      taddr def;
      int bitsize;
      int defscnt;
      int isarray=0;
      char *array_content=NULL;

      /* get name of the variable */
      if (isspace(*inner)) {  /*BUG works only at first line! */
        syntax_error(10);
        return;
      }
      id = parse_identifier(&inner);

      /* verify if label already exists */
      field = cur_struct->field;
      while (field) {
        if (!stricmp(field->name, id)) {
          syntax_error(25, id, cur_struct->name);
          return;
        }
        field = field->next;
      }

      /* get type of the variable. TODO use types defined in syntax module */
      while (*inner==' ' || *inner=='\t')
        inner++;
      type = parse_identifier(&inner);
      bitsize = get_bitsize_of_type(type, type-inner);
      if (bitsize == -1) {
        syntax_error(24, type); /*bitsize unknown */
        return;
      }
      else if (bitsize == 0) {  /* compute the size */
        char *backup;
        int i;

        while (*inner==' ' || *inner=='\t')
          inner++;
        if (*inner=='\r' || *inner=='\n') {
          syntax_error(49, type);
          return;
        }

        /* read the number of elements */
        backup = inner;
        isarray = 1;
        defscnt = parse_constexpr(&inner);
        inner = skip_eol(backup, inner);
        bitsize = 8 * defscnt;

        /* default array contains null */
        array_content = mycalloc(sizeof(char)*defscnt);
      }
      else
        isarray = 0;

      cur_struct->length += bitsize;

      /* Read the optional default value */
      while (*inner==' '|| *inner=='\t')
        inner++;
      if (*inner!='\r' && *inner!='\n') {
        if (!isarray) {  /* normal case */
          char *backup = inner;

          def = parse_constexpr(&inner);
          inner = skip_eol(backup, inner);
          while (*inner=='\r' || *inner=='\n')
            inner++;
        }
        else {
          /* Array field default value can be defined with several
           * strings and defb.
           */
          int current_bytesize = 0;
          dblock *db = NULL;

          while (current_bytesize < defscnt) {
            /* Need to populate all the childdren */
            if (*inner == ',') {
              inner++;
              while (*inner==' ' || *inner=='\t')
                inner++;
            }
            else
              general_error(6,',');  /* , expected */

            if (*inner=='\'' || *inner=='\"') {  /* we read a string */
              db = parse_string(&inner, *inner,8);
              if (db) {
                if (current_bytesize+db->size > defscnt) {
                  general_error(51); /* wrong size */
                  return;
                }
                else {
                  int i;

                  for (i=0; i<db->size; i++) {
                    array_content[current_bytesize] = db->data[i];
                    current_bytesize++;
                  }
                  myfree(db->data);
                  myfree(db);
                }
              }  
              else {  /* string parsing failed */
                general_error(50);
                return;
              }
            }
            else {
              /* it was not a string, need to see if it is a number */
              taddr byte = parse_constexpr(&inner);

              if ((unsigned int)byte > 255) {
                general_error(51); /* size mismatch (we can store only bytes) */
                return;
              }
              array_content[current_bytesize] = byte;
              current_bytesize++;
            }

            while(*inner==' ' || *inner=='\t')
              inner++;
          }

          while (*inner=='\r' || *inner=='\n')
            inner++;
        }
      }  
      else {
        inner++;
        def = 0;
      }

      /* create the new field */
      field = mymalloc(sizeof(structfield));
      field->name    = id;
      field->bitsize = bitsize;  /* 0 for an array */
      field->isarray = isarray;
      if (isarray)
        field->content.defarray = array_content;
      else
        field->content.defval = def;
      field->next = NULL;

      /* add the new field to the structure*/
      if (cur_field==NULL)
        cur_struct->field = field;
      else
        cur_field->next = field;

      cur_field = field;
    }

    label = new_abs(cur_struct->name, number_expr(cur_struct->length/8));

    for (base=0,field=cur_struct->field; field!=NULL; field=field->next) {
      char *local_name, *full_name;

      local_name = mymalloc(sizeof(char) * (strlen(field->name)+2));
      local_name[0] = '.';
      strcpy(local_name+1, field->name);
      full_name = make_local_label(cur_struct->name,
                                   strlen(cur_struct->name),
                                   local_name, 
                                   strlen(local_name));
      myfree(local_name);

      label = new_abs(full_name, number_expr(base));
      base += field->bitsize/8;
    }

    cur_struct = NULL;
  }
  else
    ierror(0);
}


char *read_next_line(void)
/* reads the next input line */
{
  char *s,*srcend,*d;
  int nparam;
  int len = MAXLINELENGTH-1;
  char *rept_end = NULL;

  /* check if end of source is reached */
  for (;;) {
    srcend = cur_src->text + cur_src->size;
    if (cur_src->srcptr >= srcend || *(cur_src->srcptr) == '\0') {
      if (--cur_src->repeat > 0) {
        cur_src->srcptr = cur_src->text;  /* back to start */
        cur_src->line = 0;
#ifdef REPTNSYM
        set_internal_abs(REPTNSYM,++cur_src->reptn);
#endif
      }
      else {
        myfree(cur_src->linebuf);  /* linebuf is no longer needed, saves memory */
        cur_src->linebuf = NULL;
        if (cur_src->parent == NULL)
          return NULL;  /* no parent source means end of assembly! */
        cur_src = cur_src->parent;  /* return to parent source */
#ifdef CARGSYM
        if (cur_src->cargexp) {
          symbol *carg = internal_abs(CARGSYM);
          carg->expr = cur_src->cargexp;  /* restore parent CARG */
        }
#endif
#ifdef REPTNSYM
        set_internal_abs(REPTNSYM,cur_src->reptn);  /* restore parent REPTN */
#endif
      }
    }
    else
      break;
  }

  cur_src->line++;
  s = cur_src->srcptr;
  d = cur_src->linebuf;
  nparam = cur_src->num_params;

  if (enddir_list!=NULL && (srcend-s)>enddir_minlen) {
    /* reading a definition, like a structure, a macro or a repeat-block,
       until an end directive is found */
    struct namelen *dir;
    int rept_nest = 1;

    if (nparam>=0 && cur_macro!=NULL)
        general_error(26,cur_src->name);  /* macro definition inside macro */

    while (s <= (srcend-enddir_minlen)) {
      if (dir = dirlist_match(s,srcend,enddir_list)) {
        /* TODO better to use the semantic of the end in order to know what
           is ending. Because this code is able to end a macro instead
           of a repeat. */
        if (cur_macro != NULL) {
          add_macro();  /* link macro-definition into hash-table */
          s += dir->len;
          enddir_list = NULL;
          break;
        }
        else if (cur_struct != NULL) {
          add_structure();  /* link structure-definition into hash-table */
          s += dir->len;
          enddir_list = NULL;
          break;
        }
        else if (--rept_nest == 0) {
          rept_end = s;
          s += dir->len;
          enddir_list = NULL;
          break;
        }
      }
      else if (cur_macro==NULL && cur_struct==NULL && reptdir_list!=NULL &&
               (dir = dirlist_match(s,srcend,reptdir_list)) != NULL) {
        s += dir->len;
        rept_nest++;
      }

      if (*s=='\"' || *s=='\'') {
        char c = *s++;

        while (s<=(srcend-enddir_minlen) && *s!=c && *s!='\n' && *s!='\r') {
          if (*s == '\\')
            s++;
          s++;
        }
      }

      if (*s == commentchar)
        s = skip_eol(s,srcend);

      if (*s == '\n') {
        cur_src->srcptr = s + 1;
        cur_src->line++;
      }
      else if (*s=='\r' && *(s-1)!='\n' && (s>=(srcend-1) || *(s+1)!='\n')) {
        cur_src->srcptr = s + 1;
        cur_src->line++;
      }
      s++;
    }

    if (enddir_list) {
      if (cur_macro)
        general_error(25,cur_macro->name);  /* missing ENDM directive */
      else if (cur_struct)
        general_error(48);                  /* missing ENDSTRUCT directive */
      else
        general_error(32);  /* missing ENDR directive */
    }

    /* ignore rest of line, treat as comment */
    s = skip_eol(s,srcend);
  }

  /* copy next line to linebuf */
  while (s<srcend && *s!='\0' && *s!='\n') {

    if (nparam>=0 && *s=='\\') {
      /* insert macro parameters */
      struct macarg *ma;
      int ma_idx;
      int nc = -1;

      if (*(s+1) == '\\') {
      	*d++ = '\\';
        nc = 1;
        if (esc_sequences) {
        	*d++ = '\\';
          nc = 2;
        }
      	s += 2;
      }
      else if (*(s+1) == '@') {
        /* \@ : insert a unique id "_nnnnnn" */
        if (len >= 7) {
          unsigned long unique_id = cur_src->id;

          *d++ = '_';
          len--;
          s += 2;
          if (*s == '!') {
            /* push id onto stack */
            if (id_stack_index >= IDSTACKSIZE)
              general_error(39);  /* id stack overflow */
            else
              id_stack[id_stack_index++] = unique_id;
            ++s;              
          }
          else if (*s == '?') {
            /* push id below the top item on the stack */
            if (id_stack_index >= IDSTACKSIZE)
              general_error(39);  /* id stack overflow */
            else if (id_stack_index <= 0)
              general_error(45);  /* insert on empty id stack */
            else {
              id_stack[id_stack_index] = id_stack[id_stack_index-1];
              id_stack[id_stack_index-1] = unique_id;
              ++id_stack_index;
            }
            ++s;
          }
          else if (*s == '@') {
            /* pull id from stack */
            if (id_stack_index <= 0)
              general_error(40);  /* id pull without matching push */
            else
              unique_id = id_stack[--id_stack_index];
            ++s;
          }
          nc = sprintf(d, "%06lu", unique_id);
        }
      }
      else if (*(s+1) == '#') {
        /* \# : insert number of parameters */
        if (len >= 2) {
          nc = sprintf(d,"%d",cur_src->num_params);
          s += 2;
        }
      }
      else if (*(s+1)=='?' && isdigit((unsigned char)*(s+2))) {
        /* \?n : insert parameter n length */
        if (len >= 3) {
          nc = sprintf(d,"%d",cur_src->param_len[*(s+2)-'0']);
          s += 3;
        }
      }
#ifdef CARGSYM
      else if (*(s+1) == '.') {
        /* \. : insert parameter #CARG */
        nc = copy_macro_carg(0,d,len);
        s += 2;
      }
      else if (*(s+1) == '+') {
        /* \+ : insert parameter #CARG and increment CARG */
        nc = copy_macro_carg(1,d,len);
        s += 2;
      }
      else if (*(s+1) == '-') {
        /* \- : insert parameter #CARG and decrement CARG */
        nc = copy_macro_carg(-1,d,len);
        s += 2;
      }
#endif
      else if (isdigit((unsigned char)*(s+1))) {
        /* \0..\9 : insert macro parameter 0..9 */
        nc = copy_macro_param(*(s+1)-'0',d,len);
        s += 2;
      }
      else if (namedmacparams && ISIDSTART(*(s+1)) &&
               (ma_idx = find_param_name(s+1,&nc)) > 0) {
         /* \argname : insert named macro parameter ma_idx */
        s += nc + 1;
        nc = copy_macro_param(ma_idx,d,len);
      }
      else if (maxmacparams>10 && !namedmacparams &&
               tolower((unsigned char)*(s+1))>='a' &&
               tolower((unsigned char)*(s+1))<('a'+maxmacparams-10)) {
        /* \a..\z : insert macro parameter 10..36 */
        nc = copy_macro_param(tolower((unsigned char)*(s+1))-'a'+10,d,len);
        s += 2;
      }
      else if (*(s+1)=='(' && *(s+2)==')') {
        /* \() is just skipped, useful to terminate named macro parameters */
        nc = 0;
        s += 3;
      }
      if (nc >= 0) {
        len -= nc;
        d += nc;
        continue;
      }
    }

    else if (*s == '\r') {
      if ((s>cur_src->srcptr && *(s-1)=='\n') ||
          (s<(srcend-1) && *(s+1)=='\n')) {
        /* ignore \r in \r\n and \n\r combinations */
        s++;
        continue;
      }
      else {
        /* treat a single \r as \n */
        s++;
        break;
      }
    }

    if (len > 0) {
      *d++ = *s++;
      len--;
    }
    else
      s++;  /* line buffer is full, ignore additional characters */
  }

  *d = '\0';
  if (s<srcend && *s=='\n')
    s++;
  cur_src->srcptr = s;

  if (listena) {
    listing *new = mymalloc(sizeof(*new));

    new->next = 0;
    new->line = cur_src->line;
    new->error = 0;
    new->atom = 0;
    new->sec = 0;
    new->pc = 0;
    new->src = cur_src;
    strncpy(new->txt,cur_src->linebuf,MAXLISTSRC);
    if (first_listing) {
      last_listing->next = new;
      last_listing = new;
    }
    else {
      first_listing = last_listing = new;
    }
    cur_listing = new;
  }

  s = cur_src->linebuf;
  if (rept_end)
    start_repeat(rept_end);
  return s;
}


int init_parse(void)
{
  macrohash = new_hashtable(MACROHTABSIZE);
  structhash = new_hashtable(STRUCTHTABSIZE);
#ifdef CARGSYM
  carg1 = number_expr(1);
#endif
  return 1;
}
