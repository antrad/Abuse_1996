/*
 *  Abuse - dark 2D side-scrolling platform game
 *  Copyright (c) 1995 Crack dot Com
 *  Copyright (c) 2005-2011 Sam Hocevar <sam@hocevar.net>
 *
 *  This software was released into the Public Domain. As with most public
 *  domain software, no warranty is made or implied by Crack dot Com, by
 *  Jonathan Clark, or by Sam Hocevar.
 */

#if defined HAVE_CONFIG_H
#   include "config.h"
#endif

#include <math.h>

#include "common.h"

#include "timing.h"
#include "loader2.h"
#include "chars.h"
#include "specs.h"
#include "lisp.h"
#include "jrand.h"
#include "menu.h"
#include "dev.h"
#include "director.h"

#include "dev.h"
#include "light.h"
#include "dprint.h"
#include "particle.h"
#include "clisp.h"
#include "compiled.h"
#include "sbar.h"
#include "help.h"
#include "loadgame.h"
#include "nfserver.h"
#include "specache.h"

extern int past_startup;

property_manager *prop;
int *backtiles;
int *foretiles;
JCFont *big_font,*console_font;
int nforetiles,nbacktiles,f_wid,f_hi,b_wid,b_hi,total_songs=0,sfx_volume,music_volume,sound_avail=0;
song *current_song=NULL;

uint16_t current_start_type,start_position_type,last_start_number;
int light_buttons[13];
int joy_picts[2*9];
palette *pal;

int big_font_pict=-1,small_font_pict=-1,console_font_pict=-1,cdc_logo;

int title_screen;

ColorFilter *color_table;


int border_tile,window_texture,
    raise_volume,lower_volume,record_button,play_button,music_button,sfx_button,
    window_colors,pause_image,damage_pict,block_pict,vmm_image,earth,earth_mask,clouds,
    numbers[10],ok_button,cancel_button;

int start_running=0;

int c_mouse1,c_mouse2,c_normal,c_target;

long bg_xmul,bg_xdiv,bg_ymul,bg_ydiv;    // brackground scroll rates
char mouse_scrolling=0,palettes_locked=1,view_shift_disabled=0;

int light_connection_color;


image *load_image(spec_entry *e, bFILE *fp)
{
    image *im = new image(fp, e);
    if (scale_mult != 1 || scale_div != 1)
        im->Scale(im->Size() * scale_mult / scale_div);
    return im;
}

image *load_image(bFILE *fp)
{
    image *im = new image(fp);
    if (scale_mult != 1 || scale_div != 1)
        im->Scale(im->Size() * scale_mult / scale_div);
    return im;
}

void use_file(char *filename, bFILE *&fp, spec_directory *&sd)
{
  char fn[100];
  fp=open_file(filename,"rb");
  if (fp->open_failure())
  {
    delete fp;
    snprintf(fn, sizeof(fn), "art/%s", filename);
    fp=open_file(fn,"rb");
    if (fp->open_failure())
    {
      printf("Unable to open file %s\n",filename);
      delete fp;
      exit(1);
    }
  }
  sd=new spec_directory(fp);
}

void done_file(bFILE *&fp, spec_directory *&sd)
{
  delete fp;
  delete sd;
}

void insert_tiles(char *filename)
{
  bFILE *fp=open_file(filename,"rb");
  if (!fp->open_failure())
  {
    int ft=0,bt=0;
    spec_directory sd(fp);
    delete fp;
    int i=0;
    for (; i<sd.total; i++)
    {
      spec_entry *se=sd.entries[i];
      if (se->type==SPEC_FORETILE)
        ft++;
      else if (se->type==SPEC_BACKTILE)
        bt++;
    }
    if (bt)
      printf("%s : adding %d background tiles (range %d-%d)\n",
         filename,bt,nbacktiles,nbacktiles+bt-1);
    if (ft)
      printf("%s : adding %d foreground tiles (range %d-%d)\n",
         filename,ft,nforetiles,nforetiles+bt-1);
    if (!ft && !bt)
      printf("Warning : file %s has no background or foreground tiles\n",filename);
    else
    {
      int fon=nforetiles,bon=nbacktiles;
      if (ft)
        foretiles=(int *)realloc(foretiles,sizeof(int)*(nforetiles+ft));
      if (bt)
        backtiles=(int *)realloc(backtiles,sizeof(int)*(nbacktiles+bt));

      for (i=0; i<sd.total; i++)
      {
    if (sd.entries[i]->type==SPEC_FORETILE)
    {
      foretiles[fon]=cache.reg(filename,sd.entries[i]->name);
      fon++;
      nforetiles++;
    }
    if (sd.entries[i]->type==SPEC_BACKTILE)
    {
      backtiles[bon]=cache.reg(filename,sd.entries[i]->name);
      bon++;
      nbacktiles++;
    }
      }
    }
  } else
    printf("Warning : insert_tiles -> file %s could not be read from\n",filename);
}

void load_tiles(Cell *file_list)
{
  bFILE *fp;
  spec_directory *sd;
  spec_entry *spe;


  int num;



  Cell *fl;
  int old_fsize=nforetiles,
      old_bsize=nbacktiles;

  for (fl=file_list; !NILP(fl); fl=lcdr(fl))
  {
    fp=open_file(lstring_value(lcar(fl)),"rb");
    if (fp->open_failure())
    {
      printf("Warning : open %s for reading\n",lstring_value(lcar(fl)));
      delete fp;
    }
    else
    {
      sd=new spec_directory(fp);
      delete fp;
      int i;
      for (i=0; i<sd->total; i++)
      {
    spe=sd->entries[i];
        switch (spe->type)
        {
          case SPEC_BACKTILE :
            if (!sscanf(spe->name,"%d",&num))
              printf("Warning : background tile %s has no number\n",spe->name);
            else if (nbacktiles<=num) nbacktiles=num+1;
          break;

          case SPEC_FORETILE :
            if (!sscanf(spe->name,"%d",&num))
              printf("Warning : foreground tile %s has no number\n",spe->name);
            else if (nforetiles<=num) nforetiles=num+1;
          break;
        }
      }
      delete sd;
    }
  }

  // enlarge the arrays if needed.
  if (nbacktiles>old_bsize)
  {
    backtiles=(int *)realloc(backtiles,sizeof(int)*nbacktiles);
    memset(backtiles+old_bsize,-1,(nbacktiles-old_bsize)*sizeof(int));
  }

  if (nforetiles>old_fsize)
  {
    foretiles=(int *)realloc(foretiles,sizeof(int)*nforetiles);
    memset(foretiles+old_fsize,-1,(nforetiles-old_fsize)*sizeof(int));
  }


// now load them up
  for (fl=file_list; !NILP(fl); fl=lcdr(fl))
  {
    char const *fn=lstring_value(lcar(fl));
    fp=open_file(fn,"rb");
    if (!fp->open_failure())
    {
      sd=new spec_directory(fp);
      delete fp;

      int i;
      for (i=0; i<sd->total; i++)
      {
    spe=sd->entries[i];
        switch (spe->type)
        {
          case SPEC_BACKTILE :

            if (sscanf(spe->name,"%d",&num))
        {
          if (backtiles[num]>=0)
          {
        dprintf("Warning : background tile %d redefined\n",num);
        cache.unreg(backtiles[num]);
          }
          backtiles[num]=cache.reg(fn,spe->name,SPEC_BACKTILE);
        }
            break;
          case SPEC_FORETILE :
            if (sscanf(spe->name,"%d",&num))
        {
          if (foretiles[num]>=0)
          {
        dprintf("Warning : foreground tile %d redefined\n",num);
        cache.unreg(foretiles[num]);
          }
          foretiles[num]=cache.reg(fn,spe->name,SPEC_FORETILE);
        }
            break;
        }
      }
      delete sd;
    } else delete fp;
  }

}


extern unsigned char fnt6x13[192*104];
char lsf[256]="abuse.lsp";

void load_data(int argc, char **argv)
{
    total_objects=0;
    total_weapons=0;
    weapon_types=NULL;
    figures=NULL;
    nforetiles=nbacktiles=0;
    foretiles=NULL;
    backtiles=NULL;
    pal=NULL;
    color_table=NULL;

# if 0
    int should_save_sd_cache = 0;

    char *cachepath;
    cachepath = (char *)malloc( strlen( get_save_filename_prefix() ) + 12 + 1 );
    sprintf( cachepath, "%ssd_cache.tmp", get_save_filename_prefix() );

    bFILE *load = open_file( cachepath, "rb" );
    if( !load->open_failure() )
    {
        sd_cache.load( load );
    }
    else
    {
        should_save_sd_cache = 1;
    }
    delete load;
#endif

  // don't let them specify a startup file we are connect elsewhere
  if (!net_start())
  {
    for (int i=1; i<argc; i++)
    {
      if (!strcmp(argv[i],"-lsf"))
      {
    i++;
    strcpy(lsf,argv[i]);
      }
      if (!strcmp(argv[i],"-a"))
      {
    i++;
    snprintf(lsf, sizeof(lsf), "addon/%s/%s.lsp", argv[i], argv[i]);
      }
    }
  }
  else if (!get_remote_lsf(net_server,lsf))
  {
    dprintf("Unable to get remote lsf from %s\n",net_server);
    exit(0);
  }
  char prog[100];
  char const *cs;

  c_mouse1=cache.reg("art/dev.spe","c_mouse1",SPEC_IMAGE,0);
  c_mouse2=cache.reg("art/dev.spe","c_mouse2",SPEC_IMAGE,0);
  c_normal=cache.reg("art/dev.spe","c_normal",SPEC_IMAGE,0);
  c_target=cache.reg("art/dev.spe","c_target",SPEC_IMAGE,0);


  snprintf(prog, sizeof(prog), "(load \"%s\")\n", lsf);

  cs=prog;
  if (!LObject::Compile(cs)->Eval())
  {
    printf("unable to open file '%s'\n",lsf);
    exit(0);
  }
  compiled_init();
  LSpace::Tmp.Clear();

  dprintf("Engine : Registering base graphics\n");
  for (int z=0; z<=11; z++)
  {
    char nm[10];
    snprintf(nm, sizeof(nm), "l%d", z);
    light_buttons[z]=cache.reg("art/dev.spe",nm,SPEC_IMAGE,0);
  }


  image *tmp_image = new image(ivec2(192, 104), fnt6x13);
  big_font=new JCFont(tmp_image);
  delete tmp_image;


  char const *ff;
  // FIXME: unnecessary duplicate call
  if (DEFINEDP(LSymbol::FindOrCreate("frame_file")->GetValue()))
    ff = lstring_value(LSymbol::FindOrCreate("frame_file")->GetValue());
  else
    ff = "art/frame.spe";

  ok_button   =      cache.reg(ff,"dev_ok",SPEC_IMAGE);
  cancel_button  =   cache.reg(ff,"cancel",SPEC_IMAGE);

//  clouds      =      cache.reg(ff,"clouds",SPEC_IMAGE);

  lower_volume=      cache.reg(ff,"lower_volume",SPEC_IMAGE);
  raise_volume=      cache.reg(ff,"raise_volume",SPEC_IMAGE);
  music_button=      cache.reg(ff,"music",SPEC_IMAGE);
  sfx_button=        cache.reg(ff,"sound_fx",SPEC_IMAGE);
  record_button=     cache.reg(ff,"record",SPEC_IMAGE);
  play_button=       cache.reg(ff,"play",SPEC_IMAGE);
  window_colors=     cache.reg(ff,"window_colors",SPEC_IMAGE);
  pause_image=       cache.reg(ff,"pause_image",SPEC_IMAGE);
  vmm_image=         cache.reg(ff,"vmm",SPEC_IMAGE);
  border_tile=       cache.reg(ff,"border_tile",SPEC_IMAGE);
  window_texture=    cache.reg(ff,"window_texture",SPEC_IMAGE);


  help_screens=NULL;
  total_help_screens=0;

  if (DEFINEDP(symbol_value(l_help_screens)))
  {
    void *v=symbol_value(l_help_screens);
    char *ff=lstring_value(CAR(v));  v=CDR(v);
    total_help_screens=0;
    while (v) { total_help_screens++; v=CDR(v); }
    if (total_help_screens)
    {
      help_screens=(int *)malloc(sizeof(int)*total_help_screens);
      v=CDR(symbol_value(l_help_screens));
      int i=0;
      for (; v; v=CDR(v),i++)
        help_screens[i]=cache.reg(ff,lstring_value(CAR(v)),SPEC_IMAGE);
    }
    else
      dprintf("Warning no help images following filename\n");
  }

  int i;
  for (i=1; i<argc; i++)
  {
    if (!strcmp(argv[i],"-ec"))
      l_empty_cache->SetValue(true_symbol);
    if (!strcmp(argv[i],"-t"))
    {
      i++;
      insert_tiles(argv[i]);
    }
  }

  if (DEFINEDP(symbol_value(l_title_screen)))
    title_screen=cache.reg_object(NULL,(LObject *)symbol_value(l_title_screen),SPEC_IMAGE,1);
  else title_screen=-1;

  if (DEFINEDP(symbol_value(l_cdc_logo)))
    cdc_logo=cache.reg_object(NULL,(LObject *)symbol_value(l_cdc_logo),SPEC_IMAGE,1);
  else cdc_logo=-1;

  start_position_type=0xffff;
  for(i=0; i<total_objects; i++)
    if (!strcmp(object_names[i],"START"))
      start_position_type=i;
  if (start_position_type==0xffff)
  {
    printf("No object named START, cannot start game.\n");
    exit(0);
  }

  sbar.load();

  load_number_icons();


  ERROR(nbacktiles,"No background tiles defined!");
  ERROR(nforetiles,"No foreground tiles defined!");
  ERROR(foretiles[0]>=0,"No black (0) foreground tile defined!");
  ERROR(backtiles[0]>=0,"No black (0) background tile defined!");
  ERROR(big_font_pict!=-1 || small_font_pict!=-1,
    "No font loaded (use load_big_font or load_small_font)!");
  f_wid=cache.foret(foretiles[0])->im->Size().x;
  f_hi=cache.foret(foretiles[0])->im->Size().y;
  b_wid=cache.backt(backtiles[0])->im->Size().x;
  b_hi=cache.backt(backtiles[0])->im->Size().y;

#if 0
    if( should_save_sd_cache )
    {
        bFILE *save = open_file( cachepath, "wb" );
        if( !save->open_failure() )
        {
            sd_cache.save( save );
        }
        delete save;
    }
#endif

    sd_cache.clear();
    past_startup = 1;
#if 0
    free( cachepath );
#endif
}





char *load_script(char *name)
{
  char fn[100];
  char *s;

  snprintf(fn, sizeof(fn), "%s", name);
  bFILE *fp=open_file(fn,"rb");
  if (fp->open_failure())
  {
    delete fp;
    return NULL;
  }

  long l=fp->file_size();
  s=(char *)malloc(l+1);
  ERROR(s,"Malloc error in load_script");

  fp->read(s,l);
  s[l]=0;
  delete fp;
  return s;
}
















