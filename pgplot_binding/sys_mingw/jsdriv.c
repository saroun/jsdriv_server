/*
Wrapper for graphics driver JSDRIV for Windows, loaded from jsdrivlib.dll
Public domain
J. Saroun, 2017, saroun@ujf.cas.cz
*/

#ifdef PG_PPU
#define JSDRIV jsdriv_
#else
#define JSDRIV jsdriv
#endif

#include <stdio.h>
#include <windows.h>
/* Dynamic DLL linking */

static HMODULE jsdrivhandle = NULL;

/*Typedef jsdriv function*/
typedef void (__stdcall *jsdtype)(int *ifunc, float rbuf[], int *nbuf, char *chr, int *lchr, unsigned len);
static jsdtype jsdriv_fnc = NULL;

/* Load dll and its procedures dynamically
Return:
<0 if error occured 
=0 already loaded
=1 if successful
*/
int initjsdriv() {
    int ires = 0;
/* already loaded => return 0*/
    if (jsdrivhandle) {
      return 0;
    };
/* try to load dll*/
    SetLastError(0); 
    jsdrivhandle = LoadLibrary("jsdrivlib.dll");
/* if failed, return error code */
    if (! jsdrivhandle) {
        ires=GetLastError();
        printf ("Error: cannot load jsdrivlib.dll, err=%d\n", ires);
        SetLastError(0);
        return -1;
    }
/* load function pointers from DLL */  
    jsdriv_fnc = (jsdtype)GetProcAddress(jsdrivhandle, "jsdriv");
    if (! jsdriv_fnc) {
       ires=GetLastError();
       printf ("Error: cannot read symbol jsdriv from jsdrivlib.dll, err=%d\n", ires);
       SetLastError(0);
       return -2;
    } else {
       printf ("Successfully loaded jsdriv_fnc from jsdrivlib.dll\n"); 
    }
    return 1;
};

void JSDRIV(int *ifunc, float rbuf[], int *nbuf, char *chr, int *lchr, unsigned len)
{
 char sbuf[1024];
 size_t slen;
 int ie = initjsdriv();
 if (ie<0) {
    printf ("Error in initjsdriv: %d\n",ie);
 }
 if (jsdriv_fnc) {
	 /* NOTE: JSDRIVER is called from fortran, it doesn't care about null-termination of strings,
       but adds the length parameter at the end */
	slen = len;
	if (slen>1023) slen=1023;
	strncpy(sbuf, chr, len);
    sbuf[slen]='\0';	
    jsdriv_fnc(ifunc, rbuf, nbuf, sbuf, lchr, len);
	strncpy(chr, sbuf, *lchr);
 } else {
   printf ("Error: cannot call jsdriv, not found in jsdrivlib.dll\n");
 }
  
};





