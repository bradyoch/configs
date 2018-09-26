#include <time.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <X11/Xlib.h>

// global variables
static Display *dpy;
static int screen;
static Window root;

static char root_name[256] = {};

// programs to spawn
static char volume[] = "/usr/bin/pamixer --get-volume";

// files to open
static const char batt_full[] = "/sys/class/power_supply/BAT1/energy_full";
static const char batt_now[] = "/sys/class/power_supply/BAT1/energy_now";

// main update function
void update(int signal);

// get info for the bar
void get_battery(char * const bat);
void get_volume(char * const vol);
void get_time(char * const tim);

int main(int argc, char const **argv) {
  dpy = XOpenDisplay(NULL);
  screen = DefaultScreen(dpy);
  root = RootWindow(dpy, screen);

  signal(SIGUSR1, update);

  while (1) {
    update(0);
    sleep(30);
  }

  XCloseDisplay(dpy);
  return 0;
}

void update(int signal) {
  char bat[10];
  char vol[10];
  char tim[100];
  root_name[0] = '\0';

  get_battery(bat);
  get_volume(vol);
  get_time(tim);

  sprintf(root_name, "BAT: %s% VOL: %s %s", bat, vol, tim);

  XStoreName(dpy, root, root_name);
  XSync(dpy, 1);
}

void get_battery(char * const bat) {
  long now;
  long full;

  FILE *f_now;
  FILE *f_full;

  f_now = fopen(batt_now, "r");
  f_full = fopen(batt_full, "r");

  fgets(bat, 10, f_now);
  now = strtol(bat, NULL, 10);

  fgets(bat, 199, f_full);
  full = strtol(bat, NULL, 10);

  sprintf(bat, "%d", (int)(now * 100 / full));

  fclose(f_full);
  fclose(f_now);
}

void get_volume(char * const vol) {
  size_t len;

  FILE *p_vol;

  p_vol = popen(volume, "r");

  fgets(vol, 10, p_vol);
  len = strlen(vol);
  vol[len - 1] = '\0';
}

void get_time(char * const tim) {
  time_t rawtime;
  struct tm *info;

  time(&rawtime);
  info = localtime(&rawtime);

  strftime(tim, 100, "%A %B %d %I:%M %p", info);
}
