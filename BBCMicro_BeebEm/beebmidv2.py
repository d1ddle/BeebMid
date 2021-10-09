#!/usr/bin/env python2
# -*- coding: utf-8 -*-

# BeebMid (Plays MIDIs by loading them from a separate file on the Beeb.)
# Version 2.1.3, (c) 2007-2010,2015-2021 Silas S. Brown., (c) 2021 d1ddle (https://d1ddle.com)
# License: Apache 2 (http://www.apache.org/licenses/LICENSE-2.0)

# BeebMid is essentially Midi Beeper (c) S.S.B. but that loads the midi data
# from a separate file decreasing disk space usage and easy data swapping.

# Some features from Midi Beeper (c) have been removed as seen neccessary to suit only the needs of the program.
# Iterations of Midi Beeper (c) are as follows:

# History is in https://github.com/ssb22/midi-beeper.git
# and https://gitlab.com/ssb22/midi-beeper.git
# and https://bitbucket.org/ssb22/midi-beeper.git
# and https://gitlab.developers.cam.ac.uk/ssb22/midi-beeper
# and in China: https://gitee.com/ssb22/midi-beeper
# but some early versions are missing from these repositories

# Convert MIDI files to BBC Micro programs
# (printed to standard output).  Set bbc_micro below:
bbc_micro = 0 # or run with --bbc
data_only = 0 # or run with --data: only outputs midi data (for use with player only)
player_only = 0 # or run with --player: only outputs player script (for use with data only)
info_only = 0 # or run with --info: outputs info about how to use (--data) and (--player)
art = 0 # or run with --art: outputs, art

acorn_electron = 0 # Don't mess

# HiBasic (Tube) support (~30k for programs) fully works
# Bas128 support (64k for programs) works but (a) bank-switching delays impact the timing of shorter notes and (b) bbc_binary option can cause "Wrap" errors during input.  However bbc_binary and bbc_ssd options should pack data into a smaller space so normal BASIC can be used.

force_monophonic = 0  # set this to 1 to have only the top line (not normally necessary)

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.



import os,sys
def delArg(a):
  found = a in sys.argv
  if found: sys.argv.remove(a)
  return found
if delArg('--bbc'): bbc_micro = 1
if delArg('--data'): data_only=bbc_micro=1
if delArg('--player'): player_only=bbc_micro=1
if delArg('--info'): info_only=bbc_micro=1
if delArg('--art'): art=1

assert not (data_only and (player_only)), "data_only means data only. Use bbc_micro (--bbc) for player & data."
assert not (player_only and (data_only)), "player_only means player only. Use bbc_micro (--bbc) for player & data."

if bbc_micro:
  # This is a compact BBC Micro program to multiplex up to
  # 9 channels of sound onto the BBC Micro's 3 channels.
  # Basically uses ENVELOPEs to do the pitch multiplexing.
  # BBC Micro's BASIC encouraged the use of @% through Z% by reserving memory for them (heap is typically small), so code can be quite obscure.
  bbc_micro = ["FOR C%=16 TO 19:SO.C%,0,0,0:N.\n" # flush all sound buffers, just in case
               "N%=0:" # N% = next available envelope number (1-16 if not using BPUT#, otherwise 1-4 but we don't want to redefine envelopes that are already associated with notes in the buffer)
               "DIM c%(8)\n" # c% is the current value of each 'channel'; 252 i.e. 4*63 is used for silence.  Data read tells the program what changes to make to this array for the next chord and for how long to sound it (see add_midi_note_chord below).
               "FOR D%=0 TO 8:c%(D%)=252:N.\n" # all channels start with silence
               # The next few lines can be abbreviated thus: "REP.:C%=0:REP.:READD%:c%(C%)=(D%A.63)*4:I%=(D%DIV64)+1:C%=C%+I%:U.I%=4:READD%:REP.:U.AD.-6>3:F.I%=0TO6S.3:S%=0:T%=0:IFc%(I%)=252:V%=0:EL.IFc%(I%+1)=252:V%=1:EL.S%=1:Q%=c%(I%+1)-c%(I%):IFc%(I%+2)=252:V%=2:EL.R%=c%(I%+2)-c%(I%+1):T%=1:V%=3" (237 keystrokes, out of a limit of 238).  But Bas128 is still too slow, even with the read loop all on 1 line like this.
               "REP.:C%=0\n" # C% is the write-index for our 'current chord' c%
               "REP.:READ D%\n" # lowest 6 bits = semitone no. (which we multiply by 4 to get pitch number); highest 2 bits = array-pointer increment - 1 (so we can increment 1, 2 or 3 places; an "increment" of 4 means end of chord)
               "c%(C%)=(D% AND 63)*4\n" # set pitch
               "I%=(D% DIV 64)+1:C%=C%+I%:U.I%=4\n" # c% array now all set
               "READ D%\n" # This will be the duration of the chord just specified
               "REP.:U.ADVAL(-6)>3\n" # BBC Micro quirk: contrary to what the manual says (in at least some printings), the number of notes in each channel's "to be played" buffer before the program waits can be 5 not 4 (on at least some versions of the BBC).  This, together with the current note, means we might need a total of 6*3=18 envelopes, and we have only 16 slots.  Hence the ADVAL loop to avoid filling the buffer completely.
               "FOR I%=0 TO 6 STEP 3\n" # handling our 'channels' as triples, up to 3 being arpeggiated into one BBC Micro channel; I% will be the index-start of each triple
               "S%=0:" # will be set to 1 if the second section of the envelope is used
               "T%=0\n" # will be set to 1 if the third section of the envelope is used
               "IF c%(I%)=252:V%=0:" # no volume if entire channel is silent (see special case below)
               "ELSE IF c%(I%+1)=252:V%=1:" # entire channel has just one note, so play it at "volume 1".  TODO: if c%(I%) is high, consider 'wobbling' the pitch to mask the SN76489's tuning inaccuracy of high notes, e.g. by setting S%=1:Q%=1.  This can be done inline (using the fact that BBC BASIC represents true as -1) using something like "S%=-(c%(I%)>150):Q%=S%:" here.  Would need to check if 150 really is a good threshold, and, if it works, also modify the datBytes string in make_bbcMicro_DFS_image: beware line-length bytes etc; will probably have to stop using the 'abbreviated' version if these extra 2 assignments would make the line too long.  Also check the bbc_sdl .replace of V%=1 below doesn't undo it (and consider turning it off if INKEY(-256) detects SDL-etc, as that environment has better tuning to begin with)
               "ELSE S%=1:Q%=c%(I%+1)-c%(I%):" # channel has at least 2 notes, so set Q% to the first pitch difference, and set S% to enable 2nd section of envelope
               "IF c%(I%+2)=252:V%=2:" # channel has exactly 2 notes, so play it at "volume 2"
               "ELSE R%=c%(I%+2)-c%(I%+1):T%=1:V%=3\n" # channel has 3 notes, so play it at "volume 3", set R% to second pitch difference, and set T% to enable 3rd section of envelope
               # (here ends what can be abbreviated as per the 'abbreviated' comment above)
               "IF V%:" # The following operations are done only if volume is not 0.  We special-case volume 0 so it doesn't use an envelope at all; this (along with the ADVAL loop above) seems to make things a little more robust, as short pauses between notes are frequent.   BBC.
               "V%=V%*24+55:" # 79, 103 or 127
               "N%=N%+1:IF N%=17:N%=1" # next available envelope number
               "\nIF V%:" # (still only if volume is not 0)
               "ENV.N%," # setting envelope number N%
               "3," # length of each step in centiseconds
               "0," # first section should sound the 1st note
               "Q%," # second section adds Q% to the pitch for each step
               "R%," # third section adds R% to the pitch for each step
               "1," # first section should have 1 step (for sounding the 1st note)
               "S%," # second section should have either 0 steps (if not used) or 1 step (for sounding note + Q%)
               "T%," # third section should have either 0 steps (if not used) or 1 step (for sounding note + Q% + R%)
               "V%,0,0,-V%," # ADSR (attack, decay, sustain, release) change per step
               "V%," # attack final volume
               "V%" # decay final volume
               ":V%=N%\n" # for the SOUND command below
               "SO.513+(I%DIV3)," # 512 = sync=2 i.e. 3 channels are to receive a note before it is to start; +1 because we're not using channel 0
               "V%," # envelope number or 0
               "c%(I%)," # first pitch of the arpeggio (or plain pitch if no arpeggio)
               "D%\n" # duration
               "N.:U.D%=0:END"]
  
  current_array = [63]*9
  keystroke_limit = 238
  def add_midi_note_chord(noteNos,microsecs):
    duration = int((microsecs*20+500000)/1000000)
    while duration > 254: # unlikely but we should cover this
      add_midi_note_chord(noteNos,254*1000000/20)
      duration -= 254
    if not duration: return
    def f(n): # convert to SOUND/4 and bound the octaves
      n -= 47 # MIDI note 69 (A4) is pitch 88 i.e. 4*22
      while n<0: n+=12 # TODO: unless we want to make a bass line using SOUND 0,-V,3,D with SOUND 1,E,P+188,D (won't work on acorn_electron; envelope E will have to set its volume params to 0, or stick with single note), or SO.0,-V,2,D for approx. 1 tone below note 0 (which doesn't tie up channel 1).  Would need to know total number of notes there'll be before deciding if can do this.  Anyway such low notes are rather indistinct on BBC hardware.
      while n>=63: n-=12 # we're using 63 for rest
      return n
    noteNos = map(f,noteNos[-9:])
    while len(noteNos)<3: noteNos.append(63)
    for a,b in [(9,0),(7,0),(9,3),(8,3),(9,6),(9,6)]:
      if len(noteNos)<a: noteNos.insert(len(noteNos)-b,63)
    # Check range of arpeggiation pitch increments, adjust
    # octave as needed (too high shouldn't happen in
    # sensible music, but double-bass too low is possible)
    for i in range(0,len(noteNos),3):
      for j in range(i+1,i+3):
        if noteNos[j]==63: break
        while noteNos[j]>noteNos[j-1]+31: noteNos[j]-=12
        while noteNos[j]<noteNos[j-1]-32: noteNos[j]+=12
    # Now calculate the DATA numbers:
    o = [] ; curSkip = 0
    for i in range(len(current_array)):
      if noteNos[i]==current_array[i] and o and curSkip<2:
        curSkip += 1 ; continue
      if curSkip: o[-1] += curSkip*64
      curSkip = 0 ; current_array[i] = noteNos[i]
      if noteNos[i:] == current_array[i:]:
        o.append(noteNos[i]+3*64) ; break # last change
      else: o.append(noteNos[i])
    o.append(duration)
    o = ",".join(map(lambda x:("%d"%x), o))
    if len(bbc_micro)>1 and len(bbc_micro[-1])+len(o)+1 <= keystroke_limit: bbc_micro[-1] += ','+o    
    else:
      bbc_micro.append("D."+o)
  def init():
    global dedup_microsec_quantise
    dedup_microsec_quantise = 50000 # 1000000/20
else: # beep
  # NSLU2 hack:
  try: event=open("/proc/bus/input/devices").read()
  except IOError: event=""
  if "ixp4xx beeper" in event:
    h=event[event.find("Handlers=",event.index("ixp4xx beeper")):]
    event="-e /dev/input/"+(h[:h.find("\n")].split()[-1])
    os.system("sync") # just in case (beep has been known to crash NSLU2 Debian Etch in rare conditions)
  else: event=""

  def init():
    global cumulative_params
    cumulative_params = []
  min_pulseLength, max_pulseLength = 10,20 # milliseconds
  repetitions_to_aim_for = 1 # arpeggiating each chord only once will do if it's brief
  def chord(freqList,millisecs):
    if not millisecs: return ""
    elif not freqList: return " -D %d" % (millisecs,) # rest
    elif len(freqList)==1: return " -n -f %d -l %d" % (freqList[0],millisecs) # one note
    else:
        pulseLength = max(min(millisecs/len(freqList)/repetitions_to_aim_for,max_pulseLength),min_pulseLength)
        return (" -D 0".join([chord([f],pulseLength) for f in freqList]))*max(1,millisecs/pulseLength/len(freqList)) # (max with 1 means at least 1 repetition - prefer a slight slow-down to missing a chord out)
    # (the above -D 0 is necessary because Debian 5's beep adds a default delay otherwise)

  command_line_len = 80000 # reduce this if you get "argument list too long" (NB the real limit is slightly more than this value)

  def runBeep(params):
    while " -n" in params: # not entirely silence
        params=params[params.find(" -n")+3:] # discard the initial "-n" and any delay before it
        brkAt = params.find(" -n",command_line_len)
        if brkAt>-1: thisP,params = params[:brkAt],params[brkAt:]
        else: thisP,params = params,""
        os.system("beep "+event+" "+thisP)

  def add_midi_note_chord(noteNos,microsecs):
    millisecs = microsecs / 1000
    if noteNos and cumulative_params and not "-D" in cumulative_params[-1].split()[-2:]: cumulative_params.append("-D 0") # necessary because Debian 5's beep adds a default delay otherwise
    cumulative_params.append(chord(map(to_freq,noteNos),millisecs))

dedup_microsec_quantise = 0 # for handling 'rolls' etc (currently used by bbc_micro; TODO: default 'beep' cmd also?)
def dedup_midi_note_chord(noteNos,microsecs):
  if force_monophonic and noteNos: noteNos=[max(noteNos)]
  else: noteNos.sort()
  global dedup_chord,dedup_microsec
  if dedup_microsec_quantise and not microsecs==None:
    global dedup_microsec_error
    microsecs += dedup_microsec_error ; oldM = microsecs
    microsecs = int((microsecs+dedup_microsec_quantise/2)/dedup_microsec_quantise) * dedup_microsec_quantise
    dedup_microsec_error = oldM - microsecs
  if noteNos == dedup_chord and microsecs:
    # it's just an extention of the existing one
    dedup_microsec += microsecs
    return
  elif microsecs==0: return # too short, quantise out (and don't have to change dedup_chord because the next one might immediately revert to it if this is a 'roll' effect)
  else: # microsecs==None (flush) or a note change
    add_midi_note_chord(dedup_chord,dedup_microsec)
    dedup_chord,dedup_microsec = noteNos,microsecs

A=440 # you can change this if you want to re-pitch
midi_note_to_freq = []
import math,re
for i in range(128): midi_note_to_freq.append((A/32.0)*math.pow(2,(len(midi_note_to_freq)-9)/12.0))
assert midi_note_to_freq[69] == A # (comment this out if using floating-point tuning because it might fail due to rounding)
def to_freq(n):
  if n==int(n): return midi_note_to_freq[int(n)]
  else: return (A/32.0)*math.pow(2,(n-9)/12.0)

# RISCOS Maestro Code was here

if art:
  non_ascii = '''
  ____            _     __  __ _     _ 
 |  _ \          | |   |  \/  (_)   | |
 | |_) | ___  ___| |__ | \  / |_  __| |
 |  _ < / _ \/ _ \ '_ \| |\/| | |/ _` |
 | |_) |  __/  __/ |_) | |  | | | (_| |
 |____/ \___|\___|_.__/|_|  |_|_|\__,_| \n\n'''
  print(non_ascii.decode('utf-8'))

# Some of the code below was taken from an old version of
# Python Midi Package by Max M,
# with much cutting-down and modifying
from types import StringType
from cStringIO import StringIO
from struct import pack, unpack
def getNibbles(byte): return (byte >> 4 & 0xF, byte & 0xF)
def setNibbles(hiNibble, loNibble):
    return (hiNibble << 4) + loNibble
def readBew(value):
    return unpack('>%s' % {1:'B', 2:'H', 4:'L'}[len(value)], value)[0]
def readVar(value):
    sum = 0
    for byte in unpack('%sB' % len(value), value):
        sum = (sum << 7) + (byte & 0x7F)
        if not 0x80 & byte: break
    return sum
def varLen(value):
    if value <= 127:
        return 1
    elif value <= 16383:
        return 2
    elif value <= 2097151:
        return 3
    else:
        return 4
def to_n_bits(value, length=1, nbits=7):
    bytes = [(value >> (i*nbits)) & 0x7F for i in range(length)]
    bytes.reverse()
    return bytes
def toBytes(value):
    return unpack('%sB' % len(value), value)
def fromBytes(value):
    if not value:
        return ''
    return pack('%sB' % len(value), *value)
NOTE_OFF = 0x80
NOTE_ON = 0x90
AFTERTOUCH = 0xA0
CONTINUOUS_CONTROLLER = 0xB0
PATCH_CHANGE = 0xC0
CHANNEL_PRESSURE = 0xD0
PITCH_BEND = 0xE0
BANK_SELECT = 0x00
MODULATION_WHEEL = 0x01
BREATH_CONTROLLER = 0x02
FOOT_CONTROLLER = 0x04
PORTAMENTO_TIME = 0x05
DATA_ENTRY = 0x06
CHANNEL_VOLUME = 0x07
BALANCE = 0x08
PAN = 0x0A
EXPRESSION_CONTROLLER = 0x0B
EFFECT_CONTROL_1 = 0x0C
EFFECT_CONTROL_2 = 0x0D
GEN_PURPOSE_CONTROLLER_1 = 0x10
GEN_PURPOSE_CONTROLLER_2 = 0x11
GEN_PURPOSE_CONTROLLER_3 = 0x12
GEN_PURPOSE_CONTROLLER_4 = 0x13
BANK_SELECT = 0x20
MODULATION_WHEEL = 0x21
BREATH_CONTROLLER = 0x22
FOOT_CONTROLLER = 0x24
PORTAMENTO_TIME = 0x25
DATA_ENTRY = 0x26
CHANNEL_VOLUME = 0x27
BALANCE = 0x28
PAN = 0x2A
EXPRESSION_CONTROLLER = 0x2B
EFFECT_CONTROL_1 = 0x2C
EFFECT_CONTROL_2 = 0x2D
GENERAL_PURPOSE_CONTROLLER_1 = 0x30
GENERAL_PURPOSE_CONTROLLER_2 = 0x31
GENERAL_PURPOSE_CONTROLLER_3 = 0x32
GENERAL_PURPOSE_CONTROLLER_4 = 0x33
SUSTAIN_ONOFF = 0x40
PORTAMENTO_ONOFF = 0x41
SOSTENUTO_ONOFF = 0x42
SOFT_PEDAL_ONOFF = 0x43
LEGATO_ONOFF = 0x44
HOLD_2_ONOFF = 0x45
SOUND_CONTROLLER_1 = 0x46
SOUND_CONTROLLER_2 = 0x47
SOUND_CONTROLLER_3 = 0x48
SOUND_CONTROLLER_4 = 0x49
SOUND_CONTROLLER_5 = 0x4A
SOUND_CONTROLLER_7 = 0x4C
SOUND_CONTROLLER_8 = 0x4D
SOUND_CONTROLLER_9 = 0x4E
SOUND_CONTROLLER_10 = 0x4F
GENERAL_PURPOSE_CONTROLLER_5 = 0x50
GENERAL_PURPOSE_CONTROLLER_6 = 0x51
GENERAL_PURPOSE_CONTROLLER_7 = 0x52
GENERAL_PURPOSE_CONTROLLER_8 = 0x53
PORTAMENTO_CONTROL = 0x54
EFFECTS_1 = 0x5B
EFFECTS_2 = 0x5C
EFFECTS_3 = 0x5D
EFFECTS_4 = 0x5E
EFFECTS_5 = 0x5F
DATA_INCREMENT = 0x60
DATA_DECREMENT = 0x61
NON_REGISTERED_PARAMETER_NUMBER = 0x62
NON_REGISTERED_PARAMETER_NUMBER = 0x63
REGISTERED_PARAMETER_NUMBER = 0x64
REGISTERED_PARAMETER_NUMBER = 0x65
ALL_SOUND_OFF = 0x78
RESET_ALL_CONTROLLERS = 0x79
LOCAL_CONTROL_ONOFF = 0x7A
ALL_NOTES_OFF = 0x7B
OMNI_MODE_OFF = 0x7C
OMNI_MODE_ON = 0x7D
MONO_MODE_ON = 0x7E
POLY_MODE_ON = 0x7F
SYSTEM_EXCLUSIVE = 0xF0
MTC = 0xF1
SONG_POSITION_POINTER = 0xF2
SONG_SELECT = 0xF3
TUNING_REQUEST = 0xF6
END_OFF_EXCLUSIVE = 0xF7
SEQUENCE_NUMBER = 0x00
TEXT            = 0x01
COPYRIGHT       = 0x02
SEQUENCE_NAME   = 0x03
INSTRUMENT_NAME = 0x04
LYRIC           = 0x05
MARKER          = 0x06
CUEPOINT        = 0x07
PROGRAM_NAME    = 0x08
DEVICE_NAME     = 0x09
MIDI_CH_PREFIX  = 0x20
MIDI_PORT       = 0x21
END_OF_TRACK    = 0x2F
TEMPO           = 0x51
SMTP_OFFSET     = 0x54
TIME_SIGNATURE  = 0x58
KEY_SIGNATURE   = 0x59
SPECIFIC        = 0x7F
FILE_HEADER     = 'MThd'
TRACK_HEADER    = 'MTrk'
TIMING_CLOCK   = 0xF8
SONG_START     = 0xFA
SONG_CONTINUE  = 0xFB
SONG_STOP      = 0xFC
ACTIVE_SENSING = 0xFE
SYSTEM_RESET   = 0xFF
META_EVENT     = 0xFF
def is_status(byte):
    return (byte & 0x80) == 0x80
class MidiToBeep:
    def update_time(self, new_time=0, relative=1):
        if relative:
            self._relative_time = new_time
            self._absolute_time += new_time
        else:
            self._relative_time = new_time - self._absolute_time
            self._absolute_time = new_time
        if self._relative_time:
            # time was advanced, so output something
            d = {}
            for c,v in self.current_notes_on: d[v+self.semitonesAdd[c]]=1
            if self.need_to_interleave_tracks: self.tracks[-1].append([d.keys(),self._relative_time*self.microsecsPerDivision])
            else: dedup_midi_note_chord(d.keys(),self._relative_time*self.microsecsPerDivision)
    def reset_time(self):
        self._relative_time = 0
        self._absolute_time = 0
    def rel_time(self): return self._relative_time
    def abs_time(self): return self._absolute_time
    def reset_run_stat(self): self._running_status = None
    def set_run_stat(self, new_status): self._running_status = new_status
    def get_run_stat(self): return self._running_status
    def set_current_track(self, new_track): self._current_track = new_track
    def get_current_track(self): return self._current_track
    def __init__(self):
        self._absolute_time = 0
        self._relative_time = 0
        self._current_track = 0
        self._running_status = None
        self.current_notes_on = []
        self.rpnLsb = [0]*16
        self.rpnMsb = [0]*16
        self.semitoneRange = [1]*16
        self.semitonesAdd = [0]*16
        self.microsecsPerDivision = 10000
    def note_on(self, channel=0, note=0x40, velocity=0x40):
        if velocity and not channel==9: self.current_notes_on.append((channel,note))
    def note_off(self, channel=0, note=0x40, velocity=0x40):
        try: self.current_notes_on.remove((channel,note))
        except ValueError: pass
    def aftertouch(self, channel=0, note=0x40, velocity=0x40): pass
    def continuous_controller(self, channel, controller, value):
        # Interpret "pitch bend range":
        if controller==64: self.rpnLsb[channel] = value
        elif controller==65: self.rpnMsb[channel] = value
        elif controller==6 and self.rpnLsb[channel]==self.rpnMsb[channel]==0:
            self.semitoneRange[channel]=value
    def patch_change(self, channel, patch): pass
    def channel_pressure(self, channel, pressure): pass
    def pitch_bend(self, channel, value):
        # Pitch bend is sometimes used for slurs
        # so we'd better interpret it (only MSB for now; full range is over 8192)
        self.semitonesAdd[channel] = (value-64)*self.semitoneRange[channel]/64.0
    def sysex_event(self, data): pass
    def midi_time_code(self, msg_type, values): pass
    def song_position_pointer(self, value): pass
    def song_select(self, songNumber): pass
    def tuning_request(self): pass
    def header(self, format=0, nTracks=1, division=96):
        self.division=division
        self.need_to_interleave_tracks = (format==1)
        self.tracks = [[]][:]
    def eof(self):
        if self.need_to_interleave_tracks:
            while True: # delete empty tracks
                try: self.tracks.remove([])
                except ValueError: break
            while self.tracks:
                minLen = min([t[0][1] for t in self.tracks])
                d = {}
                for t in self.tracks: d.update([(n,1) for n in t[0][0]])
                dedup_midi_note_chord(d.keys(),minLen)
                for t in self.tracks:
                    t[0][1] -= minLen
                    if t[0][1]==0: del t[0]
                while True: # delete empty tracks
                    try: self.tracks.remove([])
                    except ValueError: break
    def meta_event(self, meta_type, data): pass
    def start_of_track(self, n_track=0):
        self.reset_time()
        self._current_track += 1
        if self.need_to_interleave_tracks: self.tracks.append([])
    def end_of_track(self): pass
    def sequence_number(self, value): pass
    def text(self, text): pass
    def copyright(self, text): pass
    def sequence_name(self, text): pass
    def instrument_name(self, text): pass
    def lyric(self, text): pass
    def marker(self, text): pass
    def cuepoint(self, text): pass
    def program_name(self,progname): pass
    def device_name(self,devicename): pass
    def midi_ch_prefix(self, channel): pass
    def midi_port(self, value): pass
    def tempo(self, value):
        # TODO if need_to_interleave_tracks, and tempo is not already put in on all tracks, and there's a tempo command that's not at the start and/or not on 1st track, we may need to do something
        self.microsecsPerDivision = value/self.division
    def smtp_offset(self, hour, minute, second, frame, framePart): pass
    def time_signature(self, nn, dd, cc, bb): pass
    def key_signature(self, sf, mi): pass
    def sequencer_specific(self, data): pass

class RawInstreamFile:
    def __init__(self, infile=''):
        if infile:
            if isinstance(infile, StringType):
                infile = open(infile, 'rb')
                self.data = infile.read()
                infile.close()
            else:
                self.data = infile.read()
        else:
            self.data = ''
        self.cursor = 0
    def setData(self, data=''):
        self.data = data
    def setCursor(self, position=0):
        self.cursor = position
    def getCursor(self):
        return self.cursor
    def moveCursor(self, relative_position=0):
        self.cursor += relative_position
    def nextSlice(self, length, move_cursor=1):
        c = self.cursor
        slc = self.data[c:c+length]
        if move_cursor:
            self.moveCursor(length)
        return slc
    def readBew(self, n_bytes=1, move_cursor=1):
        return readBew(self.nextSlice(n_bytes, move_cursor))
    def readVarLen(self):
        MAX_VARLEN = 4
        var = readVar(self.nextSlice(MAX_VARLEN, 0))
        self.moveCursor(varLen(var))
        return var
class EventDispatcher:
    def __init__(self, outstream):
        self.outstream = outstream
        self.convert_zero_velocity = 1
        self.dispatch_continuos_controllers = 1
        self.dispatch_meta_events = 1
    def header(self, format, nTracks, division):
        self.outstream.header(format, nTracks, division)
    def start_of_track(self, current_track):
        self.outstream.set_current_track(current_track)
        self.outstream.start_of_track(current_track)
    def sysex_event(self, data):
        self.outstream.sysex_event(data)
    def eof(self):
        self.outstream.eof()
    def update_time(self, new_time=0, relative=1):
        self.outstream.update_time(new_time, relative)
    def reset_time(self):
        self.outstream.reset_time()
    def channel_messages(self, hi_nible, channel, data):
        stream = self.outstream
        data = toBytes(data)
        if (NOTE_ON & 0xF0) == hi_nible:
            note, velocity = data
            if velocity==0 and self.convert_zero_velocity:
                stream.note_off(channel, note, 0x40)
            else:
                stream.note_on(channel, note, velocity)
        elif (NOTE_OFF & 0xF0) == hi_nible:
            note, velocity = data
            stream.note_off(channel, note, velocity)
        elif (AFTERTOUCH & 0xF0) == hi_nible:
            note, velocity = data
            stream.aftertouch(channel, note, velocity)
        elif (CONTINUOUS_CONTROLLER & 0xF0) == hi_nible:
            controller, value = data
            if self.dispatch_continuos_controllers:
                self.continuous_controllers(channel, controller, value)
            else:
                stream.continuous_controller(channel, controller, value)
        elif (PATCH_CHANGE & 0xF0) == hi_nible:
            program = data[0]
            stream.patch_change(channel, program)
        elif (CHANNEL_PRESSURE & 0xF0) == hi_nible:
            pressure = data[0]
            stream.channel_pressure(channel, pressure)
        elif (PITCH_BEND & 0xF0) == hi_nible:
            hibyte, lobyte = data
            value = (hibyte<<7) + lobyte
            stream.pitch_bend(channel, value)
        else:
            raise ValueError, 'Illegal channel message!'
    def continuous_controllers(self, channel, controller, value):
        stream = self.outstream
        stream.continuous_controller(channel, controller, value)
    def system_commons(self, common_type, common_data):
        stream = self.outstream
        if common_type == MTC:
            data = readBew(common_data)
            msg_type = (data & 0x07) >> 4
            values = (data & 0x0F)
            stream.midi_time_code(msg_type, values)
        elif common_type == SONG_POSITION_POINTER:
            hibyte, lobyte = toBytes(common_data)
            value = (hibyte<<7) + lobyte
            stream.song_position_pointer(value)
        elif common_type == SONG_SELECT:
            data = readBew(common_data)
            stream.song_select(data)
        elif common_type == TUNING_REQUEST:
            stream.tuning_request(time=None)
    def meta_events(self, meta_type, data):
        stream = self.outstream
        if meta_type == SEQUENCE_NUMBER:
            number = readBew(data)
            stream.sequence_number(number)
        elif meta_type == TEXT:
            stream.text(data)
        elif meta_type == COPYRIGHT:
            stream.copyright(data)
        elif meta_type == SEQUENCE_NAME:
            stream.sequence_name(data)
        elif meta_type == INSTRUMENT_NAME:
            stream.instrument_name(data)
        elif meta_type == LYRIC:
            stream.lyric(data)
        elif meta_type == MARKER:
            stream.marker(data)
        elif meta_type == CUEPOINT:
            stream.cuepoint(data)
        elif meta_type == PROGRAM_NAME:
            stream.program_name(data)
        elif meta_type == DEVICE_NAME:
            stream.device_name(data)
        elif meta_type == MIDI_CH_PREFIX:
            channel = readBew(data)
            stream.midi_ch_prefix(channel)
        elif meta_type == MIDI_PORT:
            port = readBew(data)
            stream.midi_port(port)
        elif meta_type == END_OF_TRACK:
            stream.end_of_track()
        elif meta_type == TEMPO:
            b1, b2, b3 = toBytes(data)
            stream.tempo((b1<<16) + (b2<<8) + b3)
        elif meta_type == SMTP_OFFSET:
            hour, minute, second, frame, framePart = toBytes(data)
            stream.smtp_offset(
                    hour, minute, second, frame, framePart)
        elif meta_type == TIME_SIGNATURE:
            nn, dd, cc, bb = toBytes(data)
            stream.time_signature(nn, dd, cc, bb)
        elif meta_type == KEY_SIGNATURE:
            sf, mi = toBytes(data)
            stream.key_signature(sf, mi)
        elif meta_type == SPECIFIC:
            meta_data = toBytes(data)
            stream.sequencer_specific(meta_data)
        else:
            meta_data = toBytes(data)
            stream.meta_event(meta_type, meta_data)
class MidiFileParser:
    def __init__(self, raw_in, outstream):
        self.raw_in = raw_in
        self.dispatch = EventDispatcher(outstream)
        self._running_status = None
    def parseMThdChunk(self):
        raw_in = self.raw_in
        header_chunk_type = raw_in.nextSlice(4)
        header_chunk_zise = raw_in.readBew(4)
        if header_chunk_type != 'MThd': raise TypeError, "It is not a valid midi file!"
        self.format = raw_in.readBew(2)
        self.nTracks = raw_in.readBew(2)
        self.division = raw_in.readBew(2)
        if header_chunk_zise > 6:
            raw_in.moveCursor(header_chunk_zise-6)
        self.dispatch.header(self.format, self.nTracks, self.division)
    def parseMTrkChunk(self):
        self.dispatch.reset_time()
        dispatch = self.dispatch
        raw_in = self.raw_in
        dispatch.start_of_track(self._current_track)
        raw_in.moveCursor(4)
        tracklength = raw_in.readBew(4)
        track_endposition = raw_in.getCursor() + tracklength
        while raw_in.getCursor() < track_endposition:
            time = raw_in.readVarLen()
            dispatch.update_time(time)
            peak_ahead = raw_in.readBew(move_cursor=0)
            if (peak_ahead & 0x80):
                status = self._running_status = raw_in.readBew()
            else:
                status = self._running_status
            hi_nible, lo_nible = status & 0xF0, status & 0x0F
            if status == META_EVENT:
                meta_type = raw_in.readBew()
                meta_length = raw_in.readVarLen()
                meta_data = raw_in.nextSlice(meta_length)
                dispatch.meta_events(meta_type, meta_data)
            elif status == SYSTEM_EXCLUSIVE:
                sysex_length = raw_in.readVarLen()
                sysex_data = raw_in.nextSlice(sysex_length-1)
                if raw_in.readBew(move_cursor=0) == END_OFF_EXCLUSIVE:
                    eo_sysex = raw_in.readBew()
                dispatch.sysex_event(sysex_data)
            elif hi_nible == 0xF0:
                data_sizes = {
                    MTC:1,
                    SONG_POSITION_POINTER:2,
                    SONG_SELECT:1,
                }
                data_size = data_sizes.get(hi_nible, 0)
                common_data = raw_in.nextSlice(data_size)
                common_type = lo_nible
                dispatch.system_common(common_type, common_data)
            else:
                data_sizes = {
                    PATCH_CHANGE:1,
                    CHANNEL_PRESSURE:1,
                    NOTE_OFF:2,
                    NOTE_ON:2,
                    AFTERTOUCH:2,
                    CONTINUOUS_CONTROLLER:2,
                    PITCH_BEND:2,
                }
                data_size = data_sizes.get(hi_nible, 0)
                channel_data = raw_in.nextSlice(data_size)
                event_type, channel = hi_nible, lo_nible
                dispatch.channel_messages(event_type, channel, channel_data)
    def parseMTrkChunks(self):
        for t in range(self.nTracks):
            self._current_track = t
            self.parseMTrkChunk()
        self.dispatch.eof()
class MidiInFile:
    def __init__(self, outStream, infile=''):
        self.raw_in = RawInstreamFile(infile)
        self.parser = MidiFileParser(self.raw_in, outStream)
    def read(self):
        p = self.parser
        p.parseMThdChunk()
        p.parseMTrkChunks()
    def setData(self, data=''):
        self.raw_in.setData(data)

try: any
except: # Python 2.3 (RISC OS?)
  def any(x):
    for i in x:
      if i: return True
    return False
  def all(x):
    for i in x:
      if not i: return False
    return True

name = "BeebMid"
sys.stderr.write(name+" (c) 2007-2010, 2015-2020 Silas S. Brown. - (c) d1ddle 2021 (https://d1ddle.com)  License: Apache 2.0\n")
if not player_only and len(sys.argv)<2:
  if not info_only:
    if not art:
      sys.stderr.write("Syntax: py -2 midi-beeper.py [options] MIDI-filename ...\nOptions: --bbc | --data | --player | --info | --art\n") # (all BBC-Micro related)
elif not info_only and len(sys.argv)<2:
  sys.stderr.write("Syntax: py -2 midi-beeper.py [options] MIDI-filename ...\nOptions: --bbc | --data | --player | --info | --art\n") # (all BBC-Micro related)
elif player_only:
  bbc_micro = "\n".join(bbc_micro).split("\n")
  bbc_micro_player = []
  for i in range(0,16):
    bbc_micro_player.append(bbc_micro[i])
  print "\nAU.0,1"
  print "\n".join(bbc_micro_player)
  sys.exit(1)
elif info_only:
  print "\n\n                           ----WELCOME TO BEEBMID----"                    
  print "        --(c) Silas S. Brown (c) d1ddle (https://d1ddle.com) 2007-2021--"
  print "\n---- Copy, paste and save the player (--player) to your BBC DFS as 'PLAYER' ----"
  print "             ---- Copy, paste (--data) and 'RUN' to save DATA file ----"
  print '\nThen run the following on your BBC/BeebEm:\n\n\nCH."PLAYER"\n*Input Data File Name*\n\nRUN\n'
  print "\nYour music should play.\nThis means you can stream data from Disk and also repace music without regenerating the whole program.\nEventually I aim to expand this as a BBC midi editor."
  sys.exit(1)

for midiFile in sys.argv[1:]:
    init() ; dedup_chord,dedup_microsec = [],0
    dedup_microsec_error = 0
    sys.stderr.write("Parsing MIDI file "+midiFile+"\n")
    MidiInFile(MidiToBeep(), open(midiFile,"rb")).read()
    dedup_midi_note_chord([],None) # ensure flushed
if bbc_micro:
    if len(bbc_micro)>1 and len(bbc_micro[-1])<233: bbc_micro[-1] += ",255,0"
    else: bbc_micro.append("D.255,0")
    if data_only:
      bbc_micro = "\n".join(bbc_micro).split("\n")
      bbc_micro_data = []
      for i in range(16,len(bbc_micro)):
	bbc_micro_data.append(bbc_micro[i])
    if player_only:
      bbc_micro = "\n".join(bbc_micro).split("\n")
      print "\nPlayer length: 17\n"
      bbc_micro_player = []
      for i in range(0,15):
	bbc_micro_player.append(bbc_micro[i])
    bbc_micro = "\n".join(bbc_micro).split("\n")
    if len(bbc_micro) > 3277: bbc_micro.insert(0,"AU."+str(32768-len(bbc_micro))+",1") # (although if this is the case, program is extremely likely to exhaust the memory even in Bas128)
    else: bbc_micro.insert(0,"AU."+str(32770-10*len(bbc_micro)))
    if player_only:
      print "5 MODE 0"
      print "10 CLS"
      print "20 CLOSE #0"
      print '30 PRINTTAB(0,1) "BEEBMID SOFTWARE - PLAYER 0.2.0"'
      print '40 PRINT "(c) D1DDLE 2021 - (HTTPS://D1DDLE.COM)"'
      print '50 PRINT " ____            _     __  __ _     _ "'
      print '60 PRINT "|  _ \          | |   |  \/  (_)   | |"'
      print '70 PRINT "| |_) | ___  ___| |__ | \  / |_  __| |"'
      print '80 PRINT "|  _ < / _ \/ _ \  _ \| |\/| | |/ _  |"'
      print '90 PRINT "| |_) |  __/  __/ |_) | |  | | | (_| |"'
      print '100 PRINT "|____/ \___|\___|_.__/|_|  |_|_|\__,_|"'
      print '110 PRINT ""'
      print '120 INPUT "FILENAME: " A$'
      print '130 fnum =OPENIN A$'
      print '140 L=EXT#fnum'
      print '150 K=(PTR#fnum/L)*100'
      print '160 GCOL 0,3'
      print '170 MOVE 13,630'
      print '180 MOVE 1279*0.7,630'
      print '190 PLOT 85,1279*0.7,570'
      print '200 MOVE 13,570'
      print '210 MOVE 13,630'
      print '220 PLOT 85,1279*0.7,570'
      print '230 GCOL 0,0'
      print '240 MOVE 23,620'
      print '250 MOVE 23,580'
      print '260 PLOT 85,(1279*0.7)-10,580'
      print '270 MOVE 23,620'
      print '280 MOVE (1279*0.7)-10,620'
      print '290 PLOT 85,(1279*0.7)-10,580'
      print '300 MOVE 13,600'
      print '310 GCOL 0,3'
      print '320 PRINT "PLAYING: ";A$'
      print '330 PRINT ""'
      print '340 PRINT ""'
      print '350 PRINT ""'
      print '360 N%=0:DIM c%(8)'
      print '370 FOR D%=0 TO 8:c%(D%)=252:NEXT'
      print '380 REPEAT'
      print '390 K=(PTR#fnum/L)*100'
      print '400 DRAW ((K+1)*12.79)*0.7,600'
      print '410 C%=0'
      print '420 REPEAT'
      print '430 INPUT# fnum, D%'
      print '440 c%(C%)=(D% AND 63)*4'
      print '450 I%=(D% DIV 64)+1:C%=C%+I%'
      print '460 UNTIL I%=4'
      print '470 INPUT# fnum, D%'
      print '480 REPEAT:UNTILADVAL(-6)>3'
      print '490 FOR I%=0 TO 6 STEP 3'
      print '500 S%=0:T%=0'
      print '510 IF c%(I%)=252:V%=0:ELSE IF c%(I%+1)=252:V%=1:ELSE S%=1:Q%=c%(I%+1)-c%(I%):IF c%(I%+2)=252:V%=2:ELSE R%=c%(I%+2)-c%(I%+1):T%=1:V%=3'
      print '520 IF V%:V%=V%*24+55:N%=N%+1:IF N%=17:N%=1'
      print '530 IF V%:ENVELOPEN%,3,0,Q%,R%,1,S%,T%,V%,0,0,-V%,V%,V%:V%=N%'
      print '540 SOUND513+(I%DIV3),V%,c%(I%),D%'
      print '550 NEXT'
      print '560 UNTILD%=0'
      print '570 END'
      print '580 CLOSE #fnum'
    elif info_only:
      print "\n\n                           ----WELCOME TO BEEBMID----"                    
      print "        --(c) Silas S. Brown (c) d1ddle (https://d1ddle.com) 2007-2021--"
      print "\n---- Copy, paste and save the player (--player) to your BBC DFS as 'PLAYER' ----"
      print "             ---- Copy, paste (--data) and 'RUN' to save DATA file ----"
      print '\nThen run the following on your BBC/BeebEm:\n\n\nCH."PLAYER"\n*Input Data File Name*\n\nRUN\n'
      print "\nYour music should play.\nThis means you can stream data from Disk and also repace music without regenerating the whole program.\nEventually I aim to expand this as a BBC midi editor."
    elif not data_only:
      print "\n".join(bbc_micro)
    else:
      print "\nAU.0,1"
      print "\n".join(bbc_micro_data)
      print 'A=OPENOUT"DATA"'
      print 'REPEAT:READ D%:PRINT# A,D%:UNTIL FALSE'
      print 'CLOSE# 0'
      print "\nRUN"
