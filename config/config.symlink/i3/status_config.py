# -*- coding: utf-8 -*-
import netifaces
import i3pystatus.network
import i3pystatus.xkblayout
import i3pystatus.backlight
from i3pystatus import Status
from i3pystatus.weather import weathercom
from i3pystatus import Status
from i3pystatus.core.command import run_through_shell
from os.path import expanduser
home = expanduser("~")

intf = netifaces.interfaces()
if intf[0].startswith("lo"):
    intf = intf[1:]
# sort to put up interfaces first
intf = sorted(intf, key = i3pystatus.network.sysfs_interface_up, reverse=True)

status = Status(standalone = True)

 # starting from right
status.register("xkblayout"
                , format="{symbol}"
                , on_upscroll = ['change_layout', 1]
                , on_downscroll = ['change_layout', 1]
                )

status.register("clock"
                , format="%H:%M:%S"
                , interval=1
                , color="#00FF00"
                # , hints={}
                , hints={"separator" : False}
                )

status.register("clock"
                , format="%a %d.%m.%y"
                , interval=60
                , color="#FFFFFF"
                , hints={"separator" : False}
                )

status.register("mem"
                , format=" {percent_used_mem}%"
                , color="#00FF00"
                , warn_color="#FFFF00"
                , alert_color="#FF0000"
                , hints={}
                )

def lighter(self):
            pr = run_through_shell([home+ "/bin/brightlight", "-i", "1", "-p"])
#            pr = run_through_shell(["/etc/acpi/actions/bl_up.sh", ""])
            return pr

def darker(self):
            pr = run_through_shell([home+"/bin/brightlight", "-d", "1", "-p"])
#            pr = run_through_shell(["/etc/acpi/actions/bl_down.sh", ""])
            return pr
            # return pr

i3pystatus.backlight.Backlight.lighter = lighter 
i3pystatus.backlight.Backlight.darker = darker 

bl =status.register("backlight"
    , format=" {percentage}%"
    , backlight="intel_backlight"
    , interval=2
    , color="#FFFF11"
    )
# Shows your CPU temperature, if you have a Intel CPU
status.register("temp"
        , format="{temp:.0f}°"
        , color="#3F9BF6"
        )

status.register("cpu_usage"
        , format=" {usage:02}"
        # , color="#3F9BF6")
        , hints={"separator" : False}
        )

battery = status.register("battery"
        , alert=True
        , format="{percentage_design:.2f}%{status}"
        , charging_color="#FFFF00"
        , color="#FF793D"
        , status= {'CHR': ' ', 'DIS': ' ⚡', 'FULL': '', 'DPL': ''}
        , not_present_text=""
       )
status.register("disk"
    , path="/"
    , format=" {used}/{total}"
    , interval=10
    , color="#FFFA94"
    , round_size=0
    , critical_limit=5
    )

def getWindDirection():
  pass

status.register(
    'weather',
    format='<span font=\"20\">[{icon}]</span> <span color=\"yellow\">{current_temp}{temp_unit}</span><span color=\"#ff4d4d\">[{high_temp}{temp_unit}]</span><span color=\"#b3ecff\"> [{low_temp}{temp_unit}]</span> [{dewpoint}💧] <span color=\"#e600e6\">{wind_speed}km/h {wind_direction}</span> <span color=\"#00e600\">{pressure}{pressure_trend}</span> [ {update_error}]',
    interval=900,
    colorize=True,
    hints={'markup': 'pango'},
    backend=weathercom.Weathercom(
        location_code='LUXX0003',
        units='metric',
        update_error='<span color="#ff0000">!</span>',
    ),
)
# print(bl.darker())
# bl.darker()
# battery.fdict["remaining"]=i3pystatus.core.util.TimeWrapper(0, "%E%h:%Mm")

def cycle(self, increment=1):
        """Cycle through available interfaces in `increment` steps. Sign indicates direction."""
        interfaces = [i for i in netifaces.interfaces() if i not in self.ignore_interfaces]
        if self.interface in interfaces:
            next_index = (interfaces.index(self.interface) + increment) % len(interfaces)
            self.interface = interfaces[next_index]
        elif len(interfaces) > 0:
            self.interface = interfaces[0]
        if self.interface.startswith("w"):
            symbol = ""
            upFormat= "{quality}  "
        else:
            symbol = ""
            upFormat= ""

        self.format_down=symbol
        self.format_up=symbol+upFormat+"  {network_graph_recv}{bytes_recv}K"
        if self.network_traffic:
            self.network_traffic.clear_counters()
            self.kbs_arr = [0.0] * self.graph_width


i3pystatus.network.Network.cycle = cycle
if len(intf) > 0:
    if intf[0].startswith("w"):
        symbol = ""
        upFormat= "{quality}  "
    else:
        symbol = ""
        upFormat= ""
    network = status.register("network"
                , dynamic_color=False
                , interface=intf[0]
                , format_down=symbol
                , format_up=symbol+upFormat+"  {network_graph_recv}{bytes_recv}K"
                , color_up="#0099ff"
                , start_color="#0099ff"
                , end_color="#0099ff"
                , color_down="#555555"
                , on_rightclick = "cycle"
                , on_upscroll = ['cycle', 1]
                , on_downscroll = ['cycle', -1]
                )


status.run()

