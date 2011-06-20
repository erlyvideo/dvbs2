#include <erl_driver.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <errno.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <assert.h>
#include <sys/uio.h>

#include <sys/ioctl.h>
#include <linux/dvb/dmx.h>
#include <linux/dvb/frontend.h>
#include <linux/dvb/version.h>


#ifndef IOV_MAX
#define IOV_MAX 1000
#endif


enum {
  CMD_OPEN = 1,
  CMD_GET_PID = 2,
  CMD_START_INPUT = 3
};


#pragma pack(1)
typedef struct {
  uint32_t lo_freq;
  uint32_t hi_freq;
  uint32_t freq;
  uint32_t srate;
  uint8_t adapter;
  uint8_t tuner;
  uint8_t polarization;
} Config;
#pragma options align=reset

#define MAX_PIDS 8192

typedef struct {
  ErlDrvPort port;
  ErlDrvTermData owner_pid;
  int fe_fd;
  int dvr_fd;
  int dmx_pids[MAX_PIDS];
  uint32_t lo_frequency;
  uint32_t hi_frequency;
  uint32_t frequency;
  uint32_t symbol_rate;
  uint8_t polarization;
  uint8_t adapter;
  uint8_t tuner;
  fe_code_rate_t code_rate;
} Dvbs2;




static ErlDrvData dvbs2_drv_start(ErlDrvPort port, char *buff)
{
    Dvbs2* d = (Dvbs2 *)driver_alloc(sizeof(Dvbs2));
    bzero(d, sizeof(Dvbs2));
    d->port = port;
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    d->owner_pid = driver_caller(port);
    int i;
    d->fe_fd = -1;
    d->dvr_fd = -1;
    for(i = 0; i < MAX_PIDS; i++) {
      d->dmx_pids[i] = -1;
    }
    return (ErlDrvData)d;
}


static void dvbs2_drv_stop(ErlDrvData handle)
{
  Dvbs2* d = (Dvbs2 *)handle;
  driver_select(d->port, (ErlDrvEvent)(d->fe_fd), DO_READ|DO_WRITE, 0);
  close(d->fe_fd);
  driver_select(d->port, (ErlDrvEvent)(d->dvr_fd), DO_READ, 0);
  close(d->dvr_fd);
  int i;
  for(i = 0; i < MAX_PIDS; i++) {
    if(d->dmx_pids[i]) close(d->dmx_pids[i]);
  }
  driver_free((char*)handle);
}


static void dvb_exit(Dvbs2 *d)
{
  driver_select(d->port, (ErlDrvEvent)d->fe_fd, DO_READ|DO_WRITE, 0);
  ErlDrvTermData reply[] = {
    ERL_DRV_ATOM, driver_mk_atom("dvb_closed"),
    ERL_DRV_PORT, driver_mk_port(d->port),
    ERL_DRV_TUPLE, 2
  };
  driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
  driver_exit(d->port, 0);
}

static int dvbs2_drv_command(ErlDrvData handle, unsigned int command, char *buf, 
                   int len, char **rbuf, int rlen) {
  Dvbs2* d = (Dvbs2*) handle;
  
  switch(command) {
    case CMD_OPEN: {
      
      fprintf(stderr, "Hi\r\n");
      
      d->fe_fd = open("/dev/dvb/adapter0/frontend0", O_RDWR | O_NONBLOCK);
      if(d->fe_fd == -1) {
        driver_failure_posix(d->port, errno);
        return 0;
      }
      
      if(len != sizeof(Config)) {
        driver_failure_atom(d->port, "invalid_config");
      }
      
      Config *cfg = (Config *)buf;
      
      d->lo_frequency = cfg->lo_freq;
      d->hi_frequency = cfg->hi_freq;
      d->frequency = cfg->freq;
      d->polarization = cfg->polarization;
      d->symbol_rate = cfg->srate;
      d->code_rate = FEC_AUTO; // FEC_3_4
      
      int hi_lo = 0;
      
      struct dvb_frontend_info fe_info;
      if(ioctl(d->fe_fd,FE_GET_INFO, &fe_info) < 0) {
        close(d->fe_fd);
        driver_failure_posix(d->port, errno);
        return 0;
      }
      
      if(fe_info.type != FE_QPSK) {
        close(d->fe_fd);
        driver_failure_atom(d->port, "invalid_fe_type");
        return 0;
      }

      struct dvb_frontend_parameters feparams;
      memset(&feparams, 0, sizeof (struct dvb_frontend_parameters));
      
      feparams.frequency = d->frequency - d->lo_frequency;
      feparams.u.qpsk.symbol_rate = d->symbol_rate;
      feparams.u.qpsk.fec_inner = d->code_rate;
      
      fe_sec_voltage_t lnb_voltage = d->polarization == 'V' ? SEC_VOLTAGE_13 : SEC_VOLTAGE_18;
      if(ioctl(d->fe_fd, FE_SET_VOLTAGE, lnb_voltage) < 0) {
        close(d->fe_fd);
        driver_failure_atom(d->port, "error_setting_voltage");
        return 0;
      }

      if(ioctl(d->fe_fd, FE_SET_TONE, (hi_lo ? SEC_TONE_ON : SEC_TONE_OFF)) < 0) {
        close(d->fe_fd);
        driver_failure_atom(d->port, "error_setting_tone");
        return 0;
      }
      
      
      struct dvb_frontend_event event;
      
      
      /* The tuning of the card*/
      while(1)  {
        if (ioctl(d->fe_fd, FE_GET_EVENT, &event) < 0)	//EMPTY THE EVENT QUEUE
          break;
      }
      
      if (ioctl(d->fe_fd,FE_SET_FRONTEND,&feparams) < 0) {
        close(d->fe_fd);
        driver_failure_atom(d->port, "cannot_set_options");
        return 0;
      }


      // Streaming. Freq 12640000
      // Using DVB card "Montage Technology DS3000/TS2020"
      // Tuning DVB-S to Freq: 2040000 Hz, LO frequency 10600000 kHz Pol:V Srate=30000000, LNB number: 0
      // LNB voltage 13V
      // DISEQC SETTING SUCCEDED
      // polling....
      // Getting frontend event
      // FE_STATUS:
      // polling....
      // Getting frontend event
      // FE_STATUS:
      //      FE_HAS_SIGNAL : found something above the noise level
      //      FE_HAS_CARRIER : found a DVB signal
      //      FE_HAS_VITERBI : FEC is stable
      //      FE_HAS_SYNC : found sync bytes
      //      FE_HAS_LOCK : everything's working... 
      // Event:  Frequency: 12640000
      //         SymbolRate: 30000000
      //         FEC_inner:  9
      // 
      // Bit error rate: 0
      // Signal strength: 9464
      // SNR: 57640
      // FE_STATUS:
      //      FE_HAS_SIGNAL : found something above the noise level
      //      FE_HAS_CARRIER : found a DVB signal
      //      FE_HAS_VITERBI : FEC is stable
      //      FE_HAS_SYNC : found sync bytes
      //      FE_HAS_LOCK : everything's working... 
      // Card 0 tuned
      

      driver_select(d->port, (ErlDrvEvent)d->fe_fd, DO_READ, 1);
      memcpy(*rbuf, "ok", 2);
      return 2;
    }
    
    break;
    
    case CMD_GET_PID: {
      uint16_t pid = *(uint16_t *)buf;
      fprintf(stderr, "Request to open pid: %d\r\n", pid);
      d->dmx_pids[pid] = open("/dev/dvb/adapter0/demux0", O_RDWR);
      struct dmx_pes_filter_params pesFilterParams;
    	memset(&pesFilterParams, 0, sizeof(struct dmx_pes_filter_params));

      driver_select(d->port, (ErlDrvEvent)d->dmx_pids[pid], DO_READ, 1);

    	pesFilterParams.pid		= pid;
    	pesFilterParams.input		= DMX_IN_FRONTEND;
    	pesFilterParams.output		= DMX_OUT_TS_TAP;
    	pesFilterParams.pes_type	= (dmx_pes_type_t)DMX_PES_OTHER;
    	pesFilterParams.flags		= DMX_IMMEDIATE_START;

    	if (ioctl(d->dmx_pids[pid], DMX_SET_PES_FILTER, &pesFilterParams) < 0)  {
        fprintf(stderr, "Failed to set pid filter\r\n");
    		return 0;
    	}
      fprintf(stderr, "Added pid filter %d\r\n", pid);

      memcpy(*rbuf, "ok", 2);
      return 2;
    	
      break;
    }
    
    case CMD_START_INPUT: {
      d->dvr_fd = open("/dev/dvb/adapter0/dvr0", O_RDONLY | O_NONBLOCK);
      driver_select(d->port, (ErlDrvEvent)d->dvr_fd, DO_READ, 1);
      
      fprintf(stderr, "Started input on %d\r\n", d->dvr_fd);
      memcpy(*rbuf, "ok", 2);
      return 2;
    }
    default:
    return 0;
  }
  return 0;
}

void print_status(fe_status_t festatus) {
  fprintf(stderr, "FE_STATUS:\r\n");
  if (festatus & FE_HAS_SIGNAL) fprintf(stderr, "     FE_HAS_SIGNAL : found something above the noise level\r\n");
  if (festatus & FE_HAS_CARRIER) fprintf(stderr, "     FE_HAS_CARRIER : found a DVB signal\r\n");
  if (festatus & FE_HAS_VITERBI) fprintf(stderr, "     FE_HAS_VITERBI : FEC is stable\r\n");
  if (festatus & FE_HAS_SYNC) fprintf(stderr, "     FE_HAS_SYNC : found sync bytes\r\n");
  if (festatus & FE_HAS_LOCK) fprintf(stderr, "     FE_HAS_LOCK : everything's working... \r\n");
  if (festatus & FE_TIMEDOUT) fprintf(stderr, "     FE_TIMEDOUT : no lock within the last ... seconds\r\n");
  if (festatus & FE_REINIT) fprintf(stderr, "     FE_REINIT : frontend was reinitialized\r\n");
}


static void fe_input(Dvbs2 *d)
{
  int n = 5;
  
  struct dvb_frontend_event event;
  if(ioctl(d->fe_fd, FE_GET_EVENT, &event) < 0) {
    dvb_exit(d);
    return;
  }
  
  if(event.status & FE_HAS_LOCK) {
    fprintf(stderr, "Event:  Frequency: %d (or %d)\n",(unsigned int)((event.parameters.frequency)+d->lo_frequency),(unsigned int) abs((event.parameters.frequency)-d->lo_frequency));
    fprintf(stderr, "        SymbolRate: %d\n",event.parameters.u.qpsk.symbol_rate);
    fprintf(stderr, "        FEC_inner:  %d\n",event.parameters.u.qpsk.fec_inner);
  }
  
  int strength=0;
  if(ioctl(d->fe_fd,FE_READ_BER,&strength) >= 0)
  fprintf(stderr, "Bit error rate: %d\n",strength);

  strength=0;
  if(ioctl(d->fe_fd,FE_READ_SIGNAL_STRENGTH,&strength) >= 0)
  fprintf(stderr, "Signal strength: %d\n",strength);

  strength=0;
  if(ioctl(d->fe_fd,FE_READ_SNR,&strength) >= 0)
  fprintf(stderr, "SNR: %d\n",strength);
  
  fe_status_t festatus = 0;
  if(ioctl(d->fe_fd,FE_READ_STATUS,&festatus) >= 0)
  print_status(festatus);
  
  
  ErlDrvTermData reply[] = {
    ERL_DRV_ATOM, driver_mk_atom("dvb_ready"),
    ERL_DRV_PORT, driver_mk_port(d->port),
    ERL_DRV_TUPLE, 2
  };
  driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
}

static void dvb_input(Dvbs2 *d)
{
  ErlDrvBinary* bin = driver_alloc_binary(188*7*20);
  size_t n = read(d->dvr_fd, bin->orig_bytes, bin->orig_size);
  
  ErlDrvTermData reply[] = {
    ERL_DRV_ATOM, driver_mk_atom("dvb"),
    ERL_DRV_PORT, driver_mk_port(d->port),
    ERL_DRV_BINARY, (ErlDrvTermData)bin, (ErlDrvTermData)n, 0,
    ERL_DRV_TUPLE, 3
  };
  driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
  
}

static void dvbs2_drv_input(ErlDrvData handle, ErlDrvEvent io_event)
{
  Dvbs2* d = (Dvbs2*) handle;
  
  if(d->fe_fd == io_event) {
    fe_input(d);
  } else if(d->dvr_fd == io_event) {
    dvb_input(d);
  }
}


static void dvbs2_inet_timeout(ErlDrvData handle)
{
  Dvbs2* d = (Dvbs2 *)handle;
  fprintf(stderr, "Timeout in socket\r\n");
  dvb_exit(d);
}

ErlDrvEntry dvbs2_driver_entry = {
    NULL,			/* F_PTR init, N/A */
    dvbs2_drv_start,		/* L_PTR start, called when port is opened */
    dvbs2_drv_stop,		/* F_PTR stop, called when port is closed */
    NULL,	                /* F_PTR output, called when erlang has sent */
    dvbs2_drv_input,		/* F_PTR ready_input, called when input descriptor ready */
    NULL,	              /* F_PTR ready_output, called when output descriptor ready */
    "dvbs2_drv",		/* char *driver_name, the argument to open_port */
    NULL,			/* F_PTR finish, called when unloaded */
    NULL,     /* void *handle */
    dvbs2_drv_command,			/* F_PTR control, port_command callback */
    dvbs2_inet_timeout,			/* F_PTR timeout, reserved */
    NULL,	                     /* F_PTR outputv, reserved */
    NULL,                      /* ready_async */
    NULL,                             /* flush */
    NULL,                             /* call */
    NULL,                             /* event */
    ERL_DRV_EXTENDED_MARKER,          /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING,     /* ERL_DRV_FLAGs */
    NULL,     /* void *handle2 */
    NULL,     /* process_exit */
    NULL      /* stop_select */
};
DRIVER_INIT(dvbs2_drv) /* must match name in driver_entry */
{
    return &dvbs2_driver_entry;
}
