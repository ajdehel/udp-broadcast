/**************************************************************************************************/

#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#include <csignal>
#include <iostream>
#include <string>
#include <sstream>

#include "utils.hpp"


/***************************************************************************************************
 * Arguments for program
 **************************************************************************************************/

struct Arguments
{
  uint16_t    listener_port;
  std::string sink_host;
  uint16_t    sink_port;
  unsigned    instance_id;
};


/***************************************************************************************************
 * Function Declarations
 **************************************************************************************************/

int main( int argc, char *argv[] );
int parse_args( int argc, char *argv[], Arguments *parsed_args );
void signal_handler( int signum );
template<class InType, class OutType>
void parse( const InType &arg_in, OutType &value );


/***************************************************************************************************
 * Constants
 **************************************************************************************************/

bool STOP = false;
unsigned BUFFSIZE = 4096;


/***************************************************************************************************
 * Function Definitions
 **************************************************************************************************/

/**************************************************************************************************/
int main(int argc, char *argv[])
{
  signal(SIGINT, signal_handler);
  Arguments args = {};
  parse_args(argc, argv, &args);
  // Get socket and set SO_BROADCAST sockopt
  int sockfd = socket(AF_INET, SOCK_DGRAM, 0);
  if (sockfd < 0)
  {
    std::cerr << "Could not create Socket.";
    exit(1);
  }
  uint32_t broadcast = 1;
  if (setsockopt(sockfd, SOL_SOCKET, SO_BROADCAST, &broadcast, sizeof(broadcast)) < 0)
  {
    std::cerr << "Could not set socket to broadcast mode." << std::endl;
    exit(1);
  }
  // Bind listener socket
  struct sockaddr_in listener_addr = {};
  listener_addr.sin_family      = AF_INET;
  listener_addr.sin_addr.s_addr = INADDR_ANY;
  listener_addr.sin_port        = htons(args.listener_port);
  if (bind(sockfd,(sockaddr*)&listener_addr, sizeof(listener_addr)) < 0)
  {
    std::cerr << "Could not bind socket." << std::endl;
    exit(1);
  }
  // Set Destination Address
  struct sockaddr_in sink_addr = {};
  sink_addr.sin_family          = AF_INET;
  sink_addr.sin_addr.s_addr     = inet_addr(args.sink_host.c_str());
  sink_addr.sin_port            = htons(args.sink_port);
  // Set up run loop
  int num_msgs = 1;
  // Run loop
  uint8_t buffer[BUFFSIZE];
  while (!STOP)
  {
    std::stringstream msg_stream;
    struct sockaddr_in recv_addr = {};
    unsigned addr_len = sizeof(recv_addr);
    unsigned bytes_recv;
    bytes_recv = recvfrom(sockfd, buffer, BUFFSIZE, 0, (sockaddr*)&recv_addr, &addr_len);
    std::string recv_host = std::string( inet_ntoa(recv_addr.sin_addr) );
    uint16_t    recv_port = ntohs(recv_addr.sin_port);
    msg_stream << buffer;
    msg_stream << "C++ Client["<< args.instance_id <<"] msg #"<<num_msgs<<";;;;";
    msg_stream << "C++ Client["<< args.instance_id <<"] "<<INADDR_ANY<<":"<<args.listener_port
               << " <= "<<recv_host<<":"<<recv_port<<";;;;";
    msg_stream << "C++ Client["<< args.instance_id <<"] "<<INADDR_ANY<<":"<<args.listener_port
               << " => "<<args.sink_host<<":"<<args.sink_port<<";;;;";
    std::string msg = msg_stream.str();
    sendto(sockfd, msg.c_str(), msg.length(), 0, (sockaddr*)&sink_addr, sizeof(sink_addr));
    std::cout << "Msg "<<num_msgs<<" received"<< std::endl;

    num_msgs++;
  }
  // Close socket and exit
  close(sockfd);
  return 0;
}

/**************************************************************************************************/
void signal_handler( int signum )
{
  std::cout << std::endl << "Received Signal." << std::endl;
  STOP = true;
}

/**************************************************************************************************/
int parse_args( int argc, char *argv[], Arguments *parsed_args )
{
  std::string       arg;
  unsigned num_args_parsed = 0;
  for (unsigned i_arg = 1; i_arg < argc; )
  {
    arg = std::string(argv[i_arg]);
    // First positional arg is the Listener Port
    if (num_args_parsed == 0)
    {
      parse( arg, parsed_args->listener_port );
      num_args_parsed++;
      i_arg += 1;
    }
    // Second positional arg is the Destination IP Address <IPv4>:<PORT>
    else if (num_args_parsed == 1)
    {
      unsigned split_pos = arg.find_first_of(":");
      parse( arg.substr(0, split_pos), parsed_args->sink_host );
      parse( arg.substr(split_pos+1),  parsed_args->sink_port );
      num_args_parsed++;
      i_arg += 1;
    }
    // Third positional arg is Instance ID
    else if (num_args_parsed == 2)
    {
      parse( arg, parsed_args->instance_id );
      num_args_parsed++;
      i_arg += 1;
    }
    // Invalid Arg
    else
    {
      std::cerr << "Encountered unexpected argument." << std::endl;
      exit(1);
    }
  }
  return 0;
}

/**************************************************************************************************/
template<class InType, class OutType>
void parse( const InType &arg_in, OutType &value )
{
  std::stringstream stream;
  stream << arg_in;
  stream >> value;
}

